module TestRunner
open Domain
open System.Collections.Generic
open System.Linq
open FParsec
open System
open RestTest.ViewEngine  
      
let (|StartsWith|_|) (tag: string) (value: string) =
    if value.StartsWith(tag) then Some()
    else None

let (|InvariantEqual|_|) (cmp: string) (str:string)  = 
  if String.Compare(str, cmp, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

//// Parses input as key value pair using key:value format
//let getKeyValue(input: string) =
//    let input = input.Replace("\r\n", "").Replace("\t", "").Trim()
//    (input.Substring(0, input.IndexOf(':')).Trim(), input.Substring(input.IndexOf(':') + 1).Trim())
//
//let getValues(input: string, paramCount: int) =
//    let values = input.Split([|'\''|], System.StringSplitOptions.RemoveEmptyEntries)
//    if values.Length < paramCount then
//        failwithf "The input key is not valid: %s" input
//    
//    match values.Length with
//    | 3 -> (values.[0], values.[2])
//    | 2 -> (values.[0], values.[1])
//    | 1 -> (values.[0], "")
//    | _ -> failwithf "The input key is not valid: %s" input
//
//let getSubKey(input: string) =
//    let count = input.Split('.').Count() - 1
//    match count with
//    | 1 -> (input.Substring(input.IndexOf('.') + 1).Trim(), "")
//    | 2 -> (input.Substring(input.IndexOf('.') + 1, input.LastIndexOf('.') - input.IndexOf('.') - 1), input.Substring(input.LastIndexOf('.') + 1))
//    | _ -> failwithf "The input key is not valid: %s" input
//
//let getSectionValue(input: string[]) =
//    let (key, value) = getKeyValue(input.[0])
//    let parameters = new Dictionary<string, string>()
//    for line in input.Skip(1) do
//        let (key, value) = getKeyValue(line)
//        parameters.Add(key, value)
//    (key, value, parameters)
//




type Token = KeyGroup of string list | KeyValue of string * obj

let (<||>) p1 p2 = attempt (p1 |>> box) <|> attempt (p2 |>> box)
let spc      = many (anyOf [' '; '\t']) 
let lexeme s = pstring s .>> spc
let lexemel s= pstring s .>> spaces
let comment  = pchar '-' .>>. restOfLine false 
let blanks   = skipMany ((comment <||> spc) .>> newline .>> spc) .>> spc
let brace p  = between (lexemel "[") (lexemel "]") p
let pbool    = (lexeme "true" >>% true) <|> (lexeme "false" >>% false)
let pstr     = between (lexeme "\"") (lexeme "\"") (manySatisfy ((<>)'"'))
let pdate' s = try preturn (DateTime.Parse (s, null, Globalization.DateTimeStyles.RoundtripKind)) with _ -> fail ""
let pdate    = between spc spc (anyString 20) >>= pdate'
let ary elem = brace (sepBy (elem .>> spaces) (lexemel ","))
let pary     = ary pbool <||> ary pdate <||> ary pint32 <||> ary pstr <||> ary pfloat
let value    = pbool <||> pdate <||> pstr <||> pfloat <||> pint32 <||> pary <||> ary pary
let kvKey    = many1Chars (noneOf " \t\n:")
let keyvalue = (kvKey .>> spc) .>>. (lexeme ":" >>. value) |>> KeyValue
let kgKey    = (many1Chars (noneOf "\t\n].")) .>> spc
let keygroup = blanks >>. brace (sepBy kgKey (lexeme ".")) |>> KeyGroup
let document = blanks >>. many (keygroup <|> keyvalue .>> blanks)

let parse text =
    let resource = new RestResource()
    let currentKg = ref []
    match run document text with
    | Success(tokens,_,_) ->
        for token in tokens do
            match token with
            | KeyGroup kg ->
                currentKg := kg 
                match kg.Item(0) with
                | StartsWith "URI" ->
                    let uri = new ResourceUri()
                    uri.Name <- kg.Item(0).Substring(4)
                    uri.Method <-
                        match uri.Name.Substring(0, uri.Name.IndexOf(' ')) with
                        | "POST" -> RestSharp.Method.POST
                        | _ -> failwithf "Unsupported request method in : '%s'" uri.Name
                    
                    uri.Request <- uri.Name.Substring(uri.Name.IndexOf(' ')).Trim()
                    resource.Uris.Add(uri)
                    
                | StartsWith "EXAMPLE" ->
                    let resourceExample = new ResourceExample()
                    resourceExample.Name <- kg.Item(0).Substring(8)
                    resource.Uris.Last().Examples.Add(resourceExample)

                | _ -> ()
            | KeyValue (key,value) -> 
                let currentKg = !currentKg
                if currentKg.Any() then
                    match currentKg.[0] with
                    | StartsWith "URI" ->
                        match key with
                        | "description" -> resource.Uris.Last().Description <- unbox<string> value
                        | "note" -> resource.Uris.Last().Note <- unbox<string> value
                        | _ -> ()
                    | StartsWith "EXAMPLE" ->
                        match key with
                        | "description" -> resource.Uris.Last().Examples.Last().Description <- unbox<string> value
                        | "note" -> resource.Uris.Last().Examples.Last().Note <- unbox<string> value
                        | _ -> ()

                    | _ -> ()

                // Pair belongs to the top level resource 
                match key with
                | "description" -> resource.ResourceDescription <- unbox<string> value
                | "resource" -> resource.ResourceName <- unbox<string> value
                | _ -> ()

    | Failure(a, b, c) -> 
        Console.WriteLine(a)
        Console.WriteLine(b)
        Console.WriteLine(c)
    
    resource

let example = """
url : "http://localhost:9800"
resource : "document"
description : "Creates a new index"

-------------------------------------------------------------------------
[URI POST resource/{id}/{user}]
-------------------------------------------------------------------------
description : "Creates a new index"
note : "Use with care"
params : "
OpenIndex, int, required, false, open the newly created index 
"
statuscodes : "
200, OK
400, Invalid word supplied
"

information : "
Response Formats: XML JSON JSV CSV X-MSGPACK X-PROTOBUF SOAP 1.1 SOAP 1.2
HTTP Methods: POST
Minimum API version: v1.0
Resource URL: http://localhost:9800/index/create
"

[EXAMPLE Create a simple index]
-------------------------------------------------------------------------
description : "Creates a new index"
note : "Use with care"
queryparams : "
id: 23 
user: abc
"

headers : "
content-type: text/json
Accept-Encoding: gzip, deflate
Accept-Language: en-GB 
"

body : "
{
	'firstname':'test'
}"

deepEquals: "abc, { 'firstname':'test'	}"
equals: "abc, dsdsd"
exists: "abc"
"""


let test() =
    let result = parse example
    Console.ReadLine() |> ignore