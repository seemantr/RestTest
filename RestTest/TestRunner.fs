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

let generateParams(resource: ResourceUri, value: string) =
    let lines = value.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    for line in lines do
        let cols = line.Split([|','|], System.StringSplitOptions.None)
        let param = new ResourceParam()
        param.Name <- cols.[0]
        param.DataType <- cols.[1]
        param.DefaultValue <- cols.[2]
        param.Required <- cols.[3]
        param.Description <- cols.[4]
        resource.ResourceParams.Add(param)

let parse text =
    let rest = new RestDocument()
    let currentKg = ref []
    match run document text with
    | Success(tokens,_,_) ->
        for token in tokens do
            match token with
            | KeyGroup kg ->
                currentKg := kg 
                
                match kg.Item(0) with
                | StartsWith "RESOURCE" ->
                    let resource = new RestResource()
                    resource.ResourceName <- kg.Item(0).Substring(9)
                    rest.Resources.Add(resource)
                    
                | StartsWith "URI" ->
                    let uri = new ResourceUri()
                    uri.Name <- kg.Item(0).Substring(4)
                    uri.Method <-
                        match uri.Name.Substring(0, uri.Name.IndexOf(' ')) with
                        | "POST" -> RestSharp.Method.POST
                        | _ -> failwithf "Unsupported request method in : '%s'" uri.Name
                    
                    uri.Request <- uri.Name.Substring(uri.Name.IndexOf(' ')).Trim()
                    rest.Resources.Last().Uris.Add(uri)
                    
                | StartsWith "EXAMPLE" ->
                    let resourceExample = new ResourceExample()
                    resourceExample.Name <- kg.Item(0).Substring(8)
                    rest.Resources.Last().Uris.Last().Examples.Add(resourceExample)

                | _ -> ()
            | KeyValue (key,value) -> 
                let currentKg = !currentKg
                if currentKg.Any() then
                    match currentKg.[0] with
                    | StartsWith "RESOURCE" ->
                        match key with
                        | "description" -> rest.Resources.Last().ResourceDescription <- unbox<string> value
                        | "note" -> rest.Resources.Last().Note <- unbox<string> value
                        | _ -> ()
                    | StartsWith "URI" ->
                        match key with
                        | "description" -> rest.Resources.Last().Uris.Last().Description <- unbox<string> value
                        | "note" -> rest.Resources.Last().Uris.Last().Note <- unbox<string> value
                        | "params" -> generateParams(rest.Resources.Last().Uris.Last(), unbox<string> value)
                        | _ -> ()
                    | StartsWith "EXAMPLE" ->
                        match key with
                        | "description" -> rest.Resources.Last().Uris.Last().Examples.Last().Description <- unbox<string> value
                        | "note" -> rest.Resources.Last().Uris.Last().Examples.Last().Note <- unbox<string> value
                        | _ -> ()

                    | _ -> ()

                // Pair belongs to the top level resource 
                match key with
                | "description" -> rest.DocumentDescription <- unbox<string> value
                | "name" -> rest.DocumentName <- unbox<string> value
                | "version" -> rest.Version <- unbox<string> value
                | _ -> ()

    | Failure(a, b, c) -> 
        Console.WriteLine(a)
        Console.WriteLine(b)
        Console.WriteLine(c)
    
    rest

let example = """
url : "http://localhost:9800"
name : "FlexSearch API documentation"
description : "Creates a new index"
version: "0.2.1"

-------------------------------------------------------------------------
[RESOURCE document]
-------------------------------------------------------------------------
description : "Creates a new index"

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
    RestTest.ViewEngine.Hepler.RenderHtml(result)
    Console.ReadLine() |> ignore