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

let getListFromValues values = unbox<Collections.List<string>> values

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
                | StartsWith "RESOURCE_" -> ()
                | StartsWith "RESOURCE" ->
                    let resource = new RestResource()
                    resource.ResourceName <- kg.Item(0).Substring(9)
                    rest.Resources.Add(resource)
                
                | StartsWith "URI_" -> ()
                   
                | StartsWith "URI" ->
                    let uri = new ResourceUri()
                    uri.Name <- kg.Item(0).Substring(4)
                    uri.Method <-
                        match RestSharp.Method.TryParse(uri.Name.Substring(0, uri.Name.IndexOf(' '))) with
                        | (true, x) -> x
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

                    | StartsWith "RESOURCE_PARAMETERS" ->
                        let values = getListFromValues value
                        let param = new ResourceParam()
                        param.Name <- key
                        if key.StartsWith("+") then param.Required <- true
                        param.DataType <- values.[0]
                        param.DefaultValue <- values.[1]
                        param.Description <- values.[2]
                        rest.Resources.Last().Params.Add(param)

                    | StartsWith "RESOURCE" ->
                        match key with
                        | "description" -> rest.Resources.Last().ResourceDescription <- unbox<string> value
                        | "note" -> rest.Resources.Last().Note <- unbox<string> value
                        | _ -> ()
                    
                    | StartsWith "URI_PARAMETERS" ->
                        let values = getListFromValues value
                        let param = new ResourceParam()
                        param.Name <- key
                        if key.StartsWith("+") then param.Required <- true
                        param.DataType <- values.[0]
                        param.DefaultValue <- values.[1]
                        param.Description <- values.[2]
                        rest.Resources.Last().Uris.Last().ResourceParams.Add(param)
                    
                    | StartsWith "STATUS_CODES" ->
                        rest.Resources.Last().Uris.Last().StatusCodes.Add(key, unbox<string> value)

                    | StartsWith "URI" ->
                        match key with
                        | "description" -> rest.Resources.Last().Uris.Last().Description <- unbox<string> value
                        | "note" -> rest.Resources.Last().Uris.Last().Note <- unbox<string> value
                        | _ -> ()
                    
                    | StartsWith "EXAMPLE" ->
                        match key with
                        | "description" -> rest.Resources.Last().Uris.Last().Examples.Last().Description <- unbox<string> value
                        | "note" -> rest.Resources.Last().Uris.Last().Examples.Last().Note <- unbox<string> value
                        | _ -> ()
                    
                    | StartsWith "HEADERS" ->
                        rest.Resources.Last().Uris.Last().Examples.Last().Headers.Add(key, unbox<string> value)

                    | StartsWith "PARAMETER" ->
                        match key with
                        | "body" -> 
                            rest.Resources.Last().Uris.Last().Examples.Last().Body <- unbox<string> value
                        | _ -> rest.Resources.Last().Uris.Last().Examples.Last().QueryParams.Add(key, unbox<string> value) 

                    | StartsWith "ASSERTS" ->
                        let test = new Assert()
                        test.AssertType <- 
                            match AssertType.TryParse(key, true) with
                            | (true, x)-> x
                            | _ -> failwithf "Undefied assert condition: %s" key
                        test.Values.AddRange(getListFromValues value)
                        rest.Resources.Last().Uris.Last().Examples.Last().Asserts.Add(test)
                    | _ -> ()
                else
                    // Pair belongs to the top level resource 
                    match key with
                    | "url" -> rest.Url <- unbox<string> value
                    | "name" -> rest.DocumentName <- unbox<string> value
                    | "description" -> rest.DocumentDescription <- unbox<string> value
                    | "version" -> rest.Version <- unbox<string> value
                    | "api_version" -> rest.ApiVersion <- unbox<string> value
                    | "response_format" -> 
                        rest.ResponseFormat.AddRange(getListFromValues value)

                    | "request_format" -> 
                        rest.RequestFormat.AddRange(getListFromValues value)
                    | _ -> ()

    | Failure(a, b, c) -> 
        Console.WriteLine(a)
        Console.WriteLine(b)
        Console.WriteLine(c)
    
    rest

let example = """
url : "http://localhost:9800"
name : "FlexSearch API documentation"
description : "
FlexSearch is a high performance REST/SOAP services based full-text searching platform built on top of the popular Lucene search library. At its core it is about extensibility and maintainability with minimum overhead. FlexSearch is written in F# & C# 5.0 (.net framework 4.5). It exposes REST, SOAP and Binary based web service endpoints enabling easy integration. It has an extensive plug-in architecture with ability to customize most of the functionality with minimum amount of efforts. One area where Lunar particularly excel over competition is providing easy extensible connector model which allows a developer to tap directly into core’s indexing engine, thus avoiding the reliance on web services. This results in a greatly improved indexing performance when indexing over millions of records.
"
version: "0.2.1"
api_version: "1.0"
response_format : ["XML", "JSON"]
request_format : ["XML, JSON"]

[RESOURCE Document]
description : "Creates a new index"
-note : "Use with care"

[RESOURCE_PARAMETERS] 
OpenIndex : ["int", "default", "open the newly created index"]
CloseIndex : ["int", "default", "open the newly created index"]

[URI POST resource/{id}/{user}]

description : "Creates a new index"
note : "Use with care"

[URI_PARAMETERS] 
OpenIndex : ["int", "default", "open the newly created index"]
CloseIndex : ["int", "default", "open the newly created index"]

[STATUS_CODES]
200 : "OK"
400 : "Invalid word supplied"

[EXAMPLE Create a simple index]

description : "Creates a new index"
note : "Use with care"

[HEADERS]
content-type: "text/json"
Accept-Encoding: "gzip, deflate"
Accept-Language: "en-GB"

[PARAMETER]
id: "23"
user: "abc"

body : "
{
'firstname':'test'
}"

[ASSERTS]
deepEquals: ["abc", "{ 'firstname':'test' }"]
equals: ["abc" , "dsdsd"]
exists: ["abc"]
"""


let test() =
    let result = parse example
    RestTest.ViewEngine.Hepler.RenderHtml(result)
    Console.ReadLine() |> ignore