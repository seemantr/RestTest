module TestRunner
open Domain
open System.Collections.Generic
open System.Linq
open FParsec
open System
open RestSharp
open RestTest.ViewEngine  
open System.Text
open System.Text.RegularExpressions
open Newtonsoft.Json
      
let (|StartsWith|_|) (tag: string) (value: string) =
    if value.StartsWith(tag, true, Globalization.CultureInfo.InvariantCulture) then Some()
    else None

let (|InvariantEqual|_|) (cmp: string) (str:string)  = 
  if String.Compare(str, cmp, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

let getKey (line: string) =
    if line.IndexOf(':') <> -1 then
        line.Substring(0, line.IndexOf(":")).Trim()
    else
        failwithf "Expecting ':' in line %s" line

let getValue (line: string) =
    if line.IndexOf(':') <> -1 then
        line.Substring(line.IndexOf(":") + 1).Trim()
    else
        failwithf "Expecting ':' in line %s" line

let getValues (line: string) =
    if line.IndexOf(':') <> -1 then
        let line = line.Substring(line.IndexOf(':') + 1)
        line.Split(',').ToList()
    else
        failwithf "Expecting ':' in line %s" line

let getMultiline (lines : string[]) (i : int) =
    let mutable j = i
    let sb = new StringBuilder() 
    while j < lines.Count() && lines.[j].StartsWith("..") do
        sb.AppendLine(lines.[j]) |> ignore
        j <- j + 1
    (sb.ToString(), j)

let parse (text: string) =
    let document = new List<(string * string * string)>()
    let rest = new RestDocument()

    let mutable currentKey = ""
    let mutable currentValue = new StringBuilder()
    let mutable multiLineKey = false
    let lines = text.Split([|"\r\n"|], StringSplitOptions.None)

    for line in lines do
        let line = line.Trim()
        match line with
        | StartsWith "-" -> ()
        | "" -> ()

        // Main section
        | StartsWith "url" -> rest.Url <- getValue line
        | StartsWith "name" -> rest.DocumentName <- getValue line
        | StartsWith "version" -> rest.Version <- getValue line
        | StartsWith "api_version" -> rest.ApiVersion <- getValue line
        | StartsWith "response_format" -> rest.ResponseFormat <- getValues line
        | StartsWith "request_format" -> rest.RequestFormat <- getValues line
        | StartsWith "description" ->
            multiLineKey <- true
            currentKey <- "description" 
            
        // Resource Section
        | StartsWith "resource-description" ->
            multiLineKey <- true
            currentKey <- "resource-description" 
        
        | StartsWith "resource-note" ->
            multiLineKey <- true
            currentKey <- "resource-note"               
        
        | StartsWith "resource-parameter" ->
            multiLineKey <- false
            currentKey <- "resource-parameter"

        | StartsWith "resource" ->
            let resource = new RestResource()
            resource.ResourceName <- getValue line
            rest.Resources.Add(resource)

        // Uri Section
        | StartsWith "uri-description" ->
            multiLineKey <- true
            currentKey <- "uri-description" 
        
        | StartsWith "uri-note" ->
            multiLineKey <- true
            currentKey <- "uri-note"
                  
        | StartsWith "uri-parameter" ->
            multiLineKey <- false
            currentKey <- "uri-parameter"
        
        | StartsWith "uri-statuscodes" ->
            multiLineKey <- false
            currentKey <- "uri-statuscodes"

        | StartsWith "uri" ->
            let uri = new ResourceUri()
            uri.Name <- getValue line
            uri.Method <-
                match RestSharp.Method.TryParse(uri.Name.Substring(0, uri.Name.IndexOf(' '))) with
                | (true, x) -> x
                | _ -> failwithf "Unsupported request method in : '%s'" uri.Name
                    
            uri.Request <- uri.Name.Substring(uri.Name.IndexOf(' ')).Trim()
            rest.Resources.Last().Uris.Add(uri)

        // Example
        | StartsWith "example-description" ->
            multiLineKey <- true
            currentKey <- "example-description" 
        
        | StartsWith "example-note" ->
            multiLineKey <- true
            currentKey <- "example-note"

        | StartsWith "example-headers" ->
            multiLineKey <- false
            currentKey <- "example-headers"

        | StartsWith "example-body" ->
            multiLineKey <- true
            currentKey <- "example-body"

        | StartsWith "example" ->
            let example = new ResourceExample()
            example.Name <- getValue line
            rest.Resources.Last().Uris.Last().Examples.Add(example)

        | StartsWith ".." ->
            if multiLineKey then
                match currentKey with
                | "description" -> 
                    rest.DocumentDescription <- currentValue.ToString()
                | "resource-description" -> 
                    rest.Resources.Last().ResourceDescription <- currentValue.ToString()
                | "resource-note" -> 
                    rest.Resources.Last().Note <- currentValue.ToString()
                | "uri-description" -> 
                    rest.Resources.Last().Uris.Last().Description <- currentValue.ToString()
                | "uri-note" -> 
                    rest.Resources.Last().Uris.Last().Note <- currentValue.ToString()
                | "example-description" -> 
                    rest.Resources.Last().Uris.Last().Examples.Last().Description <- currentValue.ToString()
                | "example-note" -> 
                    rest.Resources.Last().Uris.Last().Examples.Last().Note <- currentValue.ToString()
                | "example-body" -> 
                    let body = Newtonsoft.Json.Linq.JObject.Parse(currentValue.ToString().Trim())
                    rest.Resources.Last().Uris.Last().Examples.Last().Body <- body.ToString(Newtonsoft.Json.Formatting.Indented)
                    
                    let generateResponse = true
                    if generateResponse then
                        // Make the request
                        let client = new RestClient(rest.Url)
                        let request = new RestRequest(rest.Resources.Last().Uris.Last().Request, rest.Resources.Last().Uris.Last().Method)
                        request.RequestFormat <- DataFormat.Json
                    
                        for req in rest.Resources.Last().Uris.Last().Examples.Last().Headers do
                            request.AddHeader(req.Key, req.Value) |> ignore
                        request.AddParameter("application/json", rest.Resources.Last().Uris.Last().Examples.Last().Body, ParameterType.RequestBody) |> ignore
                    
                        let result = client.Execute(request)
                        if result.StatusCode = Net.HttpStatusCode.OK then
                            let responseContent = Newtonsoft.Json.Linq.JObject.Parse(result.Content)
                            result.Content <- responseContent.ToString(Newtonsoft.Json.Formatting.Indented)

                        rest.Resources.Last().Uris.Last().Examples.Last().Response <- result
                    
                | _ -> failwithf "Unexpected '..'"

                multiLineKey <- false
                currentKey <- ""
                currentValue.Clear() |> ignore
        | _ ->
            if multiLineKey then
                currentValue.AppendLine(line) |> ignore
            else
                match currentKey with
                | "resource-parameter" -> 
                    let values = getValues line
                    let param = new ResourceParam()
                    param.Name <- getKey line
                    if param.Name.StartsWith("+") then 
                        param.Required <- true
                        param.Name <- param.Name.Substring(1)

                    param.DataType <- values.[0]
                    param.DefaultValue <- values.[1]
                    param.Description <- values.[2]
                    rest.Resources.Last().Params.Add(param)
                
                | "uri-parameter" -> 
                    let values = getValues line
                    let param = new ResourceParam()
                    param.Name <- getKey line
                    if param.Name.StartsWith("+") then param.Required <- true
                    param.DataType <- values.[0]
                    param.DefaultValue <- values.[1]
                    param.Description <- values.[2]
                    rest.Resources.Last().Uris.Last().ResourceParams.Add(param)
                
                | "uri-statuscodes" ->
                    rest.Resources.Last().Uris.Last().StatusCodes.Add(getKey line, getValue line)

                | "example-headers" ->
                    rest.Resources.Last().Uris.Last().Examples.Last().Headers.Add(getKey line, getValue line)
                | _ -> ()                           
    rest
                  
let test() =
   
    let result = parse(System.IO.File.ReadAllText("F:\\GitHub\Sample RestTest.txt"))

    RestTest.ViewEngine.Hepler.RenderHtml(result)
    Console.ReadLine() |> ignore