module TestRunner
open Domain
open System.Collections.Generic
open System.Linq
open FParsec
open System
   
let (|StartsWith|_|) (tag: string) (value: string) =
    if value.StartsWith(tag) then Some()
    else None

let (|InvariantEqual|_|) (cmp: string) (str:string)  = 
  if String.Compare(str, cmp, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

// Parses input as key value pair using key:value format
let getKeyValue(input: string) =
    let input = input.Replace("\r\n", "").Replace("\t", "").Trim()
    (input.Substring(0, input.IndexOf(':')).Trim(), input.Substring(input.IndexOf(':') + 1).Trim())

let getValues(input: string, paramCount: int) =
    let values = input.Split([|'\''|], System.StringSplitOptions.RemoveEmptyEntries)
    if values.Length < paramCount then
        failwithf "The input key is not valid: %s" input
    
    match values.Length with
    | 3 -> (values.[0], values.[2])
    | 2 -> (values.[0], values.[1])
    | 1 -> (values.[0], "")
    | _ -> failwithf "The input key is not valid: %s" input

let getSubKey(input: string) =
    let count = input.Split('.').Count() - 1
    match count with
    | 1 -> (input.Substring(input.IndexOf('.') + 1).Trim(), "")
    | 2 -> (input.Substring(input.IndexOf('.') + 1, input.LastIndexOf('.') - input.IndexOf('.') - 1), input.Substring(input.LastIndexOf('.') + 1))
    | _ -> failwithf "The input key is not valid: %s" input

let getSectionValue(input: string[]) =
    let (key, value) = getKeyValue(input.[0])
    let parameters = new Dictionary<string, string>()
    for line in input.Skip(1) do
        let (key, value) = getKeyValue(line)
        parameters.Add(key, value)
    (key, value, parameters)


let getTests(input: string) =
    input.Split([|"#test: "|], System.StringSplitOptions.RemoveEmptyEntries)
    

let parseTest(input: string) =
    let testCase = new TestCase()
    let testLines = input.Split('#')
    for testLine in testLines.Skip(1) do
        let (key, value) = getKeyValue(testLine)
        match key with
        | StartsWith "request" ->
            testCase.Request <- value
        
        | StartsWith "method" ->
            testCase.Method <- 
                match value with
                | InvariantEqual "post" -> RestSharp.Method.POST
                | InvariantEqual "get" -> RestSharp.Method.GET
                | InvariantEqual "delete" -> RestSharp.Method.DELETE
                | InvariantEqual "put" -> RestSharp.Method.PUT
                | _ -> failwithf "Request method not supported: %s" value
        
        | StartsWith "parameter" ->
            let (keyType, subKey) = getSubKey(key)
            match testCase.Parameters.TryGetValue(keyType) with
            | (true, parameter) ->
                match subKey with
                | "" -> parameter.Name <- value
                | "description" -> parameter.Description <- value
                | _ -> failwithf "Parameter not supported: %s" key
            | _ -> 
                let parameter = new Parameter()
                match subKey with
                | "" -> parameter.Name <- value
                | "description" -> parameter.Description <- value
                | _ -> failwithf "Parameter not supported: %s" key

                testCase.Parameters.Add(keyType, parameter)

        | StartsWith "header" ->
            let (keyType, subKey) = getSubKey(key)
            if testCase.Headers.ContainsKey(keyType) then
                testCase.Headers.[keyType] <- value
            else
                testCase.Headers.Add(keyType, value)

        | StartsWith "body" -> 
            testCase.Body <- value

        | StartsWith "assert" ->
            let (keyType, _) = getSubKey(key)
            match keyType with
            | InvariantEqual "deepEquals" -> 
                let (value1, value2) = getValues(value, 2)
                testCase.Asserts.Add({TestType = DeepEquals; TestProperty = value1; ExpectedValue = value2})
            | InvariantEqual "equals" -> 
                let (value1, value2) = getValues(value, 2)
                testCase.Asserts.Add({TestType = Equals; TestProperty = value1; ExpectedValue = value2})
            | InvariantEqual "exists" -> 
                let (value1, value2) = getValues(value, 1)
                testCase.Asserts.Add({TestType = Exists; TestProperty = value1; ExpectedValue = ""})
            | _ -> failwithf "Parameter not supported: %s" key
        | _ -> failwithf "Key not supported: %s" key

    testCase


let RunTest(fileData: string) =   
    let tests = new List<TestCase>()    
    let testLines = getTests(fileData.Substring(fileData.IndexOf("#test")))
    for testLine in testLines do
        tests.Add(parseTest(testLine))
    

let RunTests(options: Options) = 
    let fileData = System.IO.File.ReadAllText(options.InputFileName)
    RunTest(fileData)

