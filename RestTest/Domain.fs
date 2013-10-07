module Domain
open System.Collections.Generic

// set up a type to represent the options
type Options() = 
    
    [<CommandLine.Option('i', "inputfile", Required = true, HelpText = "Input file to be processed.")>]
    member val InputFileName  = "" with get, set

    [<CommandLine.Option('d', "inputdir", Required = false, HelpText = "Input directory which contains all the files to be processed.")>]
    member val InputDirName  = "" with get, set

    [<CommandLine.Option('o', "onputfile", Required = false, HelpText = "Output file to write the results.")>]
    member val OutputFileName  = "" with get, set

    [<CommandLine.Option('m', "mode", DefaultValue="Documentation" ,HelpText = "Mode in which test should be run. [Documentation|Test]")>]
    member val Mode  = "" with get, set

type TestType =
    | DeepEquals
    | Equals
    | NotEqual
    | Exists

type TestAssert =
    {
        TestType        :   TestType
        TestProperty    :   string
        ExpectedValue   :   string   
    }

type Parameter() =
    member val Name         = "" with get, set
    member val Value        = "" with get, set
    member val Description  = "" with get, set
    member val Default      = "" with get, set
    member val Required     = false with get, set

type TestCase() =
    member val Name        = "" with get, set
    member val Url         = "" with get, set
    member val Request     = "" with get, set
    member val Parameters  = new Dictionary<string, Parameter>() with get, set
    member val Method      = RestSharp.Method.POST with get, set
    member val Headers     = new Dictionary<string, string>() with get, set
    member val Body        = "" with get, set
    member val Asserts     = new   List<TestAssert>() with get, set 
    

type TestComponent =
    | Url of string
    | Headers of Dictionary<string, string>
    | Body of string
