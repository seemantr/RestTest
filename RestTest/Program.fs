[<EntryPoint>]
let main argv = 
    UnitTests.TestSimpleFile()
    let mutable result: Domain.Options = Unchecked.defaultof<Domain.Options>
    if CommandLine.Parser.Default.ParseArguments(argv, result) then
        TestRunner.RunTests(result)

    printfn "%A" argv
    0 // return an integer exit code
