open CodeComplexityTrendAnalyzer

[<EntryPoint>]
let main argv =
    let repo = argv.[0]
    let file = argv.[1]
    
    FileComplexity.printStats repo file

    0 // return an integer exit code
