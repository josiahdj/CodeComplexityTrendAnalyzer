namespace CodeComplexityTrendAnalyzer

module FileComplexityAnalysis =
    open CodeComplexityTrendAnalyzer.DomainTypes
    
    let toFileComplexity (commit, code) =
        let complexity = ComplexityStats.create code

        { Commit = commit
          Complexity = complexity }

    let asCsv (fcs: FileComplexity list) =
        [ yield FileComplexity.Header
          yield! fcs |> List.map (fun fc -> fc.Row) ]
