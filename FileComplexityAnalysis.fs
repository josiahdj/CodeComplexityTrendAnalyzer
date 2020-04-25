namespace CodeComplexityTrendAnalyzer

type FileAtCommit = {
    Commit: CommitInfo

}
type FileComplexity = {
        Commit: CommitInfo
        Complexity: ComplexityStats
    }
    with
        static member Header = "hash,date,author,num_lines,total_complex,avg_complex,sd"
        member this.Row = 
            let commit = this.Commit
            let complexity = this.Complexity
            sprintf "%s,%s,%s,%i,%i,%.2f,%.2f" 
                commit.Hash 
                commit.Date 
                commit.Author 
                complexity.Count 
                complexity.Total 
                complexity.Mean 
                complexity.StdDev

module FileComplexityAnalysis = 
    let toFileComplexity (commit, code) = 
        let complexity = ComplexityStats.create code
        { Commit = commit
          Complexity = complexity }

    let asCsv (fcs : FileComplexity list) =
        [
            FileComplexity.Header
            yield! fcs |> List.map (fun fc -> fc.Row)
        ]
        
