namespace CodeComplexityTrendAnalyzer

type FileAtCommit = {
    Commit: CommitInfo

}
type FileComplexity = {
        Commit: CommitInfo
        Complexity: ComplexityStats
    }

module FileComplexityAnalysis = 
    let getFileAtRev git file (commit : CommitInfo) =
        Git.getFileAtRevMemoized git file commit

    let toFileComplexity (commit, code) = 
        let complexity = ComplexityStats.create code
        { Commit = commit
          Complexity = complexity }

    let asCsv fcs =
        let asCsv' fc =
            let commit = fc.Commit
            let complexity = fc.Complexity
            sprintf "%s,%s,%s,%i,%i,%.2f,%.2f" 
                commit.Hash 
                commit.Date 
                commit.Author 
                complexity.Count 
                complexity.Total 
                complexity.Mean 
                complexity.StdDev

        [
            yield sprintf "hash,date,author,num_lines,total_complex,avg_complex,sd"
            yield! fcs |> List.map asCsv'
        ]
        
