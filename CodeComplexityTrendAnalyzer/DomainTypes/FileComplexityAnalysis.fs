namespace CodeComplexityTrendAnalyzer.DomainTypes

open CodeComplexityTrendAnalyzer

// FileComplexityAnalysis
type FileAtCommit = { Commit: CommitInfo }

type FileComplexity =
    { Commit: CommitInfo
      Complexity: ComplexityStats }
    static member Header =
        "hash,date,author,num_lines,total_complex,avg_complex,sd"

    member this.Row =
        let commit = this.Commit
        let complexity = this.Complexity

        sprintf
            "%s,%s,%s,%i,%i,%.2f,%.2f"
            commit.Hash
            (commit.Date |> DateTime.optionToString)
            commit.Author
            complexity.Count
            complexity.Total
            complexity.Mean
            complexity.StdDev
