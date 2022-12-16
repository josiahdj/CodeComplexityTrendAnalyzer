namespace CodeComplexityTrendAnalyzer

open CodeComplexityTrendAnalyzer.DomainTypes

type MemberRevision =
    { Commit: CommitInfo
      Member: CompleteMemberInfo
      LinesAdded: int
      LinesRemoved: int }
    static member Header =
        "hash,date,author,kind,member,loc,complex_tot,complex_avg,loc_added,loc_removed"

    member this.Row =
        let commit = this.Commit
        let memberInfo = this.Member

        let complexity =
            this.Member.Complexity
            |> Result.bimap id (fun e -> ComplexityStats.create List.empty)

        sprintf
            "%s,%s,%s,%A,%s,%i,%i,%.2f,%i,%i"
            commit.Hash
            (commit.Date |> DateTime.optionToString)
            commit.Author
            memberInfo.Type
            memberInfo.Name
            (memberInfo.LineCount |> Option.defaultValue 0)
            complexity.Total
            complexity.Mean
            this.LinesAdded
            this.LinesRemoved