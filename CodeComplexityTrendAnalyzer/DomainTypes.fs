namespace CodeComplexityTrendAnalyzer

open Argu

// Git
type CommitInfo = { Hash: string; Date: string; Author: string }
type LineChangeOperation = | LeaveLine | AddLine | RemoveLine
type LineChange = { Operation: LineChangeOperation; LineNumber: int; Text: string }

type DiffHunk = { 
    BeforeLine: int option
    BeforeLineCount: int option
    AfterLine: int option
    AfterLineCount: int option
    MemberName: string option
    LineChanges: LineChange list
    LinesAdded: int
    LinesRemoved: int }
type FileRevision = { File: string; Hash: string; DiffHunk: DiffHunk }

[<RequireQualifiedAccess>]
module LineChange =
    let toAbsolutePosition diff lineChange =
        let startLineForOp op =
            match op with
            | AddLine -> diff.AfterLine 
            | RemoveLine -> diff.BeforeLine 
            | LeaveLine -> diff.AfterLine 
            |> Option.defaultValue 0
        
        let startLine = startLineForOp lineChange.Operation
        startLine + lineChange.LineNumber - 1


// ComplexityStats
type ComplexityStats = private {
    Count : int
    Total : int
    Min : int
    Max : int
    StdDev : float
    Mean : float
}


// FileComplexityAnalysis
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

module ComplexityStats =
    let create (lines : string list) =
        let lineComplexity (line : string) =
            // count indentations as a proxy for complexity; see: 
            // http://softwareprocess.es/static/WhiteSpace.html
            // https://empear.com/blog/bumpy-road-code-complexity-in-context/
            (Strings.countLeadingSpaces line)/4 + (Strings.countLeadingTabs line)

        let values =
            lines
            |> List.filter Strings.containsCode 
            |> List.map lineComplexity

        let count = List.length values
        let total = List.sum values
        let max = List.max values
        let min = List.min values
        let safeDen = if count = 0 then 1 else count
        let mean = float total / float safeDen
        let stdDev = 
           let variance (v : int) =
               (float v - mean) ** 2.0
           let avgVariance sum =
               sum / float safeDen
           values |> List.sumBy variance |> avgVariance |> sqrt

        { Count = count; Total = total; Min = min; Max = max; Mean = mean; StdDev = stdDev }


// MethodAnalysis
type MemberType = | Constructor | Method | Property
type MemberInfo = { Name: string; Type: MemberType; Parameters: string option; LineCount: int; Complexity: ComplexityStats option }
type MemberRevision = { Commit: CommitInfo; Member: MemberInfo; LinesAdded: int; LinesRemoved: int } // TODO: Move LineCount & Complexity from MemberInfo to MemberRevision? Unless MemberInfo is a "snapshot", but maybe we should still have a unique "Member" entity?
    with 
        static member Header = "hash,date,author,kind,member,loc,complex_tot,complex_avg,loc_added,loc_removed"
        member this.Row = 
            let commit = this.Commit
            let memberInfo = this.Member
            let complexity = this.Member.Complexity |> Option.defaultWith (fun () -> ComplexityStats.create [])

            sprintf "%s,%s,%s,%A,%s,%i,%i,%.2f,%i,%i"
                commit.Hash 
                commit.Date 
                commit.Author 
                memberInfo.Type
                memberInfo.Name 
                memberInfo.LineCount
                complexity.Total
                complexity.Mean
                this.LinesAdded 
                this.LinesRemoved

type CommitPair = { Previous: CommitInfo; Current: CommitInfo }
module CommitPair =
    let ofTuple(prev, curr) = 
        { Previous = prev; Current = curr }

type CommitDiffs = CommitPair * FileRevision list

type CodeToDiff = { Commit: CommitInfo; FileRevisions: FileRevision list; PreviousCode: string; CurrentCode: string }

// CLI
type CliCommand =
    | Members
    | File
    | All
type CliArguments = 
    | [<MainCommand; ExactlyOnce; First>] Command of command : CliCommand
    | [<Mandatory; AltCommandLine("-r")>] RepositoryPath of repo : string
    | [<Mandatory; AltCommandLine("-s")>] SourceFile of file : string
    | [<AltCommandLine("-o")>] OutputFile
with 
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Command _ -> "Choose 'members' to return the member-level (constructors, methods, properties) file analysis; choose 'file' to return the file-level complexity growth analysis; choose 'all' to run both."
            | RepositoryPath _ -> "Specify a repository path, e.g. C:\repo_root_dir"
            | SourceFile _ -> "Specify a source file to be analyzed. The path is relalive to the repository directory, e.g. path/to/file.ext"
            | OutputFile _ -> "Output results to a file. The analysis files will be named after the source file. The binary's directory will be used."

