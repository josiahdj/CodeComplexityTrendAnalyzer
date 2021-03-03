namespace CodeComplexityTrendAnalyzer

open Argu
open Microsoft.CodeAnalysis
open System.Linq
open Microsoft.CodeAnalysis.CSharp.Syntax
open System

// Git
type CommitInfo =
    { Hash: string
      Date: DateTime option
      Author: string }

type LineChangeOperation =
    | LeaveLine
    | AddLine
    | RemoveLine

type LineChange =
    { Operation: LineChangeOperation
      LineNumber: int
      Text: string }

type DiffHunk =
    { BeforeLine: int option
      BeforeLineCount: int option
      AfterLine: int option
      AfterLineCount: int option
      MemberName: string option
      LineChanges: LineChange list
      LinesAdded: int
      LinesRemoved: int }

type FileRevision =
    { File: string
      Hash: string
      DiffHunk: DiffHunk }

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
type ComplexityStats =
    private
        { Count: int
          Total: int
          Min: int
          Max: int
          StdDev: float
          Mean: float }


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

module ComplexityStats =
    let private zero =
        { Count = 0
          Total = 0
          Min = 0
          Max = 0
          Mean = 0.0
          StdDev = 0.0 }

    let create (lines: string list) =
        match lines with
        | [] -> zero
        | _ ->
            let lineComplexity (line: string) =
                // count indentations as a proxy for complexity; see:
                // http://softwareprocess.es/static/WhiteSpace.html
                // https://empear.com/blog/bumpy-road-code-complexity-in-context/
                (Strings.countLeadingSpaces line) / 4
                + (Strings.countLeadingTabs line)

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
                let variance (v: int) = (float v - mean) ** 2.0
                let avgVariance sum = sum / float safeDen

                values
                |> List.sumBy variance
                |> avgVariance
                |> sqrt

            { Count = count
              Total = total
              Min = min
              Max = max
              Mean = mean
              StdDev = stdDev }

// MethodAnalysis
type FindDeclarationError =
    | PropertyNotFoundByName
    | MethodNotFoundByName of MethodDeclarationSyntax seq
    | MethodNotFoundWithParams of MethodDeclarationSyntax seq
    | ConstructorNotFoundByName of ConstructorDeclarationSyntax seq
    | ConstructorNotFoundWithParams of ConstructorDeclarationSyntax seq

module FindDeclarationError =
    let toMethodDeclarations err =
        match err with
        | PropertyNotFoundByName -> Seq.empty
        | MethodNotFoundByName ms
        | MethodNotFoundWithParams ms -> ms
        | ConstructorNotFoundByName _
        | ConstructorNotFoundWithParams _ -> Seq.empty

    let toConstructorDeclarations err =
        match err with
        | PropertyNotFoundByName -> Seq.empty
        | MethodNotFoundByName _
        | MethodNotFoundWithParams _ -> Seq.empty
        | ConstructorNotFoundByName cs
        | ConstructorNotFoundWithParams cs -> cs

type MemberType =
    | Constructor
    | Method
    | Property

[<StructuralComparison>]
[<StructuralEquality>]
type ComplexityError =
    | Unexpected of string
    | MemberNotFound of memberName: string * memberType: MemberType * wantedParams: string * hadParams: string list

type InitialMemberInfo =
    { Name: string
      Type: MemberType
      Parameters: string option }
    member private __.ParametersToCompare(pl: ParameterListSyntax) =
        pl.Parameters.ToList()
        |> Seq.map (fun p -> p.ToString())
        |> Seq.sort
        |> String.concat ","

    member this.Params
        with private get () =
            this.Parameters
            |> Option.defaultValue String.Empty

    member this.FindPropertyDeclaration(ast: SyntaxTree): Result<PropertyDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<PropertyDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        ms
        |> Seq.tryExactlyOne
        |> Result.ofOption PropertyNotFoundByName

    member this.FindMethodDeclaration(ast: SyntaxTree): Result<MethodDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        if ms.Any() then
            ms
            |> Seq.tryFind (fun x -> (this.ParametersToCompare x.ParameterList) = this.Params)
            |> Option.orElseWith (fun () -> ms |> Seq.tryExactlyOne) // assume that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
            |> Result.ofOption (MethodNotFoundWithParams ms)
        else
            Error(MethodNotFoundByName ms)

    member this.FindConstructorDeclaration
        (ast: SyntaxTree)
        : Result<ConstructorDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<ConstructorDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        if ms.Any() then
            ms
            |> Seq.tryFind (fun x -> (this.ParametersToCompare x.ParameterList) = this.Params)
            |> Option.orElseWith (fun () -> ms |> Seq.tryExactlyOne) // assume that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
            |> Result.ofOption (ConstructorNotFoundWithParams ms)
        else
            Error(ConstructorNotFoundByName ms)

    member this.ToLinesOfCode(ast: SyntaxTree): Result<string list, ComplexityError> =
        try
            match this.Type with
            | Property ->
                result {
                    let! p =
                        this.FindPropertyDeclaration(ast)
                        |> Result.mapError (fun err -> MemberNotFound(this.Name, this.Type, String.Empty, List.empty))

                    return
                        p
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                }
            | Method ->
                result {
                    let! m =
                        this.FindMethodDeclaration(ast)
                        |> Result.mapError
                            (fun err ->
                                let ms =
                                    FindDeclarationError.toMethodDeclarations err

                                let meths =
                                    ms
                                    |> Seq.map (fun m -> m.ParameterList.Parameters.ToString())
                                    |> List.ofSeq

                                MemberNotFound(this.Name, this.Type, this.Params, meths))

                    return
                        m
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                //    logger.Debug("Couldn't find Method {Identifier} {Parameters}. Hoping for single method (no overloads)... Options:\r\n{Constructors}", mem.Name, params', meths)
                }
            | Constructor ->
                result {
                    let! c =
                        this.FindConstructorDeclaration(ast)
                        |> Result.mapError
                            (fun err ->
                                let ms =
                                    FindDeclarationError.toConstructorDeclarations err

                                let meths =
                                    ms
                                    |> Seq.map (fun m -> m.ParameterList.Parameters.ToString())
                                    |> List.ofSeq

                                MemberNotFound(this.Name, this.Type, this.Params, meths))

                    return
                        c
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                //    logger.Debug("Couldn't find Constructor {Identifier} ({Parameters}). Hoping for for single constructor (no overloads)... Options:\r\n{Constructors}", mem.Name, params', ctors)
                }
        with ex ->
            // logger.Error(ex, "Couldn't find the {MemberKind} {Identifier} {Parameters} in the AST", mem.Type, mem.Name, params')
            Error(Unexpected(ex.ToString()))

[<StructuralComparison>]
[<StructuralEquality>]
type CompleteMemberInfo =
    { Name: string
      Type: MemberType
      Parameters: string option
      LineCount: int option
      Complexity: Result<ComplexityStats, ComplexityError> }

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

type CommitPair =
    { Previous: CommitInfo
      Current: CommitInfo }

module CommitPair =
    let ofTuple (prev, curr) = { Previous = prev; Current = curr }

type CommitDiffs = CommitPair * FileRevision list

type CodeToDiff =
    { Commit: CommitInfo
      FileRevisions: FileRevision list
      PreviousCode: string
      CurrentCode: string }

// CLI
type CliCommand =
    | Members
    | File
    | All

type CliArguments =
    | [<MainCommand; ExactlyOnce; First>] Command of command: CliCommand
    | [<Mandatory; AltCommandLine("-r")>] RepositoryPath of repo: string
    | [<Mandatory; AltCommandLine("-s")>] SourceFile of file: string
    | [<AltCommandLine("-o")>] OutputFile
    | [<AltCommandLine("-d")>] StartDate of string
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Command _ ->
                "Choose 'members' to return the member-level (constructors, methods, properties) file analysis; choose 'file' to return the file-level complexity growth analysis; choose 'all' to run both."
            | RepositoryPath _ -> "Specify a repository path, e.g. C:\repo_root_dir"
            | SourceFile _ ->
                "Specify a source file to be analyzed. The path is relalive to the repository directory, e.g. path/to/file.ext"
            | OutputFile _ ->
                "Output results to a file. The analysis files will be named after the source file. The binary's directory will be used."
            | StartDate _ -> "Date after which to analyze"
