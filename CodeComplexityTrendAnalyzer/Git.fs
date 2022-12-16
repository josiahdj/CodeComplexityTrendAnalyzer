namespace CodeComplexityTrendAnalyzer

[<RequireQualifiedAccess>]
module Git = 
    open System
    open System.IO
    open System.Text.RegularExpressions
    open Fake.Tools.Git
    open Fake.Core
    open Logging
    open CodeComplexityTrendAnalyzer.DomainTypes

    /// Gets a list of results given a git repository and a git command
    let gitResult = CommandHelper.getGitResult 

    let parseRev commit =
        let parts = String.splitStr "--" commit
        let dateMaybe = DateTime.tryParse(parts[1])
        { Hash = parts[0]; Date = dateMaybe; Author = parts[2] }
    
    /// Gets all revisions of a file from Git repository
    let revs git filePath = 
        let gitCmd = $"log --follow --date=short --pretty=format:%%h--%%ad--%%an %s{filePath}"
        git gitCmd 
        |> List.map parseRev
        |> List.rev
        
    let filesChanged git (commit: CommitInfo) : string list =
        let gitCmd = $"show --pretty=\"format:\" --name-only %s{commit.Hash}"
        git gitCmd
        
    /// Gets the source of a file at a given revision (hash) from a repository
    let rec getFileAtRev git file (commit : CommitInfo) : CommitInfo * string list =
        let theFile = String.replace "\\" "/" file
        let gitCmd = $"show %s{commit.Hash}:%s{theFile}"
        let code =
            match git gitCmd with
            | [] ->
                let filesChangedInCommit = filesChanged git commit
                let fileName = Path.GetFileName(file)
                filesChangedInCommit
                |> List.tryFind (fun f -> f.EndsWith(fileName))
                |> Option.map (fun f -> getFileAtRev git f commit)
                |> Option.map snd 
                |> Option.defaultValue []
            | lines -> lines
        commit, code

    let getFileAtRevMemoized git file = Caching.memoize (getFileAtRev git file)

    /// Gets the difference between two revisions of a file in the Unified Diff format, from a git repository
    let unifiedDiff git revBefore revAfter theFile = 
        let gitCmd = $"diff %s{revBefore}..%s{revAfter} --unified=0 -- %s{theFile}"
        git gitCmd 

    let private groupValue (name : string) (grps : GroupCollection) =
        if grps[name].Success then
            Some grps[name].Value
        else 
            None

    let private toLineInfo grps =
        let memberName = grps |> groupValue "member"
        let beforeLine = grps |> groupValue "before_line" |> Option.map int
        let beforeLineCount = grps |> groupValue "before_line_count" |> Option.map int
        let afterLine = grps |> groupValue "after_line" |> Option.map int
        let afterLineCount = grps |> groupValue "after_line_count" |> Option.map int
        
        { BeforeLine = beforeLine
          BeforeLineCount = beforeLineCount
          AfterLine = afterLine
          AfterLineCount = afterLineCount
          MemberName = memberName
          LineChanges = []
          LinesAdded = 0
          LinesRemoved = 0}
    
    let private lineInfoRegex = Regex("@@ -(?<before_line>[0-9]+)(,(?<before_line_count>[0-9]+))? \+(?<after_line>[0-9]+)(,(?<after_line_count>[0-9]+))?( @@) ?(?<member>.*?)$", RegexOptions.Compiled)
    let private diffHeaderRegex = Regex("^(diff|index|---|\+\+\+) ", RegexOptions.Compiled)
    let private lineChangeRegex = Regex("^(?<op>\+|\-)(?<line>[^+-].*?)$", RegexOptions.Compiled) // unfortunately (or fortunately) eliminates add/remove of empty lines

    let private toLineChange grps =
        let op = grps |> groupValue "op" |> Option.map (fun s -> match s with | "+" -> AddLine | "-" -> RemoveLine | _ -> LeaveLine) |> Option.get
        let line = grps |> groupValue "line" |> Option.defaultValue String.Empty
        { Operation = op; LineNumber = 0; Text = line }

    open Regex
    let private (|LineInfo|LineChange|DiffHeader|UnchangedLine|) s = 
        match s with
        | FromRegex lineInfoRegex groups -> LineInfo(toLineInfo groups)
        | FromRegex lineChangeRegex groups -> LineChange(toLineChange groups)
        | FromRegex diffHeaderRegex _ -> DiffHeader s
        | _ -> UnchangedLine s

    /// Gets a structured list of File Revisions between two revisions of a file
    let getFileChangesBetweenCommits git file (revBefore : CommitInfo) (revAfter : CommitInfo) =
        logger.Information("Parsing Hunks from {File}, Rev {RevBefore} to Rev {RevAfter} =========================================", file, revBefore.Hash, revAfter.Hash)
        let toHunks lines =
            //dumpToFile $"%s{file}-%s{revBefore.Hash}--%s{revAfter.Hash}.diff" lines
            let rec toHunks' hunks lineNum lines' = 
                let updateDiffHunk (dh : DiffHunk) chg dhs ln =
                    let added, removed = 
                        match chg.Operation with
                        | AddLine -> dh.LinesAdded + 1, dh.LinesRemoved
                        | RemoveLine -> dh.LinesAdded, dh.LinesRemoved + 1
                        | LeaveLine -> dh.LinesAdded, dh.LinesRemoved

                    let lineChanges = { chg with LineNumber = ln }::dh.LineChanges // NOTE: line changes will be in reverse order!
                    { dh with LineChanges = lineChanges; LinesAdded = added; LinesRemoved = removed }::dhs

                let parseLine line' =
                    match line' with
                    | LineInfo li -> 
                        logger.Debug("LineInfo: @{LineInfo}", li)
                        toHunks' (li::hunks) 0
                    | LineChange lc -> 
                        match hunks with
                        | [] -> 
                            failwith "this shouldn't really happen!"
                        | curr::tail ->
                            let l = lineNum + if lc.Operation = LineChangeOperation.AddLine then 1 else 0
                            logger.Debug("Hunk #{HunkNumber}, LineChange {LineNumber} ({Operation}): {Line}", hunks.Length, lc.Operation, l, lc.Text)
                            let newCurr = updateDiffHunk curr lc tail l
                            toHunks' newCurr l
                    | DiffHeader dh -> 
                        logger.Debug("DiffHeader {DiffHeader}", dh)
                        toHunks' hunks 0
                    | UnchangedLine ul -> 
                        logger.Debug("Hunk #{HunkNumber}, UnchangedLine {LineNumber}: {Line}", hunks.Length, lineNum, ul)
                        toHunks' hunks lineNum

                match lines' with
                | [] -> hunks // The End
                | [line] -> parseLine line [] // Last Line -> The End
                | line::rest -> parseLine line rest

            toHunks' List.empty<DiffHunk> 0 lines

        let toFileChanges l =
            { File = file; 
              Hash = revBefore.Hash; 
              DiffHunk = l }

        let theFile = String.replace "\\" "/" file
        
        unifiedDiff git revBefore.Hash revAfter.Hash theFile
        |> toHunks
        |> List.map toFileChanges
        |> List.rev

    let getFileChangesBetweenCommitsMemoized git file = Caching.memoize (getFileChangesBetweenCommits git file)
