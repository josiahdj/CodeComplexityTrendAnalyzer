namespace CodeComplexityTrendAnalyzer

[<RequireQualifiedAccess>]
module Git = 
    open System
    open System.Text.RegularExpressions
    open Fake.Tools.Git
    open Fake.Core
    open Logging

    /// Gets a list of results given a git repository and a git command
    let gitResult repoPath cmd = 
        CommandHelper.getGitResult repoPath cmd

    let parseRev commit =
        let parts = String.splitStr "--" commit
        { Hash = parts.[0]; Date = parts.[1]; Author = parts.[2] }
    
    /// Gets all revisions of a file from Git repository
    let revs git filePath = 
        let gitCmd = sprintf "log --date=short --pretty=format:%%h--%%ad--%%an %s" filePath
        git gitCmd 
        |> List.map parseRev
        |> List.rev

    /// Gets the source of a file at a given revision (hash) from a repository
    let getFileAtRev git file (commit : CommitInfo) : CommitInfo * string list =
        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "show %s:%s" commit.Hash theFile
        let code = git gitCmd
        commit, code

    let getFileAtRevMemoized git file = Caching.memoize (getFileAtRev git file)

    /// Gets the difference between two revisions of a file in the Unified Diff format, from a git repository
    let unifiedDiff git revBefore revAfter theFile = 
        let gitCmd = sprintf "diff %s..%s --unified=0 -- %s" revBefore revAfter theFile
        git gitCmd 

    let private lineInfoRegex = Regex("@@ -(?<before_line>[0-9]+)(,(?<before_line_count>[0-9]+))? \+(?<after_line>[0-9]+)(,(?<after_line_count>[0-9]+))?( @@) ?(?<member>.*?)$", RegexOptions.Compiled)
    let private isLineInfo s =
        let matches = lineInfoRegex.Match(s)
        let isMatch = matches.Success && matches.Groups.Count > 0
        (isMatch, matches.Groups)

    let private groupValue (name : string) (grps : GroupCollection) =
        if grps.[name].Success then
            Some grps.[name].Value
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

    let private lineChangeRegex = Regex("^(?<op>\+|\-)(?<line>[^+-].*?)$", RegexOptions.Compiled) // unfortunately (or fortunately) eliminates add/remove of empty lines
    let private isLineChange s =
        let matches = lineChangeRegex.Match(s)
        let isMatch = matches.Success && matches.Groups.Count > 0
        (isMatch, matches.Groups)

    let private toLineChange grps =
        let op = grps |> groupValue "op" |> Option.map (fun s -> match s with | "+" -> AddLine | "-" -> RemoveLine | _ -> LeaveLine) |> Option.get
        let line = grps |> groupValue "line" |> Option.defaultValue String.Empty
        { Operation = op; LineNumber = 0; Text = line }

    let private diffHeaderRegex = Regex("^(diff|index|---|\+\+\+) ", RegexOptions.Compiled)
    let private isDiffHeader s = diffHeaderRegex.IsMatch s
        
    let private (|LineInfo|LineChange|DiffHeader|UnchangedLine|) s = 
        let isInfo, grps = isLineInfo s
        if isInfo then
            LineInfo (toLineInfo grps)
        else
            let isChange, grps' = isLineChange s
            if isChange then
                LineChange (toLineChange grps')
            else 
                if isDiffHeader s then
                    DiffHeader s
                else
                    UnchangedLine s

    /// Gets a structured list of File Revisions between two revisions of a file
    let getFileChangesBetweenCommits git file (revBefore : CommitInfo) (revAfter : CommitInfo) =
        logger.Information("Parsing Hunks from {File}, Rev {RevBefore} to Rev {RevAfter} =========================================", file, revBefore.Hash, revAfter.Hash)
        let toHunks lines =
            //dumpToFile (sprintf "%s-%s--%s.diff" file revBefore revAfter) lines
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
                        | [curr] ->
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, LineChange {LineNumber} ({Operation}): {Line}", hunks.Length, lc.Operation, l, lc.Text)
                            let newCurr = updateDiffHunk curr lc [] l
                            toHunks' newCurr l
                        | curr::tail ->
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, LineChange {LineNumber} ({Operation}): {Line}", hunks.Length, lc.Operation, l, lc.Text)
                            let newCurr = updateDiffHunk curr lc tail l
                            toHunks' newCurr l
                    | DiffHeader dh -> 
                            logger.Debug("DiffHeader {DiffHeader}", dh)
                            toHunks' hunks 0
                    | UnchangedLine ul -> 
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, UnchangedLine {LineNumber}: {Line}", hunks.Length, l, ul)
                            toHunks' hunks l

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
