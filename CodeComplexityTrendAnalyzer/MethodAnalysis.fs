namespace CodeComplexityTrendAnalyzer

module MemberAnalysis =
    open Microsoft.CodeAnalysis.CSharp
    open Microsoft.CodeAnalysis.Text
    open System.Linq
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Microsoft.CodeAnalysis
    open Fake.Core
    open CodeComplexityTrendAnalyzer.DomainTypes

    let getDiffsBetweenCommits git file (commitPair: CommitPair): CommitDiffs =
        let diffs =
            Git.getFileChangesBetweenCommitsMemoized git file commitPair.Previous commitPair.Current

        commitPair, diffs

    let getFilesForCommits git file (commitDiffs: CommitDiffs): CodeToDiff =
        let commitPair, diffs = commitDiffs
        let toLines (_, ls) = String.toLines ls

        let prevCode =
            Git.getFileAtRevMemoized git file commitPair.Previous
            |> toLines
        //dumpToFile $"%s{file}-Before-%s{commitPair.Previous.Hash}.cs" (Strings.splitLines prevCode)
        let currCode =
            Git.getFileAtRevMemoized git file commitPair.Current
            |> toLines
        //dumpToFile $"%s{file}-After-%s{commitPair.Current.Hash}.cs" (Strings.splitLines currCode)
        { Commit = commitPair.Current
          FileRevisions = diffs
          PreviousCode = prevCode
          CurrentCode = currCode }

    let getMemberChangeData (revs: CodeToDiff) =
        let distinctMemberInfos (ms: CompleteMemberInfo list) =
            ms
            |> List.distinctBy (fun m -> $"%s{m.Type.ToString()}-%s{m.Name}")

        let parameters (pl: ParameterListSyntax) =
            pl.Parameters.ToList()
            |> Seq.map (fun p -> p.ToString())
            |> Seq.sort
            |> String.concat ","

        let getMemberInfos (prevAst: SyntaxTree) (currAst: SyntaxTree) (diff: FileRevision) =
            let tryGetMemberInfo (ast: SyntaxTree) lineNumber =
                let lines = ast.GetText().Lines

                if lineNumber <= lines.Count then
                    let toMemberInfo (n: MemberDeclarationSyntax) =
                        match n with
                        | :? PropertyDeclarationSyntax as p ->
                            logger.Debug("Property: @{Identifier}", p.Identifier)

                            Some
                                { Name = p.Identifier.ToString()
                                  Type = Property
                                  Parameters = None }
                        | :? MethodDeclarationSyntax as m ->
                            let ps = parameters m.ParameterList
                            logger.Debug("Method: @{Identifier}, Parameters: {Parameters}", m.Identifier, ps)

                            Some
                                { Name = m.Identifier.ToString()
                                  Type = Method
                                  Parameters = Some ps }
                        | :? ConstructorDeclarationSyntax as c ->
                            let ps = parameters c.ParameterList
                            logger.Debug("Constructor: @{Identifier}, Parameters: {Parameters}", c.Identifier, ps)

                            Some
                                { Name = c.Identifier.ToString()
                                  Type = Constructor
                                  Parameters = Some ps }
                        | _ ->
                            logger.Warning(
                                "This member declaration syntax should have been prevented: {MemberKind}",
                                n.Kind()
                            )

                            None // should (can) be prevented by the Where predicate

                    let span = lines[lineNumber-1].Span

                    let members =
                        ast
                            .GetRoot()
                            .DescendantNodes()
                            .OfType<MemberDeclarationSyntax>()
                            .Where(fun x ->
                                x.Span.IntersectsWith(span)
                                && (x :? PropertyDeclarationSyntax
                                    || x :? MethodDeclarationSyntax
                                    || x :? ConstructorDeclarationSyntax))

                    if members.Any() then
                        members
                        |> Seq.map toMemberInfo
                        |> Seq.choose id
                        |> Seq.tryHead
                    else
                        None
                else
                    logger.Error(
                        "The given line number {LineNumber} was greater than the number of lines available {LineCount}!",
                        lineNumber,
                        lines.Count
                    )

                    None // shouldn't ever happen

            let getSource op =
                match op with
                | AddLine -> currAst
                | RemoveLine -> prevAst
                | LeaveLine -> currAst

            let rec getMemberInfos' ms lcs =
                match lcs with
                | [] -> ms
                | [ lineChange ] ->
                    let lineNumber =
                        LineChange.toAbsolutePosition diff.DiffHunk lineChange

                    let ast = getSource lineChange.Operation

                    match tryGetMemberInfo ast lineNumber with
                    | Some mi -> ms |> Set.add mi
                    | _ -> ms
                | lineChange :: rest ->
                    let lineNumber =
                        LineChange.toAbsolutePosition diff.DiffHunk lineChange

                    let ast = getSource lineChange.Operation

                    match tryGetMemberInfo ast lineNumber with
                    | Some mi -> Set.add mi (getMemberInfos' ms rest)
                    | _ -> getMemberInfos' ms rest

            let getMemberStats (ast: SyntaxTree) (mem: InitialMemberInfo) =
                let lines = mem.ToLinesOfCode(ast)

                match lines with
                | Ok ls ->
                    let stats = ls |> ComplexityStats.create |> Ok

                    { Name = mem.Name
                      Type = mem.Type
                      Parameters = mem.Parameters
                      Complexity = stats
                      LineCount = Some ls.Length }
                | Error err ->
                    { Name = mem.Name
                      Type = mem.Type
                      Parameters = mem.Parameters
                      Complexity = Error err
                      LineCount = None }

            let ms =
                getMemberInfos' Set.empty diff.DiffHunk.LineChanges
                |> Set.toList
                |> List.map (getMemberStats currAst)
            // |> List.filter (fun m -> Result.isOk m.Complexity)

            diff, ms

        let getMemberRevisions (toDiff: CodeToDiff) =
            let { Commit = rev
                  FileRevisions = diffs
                  PreviousCode = prevCode
                  CurrentCode = currCode } =
                toDiff

            let parseCode (code: string) =
                let st = SourceText.From(code)
                SyntaxFactory.ParseSyntaxTree(st)

            let astBefore = parseCode prevCode
            let astAfter = parseCode currCode

            let getMemberInfos' = getMemberInfos astBefore astAfter

            let toRevisionInfos (d, ms) =
                ms
                |> distinctMemberInfos
                |> List.map
                    (fun ms' ->
                        { Commit = rev
                          Member = ms'
                          LinesAdded = d.DiffHunk.LinesAdded
                          LinesRemoved = d.DiffHunk.LinesRemoved })

            diffs
            |> List.collect (getMemberInfos' >> toRevisionInfos)
            |> tee
                (fun ms ->
                    logger.Information(
                        "Found {MemberCount} members in Revision {Revision} by {Author} on {Date}",
                        ms.Length,
                        rev.Hash,
                        rev.Author,
                        rev.Date
                    ))

        revs |> getMemberRevisions

    let asCsv (mrs: MemberRevision list) =
        [ yield MemberRevision.Header
          yield! mrs |> List.map (fun mr -> mr.Row) ]
