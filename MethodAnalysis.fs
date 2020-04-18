namespace CodeComplexityTrendAnalyzer

open Microsoft.CodeAnalysis.CSharp

type MemberType = | Constructor | Method | Property
type MemberInfo = { Name: string; Type: SyntaxKind; Parameters: string option; LineCount: int; Complexity: ComplexityStats option }
type MemberRevision = { Commit: CommitInfo; Member: MemberInfo; LinesAdded: int; LinesRemoved: int }

module MemberAnalysis = 
    open System
    open Microsoft.CodeAnalysis.Text
    open System.Linq
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Microsoft.CodeAnalysis
    open Fake.Core
    
    let analyze git file =
        let parseRevPair (prev, curr) =
            Git.parseRev prev, Git.parseRev curr

        let memoize f =
            let cache = ref Map.empty
            fun x ->
                match (!cache).TryFind(x) with
                | Some res -> res
                | None ->
                    let res = f x
                    cache := (!cache).Add(x,res)
                    res

        let getFileChangesAtRevMemoized = memoize (Git.getFileChangesAtRev git file)
        let getFileChangesAtRev' ((revBefore, revAfter) : CommitInfo * CommitInfo) =
            let diffs = getFileChangesAtRevMemoized revBefore.Hash revAfter.Hash
            revBefore, revAfter, diffs

        let getFileAtRevMemoized = memoize (Git.getFileAtRev git file)
        let getFileAtRev' ((revBefore, revAfter, diffs) : CommitInfo * CommitInfo * FileRevision list) =
            let codeBefore = getFileAtRevMemoized revBefore.Hash |> String.toLines
            //dumpToFile (sprintf "%s-Before-%s.cs" file revBefore.Hash) (Strings.splitLines codeBefore)
            let codeAfter = getFileAtRevMemoized revAfter.Hash |> String.toLines
            //dumpToFile (sprintf "%s-After-%s.cs" file revAfter.Hash) (Strings.splitLines codeAfter)
            revAfter, diffs, codeBefore, codeAfter

        let distinctMemberInfos (ms : MemberInfo list) = ms |> List.distinctBy (fun m -> sprintf "%s-%s" (m.Type.ToString()) m.Name)
        let parameters (pl : ParameterListSyntax) = pl.Parameters.ToList() |> Seq.map (fun p -> p.ToString()) |> Seq.sort |> String.concat ","

        let getMemberInfos (astBefore : SyntaxTree) (astAfter : SyntaxTree)  (diff : FileRevision) =
            let tryGetMemberInfo (ast : SyntaxTree) lineNumber =
                let lines = ast.GetText().Lines
                if lineNumber < lines.Count then
                    let toMemberInfo (n : MemberDeclarationSyntax) =
                        match n with 
                        | :? PropertyDeclarationSyntax as p -> 
                            logger.Debug("Property: @{Identifier}", p.Identifier)
                            Some { Name = p.Identifier.ToString(); Type = p.Kind(); Parameters = None; LineCount = 0; Complexity = None; }
                        | :? MethodDeclarationSyntax as m -> 
                            let ps = parameters m.ParameterList
                            logger.Debug("Method: @{Identifier}, Parameters: {Parameters}", m.Identifier, ps)
                            Some { Name = m.Identifier.ToString(); Type = m.Kind(); Parameters = Some ps; LineCount = 0; Complexity = None; }
                        | :? ConstructorDeclarationSyntax as c -> 
                            let ps = parameters c.ParameterList
                            logger.Debug("Constructor: @{Identifier}, Parameters: {Parameters}", c.Identifier, ps)
                            Some { Name = c.Identifier.ToString(); Type = c.Kind(); Parameters = Some ps; LineCount = 0; Complexity = None; }
                        | _ -> 
                            logger.Warning("This member declaration syntax should have been prevented: {MemberKind}", n.Kind())
                            None // should (can) be prevented by the Where predicate

                    let span = lines.[lineNumber].Span;
                    let members = ast.GetRoot().DescendantNodes().OfType<MemberDeclarationSyntax>().Where(fun x -> x.Span.IntersectsWith(span) && (x :? PropertyDeclarationSyntax || x :? MethodDeclarationSyntax || x :? ConstructorDeclarationSyntax))
                    if members.Any() then
                        members 
                        |> Seq.map toMemberInfo 
                        |> Seq.choose id
                        |> Seq.tryHead
                    else None
                else 
                    logger.Error("The given line number {LineNumber} was greater than the number of lines available {LineCount}!", lineNumber, lines.Count)
                    None // shouldn't ever happen

            let getSource op = 
                match op with 
                | AddLine -> astAfter
                | RemoveLine -> astBefore
                | LeaveLine -> astAfter

            let rec getMemberInfos' ms lcs =
                match lcs with
                    | [] -> ms
                    | [lineChange] -> 
                            let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                            let ast = getSource lineChange.Operation
                            tryGetMemberInfo ast lineNumber :: ms
                    | lineChange::rest -> 
                            let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                            let ast = getSource lineChange.Operation
                            tryGetMemberInfo ast lineNumber :: getMemberInfos' ms rest
            
            let getMemberStats (ast : SyntaxTree) (mem : MemberInfo) =
                let lines = 
                    let ps =  mem.Parameters |> Option.defaultValue String.Empty
                    try
                        match mem.Type with 
                        | SyntaxKind.PropertyDeclaration -> 
                            let p = ast.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single(fun x -> x.Identifier.Text = mem.Name)
                            p.ToString() |> Strings.splitLines
                        | SyntaxKind.MethodDeclaration ->
                            let m = ast.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(fun x -> x.Identifier.Text = mem.Name && (parameters x.ParameterList) = ps)
                            m.ToString() |> Strings.splitLines
                        | SyntaxKind.ConstructorDeclaration ->
                            let c = ast.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single(fun x -> x.Identifier.Text = mem.Name && (parameters x.ParameterList) = ps)
                            c.ToString() |> Strings.splitLines
                        | _ -> 
                            logger.Error("Was given a member info {Identifier} with an unrecognized MemberKind {MemberKind}", mem.Name, mem.Type)
                            []
                    with 
                    | :? InvalidOperationException as ex -> 
                        logger.Error(ex, "Couldn't find the {MemberKind} {Identifier}{Parameters} in the AST", mem.Type, mem.Name, ps)
                        []
                let stats = lines |> ComplexityStats |> Some
                { mem with Complexity = stats; LineCount = lines.Length }

            let ms = 
                //diff.DiffHunk.LineChanges 
                //|> getMemberInfos' [] 
                //|> List.choose id 
                //|> distinctMemberInfos 
                //|> List.map (getMemberStats astAfter)
                let mis = getMemberInfos' [] diff.DiffHunk.LineChanges 
                let misNoOptions = List.choose id mis
                let misDisctinct = distinctMemberInfos misNoOptions
                let misWithStats = List.map (getMemberStats astAfter) misDisctinct
                misWithStats

            diff, ms


        let getMemberRevisions ((rev, diffs, codeBefore, codeAfter) : CommitInfo * FileRevision list * string * string) =
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let astBefore = parseCode codeBefore
            let astAfter = parseCode codeAfter

            let getMemberInfos' = getMemberInfos astBefore astAfter
            let toRevisionInfos (d, ms) = 
                ms 
                |> distinctMemberInfos 
                |> List.map (fun m -> { Commit = rev; Member = m; LinesAdded = d.DiffHunk.LinesAdded; LinesRemoved = d.DiffHunk.LinesRemoved })

            diffs 
            |> List.map (getMemberInfos' >> toRevisionInfos)
            |> List.collect id
            |> tee (fun ms -> logger.Information("Found {MemberCount} members in Revision {Revision} by {Author} on {Date}", ms.Length, rev.Hash, rev.Author, rev.Date))
 
        let asCsv (methodRev : MemberRevision) =
            let revision = methodRev.Commit
            let methodInfo = methodRev.Member
            let complexity = methodRev.Member.Complexity |> Option.defaultWith (fun () -> ComplexityStats([]))

            sprintf "%s,%s,%s,%A,%s,%i,%i,%.2f,%i,%i"
                revision.Hash 
                revision.Date 
                revision.Author 
                methodInfo.Type
                methodInfo.Name 
                methodInfo.LineCount
                complexity.Total
                complexity.Mean
                methodRev.LinesAdded 
                methodRev.LinesRemoved
             
        seq {
            yield sprintf "hash,date,author,kind,member,loc,complex_tot,complex_avg,loc_added,loc_removed"
            yield! Git.revs git file
                    |> List.pairwise // NOTE, unless there is some caching, this will do double the work unnecessarily
                    |> List.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMemberRevisions)
                    |> List.collect id
                    |> List.map asCsv
        }
