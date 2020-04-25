namespace CodeComplexityTrendAnalyzer

type MemberType = | Constructor | Method | Property
type MemberInfo = { Name: string; Type: MemberType; Parameters: string option; LineCount: int; Complexity: ComplexityStats option }
type MemberRevision = { Commit: CommitInfo; Member: MemberInfo; LinesAdded: int; LinesRemoved: int }
type CommitPair = { Previous: CommitInfo; Current: CommitInfo }
    with
        static member ofTuple(prev, curr) = { Previous = prev; Current = curr }
type CommitDiffs = CommitPair * FileRevision list
type CodeToDiff = { Commit: CommitInfo; FileRevisions: FileRevision list; PreviousCode: string; CurrentCode: string }

module MemberAnalysis = 
    open System
    open Microsoft.CodeAnalysis.CSharp
    open Microsoft.CodeAnalysis.Text
    open System.Linq
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Microsoft.CodeAnalysis
    open Fake.Core

    let private memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some res -> res
            | None ->
                let res = f x
                cache := (!cache).Add(x,res)
                res

    let private getFileChangesAtRevMemoized git file = memoize (Git.getFileChangesAtRev git file)
    let getFileChangesAtRev git file (commitPair : CommitPair) : CommitDiffs =
        let diffs = getFileChangesAtRevMemoized git file commitPair.Previous commitPair.Current
        commitPair, diffs

    let private getFileAtRevMemoized git file = memoize (Git.getFileAtRev git file)
    let getFileAtRev git file (commitDiffs : CommitDiffs) : CodeToDiff =
        let (commitPair, diffs) = commitDiffs
        let prevCode = getFileAtRevMemoized git file commitPair.Previous.Hash |> String.toLines
        //dumpToFile (sprintf "%s-Before-%s.cs" file revBefore.Hash) (Strings.splitLines codeBefore)
        let currCode = getFileAtRevMemoized git file commitPair.Current.Hash |> String.toLines
        //dumpToFile (sprintf "%s-After-%s.cs" file revAfter.Hash) (Strings.splitLines codeAfter)
        { Commit = commitPair.Current; FileRevisions = diffs; PreviousCode = prevCode; CurrentCode = currCode }

    let getRawData (revs : CodeToDiff) =
        let distinctMemberInfos (ms : MemberInfo list) = ms |> List.distinctBy (fun m -> sprintf "%s-%s" (m.Type.ToString()) m.Name)
        let parameters (pl : ParameterListSyntax) = pl.Parameters.ToList() |> Seq.map (fun p -> p.ToString()) |> Seq.sort |> String.concat ","

        let getMemberInfos (prevAst : SyntaxTree) (currAst : SyntaxTree)  (diff : FileRevision) =
            let tryGetMemberInfo (ast : SyntaxTree) lineNumber =
                let lines = ast.GetText().Lines
                if lineNumber < lines.Count then
                    let toMemberInfo (n : MemberDeclarationSyntax) =
                        match n with 
                        | :? PropertyDeclarationSyntax as p -> 
                            logger.Debug("Property: @{Identifier}", p.Identifier)
                            Some { Name = p.Identifier.ToString(); Type = Property; Parameters = None; LineCount = 0; Complexity = None; }
                        | :? MethodDeclarationSyntax as m -> 
                            let ps = parameters m.ParameterList
                            logger.Debug("Method: @{Identifier}, Parameters: {Parameters}", m.Identifier, ps)
                            Some { Name = m.Identifier.ToString(); Type = Method; Parameters = Some ps; LineCount = 0; Complexity = None; }
                        | :? ConstructorDeclarationSyntax as c -> 
                            let ps = parameters c.ParameterList
                            logger.Debug("Constructor: @{Identifier}, Parameters: {Parameters}", c.Identifier, ps)
                            Some { Name = c.Identifier.ToString(); Type = Constructor; Parameters = Some ps; LineCount = 0; Complexity = None; }
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
                | AddLine -> currAst
                | RemoveLine -> prevAst
                | LeaveLine -> currAst

            let rec getMemberInfos' ms lcs =
                match lcs with
                    | [] -> ms
                    | [lineChange] -> 
                            let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                            let ast = getSource lineChange.Operation
                            match tryGetMemberInfo ast lineNumber with
                            | Some mi -> ms |> Set.add mi
                            | _ -> ms
                    | lineChange::rest -> 
                            let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                            let ast = getSource lineChange.Operation
                            match tryGetMemberInfo ast lineNumber with
                            | Some mi -> Set.add mi (getMemberInfos' ms rest)
                            | _ -> getMemberInfos' ms rest
            
            let getMemberStats (ast : SyntaxTree) (mem : MemberInfo) =
                let lines = 
                    let params' =  mem.Parameters |> Option.defaultValue String.Empty
                    try
                        match mem.Type with 
                        | Property -> 
                            let p = ast.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().First(fun x -> x.Identifier.Text = mem.Name)
                            p.ToString() |> Strings.splitLines |> Some
                        | Method ->
                            let ms = ast.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Where(fun x -> x.Identifier.Text = mem.Name)
                            if ms.Any() then
                                let m = ms |> Seq.tryFind (fun x -> (parameters x.ParameterList) = params')
                                match m with
                                | Some m' -> m'.ToString() |> Strings.splitLines |> Some
                                | None -> 
                                    let meths = ms |> Seq.map (fun m -> m.ParameterList.Parameters.ToString()) |> String.concat "\r\n" 
                                    logger.Debug("Couldn't find Method {Identifier}{Parameters}. Trying for single member. Options:\r\n{Constructors}", mem.Name, params', meths)
                                    ms.Single().ToString() |> Strings.splitLines |> Some // bet that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
                            else None
                        | Constructor ->
                            let cs = ast.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Where(fun x -> x.Identifier.Text = mem.Name)
                            if cs.Any() then
                                let c = cs |> Seq.tryFind (fun x -> (parameters x.ParameterList) = params')
                                match c with
                                | Some c' -> c'.ToString() |> Strings.splitLines |> Some
                                | None -> 
                                    let ctors = cs |> Seq.map (fun c -> c.ParameterList.Parameters.ToString()) |> String.concat "\r\n" 
                                    logger.Debug("Couldn't find Constructor {Identifier}{Parameters}. Trying for single member. Options:\r\n{Constructors}", mem.Name, params', ctors)
                                    cs.Single().ToString() |> Strings.splitLines |> Some // bet that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
                            else None
                    with 
                    | ex -> 
                        logger.Error(ex, "Couldn't find the {MemberKind} {Identifier}{Parameters} in the AST", mem.Type, mem.Name, params')
                        None

                match lines with
                | Some ls ->
                    let stats = ls |> ComplexityStats.create |> Some
                    { mem with Complexity = stats; LineCount = ls.Length }
                | _ -> mem

            let ms = 
                let mis = getMemberInfos' Set.empty diff.DiffHunk.LineChanges |> Set.toList
                let misWithStats = List.map (getMemberStats currAst) mis |> List.filter (fun m -> m.Complexity.IsSome)
                misWithStats

            diff, ms

        let getMemberRevisions (toDiff : CodeToDiff) =
            let { Commit = rev; FileRevisions = diffs; PreviousCode = prevCode; CurrentCode = currCode } = toDiff
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let astBefore = parseCode prevCode
            let astAfter = parseCode currCode

            let getMemberInfos' = getMemberInfos astBefore astAfter
            let toRevisionInfos (d, ms) = 
                ms 
                |> distinctMemberInfos 
                |> List.map (fun m -> { Commit = rev; Member = m; LinesAdded = d.DiffHunk.LinesAdded; LinesRemoved = d.DiffHunk.LinesRemoved })

            diffs 
            |> List.map (getMemberInfos' >> toRevisionInfos)
            |> List.collect id
            |> tee (fun ms -> logger.Information("Found {MemberCount} members in Revision {Revision} by {Author} on {Date}", ms.Length, rev.Hash, rev.Author, rev.Date))
        
        revs
        |> getMemberRevisions
            
    let asCsv (mrs : MemberRevision list) =
        let asCsv' (memberRev : MemberRevision) =
            let revision = memberRev.Commit
            let methodInfo = memberRev.Member
            let complexity = memberRev.Member.Complexity |> Option.defaultWith (fun () -> ComplexityStats.create [])

            sprintf "%s,%s,%s,%A,%s,%i,%i,%.2f,%i,%i"
                revision.Hash 
                revision.Date 
                revision.Author 
                methodInfo.Type
                methodInfo.Name 
                methodInfo.LineCount
                complexity.Total
                complexity.Mean
                memberRev.LinesAdded 
                memberRev.LinesRemoved

        seq {
            yield sprintf "hash,date,author,kind,member,loc,complex_tot,complex_avg,loc_added,loc_removed"
            yield! mrs |> Seq.map asCsv'
        }
         