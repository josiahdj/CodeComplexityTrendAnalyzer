namespace CodeComplexityTrendAnalyzer

type MemberType = | Constructor | Method | Property
type MemberInfo = { Name : string; Type : MemberType; LineCount : int; Complexity : ComplexityStats }
type MemberRevision = { Revision: RevisionInfo; Member: MemberInfo; LinesAdded: int; LinesRemoved: int }

module MemberAnalysis = 
    open System
    open Microsoft.CodeAnalysis.Text
    open Microsoft.CodeAnalysis.CSharp
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
        let getFileChangesAtRev' ((revBefore, revAfter) : RevisionInfo * RevisionInfo) =
            let diffs = getFileChangesAtRevMemoized revBefore.Hash revAfter.Hash
            revBefore, revAfter, diffs

        let getFileAtRevMemoized = memoize (Git.getFileAtRev git file)
        let getFileAtRev' ((revBefore, revAfter, diffs) : RevisionInfo * RevisionInfo * DiffChange list) =
            let codeBefore = getFileAtRevMemoized revBefore.Hash |> String.toLines
            //dumpToFile (sprintf "%s-Before-%s.cs" file revBefore.Hash) (Strings.splitLines codeBefore)
            let codeAfter = getFileAtRevMemoized revAfter.Hash |> String.toLines
            //dumpToFile (sprintf "%s-After-%s.cs" file revAfter.Hash) (Strings.splitLines codeAfter)
            revBefore, diffs, codeBefore, codeAfter

        let distinctMemberInfos (ms : MemberInfo list) = ms |> List.distinctBy (fun m -> m.Name,m.Type)

        let getMemberInfos (astBefore : SyntaxTree) (astAfter : SyntaxTree)  (diff : DiffChange) =
            let tryGetMemberInfo (ast : SyntaxTree) lineNumber =
                let lines = ast.GetText().Lines
                if lines.Count > lineNumber then
                    let span = lines.[lineNumber].Span;
                    let members = ast.GetRoot().DescendantNodes().OfType<MemberDeclarationSyntax>().Where(fun x -> x.Span.IntersectsWith(span))

                    let toMemberInfo (n : MemberDeclarationSyntax) =
                        match n with 
                        | :? PropertyDeclarationSyntax as p -> 
                            logger.Debug (sprintf "Property: %O" p.Identifier)
                            let spanText = p.ToString() |> Strings.splitLines
                            Some { Name = p.Identifier.ToString(); Type = Property; LineCount = spanText.Length; Complexity = ComplexityStats(spanText) }
                        | :? MethodDeclarationSyntax as m -> 
                            logger.Debug (sprintf "Method: %O" m.Identifier)
                            let spanText = m.ToString() |> Strings.splitLines
                            Some { Name = m.Identifier.ToString(); Type = Method; LineCount = spanText.Length; Complexity = ComplexityStats(spanText) }
                        | :? ConstructorDeclarationSyntax as c -> 
                            logger.Debug (sprintf "Constructor: %O" c.Identifier)
                            let spanText = c.ToString() |> Strings.splitLines
                            Some { Name = c.Identifier.ToString(); Type = Constructor; LineCount = spanText.Length; Complexity = ComplexityStats(spanText) }
                        | _ -> None

                    if members.Any() then
                        members 
                        |> Seq.map toMemberInfo 
                        |> Seq.choose id
                        |> Seq.tryHead
                    else None
                else None

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
            
            let ms = diff.DiffHunk.LineChanges |> getMemberInfos' [] |> List.choose id |> distinctMemberInfos
            diff, ms


        let getMemberRevisions ((rev, diffs, codeBefore, codeAfter) : RevisionInfo * DiffChange list * string * string) =
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let astBefore = parseCode codeBefore
            let astAfter = parseCode codeAfter

            diffs 
            |> List.map (getMemberInfos astBefore astAfter)
            |> List.map (fun (d, ms) -> d, ms |> distinctMemberInfos)
            |> List.map (fun (d, ms) -> ms |> List.map (fun m -> { Revision = rev; Member = m; LinesAdded = d.DiffHunk.LinesAdded; LinesRemoved = d.DiffHunk.LinesRemoved }))
            |> List.collect id
            |> tee (fun ms -> logger.Information("Found {MemberCount} members in Revision {Revision} by {Author} on {Date}", ms.Length, rev.Hash, rev.Author, rev.Date))
 
        let asCsv (methodRev : MemberRevision) =
            let revision = methodRev.Revision
            let methodInfo = methodRev.Member

            sprintf "%s,%s,%s,%s,%A,%i,%i,%.2f,%i,%i"
                revision.Hash 
                revision.Date 
                revision.Author 
                methodInfo.Name 
                methodInfo.Type
                methodInfo.LineCount
                methodInfo.Complexity.Total
                methodInfo.Complexity.Mean
                methodRev.LinesAdded 
                methodRev.LinesRemoved
             
        seq {
            yield sprintf "hash,date,author,member,type,lines,tot_complex,avg_complex,added,removed"
            yield! Git.revs git file
                    |> List.pairwise // NOTE, unless there is some caching, this will do double the work unnecessarily
                    |> List.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMemberRevisions)
                    |> List.collect id
                    |> List.map asCsv
        }
