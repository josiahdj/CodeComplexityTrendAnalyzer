namespace CodeComplexityTrendAnalyzer

type MethodRevision = { Revision: RevisionInfo; Member: string; LinesAdded: int; LinesRemoved: int }

module MethodAnalysis = 
    open System
    open Microsoft.CodeAnalysis.Text
    open Microsoft.CodeAnalysis.CSharp
    open System.Linq
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Microsoft.CodeAnalysis
    open Fake.Core
    
    let getMethodInfo git file =
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

        let getMemberName (astBefore : SyntaxTree) (astAfter : SyntaxTree)  (diff : DiffChange) =
            let getMemberName' (lines : TextLineCollection) (ast : SyntaxTree) lineNumber =
                if lines.Count > lineNumber then
                    let span = lines.[lineNumber].Span;
                    let members = ast.GetRoot().DescendantNodes().OfType<MemberDeclarationSyntax>().Where(fun x -> x.Span.IntersectsWith(span))

                    let printMember (n : MemberDeclarationSyntax) =
                        match n with 
                        | :? PropertyDeclarationSyntax as p -> tee logger.Debug (sprintf "Property: %O" p.Identifier)
                        | :? MethodDeclarationSyntax as m -> tee logger.Debug (sprintf "Method: %O" m.Identifier)
                        | :? ConstructorDeclarationSyntax as c -> tee logger.Debug (sprintf "Constructor: %O" c.Identifier)
                        | _ -> String.Empty
        
                    let notEmpty s =
                        s <> String.Empty

                    if members.Any() then
                        members 
                        |> Seq.map printMember 
                        |> Seq.filter notEmpty
                        |> Seq.tryHead
                    else None
                else None

            let beforeLines = astBefore.GetText().Lines
            let afterLines = astAfter.GetText().Lines
            let getSource op = 
                match op with 
                | AddLine -> afterLines, astAfter
                | RemoveLine -> beforeLines, astBefore
                | LeaveLine -> afterLines, astAfter

            match diff.DiffHunk.LineChanges with
                | [] -> diff, None
                | [lineChange] -> 
                        let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                        let (lines, ast) = getSource lineChange.Operation
                        diff, getMemberName' lines ast lineNumber
                | lineChange::rest -> 
                        let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                        let (lines, ast) = getSource lineChange.Operation
                        diff, getMemberName' lines ast lineNumber

        let getMembers ((rev, diffs, codeBefore, codeAfter) : RevisionInfo * DiffChange list * string * string) =
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let astBefore = parseCode codeBefore
            let astAfter = parseCode codeAfter

            diffs 
            |> List.map (getMemberName astBefore astAfter)
            |> List.choose (fun (d,s) -> match s with | Some st -> Some(d,st) | None -> None)
            |> List.distinctBy (fun (d,s) -> s)
            |> List.map (fun (d,m) -> { Revision = rev; Member = m; LinesAdded = d.DiffHunk.LinesAdded; LinesRemoved = d.DiffHunk.LinesRemoved })
            |> tee (fun ms -> logger.Information("Found {MemberCount} members in Revision {Revision} by {Author} on {Date}", ms.Length, rev.Hash, rev.Author, rev.Date))
 
        let asCsv (methodRev : MethodRevision) =
            let revision = methodRev.Revision
            sprintf "%s,%s,%s,%s,%i,%i" revision.Hash revision.Date revision.Author methodRev.Member methodRev.LinesAdded methodRev.LinesRemoved
             
        seq {
            yield sprintf "hash,date,author,member,added,removed"
            yield! Git.revs git file
                    |> List.pairwise // TODO: pulling each diff and file twice this way? Cache them (limit size to 1?), pull from cache second time.
                    |> List.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMembers)
                    |> List.collect id
                    |> List.map asCsv
        }
