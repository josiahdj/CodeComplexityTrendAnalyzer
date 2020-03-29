namespace CodeComplexityTrendAnalyzer

open System
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp
open System.Linq
open Fake.IO.FileSystemOperators
open Fake.IO
open Microsoft.CodeAnalysis.CSharp.Syntax

module MethodAnalysis = 
    let getMethodInfo git repoPath file =
        let filePath = repoPath </> file
        let code = File.readAsString filePath


        let st = SourceText.From(code);
        let sf = SyntaxFactory.ParseSyntaxTree(st);
    
        let span = sf.GetText().Lines.[300].Span;
        let members = sf.GetRoot().DescendantNodes().Where(fun x -> x.Span.IntersectsWith(span)).OfType<MemberDeclarationSyntax>()

        let printMember (n : MemberDeclarationSyntax) =
            match n with 
            | :? PropertyDeclarationSyntax as p -> Console.Out.WriteLine(p.Identifier.ToString()); p.Identifier.ToString()
            | :? MethodDeclarationSyntax as m -> Console.Out.WriteLine(m.Identifier.ToString()); m.Identifier.ToString()
            | :? ConstructorDeclarationSyntax as c -> Console.Out.WriteLine(c.Identifier.ToString()); c.Identifier.ToString()
            | _ -> Console.Out.WriteLine("Not in a method, property, nor constructor"); String.Empty

        members |> Seq.map printMember
