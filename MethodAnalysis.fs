namespace CodeComplexityTrendAnalyzer

open System
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp
open System.Linq
open Fake.IO.FileSystemOperators
open Fake.IO

module MethodAnalysis = 
    let getMethodInfo repo file out =
        let filePath = repo </> file
        let code = File.readAsString filePath

        let st = SourceText.From(code);
        let sf = SyntaxFactory.ParseSyntaxTree(st);
    
        let span = sf.GetText().Lines.[300].Span;
        let nodes = sf.GetRoot().DescendantNodes().Where(fun x -> x.Span.IntersectsWith(span))
        let method = nodes.Where(fun n -> n.Kind() = SyntaxKind.MethodDeclaration || n.Kind() = SyntaxKind.ConstructorDeclaration).FirstOrDefault()
        //let method = nodes.Last()
        Console.WriteLine(method.ToString())
        

