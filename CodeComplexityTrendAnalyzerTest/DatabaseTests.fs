module CodeComplexityTrendAnalyzerTest

open System
open CodeComplexityTrendAnalyzer
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let InsertAndGetCommit () =
    use db = new Database(Database.InMemoryConnectionString)
    let commit = { Hash = "hash"; Date = Some DateTime.Now;  Author = "me"}
    db.saveCommitInfo commit
    Assert.AreEqual(Some commit, db.getCommitInfo commit.Hash)

[<Test>]
let GetMissingHash () =
    use db = new Database(Database.InMemoryConnectionString)
    Assert.AreEqual(None, db.getCommitInfo "missing hash")