namespace CodeComplexityTrendAnalyzer

open FSharp.Data.Sql
open System.Data.SqlClient
open System

module Database = 
    let [<Literal>] DbVendor = Common.DatabaseProviderTypes.MSSQLSERVER
    let [<Literal>] ConnString = "Server=localhost;Database=CodeAnalysis;Integrated Security=True;"
    let [<Literal>] IndivAmount = 1000
    let [<Literal>] UseOptTypes  = true
    let [<Literal>] ContextSchemaPath = __SOURCE_DIRECTORY__ + @".\Schema.json" // ,ContextSchemaPath = contextSchemaPath
    
    type Sql =
        SqlDataProvider<
            DbVendor,
            ConnString,
            IndividualsAmount = IndivAmount,
            UseOptionTypes = UseOptTypes>
    
    let ctx = Sql.GetDataContext()
    // ctx.SaveContextSchema() |> ignore
    let commitInfo = ctx.Dbo.CommitInfo


    let toTable data =
        ()

    let saveCommitInfo (ci : CommitInfo) =
        //match DateTime.TryParse(ci.Date) with
        //| true, date ->
        //    try 
        //        let newCi = commitInfo.``Create(Author, Date, Hash)``(ci.Author, date, ci.Hash)
        //        ctx.SubmitUpdates()
        //        logger.Debug("Created commit record with hash {Hash} and Id {Id}", newCi.Hash, newCi.Id)
        //    with 
        //    | :? SqlException as ex when ex.Message.Contains("unique") -> 
        //        logger.Debug("The commit with hash {Hash} already exists.", ci.Hash)
        //| false, _ -> ()
        ()