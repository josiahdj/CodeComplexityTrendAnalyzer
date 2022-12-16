namespace CodeComplexityTrendAnalyzer

open System
open System.Collections.Generic
open CodeComplexityTrendAnalyzer.DomainTypes
open Dapper
open Microsoft.Data.Sqlite
open System.Linq

[<Sealed>]
type Database (connectionString:string) = 

    //https://github.com/dotnet/docs/tree/master/samples/snippets/standard/data/sqlite
    let connection = new SqliteConnection(connectionString)
    let tableExists tableName =
        1 = connection.QuerySingle<int>($"SELECT count(*) FROM sqlite_master WHERE type='table' AND name='{tableName}'")
        
    let dateTimeToISOString (dateTime: DateTime) =
        dateTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)

    let tryISOStringToDateTime (input: string) =
        DateTime.tryParse input

    do connection.Open()
       if not (tableExists "FileComplexity") then
           connection.Execute("""CREATE TABLE [FileComplexity] (
                                    [Id]               INTEGER PRIMARY KEY NOT NULL,
                                    [CommitInfoId]     INTEGER    NOT NULL,
                                    [TotalComplexity]  INTEGER    NOT NULL,
                                    [MinComplexity]    INTEGER    NOT NULL,
                                    [MaxComplexity]    INTEGER    NOT NULL,
                                    [StdDevComplexity] REAL NOT NULL,
                                    [MeanComplexity]   REAL NOT NULL
                                 )""") |> ignore
                                   
       if not (tableExists "MemberRevision") then
           connection.Execute("""CREATE TABLE [MemberRevision] (
                                    [Id]               INTEGER PRIMARY KEY NOT NULL,
                                    [CommitInfoId]     INT            NOT NULL,
                                    [MemberInfoId]     INT            NOT NULL,
                                    [LinesAdded]       INT            CONSTRAINT [DF_MemberRevision_LinesAdded] DEFAULT ((0)) NOT NULL,
                                    [LinesRemoved]     INT            CONSTRAINT [DF_MemberRevision_LinesRemoved] DEFAULT ((0)) NOT NULL,
                                    [Name]             STRING         NOT NULL,
                                    [Parameters]       STRING         NULL,
                                    [LineCount]        INT            CONSTRAINT [DF_MemberRevision_LineCount] DEFAULT ((0)) NOT NULL,
                                    [TotalComplexity]  INT            CONSTRAINT [DF_MemberRevision_TotalComplexity] DEFAULT ((0)) NOT NULL,
                                    [MinComplexity]    INT            CONSTRAINT [DF_MemberRevision_MinComplexity] DEFAULT ((0)) NOT NULL,
                                    [MaxComplexity]    INT            CONSTRAINT [DF_MemberRevision_MaxComplexity] DEFAULT ((0)) NOT NULL,
                                    [StdDevComplexity] REAL           CONSTRAINT [DF_MemberRevision_StdDevComplexity] DEFAULT ((0)) NOT NULL,
                                    [MeanComplexity]   REAL           CONSTRAINT [DF_MemberRevision_MeanComplexity] DEFAULT ((0)) NOT NULL
                                 )""") |> ignore

       if not (tableExists "CommitInfo") then
           connection.Execute("""CREATE TABLE [CommitInfo] (
                                    [Hash]   TEXT NOT NULL PRIMARY KEY,
                                    [Date]   TEXT NOT NULL,
                                    [Author] TEXT NOT NULL  
                                 )""") |> ignore

    let insertCommitInfo hash date author =
        connection.Execute(
            """Insert into [CommitInfo] ([Hash], [Date], [Author]) Values(@Hash, @Date, @Author)""",
            {| Hash = hash; Date = date; Author = author |}
            ) |> ignore

    static member InMemoryConnectionString = "Data Source=InMemorySample;Mode=Memory;Cache=Shared";
    
    member _.toTable data =
        ()

    member _.saveCommitInfo (ci : CommitInfo) =
        match ci.Date with
        | Some date ->
            try 
                insertCommitInfo ci.Hash (dateTimeToISOString date) ci.Author
                logger.Debug($"Created commit record with hash {ci.Hash}")
            with ex -> 
                logger.Debug(ex.Message)
                logger.Debug("The commit with hash {Hash} already exists.", ci.Hash)
        | None -> ()

    member _.getCommitInfo (hash: string) =
        match connection.Query("SELECT [Hash], [Date], [Author] from [CommitInfo] where [Hash] = @hash", {| hash = hash |}).ToArray() with
        | [| result |] -> 
            let result = result :?> IDictionary<string, obj>
            Some { Hash = result["Hash"] :?> string; Date = tryISOStringToDateTime (result["Date"]:?> string); Author = result["Author"]:?> string }
        | _ -> None
        

    interface IDisposable with 
        member _.Dispose() = connection.Close()