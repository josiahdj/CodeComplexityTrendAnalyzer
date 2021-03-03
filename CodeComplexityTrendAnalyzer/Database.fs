namespace CodeComplexityTrendAnalyzer

open System
open System.Data.Common
open Dapper
open Microsoft.Data.Sqlite

[<Sealed>]
type Database (connectionString:string) = 
    let connection = new SqliteConnection(connectionString)
    let tableExists tableName =
        1 = connection.QuerySingle<int>($"SELECT count(*) FROM sqlite_master WHERE type='table' AND name='{tableName}'")

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
                                    [Date]   TEX  NOT NULL,
                                    [Author] NVARCHAR (255) NOT NULL  
                                 )""") |> ignore

    static member InMemoryConnectionString = "Data Source=InMemorySample;Mode=Memory;Cache=Shared";
    
    member _.dateTimeToISOString (dateTime: DateTime) =
        dateTime.ToString("o", System.Globalization.CultureInfo.InvariantCulture)

    member _.insertCommitInfo hash date author =
        connection.Execute(
            """Insert into [CommitInfo] ([Hash], [Date], [Author]) Values(@Hash, @Date, @Author)""",
            {| Hash = hash; Date = date; Author = author |}
            ) |> ignore

    member _.toTable data =
        ()

    member this.saveCommitInfo (ci : CommitInfo) =
        match ci.Date with
        | Some date ->
            try 
                this.insertCommitInfo ci.Hash date ci.Author
                logger.Debug($"Created commit record with hash {ci.Hash}")
            with ex -> 
                logger.Debug(ex.Message)
                logger.Debug("The commit with hash {Hash} already exists.", ci.Hash)
        | None -> ()

    interface IDisposable with 
        member _.Dispose() = connection.Close()