CREATE TABLE [dbo].[CommitInfo] (
    [Id]     INT            IDENTITY (1, 1) NOT NULL,
    [Hash]   NVARCHAR (255) NOT NULL,
    [Date]   DATETIME2 (7)  NOT NULL,
    [Author] NVARCHAR (255) NOT NULL,
    CONSTRAINT [PK_CommitInfo] PRIMARY KEY CLUSTERED ([Id] ASC),
    CONSTRAINT [IX_UNIQUE_CommitInfo_Hash] UNIQUE([Hash])   
);
GO

CREATE INDEX [IX_CommitInfo_Fields] ON [dbo].[CommitInfo] ([Hash],[Date],[Author])
