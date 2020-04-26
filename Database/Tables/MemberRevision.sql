CREATE TABLE [dbo].[MemberRevision] (
    [Id]               INT            IDENTITY (1, 1) NOT NULL,
    [CommitInfoId]     INT            NOT NULL,
    [MemberInfoId]     INT            NOT NULL,
    [LinesAdded]       INT            CONSTRAINT [DF_MemberRevision_LinesAdded] DEFAULT ((0)) NOT NULL,
    [LinesRemoved]     INT            CONSTRAINT [DF_MemberRevision_LinesRemoved] DEFAULT ((0)) NOT NULL,
    [Name]             NVARCHAR (255) NOT NULL,
    [Parameters]       NVARCHAR (500) NULL,
    [LineCount]        INT            CONSTRAINT [DF_MemberRevision_LineCount] DEFAULT ((0)) NOT NULL,
    [TotalComplexity]  INT            CONSTRAINT [DF_MemberRevision_TotalComplexity] DEFAULT ((0)) NOT NULL,
    [MinComplexity]    INT            CONSTRAINT [DF_MemberRevision_MinComplexity] DEFAULT ((0)) NOT NULL,
    [MaxComplexity]    INT            CONSTRAINT [DF_MemberRevision_MaxComplexity] DEFAULT ((0)) NOT NULL,
    [StdDevComplexity] FLOAT (53)     CONSTRAINT [DF_MemberRevision_StdDevComplexity] DEFAULT ((0)) NOT NULL,
    [MeanComplexity]   FLOAT (53)     CONSTRAINT [DF_MemberRevision_MeanComplexity] DEFAULT ((0)) NOT NULL,
    CONSTRAINT [PK_MemberRevision] PRIMARY KEY CLUSTERED ([Id] ASC),
    CONSTRAINT [FK_MemberRevision_CommitInfo] FOREIGN KEY ([CommitInfoId]) REFERENCES [dbo].[CommitInfo] ([Id])
);

