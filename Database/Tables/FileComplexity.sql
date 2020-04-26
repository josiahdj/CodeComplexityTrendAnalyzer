CREATE TABLE [dbo].[FileComplexity] (
    [Id]               INT        IDENTITY (1, 1) NOT NULL,
    [CommitInfoId]     INT        NOT NULL,
    [TotalComplexity]  INT        NOT NULL,
    [MinComplexity]    INT        NOT NULL,
    [MaxComplexity]    INT        NOT NULL,
    [StdDevComplexity] FLOAT (53) NOT NULL,
    [MeanComplexity]   FLOAT (53) NOT NULL,
    CONSTRAINT [PK_FileComplexity] PRIMARY KEY CLUSTERED ([Id] ASC),
    CONSTRAINT [FK_FileComplexity_CommitInfo] FOREIGN KEY ([CommitInfoId]) REFERENCES [dbo].[CommitInfo] ([Id])
);

