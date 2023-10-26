USE [HPZone]
GO

/****** Object:  Table [dbo].[context]    Script Date: 10/9/2023 10:57:20 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[hpz_context](
	[ctx_id] [smallint] IDENTITY(1,1) NOT NULL,
	[ctx_naam] [nvarchar](250) NULL,
 CONSTRAINT [PK_context] PRIMARY KEY CLUSTERED 
(
	[ctx_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

