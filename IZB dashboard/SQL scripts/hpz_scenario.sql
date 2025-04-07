USE [HPZone]
GO

/****** Object:  Table [dbo].[scenario]    Script Date: 10/9/2023 10:58:24 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[hpz_scenario](
	[sc_id] [smallint] IDENTITY(1,1) NOT NULL,
	[sc_naam] [nvarchar](100) NULL,
	[sc_groep] [int] NULL,
 CONSTRAINT [PK_scenario] PRIMARY KEY CLUSTERED 
(
	[sc_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

