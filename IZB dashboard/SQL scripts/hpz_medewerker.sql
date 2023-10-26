USE [HPZone]
GO

/****** Object:  Table [dbo].[medewerker]    Script Date: 10/9/2023 10:58:18 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[hpz_medewerker](
	[mdw_id] [smallint] IDENTITY(1,1) NOT NULL,
	[mdw_naam] [nvarchar](50) NULL,
 CONSTRAINT [PK_medewerker] PRIMARY KEY CLUSTERED 
(
	[mdw_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

