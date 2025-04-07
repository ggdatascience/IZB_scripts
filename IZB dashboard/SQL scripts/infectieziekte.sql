USE [HPZone]
GO

/****** Object:  Table [dbo].[infectieziekte]    Script Date: 10/9/2023 10:58:09 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[infectieziekte](
	[groep_id] [int] IDENTITY(1,1) NOT NULL,
	[groep_naam] [nvarchar](100) NULL,
	[groep_meldingsplichtig] [tinyint] NULL,
 CONSTRAINT [PK_infectieziekte] PRIMARY KEY CLUSTERED 
(
	[groep_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

