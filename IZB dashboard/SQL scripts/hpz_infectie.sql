USE [HPZone]
GO

/****** Object:  Table [dbo].[hpz_infectie]    Script Date: 10/9/2023 10:57:52 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[hpz_infectie](
	[inf_id] [int] IDENTITY(1,1) NOT NULL,
	[inf_naam] [nvarchar](100) NULL,
	[inf_groep] [int] NULL,
 CONSTRAINT [PK_hpz_infectie] PRIMARY KEY CLUSTERED 
(
	[inf_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[hpz_infectie]  WITH CHECK ADD  CONSTRAINT [FK_hpz_infectie_infectieziekte] FOREIGN KEY([inf_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_infectie] CHECK CONSTRAINT [FK_hpz_infectie_infectieziekte]
GO

