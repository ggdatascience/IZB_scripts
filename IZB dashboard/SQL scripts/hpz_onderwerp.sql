USE [HPZone]
GO

/****** Object:  Table [dbo].[hpz_onderwerp]    Script Date: 10/9/2023 10:58:01 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[hpz_onderwerp](
	[ond_id] [int] IDENTITY(1,1) NOT NULL,
	[ond_naam] [nvarchar](250) NULL,
	[ond_groep] [int] NULL,
 CONSTRAINT [PK_hpz_onderwerp] PRIMARY KEY CLUSTERED 
(
	[ond_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[hpz_onderwerp]  WITH CHECK ADD  CONSTRAINT [FK_hpz_onderwerp_infectieziekte] FOREIGN KEY([ond_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_onderwerp] CHECK CONSTRAINT [FK_hpz_onderwerp_infectieziekte]
GO

