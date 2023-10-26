USE [HPZone]
GO

/****** Object:  Table [dbo].[vraag]    Script Date: 10/9/2023 10:58:38 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[vraag](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[hpzone_id] [bigint] NOT NULL,
	[startdatum] [date] NULL,
	[einddatum] [date] NULL,
	[ontvanger] [smallint] NULL,
	[medewerker] [smallint] NULL,
	[status] [nvarchar](25) NULL,
	[postcode] [int] NULL,
	[geslacht] [char](1) NULL,
	[typebeller] [nvarchar](50) NULL,
	[categorie] [nvarchar](50) NULL,
	[onderwerp] [int] NULL,
	[onderwerpopen] [nvarchar](255) NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_vraag] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[vraag]  WITH CHECK ADD  CONSTRAINT [FK_vraag_hpz_onderwerp] FOREIGN KEY([onderwerp])
REFERENCES [dbo].[hpz_onderwerp] ([ond_id])
GO

ALTER TABLE [dbo].[vraag] CHECK CONSTRAINT [FK_vraag_hpz_onderwerp]
GO

ALTER TABLE [dbo].[vraag]  WITH CHECK ADD  CONSTRAINT [FK_vraag_medewerker] FOREIGN KEY([medewerker])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[vraag] CHECK CONSTRAINT [FK_vraag_medewerker]
GO

ALTER TABLE [dbo].[vraag]  WITH CHECK ADD  CONSTRAINT [FK_vraag_medewerker1] FOREIGN KEY([ontvanger])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[vraag] CHECK CONSTRAINT [FK_vraag_medewerker1]
GO

