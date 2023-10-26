USE [HPZone]
GO

/****** Object:  Table [dbo].[situatie]    Script Date: 10/9/2023 10:58:32 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[situatie](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[hpzone_id] [bigint] NULL,
	[datum] [datetime] NULL,
	[invoerdatum] [datetime] NULL,
	[status] [nvarchar](25) NULL,
	[type] [nvarchar](50) NULL,
	[agent] [int] NULL,
	[scenario] [smallint] NULL,
	[zekerheid] [nvarchar](50) NULL,
	[risiconiveau] [nvarchar](50) NULL,
	[artikel26] [tinyint] NULL,
	[context] [smallint] NULL,
	[postcode] [int] NULL,
	[melding] [int] NULL,
	[medewerker] [smallint] NULL,
	[manager] [smallint] NULL,
	[aantal_risico] [int] NULL,
	[aantal_symptomatisch] [int] NULL,
	[aantal_ziekenhuis] [int] NULL,
	[aantal_overleden] [int] NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_situatie] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[situatie]  WITH CHECK ADD  CONSTRAINT [FK_situatie_context] FOREIGN KEY([context])
REFERENCES [dbo].[hpz_context] ([ctx_id])
GO

ALTER TABLE [dbo].[situatie] CHECK CONSTRAINT [FK_situatie_context]
GO

ALTER TABLE [dbo].[situatie]  WITH CHECK ADD  CONSTRAINT [FK_situatie_hpz_agent] FOREIGN KEY([agent])
REFERENCES [dbo].[hpz_agent] ([ag_id])
GO

ALTER TABLE [dbo].[situatie] CHECK CONSTRAINT [FK_situatie_hpz_agent]
GO

ALTER TABLE [dbo].[situatie]  WITH CHECK ADD  CONSTRAINT [FK_situatie_medewerker] FOREIGN KEY([medewerker])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[situatie] CHECK CONSTRAINT [FK_situatie_medewerker]
GO

ALTER TABLE [dbo].[situatie]  WITH CHECK ADD  CONSTRAINT [FK_situatie_medewerker1] FOREIGN KEY([manager])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[situatie] CHECK CONSTRAINT [FK_situatie_medewerker1]
GO

ALTER TABLE [dbo].[situatie]  WITH CHECK ADD  CONSTRAINT [FK_situatie_scenario] FOREIGN KEY([scenario])
REFERENCES [dbo].[hpz_scenario] ([sc_id])
GO

ALTER TABLE [dbo].[situatie] CHECK CONSTRAINT [FK_situatie_scenario]
GO

