-- Datastructuur voor het opslaan van de rapporten van het Nivel

USE [ODBNOG]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

-- Lijst met GGD'en
CREATE TABLE [dbo].[ggd](
	[ggd_id] [int] NOT NULL,
	[ggd_naam] [varchar](50) NOT NULL,
	[ggd_zoekterm] [varchar](50) NOT NULL,
 CONSTRAINT [PK_veiligheidsregio] PRIMARY KEY CLUSTERED 
(
	[ggd_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (1, N'GGD Groningen', N'Groningen')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (2, N'GGD Fryslân', N'Fryslân')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (3, N'GGD Drenthe', N'Drenthe')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (4, N'GGD IJsselland', N'IJsselland')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (5, N'GGD Twente', N'Twente')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (6, N'GGD Noord- en Oost-Gelderland', N'Oost-Gelderland')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (7, N'Veiligheids- en Gezondheidsregio Gelderland-Midden', N'Gezondheidsregio Gelder')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (8, N'GGD Gelderland-Zuid', N'Gelderland-Zuid')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (9, N'GGD regio Utrecht', N'Utrecht')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (10, N'GGD Hollands-Noorden', N'Hollands-Noorden')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (11, N'GGD Zaanstreek/Waterland', N'Zaanstreek')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (12, N'GGD Kennemerland', N'Kennemerland')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (13, N'GGD Amsterdam', N'Amsterdam')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (14, N'GGD Gooi en Vechtstreek', N'Gooi')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (15, N'GGD Haaglanden', N'Haaglanden')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (16, N'GGD Hollands-Midden', N'Hollands-Midden')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (17, N'GGD Rotterdam-Rijnmond', N'Rotterdam')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (18, N'Dienst Gezondheid & Jeugd Zuid-Holland Zuid', N'ZHZ')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (19, N'GGD Zeeland', N'Zeeland')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (20, N'GGD West-Brabant', N'West-Brabant')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (21, N'GGD Hart voor Brabant', N'Hart voor Brabant')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (22, N'GGD Brabant-Zuidoost', N'Brabant-Zuidoost')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (23, N'GGD Limburg-Noord', N'Limburg-Noord')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (24, N'GGD Zuid-Limburg', N'Zuid-Limburg')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (25, N'GGD Flevoland', N'Flevoland')
INSERT [dbo].[ggd] ([ggd_id], [ggd_naam], [ggd_zoekterm]) VALUES (100, N'Nederland', N'Nederland')
GO



-- Registraties per GGD
CREATE TABLE [dbo].[nivel_zorgregistraties](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[aandoening] [varchar](100) NOT NULL,
	[ggd_regio] [int] NOT NULL,
	[jaar] [int] NOT NULL,
	[week] [int] NOT NULL,
	[aantal] [int] NOT NULL,
	[per100k] [numeric](7, 2) NOT NULL,
	[prevalentie_ratio] [numeric](5, 2) NULL,
	[SPR_upper] [numeric](5, 2) NULL,
	[SPR_lower] [numeric](5, 2) NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_nivel_zorgregistraties] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY],
 CONSTRAINT [unieke_rij_key] UNIQUE NONCLUSTERED 
(
	[jaar] ASC,
	[week] ASC,
	[aandoening] ASC,
	[ggd_regio] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[nivel_zorgregistraties]  WITH CHECK ADD  CONSTRAINT [FK_nivel_zorgregistraties_ggd] FOREIGN KEY([ggd_regio])
REFERENCES [dbo].[ggd] ([ggd_id])
GO

ALTER TABLE [dbo].[nivel_zorgregistraties] CHECK CONSTRAINT [FK_nivel_zorgregistraties_ggd]
GO


USE [ODBNOG]
GO

-- Registraties per leeftijdscategorie
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[nivel_zorgregistraties_leeftijd](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[leeftijd] [varchar](50) NOT NULL,
	[aandoening] [varchar](100) NOT NULL,
	[ggd_regio] [int] NOT NULL,
	[jaar] [int] NOT NULL,
	[week] [int] NOT NULL,
	[aantal] [int] NOT NULL,
	[per100k] [numeric](7, 2) NOT NULL,
	[prevalentie_ratio] [numeric](5, 2) NULL,
	[SPR_upper] [numeric](5, 2) NULL,
	[SPR_lower] [numeric](5, 2) NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_nivel_zorgregistraties_leeftijd] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY],
 CONSTRAINT [unieke_rij_key_nivel_zorgregistraties_leeftijd] UNIQUE NONCLUSTERED 
(
	[jaar] ASC,
	[week] ASC,
	[aandoening] ASC,
	[leeftijd] ASC,
	[ggd_regio] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[nivel_zorgregistraties_leeftijd]  WITH CHECK ADD  CONSTRAINT [FK_nivel_zorgregistraties_leeftijd_ggd] FOREIGN KEY([ggd_regio])
REFERENCES [dbo].[ggd] ([ggd_id])
GO

ALTER TABLE [dbo].[nivel_zorgregistraties_leeftijd] CHECK CONSTRAINT [FK_nivel_zorgregistraties_leeftijd_ggd]
GO


