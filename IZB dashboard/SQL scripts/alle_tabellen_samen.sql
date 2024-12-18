USE [HPZone]
GO
/****** Object:  Table [dbo].[hpz_medewerker]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[infectieziekte]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[hpz_onderwerp]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[vraag]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  View [dbo].[vw_enquiries]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [dbo].[vw_enquiries]
AS
SELECT        dbo.vraag.id, dbo.vraag.hpzone_id, dbo.vraag.startdatum, dbo.vraag.einddatum, mdw_o.mdw_naam AS ontvanger, mdw_b.mdw_naam AS medewerker, dbo.vraag.status, dbo.vraag.postcode, dbo.vraag.geslacht, 
                         dbo.vraag.typebeller, dbo.vraag.categorie, dbo.hpz_onderwerp.ond_naam AS onderwerp, dbo.infectieziekte.groep_naam AS onderwerp_groep, 
                         dbo.infectieziekte.groep_meldingsplichtig AS onderwerp_meldingsplichtig, dbo.vraag.onderwerpopen, dbo.vraag.created, dbo.vraag.updated
FROM            dbo.vraag LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_o ON mdw_o.mdw_id = dbo.vraag.ontvanger LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON mdw_b.mdw_id = dbo.vraag.medewerker LEFT OUTER JOIN
                         dbo.hpz_onderwerp ON dbo.hpz_onderwerp.ond_id = dbo.vraag.onderwerp LEFT OUTER JOIN
                         dbo.infectieziekte ON dbo.infectieziekte.groep_id = dbo.hpz_onderwerp.ond_groep LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.vraag.postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO
/****** Object:  Table [dbo].[hpz_agent]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[hpz_agent](
	[ag_id] [int] IDENTITY(1,1) NOT NULL,
	[ag_naam] [nvarchar](100) NULL,
	[ag_infectie] [int] NULL,
	[ag_standaarddiagnose] [int] NULL,
	[ag_groep] [int] NULL,
 CONSTRAINT [PK_hpz_agent] PRIMARY KEY CLUSTERED 
(
	[ag_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[hpz_diagnose]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[hpz_diagnose](
	[dx_id] [int] IDENTITY(1,1) NOT NULL,
	[dx_naam] [nvarchar](100) NULL,
	[dx_groep] [int] NULL,
 CONSTRAINT [PK_hpz_diagnose] PRIMARY KEY CLUSTERED 
(
	[dx_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[hpz_context]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[hpz_infectie]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[casus]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[casus](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[hpzone_id] [bigint] NOT NULL,
	[peildatum] [date] NULL,
	[melddatum] [date] NULL,
	[meldorganisatie] [nvarchar](250) NULL,
	[geslacht] [char](1) NULL,
	[leeftijd] [tinyint] NULL,
	[postcode] [int] NULL,
	[agent] [int] NULL,
	[infectie] [int] NULL,
	[diagnose] [int] NULL,
	[diagnosezekerheid] [nvarchar](50) NULL,
	[antibioticaresistentie] [nvarchar](50) NULL,
	[buitenland] [nvarchar](50) NULL,
	[eersteziektedag] [date] NULL,
	[context] [smallint] NULL,
	[ziekenhuisopname] [tinyint] NULL,
	[overlijden] [date] NULL,
	[vaccinatie] [nvarchar](50) NULL,
	[vaccinatiedatum] [date] NULL,
	[gemeld] [date] NULL,
	[statusmelding] [nvarchar](50) NULL,
	[medewerker] [smallint] NULL,
	[casemanager] [smallint] NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_casus_1] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [dbo].[vw_cases]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [dbo].[vw_cases]
AS
SELECT        dbo.casus.id, dbo.casus.hpzone_id, dbo.casus.peildatum, dbo.casus.melddatum, dbo.casus.geslacht, dbo.casus.meldorganisatie, dbo.casus.leeftijd, dbo.casus.postcode, gem.gemeentecode, 
                         gem.gemeentenaam, COALESCE (zgag.groep_naam, zginf.groep_naam, zgdx.groep_naam) AS groep, (CASE WHEN zgag.groep_id IS NOT NULL 
                         THEN zgag.groep_meldingsplichtig WHEN zginf.groep_id IS NOT NULL THEN zginf.groep_meldingsplichtig WHEN zgdx.groep_id IS NOT NULL THEN zgdx.groep_meldingsplichtig END) AS meldingsplichtig, 
                         dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_infectie.inf_naam AS infectie, zginf.groep_naam AS infectie_groep, 
                         zginf.groep_meldingsplichtig AS infectie_meldingsplichtig, dbo.hpz_diagnose.dx_naam AS diagnose, zgdx.groep_naam AS diagnose_groep, zgdx.groep_meldingsplichtig AS diagnose_meldingsplichtig, 
                         dbo.casus.diagnosezekerheid, dbo.hpz_context.ctx_naam AS context, dbo.casus.antibioticaresistentie, dbo.casus.buitenland, dbo.casus.eersteziektedag, dbo.casus.overlijden, dbo.casus.ziekenhuisopname, 
                         dbo.casus.gemeld, dbo.casus.statusmelding, mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.casus.created, dbo.casus.updated
FROM            dbo.casus LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.casus.agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_infectie ON dbo.casus.infectie = dbo.hpz_infectie.inf_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zginf ON dbo.hpz_infectie.inf_groep = zginf.groep_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.casus.medewerker = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.casus.casemanager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_diagnose ON dbo.casus.diagnose = dbo.hpz_diagnose.dx_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgdx ON dbo.hpz_diagnose.dx_groep = zgdx.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.casus.context = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.casus.postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO
/****** Object:  Table [dbo].[situatie]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  Table [dbo].[hpz_scenario]    Script Date: 2/15/2024 2:05:03 PM ******/
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
/****** Object:  View [dbo].[vw_situations]    Script Date: 2/15/2024 2:05:03 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_situations]
AS
SELECT        dbo.situatie.id, dbo.situatie.hpzone_id, dbo.situatie.datum, dbo.situatie.invoerdatum, dbo.situatie.status, dbo.situatie.type, dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, 
                         zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_scenario.sc_naam AS scenario, dbo.hpz_scenario.sc_groep AS scenario_groep, dbo.situatie.zekerheid, dbo.situatie.risiconiveau, 
                         dbo.situatie.artikel26, dbo.hpz_context.ctx_naam AS context, dbo.situatie.postcode, gem.gemeentecode, gem.gemeentenaam, dbo.situatie.melding, mdw_a.mdw_naam AS medewerker, 
                         mdw_b.mdw_naam AS casemanager, dbo.situatie.aantal_risico, dbo.situatie.aantal_symptomatisch, dbo.situatie.aantal_ziekenhuis, dbo.situatie.aantal_overleden, dbo.situatie.created, dbo.situatie.updated
FROM            dbo.situatie LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.situatie.agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.situatie.context = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.situatie.medewerker = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.situatie.manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_scenario ON dbo.situatie.scenario = dbo.hpz_scenario.sc_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.situatie.postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO


/*** Uitvoeren tot hier. Vervolgens eerst datadump invoeren. ***/




/*** Vanaf hier pas uitvoeren NA invoegen datadump! ***/
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_context] FOREIGN KEY([context])
REFERENCES [dbo].[hpz_context] ([ctx_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_context]
GO
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_hpz_agent] FOREIGN KEY([agent])
REFERENCES [dbo].[hpz_agent] ([ag_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_hpz_agent]
GO
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_hpz_diagnose] FOREIGN KEY([diagnose])
REFERENCES [dbo].[hpz_diagnose] ([dx_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_hpz_diagnose]
GO
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_hpz_infectie] FOREIGN KEY([infectie])
REFERENCES [dbo].[hpz_infectie] ([inf_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_hpz_infectie]
GO
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_medewerker] FOREIGN KEY([medewerker])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_medewerker]
GO
ALTER TABLE [dbo].[casus]  WITH CHECK ADD  CONSTRAINT [FK_casus_medewerker1] FOREIGN KEY([casemanager])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO
ALTER TABLE [dbo].[casus] CHECK CONSTRAINT [FK_casus_medewerker1]
GO
ALTER TABLE [dbo].[hpz_agent]  WITH CHECK ADD  CONSTRAINT [FK_hpz_agent_hpz_diagnose] FOREIGN KEY([ag_standaarddiagnose])
REFERENCES [dbo].[hpz_diagnose] ([dx_id])
GO
ALTER TABLE [dbo].[hpz_agent] CHECK CONSTRAINT [FK_hpz_agent_hpz_diagnose]
GO
ALTER TABLE [dbo].[hpz_agent]  WITH CHECK ADD  CONSTRAINT [FK_hpz_agent_hpz_infectie] FOREIGN KEY([ag_infectie])
REFERENCES [dbo].[hpz_infectie] ([inf_id])
GO
ALTER TABLE [dbo].[hpz_agent] CHECK CONSTRAINT [FK_hpz_agent_hpz_infectie]
GO
ALTER TABLE [dbo].[hpz_agent]  WITH CHECK ADD  CONSTRAINT [FK_hpz_agent_infectieziekte] FOREIGN KEY([ag_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO
ALTER TABLE [dbo].[hpz_agent] CHECK CONSTRAINT [FK_hpz_agent_infectieziekte]
GO
ALTER TABLE [dbo].[hpz_diagnose]  WITH CHECK ADD  CONSTRAINT [FK_hpz_diagnose_infectieziekte] FOREIGN KEY([dx_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO
ALTER TABLE [dbo].[hpz_diagnose] CHECK CONSTRAINT [FK_hpz_diagnose_infectieziekte]
GO
ALTER TABLE [dbo].[hpz_infectie]  WITH CHECK ADD  CONSTRAINT [FK_hpz_infectie_infectieziekte] FOREIGN KEY([inf_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO
ALTER TABLE [dbo].[hpz_infectie] CHECK CONSTRAINT [FK_hpz_infectie_infectieziekte]
GO
ALTER TABLE [dbo].[hpz_onderwerp]  WITH CHECK ADD  CONSTRAINT [FK_hpz_onderwerp_infectieziekte] FOREIGN KEY([ond_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO
ALTER TABLE [dbo].[hpz_onderwerp] CHECK CONSTRAINT [FK_hpz_onderwerp_infectieziekte]
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
