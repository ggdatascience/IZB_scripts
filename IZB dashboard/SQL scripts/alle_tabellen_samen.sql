USE [HPZone_new]
GO

/****** Object:  Table [dbo].[cases]    Script Date: 3/28/2025 7:21:30 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[cases](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[ABR] [nvarchar](50) NULL,
	[Age_in_years] [int] NULL,
	[Agent] [int] NULL,
	[Case_creation_date] [datetime] NOT NULL,
	[Case_manager] [smallint] NULL,
	[Case_number] [bigint] NOT NULL,
	[Confidence] [nvarchar](50) NULL,
	[Date_closed] [date] NULL,
	[Date_of_death] [date] NULL,
	[Date_of_onset] [date] NULL,
	[Datum_definitief_in_osiris] [date] NULL,
	[Datum_geautomatiseerd_in_osiris] [date] NULL,
	[Datum_gefiatteerd_in_osiris] [date] NULL,
	[Datum_gewist] [date] NULL,
	[Datum_melding_aan_de_ggd] [date] NULL,
	[Diagnosis] [int] NULL,
	[Gender] [char](1) NULL,
	[Hospitalised] [int] NULL,
	[Infection] [int] NULL,
	[Investigating_officer] [smallint] NULL,
	[Laboratorium_waar_de_casus_gediagnosticeerd_is] [nvarchar](125) NULL,
	[Osirisnummer] [bigint] NULL,
	[Oorspronkelijke_bron_van_de_melding] [nvarchar](50) NULL,
	[Postcode] [int] NULL,
	[Principal_contextual_setting] [smallint] NULL,
	[Recent_travel_to_another_country] [nvarchar](50) NULL,
	[Status_van_de_melding] [nvarchar](50) NULL,
	[Vaccinated_in_respect_to_the_diagnosis] [nvarchar](50) NULL,
	[Vaccination_date] [date] NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_cases_1] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO


CREATE TABLE [dbo].[enquiries](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[Type_of_caller] [nvarchar](50) NULL,
	[Status] [nvarchar](50) NULL,
	[Specific_topic] [int] NULL,
	[Received_on] [datetime] NOT NULL,
	[Originally_taken_by] [smallint] NULL,
	[Handled_by] [smallint] NULL,
	[Enquiry_number] [int] NOT NULL,
	[Date_closed] [datetime] NULL,
	[Caller_postcode4] [int] NULL,
	[Broad_topic] [nvarchar](50) NULL,
	[Additional_topic] [nvarchar](255) NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_enquiries] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[situations](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[Type] [nvarchar](50) NOT NULL,
	[Status] [nvarchar](50) NULL,
	[Start_date] [date] NULL,
	[Situation_number] [int] NOT NULL,
	[Situation_creation_date] [datetime] NOT NULL,
	[Scenario] [smallint] NULL,
	[Principal_contextual_setting] [smallint] NULL,
	[Postcode] [int] NULL,
	[Osirisnummer] [int] NULL,
	[Number_potentially_at_risk] [smallint] NULL,
	[Number_of_symptomatic_cases] [smallint] NULL,
	[Number_of_fatalities] [smallint] NULL,
	[Number_hospitalised] [smallint] NULL,
	[Manager] [smallint] NULL,
	[Investigating_officer] [smallint] NULL,
	[Infectious_agent] [int] NULL,
	[Confidence] [nvarchar](50) NULL,
	[Closed_date] [datetime] NULL,
	[Artikel_26] [smallint] NULL,
	[created] [datetime] NULL,
	[updated] [datetime] NULL,
 CONSTRAINT [PK_situations] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
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

CREATE TABLE [dbo].[hpz_context](
	[ctx_id] [smallint] IDENTITY(1,1) NOT NULL,
	[ctx_naam] [nvarchar](250) NULL,
 CONSTRAINT [PK_context] PRIMARY KEY CLUSTERED 
(
	[ctx_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
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

CREATE TABLE [dbo].[hpz_medewerker](
	[mdw_id] [smallint] IDENTITY(1,1) NOT NULL,
	[mdw_naam] [nvarchar](50) NULL,
 CONSTRAINT [PK_medewerker] PRIMARY KEY CLUSTERED 
(
	[mdw_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
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

-- tot hier uitvoeren
-- nu eerst medische database vullen











-- na vullen medische database dit uitvoeren

ALTER TABLE [dbo].[hpz_onderwerp]  WITH CHECK ADD  CONSTRAINT [FK_hpz_onderwerp_infectieziekte] FOREIGN KEY([ond_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_onderwerp] CHECK CONSTRAINT [FK_hpz_onderwerp_infectieziekte]
GO

ALTER TABLE [dbo].[hpz_infectie]  WITH CHECK ADD  CONSTRAINT [FK_hpz_infectie_infectieziekte] FOREIGN KEY([inf_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_infectie] CHECK CONSTRAINT [FK_hpz_infectie_infectieziekte]
GO

ALTER TABLE [dbo].[hpz_diagnose]  WITH CHECK ADD  CONSTRAINT [FK_hpz_diagnose_infectieziekte] FOREIGN KEY([dx_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_diagnose] CHECK CONSTRAINT [FK_hpz_diagnose_infectieziekte]
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

ALTER TABLE [dbo].[situations]  WITH CHECK ADD  CONSTRAINT [FK_situations_hpz_agent] FOREIGN KEY([Infectious_agent])
REFERENCES [dbo].[hpz_agent] ([ag_id])
GO

ALTER TABLE [dbo].[situations] CHECK CONSTRAINT [FK_situations_hpz_agent]
GO

ALTER TABLE [dbo].[situations]  WITH CHECK ADD  CONSTRAINT [FK_situations_hpz_context] FOREIGN KEY([Principal_contextual_setting])
REFERENCES [dbo].[hpz_context] ([ctx_id])
GO

ALTER TABLE [dbo].[situations] CHECK CONSTRAINT [FK_situations_hpz_context]
GO

ALTER TABLE [dbo].[situations]  WITH CHECK ADD  CONSTRAINT [FK_situations_hpz_medewerker] FOREIGN KEY([Investigating_officer])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[situations] CHECK CONSTRAINT [FK_situations_hpz_medewerker]
GO

ALTER TABLE [dbo].[situations]  WITH CHECK ADD  CONSTRAINT [FK_situations_hpz_medewerker1] FOREIGN KEY([Manager])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[situations] CHECK CONSTRAINT [FK_situations_hpz_medewerker1]
GO

ALTER TABLE [dbo].[situations]  WITH CHECK ADD  CONSTRAINT [FK_situations_hpz_scenario] FOREIGN KEY([Scenario])
REFERENCES [dbo].[hpz_scenario] ([sc_id])
GO

ALTER TABLE [dbo].[situations] CHECK CONSTRAINT [FK_situations_hpz_scenario]
GO
















ALTER TABLE [dbo].[enquiries]  WITH CHECK ADD  CONSTRAINT [FK_enquiries_hpz_medewerker] FOREIGN KEY([Handled_by])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[enquiries] CHECK CONSTRAINT [FK_enquiries_hpz_medewerker]
GO

ALTER TABLE [dbo].[enquiries]  WITH CHECK ADD  CONSTRAINT [FK_enquiries_hpz_medewerker1] FOREIGN KEY([Originally_taken_by])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[enquiries] CHECK CONSTRAINT [FK_enquiries_hpz_medewerker1]
GO

ALTER TABLE [dbo].[enquiries]  WITH CHECK ADD  CONSTRAINT [FK_enquiries_hpz_onderwerp] FOREIGN KEY([Specific_topic])
REFERENCES [dbo].[hpz_onderwerp] ([ond_id])
GO

ALTER TABLE [dbo].[enquiries] CHECK CONSTRAINT [FK_enquiries_hpz_onderwerp]
GO

ALTER TABLE [dbo].[cases]  WITH CHECK ADD  CONSTRAINT [FK_cases_hpz_context] FOREIGN KEY([Principal_contextual_setting])
REFERENCES [dbo].[hpz_context] ([ctx_id])
GO

ALTER TABLE [dbo].[cases] CHECK CONSTRAINT [FK_cases_hpz_context]
GO

ALTER TABLE [dbo].[cases]  WITH CHECK ADD  CONSTRAINT [FK_cases_hpz_diagnose] FOREIGN KEY([Diagnosis])
REFERENCES [dbo].[hpz_diagnose] ([dx_id])
GO

ALTER TABLE [dbo].[cases] CHECK CONSTRAINT [FK_cases_hpz_diagnose]
GO

ALTER TABLE [dbo].[cases]  WITH CHECK ADD  CONSTRAINT [FK_cases_hpz_infectie] FOREIGN KEY([Infection])
REFERENCES [dbo].[hpz_infectie] ([inf_id])
GO

ALTER TABLE [dbo].[cases] CHECK CONSTRAINT [FK_cases_hpz_infectie]
GO

ALTER TABLE [dbo].[cases]  WITH CHECK ADD  CONSTRAINT [FK_cases_hpz_medewerker] FOREIGN KEY([Case_manager])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[cases] CHECK CONSTRAINT [FK_cases_hpz_medewerker]
GO

ALTER TABLE [dbo].[cases]  WITH CHECK ADD  CONSTRAINT [FK_cases_medewerker] FOREIGN KEY([Investigating_officer])
REFERENCES [dbo].[hpz_medewerker] ([mdw_id])
GO

ALTER TABLE [dbo].[cases] CHECK CONSTRAINT [FK_cases_medewerker]
GO

CREATE VIEW [dbo].[vw_cases]
AS
SELECT        dbo.cases.Case_number AS hpzone_id, COALESCE (dbo.cases.Date_of_onset, dbo.cases.Datum_melding_aan_de_ggd, dbo.cases.Case_creation_date) AS peildatum, 
                         COALESCE (dbo.cases.Datum_melding_aan_de_ggd, dbo.cases.Case_creation_date) AS melddatum, dbo.cases.Date_closed AS sluitdatum, dbo.cases.Gender AS geslacht, 
                         dbo.cases.Oorspronkelijke_bron_van_de_melding AS meldorganisatie, dbo.cases.Age_in_years AS leeftijd, dbo.cases.Postcode AS postcode, gem.gemeentecode, gem.gemeentenaam, 
                         COALESCE ((CASE WHEN dbo.cases.ABR = 'CPE' THEN dbo.cases.ABR ELSE NULL END), zgdx.groep_naam, zginf.groep_naam, zgag.groep_naam, dbo.cases.ABR) AS groep, 
                         (CASE WHEN dbo.cases.ABR = 'CPE' THEN 1 WHEN zgdx.groep_id IS NOT NULL THEN zgdx.groep_meldingsplichtig WHEN zginf.groep_id IS NOT NULL 
                         THEN zginf.groep_meldingsplichtig WHEN zgag.groep_id IS NOT NULL THEN zgag.groep_meldingsplichtig WHEN dbo.cases.ABR IS NOT NULL THEN 1 ELSE 0 END) AS meldingsplichtig, 
                         dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_infectie.inf_naam AS infectie, zginf.groep_naam AS infectie_groep, 
                         zginf.groep_meldingsplichtig AS infectie_meldingsplichtig, dbo.hpz_diagnose.dx_naam AS diagnose, zgdx.groep_naam AS diagnose_groep, zgdx.groep_meldingsplichtig AS diagnose_meldingsplichtig, 
                         dbo.cases.Confidence AS diagnosezekerheid, dbo.hpz_context.ctx_naam AS context, dbo.cases.Laboratorium_waar_de_casus_gediagnosticeerd_is AS lab, dbo.cases.ABR AS antibioticaresistentie, 
                         dbo.cases.Recent_travel_to_another_country AS buitenland, dbo.cases.Date_of_onset AS eersteziektedag, (CASE WHEN dbo.cases.Date_of_death IS NOT NULL THEN 1 ELSE 0 END) AS overlijden, 
                         dbo.cases.Vaccinated_in_respect_to_the_diagnosis AS vaccinatie, dbo.cases.Vaccination_date AS vaccinatiedatum, dbo.cases.Hospitalised AS ziekenhuisopname, 
                         dbo.cases.Status_van_de_melding AS statusmelding, COALESCE (dbo.cases.Datum_gefiatteerd_in_osiris, dbo.cases.Datum_definitief_in_osiris, dbo.cases.Datum_geautomatiseerd_in_osiris) AS Osirisdatum, 
                         (CASE WHEN COALESCE (dbo.cases.Datum_gefiatteerd_in_osiris, dbo.cases.Datum_definitief_in_osiris, dbo.cases.Datum_geautomatiseerd_in_osiris) IS NOT NULL THEN 1 ELSE 0 END) AS gemeld, 
                         mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.cases.created, dbo.cases.updated
FROM            dbo.cases LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.cases.Agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_infectie ON dbo.cases.Infection = dbo.hpz_infectie.inf_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zginf ON dbo.hpz_infectie.inf_groep = zginf.groep_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.cases.Investigating_officer = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.cases.Case_manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_diagnose ON dbo.cases.Diagnosis = dbo.hpz_diagnose.dx_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgdx ON dbo.hpz_diagnose.dx_groep = zgdx.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.cases.Principal_contextual_setting = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.cases.Postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO

CREATE VIEW [dbo].[vw_enquiries]
AS
SELECT        dbo.enquiries.Enquiry_number AS hpzone_id, dbo.enquiries.Received_on AS startdatum, dbo.enquiries.Date_closed AS einddatum, mdw_o.mdw_naam AS ontvanger, mdw_b.mdw_naam AS medewerker, 
                         dbo.enquiries.Status AS status, dbo.enquiries.Caller_postcode4 AS postcode, 'U' AS geslacht, dbo.enquiries.Type_of_caller AS typebeller, dbo.enquiries.Broad_topic AS categorie, 
                         dbo.hpz_onderwerp.ond_naam AS onderwerp, dbo.infectieziekte.groep_naam AS onderwerp_groep, dbo.infectieziekte.groep_meldingsplichtig AS onderwerp_meldingsplichtig, 
                         dbo.enquiries.Additional_topic AS onderwerpopen, dbo.enquiries.created, dbo.enquiries.updated
FROM            dbo.enquiries LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_o ON mdw_o.mdw_id = dbo.enquiries.Originally_taken_by LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON mdw_b.mdw_id = dbo.enquiries.Handled_by LEFT OUTER JOIN
                         dbo.hpz_onderwerp ON dbo.hpz_onderwerp.ond_id = dbo.enquiries.Specific_topic LEFT OUTER JOIN
                         dbo.infectieziekte ON dbo.infectieziekte.groep_id = dbo.hpz_onderwerp.ond_groep LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.enquiries.Caller_postcode4 LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO

CREATE VIEW [dbo].[vw_situations]
AS
SELECT        dbo.situations.Situation_number AS hpzone_id, COALESCE (dbo.situations.Start_date, dbo.situations.Situation_creation_date) AS datum, dbo.situations.Situation_creation_date AS invoerdatum, 
                         dbo.situations.Closed_date AS sluitdatum, dbo.situations.Status AS status, dbo.situations.Type AS type, dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, 
                         zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_scenario.sc_naam AS scenario, dbo.hpz_scenario.sc_groep AS scenario_groep, dbo.situations.Confidence AS zekerheid, 
                         dbo.situations.Artikel_26 AS artikel26, dbo.hpz_context.ctx_naam AS context, dbo.situations.Postcode AS postcode, gem.gemeentecode, gem.gemeentenaam, (CASE WHEN Osirisnummer IS NOT NULL THEN 1 ELSE 0 END) 
                         AS melding, mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.situations.Number_potentially_at_risk AS aantal_risico, 
                         dbo.situations.Number_of_symptomatic_cases AS aantal_symptomatisch, dbo.situations.Number_of_fatalities AS aantal_overleden, dbo.situations.Number_hospitalised AS aantal_ziekenhuis, 1 AS risiconiveau, 
                         dbo.situations.created, dbo.situations.updated
FROM            dbo.situations LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.situations.Infectious_agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.situations.Principal_contextual_setting = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.situations.Investigating_officer = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.situations.Manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_scenario ON dbo.situations.Scenario = dbo.hpz_scenario.sc_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.situations.Postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO