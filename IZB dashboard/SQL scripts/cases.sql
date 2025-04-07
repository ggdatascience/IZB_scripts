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

