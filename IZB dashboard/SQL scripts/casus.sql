USE [HPZone]
GO

/****** Object:  Table [dbo].[casus]    Script Date: 10/9/2023 10:57:12 AM ******/
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

