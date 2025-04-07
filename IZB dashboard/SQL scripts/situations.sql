USE [HPZone_new]
GO

/****** Object:  Table [dbo].[situations]    Script Date: 3/28/2025 7:22:03 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
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

