USE [HPZone]
GO

/****** Object:  Table [dbo].[hpz_agent]    Script Date: 10/9/2023 10:57:32 AM ******/
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

