USE [HPZone]
GO

/****** Object:  Table [dbo].[hpz_diagnose]    Script Date: 10/9/2023 10:57:43 AM ******/
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

ALTER TABLE [dbo].[hpz_diagnose]  WITH CHECK ADD  CONSTRAINT [FK_hpz_diagnose_infectieziekte] FOREIGN KEY([dx_groep])
REFERENCES [dbo].[infectieziekte] ([groep_id])
GO

ALTER TABLE [dbo].[hpz_diagnose] CHECK CONSTRAINT [FK_hpz_diagnose_infectieziekte]
GO

