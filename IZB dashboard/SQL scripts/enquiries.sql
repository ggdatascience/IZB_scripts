USE [HPZone_new]
GO

/****** Object:  Table [dbo].[enquiries]    Script Date: 3/28/2025 7:21:51 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
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

