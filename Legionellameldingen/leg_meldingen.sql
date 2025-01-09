SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[leg_meldingen](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[regnr] [nvarchar](50) NOT NULL,
	[melddatum] [date] NULL,
	[monsterdatum] [date] NULL,
	[handelsnaam] [nvarchar](150) NULL,
	[adres] [nvarchar](100) NULL,
	[postcode] [nvarchar](6) NULL,
	[plaats] [nvarchar](100) NULL,
	[land] [nvarchar](100) NULL,
	[kve] [int] NOT NULL,
	[typering] [nvarchar](100) NULL,
	[type_instelling] [nvarchar](100) NULL,
	[opmerkingen] [text] NULL,
 CONSTRAINT [PK_leg_meldingen] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO