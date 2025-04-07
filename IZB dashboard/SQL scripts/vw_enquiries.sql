USE [HPZone_new]
GO

/****** Object:  View [dbo].[vw_enquiries]    Script Date: 3/28/2025 7:20:33 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_enquiries]
AS
SELECT        dbo.enquiries.Enquiry_number AS hpzone_id, dbo.enquiries.Received_on AS startdatum, dbo.enquiries.Date_closed AS einddatum, mdw_o.mdw_naam AS ontvanger, mdw_b.mdw_naam AS medewerker, 
                         dbo.enquiries.Status, dbo.enquiries.Caller_postcode4 AS postcode, 'U' AS geslacht, dbo.enquiries.Type_of_caller AS typebeller, dbo.enquiries.Broad_topic AS categorie, 
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
