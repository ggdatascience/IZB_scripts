USE [HPZone]
GO

/****** Object:  View [dbo].[vw_enquiries]    Script Date: 10/9/2023 1:48:15 PM ******/
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
