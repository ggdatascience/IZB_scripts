USE [HPZone]
GO

/****** Object:  View [dbo].[vw_cases]    Script Date: 10/9/2023 1:48:05 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_cases]
AS
SELECT        dbo.casus.id, dbo.casus.hpzone_id, dbo.casus.peildatum, dbo.casus.melddatum, dbo.casus.geslacht, dbo.casus.meldorganisatie, dbo.casus.leeftijd, dbo.casus.postcode, gem.gemeentecode, 
                         gem.gemeentenaam, COALESCE ((CASE WHEN dbo.casus.antibioticaresistentie = 'CPE' THEN dbo.casus.antibioticaresistentie ELSE NULL END), zgdx.groep_naam, zginf.groep_naam, zgag.groep_naam, 
                         dbo.casus.antibioticaresistentie) AS groep, (CASE WHEN dbo.casus.antibioticaresistentie = 'CPE' THEN 1 WHEN zgdx.groep_id IS NOT NULL THEN zgdx.groep_meldingsplichtig WHEN zginf.groep_id IS NOT NULL
                          THEN zginf.groep_meldingsplichtig WHEN zgag.groep_id IS NOT NULL THEN zgag.groep_meldingsplichtig WHEN dbo.casus.antibioticaresistentie IS NOT NULL THEN 1 ELSE 0 END) AS meldingsplichtig, 
                         dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_infectie.inf_naam AS infectie, zginf.groep_naam AS infectie_groep, 
                         zginf.groep_meldingsplichtig AS infectie_meldingsplichtig, dbo.hpz_diagnose.dx_naam AS diagnose, zgdx.groep_naam AS diagnose_groep, zgdx.groep_meldingsplichtig AS diagnose_meldingsplichtig, 
                         dbo.casus.diagnosezekerheid, dbo.hpz_context.ctx_naam AS context, dbo.casus.antibioticaresistentie, dbo.casus.buitenland, dbo.casus.eersteziektedag, dbo.casus.overlijden, dbo.casus.vaccinatie, 
                         dbo.casus.vaccinatiedatum, dbo.casus.ziekenhuisopname, dbo.casus.gemeld, dbo.casus.statusmelding, mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.casus.created, 
                         dbo.casus.updated
FROM            dbo.casus LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.casus.agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_infectie ON dbo.casus.infectie = dbo.hpz_infectie.inf_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zginf ON dbo.hpz_infectie.inf_groep = zginf.groep_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.casus.medewerker = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.casus.casemanager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_diagnose ON dbo.casus.diagnose = dbo.hpz_diagnose.dx_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgdx ON dbo.hpz_diagnose.dx_groep = zgdx.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.casus.context = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.casus.postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO