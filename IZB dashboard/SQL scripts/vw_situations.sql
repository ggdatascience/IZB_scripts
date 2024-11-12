USE [HPZone]
GO

/****** Object:  View [dbo].[vw_situations]    Script Date: 10/9/2023 1:48:26 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_situations]
AS
SELECT        dbo.situatie.id, dbo.situatie.hpzone_id, dbo.situatie.datum, dbo.situatie.invoerdatum, dbo.situatie.status, dbo.situatie.type, dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, 
                         zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_scenario.sc_naam AS scenario, dbo.hpz_scenario.sc_groep AS scenario_groep, dbo.situatie.zekerheid, dbo.situatie.risiconiveau, 
                         dbo.situatie.artikel26, dbo.hpz_context.ctx_naam AS context, dbo.situatie.postcode, gem.gemeentecode, gem.gemeentenaam, dbo.situatie.melding, mdw_a.mdw_naam AS medewerker, 
                         mdw_b.mdw_naam AS casemanager, dbo.situatie.aantal_risico, dbo.situatie.aantal_symptomatisch, dbo.situatie.aantal_ziekenhuis, dbo.situatie.aantal_overleden, dbo.situatie.created, dbo.situatie.updated
FROM            dbo.situatie LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.situatie.agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.situatie.context = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.situatie.medewerker = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.situatie.manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_scenario ON dbo.situatie.scenario = dbo.hpz_scenario.sc_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.situatie.postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO
