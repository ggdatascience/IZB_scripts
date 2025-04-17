USE [HPZone]
GO

/****** Object:  View [dbo].[vw_situations]    Script Date: 3/28/2025 7:20:43 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_situations]
AS
SELECT        dbo.situations.Situation_number AS hpzone_id, COALESCE (dbo.situations.Start_date, dbo.situations.Situation_creation_date) AS datum, dbo.situations.Situation_creation_date AS invoerdatum, 
                         dbo.situations.Closed_date AS sluitdatum, dbo.situations.Status AS status, dbo.situations.Type AS type, dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, 
                         zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_scenario.sc_naam AS scenario, dbo.hpz_scenario.sc_groep AS scenario_groep, dbo.situations.Confidence AS zekerheid, 
                         dbo.situations.Artikel_26 AS artikel26, dbo.hpz_context.ctx_naam AS context, dbo.situations.Postcode AS postcode, gem.gemeentecode, gem.gemeentenaam, (CASE WHEN Osirisnummer IS NOT NULL THEN 1 ELSE 0 END) 
                         AS melding, mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.situations.Number_potentially_at_risk AS aantal_risico, 
                         dbo.situations.Number_of_symptomatic_cases AS aantal_symptomatisch, dbo.situations.Number_of_fatalities AS aantal_overleden, dbo.situations.Number_hospitalised AS aantal_ziekenhuis, 1 AS risiconiveau, 
                         dbo.situations.created, dbo.situations.updated
FROM            dbo.situations LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.situations.Infectious_agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.situations.Principal_contextual_setting = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.situations.Investigating_officer = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.situations.Manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_scenario ON dbo.situations.Scenario = dbo.hpz_scenario.sc_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.situations.Postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO
