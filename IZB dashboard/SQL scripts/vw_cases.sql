USE [HPZone_new]
GO

/****** Object:  View [dbo].[vw_cases]    Script Date: 3/28/2025 7:20:24 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_cases]
AS
SELECT        dbo.cases.Case_number AS hpzone_id, COALESCE (dbo.cases.Date_of_onset, dbo.cases.Datum_melding_aan_de_ggd, dbo.cases.Case_creation_date) AS peildatum, 
                         COALESCE (dbo.cases.Datum_melding_aan_de_ggd, dbo.cases.Case_creation_date) AS melddatum, dbo.cases.Date_closed AS sluitdatum, dbo.cases.Gender AS geslacht, 
                         dbo.cases.Oorspronkelijke_bron_van_de_melding AS meldorganisatie, dbo.cases.Age_in_years AS leeftijd, dbo.cases.Postcode, gem.gemeentecode, gem.gemeentenaam, 
                         COALESCE ((CASE WHEN dbo.cases.ABR = 'CPE' THEN dbo.cases.ABR ELSE NULL END), zgdx.groep_naam, zginf.groep_naam, zgag.groep_naam, dbo.cases.ABR) AS groep, 
                         (CASE WHEN dbo.cases.ABR = 'CPE' THEN 1 WHEN zgdx.groep_id IS NOT NULL THEN zgdx.groep_meldingsplichtig WHEN zginf.groep_id IS NOT NULL 
                         THEN zginf.groep_meldingsplichtig WHEN zgag.groep_id IS NOT NULL THEN zgag.groep_meldingsplichtig WHEN dbo.cases.ABR IS NOT NULL THEN 1 ELSE 0 END) AS meldingsplichtig, 
                         dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_infectie.inf_naam AS infectie, zginf.groep_naam AS infectie_groep, 
                         zginf.groep_meldingsplichtig AS infectie_meldingsplichtig, dbo.hpz_diagnose.dx_naam AS diagnose, zgdx.groep_naam AS diagnose_groep, zgdx.groep_meldingsplichtig AS diagnose_meldingsplichtig, 
                         dbo.cases.Confidence AS diagnosezekerheid, dbo.hpz_context.ctx_naam AS context, dbo.cases.Laboratorium_waar_de_casus_gediagnosticeerd_is AS lab, dbo.cases.ABR AS antibioticaresistentie, 
                         dbo.cases.Recent_travel_to_another_country AS buitenland, dbo.cases.Date_of_onset AS eersteziektedag, (CASE WHEN dbo.cases.Date_of_death IS NOT NULL THEN 1 ELSE 0 END) AS overlijden, 
                         dbo.cases.Vaccinated_in_respect_to_the_diagnosis AS vaccinatie, dbo.cases.Vaccination_date AS vaccinatiedatum, dbo.cases.Hospitalised AS ziekenhuisopname, 
                         dbo.cases.Status_van_de_melding AS statusmelding, COALESCE (dbo.cases.Datum_gefiatteerd_in_osiris, dbo.cases.Datum_definitief_in_osiris, dbo.cases.Datum_geautomatiseerd_in_osiris) AS Osirisdatum, 
                         (CASE WHEN COALESCE (dbo.cases.Datum_gefiatteerd_in_osiris, dbo.cases.Datum_definitief_in_osiris, dbo.cases.Datum_geautomatiseerd_in_osiris) IS NOT NULL THEN 1 ELSE 0 END) AS gemeld, 
                         mdw_a.mdw_naam AS medewerker, mdw_b.mdw_naam AS casemanager, dbo.cases.created, dbo.cases.updated
FROM            dbo.cases LEFT OUTER JOIN
                         dbo.hpz_agent ON dbo.cases.Agent = dbo.hpz_agent.ag_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgag ON dbo.hpz_agent.ag_groep = zgag.groep_id LEFT OUTER JOIN
                         dbo.hpz_infectie ON dbo.cases.Infection = dbo.hpz_infectie.inf_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zginf ON dbo.hpz_infectie.inf_groep = zginf.groep_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_a ON dbo.cases.Investigating_officer = mdw_a.mdw_id LEFT OUTER JOIN
                         dbo.hpz_medewerker AS mdw_b ON dbo.cases.Case_manager = mdw_b.mdw_id LEFT OUTER JOIN
                         dbo.hpz_diagnose ON dbo.cases.Diagnosis = dbo.hpz_diagnose.dx_id LEFT OUTER JOIN
                         dbo.infectieziekte AS zgdx ON dbo.hpz_diagnose.dx_groep = zgdx.groep_id LEFT OUTER JOIN
                         dbo.hpz_context ON dbo.cases.Principal_contextual_setting = dbo.hpz_context.ctx_id LEFT OUTER JOIN
                         Algemeen.dbo.PC4_gemeente AS pc4_g ON pc4_g.PC4 = dbo.cases.Postcode LEFT OUTER JOIN
                         Algemeen.dbo.gemeente AS gem ON gem.gemeentecode = pc4_g.gemeentecode
GO
