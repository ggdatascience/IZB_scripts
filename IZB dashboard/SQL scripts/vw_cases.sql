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

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane1', @value=N'[0E232FF0-B466-11cf-A24F-00AA00A3EFFF, 1.00]
Begin DesignProperties = 
   Begin PaneConfigurations = 
      Begin PaneConfiguration = 0
         NumPanes = 4
         Configuration = "(H (1[40] 4[20] 2[20] 3) )"
      End
      Begin PaneConfiguration = 1
         NumPanes = 3
         Configuration = "(H (1 [50] 4 [25] 3))"
      End
      Begin PaneConfiguration = 2
         NumPanes = 3
         Configuration = "(H (1 [50] 2 [25] 3))"
      End
      Begin PaneConfiguration = 3
         NumPanes = 3
         Configuration = "(H (4 [30] 2 [40] 3))"
      End
      Begin PaneConfiguration = 4
         NumPanes = 2
         Configuration = "(H (1 [56] 3))"
      End
      Begin PaneConfiguration = 5
         NumPanes = 2
         Configuration = "(H (2 [66] 3))"
      End
      Begin PaneConfiguration = 6
         NumPanes = 2
         Configuration = "(H (4 [50] 3))"
      End
      Begin PaneConfiguration = 7
         NumPanes = 1
         Configuration = "(V (3))"
      End
      Begin PaneConfiguration = 8
         NumPanes = 3
         Configuration = "(H (1[56] 4[18] 2) )"
      End
      Begin PaneConfiguration = 9
         NumPanes = 2
         Configuration = "(H (1 [75] 4))"
      End
      Begin PaneConfiguration = 10
         NumPanes = 2
         Configuration = "(H (1[66] 2) )"
      End
      Begin PaneConfiguration = 11
         NumPanes = 2
         Configuration = "(H (4 [60] 2))"
      End
      Begin PaneConfiguration = 12
         NumPanes = 1
         Configuration = "(H (1) )"
      End
      Begin PaneConfiguration = 13
         NumPanes = 1
         Configuration = "(V (4))"
      End
      Begin PaneConfiguration = 14
         NumPanes = 1
         Configuration = "(V (2))"
      End
      ActivePaneConfig = 0
   End
   Begin DiagramPane = 
      Begin Origin = 
         Top = 0
         Left = 0
      End
      Begin Tables = 
         Begin Table = "cases"
            Begin Extent = 
               Top = 6
               Left = 38
               Bottom = 136
               Right = 390
            End
            DisplayFlags = 280
            TopColumn = 2
         End
         Begin Table = "hpz_agent"
            Begin Extent = 
               Top = 6
               Left = 428
               Bottom = 136
               Right = 635
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_context"
            Begin Extent = 
               Top = 6
               Left = 673
               Bottom = 102
               Right = 843
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_diagnose"
            Begin Extent = 
               Top = 6
               Left = 881
               Bottom = 119
               Right = 1051
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_infectie"
            Begin Extent = 
               Top = 6
               Left = 1089
               Bottom = 119
               Right = 1259
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_a"
            Begin Extent = 
               Top = 102
               Left = 673
               Bottom = 198
               Right = 843
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "zgag"
            Begin Extent = 
               Top = 120
               Left = 1089
               Bottom = 233
               Right = 1303
            End
            DisplayFl' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_cases'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane2', @value=N'ags = 280
            TopColumn = 0
         End
         Begin Table = "pc4_g"
            Begin Extent = 
               Top = 138
               Left = 38
               Bottom = 268
               Right = 208
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "gem"
            Begin Extent = 
               Top = 138
               Left = 246
               Bottom = 268
               Right = 418
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "zginf"
            Begin Extent = 
               Top = 138
               Left = 456
               Bottom = 251
               Right = 670
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_b"
            Begin Extent = 
               Top = 120
               Left = 881
               Bottom = 216
               Right = 1051
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "zgdx"
            Begin Extent = 
               Top = 216
               Left = 708
               Bottom = 329
               Right = 922
            End
            DisplayFlags = 280
            TopColumn = 0
         End
      End
   End
   Begin SQLPane = 
   End
   Begin DataPane = 
      Begin ParameterDefaults = ""
      End
   End
   Begin CriteriaPane = 
      Begin ColumnWidths = 11
         Column = 1440
         Alias = 900
         Table = 1170
         Output = 720
         Append = 1400
         NewValue = 1170
         SortType = 1350
         SortOrder = 1410
         GroupBy = 1350
         Filter = 1350
         Or = 1350
         Or = 1350
         Or = 1350
      End
   End
End
' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_cases'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPaneCount', @value=2 , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_cases'
GO

