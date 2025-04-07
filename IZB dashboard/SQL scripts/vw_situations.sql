USE [HPZone_new]
GO

/****** Object:  View [dbo].[vw_situations]    Script Date: 3/28/2025 7:20:43 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vw_situations]
AS
SELECT        dbo.situations.Situation_number AS hpzone_id, COALESCE (dbo.situations.Start_date, dbo.situations.Situation_creation_date) AS datum, dbo.situations.Situation_creation_date AS invoerdatum, 
                         dbo.situations.Closed_date AS sluitdatum, dbo.situations.Status, dbo.situations.Type, dbo.hpz_agent.ag_naam AS agent, zgag.groep_naam AS agent_groep, 
                         zgag.groep_meldingsplichtig AS agent_meldingsplichtig, dbo.hpz_scenario.sc_naam AS scenario, dbo.hpz_scenario.sc_groep AS scenario_groep, dbo.situations.Confidence AS zekerheid, 
                         dbo.situations.Artikel_26 AS artikel26, dbo.hpz_context.ctx_naam AS context, dbo.situations.Postcode, gem.gemeentecode, gem.gemeentenaam, (CASE WHEN Osirisnummer IS NOT NULL THEN 1 ELSE 0 END) 
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

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane1', @value=N'[0E232FF0-B466-11cf-A24F-00AA00A3EFFF, 1.00]
Begin DesignProperties = 
   Begin PaneConfigurations = 
      Begin PaneConfiguration = 0
         NumPanes = 4
         Configuration = "(H (1[41] 4[20] 2[21] 3) )"
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
         Begin Table = "hpz_agent"
            Begin Extent = 
               Top = 6
               Left = 284
               Bottom = 136
               Right = 491
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "zgag"
            Begin Extent = 
               Top = 6
               Left = 529
               Bottom = 119
               Right = 743
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_context"
            Begin Extent = 
               Top = 6
               Left = 781
               Bottom = 102
               Right = 951
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_a"
            Begin Extent = 
               Top = 6
               Left = 989
               Bottom = 102
               Right = 1159
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_b"
            Begin Extent = 
               Top = 102
               Left = 781
               Bottom = 198
               Right = 951
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_scenario"
            Begin Extent = 
               Top = 102
               Left = 989
               Bottom = 215
               Right = 1159
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "pc4_g"
            Begin Extent = 
               Top = 120
               Left = 529
               Bottom = 250
               Right = 699
            End
            DisplayFlags = 2' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_situations'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane2', @value=N'80
            TopColumn = 0
         End
         Begin Table = "gem"
            Begin Extent = 
               Top = 164
               Left = 330
               Bottom = 294
               Right = 502
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "situations"
            Begin Extent = 
               Top = 18
               Left = 19
               Bottom = 279
               Right = 276
            End
            DisplayFlags = 280
            TopColumn = 11
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
' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_situations'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPaneCount', @value=2 , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_situations'
GO

