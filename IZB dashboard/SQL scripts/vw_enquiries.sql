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
         Begin Table = "enquiries"
            Begin Extent = 
               Top = 6
               Left = 38
               Bottom = 316
               Right = 230
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_o"
            Begin Extent = 
               Top = 6
               Left = 268
               Bottom = 102
               Right = 438
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "mdw_b"
            Begin Extent = 
               Top = 6
               Left = 476
               Bottom = 102
               Right = 646
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "hpz_onderwerp"
            Begin Extent = 
               Top = 6
               Left = 684
               Bottom = 119
               Right = 854
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "infectieziekte"
            Begin Extent = 
               Top = 6
               Left = 892
               Bottom = 119
               Right = 1106
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "pc4_g"
            Begin Extent = 
               Top = 6
               Left = 1144
               Bottom = 136
               Right = 1314
            End
            DisplayFlags = 280
            TopColumn = 0
         End
         Begin Table = "gem"
            Begin Extent = 
               Top = 102
               Left = 268
               Bottom = 232
               Right = 440
            End
            DisplayFlags = 28' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_enquiries'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPane2', @value=N'0
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
' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_enquiries'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_DiagramPaneCount', @value=2 , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'VIEW',@level1name=N'vw_enquiries'
GO

