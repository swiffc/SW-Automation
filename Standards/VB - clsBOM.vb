'____Simple iLogic Class Definition_______
' AddVbFile "VB - clsDebugViewer.vb"
' AddVbFile "VB - clsFileMgmt.vb"
' AddVbFile "VB - clsFileProperties.vb"

Imports clsDebugViewer

  ''The following are explicitly implied Imports (Auto)
  'Imports System
  'Imports System.Math
  'Imports System.Collections
  'Imports Microsoft.VisualBasic
  'Imports Autodesk.iLogic.Interfaces
  'Imports Autodesk.iLogic.Runtime
  'If you use MessageBox in a rule, then iLogic automatically adds the following statement:
  'Imports System.Windows.Forms
  'If you use ThisApplication or other Autodesk Inventor objects, then iLogic automatically adds the following:
  Imports Inventor
''

Public Class clsBOM
''' Contains Following Methods
'' Public Functions:: 
''           DoSomething(Integer) {Shared}
'''             Reduces Given Value by 1
''           
'' Public Subs::
''           Main
'''             Main Driving Command
''           CauseSomething {Shared}
'''             Renames a FileObject
''           

#Region "Declarations"
  '' All Private Variables and Settings
  '' Precede Names with a simple underscore
  ' Public Shared Dim prp As New clsFileProperties
  ' ''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"
  ' Public Shared Dim fil As New clsFileMgmt
  ''Use 'fil.' as a prefix for any method called from "VB - clsFileMgmt.vb"

  '''Inventor Objects
  ' Public Shared Dim oApp As Object
  Public Shared Dim m_Doc As Document
  ' Public Shared Dim oTG As TransientGeometry
  ' Public Shared Dim oTO As TransientObjects
  ' Public Shared Dim oCM As CommandManager
  ' Public Shared Dim oGO As GeneralOptions
#End Region


  ' Public Sub Main()
  '   Try
  '     oApp = ThisApplication
  '     oTG = oApp.TransientGeometry
  '     oTO = oApp.TransientObjects
  '     oCM = oApp.CommandManager
  '     oGO = oApp.GeneralOptions
  '    Catch ex As Exception 
  '     Echo("Error with Application :: " & ex.Message) 
  '     Exit Sub
  '   End Try

  '   Try
  '     m_Doc = ThisDoc.Document
  '    Catch ex As Exception 
  '     Echo("Error with Document Type :: " & ex.Message) 
  '     Exit Sub
  '   End Try
  ' End Sub


#Region "Methods"
  ''Actionable Functions or Sub Methods

Public Sub New( Value As Inventor.AssemblyDocument )
  Try

    m_Doc = Value
  Catch ex As Exception
    Echo(ex.Message)

  End Try
End Sub

    ''' <summary>
    ''' This method returns a vlue that is 1 less than 
    ''' the value supplied. 
    ''' <example>
    ''' For example:
    ''' <code>
    '''     Dim Foo As Integer = 12
    '''     Bar = DoSomething(Foo)
    ''' </code>
    ''' results in <c>Bar</c> having the value <c>11</c>.
    ''' </example>
    ''' </summary>
    ''' <param name="Value">Integer Value to be modified</param>
    ''' <returns>'Value - 1'</returns>
    ''' <remarks>This is already a function that is shown for reference</remarks>
    Public Sub BOMQuery()
        ' Set a reference to the assembly document.
        ' This assumes an assembly document is active.
        ' Dim m_Doc As AssemblyDocument
        ' m_Doc = ThisApplication.ActiveDocument

        Dim FirstLevelOnly As Boolean
        If MsgBox("First level only?", vbYesNo) = vbYes Then
            FirstLevelOnly = True
        Else
            FirstLevelOnly = False
        End If
        
        ' Set a reference to the BOM
        Dim oBOM As BOM
        oBOM = m_Doc.ComponentDefinition.BOM
        
        ' Set whether first level only or all levels.
        If FirstLevelOnly Then
            oBOM.StructuredViewFirstLevelOnly = True
        Else
            oBOM.StructuredViewFirstLevelOnly = False
        End If

        ' Make sure that the structured view is enabled.
        oBOM.StructuredViewEnabled = True

        'Set a reference to the "Structured" BOMView
        Dim oBOMView As BOMView
        oBOMView = oBOM.BOMViews.Item("Structured")

        Echo(String.Format("{0,-15}{1,10} {2,-30}{3}", "Item", "Quantity", "     Part Number", "Description")) 
        Echo("----------------------------------------------------------------------------------")

        'Initialize the tab for ItemNumber
        Dim ItemTab As Long
        ItemTab = -3
        Call QueryBOMRowProperties(oBOMView.BOMRows, ItemTab)
    End Sub

    Private Sub QueryBOMRowProperties(oBOMRows As BOMRowsEnumerator, ItemTab As Long)
        ItemTab = ItemTab + 3
        ' Iterate through the contents of the BOM Rows.
        Dim i As Long
        For i = 1 To oBOMRows.Count
            ' Get the current row.
            Dim oRow As BOMRow
            oRow = oBOMRows.Item(i)

            'Set a reference to the primary ComponentDefinition of the row
            Dim oCompDef As ComponentDefinition
            oCompDef = oRow.ComponentDefinitions.Item(1)

            Dim oPartNumProperty As [Property]
            Dim oDescripProperty As [Property]

            If Typeof oCompDef Is VirtualComponentDefinition Then
                'Get the file property that contains the "Part Number"
                'The file property is obtained from the virtual component definition
                oPartNumProperty = oCompDef.PropertySets _
                    .Item("Design Tracking Properties").Item("Part Number")

                'Get the file property that contains the "Description"
                oDescripProperty = oCompDef.PropertySets _
                    .Item("Design Tracking Properties").Item("Description")

                Echo(String.Format("{0}{1,-" & Max(0,15 - ItemTab) & "}{2," & Max(0,10 - ItemTab) & "}  {3,-" & Max(0,30 - ItemTab) & "}{3}", Space(ItemTab), oRow.ItemNumber, oRow.ItemQuantity, oPartNumProperty.Value, oDescripProperty.Value)) 

            Else
                'Get the file property that contains the "Part Number"
                'The file property is obtained from the parent
                'document of the associated ComponentDefinition.
                oPartNumProperty = oCompDef.Document.PropertySets _
                    .Item("Design Tracking Properties").Item("Part Number")

                'Get the file property that contains the "Description"
                oDescripProperty = oCompDef.Document.PropertySets _
                    .Item("Design Tracking Properties").Item("Description")

                Echo(String.Format("{0}{1,-" & Max(0,15 - ItemTab) & "}{2," & Max(0,10 - ItemTab) & "}  {3,-" & Max(0,30 - ItemTab) & "}{3}", Space(ItemTab), oRow.ItemNumber, oRow.ItemQuantity, oPartNumProperty.Value, oDescripProperty.Value)) 

                'Recursively iterate child rows if present.
                If Not oRow.ChildRows Is Nothing Then
                    Call QueryBOMRowProperties(oRow.ChildRows, ItemTab)
                End If
            End If
        Next
        ItemTab = ItemTab - 3
    End Sub

#End Region
End Class
