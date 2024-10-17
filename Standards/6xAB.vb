'____iLogic Definition for Library/Standard Part_______
AddVbFile "VB - clsDebugViewer.vb"
AddVbFile "VB - clsFileProperties.vb"
AddVbFile "VB - clsPart.vb"

Imports clsDebugViewer
Imports Inventor.SelectionFilterEnum

Public Class hp6xAB
''' Contains the instructions typical for the
''' 6xAB Template -> Header Plug & Tube Part

#Region "Shared Declarations"
  '' All Public Shared Variables and Settings
  Public Shared Dim prp As New clsFileProperties
  ''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"
  Public Shared oApp As Application
  Public Shared oTG As TransientGeometry
  Public Shared oTO As TransientObjects
  Public Shared oCM As CommandManager

#End Region

    ''' <summary>
    ''' This method cycles through each parameter and replaces 
    ''' the prefix based on a search string with the given values. 
    ''' </summary>
    ''' <remarks>The main function should only be called from a 'virgin' file</remarks>
    Public Sub Main()
      'LogicVb.UpdateWhenDone = True
      Dim oDoc As Document = ThisDoc.Document

      ' Get Application and Transient Functions
      oApp = ThisApplication
      oTG = oApp.TransientGeometry
      oTO = oApp.TransientObjects
      oCM = oApp.CommandManager

      Call SetStdNumber()

      Try
      Call PrefixParameterNames( oDoc.ComponentDefinition.Parameters.ModelParameters ,
                                 "dH1" , "dH_" )
      iHdr =  InputBox("Which header is this?", "Integer Input")
      Call PrefixParameterNames( oDoc.ComponentDefinition.Parameters.ModelParameters ,
                                 "dH_" , "dH" & iHdr )
      Catch
      End Try

      Call MakeSomeHoles(oDoc)

    End Sub


    Sub MakeSomeHoles(Optional ByRef plSource As WorkPlane = Nothing)
      If plSource Is Nothing Then plSource = GetSingleSelection(kWorkPlaneFilter)

      rwSketch = 
      Dim rwCount As Integer
      rwCount = Max( 1 , InputBox("How Many Rows are to be processed?", "Basic Details") )
      For i As Integer = 1 To rwCount
        
      Next i

    End Sub


    ''' <summary>
    ''' This method assigns the standard ID for calling/running this rule.
    ''' </summary>
    ''' <param name="sID">String should be the same as the File/Class ID</param>
    ''' <remarks></remarks>
    Sub SetStdNumber(Optional ByVal sID As String = "6xAB")
     prp.SetiPropValue(ThisDoc.Document,
                       "Standard Part",
                       sID,
                       "Hudson Properties")
    End Sub

    ''' <summary>
    ''' This method cycles through each parameter and replaces 
    ''' the prefix based on a search string with the given values. 
    ''' <example>
    ''' For example:
    ''' <code>
    '''     Dim oDoc As Document = ThisDoc.Document
    '''     Call PrefixParameters(oDoc, "_", "1")
    ''' </code>
    ''' results in <c>dH_:LG</c> having the value <c>dH1:LG</c>.
    ''' </example>
    ''' </summary>
    ''' <param name="oParamSet">Parameter Set from a Component Defintion (eg. Parameters, ReferenceParameters, ModelParameters ...)</param>
    ''' <param name="sFind">String to Search For</param>
    ''' <param name="sNew">String to Replace With</param>
    ''' <returns></returns>
    ''' <remarks>This is already a function that is shown for reference</remarks>
    Sub PrefixParameterNames(ByRef oParamSet As Object,
                             ByRef sFind As String,
                             ByRef sNew As String)
    '
      Try
        For Each Prm As Parameter in oParamSet
        Try
            If Left(Prm.Name , Len(sFind) ) = sFind Then 
                Echo("Parameter " & Prm.Name & " is changed to:")
                Prm.Name = sNew & Right( Prm.Name , Max( Len(Prm.Name) - Len(sFind) , 0 ) )
                Echo("          \----> " & Prm.Name)
            End If

        Catch ex1 As Exception
                Echo(ex1.Message)
        End Try
      Next Prm

      Catch ex As Exception
        Echo(ex.Message)
        Exit Sub
      End Try
    End Sub

End Class


    ''' <summary>
    ''' This method changes the point's location by the given x- and 
    ''' y-offsets.
    ''' <example>
    ''' For example:
    ''' <code>
    '''     Dim p As Point = New Point(3,5)
    '''     p.Translate(-1,3)
    ''' </code>
    ''' results in <c>p</c>'s having the value (2,8).
    ''' </example>
    ''' </summary>
    ''' <param name="xTrans">Integer Value to Translate Point (eg. 1, 2, -3, ...)</param>
    ''' <param name="YTrans">Integer Value to Translate Point (eg. 1, 2, -3, ...)</param>
    ''' <returns>Nothing</returns>
    ''' <remarks>This is already a function that is shown for reference</remarks>
