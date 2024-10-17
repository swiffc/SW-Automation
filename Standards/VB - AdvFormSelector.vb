AddVbFile "VB - clsFileProperties.vb"
AddVbFile "VB - clsDebugViewer.vb"

Imports clsDebugViewer
Sub Main()
  Dim oDoc As Object ' Document = ThisDoc.Document

  Select Case ThisDoc.Document.DocumentType
  Case 12290
    oDoc = TryCast( ThisDoc.Document , Inventor.PartDocument )
    Echo("Document Cast as Part")
  Case 12291
    oDoc = TryCast( ThisDoc.Document , Inventor.AssemblyDocument )
    Echo("Document Cast as Assembly")
  Case 12292
    oDoc = TryCast( ThisDoc.Document , Inventor.DrawingDocument )
    Echo("Document Cast as Drawing")
  Case Else
    oDoc = TryCast( ThisDoc.Document , Inventor.Document )
    Echo("Document Cast as Document")

  End Select


  Dim prp As New clsFileProperties
  Dim Selection As String

  Try
    Selection = prp.GetParamValue( oDoc , "Form Type") 'ThisDoc.Document, "Form Type")
    Echo("Try Selection = " & Selection & "  ::" & oDoc.DocumentType)
    If Selection = "" Then Selection = prp.GetiPropValue(oDoc, "Form Type")
   Catch ex As Exception
    Echo(String.Format("Issue with {0} :: {1}", "AdvFormSelector on GetParamValue", ex.Message) )
    Selection = prp.GetiPropValue(oDoc, "Form Type")
    Echo("Catch Selection = " & Selection)
  End Try

  Echo("Selection is now " & Selection)
  ''Populate Dictionary with known form names
  InitializeFormList()

  ''Using retrieved value, get the string for the Form from the Dictionary
  If Selection <> "" Then sForm1 = tForms(Selection) : Echo(sFormNo)

  If sForm1 = "" Then sForm1 = tForms(4)

  ''Call Form
 Try
   iLogicForm.ShowGlobal(sForm1, FormMode.NonModal)
 
  Catch ex As Exception
   MsgBox(ex.Message)
 End Try

End Sub

'''_______________ Child Sub Reqs End Here _______________
''' Contains Test Methods for Class
'' Public Functions:: 
''           testRef(Boolean) {Shared}
''           testVal(Integer) {Shared}
''           testDual(String) {Shared}
'' Public Subs::
''           testDual {Shared}
'------------ Visual Basic Code Below ------------'

Public tForms As New Dictionary(Of String, String)

Sub InitializeFormList()
  tForms(1) = "SEI - Drawing - Detailed"
  tForms(2) = "SEI - Drawing - Simple"
  tForms(3) = "SEI - Drawing - Multiple Parts"
  tForms(4) = "SEI - General Tools"
End Sub


' Sub InitializePartList()
'     'Assign value to dictionary(key) 
'     tParts(14) = New FormList(0.065, 0.072, "11-1111")
'     tParts(16) = New FormList(0.051, 0.056, "11-1112")
'     tParts(18) = New FormList(0.040, 0.044, "11-1113")

' End Sub
 

 
' '____________________________________________
' ''Define new Class/Variable Type with subset variables.
' Public tParts As New Dictionary(Of Double, FormList)
' Class FormList
'     'Setup Variables/FeatureNames to be Tracked in Table/Array
'     Public MinorDiam as Double
'     Public MajorDiam as Double
'     Public PartNumber As String
'     Public Description As String

'     Public Sub New( MinorDiam as Double, MajorDiam as Double, PartNumber As String)
'         Me.MinorDiam = MinorDiam
'         Me.MajorDiam = MajorDiam
'         Me.PartNumber = PartNumber
'         Me.Description = MajorDiam & " MjrDia x " & MinorDiam & " Mnr Dia"

'     End Sub

' End Class 