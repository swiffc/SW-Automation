'____Simple iLogic Class Definition_______
AddVbFile "VB - clsDebugViewer.vb"
AddVbFile "VB - clsFileMgmt.vb"
AddVbFile "VB - clsFileProperties.vb"

Imports clsDebugViewer

Public Class AsDesigned
''' Contains Following Methods
''' Public Functions:: 
'''           DoSomething(Integer) {Shared}
'''             Reduces Given Value by 1
'''           
''' Public Subs::
'''           Main
'''             Main Driving Command
'''           CauseSomething {Shared}
'''             Renames a FileObject
'''           

#Region "Declarations"
  '' All Private Variables and Settings
  '' Precede Names with a simple underscore
  Public Shared Dim prp As New clsFileProperties
  ''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"
  Public Shared Dim fil As New clsFileMgmt
  ''Use 'fil.' as a prefix for any method called from "VB - clsFileMgmt.vb"

#End Region


  Public Sub Main()
    Dim oDoc As Document = ThisDoc.Document
    Dim sID As String

    prp.EchoIProperties(oDoc)

    sID = InputBox("Enter Standard Part Value")
     prp.SetiPropValue(oDoc,
                       "Standard Part",
                       "sID",
                       "Hudson Properties")

  End Sub


#Region "Methods"
 ''Actionable Functions or Sub Methods

  Public Sub CauseSomething(ByVal oFile As Object)
  'System.IO.Rename(oFile.Path, "c:\Test")
  End Sub

#End Region
End Class

