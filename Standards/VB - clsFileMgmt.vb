'<IsStraightVb>True</IsStraightVb>
Imports clsDebugViewer
Imports Inventor
Imports System.Windows.Forms


'''_______________ Child Sub Reqs Start Here _______________
  'AddVbFile "VB - clsFileMgmt.vb"
  'AddVbFile "VB - clsDebugViewer.vb"

  'Sub Main()
  ' Dim fil As New clsFileMgmt
  '''Use 'fil.' as a prefix for any method called from "VB - clsFileMgmt.vb"
  'End Sub
'''_______________ Child Sub Reqs End Here _______________


'--Class iLogic Start--------------------------------------------------------------------------------------------------


Public Class clsFileMgmt
''' Contains Test Methods for Class
''' Public Functions:: 
'''     GetValidSaveFileName(String, String, {String}, {Bool}, {String}, {Bool}, {String}, {Bool}, {String}) As String
'''     GetFileName(Document, {Bool}) As String
'''     GetFolderPath(Document, {String}) As String
'''     GetFilePath(Document) As String
'''     FolderDialogSelection({String}) As String
'''     OpenDialog(String, String, Document) As String
'''     OpenFile(String) As String
''' Valid File is a little ... robust, look at trimming down and splitting into different methods.
''' Public Subs::
'''
'--------------------------------------------------------------------------
''Class Public Variable Declarations
 Public Dim bVerboseDebug_clsFileMgmt As Boolean = True
'--------------------------------------------------------------------------

Sub Test()
 '''Run this sub for testing Class Connection and Sub Functions
  Dim fd As String = "C:\Vault Working Folder\Temporary Export Backup\Test\"
  'Dim fn As String = "Document"
  Dim fn As String = "_DeleteMe_"
  Dim fx As String = "txt"
  Dim ProjFolder As String = "Area51"
  Dim UseTempDir As Boolean = True
  Dim CreateTempInstructionFile As Boolean = True
  Dim TempInstructionFileName As String ' = "What What"
    'fn = "_Temporary Files Generated Due to Duplication in Automation Scripts"
  fn = GetValidSaveFileName(fd,fn,fx,UseTempDir,ProjFolder,CreateTempInstructionFile,,TempInstructionFileName)

  Try
  System.IO.File.Create(fn)
  'ThisDoc.Document.Close(fn)
  'ThisDoc.Launch(fd)
  DebugLine("GetValidSaveFileName Tester",bVerboseDebug_clsFileMgmt,3):EchoLine(2)
  Catch ex As Exception
  DebugLine("GetValidSaveFileName Tester",bVerboseDebug_clsFileMgmt,4,ex.Message,1):EchoLine(2)
  End Try
End Sub


'  bTempDir 'Move Files to Temp Directory?
'  sProjFolder 'Move Files to This Subfolder in Temp Directory
'  bTempFil 'Create Instruction File for Temp Folder?
'  bCreateDir  'Create Missing Directories?

Public Function OpenDialog(ByVal DocType As Integer,
                           ByVal DialogTitle As String,
                           Optional ByVal oStartPath As String = "") As String
''Get filename using Windows File Dialog Box
 DebugLine("Open Dialog", bVerboseDebug_clsFileMgmt,1):Echo(Space(15) & "Filter Type:" & Space(1) & DocType, bVerboseDebug_clsFileMgmt):Echo(Space(15) & "Title:" & Space(7) & DialogTitle, bVerboseDebug_clsFileMgmt):If oStartPath <> "" Then Echo(Space(15) & "Start Path:" & Space(2) & oStartPath, bVerboseDebug_clsFileMgmt)
 Dim Count As Integer = 0
 Do
   Try
    Dim oFileDlg As New OpenFileDialog() 'Using Standard Windows Form until Inventor FileDialog is Discovered
    'Dim oFileDlg As Inventor.FileDialog = Nothing
    'Inventor.InventorVb.Application.CreateFileDialog(oFileDlg)
    'Inventor.Application.CreateFileDialog(oFileDlg)
    'oFileDlg.Filter = "Inventor Assembly File (*.iam)|*.iam|All Files (*.*)|*.*"
    Select Case DocType
     Case 1
        oFileDlg.Filter = "Autodesk Inventor Part Files (*.ipt)|*.ipt"
        'Filetype = ".ipt"
     Case 2 
        oFileDlg.Filter = "Autodesk Inventor Assembly Files (*.iam)|*.iam"
        'Filetype = ".iam"
     Case 3
        oFileDlg.Filter = "Autodesk Inventor Drawing Files (*.idw)|*.idw"
        'Filetype = ".idw"
     Case 4
        oFileDlg.Filter = "Excel File (*.xls;*.xlsx)|*.xls;*.xlsx"
     Case Else
        oFileDlg.Filter = "Inventor Model Files (*.iam;*.ipt)|*.iam;*.ipt|Drawings (*.idw;*.dwg)|*.idw;*.dwg|Autodesk Inventor Files|*.iam;*.ipt;*.idw;*.dwg;*.ipn|All Files (*.*)|*.*"
        oFileDlg.FilterIndex = 3
     End Select
    'oFileDlg.DialogTitle = DialogTitle
    oFileDlg.Title = DialogTitle
    oFileDlg.InitialDirectory = oStartPath'GetPath(oFile)
    oFileDlg.FileName = "" 'oStartPath
    oFileDlg.MultiSelect = False
    oFileDlg.RestoreDirectory = True
    'oFileDlg.CancelError = True
    If oFileDlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then oFileDlg.OpenFile()
    'oFileDlg.ShowOpen()
    oFileName = oFileDlg.FileName
    OpenDialog = oFileName
    If Not oFileName = "" Then DebugLine("OpenDialog", bVerboseDebug_clsFileMgmt, 4, OpenDialog)
   Catch exD As Exception
    MessageBox.Show("File not chosen. Please Select a File from File Selection Dialog Box.", "File Selection Dialog Error", MessageBoxButtons.Ok, MessageBoxIcon.Warning)
    oFileName = ""
    Echo("Error found : " & exD.Message)
   End Try

    Count = Count + 1
    If Not oFileName = "" Then Return oFileName


 Loop While Count < 4

    MessageBox.Show("File not chosen. Rerun Rule for Use", "File Selection Dialog Cancellation", MessageBoxButtons.Ok, MessageBoxIcon.Error)
    Return oFileName
End Function


    Public Function OpenFile(ByVal StrFilter As String) As String

        Dim filename As String = ""

        Dim ofDlg As System.Windows.Forms.OpenFileDialog = New System.Windows.Forms.OpenFileDialog()

        Dim user As String = System.Windows.Forms.SystemInformation.UserName

        ofDlg.Title = "Open File"
        ofDlg.InitialDirectory = "C:\Documents and Settings\" + user + "\Desktop\"

        ofDlg.Filter = StrFilter 'Example: "Inventor files (*.ipt; *.iam; *.idw)|*.ipt;*.iam;*.idw"
        ofDlg.FilterIndex = 1
        ofDlg.RestoreDirectory = True

        If (ofDlg.ShowDialog() = DialogResult.OK) Then
            filename = ofDlg.FileName
        End If

        OpenFile = filename

    End Function


Public Function GetValidSaveFileName(ByVal FileDirectory As String,
                                      ByVal FileName As String,
                             Optional ByVal FileExtension As String = "",
                             Optional ByVal bTempDir As Boolean = False,
                             Optional ByVal sProjFolder As String = "",
                             Optional ByVal bTempFil As Boolean = False,
                             Optional ByVal sTempDir As String = "C:\Vault Working Folder\Temporary Backup Files\",
                             Optional ByVal bCreateDir As Boolean = True,
                             Optional ByVal sTempFil As String = "_Temporary Files Generated from Scripts --Safe To Delete--"
                             ) As String
'''Identify if Filename & Extension exists in the given Directory, 
'''If it does then option to add '(#)' as required or Move to Temp Directory
   DebugLine("GetValidSaveFileName",bVerboseDebug_clsFileMgmt,2,"FileDirectory == " & FileDirectory,2):DebugLine(Space(50) & " -:- FileName   == " & FileName,bVerboseDebug_clsFileMgmt):DebugLine(Space(50) & " -:- FileExtension == " & FileExtension,bVerboseDebug_clsFileMgmt):   If bTempDir Then DebugLine(Space(50) & " -:- Temp Directory == " & sTempDir,bVerboseDebug_clsFileMgmt)

  Dim Qty As Integer = 0
  Dim sFileName As String
  Dim sFileNameOld As String

  If Not Right(FileDirectory,1) = "\" Then FileDirectory = FileDirectory & "\"

  If Not FileExtension = "" 
    If Not Left(FileExtension,1) = "." Then FileExtension = "." & FileExtension
  End If

  sFileName = FileDirectory & FileName & FileExtension
  Echo(sFileName,5,bVerboseDebug_clsFileMgmt)

  If bCreateDir And Not System.IO.Directory.Exists(FileDirectory)
  'If Not System.IO.Directory.Exists(FileDirectory)
   System.IO.Directory.CreateDirectory(FileDirectory)
   DebugLine("Directory was Created",bVerboseDebug_clsFileMgmt,6,FileDirectory,3)
   DebugLine("GetValidSaveFileName",bVerboseDebug_clsFileMgmt,4,sFileName,2)
   Return sFileName
  End If

  If Not bCreateDir And Not System.IO.Directory.Exists(FileDirectory)
   Return ""
  End If

  If bTempDir And System.IO.File.Exists(sFileName)
    If Not sProjFolder = "" Then sTempDir = sTempDir & sProjFolder
    DebugLine("GetValidSaveFileName Get Temp Folder",bVerboseDebug_clsFileMgmt,4,sTempDir,2)
    sFileNameOld = GetValidSaveFileName(sTempDir, FileName, FileExtension, False,,bTempFil,,)
    Try
     Rename(sFileName, sFileNameOld)
      DebugLine("File Moved from " & sFileName,bVerboseDebug_clsFileMgmt,6, "to " & sFileNameOld):EchoLine()
      DebugLine("GetValidSaveFileName",bVerboseDebug_clsFileMgmt,4,sFileName,2)
     Return sFileName
    Catch ex As Exception
      DebugLine("Can't Move" & sFileName & " Because Inventor",bVerboseDebug_clsFileMgmt,5, ex.Message)
      Return ""
    End Try
  End If

     If bTempFil
      sTempFil = FileDirectory & sTempFil
      If Not System.IO.File.Exists(sTempFil) Then System.IO.File.Create(sTempFil)
     End If

  Try
    While System.IO.File.Exists(sFileName)
        sFileNameOld = sFileName
        Qty = Qty + 1
        sFileName = FileDirectory & FileName & "(" & Qty & ")" & FileExtension
      DebugLine(" -*- File Found " & sFileNameOld,,,)
      DebugLine(" -*- New FileName is " & sFileName):EchoLine()
    End While
  Catch ex As Exception
   DebugLine(" *** GetValidSaveFileName",,5,ex.Message,3)
   Exit Function
  End Try
   GetValidSaveFileName = sFileName
    DebugLine("GetValidSaveFileName",,4,sFileName,2)
End Function


Public Function GetFileName(ByVal oFile As Document,
                    Optional ByRef bExt As Boolean = True
                     ) As String
''Return Filename of Document (by searching for & trimming from last '\')
 Dim iPathLength As Integer
 Dim sFileName As String
 Dim bVerboseDebug_clsFileMgmt As Boolean = False
 sFileName = oFile.FullFileName
  DebugLine("GetFileName",bVerboseDebug_clsFileMgmt,1,,2)
 iPathLength = InStrRev(sFileName,"\")
  DebugLine(" --- iPathLength", bVerboseDebug_clsFileMgmt,6,iPathLength)
 sFileName = Right(sFileName, Len(sFileName) - iPathLength)
  DebugLine(" --- sFileName", bVerboseDebug_clsFileMgmt,6,sFileName)
 If bExt
   GetFileName = sFileName
   DebugLine(" --- bExt", bVerboseDebug_clsFileMgmt, 6, "True")
  Else
   GetFileName = Left(sFileName,Len(sFileName)-4)
   DebugLine(" --- bExt", bVerboseDebug_clsFileMgmt, 6, "False")
 End If
 DebugLine(" -- GetFileName",bVerboseDebug_clsFileMgmt,4,GetFileName)
End Function


Public Function GetFilePath(ByVal oFile As Document) As String
''Return Path of Document (by searching for & trimming from last '\')
 Dim PathLength As Integer
 PathLength = InStrRev(oFile.FullFileName,"\")
 GetFilePath = Left(oFile.FullFileName, PathLength)
 DebugLine("GetFilePath", bVerboseDebug_clsFileMgmt, 4, GetFilePath)
End Function


Public Function GetFolderPath(ByVal fDocument As Document,
                     Optional ByRef fProjFldrName As String = "",
                     Optional ByRef fName As String = "") 
'' Get Folder Path from Given Path and Embedded Folder Search List
    oPath = fDocument.FullDocumentName ' fDocument.Path
   ' oFolder = Left(oPath, InStrRev(oPath, "\")) & "Output"
   DebugLine("GetFolderPath",,1,,2)
  Dim slFolderStructure As New SortedList()
  Dim i, n, j, k, l As Integer
  'Dim j As Integer = 0
  'Dim n As Integer
  'Dim k As Integer
  Dim iFolderIndex As Integer
  Dim bFolderIndex As Boolean
  Dim sFolderRawName As String
  Dim iPosition As Integer
  Dim iPathLength As Integer
  Dim sPath As String = Left(oPath, InStrRev(oPath, "\"))
  Dim bHudson As Boolean = True
   ''Identify Search Pattern/Names
   Dim aCheckFolders As New List(Of String) From { LCase("HPC"),
                                                   LCase("HCC"),
                                                   LCase("SE"),
                                                   LCase("SmithCo"),
                                                   LCase("Hudson Work"),
                                                   LCase("Vault Working Folder"),
                                                   LCase("Inventor Projects"),
                                                   LCase("Engineering Standards") 
                                                   }
    aCheckFolders.Sort()
 If bVerboseDebug_clsFileMgmt
    Dim sSearchFolders As String = String.Empty
    For Each s in aCheckFolders 
      sSearchFolders = s & " : " & sSearchFolders
    Next:DebugLine("Folder to Search For",,6,sSearchFolders,2)
 End If 'Search Folder String

   iPathLength = Len(sPath)
   sPath = Left(sPath,Len(sPath))
   iPosition = InStrRev(sPath, "\")
    BreakTitle(" ------------ Identify Folder String ------------",bVerboseDebug_clsFileMgmt):DebugLine("Initial Path",bVerboseDebug_clsFileMgmt,6,sPath,2):DebugLine("Initial Slash",bVerboseDebug_clsFileMgmt,6,iPosition,2):DebugLine("Initial Length",bVerboseDebug_clsFileMgmt,6,iPathLength,2)

   sFolder = Mid(sPath,iPosition+1,iPathLength-iPosition)
    BreakTitle(" --------------- String Segments --------------- ",bVerboseDebug_clsFileMgmt):DebugLine(" -No-" & Space(3) & "-Key-" & Space(3) & "-Folder-" & Space(14) & "-Next String s-f",bVerboseDebug_clsFileMgmt)

    j = 0 : n = 100 : l = n 'using n=100 as max possible levels, increase as needed.
   While j < l
     j = j + 1
     n = n - 1
     sFolder = Mid(sPath,iPosition+1,iPathLength-iPosition)
     sPath = Left(sPath, iPosition-1)
     iPathLength = Len(sPath)
     iPosition = Max(InStrRev(sPath, "\"),1)
    slFolderStructure.Add(n,sFolder)
      DebugLine("  " & j & Space(Max(8-Len(Str(j)),0)) & n & Space(Max((10-Len(n)),0)) & sFolder & Space(Max((25-Len(sFolder)),0)) & iPosition & " to " & iPathLength,bVerboseDebug_clsFileMgmt)

     If iPosition = 1
      sFolder = Left(sPath, iPathLength)
      slFolderStructure.Add(n-1,sFolder)
       DebugLine(Space(4) & "Folder Drive " & sFolder,bVerboseDebug_clsFileMgmt)
      Exit While
     End If
   End While


    BreakTitle(" --------------- Sorted List --------------- ",bVerboseDebug_clsFileMgmt):DebugLine(Space(5) & "-KEY-" & Space(2) & "-VALUE-",bVerboseDebug_clsFileMgmt)
   If bVerboseDebug_clsFileMgmt
    For i = 0 To slFolderStructure.Count - 1
      DebugLine(Space(6) & slFolderStructure.GetKey(i) & Space(5) & slFolderStructure.GetByIndex(i))
    Next i
   End If


    BreakTitle(" --------------- Find Folder --------------- ",bVerboseDebug_clsFileMgmt):DebugLine(Space(5) & "-KEY-" & Space(2) & "-VALUE-" & Space(15) & "-KVP-",bVerboseDebug_clsFileMgmt)
   k = slFolderStructure.Count-1
   For i = 0 To slFolderStructure.Count - 1
       DebugLine(Space(6) & slFolderStructure.GetKey(k) & Space(5) & slFolderStructure.GetByIndex(k),bVerboseDebug_clsFileMgmt)
      sFolderRawName = slFolderStructure.GetByIndex(k)
      If aCheckFolders.Contains(LCase(sFolderRawName))
       DebugLine(Space(25) & " Found =" & k & "=>" & slFolderStructure.GetKey(k) & ", " & slFolderStructure.GetByIndex(k),bVerboseDebug_clsFileMgmt)
       iFolderIndex = k
       bFolderIndex = True
       i = slFolderStructure.Count
      End If
         k = k - 1
   Next i


 oFolder = String.Empty
    BreakTitle(" --------------- Build Folder --------------- ",bVerboseDebug_clsFileMgmt):DebugLine(Space(5) & "-KEY-" & Space(2) & "-VALUE-" & Space(18) & "-PATH-",bVerboseDebug_clsFileMgmt)
  If bFolderIndex
    For i = 0 To Min((iFolderIndex+1),(slFolderStructure.Count-1)) 'Min((k + 2),(slFolderStructure.Count-1))
      oFolder = oFolder & slFolderStructure.GetByIndex(i) & "\" 
        DebugLine(Space(6) & slFolderStructure.GetKey(i) & Space(5) & slFolderStructure.GetByIndex(i) & Space(Max((23-Len(slFolderStructure.GetByIndex(i))),0)) & oFolder,bVerboseDebug_clsFileMgmt)
    Next i

    fProjFldrName = slFolderStructure.GetByIndex(Min((iFolderIndex+1),(slFolderStructure.Count-1)))
    oFolder = oFolder & "DC Export\"

   Else
    For i = 0 To Min((k + 2),(slFolderStructure.Count-1))
      oFolder = oFolder & slFolderStructure.GetByIndex(i) & "\"
        DebugLine(Space(6) & slFolderStructure.GetKey(i) & Space(5) & slFolderStructure.GetByIndex(i) & Space(Max((23-Len(slFolderStructure.GetByIndex(i))),0)) & oFolder,bVerboseDebug_clsFileMgmt)
    Next i
    '-+- Windows Folder Selection GetFolderPath(oFolder)
      DebugLine(" -*- Folder Search Path Not Found",bVerboseDebug_clsFileMgmt,4,oFolder,)

    oFolder = FolderDialogSelection(oFolder)
    fProjFldrName = Right(Left(oFolder,Len(oFolder)-1),Len(Left(oFolder,Len(oFolder)-1)) - InStrRev(Left(oFolder,Len(oFolder)-1),"\"))

  End If 'Folder Index

  If Not Right(oFolder,1) = "\" Then oFolder = oFolder & "\"
  DebugLine("GetFolderPath",bVerboseDebug_clsFileMgmt,4,oFolder,2)

  If InStr(LCase(oFolder),LCase("SmithCo")) <> 0 _ 
  Or InStr(LCase(oFolder),LCase("Inventor Projects")) <> 0 _ 
  oR InStr(LCase(oFolder),LCase("SE")) <> 0 Then bHudson = False

  If Not bHudson Then
    Echo("SmithCo is all sorts of Fun!")
    fName = ConvertSEIProject(fProjFldrName)
   Else 
    Echo("Hudson Rules!")
    fName = ""
  End If

 Return oFolder
End Function


Public Function FolderDialogSelection(Optional ByVal fFolder As String = "") As String
'' Create a new Folder Dialog object, starting at the given folder path if required.
'' Given path is created if it doesn't already exist.
     DebugLine("FolderDialogSelection",bVerboseDebug_clsFileMgmt,1,,2)
    Dim oFileDlg As New FolderBrowserDialog
    Dim sMessage As String

    sMessage = "Please Select Location for File Output"
     With oFileDlg
                .Description = sMessage
                .ShowNewFolderButton = True
                .SelectedPath = fFolder
                '.RootFolder = Environment.SpecialFolder.MyComputer

     End With
    
     oFileDlg.ShowDialog()
     FolderDialogSelection = oFileDlg.SelectedPath & "\"
     DebugLine("FolderDialogSelection",bVerboseDebug_clsFileMgmt,4,FolderDialogSelection,2)
End Function


Private Function ConvertSEIProject(ByVal fName As String)
'' Convert from 20##B0NNN structure to S##NNN String
 Dim sConverted As String
 DebugLine("ConvertSEIProject", , 2, fName)
 ' bVerboseDebug_clsFileMgmt
  If Left(fName,2) = "20"
   Try
      sConverted = "S" & Mid(fName,3,2) & Right(fName,3)
      DebugLine("ConvertSEIProject", , 4, sConverted)
     Return sConverted
    Catch ex As Exception
      Echo("Error in ConvertSEIProject " & ex.Message)
     Return ""
   End Try
  End If
  Return fName
End Function

End Class
'--Class iLogic End--------------------------------------------------------------------------------------------------
