'Check if SolidWorks is running

Dim i
Dim strComputer
Dim FindProc
 
strComputer = "."

FindProc = "sldworks.exe"

Set objWMIService = GetObject("winmgmts:" _
    & "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")
Set colProcessList = objWMIService.ExecQuery _
    ("Select Name from Win32_Process WHERE Name LIKE '" & FindProc & "%'")

If colProcessList.count>0 then
    'SolidWorks is Running

else
    Set shell = createobject("wscript.shell")
    'shell.popup "SolidWorks is not running! Please run SolidWorks and try again.", 0, "Error", 48
    WScript.Quit
End if

Set objWMIService = Nothing
Set colProcessList = Nothing




'Open files assuming SolidWorks is running



Set swApp = CreateObject("SldWorks.Application")
swApp.Visible = True

'If you want this file to open parts, set variable "openParts" to True. Otherwise, set it to False.

Dim openParts
openParts = False

Dim filePath
filePath = WScript.Arguments.Item(0)

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objFile = objFSO.GetFile(filePath)

If filePath <> "" then

	Dim docSpec
	Set docSpec = swApp.GetOpenDocSpec(filePath)

	If objFile.Type = "SOLIDWORKS Drawing Document" Then
		docSpec.DetailingMode = True
	ElseIf objFile.Type = "SOLIDWORKS Assembly Document" Then
		docSpec.ViewOnly = True
	ElseIf openParts AND objFile.Type = "SOLIDWORKS Part Document" Then
		docSpec.ViewOnly = True
	Else
		WScript.Quit
	End If

	Dim swModel
	Set swModel = swApp.OpenDoc7(docSpec)

	If swModel is Nothing Then
		MsgBox "Failed to open document"
	End If
	
Else
	MsgBox "File path is not specified"
End If