oDoc = ThisDoc.Document
oParameters = oDoc.ComponentDefinition.Parameters
volume = iProperties.Volume

Dim dflt As String
Dim sNames As New List(Of String)
For Each Prm In oParameters
	sNames.Add(Prm.Name)
	If Prm.Name.Contains("THK") Then dflt = Prm.Name
Next Prm

Dim sParam As String
sParam = InputListBox("Part Thickness", sNames, dflt, Title := "Select", ListName := "Parameters")

Dim Param As Parameter
Param = oParameters.Item(sParam)
'Param.Value

Dim dblVol As Double
dblVol = Round(oDoc.ComponentDefinition.MassProperties.Volume,6)

Dim dblArea As Double
dblArea = Round( ( dblVol / Param.Value ) / ( 2.54 * 2.54 ) / 144 ,6)

''MsgBox("Thickness: " & Chr(13) & "           " & Param.Value/2.54 & " in " & Param.Value & " cm"  & Chr(13) &
''	   "Volume:    " & Chr(13) & "           " & Round(iProperties.Volume,6) & "in^3" & Chr(13) &
''	   "           " & dblVol & "cm^3" & Chr(13) & Chr(13) & 
''	   "Area:      " & Chr(13) & "           " & Round(dblVol / Param.Value,6) & "cm^2" & Chr(13) &
''	   "           " & Round( ( dblVol / Param.Value ) / ( 2.54 * 2.54 ),6) & "in^2" & Chr(13) &
''	   "           " & Round( ( dblVol / Param.Value ) / ( 2.54 * 2.54 ) / 144 ,6)& "ft^2")
'MsgBox("Thickness: " & Chr(13) & 
'	   "           " & Param.Value/2.54 & " in " & Chr(13) &
'	   "Volume:    " & Chr(13) & 
'	   "           " & Round(iProperties.Volume,6) & "in^3" & Chr(13) &
'	   "Area:      " & Chr(13) & 
'	   "           " & dblArea & "ft^2")
	   
'sParam = InputListBox("Thickness", sNames, oParameters.Item(1).Name, Title := "Title", ListName := "List")
'Dim aParam As Parameter
'aParam = oParameters.Item(sParam)
'aParam.Expression = dblArea & "ft^2"

iProperties.Value("Custom", "RM Qty") = Round(dblArea,3)



iProperties.Value("Custom", "RM UoM") = "SF"
