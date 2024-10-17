
'____iLogic Definition for Library/Standard Part_______
AddVbFile "VB - clsDebugViewer.vb"
AddVbFile "VB - clsFileProperties.vb"

Imports clsDebugViewer

Public Class hp6xEF
''' Contains the instructions to resize a full featured part.

#Region "Shared Declarations"
  '' All Public Shared Variables and Settings
  Public Shared Dim listParts As New List(Of String)
  Public Shared Dim prp As New clsFileProperties
  ''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"

#End Region


  Public Sub Main()
    Dim oDoc As Document = ThisDoc.Document
    Dim sSelection As String
    Dim tValue As PartTable

    ''Populate the variable lists
    Call InitializePropertyTable()

    ''Prompt for the selection size
    sSelection = InputListBox("Select Part Number", listParts, "" , Title := "Standard Part Selection", ListName := "Available Parts")
    If sSelection = "" Then
      Echo("Nothing Selected Setting Values to ->" & listParts.Item(0))
	  Call ResetPart(listParts.Item(0))
	  oDoc.Update2
	  Exit Sub
	End If
	  
	  
    ''Populate the part with the appropriate values
    Call PopulatePart(sSelection)

   oDoc.Update2

  End Sub


#Region "Methods"
 ''Actionable Functions or Sub Methods


  Public Sub PopulatePart(Size As String)
    ' TryGetValue() Gets TValue for TKey and assigns it to variable, also passes T/F Boolean
    ' to register if the Key was found in the Table

    'Set reference to custom class'
    Dim tValue As PartTable

    If tParts.TryGetValue(Size, tValue) Then
      tValue.Apply(ThisDoc.Document)
     Else
      Console.WriteLine("Key = " & Size & " is not found.")
      Exit Sub
    End If

  End Sub


  Public Sub ResetPart(Size As String)
    ' TryGetValue() Gets TValue for TKey and assigns it to variable, also passes T/F Boolean
    ' to register if the Key was found in the Table

    'Set reference to custom class'
    Dim tValue As PartTable
	Dim sListDesc As String = ""
	Dim itm As String
	
	'Populate the description for Library Setting
	For Each itm In listParts
	 sListDesc = sListDesc & itm & " ; "
	Next

	sListDesc = Left(sListDesc, Max(Len(sListDesc) - 2 , 1))

    If tParts.TryGetValue(Size, tValue) Then
      tValue.ResetTo(ThisDoc.Document, sListDesc)
     Else
      Console.WriteLine("Key = " & Size & " is not found.")
      Exit Sub
    End If

  End Sub


'''
'''
''' *** Modification Begins Here ******************************
'''
'''


  Public Sub InitializePropertyTable()
    '''Assign values to dictionary(key) using the defined 'new' function
    '''Populate the available parts (listParts) for selection

    '''---Standard Parts--- **Template is only Set for Three(3) Variables**
    tParts("W710") = New PartTable( "W710" , "6.25" ,  ,  ):listParts.Add("W710")
    tParts("W7114") = New PartTable( "W7114" , "11" ,  ,  ):listParts.Add("W7114")
    tParts("W7115") = New PartTable( "W7115" , "12 +  1.75" ,  ,  ):listParts.Add("W7115")
    tParts("W7116") = New PartTable( "W7116" , "12 +  6.25" ,  ,  ):listParts.Add("W7116")

    '''---Galvanized Parts--- **Template is only Set for Three(3) Variables**
    tParts("0W710") = New PartTable( "0W710" , "6.25" ,  , ):listParts.Add("0W710")
    tParts("0W7114") = New PartTable( "0W7114" , "11" ,  , ):listParts.Add("0W7114")
    tParts("0W7115") = New PartTable( "0W7115" , "12 +  1.75" ,  , ):listParts.Add("0W7115")
    tParts("0W7116") = New PartTable( "0W7116" , "12 +  6.25" ,  , ):listParts.Add("0W7116")

    '''---Material Variation Parts--- **Template is only Set for Three(3) Variables**
    tParts("W710-1") = New PartTable( "W710-1" , "6.25" ,  ,  ):listParts.Add("W710-1")
    tParts("W7114-1") = New PartTable( "W7114-1" , "11" ,  ,  ):listParts.Add("W7114-1")
    tParts("W7115-1") = New PartTable( "W7115-1" , "12 +  1.75" ,  ,  ):listParts.Add("W7115-1")
    tParts("W7116-1") = New PartTable( "W7116-1" , "12 +  6.25" ,  ,  ):listParts.Add("W7116-1")

  End Sub

#End Region


  Public tParts As New Dictionary(Of String, PartTable) 


  Public Class PartTable
  '''This is the definition of the standard part dimensions. 
  '''All controlled need to be included here and in the InitializePropertyTable() method above.

  'Setup Variables/FeatureNames to be Tracked in Table/Array
   ''Define variables as type: 
    ''   Double for 1.0, 2.0, ..., 00000.00; 
    ''   String for 10A-AAZ
   Private _listParam As New SortedList()
   Private _listIProp As New SortedList()
   Private _Title As String
   Private _PartNumber As String
   Private _DrawingNo As String 


  Public Sub New(PartNumber As String,
                Optional dA As String = "",
                Optional dB As String = "",
                Optional dC As String = "")
  '''Identify the Parameters and Values to be Populated
    If dA <> String.Empty Then _listParam.Add("d:A", dA)
    If dB <> String.Empty Then _listParam.Add("d:B", dB)
    If dC <> String.Empty Then _listParam.Add("d:C", dC)

  '''Set to Drawing Number the Design is Being Made From;
   _DrawingNo  = "W710"

  '''Set Title of the Design;
   _Title  = "FRAME LIFTING LUG"

  '''Set the Description Format, it is possible to feed from another variable
   _listIProp.Add("Description", "LUG, LIFTING, FRAME") ''dA & """" & " x " & dB & """" & " x " & dC & """")

  '''Set the Part Number from variable 
   _listIProp.Add("Part Number", PartNumber): _PartNumber = PartNumber 

  End Sub


'''
'''
''' *** Modification Ends Here ******************************
'''
'''


   Public Sub Apply(oDoc As PartDocument)
   '''Set Part 'Values'
    'Set Custom iProperty to Code Revision State 
     prp.SetiPropValue(oDoc,
                       "Standard Library Part",
                       _DrawingNo,
                       "Hudson Properties")

     'Add Note Property for -possible- Future Use
     prp.SetiPropValue(oDoc,
                       "Std Part Note",
                       "NOTE: This part is detailed on Drawing No. " & _DrawingNo & " as PN:" & PartNumber & ".",
                       "Hudson Notes")

     For i = 0 To _listParam.Count - 1
       Echo( "Key:  "& _listParam.GetKey(i) & "  -- Value:  " & _listParam.GetByIndex(i))
       oDoc.ComponentDefinition.Parameters(_listParam.GetKey(i)).Expression = _listParam.GetByIndex(i)
     Next i

     For i = 0 To _listIProp.Count - 1
       Echo( "Key:  "& _listIProp.GetKey(i) & "  -- Value:  " & _listIProp.GetByIndex(i))
       prp.SetiPropValue(oDoc, _listIProp.GetKey(i) , _listIProp.GetByIndex(i))
     Next i

     oDoc.DisplayName = _listIProp("Part Number")

   End Sub

   Public Sub ResetTo(oDoc As PartDocument, Desc As String)
   '''Reset Part to 'Library Status'
    'Set Custom iProperty to Code Revision State
     prp.SetiPropValue(oDoc,
                       "Standard Library Part",
                       _DrawingNo,
                       "Hudson Properties")

     'Add Note Property for -possible- Future Use
     prp.SetiPropValue(oDoc,
                       "Std Part Note",
                       "NOTE: This part is detailed on Drawing No. " & _DrawingNo & " as PN:" & _PartNumber & ".",
                       "Hudson Notes")


     For i = 0 To _listParam.Count - 1
       Echo( "Key:  "& _listParam.GetKey(i) & "  -- Value:  " & _listParam.GetByIndex(i))
       oDoc.ComponentDefinition.Parameters(_listParam.GetKey(i)).Expression = _listParam.GetByIndex(i)
     Next i

     For i = 0 To _listIProp.Count - 1
       Echo( "Key:  "& _listIProp.GetKey(i) & "  -- Value:  " & _listIProp.GetByIndex(i))
       prp.SetiPropValue(oDoc, _listIProp.GetKey(i) , "")
     Next i

     oDoc.DisplayName = _DrawingNo
     prp.SetiPropValue(oDoc,
                       "Description",
                       "Library Part for :: " & Desc)
     prp.SetiPropValue(oDoc,
                       "Part Number",
                       _DrawingNo)


   End Sub

  End Class 

End Class



