' <IsStraightVb>True</IsStraightVb>
Imports clsDebugViewer
Imports Inventor
Imports System.Windows.Forms


'''_______________ Child Sub Reqs Start Here _______________
  'AddVbFile "VB - clsFileProperties.vb"
  'AddVbFile "VB - clsDebugViewer.vb"

  'Sub Main()
  ' Dim prp As New clsFileProperties
  '''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"
  'End Sub
'''_______________ Child Sub Reqs End Here _______________


Public Class clsFileProperties
'' Public Functions:: 
''     DocumentType(Document, String, String, Boolean)
''     CheckiPropExists(Document, String, {String}) As Boolean
''     GetiPropValue(Document, String, {String}) As String
''     SetiPropValue(Document, String, String, {String}) As String
''     GetParamValue(Document, String) As String
''     TrimSheetName(String)
''     
'' Public Subs::
''     FillUserDefinediProp(Document, String, String)
''     EchoParameters(PartDocument or AssemblyDocument)
''     DeleteParameter

#Region "Declarations"
 '--------------------------------------------------------------------------
 ''Class Public Variable Declarations
 Public Dim bVerboseDebug_clsFileProperties As Boolean = False
 'Public Dim flm As New clsFileMgmt
  '''Use 'flm.' as a prefix for any method called from "VB - clsFileMgmt.vb"
 
 Public Dim sCustomPropertySet As PropertySet
 Public Dim sTestValue As String
 
 '--------------------------------------------------------------------------
#End Region

#Region "Document Methods"

  Public Function TrimSheetName(ByVal sName As String) As String
      If InStrRev(sName,":") > 0 Then sName = Left(sName, InStrRev(sName,":")-1)
      If InStrRev(sName,"_") > 0 Then sName = Left(sName, InStrRev(sName,"_")-1)
   Return sName
  End Function


  Public Function DocumentType(ByVal fDocument As Document,
                      Optional ByRef fTypeExpected As String = "",
                      Optional ByRef fSubType As String = "",
                      Optional ByRef bTypeExpected As Boolean = True)
  ''Identify document type and subtype, Return Actual type VALUE

    Dim oDoc as Document
    oDoc = fDocument
    EchoLine()
    DebugLine("DocumentType Enumerator ",,1)
    Echo("Name --                      Value -- Description")
    Select Case oDoc.DocumentType
    Case 12291
      Echo("kAssemblyDocumentObject      12291 Assembly Document. " & oDoc.SubType)
      If oDoc.SubType = "{28EC8354-9024-440F-A8A2-0E0E55D635B0}" Then fSubType = "Weldment"
      If oDoc.SubType = "{E60F81E1-49B3-11D0-93C3-7E0706000000}" Then fSubType = "Assembly"

      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12294
      Echo("kDesignElementDocumentObject 12294 Design Element Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12292
      Echo("kDrawingDocumentObject       12292 Drawing Document. " & oDoc.SubType)
      If oDoc.SubType = "{BBF9FDF1-52DC-11D0-8C04-0800090BE8EC}" Then fSubType = "Drawing"

      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12295
      Echo("kForeignModelDocumentObject  12295 Foreign Model Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12297
      Echo("kNoDocument                  12297 No Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12290
      Echo("kPartDocumentObject          12290 Part Document. " & oDoc.SubType)
      If oDoc.SubType = "{4D29B490-49B2-11D0-93C3-7E0706000000}" Then fSubType = "Part"
      If oDoc.SubType = "{9C464203-9BAE-11D3-8BAD-0060B0CE6BB4}" Then fSubType = "Sheet Metal"

      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12293
      Echo("kPresentationDocumentObject  12293 Presentation Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12296
      Echo("kSATFileDocumentObject       12296 SAT File Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case 12289
      Echo("kUnknownDocumentObject       12289 Unknown Document. " & oDoc.SubType)
      If fTypeExpected = ""
      EchoLine():Return oDoc.DocumentType
      Else
      bTypeExpected = (fTypeExpected = oDoc.DocumentType)
      End If

    Case Else
      Echo(oDoc.DocumentType)

    End Select

    EchoLine()
    ' BreakTitle("File & Document Type Enumerators")
    ' Echo("Name --                      Value -- Description")
    ' Echo("")
    ' Echo("DocumentTypeEnum Enumerator ")
    ' Echo("kAssemblyDocumentObject      12291 Assembly Document. ")
    ' Echo("kDesignElementDocumentObject 12294 Design Element Document. ")
    ' Echo("kDrawingDocumentObject       12292 Drawing Document. ")
    ' Echo("kForeignModelDocumentObject  12295 Foreign Model Document. ")
    ' Echo("kNoDocument                  12297 No Document. ")
    ' Echo("kPartDocumentObject          12290 Part Document. ")
    ' Echo("kPresentationDocumentObject  12293 Presentation Document. ")
    ' Echo("kSATFileDocumentObject       12296 SAT File Document. ")
    ' Echo("kUnknownDocumentObject       12289 Unknown Document. ")
    ' Echo("")
    ' Echo("DefaultDrawingFileTypeEnum Enumerator ")
    ' Echo("kDWGDefaultDrawingFileType   69633 DWGDefaultDrawingFileType. ")
    ' Echo("kIDWDefaultDrawingFileType   69634 IDWDefaultDrawingFileType. ")
    ' Echo("")
    ' Echo("FileTypeEnum Enumerator ")
    ' Echo("kAssemblyFileType            56323 An assembly file. ")
    ' Echo("kAssociativeCADFileType      56328 An associative CAD file. ")
    ' Echo("kDesignElementFileType       56326 A design element file. ")
    ' Echo("kDrawingFileType             56324 A drawing file. ")
    ' Echo("kForeignFileType             56327 A foreign file. ")
    ' Echo("kPartFileType                56322 A part file. ")
    ' Echo("kPresentationFileType        56325 A presentation file. ")
    ' Echo("kUnknownFileType             56321 An unknown type of file. ")


  End Function

#End Region

#Region "iProperties"
  Public Function CheckiPropExists(ByVal fDoc As Document,
                                   ByVal PropName As String,
                          Optional ByRef PropSet As String = "") As Boolean
  ''Check and see if a certain property exists
    CheckValue = GetiPropValue(fDoc, PropName, PropSet) 
    If CheckValue <> "" Then' Or PropSet <> "" Then '_ 
                '   Or PropSet = "Inventor Summary Information" _ 
                '   Or PropSet = "Inventor Document Summary Information" _ 
                '   Or PropSet = "Design Tracking Properties" _ 
                '   Or PropSet = "Inventor User Defined Properties" _ 
                ' Then
      Echo("Property " & PropName & " is found in " & PropSet & ".",15, bVerboseDebug_clsFileProperties )
      Return True
     Else
        If PropSet <> "" Then 
              Echo("Property " & PropName & " is found in " & PropSet & ".",15, bVerboseDebug_clsFileProperties )
              Return True
        End If
        Echo(PropSet)
        Return False
    End If
  End Function


  Public Function GetiPropValue(ByVal fDoc As Document,
                                ByVal PropName As String,
                       Optional ByRef PropSet As String = "") As String
  ''Get Value for Given PropertyName in Given Document
   DebugLine("Get iProperty",bVerboseDebug_clsFileProperties,2,PropName)

     Dim sPropSetName As String

   For Each oPropSet In fDoc.PropertySets
    sPropSetName = oPropSet.Name
    'Echo(sPropSetName)

    For Each oProp In oPropSet
     'Echo(" --- " & oProp.Name)
      If UCase(oProp.Name) = UCase(PropName)
        PropSet = sPropSetName

        If Not ( oProp.Name = "" Or oProp.Name = String.Empty )
          GetiPropValue = oProp.Value
          DebugLine("Found in Set (""" & sPropSetName & """)",bVerboseDebug_clsFileProperties,4,GetiPropValue)
         Exit Function
        End If

       Echo("iProperty " & PropName & " Found in Set (""" & sPropSetName & """)", bVerboseDebug_clsFileProperties )
       Return ""
      End If
    Next
   Next

    DebugLine("iProperty Not Found",bVerboseDebug_clsFileProperties,3)
    Return ""

  End Function


  Public Function SetiPropValue(ByVal fDoc As Document,
                                ByVal PropName As String,
                                ByVal PropValue As String,
                       Optional ByVal PropSetName As String _ 
                                 = "Inventor User Defined Properties") As String
  ''Set Value for Given PropertyName in Given Document
   DebugLine("Set iProperty",bVerboseDebug_clsFileProperties,2, PropName & " - " & PropValue)
   Dim sPropSet As String
   Dim bExists As Boolean
    Try
      ''Get Inventor Property Sets
      bExists = CheckiPropExists(fDoc, PropName, sPropSet)
      If bExists 'iProperty was found
        Try
          oPropertySet = fDoc.PropertySets.Item(sPropSet)
          oPropertySet.Item(PropName).Value = PropValue

        Catch ex As Exception
          ' MessageBox.Show("Problem with " & fDoc.DisplayName & Chr(13) & _ 
          '           "Trying to Set Property Sets." & Chr(13)  & Chr(13) & _ 
          '           "Exception Thrown: " & ex.Message, fDoc.DisplayName)
          Echo("Problem with SetiPropValue (bExists = True) in" & fDoc.DisplayName & " bExists:" & bExists)
          Echo("  \ ---  Trying to Set Property Sets." )
          Echo("   \ --  Exception Thrown: " & ex.Message)
        End Try

      Else 'iProperty needs to be made
        FillUserDefinediProp(fDoc, PropName, PropValue, PropSetName)

      End If

    Catch ex As Exception 'possible mistake in finding.. trying to just set again
        oPropertySet = fDoc.PropertySets.Item(sPropSet)
        oPropertySet.Item(PropName).Value = PropValue
      ' MessageBox.Show("Problem with " & fDoc.DisplayName & Chr(13) & _ 
      '             "Trying to Get Property Sets." & Chr(13)  & Chr(13) & _ 
      '             "Exception Thrown: " & ex.Message, fDoc.DisplayName)
        Echo("Problem with SetiPropValue in " & fDoc.DisplayName )
        Echo("  \ ---  Trying to Set Property Sets." )
        Echo("   \ --  Exception Thrown: " & ex.Message)
    End Try

  End Function


  Public Sub FillUserDefinediProp(ByVal fDoc As Document,
                                  ByVal PropName As String,
                                  ByVal PropValue As String,
                         Optional ByVal PropSetName As String = "Inventor User Defined Properties")
  ''Assign Given Value to Given PropertyName in Given Document

  
    Try
    ''Get Inventor User Defined Property Set
    oPropSets = fDoc.PropertySets
    If Not oPropSets.PropertySetExists(PropSetName) Then oPropSets.Add(PropSetName)

    sCustomPropertySet = oPropSets.Item(PropSetName)

    Catch ex As Exception
      ' MessageBox.Show("Problem with " & fDoc.DisplayName & Chr(13) & _ 
      '             "Trying to Set " & PropName & " to " & PropValue & Chr(13)  & Chr(13) & _ 
      '             "Exception Thrown: " & ex.Message, fDoc.DisplayName)
        Echo("Problem with FillUserDefinediProp in " & fDoc.DisplayName )
        Echo("  \ ---  Trying to Set Property Sets." )
        Echo("   \ --  Exception Thrown: " & ex.Message)
    End Try

    Try
    ''Assign Value to Defined PropertyName, Creating Parameter if needed.
      sTestValue = sCustomPropertySet.Item(PropName)

    Catch ex As Exception
      ''Assume error means not found
      sCustomPropertySet.Add("", PropName)

    Finally
      sCustomPropertySet.Item(PropName).Value = PropValue
      DebugLine(PropName,,4,PropValue,2)
    End Try
  End Sub


  Public Sub EchoIProperties(ByVal fDoc As Document)
    ''Echo Values for All Properties PropertyName in Given Document
    DebugLine("EchoIProperties",, 2)

    Dim sPropSetName As String

    For Each oPropSet In fDoc.PropertySets
      sPropSetName = oPropSet.Name
      BreakTitle(sPropSetName)
          Echo(
              "Name" & Space(Max(1,30 - Len("Name"))) &
              "Value") : DebugLine(sMessage,,,,150)
      For Each oProp In oPropSet
      'Echo(" --- " & oProp.Name)
    ' If oProp.Value
      Try
        If Len(oProp.Value) > 0
        Echo(
            oProp.Name & Space(Max(1,25- Len(oProp.Name))) &
            oProp.Value
            )
            End If
      Catch
      End Try
    ' End If
      Next
   Next

  End Sub


#End Region

#Region "Parameter Modification"
  Public Function GetParamValue(ByVal fDoc As PartDocument, 
                                ByVal param As String) As String
  ''Cycles through all available parameters and returns native value
    Dim oParameterSet As Parameters
    Dim oParameter As Parameter
    Dim bExists As Boolean = False
    ' Dim oUOM As UnitsOfMeasure _ 
    '          = fDoc.UnitsOfMeasure

    ' Dim utype as Inventor.UnitsTypeEnum
      ' Select Case unittype
      '   Case = "Angle"
      '     utype = uom.AngleUnits
      '   Case = "Length"
      '     utype = uom.LengthUnits
      '   Case = "Mass"
      '     utype = uom.MassUnits
      '   Case Else
      '     MsgBox("Improper Unit Selection for Parameter " & param & " in Part " & flm.GetFileName(fDoc))
      ' End Select


    Try
      oParameterSet = fDoc.ComponentDefinition.Parameters
     Catch
      oParameterSet = fDoc.Parameters
    End Try


    Try
      For Each oParameter In oParameterSet
        If oParameter.Name = param 'Then exists = True
         Echo(oParameter.Name & " is found.", bVerboseDebug_clsFileProperties)
          sTestValue = oParameter.Value 'uom.ConvertUnits(oparams.Item(param).Value, unit, utype)
          GetParamValue = sTestValue
          DebugLine("GetParamValue", bVerboseDebug_clsFileProperties, 4, sTestValue)
          Return GetParamValue
        End If
      Next 

    Catch ex As Exception
      DebugLine("GetParamValue", , 5, ex.Message)

    End Try
  End Function

  Public Function GetParamExpression(ByVal fDoc As PartDocument, 
                                ByVal param As String) As String
  ''Cycles through all available parameters and returns native value
    Dim oParameterSet As Parameters
    Dim oParameter As Parameter
    Dim bExists As Boolean = False
    ' Dim oUOM As UnitsOfMeasure _ 
    '          = fDoc.UnitsOfMeasure

    ' Dim utype as Inventor.UnitsTypeEnum
      ' Select Case unittype
      '   Case = "Angle"
      '     utype = uom.AngleUnits
      '   Case = "Length"
      '     utype = uom.LengthUnits
      '   Case = "Mass"
      '     utype = uom.MassUnits
      '   Case Else
      '     MsgBox("Improper Unit Selection for Parameter " & param & " in Part " & flm.GetFileName(fDoc))
      ' End Select


    Try
      oParameterSet = fDoc.ComponentDefinition.Parameters
     Catch
      oParameterSet = fDoc.Parameters
    End Try


    Try
      For Each oParameter In oParameterSet
        If oParameter.Name = param 'Then exists = True
         Echo(oParameter.Name & " is found.", bVerboseDebug_clsFileProperties)
          sTestValue = oParameter.Expression 'uom.ConvertUnits(oparams.Item(param).Value, unit, utype)
          DebugLine("GetParamExpression", bVerboseDebug_clsFileProperties, 4, sTestValue)
          Return sTestValue
        End If
      Next 

    Catch ex As Exception
      DebugLine("GetParamValue", , 5, ex.Message)

    End Try
  End Function

  Public Function GetParamValue(ByVal fDoc As AssemblyDocument, 
                                ByVal param As String) As String
  ''Cycles through all available parameters and returns native value
    Dim oParameterSet As Parameters
    Dim oParameter As Parameter
    Dim bExists As Boolean = False
    ' Dim uom as UnitsofMeasure = fDoc.UnitsofMeasure
    ' Dim utype as Inventor.UnitsTypeEnum
      ' Select Case unittype
      '   Case = "Angle"
      '     utype = uom.AngleUnits
      '   Case = "Length"
      '     utype = uom.LengthUnits
      '   Case = "Mass"
      '     utype = uom.MassUnits
      '   Case Else
      '     MsgBox("Improper Unit Selection for Parameter " & param & " in Part " & flm.GetFileName(fDoc))
      ' End Select


    Try
      oParameterSet = fDoc.ComponentDefinition.Parameters
     Catch
      oParameterSet = fDoc.Parameters
    End Try


    Try
      For Each oParameter In oParameterSet
        If oParameter.Name = param 'Then exists = True
         Echo(oParameter.Name & " is found.")
          sTestValue = oParameter.Value 'uom.ConvertUnits(oparams.Item(param).Value, unit, utype)
          GetParamValue = sTestValue
          DebugLine("GetParamValue", bVerboseDebug_clsFileProperties, 4, sTestValue)
          Return GetParamValue
        End If
      Next 

    Catch ex As Exception
      DebugLine("GetParamValue", , 5, ex.Message)

    End Try
  End Function

  Public Sub DeleteParameter(ByVal fDoc As Document, 
                             ByVal sParamName As String)
  ''Delete Add User Parameter
   BreakTitle("Delete Parameter")
    Dim oDoc As Document
    Dim oUserParameterSet As UserParameters
    Dim param As Parameter


   Try
    ' Get the active document.  Assumes a part document is active.
    oDoc = fDoc

    ' Get the UserParameters collection
    oUserParameterSet = oDoc.Parameters.UserParameters

   Catch ex As Exception
   DebugLine("DeleteParameter", , 5, ex.Message)
    oUserParameterSet = oDoc.ComponentDefinition.Parameters.UserParameters
     
   End Try

    For Each param in oUserParameterSet
     If param.Name = sParamName 
       param.Delete
       Exit Sub
      End If
    Next


  End Sub


  Public Sub CreateParameter(ByVal fDoc As Document, 
                                ByVal sParamName As String,
                                ByVal sParamValue As String,
                                ByVal sParameterUnitType As Integer,
                                Optional ByVal bDelete As Boolean = False)
  ''Add User Parameter
    '' sParameterUnitType Can Equal
    ''  kText Units (Text Object)
    ''  kBoolean Units (True False)
    ''  kMillimeter Units (mm)
    '' and all others in the UnitsTypeEnum Enumerator typelist
    BreakTitle("Create Parameter")

    '-+-' successful in creating a parameter and making it a list, only one valute is populating though.
    Dim oDoc As Document _ 
             = fDoc
    Dim userParams As UserParameters
    Dim param, prm As Parameter
    Dim bExists As Boolean _ 
                = False

    Try
    ' Get the UserParameters collection
     userParams = oDoc.Parameters.UserParameters
    Catch
     userParams = oDoc.ComponentDefinition.Parameters.UserParameters
    End Try

    For Each prm in userParams
      If prm.Name = sParamName Then bExists = True
      If bExists And bDelete 
       prm.Delete
       bExists = False
      End If
      If bExists And Not bDelete 
       prm.Value = sParamValue
       Exit Sub
      End If
    Next

    Try
     param = userParams.AddByExpression(sParamName, sParamValue, sParameterUnitType)
     'param=userParams.AddByExpression(sParamName, sParamValue, sParameterUnitType)
    Catch ex As Exception
     Echo("AddByExpression Failed")
     param = userParams.AddByValue(sParamName, sParamValue, sParameterUnitType)
    End Try

     Echo("Parameter Set to " & param.Value)
  End Sub


  Public Sub CreateParameter(ByVal fDoc As Document, 
                                ByVal sParamName As String,
                                ByVal listParamValues() As String,
                                ByVal sParameterUnitType As Integer,
                                Optional ByVal bDelete As Boolean = False)
  ''Add User Parameter
    '' sParameterUnitType Can Equal
    ''  kText Units (Text Object)
    ''  kBoolean Units (True False)
    ''  kMillimeter Units (mm)
    '' and all others in the UnitsTypeEnum Enumerator typelist
    BreakTitle("Create Parameter")

    Dim oDoc As Document = fDoc
    Dim userParams As UserParameters
    Dim param As Parameter
    Dim bExists As Boolean = False
    'Dim listParameters(2) As String
    'Dim listParameters = New String() {"""P1""","""P2""","""P3"""}

    Try
    ' Get the UserParameters collection
     userParams = oDoc.Parameters.UserParameters
    Catch
     userParams = oDoc.ComponentDefinition.Parameters.UserParameters
    End Try

    For Each prm in userParams
      If prm.Name = sParamName Then bExists = True
      If bExists And bDelete 
       prm.Delete
       bExists = False
      End If
      If bExists And Not bDelete 
       prm.Value = sParamValue
       Exit Sub
      End If
    Next

    Try
     param=userParams.AddByExpression(sParamName, sParamValue, sParameterUnitType)
     'param=userParams.AddByExpression(sParamName, sParamValue, sParameterUnitType)
    Catch ex As Exception
     Echo("AddByExpression Failed")
     param=userParams.AddByValue(sParamName, sParamValue, sParameterUnitType)
    End Try

     Echo("Parameter Set to " & param.Value)

      param.ExpressionList.SetExpressionList(listParamValues)
      cnt = param.ExpressionList.Count
     
  End Sub

  Public Sub EchoParameters(ByRef oDoc As Document,
                            ByRef cdDoc As ComponentDefinition)
  '' Iterate through the Parameters collection to obtain
  '' and display information about the Parameters

    Dim sType, sName, sValue, sHealth, sUnit As String
    Dim oParams As Parameters _ 
             = cdDoc.Parameters
    Dim oUOM As UnitsOfMeasure _ 
             = oDoc.UnitsOfMeasure

    DebugLine("EchoParameters - Document",,1)


    Dim iNumParams As Long
   BreakTitle("All Available Parameters")
        Echo("Type" & Space(Max(0,25 - Len("Type"))) &
             "Name" & Space(Max(1,25 - Len("Name"))) &
             "Value") : DebugLine(sMessage,,,,150)' EchoLine(,,False)
    For iNumParams = 1 To oParams.Count
        sName = oParams.Item(iNumParams).Name

        Select Case oParams.Item(iNumParams).Type
            Case ObjectTypeEnum.kModelParameterObject
                sType = "Model Parameter"
            Case ObjectTypeEnum.kTableParameterObject
                sType = "Table Parameter"
            Case ObjectTypeEnum.kUserParameterObject
                sType = "User Parameter"
            Case Else
                sType = "Unknown"
        End Select

        sValue = CStr(oParams.Item(iNumParams).Value)

        Select Case oParams.Item(iNumParams).HealthStatus
            Case HealthStatusEnum.kDeletedHealth
                    sHealth = Space(5) & "Health Status: " & "Deleted"
            Case HealthStatusEnum.kDriverLostHealth
                    sHealth = Space(5) & "Health Status: " & "Driver Lost"
            Case HealthStatusEnum.kInErrorHealth
                    sHealth = Space(5) & "Health Status: " & "In Error"
            Case HealthStatusEnum.kOutOfDateHealth
                    sHealth = Space(5) & "Health Status: " & "Out of Date"
            Case HealthStatusEnum.kUnknownHealth
                    sHealth = Space(5) & "Health Status: " & "Unknown"
            Case HealthStatusEnum.kUpToDateHealth
                    sHealth = Space(5) & "Health Status: " & "Up to Date"
        End Select

      Echo(sType & Space(Max(1,25 - Len(sType))) &
           sName & Space(Max(1,25 - Len(sName))) &
           sValue & Space(Max(1,15-(Len(sValue)))) &
           sHealth
           )

    Next iNumParams

    ' Obtain the Model Parameters collection
    Dim oModelParams As ModelParameters _ 
             = oParams.ModelParameters

    ' Iterate through the Model Parameters collection
    Dim iNumModelParams As Long
   BreakTitle("Only Model Values")
    For iNumModelParams = 1 To oModelParams.Count
        sName = oModelParams.Item(iNumModelParams).Name
        sValue =  oModelParams.Item(iNumModelParams).Value
        sUnit = oModelParams.Item(iNumModelParams).Units
        sValue = oUOM.GetStringFromValue(sValue, sUnit)

      Echo(Space(25) &
           sName & Space(Max(1,20 - ( Len(sValue) + Len(sName) ))) &
           sValue )' & Space(Max(1,17 - Len(sValue))) & sUnit )

    Next iNumModelParams

    ' Update the model.
     oDoc.Update
  End Sub

  Public Sub EchoParameters(ByRef oDoc As PartDocument)
  '' Iterate through the Parameters collection to obtain
  '' and display information about the Parameters

    Dim sType, sName, sValue, sHealth, sUnit As String
    Dim oParams As Parameters _ 
             = oDoc.ComponentDefinition.Parameters
    Dim oUOM As UnitsOfMeasure _ 
             = oDoc.UnitsOfMeasure

    DebugLine("EchoParameters - PartDocument",,1)


    Dim iNumParams As Long
   BreakTitle("All Available Parameters")
        Echo("Type" & Space(Max(0,25 - Len("Type"))) &
             "Name" & Space(Max(1,25 - Len("Name"))) &
             "Value") : DebugLine(sMessage,,,,150)' EchoLine(,,False)
    For iNumParams = 1 To oParams.Count
        sName = oParams.Item(iNumParams).Name

        Select Case oParams.Item(iNumParams).Type
            Case ObjectTypeEnum.kModelParameterObject
                sType = "Model Parameter"
            Case ObjectTypeEnum.kTableParameterObject
                sType = "Table Parameter"
            Case ObjectTypeEnum.kUserParameterObject
                sType = "User Parameter"
            Case Else
                sType = "Unknown"
        End Select

        sValue = CStr(oParams.Item(iNumParams).Value)

        Select Case oParams.Item(iNumParams).HealthStatus
            Case HealthStatusEnum.kDeletedHealth
                    sHealth = Space(5) & "Health Status: " & "Deleted"
            Case HealthStatusEnum.kDriverLostHealth
                    sHealth = Space(5) & "Health Status: " & "Driver Lost"
            Case HealthStatusEnum.kInErrorHealth
                    sHealth = Space(5) & "Health Status: " & "In Error"
            Case HealthStatusEnum.kOutOfDateHealth
                    sHealth = Space(5) & "Health Status: " & "Out of Date"
            Case HealthStatusEnum.kUnknownHealth
                    sHealth = Space(5) & "Health Status: " & "Unknown"
            Case HealthStatusEnum.kUpToDateHealth
                    sHealth = Space(5) & "Health Status: " & "Up to Date"
        End Select

      Echo(sType & Space(Max(1,25 - Len(sType))) &
           sName & Space(Max(1,25 - Len(sName))) &
           sValue & Space(Max(1,15-(Len(sValue)))) &
           sHealth
           )

    Next iNumParams

    ' Obtain the Model Parameters collection
    Dim oModelParams As ModelParameters _ 
             = oParams.ModelParameters

    ' Iterate through the Model Parameters collection
    Dim iNumModelParams As Long
   BreakTitle("Only Model Values")
    For iNumModelParams = 1 To oModelParams.Count
        sName = oModelParams.Item(iNumModelParams).Name
        sValue =  oModelParams.Item(iNumModelParams).Value
        sUnit = oModelParams.Item(iNumModelParams).Units
        sValue = oUOM.GetStringFromValue(sValue, sUnit)

      Echo(Space(25) &
           sName & Space(Max(1,20 - ( Len(sValue) + Len(sName) ))) &
           sValue )' & Space(Max(1,17 - Len(sValue))) & sUnit )

    Next iNumModelParams

    ' Update the model.
     oDoc.Update
  End Sub

  Public Sub EchoParameters(ByRef oDoc As AssemblyDocument)
  '' Iterate through the Parameters collection to obtain
  '' and display information about the Parameters

    Dim sType, sName, sValue, sHealth, sUnit As String
    Dim oParams As Parameters _ 
             = oDoc.ComponentDefinition.Parameters
    Dim oUOM As UnitsOfMeasure _ 
             = oDoc.UnitsOfMeasure

    DebugLine("EchoParameters - AssemblyDocument",,1)


    Dim iNumParams As Long
   BreakTitle("All Available Parameters")
        Echo("Type" & Space(Max(0,25 - Len("Type"))) &
             "Name" & Space(Max(1,25 - Len("Name"))) &
             "Value") : DebugLine(sMessage,,,,150)' EchoLine(,,False)
    For iNumParams = 1 To oParams.Count
        sName = oParams.Item(iNumParams).Name

        Select Case oParams.Item(iNumParams).Type
            Case ObjectTypeEnum.kModelParameterObject
                sType = "Model Parameter"
            Case ObjectTypeEnum.kTableParameterObject
                sType = "Table Parameter"
            Case ObjectTypeEnum.kUserParameterObject
                sType = "User Parameter"
            Case Else
                sType = "Unknown"
        End Select

        sValue = CStr(oParams.Item(iNumParams).Value)

        Select Case oParams.Item(iNumParams).HealthStatus
            Case HealthStatusEnum.kDeletedHealth
                    sHealth = Space(5) & "Health Status: " & "Deleted"
            Case HealthStatusEnum.kDriverLostHealth
                    sHealth = Space(5) & "Health Status: " & "Driver Lost"
            Case HealthStatusEnum.kInErrorHealth
                    sHealth = Space(5) & "Health Status: " & "In Error"
            Case HealthStatusEnum.kOutOfDateHealth
                    sHealth = Space(5) & "Health Status: " & "Out of Date"
            Case HealthStatusEnum.kUnknownHealth
                    sHealth = Space(5) & "Health Status: " & "Unknown"
            Case HealthStatusEnum.kUpToDateHealth
                    sHealth = Space(5) & "Health Status: " & "Up to Date"
        End Select

      Echo(sType & Space(Max(1,25 - Len(sType))) &
           sName & Space(Max(1,25 - Len(sName))) &
           sValue & Space(Max(1,15-(Len(sValue)))) &
           sHealth
           )

    Next iNumParams

    ' Obtain the Model Parameters collection
    Dim oModelParams As ModelParameters _ 
             = oParams.ModelParameters

    ' Iterate through the Model Parameters collection
    Dim iNumModelParams As Long
   BreakTitle("Only Model Values")
    For iNumModelParams = 1 To oModelParams.Count
        sName = oModelParams.Item(iNumModelParams).Name
        sValue =  oModelParams.Item(iNumModelParams).Value
        sUnit = oModelParams.Item(iNumModelParams).Units
        sValue = oUOM.GetStringFromValue(sValue, sUnit)

      Echo(Space(25) &
           sName & Space(Max(1,20 - ( Len(sValue) + Len(sName) ))) &
           sValue )' & Space(Max(1,17 - Len(sValue))) & sUnit )

    Next iNumModelParams

    ' Update the model.
     oDoc.Update
  End Sub

#End Region

#Region "Enumerators"

  Public Sub Bill()

    BreakTitle("Bill of Material Enumerators")
    Echo("Name --                  Value -- Description")
    Echo("BOMQuantityTypeEnum Enumerator")
    Echo("kEachBOMQuantity         52225 The quantity is determined by each item. ")
    Echo("kParameterBOMQuantity    52226 The quantity is determined by a referenced parameter. ")
    Echo("")
    Echo("BOMStructureEnum Enumerator ")
    Echo("kDefaultBOMStructure     51969 The default structure type. ")
    Echo("kInseparableBOMStructure 51974 The inseparable structure type. ")
    Echo("kNormalBOMStructure      51970 The normal structure type. ")
    Echo("kPhantomBOMStructure     51971 The phantom structure type. ")
    Echo("kPurchasedBOMStructure   51973 The purchased structure type. ")
    Echo("kReferenceBOMStructure   51972 The reference structure type. ")
    Echo("kVariesBOMStructure      51975 The structure type varies amongst references. ")
    Echo("")
    Echo("BOMViewTypeEnum Enumerator")
    Echo("kModelDataBOMViewType    62465 The 'raw' view. ")
    Echo("kPartsOnlyBOMViewType    62467 The parts-only View. ")
    Echo("kStructuredBOMViewType   62466 The Structured View. ")


  End Sub


  Public Sub EchoEnums()
    Echo("  ValueUnitsTypeEnum Enumerator")
    Echo("  kAngleUnits            94977  Angle units. ")
    Echo("  kAngularVelocityUnits  94978  AngularVelocity units. ")
    Echo("  kAreaUnits             94979  Area units. ")
    Echo("  kCurrentUnits          94980  Current units. ")
    Echo("  kForceUnits            94981  Force units. ")
    Echo("  kLengthUnits           94982  Length units. ")
    Echo("  kMassUnits             94983  Mass units. ")
    Echo("  kPowerUnits            94984  Power units. ")
    Echo("  kPressureUnits         94985  Pressure units. ")
    Echo("  kSpeedUnits            94986  Speed units. ")
    Echo("  kTemperatureUnits      94987  Temperature units. ")
    Echo("  kTimeUnits             94988  Time units. ")
    Echo("  kUnitless              94989  Unitless. ")
    Echo("  kVoltageUnits          94990  Voltage units. ")
    Echo("  kVolumeUnits           94991  Volume units. ")
    Echo("  kWorkUnits             94992  Work units. ")
    Echo("  ")
    Echo("  UnitsTypeEnum Enumerator ")
    Echo("  kAcreAreaUnits                      11301  Acre Area.  ")
    Echo("  kAmpElectricalCurrentUnits          11327  Amp electrical Current.  ")
    Echo("  kBooleanUnits                       11347  Boolean (Yes/No).  ")
    Echo("  kBTUWorkUnits                       11320  BTU Work.  ")
    Echo("  kCalorieWorkUnits                   11319  Calorie Work.  ")
    Echo("  kCandelaLuminousIntensityUnits      11342  Candela luminous intensity.  ")
    Echo("  kCelsiusTemperatureUnits            11296  Celsius Temperature.  ")
    Echo("  kCentimeterLengthUnits              11268  Centimeter Length.  ")
    Echo("  kCircularMilAreaUnits               11326  CircularMil Area.  ")
    Echo("  kCompositeUnits                     11322  Composite.  ")
    Echo("  kCoulombElectricalChargeUnits       11330  Coulomb electrical Charge.  ")
    Echo("  kCupVolumeUnits                     11306  Cup Volume.  ")
    Echo("  kDatabaseAngleUnits                 11277  Database units for angle -- ALWAYS Radian.  ")
    Echo("  kDatabaseLengthUnits                11267  Database units for length -- ALWAYS Centimeter.  ")
    Echo("  kDatabaseMassUnits                  11282  Database units for mass -- ALWAYS Kilogram.  ")
    Echo("  kDatabaseTemperatureUnits           11294  Database units for temperature -- ALWAYS Kelvin.  ")
    Echo("  kDatabaseTimeUnits                  11289  Database units for time -- ALWAYS Second.  ")
    Echo("  kDefaultDisplayAngleUnits           11276  Current default display units for Angle.  ")
    Echo("  kDefaultDisplayLengthUnits          11266  Current default display units for length.  ")
    Echo("  kDefaultDisplayMassUnits            11281  Current default display units for Mass.  ")
    Echo("  kDefaultDisplayTemperatureUnits     11293  Current default display units for Temperature.  ")
    Echo("  kDefaultDisplayTimeUnits            11288  Current default display units for Time.  ")
    Echo("  kDegreeAngleUnits                   11279  Degree Angle.  ")
    Echo("  kDyneForceUnits                     11312  Dyne Force.  ")
    Echo("  kErgWorkUnits                       11318  Erg Work.  ")
    Echo("  kFahrenheitTemperatureUnits         11297  Fahrenheit Temperature.  ")
    Echo("  kFaradElectricalCapacitanceUnits    11331  Farad electrical Capacitance.  ")
    Echo("  kFeetPerSecondSpeedUnits            11299  FeetPerSecond Speed.  ")
    Echo("  kFootLengthUnits                    11273  Foot Length.  ")
    Echo("  kGallonVolumeUnits                  11303  Gallon Volume.  ")
    Echo("  kGammaMagneticInductionUnits        11337  Gamma magnetic Induction.  ")
    Echo("  kGaussMagneticInductionUnits        11338  Gauss magnetic Induction.  ")
    Echo("  kGradAngleUnits                     11280  Grad Angle.  ")
    Echo("  kGramMassUnits                      11284  Gram Mass.  ")
    Echo("  kHenryElectricalInductanceUnits     11339  Henry electrical Inductance.  ")
    Echo("  kHertzFrequencyUnits                11341  Hertz Frequency.  ")
    Echo("  kHorsePowerPowerUnits               11316  HorsePower Power.  ")
    Echo("  kHourTimeUnits                      11292  Hour Time.  ")
    Echo("  kInchLengthUnits                    11272  Inch Length.  ")
    Echo("  kJouleWorkUnits                     11317  Joule Work.  ")
    Echo("  kKelvinTemperatureUnits             11295  Kelvin Temperature.  ")
    Echo("  kKilogramMassUnits                  11283  Kilogram Mass.  ")
    Echo("  kKSIPressureUnits                   11310  KSI Pressure.  ")
    Echo("  kLbForceUnits                       11313  Lb Force.  ")
    Echo("  kLbMassMassUnits                    11286  LbMass Mass.  ")
    Echo("  kLiterVolumeUnits                   11302  Liter Volume.  ")
    Echo("  kLumenLuminousFluxUnits             11343  Lumen luminous flux.  ")
    Echo("  kLuxIlluminationUnits               11344  Lux illumination.  ")
    Echo("  kMaxwellMagneticFluxUnits           11335  Maxwell magnetic Flux.  ")
    Echo("  kMeterLengthUnits                   11270  Meter Length.  ")
    Echo("  kMetersPerSecondSpeedUnits          11298  MetersPerSecond Speed.  ")
    Echo("  kmhoElectricalConductanceUnits      11333  mho electrical Conductance.  ")
    Echo("  kMicronLengthUnits                  11271  Micron Length.  ")
    Echo("  kMileLengthUnits                    11275  Mile Length.  ")
    Echo("  kMilesPerHourSpeedUnits             11300  MilesPerHour Speed.  ")
    Echo("  kMilLengthUnits                     11324  Mil Length.  ")
    Echo("  kMillimeterLengthUnits              11269  Millimeter Length.  ")
    Echo("  kMinuteTimeUnits                    11291  Minute Time.  ")
    Echo("  kMoleSubstanceUnits                 11345  Mole Substance or gram molecular weight.  ")
    Echo("  kNauticalMileLengthUnits            11323  NauticalMile Length.  ")
    Echo("  kNewtonForceUnits                   11311  Newton Force.  ")
    Echo("  kOerstedMagneticInductionUnits      11340  Oersted magnetic Induction.  ")
    Echo("  kOhmElectricalResistanceUnits       11329  Ohm electrical Resistance.  ")
    Echo("  kOunceForceUnits                    11314  Ounce Force.  ")
    Echo("  kOunceMassUnits                     11287  Ounce Mass.  ")
    Echo("  kOunceVolumeUnits                   11307  Ounce Volume.  ")
    Echo("  kPascalPressureUnits                11308  Pascal Pressure.  ")
    Echo("  kPintVolumeUnits                    11305  Pint Volume.  ")
    Echo("  kPSIPressureUnits                   11309  PSI Pressure.  ")
    Echo("  kQuartVolumeUnits                   11304  Quart Volume.  ")
    Echo("  kRadianAngleUnits                   11278  Radian Angle.  ")
    Echo("  kRPMAngularVelocityUnits            11321  RPM AngularVelocity.  ")
    Echo("  kSecondTimeUnits                    11290  Second Time.  ")
    Echo("  kSiemensElectricalConductanceUnits  11332  Siemens electrical Conductance.  ")
    Echo("  kSlugMassUnits                      11285  Slug Mass.  ")
    Echo("  kSteradianAngleUnits                11325  Steradian Angle.  ")
    Echo("  kTeslaMagneticInductionUnits        11336  Tesla magnetic Induction.  ")
    Echo("  kTextUnits                          11346  Text (String).  ")
    Echo("  kUnitlessUnits                      11265  No dimension associated with this value.  ")
    Echo("  kVoltElectricalVoltageUnits         11328  Volt electrical Voltage.  ")
    Echo("  kWattPowerUnits                     11315  Watt Power.  ")
    Echo("  kWeberMagneticFluxUnits             11334  Weber magnetic Flux.  ")
    Echo("  kYardLengthUnits                    11274  Yard Length.  ")

  End Sub

#End Region

End Class
