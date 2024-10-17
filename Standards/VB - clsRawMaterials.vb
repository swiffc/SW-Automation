' <IsStraightVb>True</IsStraightVb>
'AddVbFile "VB - clsDebugViewer.vb"
Imports clsDebugViewer
Imports clsStringMath
Imports Inventor

'################################################################################
'# Fur Usage in a iLogic Rule Add the Following Two Lines Above Sub Main()
' AddVbFile "VB - clsRawMaterials.vb"
' AddVbFile "VB - clsStringMath.vb"
' Imports clsRawMaterials
'# 
'################################################################################
'# Example :: 
'#
' AddVbFile "VB - clsDebugViewer.vb"
' AddVbFile "VB - clsRawMaterials.vb"
' Imports clsRawMaterials
' Imports clsDebugViewer

' Sub Main()
'  Dim oDoc As Document = ThisDoc.Document

'  EchoTitle("Working Fraction Finder")
	
' 	LongValue = getFractionalFeet( Max(d1,d0) / 12 )
' 	ShortValue = getFractionalFeet( Min(d1,d0) / 12 )
' End Sub

'################################################################################

Class clsRawMaterials

#Region "Angles"
  ''' <summary>
  ''' Using LegA/B, Thk, Mat, Length values. The measures are evaluated to Vis Numbers
  ''' and formatted strings are returned for use as Descriptions and Measures
  ''' </summary>
  ''' <param name="Measures">Double Array: LegA; LegB; THK</param>
  ''' <param name="Material">String: Material</param>
  ''' <param name="Length">(Optional) Length (in)</param>
  ''' <returns>String Array: Part Desc; StockNo; RM Desc; RM QTY; RM UoM </returns>
  ''' <remarks></remarks>
  Public Shared Function getAngleRM( ByVal ShapeMeasures As Double(),
                                     ByVal Material As String,
                            Optional ByVal Length As Double = 0.0 ) As String()
    Dim AngleLookup As String()
    Dim sShapeMeasures(ShapeMeasures.GetUpperBound(0)) As String
    Dim sMeasure As String

    For index = 0 To ShapeMeasures.GetUpperBound(0)
      sMeasure = getShortFeet(ShapeMeasures(index)/12)
      sMeasure = sMeasure.Replace(" "c, "-"c)
      If sMeasure.EndsWith(""""c) Then sMeasure = Left(sMeasure,Len(sMeasure) - 1)
      sShapeMeasures(index) = sMeasure
    Next

    Dim sLegA, sLegB, sThk As String
    If sShapeMeasures.GetUpperBound(0) = 1 Then
      sLegA = sShapeMeasures(0)
      sLegB = sLegA
      sThk = sShapeMeasures(1)
      Else If sShapeMeasures.GetUpperBound(0) = 2 Then
      sLegA = sShapeMeasures(0)
      sLegB = sShapeMeasures(1)
      sThk = sShapeMeasures(2)
      Else
      Exit Function
    End If

    Dim BaseDesc, PartDesc, RMDesc As String
    BaseDesc = sLegA & " X " & sLegB & " X " & sThk
    PartDesc = "=<Title> (L" & sLegA & "X" & sLegB & "X" & sThk & ")"
    'Return RM Description formatted as: ANGLE,1"x1"x1/4",A36 or ANGLE,2-1/2"x1-1/2"x1/4",A36
    RMDesc = "=ANGLE," & sLegA & """" & "x" & sLegB & """" & "x" & sThk & """" & ", <material>"
    Echo("Angle As : " )

    Select Case Material
      Case "A36"
        AngleLookup = getAngleRM_A36(BaseDesc)
        If AngleLookup(0) = "" Then AngleLookup = getAngleRM_A572(BaseDesc)
      Case "A572 50"
        AngleLookup = getAngleRM_A572(BaseDesc)
        If AngleLookup(0) = "" Then AngleLookup = getAngleRM_A36(BaseDesc)
      Case Else
        AngleLookup = getAngleRM_A572(BaseDesc)
        If AngleLookup(0) = "" Then AngleLookup = getAngleRM_A36(BaseDesc)
    End Select

    If AngleLookup(0) = "" Then AngleLookup = { "" , "" }

    Echo( Space(5) & "\= PD : "& PartDesc)
    Echo( Space(5) & "\= RD : " & AngleLookup(0) & " : " & RMDesc & " : " & AngleLookup(1) )

    Return { PartDesc , AngleLookup(0) , RMDesc , AngleLookup(1) }
  End Function

  ''' <summary>
  ''' Returns RNDM stock number and material type
  ''' </summary>
  ''' <param name="BaseDesc">The double to convert.</param>
  ''' <returns>String Array ( StockNo, MaterialName)</returns>
  ''' <remarks></remarks>
  Public Shared Function getAngleRM_A36( ByVal BaseDesc As String) As String()
    Dim returnvalues As String()
    Dim stock, mat As String
    
    'default material is A36
    mat = "A36"

    Select Case BaseDesc
      Case "1 X 1 X 1/4" 
        stock = "12005"
      Case "1 X 1 X 1/8" 
        stock = "57737"
      Case "1-1/2 X 1-1/2 X 1/4" 
        stock = "12011"
      Case "1-1/2 X 1-1/2 X 1/8" 
        stock = "54275"
      Case "1-1/2 X 1-1/2 X 3/16" 
        stock = "12010"
      Case "1-1/4 X 1-1/4 X 1/4" 
        stock = "12008"
      Case "1-1/4 X 1-1/4 X 1/8" 
        stock = "12009"
      Case "1-1/4 X 1-1/4 X 3/16" 
        stock = "70840"
      Case "1-3/4 X 1-1/4 X 1/4" 
        stock = "12013"
      Case "1-3/4 X 1-3/4 X 1/4" 
        stock = "57753"
      Case "2 X 1-1/2 X 1/4" 
        stock = "12109"
      Case "2 X 1-1/2 X 3/16" 
        stock = "55961"
      Case "2 X 2 X 1/4" 
        stock = "12019"
      Case "2 X 2 X 1/8" 
        stock = "12017"
      Case "2 X 2 X 3/16" 
        stock = "12018"
      Case "2 X 2 X 3/8" 
        stock = "12020"
      Case "2 X 2 X 5/16" 
        stock = "53007"
      Case "2-1/2 X 1-1/2 X 1/4" 
        stock = "12114"
      Case "2-1/2 X 2 X 1/4" 
        stock = "12124"
      Case "2-1/2 X 2 X 3/16" 
        stock = "12119"
      Case "2-1/2 X 2 X 3/8" 
        stock = "56344"
      Case "2-1/2 X 2 X 5/16" 
        stock = "12125"
      Case "2-1/2 X 2-1/2 X 1/2" 
        stock = "60671"
      Case "2-1/2 X 2-1/2 X 1/4" 
        stock = "12022"
      Case "2-1/2 X 2-1/2 X 3/16" 
        stock = "12021"
      Case "2-1/2 X 2-1/2 X 3/8" 
        stock = "12024"
      Case "2-1/2 X 2-1/2 X 5/16" 
        stock = "12023"
      Case "2-1/4 X 2-1/4 X 1/4" 
        stock = "69648"
      Case "3 X 2 X 1/2" 
        stock = "54634"
      Case "3 X 2 X 1/4" 
        stock = "12133"
      Case "3 X 2 X 1/8" 
        stock = "12131"
      Case "3 X 2 X 3/16" 
        stock = "12132"
      Case "3 X 2 X 3/8" 
        stock = "12138"
      Case "3 X 2 X 5/16" 
        stock = "12134"
      Case "3 X 2-1/2 X 1/2" 
        stock = "59454"
      Case "3 X 2-1/2 X 1/4" 
        stock = "12143"
      Case "3 X 2-1/2 X 3/16" 
        stock = "12142"
      Case "3 X 2-1/2 X 3/8" 
        stock = "12147"
      Case "3 X 2-1/2 X 5/16" 
        stock = "56915"
      Case "3 X 3 X 1/2" 
        stock = "12031"
      Case "3 X 3 X 1/4" 
        stock = "12027"
      Case "3 X 3 X 3/16" 
        stock = "12026"
      Case "3 X 3 X 3/8" 
        stock = "12029"
      Case "3 X 3 X 5/16" 
        stock = "12028"
      Case "3-1/2 X 2-1/2 X 1/4" 
        stock = "12153"
      Case "3-1/2 X 2-1/2 X 3/8" 
        stock = "57846"
      Case "3-1/2 X 2-1/2 X 5/16" 
        stock = "57305"
      Case "3-1/2 X 3 X 1/2" 
        stock = "12163"
      Case "3-1/2 X 3 X 1/4" 
        stock = "12159"
      Case "3-1/2 X 3 X 3/8" 
        stock = "12161"
      Case "3-1/2 X 3 X 5/16" 
        stock = "12160"
      Case "3-1/2 X 3-1/2 X 1/2" 
        stock = "12044"
      Case "3-1/2 X 3-1/2 X 1/4" 
        stock = "12040"
      Case "3-1/2 X 3-1/2 X 3/8" 
        stock = "12042"
      Case "3-1/2 X 3-1/2 X 5/16" 
        stock = "12041"
      Case "4 X 3 X 1/2" 
        stock = "12174"
      Case "4 X 3 X 1/4" 
        stock = "12169"
      Case "4 X 3 X 3/4" 
        stock = "57984"
      Case "4 X 3 X 3/8" 
        stock = "12173"
      Case "4 X 3 X 5/16" 
        stock = "12170"
      Case "4 X 3 X 5/8" 
        stock = "59629"
      Case "4 X 3-1/2 X 1/2" 
        stock = "53519"
      Case "4 X 3-1/2 X 1/4" 
        stock = "12181"
      Case "4 X 3-1/2 X 3/8" 
        stock = "69030"
      Case "4 X 3-1/2 X 5/16" 
        stock = "53150"
      Case "4 X 3-1/2 X 5/8" 
        stock = "77662"
      Case "4 X 4 X 1/2" 
        stock = "12048"
      Case "4 X 4 X 1/4" 
        stock = "12045"
      Case "4 X 4 X 3/4" 
        stock = "12049"
      Case "4 X 4 X 3/8" 
        stock = "12047"
      Case "4 X 4 X 5/16" 
        stock = "12046"
      Case "4 X 4 X 5/8" 
        stock = "12065"
      Case "5 X 3 X 1/2" 
        stock = "12189"
      Case "5 X 3 X 1/4" 
        stock = "12186"
      Case "5 X 3 X 3/8" 
        stock = "12188"
      Case "5 X 3 X 5/16" 
        stock = "12187"
      Case "5 X 3 X 5/8" 
        stock = "58756"
      Case "5 X 3-1/2 X 1/2" 
        stock = "54297"
      Case "5 X 3-1/2 X 1/4" 
        stock = "12192"
      Case "5 X 3-1/2 X 3/4" 
        stock = "55999"
      Case "5 X 3-1/2 X 3/8" 
        stock = "12197"
      Case "5 X 3-1/2 X 5/16" 
        stock = "12196"
      Case "5 X 3-1/2 X 5/8" 
        stock = "59576"
      Case "5 X 5 X 1/2" 
        stock = "56777"
      Case "5 X 5 X 3/4" 
        stock = "72218"
      Case "5 X 5 X 3/8" 
        stock = "74818"
      Case "5 X 5 X 5/16" 
        stock = "12050"
      Case "5 X 5 X 5/8" 
        stock = "69433"
      Case "6 X 3-1/2 X 1/2" 
        stock = "77615"
      Case "6 X 3-1/2 X 3/8" 
        stock = "12206"
      Case "6 X 3-1/2 X 5/16" 
        stock = "12205"
      Case "6 X 4 X 1/2" 
        stock = "12214"
      Case "6 X 4 X 3/4" 
        stock = "57196"
      Case "6 X 4 X 3/8" 
        stock = "12212"
      Case "6 X 4 X 5/8" 
        stock = "54458"
      Case "6 X 4 X 7/8" 
        stock = "79829"
      Case "6 X 6 X 1" 
        stock = "66997"
      Case "6 X 6 X 1/2" 
        stock = "12062"
      Case "6 X 6 X 3/4" 
        stock = "58933"
      Case "6 X 6 X 3/8" 
        stock = "12061"
      Case "6 X 6 X 5/16" 
        stock = "12060"
      Case "6 X 6 X 5/8" 
        stock = "73318"
      Case "7 X 4 X 3/4" 
        stock = "69054"
      Case "8 X 4 X 1" 
        stock = "66093"
      Case "8 X 4 X 1/2" 
        stock = "12220"
      Case "8 X 4 X 3/4" 
        stock = "71624"
      Case "8 X 6 X 3/4" 
        stock = "70776"
      Case "9 X 4 X 1/2" 
        stock = "69147"

      Case Else 'Nothing Found
        stock = "" : mat = ""
    End Select

    returnvalues = { stock , mat }
    Return returnvalues
  End Function

  Public Shared Function getAngleRM_A572( ByVal BaseDesc As String) As String()
    Dim returnvalues As String()
    Dim stock, mat As String

    'default material is A572
    mat = "A572 50"

    Select Case BaseDesc
    Case "1 X 1 X 1/4" 
      stock = "78531" 
    Case "1-1/2 X 1-1/2 X 1/4" 
      stock = "65842" 
    Case "1-1/2 X 1-1/2 X 3/16" 
      stock = "76288" 
    Case "1-1/4 X 1-1/4 X 3/16" 
      stock = "70852" 
    Case "1-3/4 X 1-3/4 X 1/4" 
      stock = "71572" 
    Case "2 X 1-1/2 X 1/4" 
      stock = "60017" 
    Case "2 X 2 X 1/4" 
      stock = "60013" 
    Case "2 X 2 X 3/16" 
      stock = "54969" 
    Case "2 X 2 X 3/8" 
      stock = "73266" 
    Case "2-1/2 X 2 X 1/4" 
      stock = "54993" 
    Case "2-1/2 X 2 X 3/16" 
      stock = "54912" 
    Case "2-1/2 X 2-1/2 X 1/2" 
      stock = "60802" 
    Case "2-1/2 X 2-1/2 X 1/4" 
      stock = "53499" 
    Case "2-1/2 X 2-1/2 X 3/16" 
      stock = "54888" 
    Case "2-1/2 X 2-1/2 X 3/8" 
      stock = "60130" 
    Case "2-1/2 X 2-1/2 X 5/16" 
      stock = "72898" 
    Case "3 X 2 X 1/4" 
      stock = "53498" 
    Case "3 X 2 X 3/16" 
      stock = "60014" 
    Case "3 X 2 X 3/8" 
      stock = "60039" 
    Case "3 X 2-1/2 X 1/2" 
      stock = "60427" 
    Case "3 X 2-1/2 X 1/4" 
      stock = "54913" 
    Case "3 X 2-1/2 X 3/16" 
      stock = "54996" 
    Case "3 X 2-1/2 X 3/8" 
      stock = "60731" 
    Case "3 X 3 X 1/2" 
      stock = "54864" 
    Case "3 X 3 X 1/4" 
      stock = "54986" 
    Case "3 X 3 X 3/16" 
      stock = "54949" 
    Case "3 X 3 X 3/8" 
      stock = "60132" 
    Case "3 X 3 X 5/16" 
      stock = "54914" 
    Case "3-1/2 X 2-1/2 X 1/4" 
      stock = "54919" 
    Case "3-1/2 X 2-1/2 X 5/16" 
      stock = "66387" 
    Case "3-1/2 X 3 X 1/2" 
      stock = "73814" 
    Case "3-1/2 X 3 X 1/4" 
      stock = "60428" 
    Case "3-1/2 X 3-1/2 X 1/2" 
      stock = "71463" 
    Case "3-1/2 X 3-1/2 X 1/4" 
      stock = "54992" 
    Case "3-1/2 X 3-1/2 X 3/8" 
      stock = "58585" 
    Case "3-1/2 X 3-1/2 X 5/16" 
      stock = "66246" 
    Case "4 X 3 X 1/2" 
      stock = "60363" 
    Case "4 X 3 X 1/4" 
      stock = "54998" 
    Case "4 X 3 X 3/8" 
      stock = "60129" 
    Case "4 X 3 X 5/16" 
      stock = "60483" 
    Case "4 X 3 X 5/8" 
      stock = "73582" 
    Case "4 X 3-1/2 X 1/4" 
      stock = "31638" 
    Case "4 X 3-1/2 X 3/8" 
      stock = "66706" 
    Case "4 X 3-1/2 X 5/8" 
      stock = "65773" 
    Case "4 X 4 X 1/2" 
      stock = "60729" 
    Case "4 X 4 X 1/4" 
      stock = "71817" 
    Case "4 X 4 X 3/4" 
      stock = "69201" 
    Case "4 X 4 X 3/8" 
      stock = "60470" 
    Case "4 X 4 X 5/16" 
      stock = "69408" 
    Case "4 X 4 X 5/8" 
      stock = "65771" 
    Case "5 X 3 X 1/2" 
      stock = "60776" 
    Case "5 X 3 X 1/4" 
      stock = "54999" 
    Case "5 X 3 X 3/4" 
      stock = "65619" 
    Case "5 X 3 X 3/8" 
      stock = "78672" 
    Case "5 X 3 X 5/16" 
      stock = "69632" 
    Case "5 X 3 X 5/8" 
      stock = "65870" 
    Case "5 X 3-1/2 X 1/2" 
      stock = "65617" 
    Case "5 X 3-1/2 X 1/4" 
      stock = "72123" 
    Case "5 X 3-1/2 X 3/4" 
      stock = "74864" 
    Case "5 X 3-1/2 X 5/16" 
      stock = "60323" 
    Case "5 X 5 X 1/2" 
      stock = "60771" 
    Case "5 X 5 X 3/4" 
      stock = "60523" 
    Case "5 X 5 X 3/8" 
      stock = "79974" 
    Case "5 X 5 X 5/16" 
      stock = "54920" 
    Case "5 X 5 X 5/8" 
      stock = "65660" 
    Case "5 X 5 X 7/8" 
      stock = "74870" 
    Case "6 X 3-1/2 X 1/2" 
      stock = "77007" 
    Case "6 X 3-1/2 X 3/8" 
      stock = "73265" 
    Case "6 X 3-1/2 X 5/16" 
      stock = "66705" 
    Case "6 X 4 X 1/2" 
      stock = "66358" 
    Case "6 X 4 X 3/4" 
      stock = "54988" 
    Case "6 X 4 X 3/8" 
      stock = "73799" 
    Case "6 X 4 X 5/16" 
      stock = "65769" 
    Case "6 X 4 X 5/8" 
      stock = "78663" 
    Case "6 X 4 X 7/8" 
      stock = "79912" 
    Case "6 X 6 X 1/2" 
      stock = "77774" 
    Case "6 X 6 X 3/4" 
      stock = "60187" 
    Case "6 X 6 X 3/8" 
      stock = "77340" 

      Case Else
        stock = "" 
    End Select

    returnvalues = { stock , mat }
    Return returnvalues
  End Function
#End Region

#Region "Beams"
  ''' <summary>
  ''' Using Shape Size and Weight values. The measures are evaluated to Vis Numbers
  ''' and formatted strings are returned for use as Descriptions and Measures
  ''' </summary>
  ''' <param name="BeamWidth">String: W Shape's Size</param>
  ''' <param name="BeamWeight">String: W Shape's Weight</param>
  ''' <param name="Length">(Optional) Length (in)</param>
  ''' <returns>String Array: Part Desc; StockNo; RM Desc; Material </returns>
  ''' <remarks></remarks>
  Public Shared Function getBeamRM_A992( ByVal BeamWidth As String,
                                         ByVal BeamWeight As String,
                                Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BeamWidth & "x" & BeamWeight 
    Dim GW , GH , GT , GT1 , GIR As String
 

    ' Default material is A992
    mat = "A992"

    Select Case sSizeWt
      Case "4x13" 
        stock = "13002" : GW = "4.06" :  GH = "4.16" : GT = "0.28" : GT1 = "0.345" : GIR = "0.25"
      Case "5x16" 
        stock = "55965" : GW = "5" :  GH = "5.01" : GT = "0.24" : GT1 = "0.36" : GIR = "0.3"
      Case "6x9" 
        stock = "69882" : GW = "3.94" :  GH = "5.9" : GT = "0.17" : GT1 = "0.215" : GIR = "0.25"
      Case "6x12" 
        stock = "13009" : GW = "4" :  GH = "6.03" : GT = "0.23" : GT1 = "0.28" : GIR = "0.25"
      Case "6x16" 
        stock = "13010" : GW = "4.03" :  GH = "6.28" : GT = "0.26" : GT1 = "0.405" : GIR = "0.25"
      Case "6x15" 
        stock = "13011" : GW = "5.99" :  GH = "5.99" : GT = "0.23" : GT1 = "0.26" : GIR = "0.25"
      Case "6x20" 
        stock = "13012" : GW = "6.02" :  GH = "6.2" : GT = "0.26" : GT1 = "0.365" : GIR = "0.25"
      Case "6x25" 
        stock = "66828" : GW = "6.08" :  GH = "6.38" : GT = "0.32" : GT1 = "0.455" : GIR = "0.25"
      Case "8x10" 
        stock = "13017" : GW = "3.94" :  GH = "7.89" : GT = "0.17" : GT1 = "0.205" : GIR = "0.3"
      Case "8x13" 
        stock = "13018" : GW = "4" :  GH = "7.99" : GT = "0.23" : GT1 = "0.255" : GIR = "0.3"
      Case "8x15" 
        stock = "66433" : GW = "4.02" :  GH = "8.11" : GT = "0.245" : GT1 = "0.315" : GIR = "0.3"
      Case "8x18" 
        stock = "13021" : GW = "5.25" :  GH = "8.14" : GT = "0.23" : GT1 = "0.33" : GIR = "0.3"
      Case "8x21" 
        stock = "13022" : GW = "5.27" :  GH = "8.28" : GT = "0.25" : GT1 = "0.4" : GIR = "0.3"
      Case "8x24" 
        stock = "13024" : GW = "6.50" :  GH = "7.93" : GT = "0.245" : GT1 = "0.4" : GIR = "0.4"
      Case "8x28" 
        stock = "13025" : GW = "6.54" :  GH = "8.06" : GT = "0.285" : GT1 = "0.465" : GIR = "0.4"
      Case "8x31" 
        stock = "13027" : GW = "8" :  GH = "8" : GT = "0.285" : GT1 = "0.435" : GIR = "0.4"
      Case "8x35" 
        stock = "13028" : GW = "8.02" :  GH = "8.12" : GT = "0.31" : GT1 = "0.495" : GIR = "0.4"
      Case "8x40" 
        stock = "13031" : GW = "8.07" :  GH = "8.25" : GT = "0.36" : GT1 = "0.56" : GIR = "0.4"
      Case "8x48" 
        stock = "13033" : GW = "8.11" :  GH = "8.5" : GT = "0.4" : GT1 = "0.685" : GIR = "0.4"
      Case "10x15" 
        stock = "13036" : GW = "4" :  GH = "9.99" : GT = "0.23" : GT1 = "0.27" : GIR = "0.3"
      Case "10x19" 
        stock = "69370" : GW = "4.02" :  GH = "10.2" : GT = "0.25" : GT1 = "0.395" : GIR = "0.3"
      Case "10x22" 
        stock = "69624" : GW = "5.75" :  GH = "10.2" : GT = "0.24" : GT1 = "0.36" : GIR = "0.3"
      Case "10x26" 
        stock = "69800" : GW = "5.77" :  GH = "10.3" : GT = "0.26" : GT1 = "0.44" : GIR = "0.3"
      Case "10x30" 
        stock = "69932" : GW = "5.81" :  GH = "10.5" : GT = "0.3" : GT1 = "0.51" : GIR = "0.3"
      Case "10x33" 
        stock = "65916" : GW = "7.96" :  GH = "9.73" : GT = "0.29" : GT1 = "0.435" : GIR = "0.5"
      Case "10x49" 
        stock = "13048" : GW = "10" :  GH = "10" : GT = "0.34" : GT1 = "0.56" : GIR = "0.5"
      Case "12x14" 
        stock = "13054" : GW = "3.97" :  GH = "11.9" : GT = "0.2" : GT1 = "0.225" : GIR = "0.3"
      Case "12x16" 
        stock = "54088" : GW = "3.99" :  GH = "12" : GT = "0.22" : GT1 = "0.265" : GIR = "0.3"
      Case "12x19" 
        stock = "53694" : GW = "4.01" :  GH = "12.2" : GT = "0.235" : GT1 = "0.35" : GIR = "0.3"
      Case "12x22" 
        stock = "56046" : GW = "4.03" :  GH = "12.3" : GT = "0.26" : GT1 = "0.425" : GIR = "0.3"
      Case "12x26" 
        stock = "69359" : GW = "6.49" :  GH = "12.2" : GT = "0.23" : GT1 = "0.38" : GIR = "0.3"
      Case "12x30" 
        stock = "65917" : GW = "6.52" :  GH = "12.3" : GT = "0.26" : GT1 = "0.44" : GIR = "0.3"
      Case "12x35" 
        stock = "70146" : GW = "6.56" :  GH = "12.5" : GT = "0.3" : GT1 = "0.52" : GIR = "0.3"
      Case "12x40" 
        stock = "13064" : GW = "8.01" :  GH = "11.9" : GT = "0.295" : GT1 = "0.515" : GIR = "0.5"
      Case "12x45" 
        stock = "59160" : GW = "8.05" :  GH = "12.1" : GT = "0.335" : GT1 = "0.575" : GIR = "0.5"
      Case "12x50" 
        stock = "69407" : GW = "8.08" :  GH = "12.2" : GT = "0.37" : GT1 = "0.64" : GIR = "0.5"
      Case "12x53" 
        stock = "13067" : GW = "10" :  GH = "12.1" : GT = "0.345" : GT1 = "0.575" : GIR = "0.6"
      Case "12x58" 
        stock = "69294" : GW = "10" :  GH = "12.2" : GT = "0.36" : GT1 = "0.64" : GIR = "0.6"
      Case "12x65" 
        stock = "58539" : GW = "12" :  GH = "12.1" : GT = "0.39" : GT1 = "0.605" : GIR = "0.6"
      Case "12x72" 
        stock = "58617" : GW = "12" :  GH = "12.3" : GT = "0.43" : GT1 = "0.67" : GIR = "0.6"
      Case "12x79" 
        stock = "70779" : GW = "12.1" :  GH = "12.4" : GT = "0.47" : GT1 = "0.735" : GIR = "0.6"
      Case "14x22" 
        stock = "69685" : GW = "5" :  GH = "13.7" : GT = "0.23" : GT1 = "0.335" : GIR = "0.4"
      Case "14x26" 
        stock = "69019" : GW = "5.03" :  GH = "13.9" : GT = "0.255" : GT1 = "0.42" : GIR = "0.4"
      Case "14x30" 
        stock = "13079" : GW = "6.73" :  GH = "13.8" : GT = "0.27" : GT1 = "0.385" : GIR = "0.4"
      Case "14x34" 
        stock = "13080" : GW = "6.75" :  GH = "14" : GT = "0.285" : GT1 = "0.455" : GIR = "0.4"
      Case "14x38" 
        stock = "70065" : GW = "6.77" :  GH = "14.1" : GT = "0.31" : GT1 = "0.515" : GIR = "0.4"
      Case "14x43" 
        stock = "13082" : GW = "8" :  GH = "13.7" : GT = "0.305" : GT1 = "0.53" : GIR = "0.6"
      Case "14x48" 
        stock = "13084" : GW = "8.03" :  GH = "13.8" : GT = "0.34" : GT1 = "0.595" : GIR = "0.6"
      Case "14x53" 
        stock = "55565" : GW = "8.06" :  GH = "13.9" : GT = "0.37" : GT1 = "0.66" : GIR = "0.6"
      Case "14x61" 
        stock = "55566" : GW = "10" :  GH = "13.9" : GT = "0.375" : GT1 = "0.645" : GIR = "0.6"
      Case "14x68" 
        stock = "60095" : GW = "10" :  GH = "14" : GT = "0.415" : GT1 = "0.72" : GIR = "0.6"
      Case "14x82" 
        stock = "53695" : GW = "10.1" :  GH = "14.3" : GT = "0.51" : GT1 = "0.855" : GIR = "0.6"
      Case "14x90" 
        stock = "57429" : GW = "14.5" :  GH = "14.0" : GT = "0.44" : GT1 = "0.71" : GIR = "0.6"
      Case "14x109" 
        stock = "69218" : GW = "14.6" :  GH = "14.3" : GT = "0.525" : GT1 = "0.86" : GIR = "0.6"
      Case "16x26" 
        stock = "57226" : GW = "5.5" :  GH = "15.7" : GT = "0.25" : GT1 = "0.345" : GIR = "0.4"
      Case "16x31" 
        stock = "13099" : GW = "5.53" :  GH = "15.9" : GT = "0.275" : GT1 = "0.44" : GIR = "0.4"
      Case "16x36" 
        stock = "13100" : GW = "6.99" :  GH = "15.9" : GT = "0.295" : GT1 = "0.43" : GIR = "0.4"
      Case "16x40" 
        stock = "57416" : GW = "7" :  GH = "16.0" : GT = "0.305" : GT1 = "0.505" : GIR = "0.4"
      Case "16x45" 
        stock = "55762" : GW = "7.04" :  GH = "16.1" : GT = "0.345" : GT1 = "0.565" : GIR = "0.4"
      Case "16x50" 
        stock = "13103" : GW = "7.07" :  GH = "16.3" : GT = "0.38" : GT1 = "0.63" : GIR = "0.4"
      Case "16x57" 
        stock = "56531" : GW = "7.12" :  GH = "16.4" : GT = "0.43" : GT1 = "0.715" : GIR = "0.4"
      Case "16x67" 
        stock = "55761" : GW = "10.2" :  GH = "16.3" : GT = "0.395" : GT1 = "0.665" : GIR = "0.4"
      Case "18x35" 
        stock = "70153" : GW = "6" :  GH = "17.7" : GT = "0.3" : GT1 = "0.425" : GIR = "0.4"
      Case "18x40" 
        stock = "71887" : GW = "6.02" :  GH = "17.9" : GT = "0.315" : GT1 = "0.525" : GIR = "0.4"
      Case "18x46" 
        stock = "67021" : GW = "6.06" :  GH = "18.1" : GT = "0.36" : GT1 = "0.605" : GIR = "0.4"
      Case "18x50" 
        stock = "70102" : GW = "7.5" :  GH = "18" : GT = "0.355" : GT1 = "0.57" : GIR = "0.4"
      Case "18x55" 
        stock = "55763" : GW = "7.53" :  GH = "18.1" : GT = "0.39" : GT1 = "0.63" : GIR = "0.4"
      Case "18x60" 
        stock = "55764" : GW = "7.56" :  GH = "18.2" : GT = "0.415" : GT1 = "0.695" : GIR = "0.4"
      Case "18x65" 
        stock = "54014" : GW = "7.59" :  GH = "18.4" : GT = "0.45" : GT1 = "0.75" : GIR = "0.4"
      Case "18x76" 
        stock = "58618" : GW = "11.0" :  GH = "18.2" : GT = "0.425" : GT1 = "0.68" : GIR = "0.4"
      Case "21x44" 
        stock = "72108" : GW = "6.5" :  GH = "20.7" : GT = "0.35" : GT1 = "0.45" : GIR = "0.5"
      Case "21x50" 
        stock = "56021" : GW = "6.53" :  GH = "20.8" : GT = "0.38" : GT1 = "0.535" : GIR = "0.5"
      Case "21x57" 
        stock = "69383" : GW = "6.56" :  GH = "21.1" : GT = "0.405" : GT1 = "0.65" : GIR = "0.5"
      Case "21x62" 
        stock = "13121" : GW = "8.24" :  GH = "21" : GT = "0.4" : GT1 = "0.615" : GIR = "0.5"
      Case "21x68" 
        stock = "56038" : GW = "8.27" :  GH = "21.1" : GT = "0.43" : GT1 = "0.685" : GIR = "0.5"
      Case "21x111" 
        stock = "57551" : GW = "12.3" :  GH = "21.5" : GT = "0.55" : GT1 = "0.875" : GIR = "0.5"
      Case "24x55" 
        stock = "57415" : GW = "7.01" :  GH = "23.6" : GT = "0.395" : GT1 = "0.505" : GIR = "0.5"
      Case "24x62" 
        stock = "56092" : GW = "7.04" :  GH = "23.7" : GT = "0.43" : GT1 = "0.59" : GIR = "0.5"
      Case "24x68" 
        stock = "13127" : GW = "8.97" :  GH = "23.7" : GT = "0.415" : GT1 = "0.585" : GIR = "0.5"
      Case "24x76" 
        stock = "59159" : GW = "8.99" :  GH = "23.9" : GT = "0.44" : GT1 = "0.68" : GIR = "0.5"
      Case "24x84" 
        stock = "53231" : GW = "9.02" :  GH = "24.1" : GT = "0.47" : GT1 = "0.77" : GIR = "0.5"
      Case "24x104" 
        stock = "59158" : GW = "12.8" :  GH = "24.1" : GT = "0.5" : GT1 = "0.75" : GIR = "0.5"
      Case "27x84" 
        stock = "58661" : GW = "10" :  GH = "26.7" : GT = "0.46" : GT1 = "0.64" : GIR = "0.6"
      Case "27x102" 
        stock = "60073" : GW = "10.0" :  GH = "27.1" : GT = "0.515" : GT1 = "0.83" : GIR = "0.6"
      Case "27x114" 
        stock = "13142" : GW = "10.1" :  GH = "27.3" : GT = "0.57" : GT1 = "0.93" : GIR = "0.6"
      Case "33x118" 
        stock = "56945" : GW = "11.5" :  GH = "32.9" : GT = "0.55" : GT1 = "0.74" : GIR = "0.7"
      Case "36x135" 
        stock = "56946" : GW = "12" :  GH = "35.6" : GT = "0.6" : GT1 = "0.79" : GIR = "0.75"

      'Case "4x13" ::\::          stock = "13002" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "5x16" ::\::          stock = "55965" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "6x12" ::\::          stock = "13009" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "6x15" ::\::          stock = "13011" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "6x16" ::\::          stock = "13010" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "6x20" ::\::          stock = "13012" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "6x25" ::\::          stock = "66828" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "6x9" ::\::          stock = "69882" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '13008 ::\:: ::\::      Case "8x10" ::\::          stock = "13017" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x13" ::\::          stock = "13018" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x15" ::\::          stock = "66433" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '13019 ::\::      Case "8x18" ::\::          stock = "13021" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x21" ::\::          stock = "13022" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x24" ::\::          stock = "13024" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x28" ::\::          stock = "13025" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x31" ::\::          stock = "13027" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x35" ::\::          stock = "13028" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x40" ::\::          stock = "13031" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "8x48" ::\::          stock = "13033" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "10x15" ::\::          stock = "13036" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x19" ::\::          stock = "69370" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x22" ::\::          stock = "69624" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x26" ::\::          stock = "69800" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x30" ::\::          stock = "69932" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x33" ::\::          stock = "65916" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "10x49" ::\::          stock = "13048" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "12x14" ::\::          stock = "13054" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x16" ::\::          stock = "54088" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x19" ::\::          stock = "53694" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x22" ::\::          stock = "56046" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x26" ::\::          stock = "69359" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x30" ::\::          stock = "65917" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x35" ::\::          stock = "70146" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '60619 ::\::      Case "12x40" ::\::          stock = "13064" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x45" ::\::          stock = "59160" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x50" ::\::          stock = "69407" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '56022 ::\::      Case "12x53" ::\::          stock = "13067" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x58" ::\::          stock = "69294" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x65" ::\::          stock = "58539" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x72" ::\::          stock = "58617" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "12x79" ::\::          stock = "70779" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "14x109" ::\::          stock = "69218" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x22" ::\::          stock = "69685" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x26" ::\::          stock = "69019" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x30" ::\::          stock = "13079" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x34" ::\::          stock = "13080" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x38" ::\::          stock = "70065" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x43" ::\::          stock = "13082" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x48" ::\::          stock = "13084" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x53" ::\::          stock = "55565" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x61" ::\::          stock = "55566" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x68" ::\::          stock = "60095" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x82" ::\::          stock = "53695" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "14x90" ::\::          stock = "57429" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "16x26" ::\::          stock = "57226" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x31" ::\::          stock = "13099" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x36" ::\::          stock = "13100" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x40" ::\::          stock = "57416" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x45" ::\::          stock = "55762" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x50" ::\::          stock = "13103" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x57" ::\::          stock = "56531" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "16x67" ::\::          stock = "55761" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "18x35" ::\::          stock = "70153" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '59210 ::\::      Case "18x40" ::\::          stock = "71887" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x46" ::\::          stock = "57021" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x50" ::\::          stock = "70102" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x55" ::\::          stock = "55763" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x60" ::\::          stock = "55764" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x65" ::\::          stock = "54014" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "18x76" ::\::          stock = "58618" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::       ::\::      Case "21x111" ::\::          stock = "57551" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "21x44" ::\::          stock = "72108" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "21x50" ::\::          stock = "56021" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "21x57" ::\::          stock = "69383" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" '57043 ::\::      Case "21x62" ::\::          stock = "13121" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "21x68" ::\::          stock = "56038" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "24x104" ::\::          stock = "59158" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "24x55" ::\::          stock = "57415" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "24x62" ::\::          stock = "56092" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::      Case "24x68" ::\::          stock = "13127" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992"     ::\::      Case "24x76" ::\::          stock = "59159" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992"     ::\::      Case "24x84" ::\::          stock = "53231" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992"     ::\:: ::\::      Case "27x102" ::\::          stock = "60073" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992"     ::\::      Case "27x114" ::\::          stock = "13142" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992"     ::\::      Case "27x84" ::\::          stock = "58661" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::       ::\::      Case "33x118" ::\::          stock = "56945" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\:: ::\::      Case "36x135" ::\::          stock = "56946" ' :  size = "BEAM,W," & sSizeWt  :  mat = "A992" ::\::

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (W" & sSizeWt & "#" &  ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BEAM,W," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock , RMDesc , mat , GW , GH , GT , GT1 , GIR }
                  '  0          1       2        3     4    5    6    7     8
    Return returnvalues
  End Function

  Public Shared Function getSBeamRM_A992( ByVal BeamWidth As String,
                                   ByVal BeamWeight As String,
                          Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BeamWidth & "x" & BeamWeight 

    ' Default material is A992
    mat = "A992"

    Select Case sSizeWt
      Case "10x35" 
        stock = "58118"
      Case "12x31.8" 
        stock = "71631"
      Case "18x54.7" 
        stock = "13230"
      Case "3x5.7" 
        stock = "13202"
      Case "3x7.5" 
        stock = "13203"
      Case "4x7.7" 
        stock = "13205"
      Case "4x9.5" 
        stock = "53242"
      Case "5x10" 
        stock = "13208"
      Case "8x18.4" 
        stock = "13217"
      Case "8x23" 
        stock = "58844"

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (S" & sSizeWt & ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BEAM,S," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock, RMDesc , mat }

    If stock = "" Then returnvalues = getSBeamRM_A572(BeamWidth,BeamWeight,Length)
    Echo("Returns: { PartDesc: " & PartDesc & ", stock: " & stock & ", RMDesc: " & RMDesc & " , mat: " & mat & " }")

    Return returnvalues
  End Function  

  Public Shared Function getSBeamRM_A572( ByVal BeamWidth As String,
                                   ByVal BeamWeight As String,
                          Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BeamWidth & "x" & BeamWeight 

    ' Default material is A992
    mat = "A572 50"

    Select Case sSizeWt
      Case "10x25.4" 
          stock = "60951"

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (S" & sSizeWt & ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BEAM,S," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock, RMDesc , mat }

    If stock = "" Then returnvalues = getSBeamRM_A36(BeamWidth,BeamWeight,Length)
    Echo("Returns: { PartDesc: " & PartDesc & ", stock: " & stock & ", RMDesc: " & RMDesc & " , mat: " & mat & " }")

    Return returnvalues
  End Function  

  Public Shared Function getSBeamRM_A36( ByVal BeamWidth As String,
                                         ByVal BeamWeight As String,
                                Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BeamWidth & "x" & BeamWeight 

    ' Default material is A992
    mat = "A36"

    Select Case sSizeWt
      Case "10x25.4"
        stock = "13220"
      Case "5x14.75"
        stock = "69215"
      Case "6x12.5"
        stock = "72913"

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (S" & sSizeWt & ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BEAM,S," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock, RMDesc , mat }
    Echo(sSizeWt & ":: Returns: { PartDesc: " & PartDesc & ", stock: " & stock & ", RMDesc: " & RMDesc & " , mat: " & mat & " }")

    Return returnvalues
  End Function  
#End Region

#Region "Bars"
  ''' <summary>
  ''' Using Shape Size and Weight values. The measures are evaluated to Vis Numbers
  ''' and formatted strings are returned for use as Descriptions and Measures
  ''' </summary>
  ''' <param name="BeamWidth">String: W Shape's Size</param>
  ''' <param name="BeamWeight">String: W Shape's Weight</param>
  ''' <param name="Length">(Optional) Length (in)</param>
  ''' <returns>String Array: Part Desc; StockNo; RM Desc; Material </returns>
  ''' <remarks></remarks>
  Public Shared Function getBarRM_A36( ByVal BarThk As String,
                                        ByVal BarWidth As String,
                               Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BarThk & "x" & BarWidth 

    ' Default material is A36
    mat = "A36"

    Select Case sSizeWt
      ''' A36
      Case "1x2"
        stock = "65969"
      Case "1x6"
        stock = "53117"
      Case "1x8"
        stock = "57627"
      Case "1-1/4x3"
        stock = "60610"
      Case "1-1/4x8"
        stock = "57628"
      Case "1/2x1"
        stock = "12376"
      Case "1/2x1-1/2"
        stock = "59377"
      Case "1/2x12"
        stock = "58232"
      Case "1/2x2"
        stock = "12380"
      Case "1/2x2-1/2"
        stock = "12385"
      Case "1/2x3"
        stock = "12387"
      Case "1/2x3/4"
        stock = "66124"
      Case "1/2x4-1/2"
        stock = "54771"
      Case "1/2x5"
        stock = "54069"
      Case "1/2x6"
        stock = "56877"
      Case "1/2x8"
        stock = "56878"
      Case "1/4x1"
        stock = "12323"
      Case "1/4x1-1/2"
        stock = "12325"
      Case "1/4x1-1/2x20'"
        stock = "32520"
      Case "1/4x1-1/4"
        stock = "12324"
      Case "1/4x1-3/4"
        stock = "12326"
      Case "1/4x2"
        stock = "12330"
      Case "1/4x2-1/2"
        stock = "12335"
      Case "1/4x3"
        stock = "12337"
      Case "1/4x3/4"
        stock = "79055"
      Case "1/4x4"
        stock = "12339"
      Case "1/4x4-1/2"
        stock = "56091"
      Case "1/4x5"
        stock = "54021"
      Case "1/4x6"
        stock = "53161"
      Case "1/8x1-1/2"
        stock = "70400"
      Case "1/8x2"
        stock = "12310"
      Case "1/8x3"
        stock = "75000"
      Case "2x4"
        stock = "65744"
      Case "3/16x1-1/2"
        stock = "12316"
      Case "3/16x1-3/4"
        stock = "70414"
      Case "3/16x2"
        stock = "12319"
      Case "3/16x3"
        stock = "60772"
      Case "3/16x4"
        stock = "53131"
      Case "3/4x1-1/2"
        stock = "70380"
      Case "3/4x2"
        stock = "57064"
      Case "3/4x2-1/2"
        stock = "56947"
      Case "3/4x5"
        stock = "54316"
      Case "3/4x6"
        stock = "57717"
      Case "3/4x8"
        stock = "57612"
      Case "3/8x1"
        stock = "12359"
      Case "3/8x1-1/2"
        stock = "58354"
      Case "3/8x1-3/4"
        stock = "54147"
      Case "3/8x10"
        stock = "20217"
      Case "3/8x2"
        stock = "12365"
      Case "3/8x2-1/2"
        stock = "12368"
      Case "3/8x2-3/4"
        stock = "32338"
      Case "3/8x3"
        stock = "12370"
      Case "3/8x3-1/2"
        stock = "60092"
      Case "3/8x4"
        stock = "59824"
      Case "3/8x4-1/2"
        stock = "12373"
      Case "3/8x5"
        stock = "78183"
      Case "3/8x6"
        stock = "59714"
      Case "3/8x8"
        stock = "57626"
      Case "5/16x2"
        stock = "53481"
      Case "5/16x4"
        stock = "66086"
      Case "5/8x1"
        stock = "12395"
      Case "5/8x1-1/2"
        stock = "70305"
      Case "5/8x1-3/4"
        stock = "70401"
      Case "5/8x2"
        stock = "57630"
      Case "5/8x3"
        stock = "59804"
      Case "5/8x8"
        stock = "54186"
      Case "7/16x2"
        stock = "53243"
      Case "7/8x2"
        stock = "32426"

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (" & sSizeWt & ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BAR,FLT," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock , RMDesc , mat }
                  '  0          1       2        3
    Return returnvalues
  End Function

  Public Shared Function getBarRM_A572( ByVal BarThk As String,
                                        ByVal BarWidth As String,
                               Optional ByVal Length As Double = 0.0) As String()
    Dim returnvalues As String()
    Dim stock, mat, sSizeWt As String
    sSizeWt = BarThk & "x" & BarWidth 

    ' Default material is A572,50
    mat = "A572 50"

    Select Case sSizeWt
      ''' A572,50
      Case "1x2"
        stock = "65900"
      Case "1/2x1-1/2"
        stock = "60807"
      Case "1/2x2"
        stock = "60553"
      Case "1/2x2-1/2"
        stock = "65708"
      Case "1/2x3"
        stock = "60254"
      Case "1/2x3-1/2"
        stock = "60588"
      Case "1/2x4"
        stock = "60554"
      Case "1/2x5"
        stock = "72921"
      Case "1/4x1"
        stock = "60007"
      Case "1/4x1-1/2"
        stock = "60018"
      Case "1/4x1-1/4"
        stock = "72803"
      Case "1/4x1-3/4"
        stock = "66742"
      Case "1/4x2"
        stock = "54915"
      Case "1/4x2-1/2"
        stock = "54922"
      Case "1/4x3"
        stock = "54923"
      Case "1/4x4"
        stock = "70558"
      Case "1/4x6"
        stock = "54954"
      Case "1/8X1-1/2"
        stock = "71569"
      Case "1/8x2"
        stock = "54916"
      Case "3/16x2"
        stock = "60016"
      Case "3/16x4"
        stock = "71570"
      Case "3/4x1-1/2"
        stock = "70381"
      Case "3/4x2"
        stock = "54917"
      Case "3/4x2-1/2"
        stock = "72628"
      Case "3/4x3"
        stock = "75176"
      Case "3/4x3-1/2"
        stock = "60762"
      Case "3/8x1"
        stock = "71229"
      Case "3/8x1-1/2"
        stock = "71018"
      Case "3/8x2"
        stock = "60131"
      Case "3/8x2-1/2"
        stock = "54984"
      Case "3/8x3"
        stock = "54865"
      Case "3/8x3-1/2"
        stock = "60587"
      Case "3/8x4-1/2"
        stock = "60361"
      Case "3/8x5"
        stock = "60785"
      Case "5/16x2"
        stock = "70853"
      Case "5/8x2"
        stock = "54985"
      Case "5/8x2-1/2"
        stock = "78195"
      Case "5/8x3"
        stock = "60968"
      Case "5/8x8"
        stock = "65673"

      Case Else ' nothing Found
          stock = "" :  mat = ""
    End Select

    Dim PartDesc, RMDesc As String
    PartDesc = "=<Title> (" & sSizeWt & ")"

    'Return RM Description formatted as: BEAM,W,27x84,A992
    RMDesc = "=BAR,FLT," & sSizeWt & ", <Material>"

    returnvalues = { PartDesc , stock , RMDesc , mat }
                  '  0          1       2        3
    Return returnvalues
  End Function

#End Region

End Class

