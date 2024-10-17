' <IsStraightVb>True</IsStraightVb>
'AddVbFile "VB - clsDebugViewer.vb"
Imports clsDebugViewer
Imports Inventor
'################################################################################
'# Fur Usage in a iLogic Rule Add the Following Two Lines Above Sub Main()
' AddVbFile "VB - clsStringMath.vb"
' Imports clsStringMath
'# 
'################################################################################
'# Example :: 
'#
' AddVbFile "VB - clsDebugViewer.vb"
' AddVbFile "VB - clsStringMath.vb"
' Imports clsStringMath
' Imports clsDebugViewer

' Sub Main()
'  Dim oDoc As Document = ThisDoc.Document

'  EchoTitle("Working Fraction Finder")
  
'   LongValue = getFractionalFeet( Max(d1,d0) / 12 )
'   ShortValue = getFractionalFeet( Min(d1,d0) / 12 )
' End Sub

'################################################################################

Class clsStringMath

#Region "Fractions"
    ''' <summary>
    ''' Return a fraction string from a double.
    ''' </summary>
    ''' <param name="d">The double to convert.</param>
    ''' <returns>The converted string.</returns>
    ''' <remarks></remarks>
    Public Shared Function getShortFeet( ByVal dFeet As Double,
                                Optional ByVal iDenominator As Integer = 32  ) As String
      Echo("Rounding " & dFeet & " to the nearest " & iDenominator & "th")
      Dim Check As String = getFractionalFeet( dFeet , iDenominator )
      If Check.StartsWith("0'-") Then Check = Trim( Right(Check, Len(Check) - 3))
      Return Check
    End Function

    Public Shared Function getFractionalFeet(ByVal dFeet As Double,
                                    Optional ByVal iDenominator As Integer = 32 ) As String
      Dim sFracFeet As String
      'Dim iDenominator = 32  'highest fractional accuracy

      Try
          Dim iValue As Integer = Floor( dFeet )
          Dim dInches As Double = ( dFeet - iValue ) * 12

          If dInches > 0 Then 
            sFracFeet = iValue & "'-"
          Else
            Return iValue & "'"
          End If
          Echo("Value : " & Round(dFeet,4) & " : converts to ::" & iValue & "ft " & dInches & " in" )

          Dim sInches As String
          Dim iInches As Integer = Floor( dInches )
          Dim dFraction As Double = ( dInches - iInches )
          Echo("Value : " & Round(dInches,4) & " : converts to ::" & iInches & " " & GetFraction(dFraction, iDenominator) & "in ")

          If (1 - dFraction) < ( 1 / ( 2 * iDenominator) ) Then 'Check if difference is measureable for fractions
          Echo("Check if difference is measureable for fractions")
            sFracFeet = sFracFeet & Round(dInches, 0, MidpointRounding.AwayFromZero) & """"
            Echo("Returning :1: " & sFracFeet) : Return CheckString(sFracFeet)
          End If

          If dFraction < ( 1 / ( 2 * iDenominator)) Then
            If iInches > 0 Then sFracFeet &= iInches & """"
            Echo( sFracFeet & " --- " & GetFraction(dFraction, iDenominator) )
            Echo("Returning :2: " & sFracFeet) : Return CheckString(sFracFeet)
          Else
            If dFraction > 0 Then 
                If iInches > 0 Then sInches = iInches & " "
                sInches &= GetFraction(dFraction, iDenominator) & """"
              Else
                If iInches > 0 Then sInches = iInches & """"
            End If
            sFracFeet = sFracFeet & sInches
            Echo(sFracFeet)
            Echo("Returning :3: " & sFracFeet) : Return CheckString(sFracFeet)
          End If
        Catch ex As Exception 
         Echo("Error in getFractionalFeet :: " & ex.Message)
      End Try
      Echo("Returning :() " & sFracFeet)

      Return CheckString(sFracFeet)
    End Function

    Public Shared Function CheckString(sFeet As String) As String
      Dim sFracFeet As String
      Try
        sFracFeet = sFeet
        If sFracFeet.EndsWith("-") Then sFracFeet = Left(sFracFeet, Len(sFracFeet) - 1 )
        'If sFracFeet.StartsWith("0'-") Then sFracFeet = Right(sFracFeet, Len(sFracFeet) - 3 )
      Catch ex As Exception
      End Try
      Return sFracFeet
    End Function

    Public Shared Function GetFraction(ByVal d As Double,
                Optional ByVal MaxDenom As Integer = 32) As String
        'Dim fix As Double = 2 * MaxDenom 'Encourage a Round Up
        'd = Ceiling( d * fix ) / fix
        If d = 0 Then Return "0"
        Return GetRoundedFraction(d, MaxDenom)
    End Function

    Public Shared Function GetNumerator(ByVal x As Decimal, ByVal denominator As Integer) As Decimal
      'discard whole number
      x = x Mod 1
      'get numerator
      x = x * denominator
      'round up to whole number
      Return Decimal.Ceiling(x)
    End Function

    Public Shared Function GetGCF(ByVal x As Integer, ByVal x1 As Integer) As Integer
      x = Math.Abs(x)
      x1 = Math.Abs(x1)

      If x = x1 Then Return x
        If x > x1 Then
          If x Mod x1 = 0 Then
          Return x1

        Else
          For i As Integer = x1 - 1 To 1 Step - 1
            If x Mod i = 0 And x1 Mod i = 0 Then
              Return i
            End If
          Next

        End If
      Else
        If x1 Mod x = 0 Then
        Return x

        Else
          For i As Integer = x - 1 To 1 Step - 1
            If x1 Mod i = 0 And x Mod i = 0 Then
              Return i
            End If
          Next

        End If
      End If
    End Function

    Public Shared Function GetRoundedFraction(ByVal d As Decimal, ByVal Maxd As Integer) As String

      Dim Numerator As Integer = GetNumerator(d, Maxd)
      Dim GCF As Integer = GetGCF(Numerator, Maxd)

      If GCF = 0 Then Return "0"

      Numerator = CInt(Numerator / GCF)

      Dim Denominator As Integer = CInt(Maxd / GCF)
      Return (Numerator & "/" & Denominator)

    End Function
#End Region

#Region "Sheet Metal"

  Public Shared Function GetSheetMetalDescription(ByVal SelectType As String, 
                                                  ByVal sShort As String, ByVal dThickness As Double, ByVal dWidth As Double,
                                         Optional ByVal sCritical As String = "") As String
    Dim sDescription As String = sShort

    Try
        If Len(sCritical) > 0 Then sDescription &= ", " & sCritical

        Dim sThickness As String = " 10 Ga"
        If Not ( CDbl(dThickness) > 0.1265 And CDbl(dThickness) < 0.1425 ) Then sThickness = GetFraction(dThickness,32)

        Select Case UCase(SelectType)
          Case "PL"
            sDescription &= " ( PL" & sThickness & " x " & Round(dWidth,3) & " )"

          Case "PRL"
            sDescription &= " ( PRL, " & Trim(sThickness) & " x " & Round(dWidth,3) & " )"

          Case "PRC"
            sDescription &= " ( PRC, " & Trim(sThickness) & " x " & Round(dWidth,3) & " )"

          Case "PRZ"
            sDescription &= " ( PRZ, " & Trim(sThickness) & " x " & Round(dWidth,3) & " )"
        End Select

    Catch ex As Exception
    End Try

    Echo("  ---  GetSheetMetalDescription returned as: " & sDescription)
    Return sDescription
  End Function

  Public Shared Function GetSheetMetalArea(oCD As ComponentDefinition)
    Try
        Dim oMP As MassProperties
        oMP = oCD.MassProperties
        
        Dim smArea As Double
        smArea = oMP.Volume / oCD.Thickness.Value 'cm^2
        smArea = smArea / ( 2.54 * 2.54 ) 'in^2
        smArea = smArea / ( 12 * 12 ) 'ft^2

        If smArea < 100 Then 
          smArea = DecimalRounding(smArea, 2)
        Else
          smArea = DecimalRounding(smArea, 1)
        End If

        Return smArea
        
    Catch ex As Exception : MsgBox("SetSMArea :: " & ex.Message)
    End Try

  End Function

  Public Shared Function DecimalRounding(Value As Double, Accuracy As Integer) As String
    Dim iCal As Integer = 1
    Dim sCal As String = "1"
    Dim ReturnValue As String = Value

    Try
      If Accuracy = 0 Then
        sCal = "1"
       Else
        For i As Integer = 1 To Accuracy
          sCal &= "0"
        Next i
        iCal = CInt(sCal)
      End If

    Catch ex1 As Exception : MsgBox("DecimalRounding :: " & ex1.Message)
      Return ""

    End Try

    ReturnValue = Ceil( Value * iCal ) / iCal
    
    Dim Index As Integer = Max( ReturnValue.IndexOf("."c) , 0 )
    If Index = 0 Then
      Index = Len( ReturnValue )
      If Not ReturnValue.EndsWith("."c) Then ReturnValue &= "."c
    End If

    Dim Decimals As Integer = Len( ReturnValue ) - Index
    If Decimals < Accuracy
      For i As Integer = Decimals To Accuracy
        ReturnValue &= "0"
      Next i
    End If

    Return ReturnValue
  End Function

#End Region

End Class

