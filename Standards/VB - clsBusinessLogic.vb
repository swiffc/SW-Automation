' <IsStraightVb>True</IsStraightVb>
Imports clsDebugViewer
Imports Inventor
Imports System.Windows.Forms


'''_______________ Child Sub Reqs Start Here _______________
  'AddVbFile "VB - clsBusinessLogic.vb"
  'AddVbFile "VB - clsDebugViewer.vb"

  'Sub Main()
  ' Dim bl As New clsBusinessLogic
  '''Use 'bl.' as a prefix for any method called from "VB - clsBusinessLogic.vb"
  'End Sub
'''_______________ Child Sub Reqs End Here _______________


'--Class iLogic Start--------------------------------------------------------------------------------------------------


Public Class clsBusinessLogic
''' Contains Test Methods for Class
''' Public Functions:: 
'''     GetValidSaveFileName(String, String, {String}, {Bool}, {String}, {Bool}, {String}, {Bool}, {String}) As String
'''
''' Public Subs::
'''
'--------------------------------------------------------------------------
''Class Public Variable Declarations
 Public Dim bVerboseDebug_clsBusinessLogic As Boolean = True
'--------------------------------------------------------------------------

Sub Test()
 '''Run this sub for testing Class Connection and Sub Functions

  Try
  Catch ex As Exception
  End Try
End Sub

Function GetTipClearance(ByVal fanDiameter As Double) As Double
    Dim tipClearance As Double
    ' calculate fan tip clearance
    ' evaluated as radial clearance from 
    ' Radial Max&Min Values given in 
    '    			:: API 661e7(2015) p26 Table 6 ::
    ' Max + Min / 2 gives best case average, radial
    Select Case fanDiameter
        Case 0 To 108 		'         Fan_Dia <= 9 ft
            tipClearance = ( 1/4 + 1/2 ) / 2
        Case 108 To 132		'  9 ft < Fan_Dia <= 11 ft
            tipClearance = ( 1/4 + 5/8 ) / 2
        Case Else			' 11 ft < Fan_Dia
            tipClearance = ( 1/4 + 3/4 ) / 2		
    End Select

    Return tipClearance
End Function

'Function GetFanRingBoltCount(ByVal fanDiameter As Double, <Out()> ByRef degrees As Double) As Integer
Function GetFanRingBoltAngle(ByVal fanDiameter As Double) As Double
    Dim angle, holeCount As Double
    FanRingHolesAndAngles(fanDiameter, angle, holeCount)
 Return angle
End Function

Function GetFanRingBoltCount(ByVal fanDiameter As Double) As Double
    Dim angle, holeCount As Double
    FanRingHolesAndAngles(fanDiameter, angle, holeCount)
    Return holeCount
End Function

Private Sub FanRingHolesAndAngles(ByVal fanDiameter As Double, ByRef angle As Double, ByRef holeCount As Double)
    Select Case fanDiameter
        Case Is > 144
            angle = 11.25
            holeCount = 8

        Case Is > 96
            angle = 15.0
            holeCount = 6

        Case Is > 72
            angle = 22.5
            holeCount = 4

        Case Is > 60
            angle = 30.0
            holeCount = 3

        Case Else
            angle = 30.0
            holeCount = 6
         
    End Select
    
    ' If fanDiameter > 144 Then
    '     angle = 11.25
    '     holeCount = 8
    ' End If
    
    ' If fanDiameter <= 144 Then
    '     angle = 15.0
    '     holeCount = 6
    ' End If
    
    ' If fanDiameter <= 96 Then
    '     angle = 22.5
    '     holeCount = 4
    ' End If
    
    ' If fanDiameter <= 72 Then
    '     angle = 30.0
    '     holeCount = 3
    ' End If
	
	' If fanDiameter <= 60 Then
    '     holeCount = 6
    ' End If
End Sub

Function GetFanRingBoltSpacingArcLength(ByVal fanDiameter As Double, ByVal fanRingGage As Double) As Double
    Echo("Given Fan Diameter: " & fanDiameter)
    Echo("Given Flange Gage: " & fanRingGage)
    'fan deck connections  
    Dim angle, holeCount As Double
    angle = GetFanRingBoltAngle(fanDiameter)
    holeCount = 360/angle
    Echo("Angle Between Holes: " & angle)
    Echo("Hole Count: " & holeCount)

    Dim  ringID, PCD As Double
    ringID = GetFanRingID(fanDiameter)
    PCD = ringID + (2 * fanRingGage)

Echo("Ring Inside Diameter: " & ringID)
Echo("Hole Pitch Circle Diametert: " & PCD)

    Dim p, arcLength As Double
    p = 355/113
    arcLength = (p * PCD) / holeCount
Echo("Calculated Circumfrential Length.. Arc Length: " & arcLength)
 Return arcLength
End Function

 Function GetFanRingID(ByVal fanDiameter As Double) As Double
	Dim tipRadialClearance = GetTipClearance(fanDiameter)
	Dim ringID As Double
    ringID = fanDiameter + (2 * tipRadialClearance)
    Return ringID
 End Function

End Class
'--Class iLogic End--------------------------------------------------------------------------------------------------
