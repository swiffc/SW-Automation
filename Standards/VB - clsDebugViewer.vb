' <IsStraightVb>True</IsStraightVb>
Imports System.Diagnostics


'''_______________ Child Sub Reqs Start Here _______________
'AddVbFile "VB - clsDebugViewer.vb"
'Imports clsDebugViewer
'Sub Main()
'
'End Sub
'''_______________ Child Sub Reqs End Here _______________


Public Class clsDebugViewer
'''Contains the methods used for trace.line functions passed to the MS Debug Viewer
  '''_____ Start Namespace Requirements _____
    'AddVbFile "VB - clsDebugViewer.vb"
    'Imports clsDebugViewer
  '''_____ End Namespace Requirements _____
'' Public Functions:: N/A
'' Public Subs::
''     Echo(String, {Integer}) {Shared}
 '       (Indentable Simple Text)
''     EchoTitle(String, {Integer}) {Shared}
 '       (Fully Encapsulated Title for Major Processes Text)
''     EchoLine({Integer}, {Bool}, {Bool}) {Shared}
 '       (120 Underscores, Repeatable, Option for Space After {T})
''     BreakTitle(String, {Bool}, {Bool}) {Shared}
 '       (Centered Text with Double Lines, Option for Space After {T})
''     DebugLine(String, {Boolean}, {Integer}, {String}, {Integer}) {Shared}
 '       (Extended Messaging, See Below)
''     ClearDebug()

  Sub Main()
   EchoTitle("clsDebugViewer Test")
    DebugLine("Testing Debug Line Script"):DebugLine("",,,,120)
    DebugLine("Type 1",,1,"Supplemental",1)
    DebugLine("Type 2",,2,"Supplemental",2):DebugLine("",,,,120)
    DebugLine("Type 3",,3,"Supplemental",1)
    DebugLine("Type 4",,4,"Supplemental",2)
    DebugLine("Type 5",,5,"Supplemental",3):DebugLine("",,,,120)
    DebugLine("Type 6",,6,"Supplemental",1)
   EchoLine()
   EchoLine(,,False)
   BreakTitle("Break Title")
   Echo("Its Simple")
   EchoLine(3,,False)
  End Sub


#Region "Methods"
  Public Shared Sub Echo(Optional ByVal Message As String = "",
                         Optional ByVal Indent As Integer=0,
                         Optional ByVal bVerbose As Boolean = True)
  '
   If Not bVerbose Then Exit Sub

   DebugLine(Message,,,,Indent)
  End Sub

  Public Shared Sub Echo(Optional ByVal Message As String = "",
                         Optional ByVal bVerbose As Boolean = True)
  '
   If Not bVerbose Then Exit Sub

   DebugLine(Message,,,,)
  End Sub

  Public Shared Sub Echo(Optional ByVal Message As String = "")
  '

   DebugLine(Message,,,,)
  End Sub

  Public Shared Sub EchoTitle(ByVal Title As String, 
                     Optional ByVal bVerbose As Boolean = True)
  '

   If Not bVerbose Then Exit Sub

   EchoLine(,,False):BreakTitle(Title,,False):EchoLine(,,False):Echo("")
  End Sub

  Public Shared Sub EchoLine(Optional ByVal Qty As Integer=0,
                             Optional ByVal bVerbose As Boolean = True,
                             Optional ByVal bSpaceAfter As Boolean = True)
  '
   If Not bVerbose Then Exit Sub

    'Dim j As Integer
    Dim i As Integer
    Dim sLine As String = ""

    For i = 1 To 120
     sLine &= "_"
    Next i

    For i = 0 To Max(0,Qty-1)
    Debugline(sLine)
    Next i
    If bSpaceAfter Then Echo("")
  End Sub

  Public Shared Sub BreakTitle(ByVal Title As String,
                      Optional ByVal bVerbose As Boolean = True,
                      Optional ByVal bSpaceAfter As Boolean = True)
  '

   If Not bVerbose Then Exit Sub

    Dim i As Integer
    Dim iSpaceLength As Integer
    Dim sLead As String = ""
    Dim sTail As String = ""

    iSpaceLength = 120 - ( 2 + Len(Title))

    For i = 1 To iSpaceLength\2
     sLead &= "_"
     sTail &= "_"
    Next i
    If iSpaceLength Mod 2 <> 0 Then sTail = sTail & "_"

    DebugLine(sLead & Space( 2 + Len(Title)) & sTail )
    DebugLine(sLead & " " & Title & " " & sTail )
    If bSpaceAfter Then Echo("")

  End Sub

  Public Shared Sub DebugLine(ByVal Message As String,
              Optional ByRef Output As Boolean = True,
              Optional ByVal MsgType As Integer = 0,
              Optional ByVal Supplement As String = "",
              Optional ByVal Indent As Integer = 0)
  '  DebugLine(sMessage, bVerboseDebug,,,) ''For Testing Only
  '  DebugLine(sMessage,,,,) ''Always On

    If Not Output Then Exit Sub

    Dim Pre As String = ""
    ''Calculate Indent String for Debug Viewer
    If Indent > 0
      Pre = " "
      For i = 1 To Indent
        Pre = Pre & "-"
        i= i+1
      Next i
      Message = Pre & " " & Message
    End If

   Try
    Select Case MsgType
    Case 1
    'F.Start
    Trace.WriteLine(
      "[AIP] " & Message &
      " Function Started")
    Case 2
    'F.Start w/ Values
      Trace.WriteLine(
        "[AIP] " & Message &
        " Function Started with Values:- " & Supplement)
    Case 3
    'F.Exit
      Trace.WriteLine(
        "[AIP] " & Message &
        " Function Exited")
    Case 4
    'F.Exit w/Values
      Trace.WriteLine(
        "[AIP] " & Message &
        " Function Exited with Return Values:- " & Supplement)
    Case 5
    'F.Exit w/Error
      Trace.WriteLine(
        "[AIP] " & Message &
        " Function Exited with Error :: " & Supplement)
    Case 6
    'Message :: Supplement
      Trace.WriteLine(
        "[AIP] " & Message &
        " :: " & Supplement)
    Case Else
    'Message
      Trace.WriteLine(
        "[AIP] " & Message)
    End Select
   Catch ex As Exception
    MsgBox("Error in the DebugScript " & ex.Message)
    Exit Sub
   End Try
  End Sub

  Public Shared Sub ClearDebug()

    Trace.WriteLine("DBGVIEWCLEAR   [AIP]")
    'Echo()

  End Sub

  Public Shared Sub EchoEnums()
    Echo("	ValueUnitsTypeEnum Enumerator")
    Echo("	kAngleUnits 94977 Angle units. ")
    Echo("	kAngularVelocityUnits 94978 AngularVelocity units. ")
    Echo("	kAreaUnits 94979 Area units. ")
    Echo("	kCurrentUnits 94980 Current units. ")
    Echo("	kForceUnits 94981 Force units. ")
    Echo("	kLengthUnits 94982 Length units. ")
    Echo("	kMassUnits 94983 Mass units. ")
    Echo("	kPowerUnits 94984 Power units. ")
    Echo("	kPressureUnits 94985 Pressure units. ")
    Echo("	kSpeedUnits 94986 Speed units. ")
    Echo("	kTemperatureUnits 94987 Temperature units. ")
    Echo("	kTimeUnits 94988 Time units. ")
    Echo("	kUnitless 94989 Unitless. ")
    Echo("	kVoltageUnits 94990 Voltage units. ")
    Echo("	kVolumeUnits 94991 Volume units. ")
    Echo("	kWorkUnits 94992 Work units. ")
    EchoLine()
    Echo("	UnitsTypeEnum Enumerator ")
    Echo("	kAcreAreaUnits	                11301	Acre Area.	")
    Echo("	kAmpElectricalCurrentUnits	    11327	Amp electrical Current.	")
    Echo("	kBooleanUnits	                  11347	Boolean (Yes/No).	")
    Echo("	kBTUWorkUnits	                  11320	BTU Work.	")
    Echo("	kCalorieWorkUnits	              11319	Calorie Work.	")
    Echo("	kCandelaLuminousIntensityUnits	11342	Candela luminous intensity.	")
    Echo("	kCelsiusTemperatureUnits	      11296	Celsius Temperature.	")
    Echo("	kCentimeterLengthUnits	        11268	Centimeter Length.	")
    Echo("	kCircularMilAreaUnits	          11326	CircularMil Area.	")
    Echo("	kCompositeUnits	                11322	Composite.	")
    Echo("	kCoulombElectricalChargeUnits	  11330	Coulomb electrical Charge.	")
    Echo("	kCupVolumeUnits	                11306	Cup Volume.	")
    Echo("	kDatabaseAngleUnits	            11277	Database units for angle -- ALWAYS Radian.	")
    Echo("	kDatabaseLengthUnits	          11267	Database units for length -- ALWAYS Centimeter.	")
    Echo("	kDatabaseMassUnits	            11282	Database units for mass -- ALWAYS Kilogram.	")
    Echo("	kDatabaseTemperatureUnits	      11294	Database units for temperature -- ALWAYS Kelvin.	")
    Echo("	kDatabaseTimeUnits	            11289	Database units for time -- ALWAYS Second.	")
    Echo("	kDefaultDisplayAngleUnits	      11276	Current default display units for Angle.	")
    Echo("	kDefaultDisplayLengthUnits	    11266	Current default display units for length.	")
    Echo("	kDefaultDisplayMassUnits	      11281	Current default display units for Mass.	")
    Echo("	kDefaultDisplayTemperatureUnits	11293	Current default display units for Temperature.	")
    Echo("	kDefaultDisplayTimeUnits	      11288	Current default display units for Time.	")
    Echo("	kDegreeAngleUnits	              11279	Degree Angle.	")
    Echo("	kDyneForceUnits	                11312	Dyne Force.	")
    Echo("	kErgWorkUnits	                  11318	Erg Work.	")
    Echo("	kFahrenheitTemperatureUnits	    11297	Fahrenheit Temperature.	")
    Echo("	kFaradElectricalCapacitanceUnits	11331	Farad electrical Capacitance.	")
    Echo("	kFeetPerSecondSpeedUnits	      11299	FeetPerSecond Speed.	")
    Echo("	kFootLengthUnits	              11273	Foot Length.	")
    Echo("	kGallonVolumeUnits	            11303	Gallon Volume.	")
    Echo("	kGammaMagneticInductionUnits	  11337	Gamma magnetic Induction.	")
    Echo("	kGaussMagneticInductionUnits	  11338	Gauss magnetic Induction.	")
    Echo("	kGradAngleUnits	                11280	Grad Angle.	")
    Echo("	kGramMassUnits	                11284	Gram Mass.	")
    Echo("	kHenryElectricalInductanceUnits	11339	Henry electrical Inductance.	")
    Echo("	kHertzFrequencyUnits	          11341	Hertz Frequency.	")
    Echo("	kHorsePowerPowerUnits	          11316	HorsePower Power.	")
    Echo("	kHourTimeUnits	                11292	Hour Time.	")
    Echo("	kInchLengthUnits	              11272	Inch Length.	")
    Echo("	kJouleWorkUnits	                11317	Joule Work.	")
    Echo("	kKelvinTemperatureUnits	        11295	Kelvin Temperature.	")
    Echo("	kKilogramMassUnits	            11283	Kilogram Mass.	")
    Echo("	kKSIPressureUnits	              11310	KSI Pressure.	")
    Echo("	kLbForceUnits	                  11313	Lb Force.	")
    Echo("	kLbMassMassUnits	              11286	LbMass Mass.	")
    Echo("	kLiterVolumeUnits	              11302	Liter Volume.	")
    Echo("	kLumenLuminousFluxUnits	        11343	Lumen luminous flux.	")
    Echo("	kLuxIlluminationUnits	          11344	Lux illumination.	")
    Echo("	kMaxwellMagneticFluxUnits	      11335	Maxwell magnetic Flux.	")
    Echo("	kMeterLengthUnits	              11270	Meter Length.	")
    Echo("	kMetersPerSecondSpeedUnits	    11298	MetersPerSecond Speed.	")
    Echo("	kmhoElectricalConductanceUnits	11333	mho electrical Conductance.	")
    Echo("	kMicronLengthUnits	            11271	Micron Length.	")
    Echo("	kMileLengthUnits	              11275	Mile Length.	")
    Echo("	kMilesPerHourSpeedUnits	        11300	MilesPerHour Speed.	")
    Echo("	kMilLengthUnits	                11324	Mil Length.	")
    Echo("	kMillimeterLengthUnits	        11269	Millimeter Length.	")
    Echo("	kMinuteTimeUnits	              11291	Minute Time.	")
    Echo("	kMoleSubstanceUnits	            11345	Mole Substance or gram molecular weight.	")
    Echo("	kNauticalMileLengthUnits	      11323	NauticalMile Length.	")
    Echo("	kNewtonForceUnits	              11311	Newton Force.	")
    Echo("	kOerstedMagneticInductionUnits	11340	Oersted magnetic Induction.	")
    Echo("	kOhmElectricalResistanceUnits	  11329	Ohm electrical Resistance.	")
    Echo("	kOunceForceUnits	              11314	Ounce Force.	")
    Echo("	kOunceMassUnits	                11287	Ounce Mass.	")
    Echo("	kOunceVolumeUnits	              11307	Ounce Volume.	")
    Echo("	kPascalPressureUnits	          11308	Pascal Pressure.	")
    Echo("	kPintVolumeUnits	              11305	Pint Volume.	")
    Echo("	kPSIPressureUnits	              11309	PSI Pressure.	")
    Echo("	kQuartVolumeUnits	              11304	Quart Volume.	")
    Echo("	kRadianAngleUnits	              11278	Radian Angle.	")
    Echo("	kRPMAngularVelocityUnits	      11321	RPM AngularVelocity.	")
    Echo("	kSecondTimeUnits	              11290	Second Time.	")
    Echo("	kSiemensElectricalConductanceUnits	11332	Siemens electrical Conductance.	")
    Echo("	kSlugMassUnits	                11285	Slug Mass.	")
    Echo("	kSteradianAngleUnits	          11325	Steradian Angle.	")
    Echo("	kTeslaMagneticInductionUnits	  11336	Tesla magnetic Induction.	")
    Echo("	kTextUnits	                    11346	Text (String).	")
    Echo("	kUnitlessUnits	                11265	No dimension associated with this value.	")
    Echo("	kVoltElectricalVoltageUnits	    11328	Volt electrical Voltage.	")
    Echo("	kWattPowerUnits	                11315	Watt Power.	")
    Echo("	kWeberMagneticFluxUnits	        11334	Weber magnetic Flux.	")
    Echo("	kYardLengthUnits	              11274	Yard Length.	")
  End Sub
#End Region
End Class


'''Message Box Settings

' Return Value
' Constant    Value
' OK          1
' Cancel      2
' Abort       3
' Retry       4
' Ignore      5
' Yes         6
' No          7

' The MsgBoxStyle enumeration values are listed in the following table.
' Member              Value           Description
' OKOnly              0               Displays OK button only.
' OKCancel            1               Displays OK and Cancel buttons.
' AbortRetryIgnore    2               Displays Abort, Retry, and Ignore buttons.
' YesNoCancel         3               Displays Yes, No, and Cancel buttons.
' YesNo               4               Displays Yes and No buttons.
' RetryCancel         5               Displays Retry and Cancel buttons.

' Critical            16              Displays Critical Message icon.
' Question            32              Displays Warning Query icon.
' Exclamation         48              Displays Warning Message icon.
' Information         64              Displays Information Message icon.

' DefaultButton1      0               First button is default.
' DefaultButton2      256             Second button is default.
' DefaultButton3      512             Third button is default.

' ApplicationModal    0               Application is modal. The user must respond to the message box before continuing work in the current application.
' SystemModal         4096            System is modal. All applications are suspended until the user responds to the message box.
' MsgBoxSetForeground 65536           Specifies the message box window as the foreground window.
' MsgBoxRight         524288          Text is right-aligned.
' MsgBoxRtlReading    1048576         Specifies text should appear as right-to-left reading on Hebrew and Arabic systems.

' Dim oMsgStyle1 As MsgBoxStyle _ 
'                = MsgBoxStyle.YesNo Or _ 
'                  MsgBoxStyle.DefaultButton1 Or _ 
'                  MsgBoxStyle.Critical Or _ 
'                  MsgBoxStyle.MsgBoxRtlReading

' ''Call MsgBox with (Message, Settings, Title)
' MsgBox("Continue Running Rules?",
'        oMsgStyle1,
'        "Error")

' ''Results from MsgBox can be evaluated via plaintxt
' MsgBoxResult.Cancel 'Yes No Retry Ok Abort Ignore  


'-------For Simple Debug-------
'-----------Copy Below and Remove 'Single' Comments----------------------
  ' Imports System.Diagnostics


  '  Dim bVerboseDebug As Boolean = True
  '  DebugLine("This Rule Has Fired", bVerboseDebug)

  ' Private Sub DebugLine(ByVal Message As String, Optional ByRef Output As Boolean = True)
  ' ''Insert the following at the start of the program before Sub Main():: Imports System.Diagnostics
  '  If Output Then Trace.WriteLine("[AIP] " & Message)
  ' End Sub
'-----------Copy Above and Remove 'Single' Comments----------------------
