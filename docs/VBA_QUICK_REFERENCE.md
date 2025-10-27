# VBA Quick Reference for SolidWorks

Quick reference guide for common SolidWorks API operations in VBA.

---

## Basic Setup

### Connecting to SolidWorks
```vba
Dim swApp As SldWorks.SldWorks
Set swApp = Application.SldWorks
```

### Getting Active Document
```vba
Dim swModel As SldWorks.ModelDoc2
Set swModel = swApp.ActiveDoc

If swModel Is Nothing Then
    MsgBox "No document open!"
    Exit Sub
End If
```

### Document Types
```vba
Dim docType As Long
docType = swModel.GetType()

Select Case docType
    Case swDocPART
        ' Part document
    Case swDocASSEMBLY
        ' Assembly document
    Case swDocDRAWING
        ' Drawing document
End Select
```

---

## File Operations

### Open Document
```vba
Dim errors As Long, warnings As Long
Dim filePath As String
filePath = "C:\path\to\file.SLDPRT"

Set swModel = swApp.OpenDoc6(filePath, swDocPART, swOpenDocOptions_Silent, "", errors, warnings)
```

### Save Document
```vba
' Regular save
swModel.Save3 swSaveAsOptions_Silent, 0, 0

' Save As
Dim newPath As String
newPath = "C:\path\to\newfile.SLDPRT"
swModel.SaveAs3 newPath, swSaveAsOptions_Silent, swFileSaveAsOptions_Silent
```

### Close Document
```vba
swApp.CloseDoc swModel.GetTitle()
```

---

## Sketch Operations

### Create Sketch
```vba
' Select a plane first
Dim swPart As SldWorks.PartDoc
Set swPart = swModel

' Insert sketch
swModel.InsertSketch2 True
```

### Draw Line
```vba
Dim swSketch As SldWorks.SketchManager
Set swSketch = swModel.SketchManager

' Create line from (0,0) to (100,100) in mm
swSketch.CreateLine 0, 0, 0, 0.1, 0.1, 0
```

### Draw Circle
```vba
' Create circle at (50,50) with radius 25mm
swSketch.CreateCircleByRadius 0.05, 0.05, 0, 0.025
```

### Draw Rectangle
```vba
' Create rectangle
' Corner 1: (10,10), Corner 2: (90,60)
swSketch.CreateCornerRectangle 0.01, 0.01, 0, 0.09, 0.06, 0
```

### Add Dimension
```vba
Dim swDim As SldWorks.DisplayDimension
Set swDim = swModel.AddDimension2(x, y, z)
```

---

## Features

### Extrude Boss/Base
```vba
' Create extrusion
swModel.FeatureManager.FeatureExtrusion2 _
    True, False, False, _                  ' SD, Flip, Dir
    swEndCondBlind, swEndCondBlind, _       ' End conditions
    0.05, 0, _                             ' Depth (50mm), Depth2
    False, False, False, False, _           ' Draft options
    0, 0, _                                ' Draft angles
    False, False, False, False, _           ' Offset options
    0, 0, _                                ' Offset distances
    False, _                               ' Thin feature
    0, 0, 0, _                            ' Thin parameters
    False, False, False                    ' Merge, etc
```

### Cut Extrude
```vba
swModel.FeatureManager.FeatureCut3 _
    True, False, False, _
    swEndCondThroughAll, 0, _
    0.01, 0.01, _
    False, False, False, False, _
    0, 0, _
    False, False, False, False, False, _
    0, 0, 0, 0, _
    False, swStartSketchPlane, 0, _
    False, False
```

### Revolve
```vba
swModel.FeatureManager.FeatureRevolve2 _
    True, True, False, _                   ' Single dir, direction, thin
    False, False, _                        ' Merge, Use auto select
    swEndCondBlind, swEndCondBlind, _      ' End conditions
    6.28318530718, 0, _                   ' Angle (2*PI radians = 360°), Angle2
    False, False, _                        ' Offset options
    0, 0, _                               ' Offsets
    0, 0, 0, _                            ' Thin wall parameters
    0, False, False                        ' Direction, merge, use multi
```

### Hole Wizard
```vba
Dim swHole As SldWorks.WizardHoleFeatureData2
Set swHole = swModel.FeatureManager.HoleWizardManager.CreateWizardHoleFeatureData(swWzdCounterBore)

swHole.Type = swWzdCounterBore
swHole.Size = swStandardIso
swHole.StandardType = swHolStandard
swHole.StandardSize = "M5"
```

---

## Selection

### Select Object
```vba
Dim swSelMgr As SldWorks.SelectionMgr
Set swSelMgr = swModel.SelectionManager

' Select face, edge, or vertex
Dim swSelData As SldWorks.SelectData
Set swSelData = swSelMgr.CreateSelectData

swModel.Extension.SelectByID2 "Face1", "FACE", 0, 0, 0, False, 0, Nothing, 0
```

### Get Selected Objects
```vba
Dim selCount As Long
selCount = swSelMgr.GetSelectedObjectCount2(-1)

Dim i As Long
For i = 1 To selCount
    Dim swSelObj As Object
    Set swSelObj = swSelMgr.GetSelectedObject6(i, -1)
Next i
```

### Clear Selection
```vba
swModel.ClearSelection2 True
```

---

## Materials

### Assign Material
```vba
Dim swPart As SldWorks.PartDoc
Set swPart = swModel

' Assign material from library
swPart.SetMaterialPropertyName2 "Default", "C:\...\SolidWorks Materials\steel.sldmat", "AISI 1020"
```

### Get Material
```vba
Dim matDB As String, matName As String
swPart.GetMaterialPropertyName2 "Default", matDB, matName
```

---

## Mass Properties

### Get Mass Properties
```vba
Dim swMassProp As SldWorks.MassProperty
Set swMassProp = swModel.Extension.CreateMassProperty()

Dim mass As Double
mass = swMassProp.Mass  ' in kg

Dim volume As Double
volume = swMassProp.Volume  ' in m³

Dim com(2) As Double
com = swMassProp.CenterOfMass  ' [x, y, z]
```

---

## Custom Properties

### Add Custom Property
```vba
Dim swCustProp As SldWorks.CustomPropertyManager
Set swCustProp = swModel.Extension.CustomPropertyManager("")

' Add property
swCustProp.Add3 "PartNumber", swCustomInfoText, "12345", swCustomPropertyOnlyIfNew
```

### Get Custom Property
```vba
Dim propValue As String, resolvedValue As String
swCustProp.Get5 "PartNumber", False, propValue, resolvedValue, False
```

### Delete Custom Property
```vba
swCustProp.Delete2 "PartNumber"
```

---

## Assembly Operations

### Get Components
```vba
Dim swAssy As SldWorks.AssemblyDoc
Set swAssy = swModel

Dim vComps As Variant
vComps = swAssy.GetComponents(False)

Dim i As Long
For i = 0 To UBound(vComps)
    Dim swComp As SldWorks.Component2
    Set swComp = vComps(i)
    Debug.Print swComp.Name2
Next i
```

### Add Component
```vba
Dim swComp As SldWorks.Component2
Set swComp = swAssy.AddComponent5( _
    "C:\path\to\part.SLDPRT", _  ' File path
    swAddComponentOptions_Default, _
    "", _                         ' Configuration
    False, _                      ' Use existing
    "", _                         ' BOM options
    0, 0, 0 _                    ' X, Y, Z position
)
```

---

## Utilities

### Rebuild Model
```vba
swModel.ForceRebuild3 False  ' False = current config only
```

### View Zoom to Fit
```vba
swModel.ViewZoomtofit2
```

### Show/Hide Component
```vba
swComp.Visible = swComponentVisible  ' Show
swComp.Visible = swComponentHidden   ' Hide
```

### Error Checking
```vba
On Error GoTo ErrorHandler

' Your code here

Exit Sub

ErrorHandler:
    MsgBox "Error " & Err.Number & ": " & Err.Description, vbCritical
    Exit Sub
```

---

## Common Constants

### Document Types
- `swDocPART` = 1
- `swDocASSEMBLY` = 2
- `swDocDRAWING` = 3

### End Conditions
- `swEndCondBlind` = 0
- `swEndCondThroughAll` = 1
- `swEndCondUpToNext` = 2
- `swEndCondUpToVertex` = 3
- `swEndCondUpToSurface` = 4

### Selection Types
- `"FACE"` - Face
- `"EDGE"` - Edge
- `"VERTEX"` - Vertex
- `"SKETCHSEGMENT"` - Sketch segment
- `"DIMENSION"` - Dimension

---

## Best Practices

1. **Always check for Nothing**: Check if objects exist before using them
2. **Use error handling**: Wrap code in error handlers
3. **Clear selections**: Clear selections when done
4. **Rebuild when needed**: Force rebuild after making changes
5. **Use meaningful names**: Name features for easier reference
6. **Comment your code**: Explain what complex sections do
7. **Test on simple parts**: Test macros on simple geometry first
8. **Save before running**: Always save your work before testing

---

## Debugging Tips

### Immediate Window
```vba
Debug.Print "Variable value: " & myVar
```

### Breakpoints
- Press F9 to set breakpoint
- F5 to run, F8 to step through

### Watch Window
- Add variables to watch window
- Right-click variable ? Add Watch

---

For more information, see the SolidWorks API Help documentation.

