' <IsStraightVb>True</IsStraightVb>
Imports clsDebugViewer
Imports Inventor
Imports System.Windows.Forms


'''_______________ Child Sub Reqs Start Here _______________
  'AddVbFile "VB - clsPart.vb"
  'AddVbFile "VB - clsDebugViewer.vb"

  'Sub Main()' Get Application and Transient Functions
    ' oApp = ThisApplication
    ' oTG = oApp.TransientGeometry
    ' oTO = oApp.TransientObjects
    ' oCM = oApp.CommandManager
    ' Dim prt As New clsPart
    '''Use 'prt.' as a prefix for any method called from "VB - clsPart.vb"
  'End Sub
'''_______________ Child Sub Reqs End Here _______________


Public Class clsPart
''' Contains Test Methods for Class
''' Public Functions:: 
'''     
''' Valid File is a little ... robust, look at trimming down and splitting into different methods.
''' Public Subs::
'''     HoleFeatureLinearPlacement()

#Region "Declarations"
'--------------------------------------------------------------------------
''Class Public Variable Declarations
 Private Shared Dim bVerboseDebug_clsPart As Boolean = True    
 Public Shared Dim oApp As Inventor.Application
    Public Shared Dim oTG As TransientGeometry
    Public Shared Dim oTO As TransientObjects
    Public Shared Dim oCM As CommandManager

'--------------------------------------------------------------------------
#End Region


Sub Main()
 '''Run this sub for testing Class Connection and Sub Functions
      ' Get Application and Transient Functions
      oApp = ThisApplication
      oTG = oApp.TransientGeometry
      oTO = oApp.TransientObjects
      oCM = oApp.CommandManager
  Try

' if in an Assembly then look for part type 6xAB or 6x_ for row information
'     ''get point info from?? Circle Ctr? or 2d Sketch Points? or 3d Points?
'     StartPoint = GetPoint()
'     EndPoint = GetPoint()

'     if no row info found then
'        Get Vert shift from StartPoint
'        Get Vert shift from EndPoint
'        Pick Vertical Axis for Alignment
'        Pick MidPlane

'''         Partition Plate never has holes any rotation
'''  StartPt.Row                   Partition Plate : Start Below Row #                       PP:1:SRow
'''  StartPt.Hole                  Partition Plate : Start Hole #                            PP:1:SHole
'''  EndPt.Row                     Partition Plate : Finish Below Row #                      PP:1:FRow
'''  EndPt.Hole                    Partition Plate : Finish Hole #                           PP:1:FHole
'''  Plt.Thk                       Partition Plate : Thickness                               PP:1:Thk
'''  Plt Width                     Partition Plate : Width (X per API.661)                   PP:1:X
'''  (Horz)ShiftDwn                Partition Plate : Y Shift Down from Tube CL               PP:1:YS
'''  StartPt.X-.Shift              Partition Plate : Z Shift Left of Start Hole              PP:1:SZS
'''  EndPt.X-.Shift                Partition Plate : Z Shift Left of Finish Hole             PP:1:FZS

'''          Stiffener Is Always Horizontal
'''  Y.Start                       Stiffener Plate : Below Row #                            SP:10:RN
'''  Y.ShiftDwn                    Stiffener Plate : Y Shift Down from Tube CL              SP:10:YS
'''  X.ShiftFromWet                Stiffener Plate : Z Shift Away from End Pl Wet           SP:10:ZS
'''  Plt.Thk                       Stiffener Plate : Thickness                              SP:10:Thk
'''  Plt.Wd                        Stiffener Plate : Width (X per API.661)                  SP:10:X
'''  Cut.Wd                        Stiffener Plate : Window Width                           SP:10:W:X
'''  Cut.LG                        Stiffener Plate : Window Length                          SP:10:W:Z
'''  Cut.Qty1                      Stiffener Plate : Window Qty Before Var                  SP:10:W:QTY1
'''  Cut.Space                     Stiffener Plate : Window Pitch Before Var                SP:10:W:Pitch1
'''  Cut.0toX1                     Stiffener Plate : Window Loc to Plate Left               SP:10:W:LocX1
'''  Cut2.Qty                      Stiffener Plate : Window Qty After Var                   SP:10:W:QTY2
'''  Cut2.Space                    Stiffener Plate : Window Pitch After Var                 SP:10:W:Pitch2
'''  Cut2.0toX1                    Stiffener Plate : Window After Var Loc to Plate Left     SP:10:W:LocX2
' Use a 'block' for hole on stiffener, this allows one set of dims without cluttering the system
' only the 0-X1 dims will be required at that point


'     end if


' else exit   

  Catch ex As Exception

  End Try


End Sub


Public Sub HoleFeatureLinearPlacement(ByRef oDoc As Document)
    ' Create a new part document, using the default part template.
    Dim oPartDoc As PartDocument _ 
          = oDoc
          '= oApp.Documents.Add(kPartDocumentObject, _
          '                                oApp.FileManager.GetTemplateFile(kPartDocumentObject))

    ' Set a reference to the component definition.
    Dim oCompDef As PartComponentDefinition _ 
         = oPartDoc.ComponentDefinition

    ' Create a new sketch on the X-Y work plane.
    Dim oSketch As PlanarSketch
     oSketch = oCompDef.Sketches.Add(oCompDef.WorkPlanes.Item(3))

    ' Set a reference to the transient geometry object.
    Dim oTransGeom As TransientGeometry
     oTransGeom = oApp.TransientGeometry

    ' Create a square on the sketch.
    Call oSketch.SketchLines.AddAsTwoPointRectangle( _
                                        oTransGeom.CreatePoint2d(0, 0), _
                                        oTransGeom.CreatePoint2d(6, 6))

    ' Create the profile.
    Dim oProfile As Profile
     oProfile = oSketch.Profiles.AddForSolid

    ' Create an extrusion.
    Dim oExtrudeDef As ExtrudeDefinition
     oExtrudeDef = oCompDef.Features.ExtrudeFeatures.CreateExtrudeDefinition(oProfile, kJoinOperation)
    Call oExtrudeDef.SetDistanceExtent("2 cm", kNegativeExtentDirection)
    Dim oExtrude As ExtrudeFeature
     oExtrude = oCompDef.Features.ExtrudeFeatures.Add(oExtrudeDef)

    ' Get the start face of the extrude.
    Dim oFace As Face
     oFace = oExtrude.StartFaces(1)
    
    ' Get two adjacent edges on the start face.
    Dim oEdge1, oEdge2 As Edge
     oEdge1 = oFace.Edges(1)
     oEdge2 = oFace.Edges(2)
    
    ' Create a bias point for hole placement to place it at
    ' the expected location. This is the model point
    ' corresponding to the center of the square in the sketch.
    Dim oBiasPoint As Point
     oBiasPoint = oSketch.SketchToModelSpace(oTransGeom.CreatePoint2d(1.5, 1.5))
    
    ' Create the hole feature placement definition.
    Dim oLinearPlacementDef As LinearHolePlacementDefinition
     oLinearPlacementDef = oCompDef.Features.HoleFeatures.CreateLinearPlacementDefinition _
    (oFace, oEdge1, "2 cm", oEdge2, "2 cm", oBiasPoint)
    
    ' Create the hole feature.
    Call oCompDef.Features.HoleFeatures.AddDrilledByThroughAllExtent( _
                            oLinearPlacementDef, "1 cm", kPositiveExtentDirection)
End Sub


Public Sub LinkParametersfromDerivedPart(ByRef oPartDoc As Document, 
                                         ByRef listParamNames As List(Of String))
''https://forums.autodesk.com/t5/inventor-forum/ilogic-to-update-linked-parameters/td-p/5580883
    ' If ThisDoc.Document.DocumentType <> DocumentTypeEnum.kPartDocumentObject Then
    '     MessageBox.Show("Make a Part Document the active document", _
    '         "Add Derived Parameter", MessageBoxButtons.OK, MessageBoxIcon.Information)
    '     Exit Sub
    ' End If
    ' Dim oPartDoc As PartDocument = ThisDoc.Document


    
    If oPartDoc.ComponentDefinition.ReferenceComponents.DerivedPartComponents.Count = 0 Then
        MessageBox.Show("No Derived Part Components in this part",
                        "Add Derived Parameter",
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Information)
        Exit Sub
    End If

    Dim oDerPartComp As DerivedPartComponent _
         = oPartDoc.ComponentDefinition.ReferenceComponents.DerivedPartComponents(1)

    'list of parameters to be derived from the MasterPart
    Dim NamesList() As String = {"A1","A2","A3","A4","A5", "B1", "B2"}  
    
    For Each Name As String in NamesList
        AddDerivedParameter(oDerPartComp, Name)
    Next 
    oPartDoc.Update

End Sub


Sub SelectivelyLinkParams(ByRef oSourceDoc As Document, ByRef oPartDoc As Document, ByVal listParamNames As List(Of String))
'''Link list of Named Parameters to 
    ''' Open the source document invisible.
    ' Dim oSourceDoc As PartDocument
    ' Set oSourceDoc = oApp.Documents.Open("C:\temp\block.ipt", False)
    
    ' Set a reference to the component definition.
    Dim oSourceCompDef As PartComponentDefinition _ 
         = oSourceDoc.ComponentDefinition
    
    Dim oParamsToLink As ObjectCollection _ 
        = oApp.TransientObjects.CreateObjectCollection
    
    ' Add parameters named "d0" and "d1".
    ' This assumes that the source document contains
    ' parameters with these names.
    For Each sParam As String In listParamNames
      oParamsToLink.Add(oSourceCompDef.Parameters.Item(sParam))
    Next
    ' oParamsToLink.Add oSourceCompDef.Parameters.Item("d0")
    ' oParamsToLink.Add oSourceCompDef.Parameters.Item("d1")
    
    ''' Create a new part document, using the default part template.
    ' Dim oPartDoc As PartDocument
    ' Set oPartDoc = oApp.Documents.Add(kPartDocumentObject)
   
    ' Create a derived parameter table that links only to "d0"
    ' and "d1" in the source part.
    ' Note: If parameters "d0" and "d1" in the source part
    ' are not already exported, they will be automatically
    ' exported and hence will result in changing the source part.
    Dim oDerivedParamTable As DerivedParameterTable _ 
         = oPartDoc.ComponentDefinition.Parameters. _
                    DerivedParameterTables.Add2("C:\temp\block.ipt", oParamsToLink)
    
    ' Add parameter named "d2"
    ' This assumes that the source document
    ' contains a parameters named "d2".
    oParamsToLink.Add( oSourceCompDef.Parameters.Item("d2"))
    
    ' Change derived parameter table so it also links to "d2".
    oDerivedParamTable.LinkedParameters = oParamsToLink
End Sub

Private Sub AddDerivedParameter(ByRef oDerPartComp As DerivedPartComponent,
                               ByRef Name As String)
''Add Parameter from Derived Part by name
    Dim oDerivedPartDef As DerivedPartUniformScaleDef = oDerPartComp.Definition
   
    For Each oEntity As DerivedPartEntity In oDerivedPartDef.Parameters
        If (oEntity.ReferencedEntity.Name = Name) Then
            oEntity.IncludeEntity = True
            Exit For
        End If
    Next
    'Set Definition back, so DerivedPart Document is updated
    oDerPartComp.Definition = oDerivedPartDef
End Sub

Public Sub CreateParametersAndGroup(oPartDoc As PartDocument)

    ' Create a new Part document.
'    Dim oPartDoc As PartDocument
'     oPartDoc = oApp.Documents.Add(kPartDocumentObject, _
'                 oApp.FileManager.GetTemplateFile(kPartDocumentObject))
    
    ' Set a reference to the compdef.
    Dim oCompDef As PartComponentDefinition
     oCompDef = oPartDoc.ComponentDefinition

    ' Create a model parameter
    Dim oModelParam As ModelParameter
     oModelParam = oCompDef.Parameters.ModelParameters.AddByValue(2, UnitsTypeEnum.kCentimeterLengthUnits)
    
    ' Create a reference parameter
    Dim oReferenceParam As ReferenceParameter
     oReferenceParam = oCompDef.Parameters.ReferenceParameters.AddByValue(4, UnitsTypeEnum.kCentimeterLengthUnits)
    
    ' Create a user parameter
    Dim oUserParam As UserParameter
     oUserParam = oCompDef.Parameters.UserParameters.AddByValue("length", 6, UnitsTypeEnum.kCentimeterLengthUnits)
    
    ' Create a new custom parameter group
    Dim oCustomParamGroup As CustomParameterGroup
     oCustomParamGroup = oCompDef.Parameters.CustomParameterGroups.Add("Custom Group", "CustomGroup1")
    
    ' Add the created parameters to this group
    ' Note that adding the parameters to the custom group
    ' does not remove it from the original group.
    Call oCustomParamGroup.Add(oModelParam)
    Call oCustomParamGroup.Add(oReferenceParam)
    Call oCustomParamGroup.Add(oUserParam)
End Sub


Public Sub AddOccurrencesToFolder(ByRef oDoc As Document)
    ' Dim oDoc As AssemblyDocument
    ' Set oDoc = oApp.ActiveDocument

    Dim oDef As AssemblyComponentDefinition _ 
         = oDoc.ComponentDefinition

    Dim oPane As BrowserPane _ 
         = oDoc.BrowserPanes.ActivePane

    Dim oOccurrenceNodes As ObjectCollection _ 
         = oTO.CreateObjectCollection

    ' Dim oOcc As ComponentOccurrence
    For Each oOcc As ComponentOccurrence In oDef.Occurrences

        Dim oNode As BrowserNode _ 
             = oPane.GetBrowserNodeFromObject(oOcc)

        oOccurrenceNodes.Add( oNode)
    Next

    Dim oFolder As BrowserFolder _ 
         = oPane.AddBrowserFolder("My Occurrence Folder", oOccurrenceNodes)
End Sub

    ''' <summary>
    ''' This method requires user interaction to select a feature. 
    ''' </summary>
    ''' <param name="oFilter">Inventor.SelectionFilterEnum</param>
    ''' <returns>Selected Filtered Object</returns>
    ''' <remarks>There is no error checking for escape characters</remarks>
    Public Shared Function GetSingleSelection(oFilter As Object)
        ' Get a feature selection from the user
        Dim oObject As Object 
        oObject  = oCM.Pick(oFilter, "Pick a feature")
        'SelectionFilterEnum.
          ' kAllCircularEntities                    18435 All circular entities filter. 
          ' kAllCustomGraphicsFilter                18436 All custom graphics. 
          ' kAllEntitiesFilter                      18439 All entities filter(all entities should be selectable). 
          ' kAllLinearEntities                      18433 All linear entities filter. 
          ' kAllPlanarEntities                      18432 All planar entities filter. 
          ' kAllPointEntities                       18434 All point entities filter. 
          ' kAssemblyFeatureFilter                  16646 Assembly feature filter. 
          ' kAssemblyLeafOccurrenceFilter           16643 Assembly leaf occurrence filter. 
          ' kAssemblyOccurrenceFilter               16640 Assembly occurrence filter. 
          ' kAssemblyOccurrencePatternElementFilter 16645 Assembly occurrence pattern element filter. 
          ' kAssemblyOccurrencePatternFilter        16644 Assembly occurrence pattern filter. 
          ' kCustomBrowserNodeFilter                18437 Custom browser node. 
          ' kDrawingAutoCADBlockDefinitionFilter    16923 Drawing Block Definition filter. 
          ' kDrawingAutoCADBlockFilter              16922 Drawing Block Reference filter. 
          ' kDrawingBalloonFilter                   16906 Drawing balloon filter. 
          ' kDrawingBorderDefinitionFilter          16909 Drawing border definition filter. 
          ' kDrawingBorderFilter                    16913 Drawing Border instance filter. 
          ' kDrawingCenterlineFilter                16915 Drawing Centerline filter. 
          ' kDrawingCentermarkFilter                16916 Drawing Centermark filter. 
          ' kDrawingCurveSegmentFilter              16914 Drawing curve segment filter. 
          ' kDrawingCustomTableFilter               16905 Drawing custom table filter. 
          ' kDrawingDefaultFilter                   16896 Drawing default filter. 
          ' kDrawingDimensionFilter                 16900 Drawing dimension filter. 
          ' kDrawingFeatureControlFrameFilter       16918 Drawing feature control frame filter. 
          ' kDrawingHoleTableFilter                 16902 Drawing hole table filter. 
          ' kDrawingHoleTagFilter                   16903 Drawing hole tag filter. 
          ' kDrawingNoteFilter                      16899 Drawing note filter. 
          ' kDrawingOriginIndicatorFilter           16920 Drawing OriginIndicator filter. 
          ' kDrawingPartsListFilter                 16901 Drawing parts list filter. 
          ' kDrawingRevisionTableFilter             16904 Drawing revision table filter. 
          ' kDrawingSheetFilter                     16897 Drawing sheet filter. 
          ' kDrawingSheetFormatFilter               16917 Drawing sheet format filter. 
          ' kDrawingSketchedSymbolDefinitionFilter  16908 Drawing sketched symbol definition filter. 
          ' kDrawingSketchedSymbolFilter            16907 Drawing sketched symbol filter. 
          ' kDrawingSurfaceTextureSymbolFilter      16919 Drawing surfacetexture symbol filter. 
          ' kDrawingTitleBlockDefinitionFilter      16910 Drawing title block definition filter. 
          ' kDrawingTitleBlockFilter                16912 Drawing title block instance filter. 
          ' kDrawingViewFilter                      16898 Drawing view filter. 
          ' kDrawingViewLabelFilter                 16921 Drawing surfacetexture symbol filter. 
          ' kFeatureDimensionFilter                 18438 Feature dimension filter. 
          ' kModelAnnotationFilter                  16388 Model annotation filter. 
          ' kPartBodyFilter                         15890 Part body filter. 
          ' kPartDefaultFilter                      15886 Part default filter. 
          ' kPartEdgeCircularFilter                 15874 Part edge circular filter. 
          ' kPartEdgeFilter                         15873 Part edge filter. 
          ' kPartEdgeLinearFilter                   15875 Part edge linear filter. 
          ' kPartEdgeMidpointFilter                 15876 Part edge midpoint filter. 
          ' kPartFaceConicalFilter                  15880 Part face conical filter. 
          ' kPartFaceCylindricalFilter              15879 Part face cylindrical filter. 
          ' kPartFaceFilter                         15877 Part face filter. 
          ' kPartFacePlanarFilter                   15878 Part face planar filter. 
          ' kPartFaceSphericalFilter                15882 Part face spherical filter. 
          ' kPartFaceToroidalFilter                 15881 Part face toroidal filter. 
          ' kPartFeatureFilter                      15884 Part feature filter. 
          ' kPartMeshEdgeCircularFilter             15903 Part mesh circular edge filter. 
          ' kPartMeshEdgeFilter                     15897 Mesh Edge filter. 
          ' kPartMeshEdgeLinearFilter               15904 Part mesh linear edge filter. 
          ' kPartMeshFaceConicalFilter              15900 Part mesh conical face filter. 
          ' kPartMeshFaceCylindricalFilter          15899 Part mesh cylindrical face filter. 
          ' kPartMeshFaceFilter                     15896 Mesh Face filter. 
          ' kPartMeshFacePlanarFilter               15901 Part mesh planar face filter. 
          ' kPartMeshFaceSphericalFilter            15902 Part mesh spherical face filter. 
          ' kPartMeshFeatureFilter                  15894 Mesh feature filter. 
          ' kPartMeshFeatureSetFilter               15895 Mesh feature set filter. 
          ' kPartMeshVertexFilter                   15898 Mesh Vertex filter. 
          ' kPartSurfaceFeatureFilter               15885 Part surface feature filter. 
          ' kPartVertexFilter                       15883 Part vertext filter. 
          ' kPointCloudFilter                       15891 Point cloud filter. 
          ' kPointCloudPlaneFilter                  15893 Point cloud plane filter. 
          ' kPointCloudPointFilter                  15892 Point cloud point filter. 
          ' kPublicationComponentFilter             17153 Publication component filter. 
          ' kPublicationEdgeFilter                  17156 Publication Edge filter. 
          ' kPublicationFaceFilter                  17155 Publication face filter. 
          ' kPublicationLeafComponentFilter         17154 Publication leaf component filter. 
          ' kPublicationTrailNodeFilter             17159 Publication trail node filter. 
          ' kPublicationTrailSegmentFilter          17160 Publication trail segment filter. 
          ' kPublicationTweakPathFilter             17158 Publication tweakpath filter. 
          ' kPublicationVertexFilter                17157 Publication Vertex filter. 
          ' kSketch3DCurveCircularFilter            17666 3D Sketch curve circular filter. 
          ' kSketch3DCurveEllipseFilter             17667 3D Sketch curve ellipse filter. 
          ' kSketch3DCurveFilter                    17664 3D Sketch curve filter. 
          ' kSketch3DCurveLinearFilter              17665 3D Sketch curve linear filter. 
          ' kSketch3DCurveSplineFilter              17668 3D Sketch curve spline filter. 
          ' kSketch3DDefaultFilter                  17670 3D Sketch default filter. 
          ' kSketch3DDimConstraintFilter            17672 3D Sketch dimension constraint filter. 
          ' kSketch3DObjectFilter                   17671 3D Sketch filter. 
          ' kSketch3DPointFilter                    17669 3D Sketch point filter. 
          ' kSketch3DProfileFilter                  17673 3D Sketch profile filter. 
          ' kSketchBlockDefinitionFilter            16141 SketchBlock definition filter. 
          ' kSketchBlockFilter                      16142 SketchBlock filter. 
          ' kSketchCurveCircularFilter              16131 Sketch curve circular filter. 
          ' kSketchCurveEllipseFilter               16132 Sketch curve ellipse filter. 
          ' kSketchCurveFilter                      16129 Sketch curve filter. 
          ' kSketchCurveLinearFilter                16130 Sketch curve linear filter. 
          ' kSketchCurveSplineFilter                16133 Sketch curve spline filter. 
          ' kSketchDefaultFilter                    16135 Sketch default filter. 
          ' kSketchDimConstraintFilter              16128 Sketch dimension constraint filter. 
          ' kSketchImageFilter                      16137 SketchImage filter. 
          ' kSketchObjectFilter                     16136 Sketch filter. 
          ' kSketchPointFilter                      16134 Sketch point filter. 
          ' kSketchProfileFilter                    16139 2D Profile filter. 
          ' kSketchProjectedCutFilter               16140 ProjectedCut filter. 
          ' kSketchTextBoxFilter                    16138 Sketch TextBox filter. 
          ' kUserCoordinateSystemFilter             16387 UserCoordinateSystem Filter. 
          ' kWorkAxisFilter                         16384 Work axis filter. 
          ' kWorkPlaneFilter                        16385 Work plane filter. 
          ' kWorkPointFilter                        16386 Work point filter. 


        Echo( "Picked: " & oObject.Type)
        Echo( "        " & [Enum].GetName(GetType(ObjectTypeEnum), oObject.Type))
        Return oObject
    End Function

    Public Sub CheckSelection(Optional sPrompt As String = "Pick an object")
        ' Get a feature selection from the user
        Dim oObject As Object 
        oObject	= oCM.Pick(SelectionFilterEnum.kAllEntities, sPrompt)

        Echo( "Picked: " & oObject.Type)
        Echo( "        " & [Enum].GetName(GetType(ObjectTypeEnum) & ":" & oObject.Type))

        MsgBox(("Selected {0} :# {1}",[Enum].GetName(GetType(ObjectTypeEnum), oObject.Type))

    End Sub


Public Sub GetFeatureInfo()
    ' Get the active document assuming it is a part.
    Dim partDoc As PartDocument
    partDoc = oApp.ActiveDocument

    ' Get the component definition.  This owns the part specific info for the part.
    Dim partDef As PartComponentDefinition
    partDef = partDoc.ComponentDefinition
    
    ' Iterate over the extrude features.
    Dim extrude As ExtrudeFeature
    For Each extrude In partDef.Features.ExtrudeFeatures
        Debug.Print("[AIP] iLogic:" & extrude.Name)
        
        ' Get the definition object from the feature.
        Dim extrudeDef As ExtrudeDefinition
        extrudeDef = extrude.Definition
        
        ' Display information in the definition object.
        Select Case extrudeDef.ExtentType
            Case kDistanceExtent
                Dim distance As DistanceExtent
                distance = extrudeDef.Extent

                Debug.Print("[AIP] iLogic:" & "Distance")
               ' Call DisplayToleranceInfo(distance.distance)
            Case kFromToExtent
                Dim fromTo As FromToExtent
                fromTo = extrudeDef.Extent

                Debug.Print("[AIP] iLogic:" & "FromTo extent between to faces and/or work features.")
            Case kThroughAllExtent
                Dim throughAll As ThroughAllExtent
                throughAll = extrudeDef.Extent

                Debug.Print("[AIP] iLogic:" & "Through all extent.")
            Case kToExtent
                Dim toExt As ToExtent
                toExt = extrudeDef.Extent

                Debug.Print("[AIP] iLogic:" & "To a face or work plane extent.")
            Case kToNextExtent
                Dim toNext As ToNextExtent
                toNext = extrudeDef.Extent

                Debug.Print("[AIP] iLogic:" & "To next extent.")
            Case Else
                Debug.Print("[AIP] iLogic:" & "Unhandled case: " & extrudeDef.ExtentType)
        End Select
    Next
End Sub


    ''' <summary>
    ''' This Class Collection is for Inventor Sketches for easy use & manipluation
    ''' </summary>
    ''' <remarks></remarks>
  Public Class hphSketch
      Public Property Name As String
      Public Property PartDoc As PartDocument
      Public Property WkPlane As WorkPlane
      Public Property Sk As PlanarSketch
      Public Property UOM As UnitsOfMeasure
      Public Property CompDef As PartComponentDefinition
      Public Property CtrPoint As SketchPoint
      Public Property SkLines As New SortedList(Of String, SketchLine)
      Public Property Dims As New SortedList(Of String, DimensionConstraint)
      Public Property DimList As New List(Of String)
      Public Property Parameters As New SortedList(Of String, Parameter)


    ''' <summary>
    ''' This method is a constructor for the hphSketch Object 
    ''' <example>
    ''' For example:
    ''' <code>
    '''     Dim oDoc As PartDocument = ThisDoc.Document
    '''     Dim oWkPlane As Workplane = oDoc.WorkPlanes.Item(3)
    '''     Dim mySketch As hphSketch
    '''     mySketch = New hphSketch(oDoc, oWkPlane)
    ''' </code>
    ''' results in a new planar sketch being created.
    ''' </example>
    ''' </summary>
    ''' <param name="oPartDoc">Part Document</param>
    ''' <param name="oWPlane">WorkPlaene from Part Definition</param>
    ''' <param name="sNew">String to Replace With</param>
    ''' <returns></returns>
    ''' <remarks>This is already a function that is shown for reference</remarks>
      Public Sub New(ByRef oPartDoc As PartDocument, ByRef oWPlane As WorkPlane)
          If oPartDoc Is Nothing Then 
            PartDoc = oApp.Documents.Add(DocumentTypeEnum.kPartDocumentObject,
                        oApp.FileManager.GetTemplateFile(DocumentTypeEnum.kPartDocumentObject),
                        True)
          Else
            PartDoc = oPartDoc
          End If
            WkPlane = oWPlane
            CompDef = PartDoc.ComponentDefinition

            'Get Units of Measure reference
            UOM = PartDoc.UnitsOfMeasure

          ' Create a new sketch on the X-Y work plane.
          Sk = CompDef.Sketches.Add(WkPlane)
          Echo("Planar Sketch Created", 5, bVerboseDebug)

                    ' Set Origin to Center (Redundant)
          Sk.OriginPoint = CompDef.WorkPoints.Item(1)

          ' Project Center Point to Sketch
          CtrPoint = Sk.AddByProjectingEntity(CompDef.WorkPoints.Item(1))

            Echo("New hphSketch has been created",5,bVerboseDebug)
      End Sub


      Public Sub OffsetLineFromCenter(ByRef oLine As SketchLine,
                                      ByVal dOffset As Double, _ ' value in database units, cm '' Offset a Line on the sketch away from ctr point
                                      ByVal sParamName() As String, _       'd:(0):(1) = vb:(0):(2) , d:(0):(3) = vb:(0):(4) , [d:(0):(5) = vb:(0):(6)]
                             Optional ByVal bHalfSet As Boolean = False)     'Where: { 0:Header, 1:Length, 2:EqualsName, 3:Offset, 4:Equals Name, [5:Final Length, 6:Equals Name] }
      ''Create an Offset Line that is Constrained to center with option of adding a half distance line
        Echo("Offsetting a Line by " & dOffset, 6, bVerboseDebug)

          Dim oCollection As ObjectCollection
          Dim oNewLine As SketchLine
          Dim bHorz As Boolean
          Dim iX, iY As Double
          Dim iX1, iY1 As Double
          Dim iX2, iY2 As Double
          Dim ptOffsetPoint, ptHalfOffsetPoint As Point2d
          Dim ptDimLGOffset, ptDimTHKOffset As Point2d

        Try
          oCollection = oTO.CreateObjectCollection
          oCollection.Add(oLine)

          iX1 = oLine.StartSketchPoint.Geometry.X : iY1 = oLine.StartSketchPoint.Geometry.Y
          iX2 = oLine.EndSketchPoint.Geometry.X : iY2 = oLine.EndSketchPoint.Geometry.Y

          If iX1 = iX2 Then 'Assume the Line is Vertical
            iX = ( Math.Abs( iX1 ) + Math.Abs( dOffset ) ) * Math.Sign( iX1 )
            iY = (oLine.StartSketchPoint.Geometry.Y + oLine.EndSketchPoint.Geometry.Y ) / 2

            bHorz = False

            ptDimLength = oTG.CreatePoint2d( _ 
                                            ( iX + ( Math.Abs( dOffset / 2 ) * Math.Sign( iX1 ) ) ) ,
                                            iY _ '( (Math.Abs( iY ) + ( Math.Abs( dOffset / 2 )) * Math.Sign( iY1 ) ) )
                                            )

            ptDimTHK = oTG.CreatePoint2d( _ 
                                         ( iX - ( Math.Abs( dOffset / 4 ) * Math.Sign( iX1 ) ) ) ,
                                         ( (Math.Abs( iY ) + ( Math.Abs( dOffset / 4 )) * Math.Sign( iY1 ) ) )
                                         )

            ptDimLGOffset = oTG.CreatePoint2d( _ 
                                              ( (Math.Abs( iX1 ) + Math.Abs( dOffset / 4 )) * Math.Sign( iX1 ) ) ,
                                              ( (Math.Abs( iY ) + Math.Abs( dOffset / 2 )) * Math.Sign( iY1 ) )
                                              )

            ptHalfOffsetPoint = oTG.CreatePoint2d( _ 
                                                  ( iX1 + ( Math.Abs( dOffset / 2 ) * Math.Sign( iX1 ) ) ),
                                                  iY
                                                  )


           ElseIf iY1 = iY2 ' Assume the Line is Horizontal
            iX = (oLine.StartSketchPoint.Geometry.X + oLine.EndSketchPoint.Geometry.X ) / 2
            iY = ( Math.Abs( iY1 ) + Math.Abs( dOffset ) ) * Math.Sign( iY1 )

            bHorz = True

            ptDimLength = oTG.CreatePoint2d( _ 
                                            iX ,
                                            ( iY + Math.Abs( dOffset / 2 ) * Math.Sign( iY1 ) ) _ '( (Math.Abs( iY ) + ( Math.Abs( dOffset / 2 )) * Math.Sign( iY1 ) ) )
                                            )

            ptDimTHK = oTG.CreatePoint2d( _ 
                                          Math.Abs( iX ) + ( Math.Abs( dOffset / 4 ) * Math.Sign( iX1 ) ) , _ '( (Math.Abs( iX ) + Math.Abs( dOffset / 4 )) * Math.Sign( iX1 ) ) ,
                                         ( iY - ( Math.Abs( dOffset / 4 ) * Math.Sign( iY1 ) ) )
                                         )

            ptDimLGOffset = oTG.CreatePoint2d( _ 
                                              ( (Math.Abs( iX ) + Math.Abs( dOffset / 2 )) * Math.Sign( iX1 ) ) ,
                                              ( (Math.Abs( iY1 ) + Math.Abs( dOffset / 4 )) * Math.Sign( iY1 ) )
                                              )

            ptHalfOffsetPoint = oTG.CreatePoint2d( _ 
                                                  iX ,
                                                  ( iY1 + ( Math.Abs( dOffset / 2 ) * Math.Sign( iY1 ) ) )
                                                  )


           Else
            Echo("Offset Line Error, Line is Not Horizontal or Vertical :: " & _ 
                "1(" & iX1 & "x," & iY1 & "y) : 2(" & iX2 & "x," & iY2 & "y)" )

          End If 'Horizontal vs Vertical

          ptOffsetPoint = oTG.CreatePoint2d(iX, iY)

          Echo("New X,Y is " & iX & "," & iY & " from: " & _ 
               "1(" & iX1 & "x," & iY1 & "y) : 2(" & iX2 & "x," & iY2 & "y)" )

          Dim oOffsetLines As SketchEntitiesEnumerator _ 
                           = Sk.OffsetSketchEntitiesUsingPoint(oCollection, ptOffsetPoint, False)
          'Sketch.OffsetSketchEntitiesUsingPoint( SketchEntities As ObjectCollection, OffsetPoint As Point2d, [IncludeConnectedEntities] As Boolean, [CreateOffsetConstraints] As Boolean ) As SketchEntitiesEnumerator

          oNewLine = oOffsetLines.Item(1)

         Catch ex As Exception
          Echo("Error with LineOffset ::" & ex.Message)
        End Try 'OffsetLine Creation

          'Add midpoint for constraint/alignment to ctr point
          Dim newMidPoint As SketchPoint
          newMidPoint = Sk.SketchPoints.Add(ptOffsetPoint, False)

            With Sk.GeometricConstraints
              .AddMidpoint(newMidPoint, oNewLine)

              If Not bHorz Then .AddHorizontalAlign(CtrPoint, newMidPoint)
              If bHorz Then .AddVerticalAlign(CtrPoint, newMidPoint)

            End With
              Echo("Sketch Geometric Constraints have been made", bVerboseDebug)


          '' Create Dimensions for Length and Offset(thickness) of Line
          Dim oDimLength, oDimThk As DimensionConstraint
          ' Create a dimension between the end points of the line.
          oDimLength = Sk.DimensionConstraints.AddTwoPointDistance( _
                                            oNewLine.StartSketchPoint,
                                            oNewLine.EndSketchPoint,
                                            DimensionOrientationEnum.kAlignedDim,
                                            ptDimLength,
                                            False)
          oDimThk = Sk.DimensionConstraints.AddOffset( _
                                            oNewLine,
                                            oLine,
                                            ptDimTHK,
                                            False,)
              Echo("Sketch Dimensional Constraints have been made", bVerboseDebug)


            Call SetDim(oDimLength,
                        "dH" & sParamName(0) & ":" & sParamName(1),
                        "vb:" & sParamName(0) & ":" & sParamName(2) )

            Call SetDim(oDimThk,
                        "dH" & sParamName(0) & ":" & sParamName(3),
                        "vb:" & sParamName(0) & ":" & sParamName(4) )
              Echo("Sketch Dimensions for Length & Offset are Complete", bVerboseDebug)



          If bHalfSet 'Create a half line that is new length and half the offset.
            Echo("Starting Overlay Line", 15, bVerboseDebug)
            Dim oLineFinalLength As SketchLine
            Dim oDimFinalLength As DimensionConstraint
            Dim newMidPoint2 As SketchPoint
            newMidPoint2 = Sk.SketchPoints.Add(ptHalfOffsetPoint, False)

            Try
              Dim oLineFinalLengths As SketchEntitiesEnumerator
              oLineFinalLengths = Sk.OffsetSketchEntitiesUsingPoint(oCollection, ptHalfOffsetPoint, False)
              'Sketch.OffsetSketchEntitiesUsingPoint( SketchEntities As ObjectCollection, OffsetPoint As Point2d, [IncludeConnectedEntities] As Boolean, [CreateOffsetConstraints] As Boolean ) As SketchEntitiesEnumerator
              Echo("Overlay Line Created", 15, bVerboseDebug)

              oLineFinalLength = oLineFinalLengths.Item(1)

             Catch ex As Exception
              Echo("Error with Overlay LineOffset ::" & ex.Message)
            End Try 'OffsetLine Creation

            ' Create a dimension between the end points of the line.
            oDimFinalLength = Sk.DimensionConstraints.AddTwoPointDistance( _
                                              oLineFinalLength.StartSketchPoint,
                                              oLineFinalLength.EndSketchPoint,
                                              DimensionOrientationEnum.kAlignedDim,
                                              ptDimLGOffset,
                                              False)
             Echo("DimensionConstraints for Overlay created", bVerboseDebug)

            Sk.GeometricConstraints.AddSymmetry( oLine , oNewLine , oLineFinalLength )
            'Function AddSymmetry(EntityOne As SketchEntity, EntityTwo As SketchEntity, SymmetryAxis As SketchLine) As SymmetryConstraint
             Echo("GeometricConstraints for Overlay created", bVerboseDebug)

            Call SetDim(oDimFinalLength,
                        "dH" & sParamName(0) & ":" & sParamName(5),
                        "vb:" & sParamName(0) & ":" & sParamName(6) )

            With Sk.GeometricConstraints
              .AddMidpoint(newMidPoint2, oLineFinalLength)

              If Not bHorz Then .AddHorizontalAlign(CtrPoint, newMidPoint2)
              If bHorz Then .AddVerticalAlign(CtrPoint, newMidPoint2)

            End With
             Echo("GeometricConstraints for Overlay created")


          End If

         DebugLine("OffsetLineFromCenter", 2, bVerboseDebug)
      End Sub


      Public Sub SetDim(ByRef oDim As DimensionConstraint,
                        ByVal sDimName As String,
               Optional ByVal sDimExpr As String = Nothing)
      ''Set Dimension Name and Expression


            Try
              Echo("Set " & oDim.Parameter.Name & "'s name to " & sDimName, 8, bVerboseDebug)
              oDim.Parameter.Name = sDimName
              Echo("--> Success", 12, bVerboseDebug)
               If Not sDimExpr = Nothing
                Try
                  oDim.Parameter.Expression = sDimExpr
                Catch ex As Exception
                  Echo(ex.Message)
                End Try
               End If

             Catch ex As Exception
              Echo(sDimName & " exists. Setting Expression of " &  oDim.Parameter.Name  & " to equal " & sDimName & " :: " & ex.Message)
              oDim.Parameter.Expression = sDimName
            End Try
      End Sub


      Public Sub ConstrainedRect(ByVal dLength As Double, ByVal dWidth As Double,
                                 ByVal sSkName() As String, _ '0:Sketch Name, 1:Dim(1)Name, 2:Dim(2)Name, 3:Dim (1) expression, 4:Dim(2)Expression
                                 ByRef sUnit As String)
      '' Create a new sketch on a Workplane in a part document, using the default part template.
          EchoLine(,bVerboseDebug,False)
          'DebugLine("ConstrainedRect", , 2, dLength & " " & sUnit & " X " & dWidth & " " & sUnit,2)
          DebugLine("ConstrainedRect", , 2, dLength & " cm X " & dWidth & " cm",2)

          'For Error Collection/Management Get Transaction Object reference
          Dim oTrans As Transaction
          Dim oCoords(3) As Point2d
          Dim oLines(3) As SketchLine

        '' Create Rectangle (Undo Enabled)  
        oTrans = oApp.TransactionManager. _ 
                StartTransaction(oApp.ActiveDocument, _
                                  "Create Rectangle")

          Try
            Name = sSkName(0)
            BreakTitle(Name & " is a Rectangle", bVerboseDebug)

            ''Generate All Four(4) Points of the Rectangle
            ''CreatePoint2d(-dWidth/2,  dLength/2)
            oCoords(0) = oTG.CreatePoint2d(-dWidth/2,  dLength/2)
            oCoords(1) = oTG.CreatePoint2d( dWidth/2,  dLength/2)
            oCoords(2) = oTG.CreatePoint2d( dWidth/2, -dLength/2)
            oCoords(3) = oTG.CreatePoint2d(-dWidth/2, -dLength/2)
            Echo("Coordinate Collection Created", 7, bVerboseDebug)
            ' If bVerboseDebug
            '   For i = 0 To oCoords.GetUpperBound(0)
            '    Echo(oCoords(i).X & " X, " & oCoords(i).Y & " Y", ,5)
            '   Next i
            ' End If

            ''Generate Lines from Points Above
            ''AddByTwoPoints(StartPoint As Object, EndPoint As Object) As SketchLine
            oLines(0) = Sk.SketchLines.AddByTwoPoints(oCoords(0), oCoords(1))
              SkLines("Y Pos") = oLines(0)
            oLines(1) = Sk.SketchLines.AddByTwoPoints(oLines(0).EndSketchPoint, oCoords(2))
              SkLines("X Pos") = oLines(1)
            oLines(2) = Sk.SketchLines.AddByTwoPoints(oLines(1).EndSketchPoint, oCoords(3))
              SkLines("Y Neg") = oLines(2)
            oLines(3) = Sk.SketchLines.AddByTwoPoints(oLines(2).EndSketchPoint, oLines(0).StartSketchPoint)
              SkLines("X Neg") = oLines(3)
            Echo("Line Collection Created", 7, bVerboseDebug)

            ''Add GeometricConstraints to Maintain Rectangularity /new word/
            With Sk.GeometricConstraints
              .AddHorizontal(oLines(0))
              .AddVertical(oLines(1))
              .AddParallel(oLines(0),oLines(2))
              .AddParallel(oLines(1),oLines(3))
              '.AddPerpendicular(oLines(0),oLines(1)) ''OverConstrainMuch
            End With

          Catch ex As Exception
            oTrans.Abort
            Echo("Unable to Create Sketch - ConstrainedRect Exiting :: " & ex.Message)
            Exit Sub

          End Try 'lineart generated

        oTrans.End 'Undo Wrapper for Rectangle Sketch Creation
        Echo("The Sketch was Created",5,bVerboseDebug)

        '' Create Dimensions for Rectangle (Undo Enabled)  
        oTrans = oApp.TransactionManager. _ 
                StartTransaction(oApp.ActiveDocument, _
                                  "Dimension Rectangle")
          ''Dimension Top Line
          Dim oDimWidth, oDimHalfWidth As DimensionConstraint
          ' Create a dimension between the end points of the line.
          oDimWidth = Sk.DimensionConstraints.AddTwoPointDistance( _
                                            oLines(0).StartSketchPoint,
                                            oLines(0).EndSketchPoint,
                                            DimensionOrientationEnum.kAlignedDim,
                                            oTG.CreatePoint2d( ( oCoords(3).X / 2 ) , oCoords(1).Y / 4),
                                            False)
          Dims( "oDimX1" ) = oDimWidth

          oDimHalfWidth = Sk.DimensionConstraints.AddOffset( _
                                            oLines(3),
                                            CtrPoint,
                                            oTG.CreatePoint2d( ( oCoords(3).X / 2 ) , oCoords(1).Y / 5),
                                            False,)
          Dims( "oDimX2" ) = oDimHalfWidth
          Echo("The Dimension Top Line was Created",5,bVerboseDebug)

          ''Dimension Left Line
          Dim oDimLength, oDimHalfLength As DimensionConstraint
          ' Create a dimension between the end points of the line.
          oDimLength = Sk.DimensionConstraints.AddOffset( _
                                            oLines(0),
                                            oLines(2),
                                            oTG.CreatePoint2d(oCoords(3).X / 3 ,  (oCoords(0).Y / ( 3 / 2 ) ) ),
                                            False)
          Dims( "oDimY1" ) = oDimLength
          oDimHalfLength = Sk.DimensionConstraints.AddOffset( _
                                            oLines(0),
                                            CtrPoint,
                                            oTG.CreatePoint2d(oCoords(3).X / 5 , (oCoords(0).Y / 2 ) ),
                                            False,)
          Dims( "oDimY2" ) = oDimHalfLength
          Echo("The Dimension Left Line was Created",5,bVerboseDebug)
        oTrans.End 'Undo Wrapper for Rectangle Dimensions Creation

        ''Begin Naming Sequence 'sSkName=> 0:Sketch Name, 1:Dim(1)Name, 2:Dim(2)Name, 3:Dim(1)Expression, 4:Dim(2)Expression
          Try
            ' Name Sketch from Input
            Sk.Name = "Sk - " & sSkName(0)
            Echo("Sketch is now named " & sSkName(0), 5, bVerboseDebug)
          Catch
            Echo("Sketch " & sSkName(0) & " Already Exists", 5, bVerboseDebug)
            EchoLine()
          End Try


            Call SetDim(oDimLength,
                        "d" & sSkName(1),
                        sSkName(3) )

            Call SetDim(oDimWidth,
                        "d" & sSkName(2),
                         sSkName(4) )
            'SetDim(ByRef oDim As DimensionConstraint, ByVal sDimName As String, Optional ByVal sDimExpr As String = Nothing)

          oDimHalfWidth.Parameter.Expression = oDimWidth.Parameter.Name & " / 2 ul"
          oDimHalfLength.Parameter.Expression = oDimLength.Parameter.Name & " / 2 ul"

          ' Dim oCPF As CustomPropertyFormat
          ' With oCPF
          '   .ShowUnitsString = False
          '   .PropertyType = CustomPropertyTypeEnum.kTextPropertyType ' CustomPropertyTypeEnum : kNumberPropertyType 85250 Number property.  kTextPropertyType 85249 Text property. 
          '   .Precision = CustomPropertyPrecisionEnum.kSixteenthsFractionalLengthPrecision
          ' End With
          ' oDimLength.Parameter.CustomPropertyFormat = oCPF
          ' oDimWidth.Parameter.CustomPropertyFormat = oCPF

          ' ' Create rectangles on the sketch.
          ' Call Sk.SketchLines.AddAsTwoPointRectangle(oCoords(0), oCoords(2))
          ' Call sketch.SketchLines.AddAsTwoPointCenteredRectangle(tg.CreatePoint2d(0, 0), tg.CreatePoint2d(8, 3))
          ' Call sketch.SketchLines.AddAsThreePointCenteredRectangle(tg.CreatePoint2d(20, 0), tg.CreatePoint2d(28, 3), tg.CreatePoint2d(24, 9))
          Echo("Sketch Created", 10, bVerboseDebug)
          EchoLine()

      End Sub
  End Class













' Sub Main()
  '''iLogic code by Clint Brown @ClintCadline, adapted from Inventor API code found here:
  '''http://adndevblog.typepad.com/manufacturing/2013/04/create-virtual-component-by-code.html
  '''This iLogic code was originally posted on Cadline Community
  '''https://www.cadlinecommunity.co.uk/hc/en-us/articles/212726349
  ' On Error Goto ClintsErrorTrapper:
  '''Check that we are in an Assembly
  ' oDoc = ThisDoc.ModelDocument
  ' If oDoc.DocumentType = kPartDocumentObject Then
  ' MessageBox.Show("You must be in an Assembly to run this rule!", "Cadline - Virtual Part creator")
  ' Return
  ' End If

  ' Dim oAssDef As AssemblyComponentDefinition 
  ' oAssDef = ODoc.ComponentDefinition 
  ' Dim oMatrix As matrix 
  ' oMatrix = oApp.TransientGeometry.CreateMatrix
  ' NameOfVirtualPart = InputBox("What would you like to call the new Virtual Part?", "Cadline - Virtual Part creator", "Please specify")
  ''' add one virtual occurrence 
  ' Dim oNewOcc As ComponentOccurrence 
  ' oNewOcc = oAssDef.Occurrences.AddVirtual(NameOfVirtualPart, oMatrix) 
  ' Dim oCVirtualCompDef As VirtualComponentDefinition 
  ' oCVirtualCompDef = oNewOcc.Definition 
  ' Exit Sub

  ' ClintsErrorTrapper:
  ' MessageBox.Show("You already have a Virtual part with that name!", "Cadline - Virtual Part creator",MessageBoxButtons.Ok,MessageBoxIcon.Exclamation,MessageBoxDefaultButton.Button1)
' End Sub


End Class
'--Class iLogic End--------------------------------------------------------------------------------------------------
