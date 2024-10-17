'____iLogic Definition for Library/Standard Part_______
AddVbFile "VB - clsDebugViewer.vb"
AddVbFile "VB - clsFileProperties.vb"
AddVbFile "VB - clsPart.vb"

Imports Inventor.SelectionFilterEnum
Imports clsDebugViewer
Imports Microsoft.VisualBasic.Compatibility.VB6.Support

Public Class cls6xGZ
''' Contains the instructions typical for the
''' 6xGZ Template -> Header Partition / Stiffener Part
''' This part has the option for multiple cut slots

  #Region "Shared Declarations"
    '' All Public Shared Variables and Settings
    Public Shared Dim prp As New clsFileProperties
    ''Use 'prp.' as a prefix for any method called from "VB - clsFileProperties.vb"
    Public Shared Dim oApp As Inventor.Application
    Public Shared Dim oTG As TransientGeometry
    Public Shared Dim oTO As TransientObjects
    Public Shared Dim oCM As CommandManager

  #End Region

    ''' <summary>
    ''' This method performs testing & creation tasks for a new
    ''' part based on user selection.
    ''' </summary>
    ''' <remarks>Use 'New()' for New Items from Code</remarks>
    Public Sub Main()
      'LogicVb.UpdateWhenDone = True
      Dim oDoc As Document = ThisDoc.Document

      ' Get Application and Transient Functions
      oApp = ThisApplication
      oTG = oApp.TransientGeometry
      oTO = oApp.TransientObjects
      oCM = oApp.CommandManager

      PtA = GetSingleSelection(kAllPointEntities)
      PtB = GetSingleSelection(kAllPointEntities)

      Dim plTest As WorkPlane = GetSingleSelection(kWorkPlaneFilter)
      'plTest.Construction = True
      
      'plTest.Construction = False
      ' kWorkPointObject
      ' kSketechPointObject

      ' kVertexProxyObject
      ' kWorkPointProxyObject
      ' kSketechPointProxyObject

        'Select 2Points And Convert To Point2d
        'oMatrix = GetMatrix(ptA, ptB)
        'sPlateName = GetValidName(oDoc)
        'Call MakePlate(sPlateName, oMatrix)

    End Sub

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
          ' kDrawingSurfaceTextureSymbolFilter 16919 Drawing surfacetexture symbol filter. 
          ' kDrawingTitleBlockDefinitionFilter 16910 Drawing title block definition filter. 
          ' kDrawingTitleBlockFilter 16912 Drawing title block instance filter. 
          ' kDrawingViewFilter 16898 Drawing view filter. 
          ' kDrawingViewLabelFilter 16921 Drawing surfacetexture symbol filter. 
          ' kFeatureDimensionFilter 18438 Feature dimension filter. 
          ' kModelAnnotationFilter 16388 Model annotation filter. 
          ' kPartBodyFilter 15890 Part body filter. 
          ' kPartDefaultFilter 15886 Part default filter. 
          ' kPartEdgeCircularFilter 15874 Part edge circular filter. 
          ' kPartEdgeFilter 15873 Part edge filter. 
          ' kPartEdgeLinearFilter 15875 Part edge linear filter. 
          ' kPartEdgeMidpointFilter 15876 Part edge midpoint filter. 
          ' kPartFaceConicalFilter 15880 Part face conical filter. 
          ' kPartFaceCylindricalFilter 15879 Part face cylindrical filter. 
          ' kPartFaceFilter 15877 Part face filter. 
          ' kPartFacePlanarFilter 15878 Part face planar filter. 
          ' kPartFaceSphericalFilter 15882 Part face spherical filter. 
          ' kPartFaceToroidalFilter 15881 Part face toroidal filter. 
          ' kPartFeatureFilter 15884 Part feature filter. 
          ' kPartMeshEdgeCircularFilter 15903 Part mesh circular edge filter. 
          ' kPartMeshEdgeFilter 15897 Mesh Edge filter. 
          ' kPartMeshEdgeLinearFilter 15904 Part mesh linear edge filter. 
          ' kPartMeshFaceConicalFilter 15900 Part mesh conical face filter. 
          ' kPartMeshFaceCylindricalFilter 15899 Part mesh cylindrical face filter. 
          ' kPartMeshFaceFilter 15896 Mesh Face filter. 
          ' kPartMeshFacePlanarFilter 15901 Part mesh planar face filter. 
          ' kPartMeshFaceSphericalFilter 15902 Part mesh spherical face filter. 
          ' kPartMeshFeatureFilter 15894 Mesh feature filter. 
          ' kPartMeshFeatureSetFilter 15895 Mesh feature set filter. 
          ' kPartMeshVertexFilter 15898 Mesh Vertex filter. 
          ' kPartSurfaceFeatureFilter 15885 Part surface feature filter. 
          ' kPartVertexFilter 15883 Part vertext filter. 
          ' kPointCloudFilter 15891 Point cloud filter. 
          ' kPointCloudPlaneFilter 15893 Point cloud plane filter. 
          ' kPointCloudPointFilter 15892 Point cloud point filter. 
          ' kPublicationComponentFilter 17153 Publication component filter. 
          ' kPublicationEdgeFilter 17156 Publication Edge filter. 
          ' kPublicationFaceFilter 17155 Publication face filter. 
          ' kPublicationLeafComponentFilter 17154 Publication leaf component filter. 
          ' kPublicationTrailNodeFilter 17159 Publication trail node filter. 
          ' kPublicationTrailSegmentFilter 17160 Publication trail segment filter. 
          ' kPublicationTweakPathFilter 17158 Publication tweakpath filter. 
          ' kPublicationVertexFilter 17157 Publication Vertex filter. 
          ' kSketch3DCurveCircularFilter 17666 3D Sketch curve circular filter. 
          ' kSketch3DCurveEllipseFilter 17667 3D Sketch curve ellipse filter. 
          ' kSketch3DCurveFilter 17664 3D Sketch curve filter. 
          ' kSketch3DCurveLinearFilter 17665 3D Sketch curve linear filter. 
          ' kSketch3DCurveSplineFilter 17668 3D Sketch curve spline filter. 
          ' kSketch3DDefaultFilter 17670 3D Sketch default filter. 
          ' kSketch3DDimConstraintFilter 17672 3D Sketch dimension constraint filter. 
          ' kSketch3DObjectFilter 17671 3D Sketch filter. 
          ' kSketch3DPointFilter 17669 3D Sketch point filter. 
          ' kSketch3DProfileFilter 17673 3D Sketch profile filter. 
          ' kSketchBlockDefinitionFilter 16141 SketchBlock definition filter. 
          ' kSketchBlockFilter 16142 SketchBlock filter. 
          ' kSketchCurveCircularFilter 16131 Sketch curve circular filter. 
          ' kSketchCurveEllipseFilter 16132 Sketch curve ellipse filter. 
          ' kSketchCurveFilter 16129 Sketch curve filter. 
          ' kSketchCurveLinearFilter 16130 Sketch curve linear filter. 
          ' kSketchCurveSplineFilter 16133 Sketch curve spline filter. 
          ' kSketchDefaultFilter 16135 Sketch default filter. 
          ' kSketchDimConstraintFilter 16128 Sketch dimension constraint filter. 
          ' kSketchImageFilter 16137 SketchImage filter. 
          ' kSketchObjectFilter 16136 Sketch filter. 
          ' kSketchPointFilter 16134 Sketch point filter. 
          ' kSketchProfileFilter 16139 2D Profile filter. 
          ' kSketchProjectedCutFilter 16140 ProjectedCut filter. 
          ' kSketchTextBoxFilter 16138 Sketch TextBox filter. 
          ' kUserCoordinateSystemFilter 16387 UserCoordinateSystem Filter. 
          ' kWorkAxisFilter 16384 Work axis filter. 
          ' kWorkPlaneFilter 16385 Work plane filter. 
          ' kWorkPointFilter 16386 Work point filter. 


        Echo( "Picked: " & oObject.Type)
        Echo( "        " & [Enum].GetName(GetType(ObjectTypeEnum), oObject.Type))
        Return oObject
    End Function

    Public Sub CheckSelection(Optional sPrompt As String = "Pick a Point")
        ' Get a feature selection from the user
        Dim oObject As Object 
        oObject	= oCM.Pick(SelectionFilterEnum.kAllPointEntities, "Pick a Point")

        MsgBox( "Picked: " & oObject.Name)

    End Sub


    Structure Plate
        Const Path = "\6xGZ.ipt"
    End Structure


    ''' <summary>
    ''' This method assigns the standard ID for calling/running this rule.
    ''' </summary>
    ''' <param name="sID">String should be the same as the File/Class ID</param>
    ''' <remarks></remarks>
    Sub SetStdNumber(Optional ByVal sID As String = "6xGZ")
     prp.SetiPropValue(ThisDoc.Document,
                       "Standard Part",
                       sID,
                       "Hudson Properties")
    End Sub

    ''' <summary>
    ''' This method cycles through each parameter and replaces 
    ''' the prefix based on a search string with the given values. 
    ''' <example>
    ''' For example:
    ''' <code>
    '''     Dim oDoc As Document = ThisDoc.Document
    '''     Call PrefixParameters(oDoc, "_", "1")
    ''' </code>
    ''' results in <c>dH_:LG</c> having the value <c>dH1:LG</c>.
    ''' </example>
    ''' </summary>
    ''' <param name="oParamSet">Parameter Set from a Component Defintion (eg. Parameters, ReferenceParameters, ModelParameters ...)</param>
    ''' <param name="sFind">String to Search For</param>
    ''' <param name="sNew">String to Replace With</param>
    ''' <returns></returns>
    ''' <remarks>This is already a function that is shown for reference</remarks>
    Sub PrefixParameterNames(ByRef oParamSet As Object,
                             ByRef sFind As String,
                             ByRef sNew As String)
    '
      Try
        For Each Prm As Parameter in oParamSet
        Try
            If Left(Prm.Name , Len(sFind) ) = sFind Then 
                Echo("Parameter " & Prm.Name & " is changed to:")
                Prm.Name = sNew & Right( Prm.Name , Max( Len(Prm.Name) - Len(sFind) , 0 ) )
                Echo("          \----> " & Prm.Name)
            End If

        Catch ex1 As Exception
                Echo(ex1.Message)
        End Try
      Next Prm

      Catch ex As Exception
        Echo(ex.Message)
        Exit Sub
      End Try
    End Sub


    ''' <summary>
    ''' This function determines the matrix position for a part given
    ''' two point inputs.
    ''' </summary>
    ''' <param name="ptStart">Point2d Construct (Startpoint)</param>
    ''' <param name="ptEnd">Point2d Construct (Endpoint)</param>
    ''' <returns>Matrix for Position of Part</returns>
    ''' <remarks>Might need to get a sequence for rotation?</remarks>
    Public Shared Function GetMatrix(ptStart As Point2d, ptEnd As Point2d) As Matrix

      Dim dXTrans, dYTrans, dZTrans As Double
      Dim dXRotation, dYRotation, dZRotation As Double
      
      dZTrans = 0
      Try
            dXTrans = ( ptStart.X - ptEnd.X ) / 2
            dYTrans = ( ptStart.Y - ptEnd.Y ) / 2
        Catch ex As Exception
            dXTrans = 0
            dYTrans = 0
            Echo( "Error occurred on translation formula :: " & ex.Message)
      End Try

      ' If y = y then Angle = 90
      ' if y <> y And x <> x then getangle(xy1, xy2)


      ' Get Matrix object for Part Placement
      Dim oMatrix, oX_RotationMatrix, oY_RotationMatrix, oZ_RotationMatrix As Matrix


      oMatrix = oTG.CreateMatrix
      oX_RotationMatrix = oTG.CreateMatrix
      oY_RotationMatrix = oTG.CreateMatrix
      oZ_RotationMatrix = oTG.CreateMatrix
      Echo("Translation & Rotation Matrices Made", 8, bVerboseDebug)

' y = mx + b
' y2 - y1 / x2 - x1 = m

' x = x2 - x1
' y = y2 - y1

' tan(alpha) = y / x
' sin(alpha) = opp / hyp

' yt = row(1).ybottom - row(0).ytop
' hyp = yt / sin(alpha)

' plate( (wet - ( 2 * weld.gap) ) , hyp )

      ' Set Part Rotation about X Axis
      Call oX_RotationMatrix.SetToRotation(dPi/2,
                            oTG.CreateVector(1, 0, 0),
                            oTG.CreatePoint(0, 0, 0))
      ' Set Part Rotation about Y Axis
      Call oY_RotationMatrix.SetToRotation(dPi/2,
                            oTG.CreateVector(0, 1, 0),
                            oTG.CreatePoint(0, 0, 0))
      ' Set Part Rotation about Z Axis
      Call oZ_RotationMatrix.SetToRotation(dPi/2,
                            oTG.CreateVector(0, 0, 1),
                            oTG.CreatePoint(0, 0, 0))
      Echo("Rotation Set")

      Call oMatrix.TransformBy(oX_RotationMatrix)
      Call oMatrix.TransformBy(oY_RotationMatrix)
      Call oMatrix.TransformBy(oZ_RotationMatrix)
      Echo("Rotation Applied to Matrix")

      Call oMatrix.SetTranslation(oTG.CreateVector(dXTrans, dYTrans, dZTrans))

      Return oMatrix
    End Function

    Public Shared Dim dPi As Double = 4.0 * Math.Atan(1.0) '333/106
    Public Function GetRads(ByVal oValue As Double)
        Return ( dPi / ( 180 / oValue ) )
    End Function

    Public Function GetAngle(ByVal Pt1 As Point2d, ByVal Pt2 As Point2d)
        Dim dReturnAngle As Double
        
        Return dReturnAngle
    End Function

  '  ''' <summary>
  '  ''' On every drawing Autodesk Inventor include a thumbnail
  '  ''' We can also get that picture out of the IProperties and convert it to System.Drawing.Image
  '  ''' You must add a reference to Microsoft.VisualBasic.Compatibility
  '  ''' 
  '  ''' I dont use it anymore:
  '  ''' To read Thumbnails out of every file use this: http://www.espend.de/node/34 it works
  '  ''' with every file that has a thumbnail handler in the windows explorer
  '  ''' </summary>
  '  ''' <param name="FullFileName">A Autodesk Inventor File (eg. IAM, IPT, IDW, ...)</param>
  '  ''' <returns>The thumbnail of the Inventor file as Image</returns>
  '  ''' <remarks></remarks>
  '  Shared Function GetThumbnail(ByVal FullFileName As String) As System.Drawing.Image
  '      Try
  '          Dim InventorObject As New ApprenticeServerComponent
  '          Dim InventorDocument As ApprenticeServerDocument = InventorObject.Open(FullFileName)
  '          Dim value As Object = InventorDocument.PropertySets.Item("Inventor Summary Information").Item("Thumbnail").Value

  '          InventorDocument.Close() : InventorDocument = Nothing
  '          InventorObject.Close() : InventorObject = Nothing

  '          'change it if you need that stuff
  '          'Return Nothing
  '          Return Microsoft.VisualBasic.Compatibility.VB6.Support.IPictureDispToImage(value)
  '        Catch ex As Exception
  '          Return Nothing
  '      End Try
  '  End Function

End Class