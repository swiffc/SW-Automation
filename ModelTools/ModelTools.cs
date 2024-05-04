using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Runtime.Remoting.Metadata.W3cXsd2001;
using System.Text;
using System.Threading.Tasks;
using cTools = ModelTools.ReleaseCOM;

namespace Tools
{
    public static class ModelTools
    {
        public static double WeldClearance => 0.0625;
        public static double AssemblyClearance => 0.125;
        public static double InterferenceClearance => 0.25;
        public static double MaxSheetWidth => 72;
        public static double HoleToEdge_General => 3;

        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        public static ModelDoc2 Open(string filePath, string configurationName = null, bool newLine = false)
        {
            if (newLine)
            {
                Debug.WriteLine("\n");
            }
            // Local variables
            int errors = 0;
            int warnings = 0;
            string extension = Path.GetExtension(filePath);
            int documentType;

            // Set documentType
            if (extension.Equals(".SLDPRT", StringComparison.OrdinalIgnoreCase))
            {
                documentType = (int)swDocumentTypes_e.swDocPART;
            }
            else if (extension.Equals(".SLDASM", StringComparison.OrdinalIgnoreCase))
            {
                documentType = (int)swDocumentTypes_e.swDocASSEMBLY;
            }
            else if (extension.Equals(".SLDDRW", StringComparison.OrdinalIgnoreCase))
            {
                documentType = (int)swDocumentTypes_e.swDocDRAWING;
            }
            else
            {
                throw new ArgumentException("Unknown file extension");
            }

            // Open the SOLIDWORKS document using the API call
            ModelDoc2 modelDoc2 = SW.OpenDoc6(
                filePath,
                documentType,
                (int)swOpenDocOptions_e.swOpenDocOptions_Silent,
                configurationName,
                errors, warnings);

            // Log errors and warnings
            if (errors != 0)
            {
                Debug.WriteLine($"Errors encountered while opening: {errors}");
            }
            if (warnings != 0)
            {
                Debug.WriteLine($"Warnings encountered while opening: {warnings}");
            }

            //modelDoc2.FeatureManager.EnableFeatureTree = false;

            return modelDoc2;
        }
        public static void SaveEverything()
        {
            Debug.WriteLine($"Saving all components in active document {Path.GetFileNameWithoutExtension(SW.IActiveDoc2.GetPathName())}");

            object[] componentObj = (SW.IActiveDoc2 as AssemblyDoc).GetComponents(false);

            var componentFileNamePairs = new List<KeyValuePair<Component2, string>>();
            foreach (var obj in componentObj)
            {
                Component2 component2 = obj as Component2;
                if (component2 != null)
                {
                    ModelDoc2 modelDoc2 = component2.GetModelDoc2();
                    if (modelDoc2 != null)
                    {
                        string pathName = modelDoc2.GetPathName();
                        if (!string.IsNullOrEmpty(pathName))
                        {
                            string fileName = Path.GetFileNameWithoutExtension(pathName);
                            // Create the pair and add to the list
                            var pair = new KeyValuePair<Component2, string>(component2, fileName);
                            componentFileNamePairs.Add(pair);
                        }
                    }
                }
            }

            // Remove duplicates and sort the list
            var uniqueAndSortedComponentFileNamePairs = componentFileNamePairs
                .GroupBy(pair => pair.Value)
                .Select(group => group.First())
                .OrderBy(pair => pair.Value)
                .ToList();

            // Save each unique and sorted component
            foreach (var pair in uniqueAndSortedComponentFileNamePairs)
            {
                ModelDoc2 modelDoc2 = pair.Key.GetModelDoc2();
                if (modelDoc2 != null)
                {
                    bool saveStatus = modelDoc2.Save3((int)swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0);
                    if (saveStatus)
                    {
                        Debug.WriteLine($"Saved: {pair.Value}");
                    }
                    else
                    {
                        Debug.WriteLine($"Failed to save: {pair.Value}");
                    }
                }
            }

            // Optionally, save the active document itself
            bool mainDocSaveStatus = SW.IActiveDoc2.Save3((int)swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0);
            if (mainDocSaveStatus)
            {
                Debug.WriteLine("Main document saved successfully.");
            }
            else
            {
                Debug.WriteLine("Failed to save the main document.");
            }
        }

        public static void Lock()
        {
            SW.IActiveDoc2.Lock();
        }
        public static void Unlock()
        {
            SW.IActiveDoc2.UnLock();
        }
        public static Component2 InsertComponent(string filePath, AssemblyDoc assemblyDoc)
        {
            string assemblyName = Path.GetFileNameWithoutExtension((assemblyDoc as ModelDoc2).GetPathName());
            Component2 component2 = assemblyDoc.AddComponent5
                (filePath,
                (int)swAddComponentConfigOptions_e.swAddComponentConfigOptions_CurrentSelectedConfig, null,
                false, null,
                0, 0, 0);
            Debug.WriteLine($"      Inserted new component:    {component2.Name2}");
            return component2;
        }
        public static void Close(string filePath)
        {
            SW.CloseDoc(filePath);
            string fileName = Path.GetFileNameWithoutExtension(filePath);
            string partNumber = fileName.Substring(fileName.IndexOf("-") + 1);
            Debug.WriteLine($"   Closed {partNumber}");
        }
        public static bool EditDimension(string dimensionName, string treeName, double newValue, ModelDoc2 modelDoc2)
        {
            bool editSuccessful = false;

            if (modelDoc2 == null)
            {
                Debug.WriteLine("   Error: ModelDoc2 is null.");
                return false;
            }

            string equationName = $"{dimensionName}@{treeName}";
            Dimension dimension = null;

            try
            {
                dimension = modelDoc2.Parameter(equationName);

                if (dimension != null)
                {
                    dimension.SetValue3(newValue, (int)swSetValueInConfiguration_e.swSetValue_UseCurrentSetting, null);
                    //Debug.WriteLine($"   Dimension {dimension.Name} modified");
                    editSuccessful = true;
                }
                else
                {
                    Debug.WriteLine($"   Error: Dimension {equationName} not found.");
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"   Exception encountered: {ex.Message}");
            }
            finally
            {
                if (dimension != null)
                {
                    Marshal.ReleaseComObject(dimension);
                    dimension = null;
                }
            }

            return editSuccessful;
        }
        public static void Rebuild(bool zoomToIsometric = false)
        {
            SW.IActiveDoc2.EditRebuild3();
            if (zoomToIsometric)
            {
                SW.IActiveDoc2.ShowNamedView2("*Isometric", -1);
                SW.IActiveDoc2.ViewZoomtofit2();
            }
        }
        public static void ShowTopView()
        {
            SW.IActiveDoc2.EditRebuild3();
            SW.IActiveDoc2.ShowNamedView2("*Top", -1);
            SW.IActiveDoc2.ViewZoomtofit2();
        }

        public static Dictionary<(double Min, double Max), (double Min, double Max)> TipClearance = new Dictionary<(double Min, double Max), (double Min, double Max)>
        {
            {(0,     108),   (0.25, 0.5)},
            {(108.001, 132),   (0.25, 0.625)},
            {(132.001,   1000),  (0.25, 0.75)}
        };
        public static (double Min, double Max) GetTipClearance(double value)
        {
            foreach (var range in TipClearance.Keys)
            {
                if (value >= range.Min && value <= range.Max)
                {
                    return TipClearance[range];
                }
            }
            throw new KeyNotFoundException("Value not within any range.");
        }

        public static Dictionary<double, double> BendTable = new Dictionary<double, double>
        {
            //  THK     R
            { 0.1344, 0.2500 },
            { 0.1875, 0.3125 },
            { 0.2500, 0.5000 },
            { 0.3125, 0.5625 },
            { 0.3750, 0.6875 }
        };

        public static double[] PositionMaxtrix { get; set; } = new double[16]
        {
            1,0,0,0,
            1,0,0,0,
            1,0,0,0,
            1,0,0,0
        };

        public static void InchesToMeters(double[] positionMatrix)
        {
            positionMatrix[9] = positionMatrix[9] * 0.0254;
            positionMatrix[10] = positionMatrix[10] * 0.0254;
            positionMatrix[11] = positionMatrix[11] * 0.0254;
        }

        public static void MetersToInches(double[] positionMatrix)
        {
            const double METERS_TO_INCHES_FACTOR = 39.3701; // 1 meter = 39.3701 inches
            positionMatrix[9] = positionMatrix[9] * METERS_TO_INCHES_FACTOR;
            positionMatrix[10] = positionMatrix[10] * METERS_TO_INCHES_FACTOR;
            positionMatrix[11] = positionMatrix[11] * METERS_TO_INCHES_FACTOR;
        }


        public static void SetPosition(Component2 component)
        {
            InchesToMeters(PositionMaxtrix);
            MathTransform mathTransform = SW.GetMathUtility().CreateTransform(PositionMaxtrix);
            component.Transform2 = mathTransform;

            PositionMaxtrix = new double[16]
            {
                1,0,0,0,
                1,0,0,0,
                1,0,0,0,
                1,0,0,0
             };

            Debug.WriteLine($"Position set: {component.Name2}");

            cTools.Release(ref mathTransform);
        }

        public static void X_Translation(double value)
        {
            PositionMaxtrix[9] = value;
        }
        public static void Y_Translation(double value)
        {
            PositionMaxtrix[10] = value;
        }
        public static void Y_Rotation(double degrees)
        {
            double radians = degrees * Math.PI / 180.0;
            PositionMaxtrix[0] = Math.Cos(radians);
            PositionMaxtrix[2] = Math.Sin(radians);
            PositionMaxtrix[6] = -Math.Sin(radians);
            PositionMaxtrix[8] = Math.Cos(radians);
        }

        public static void Z_Translation(double value)
        {
            PositionMaxtrix[11] = value;
        }

        public static void PrintPositionMatrixValues(Component2 component2)
        {
            const double METERS_TO_INCHES_FACTOR = 39.3701; // 1 meter = 39.3701 inches
            double[] positionData = component2.Transform2.ArrayData;

            for (int i = 0; i < positionData.Length; i++)
            {
                double x = positionData[i];

                // Convert values for specific indices
                if (i == 9 || i == 10 || i == 11)
                {
                    x *= METERS_TO_INCHES_FACTOR;
                    positionData[i] = x;
                }
            }

            Debug.WriteLine
            (
                "\n" +
                $"|  a0: {(positionData[0] >= 0 ? " " : "")}{positionData[0]:0.000}     b1: {(positionData[1] >= 0 ? " " : "")}{positionData[1]:0.000}     c2: {(positionData[2] >= 0 ? " " : "")}{positionData[2]:0.000}  .   n9: {(positionData[9] >= 0 ? " " : "")}{positionData[9]:0.000} |" + "\n" +
                $"|  d3: {(positionData[3] >= 0 ? " " : "")}{positionData[3]:0.000}     e4: {(positionData[4] >= 0 ? " " : "")}{positionData[4]:0.000}     f5: {(positionData[5] >= 0 ? " " : "")}{positionData[5]:0.000}  .  o10: {(positionData[10] >= 0 ? " " : "")}{positionData[10]:0.000} |" + "\n" +
                $"|  g6: {(positionData[6] >= 0 ? " " : "")}{positionData[6]:0.000}     h7: {(positionData[7] >= 0 ? " " : "")}{positionData[7]:0.000}     i8: {(positionData[8] >= 0 ? " " : "")}{positionData[8]:0.000}  .  p11: {(positionData[11] >= 0 ? " " : "")}{positionData[11]:0.000} |" + "\n" +
                $"| j13: {(positionData[13] >= 0 ? " " : "")}{positionData[13]:0.000}    k14: {(positionData[14] >= 0 ? " " : "")}{positionData[14]:0.000}    l15: {(positionData[15] >= 0 ? " " : "")}{positionData[15]:0.000}  .  m12: {(positionData[12] >= 0 ? " " : "")}{positionData[12]:0.000} |" + "\n" + "\n"
            );
        }



        public static SketchPoint CreatePoint(double x, double y, double z)
        {
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            SketchManager sketchManager = modelDoc2.SketchManager;
            SketchPoint point;

            bool sketchExists = modelDoc2.Extension.SelectByID2("Reference", "SKETCH", 0, 0, 0, false, 0, null, 0);
            if (sketchExists)
            {
                modelDoc2.EditSketch();
                point = CreateFixedPoint();
            }
            else
            {
                sketchManager.Insert3DSketch(false);
                point = CreateFixedPoint();

                Feature sketchFeature = modelDoc2.Extension.GetLastFeatureAdded();
                sketchFeature.Name = "Reference";
            }

            return point;

            SketchPoint CreateFixedPoint()
            {
                SketchPoint sketchPoint = sketchManager.CreatePoint(x / 39.3701, y / 39.3701, z / 39.3701);
                modelDoc2.SketchAddConstraints("sgFIXED");
                sketchManager.Insert3DSketch(false);

                return sketchPoint;
            }
        }
        public static SketchSegment ConnectPoints(bool closeLoop = false, bool makeConstruction = false, params SketchPoint[] points)
        {
            if (points == null || points.Length < 2)
            {
                throw new ArgumentException("At least two points are required to create a line.");
            }

            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            SketchManager sketchManager = modelDoc2.SketchManager;
            modelDoc2.Extension.SelectByID2("Reference", "SKETCH", 0, 0, 0, false, 0, null, 0);
            modelDoc2.EditSketch();

            for (int i = 0; i < points.Length; i++)
            {
                SketchPoint startPt = points[i];
                SketchPoint endPt = i < points.Length - 1 ? points[i + 1] : points[0]; // Loop back if closeLoop is true

                // Create a line between each pair of points
                SketchSegment line = sketchManager.CreateLine(
                    startPt.X, startPt.Y, startPt.Z,
                    endPt.X, endPt.Y, endPt.Z
                );

                if (makeConstruction)
                {
                    line.ConstructionGeometry = true; // Set line as construction geometry
                }

                modelDoc2.SketchAddConstraints("sgFIXED");

                if (points.Length == 2)
                {
                    sketchManager.Insert3DSketch(false);
                    return line;
                }


                // If not closing the loop, break after the last line segment is created
                if (!closeLoop && i == points.Length - 2)
                    break;
            }

            // Close the 3D sketch
            sketchManager.Insert3DSketch(false);

            // Deselect everything after applying 'Fixed' constraint
            modelDoc2.ClearSelection2(true);

            return null;
        }



        public static void ClearReferenceSketch()
        {
            Feature referenceSketch = (SW.IActiveDoc2 as AssemblyDoc).FeatureByName("Reference");
            if (referenceSketch != null)
            {
                SW.IActiveDoc2.ClearSelection2(true);
                referenceSketch.Select2(false, 0);
                SW.IActiveDoc2.Extension.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
            }
            SW.IActiveDoc2.ShowNamedView2("*Isometric", -1);
            SW.IActiveDoc2.ViewZoomtofit2();
        }

        public static void CreatePlane(string planeName, SketchPoint p1, SketchPoint p2, SketchPoint p3)
        {
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            IModelDocExtension modelDocExt = modelDoc2.Extension;
            FeatureManager featureManager = modelDoc2.FeatureManager;
            modelDoc2.ClearSelection2(true);

            Feature existingPlane = (modelDoc2 as AssemblyDoc).FeatureByName(planeName);
            if (existingPlane != null)
            {
                // If it exists, select and delete the existing plane
                bool isSelected = existingPlane.Select2(false, 0);
                if (isSelected)
                {
                    modelDocExt.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
                }
            }


            bool p1Selected = modelDocExt.SelectByID2("", "EXTSKETCHPOINT", p1.X, p1.Y, p1.Z, false, 0, null, 0);
            bool p2Selected = modelDocExt.SelectByID2("", "EXTSKETCHPOINT", p2.X, p2.Y, p2.Z, true, 1, null, 0);
            bool p3Selected = modelDocExt.SelectByID2("", "EXTSKETCHPOINT", p3.X, p3.Y, p3.Z, true, 2, null, 0);


            // Create a plane through the selected points
            Feature planeFeature = featureManager.InsertRefPlane
                (
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_Coincident, 0,
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_Coincident, 0,
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_Coincident, 0
                );

            planeFeature.Name = planeName;

            cTools.Release(ref modelDocExt);
            cTools.Release(ref featureManager);
            cTools.Release(ref existingPlane);
        }
        public static void CreatePlane(string planeName, SketchPoint p, SketchSegment s)
        {
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            IModelDocExtension modelDocExt = modelDoc2.Extension;
            FeatureManager featureManager = modelDoc2.FeatureManager;
            modelDoc2.ClearSelection2(true);

            Feature existingPlane = (modelDoc2 as AssemblyDoc).FeatureByName(planeName);
            if (existingPlane != null)
            {
                // If it exists, select and delete the existing plane
                bool isSelected = existingPlane.Select2(false, 0);
                if (isSelected)
                {
                    modelDocExt.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
                }
            }

            bool p1Selected = modelDocExt.SelectByID2("", "EXTSKETCHPOINT", p.X, p.Y, p.Z, false, 0, null, 0);
            bool p2Selected = modelDocExt.SelectByID2(s.GetName() + "@Reference", "EXTSKETCHSEGMENT", p.X, p.Y, p.Z, true, 1, null, 0);

            // Create a plane through the selected points
            Feature planeFeature = featureManager.InsertRefPlane
                (
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_Coincident, 0,
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_Perpendicular, 0,
                0, 0
                );

            planeFeature.Name = planeName;

            cTools.Release(ref existingPlane);
            cTools.Release(ref planeFeature);
            cTools.Release(ref modelDocExt);
            cTools.Release(ref featureManager);
        }

        public static void CreatePlane(Feature plane1, Feature plane2, bool referenceFlip = false)
        {
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;
            IModelDocExtension modelDocExt = modelDoc2.Extension;
            FeatureManager featureManager = modelDoc2.FeatureManager;

            string planeName = $"MidPlane[{plane1.Name}, {plane2.Name}]";

            Feature existingPlane = (modelDoc2 as AssemblyDoc).FeatureByName(planeName);
            if (existingPlane != null)
            {
                // If it exists, select and delete the existing plane
                bool isSelected = existingPlane.Select2(false, 0);
                if (isSelected)
                {
                    modelDocExt.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
                }
            }

            // Select the first plane without clearing the selection and without toggling selection
            modelDocExt.SelectByID2(plane1.Name, "PLANE", 0, 0, 0, false, 0, null, 0);

            // Now select the second plane, adding to the current selection
            modelDocExt.SelectByID2(plane2.Name, "PLANE", 0, 0, 0, true, 1, null, 0);

            int reference;
            if (referenceFlip)
            {
                reference = (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_MidPlane | (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_OptionReferenceFlip;
            }
            else
            {
                reference = (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_MidPlane;
            }

            // Create a midplane with the flip option on the first plane
            Feature planeFeature = featureManager.InsertRefPlane
            (
                reference, 0,
                (int)swRefPlaneReferenceConstraints_e.swRefPlaneReferenceConstraint_MidPlane, 0,
                0, 0
            );

            if (planeFeature != null)
            {
                planeFeature.Name = planeName;
            }
            else
            {
                // Handle the case where the plane wasn't created
                Console.WriteLine("The midplane could not be created.");
            }

            cTools.Release(ref planeFeature);
            cTools.Release(ref modelDocExt);
            cTools.Release(ref featureManager);
        }



        public static void AddDrivenDimension(SketchSegment seg1, SketchSegment seg2)
        {
            ModelDoc2 swModel = SW.IActiveDoc2;

            // Attempt to find the 3D sketch named "Reference"
            Feature feature = (swModel as AssemblyDoc).FeatureByName("Reference");
            // Edit the sketch named "Reference"
            feature.Select2(false, 0);
            swModel.EditSketch();

            // Select the first segment
            seg1.Select4(true, null);

            seg2.Select4(true, null);
            double x = 0.0;
            double y = 0.0;

            DisplayDimension dispDim = swModel.AddDimension2(x, y, 0);
            if (dispDim != null)
            {
                // Set the dimension to be driven
                Dimension dim = dispDim.GetDimension();
                dim.DrivenState = (int)swDimensionDrivenState_e.swDimensionDriven;
            }

            Rebuild();

        }



        // triangles
        public static void AAS(double oppositeAngle, double oppositeSide, out double adjacentSide, out double hypotenuse)
        {
            double oppositeAngleRadians = oppositeAngle * (Math.PI / 180.0);
            adjacentSide = oppositeSide / Math.Tan(oppositeAngleRadians);
            hypotenuse = Math.Sqrt(Math.Pow(oppositeSide, 2) + Math.Pow(adjacentSide, 2));
        }
        public static void AAS(double oppositeAngle, out double oppositeSide, double adjacentSide, out double hypotenuse)
        {
            double oppositeAngleRadians = oppositeAngle * (Math.PI / 180.0);
            oppositeSide = adjacentSide * Math.Tan(oppositeAngleRadians);
            hypotenuse = Math.Sqrt(Math.Pow(oppositeSide, 2) + Math.Pow(adjacentSide, 2));
        }
        public static void AAS(double oppositeAngle, out double oppositeSide, out double adjacentSide, double hypotenuse)
        {
            double oppositeAngleRadians = oppositeAngle * (Math.PI / 180.0);
            oppositeSide = hypotenuse * Math.Sin(oppositeAngleRadians);
            adjacentSide = hypotenuse * Math.Cos(oppositeAngleRadians);
        }
<<<<<<< HEAD
=======
        public static void AAS(out double oppositeAngle, double oppositeSide, double adjacentSide, out double hypotenuse)
        {
            hypotenuse = Math.Sqrt(Math.Pow(oppositeSide, 2) + Math.Pow(adjacentSide, 2));
            oppositeAngle = Math.Atan2(oppositeSide, adjacentSide) * (180.0 / Math.PI);
        }
>>>>>>> releases/v4.0.0



        public static void DisableAssemblyUI()
        {
            SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        public static void EnableAssemblyUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocASSEMBLY);
        }
        public static void DisablePartUI()
        {
            SW.DocumentVisible(false, (int)swDocumentTypes_e.swDocPART);
        }
        public static void EnablePartUI()
        {
            SW.DocumentVisible(true, (int)swDocumentTypes_e.swDocPART);
        }



        public static double[] MirrorMatrix_XYplane(double[] originalMatrix)
        {
            double[] mirroredMatrix = (double[])originalMatrix.Clone();

            mirroredMatrix[3] = -mirroredMatrix[3];
            mirroredMatrix[4] = -mirroredMatrix[4];
            mirroredMatrix[8] = -mirroredMatrix[8];

            mirroredMatrix[11] = -mirroredMatrix[11];

            return mirroredMatrix;
        }

        public static double[] MirrorMatrix_YZplane(double[] originalMatrix)
        {
            double[] mirroredMatrix = (double[])originalMatrix.Clone();

            mirroredMatrix[0] = -mirroredMatrix[0];
            mirroredMatrix[4] = -mirroredMatrix[4];
            mirroredMatrix[5] = -mirroredMatrix[5];
            mirroredMatrix[6] = -mirroredMatrix[6];

            mirroredMatrix[9] = -mirroredMatrix[9];

            return mirroredMatrix;
        }




        public static SldWorks GetSW()
        {
            return SW;
        }




        public static void Z_Axis_Rotate(double degrees)
        {
            double radians = degrees * (Math.PI / 180);

            PositionMaxtrix[0] = Math.Cos(radians);
            PositionMaxtrix[1] = Math.Sin(radians);
            PositionMaxtrix[3] = -Math.Sin(radians);
            PositionMaxtrix[4] = Math.Cos(radians);

        }
        public static void X_Axis_Rotate(double degrees)
        {
            double radians = degrees * (Math.PI / 180);

            PositionMaxtrix[4] = Math.Sin(radians);
            PositionMaxtrix[5] = Math.Cos(radians);
            PositionMaxtrix[7] = -Math.Cos(radians);
            PositionMaxtrix[8] = Math.Sin(radians);
        }
        private static Matrix4x4 Z_Axis_Rotate2(double degrees)
        {
            double radians = degrees * (Math.PI / 180);
            return new Matrix4x4(
                (float)Math.Cos(radians), (float)-Math.Sin(radians), 0, 0,
                (float)Math.Sin(radians), (float)Math.Cos(radians), 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1
            );
        }
        private static Matrix4x4 X_Axis_Rotate2(double degrees)
        {
            double radians = degrees * (Math.PI / 180);
            return new Matrix4x4(
                1, 0, 0, 0,
                0, (float)Math.Cos(radians), (float)-Math.Sin(radians), 0,
                0, (float)Math.Sin(radians), (float)Math.Cos(radians), 0,
                0, 0, 0, 1
            );
        }
        private static Matrix4x4 Y_Axis_Rotate2(double degrees)
        {
            double radians = degrees * (Math.PI / 180);
            return new Matrix4x4(
                (float)Math.Cos(radians), 0, (float)Math.Sin(radians), 0,
                0, 1, 0, 0,
                (float)-Math.Sin(radians), 0, (float)Math.Cos(radians), 0,
                0, 0, 0, 1
            );
        }
        public static void Rotate(double xDegrees, double yDegrees, double zDegrees)
        {


            Matrix4x4 xRotation = X_Axis_Rotate2(xDegrees);
            Matrix4x4 yRotation = Y_Axis_Rotate2(yDegrees);
            Matrix4x4 zRotation = Z_Axis_Rotate2(zDegrees);

            Matrix4x4 matrix = Matrix4x4.Multiply(Matrix4x4.Multiply(xRotation, yRotation), zRotation);

            PositionMaxtrix[0] = matrix.M11;
            PositionMaxtrix[1] = matrix.M12;
            PositionMaxtrix[2] = matrix.M13;
            PositionMaxtrix[3] = matrix.M21;
            PositionMaxtrix[4] = matrix.M22;
            PositionMaxtrix[5] = matrix.M23;
            PositionMaxtrix[6] = matrix.M31;
            PositionMaxtrix[7] = matrix.M32;
            PositionMaxtrix[8] = matrix.M33;
        }
        public static bool DeleteComponentByName(string compName, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            string modelDocPath = modelDoc2.GetPathName();
            string assemblyPartNumber = Path.GetFileNameWithoutExtension(SW.IActiveDoc2.GetPathName());

            bool isSelected = SW.IActiveDoc2.Extension.SelectByID2($"{compName}@{assemblyPartNumber}", "COMPONENT", 0, 0, 0, false, 0, null, 0);
            if (isSelected)
            {
                SW.IActiveDoc2.Extension.DeleteSelection2(0);
            }
            return isSelected;
        }
        public static void MakeTransParent(Component2 component2)
        {
            SW.IActiveDoc2.Extension.SelectByID2
            (
                component2.Name2 + $"@{Path.GetFileNameWithoutExtension(SW.IActiveDoc2.GetPathName())}",
                "COMPONENT", 0, 0, 0, false, 0, null, 0
            );
            (SW.IActiveDoc2 as AssemblyDoc).SetComponentTransparent(true);
        }



        public static AssemblyDoc OpenAssembly(string filePath, string configurationName = null)
        {
            ModelDoc2 modelDoc2 = Open(filePath, configurationName);
            SW.ActivateDoc3(filePath, false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
            return modelDoc2 as AssemblyDoc;
        }
        public static AssemblyDoc OpenAssemblySilent(string filePath, string configurationName)
        {
            DisableAssemblyUI();
            ModelDoc2 modelDoc2 = Open(filePath, configurationName);
            EnableAssemblyUI();
            SW.ActivateDoc3(filePath, false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
            modelDoc2.Visible = false;

            return modelDoc2 as AssemblyDoc;
        }


        private static string GetPartNumber(string filePath)
        {
            string fileName = Path.GetFileNameWithoutExtension(filePath);
            return fileName.Substring(fileName.IndexOf("-") + 1);
        }


        public static bool[] SuppressFeatures(bool suppress, ModelDoc2 modelDoc2, params string[] features)
        {
            bool[] results = new bool[features.Length];

            for (int i = 0; i < features.Length; i++)
            {
                string featureName = features[i];
                bool isSelected = modelDoc2.Extension.SelectByID2(featureName, "BODYFEATURE", 0, 0, 0, false, 0, null, 0);

                if (!isSelected)
                {
                    results[i] = false;
                    continue;
                }

                var feature = modelDoc2.ISelectionManager.GetSelectedObject6(1, -1);
                bool isCurrentlySuppressed = ((Feature)feature).IsSuppressed();

                if (suppress && !isCurrentlySuppressed)
                {
                    results[i] = modelDoc2.EditSuppress2();
                }
                else if (!suppress && isCurrentlySuppressed)
                {
                    results[i] = modelDoc2.EditUnsuppress2();
                }
                else
                {
                    results[i] = true;
                }

                modelDoc2.ClearSelection2(true);
            }

            return results;
        }


        public static void HolePattern(double span, out double count, out double spacing, double maxSpacing = 24)
        {
            double noSpaces = Math.Ceiling(span / maxSpacing);
            count = noSpaces + 1;
            spacing = span / noSpaces;
        }

        public static void RemoveComponent(Component2 component2, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            string assemblyName = Path.GetFileNameWithoutExtension(modelDoc2.GetPathName());
            string componentName = component2.Name2;

            bool selected = modelDoc2.Extension.SelectByID2($"{componentName}@{assemblyName}", "COMPONENT", 0, 0, 0, true, 0, null, 0);

            bool deleted = modelDoc2.Extension.DeleteSelection2(0);
            if (deleted)
            {
                Debug.WriteLine($"Deleted component instance: {componentName}");
            }
            else throw new Exception();
        }


        public static List<string> ToBeDeleted = new List<string>();

        public static void ClearList_ToBeDeleted()
        {
            foreach (var filePath in ToBeDeleted)
            {
                if (filePath.Contains("JOBNO"))
                    continue;

                File.Delete(filePath);
                Debug.WriteLine($"Deleted file: {filePath}");
            }
            ToBeDeleted.Clear();
        }

        public static void CloseEverything()
        {
            SW.CloseAllDocuments(true);
        }







        public static void SortSheetsInDrawing()
        {
            DrawingDoc activeDrawing = SW.IActiveDoc2 as DrawingDoc;

            if (activeDrawing == null) return; // Exit if the active document is not a drawing.

            // 1. Retrieve all sheet names
            string[] allSheetNames = activeDrawing.GetSheetNames();

            // 2. Sort the sheet names with custom logic
            var sortedSheetNames = allSheetNames
                .OrderBy(sheet => sheet) // Preliminary sort to ensure consistency
                .Select(sheet => new
                {
                    OriginalName = sheet,
                    BaseName = GetBaseName(sheet),
                    IsFP = sheet.EndsWith("_FP")
                })
                .OrderBy(sheetInfo => sheetInfo.BaseName, new CustomStringComparer())
                .ThenBy(sheetInfo => sheetInfo.IsFP) // Ensures _FP versions follow their base sheet
                .Select(sheetInfo => sheetInfo.OriginalName)
                .ToArray();

            // 3. Use ReorderSheets to rearrange the sheets based on the sorted names
            activeDrawing.ReorderSheets(sortedSheetNames);

            // Activate the first sheet
            activeDrawing.ActivateSheet(sortedSheetNames[0]);
        }

        // Extracts the base name, excluding the "_FP" suffix if present
        private static string GetBaseName(string sheetName)
        {
            var baseName = sheetName.EndsWith("_FP") ? sheetName.Substring(0, sheetName.Length - 3) : sheetName;
            // Extract the portion after the last '-' which will be the sorting key
            var parts = baseName.Split('-');
            return parts.Length > 0 ? parts[parts.Length - 1] : baseName;
        }

        // Custom comparer for the complex sorting logic
        public class CustomStringComparer : IComparer<string>
        {
            public int Compare(string x, string y)
            {
                // Special handling for numeric values to ensure correct ordering
                bool xIsNumeric = int.TryParse(x, out int xVal);
                bool yIsNumeric = int.TryParse(y, out int yVal);

                if (xIsNumeric && yIsNumeric)
                {
                    return xVal.CompareTo(yVal);
                }
                if (xIsNumeric) return -1; // Numbers always come before letters
                if (yIsNumeric) return 1;  // Numbers always come before letters

                // Special case for single vs double letters (e.g., A vs AA)
                if (x.Length == 1 && y.Length == 1)
                {
                    return x.CompareTo(y); // Compare single letters directly
                }
                if (x.Length == 1 && y.Length > 1)
                {
                    return x[0] == y[0] ? -1 : x[0].CompareTo(y[0]); // Single letter comes before its repeats (A vs AA)
                }
                if (x.Length > 1 && y.Length == 1)
                {
                    return x[0] == y[0] ? 1 : x[0].CompareTo(y[0]); // Single letter comes before its repeats (A vs AA)
                }

                // Both are multiple-letter strings (e.g., AB vs CD); handle 'AA' comes after 'Z'
                if (x[0] == 'A' && y[0] != 'A') return 1;  // 'AA', 'AB', etc., come after 'Z'
                if (x[0] != 'A' && y[0] == 'A') return -1; // 'AA', 'AB', etc., come after 'Z'

                return string.Compare(x, y, StringComparison.Ordinal);
            }
        }


    }
}
