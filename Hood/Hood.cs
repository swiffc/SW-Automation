using EPDM.Interop.epdm;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using static Hood.AssemblyHelpers;
using static Hood.Trig;
using cTools = ModelTools.ReleaseCOM;
using mTools = Tools.ModelTools;
using fTools = FileTools.FileTools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar;
using System.Reflection;


namespace Hood
{
    internal class Hood : HoodData
    {
        public Hood()
        {
            var fTools = new FileTools.FileTools("Hood", Project, Bank, "3", Initials);
            bool bankExists;
            do
            {
                // Check if bank exists
                string desktopBankFile = $@"{DesktopFolderPath}\{Project}-3{Bank}.SLDASM";
                bankExists = File.Exists(desktopBankFile);

                if (bankExists)
                {
                    // Properties
                    SetStrength();
                    SetEndPanelProperties();
                    SetSidePanelProperties();
                    SetTopPanelProperties();
                    SetStiffenerProperties();

                    // Trig
                    FindEndPanelAngle();
                    FindSidePanelAngle();
                    DefineHoodInterior();
                    CalculateCornerAngle();

                    // Post-trig
                    AdjustEndStiffenerProperties();
                    AdjustSideStiffenerProperties();
                    AdjustEndPanelProperties();
                    AdjustSidePanelProperties();


                    // Create desktop instances of platform files with unique part numbers
                    //Hood.PartNo.Dynamic = fTools.CreateNew_ComponentFile($"{AssemblyNumber}{Bank}");
                    EndPanel.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("157");
                    SidePanel.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("182");
                    TopPanel.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("194");
                    CornerAngle.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("187");
                    Stiffener.End.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("188");
                    Stiffener.Side.PartNo.Dynamic = fTools.CreateNew_SubComponentFile("189");


                    // Modify Parts
                    ModifyEndPanel();
                    ModifySidePanel();
                    ModifyTopPanel();
                    ModifyCornerAngle();
                    ModifyEndStiffener();
                    ModifySideStiffener();
                    //AssemblyDoc hoodAssembly = mTools.OpenAssembly(Hood.FilePath);

                    // Assemble
                    AssembleHood();

                    // Drawing
                    string desktopDrawing = fTools.CreateDrawing("3");

                    fTools.ReplaceDrawingReference(EndPanel.PartNo.Dynamic, desktopDrawing, "157");
                    fTools.ReplaceDrawingReference(SidePanel.PartNo.Dynamic, desktopDrawing, "182");
                    fTools.ReplaceDrawingReference(TopPanel.PartNo.Dynamic, desktopDrawing, "194");
                    fTools.ReplaceDrawingReference(CornerAngle.PartNo.Dynamic, desktopDrawing, "187");
                    fTools.ReplaceDrawingReference(Stiffener.End.PartNo.Dynamic, desktopDrawing, "188");
                    fTools.ReplaceDrawingReference(Stiffener.Side.PartNo.Dynamic, desktopDrawing, "189");

                    DrawingDoc drawingDoc = fTools.OpenDrawing(desktopDrawing);

                    fTools.RenameSheet("157_EndPanel", EndPanel.PartNo.Dynamic, drawingDoc);
                    fTools.RenameSheet("157_FP", EndPanel.PartNo.Dynamic + " (FP)", drawingDoc);
                    fTools.RenameSheet("182_SidePanel", SidePanel.PartNo.Dynamic, drawingDoc);
                    fTools.RenameSheet("182_FP", SidePanel.PartNo.Dynamic + " (FP)", drawingDoc);
                    fTools.RenameSheet("187_CornerAngle", CornerAngle.PartNo.Dynamic, drawingDoc);
                    fTools.RenameSheet("187_FP", CornerAngle.PartNo.Dynamic + " (FP)", drawingDoc);
                    fTools.RenameSheet("188_EndStiffener", Stiffener.End.PartNo.Dynamic, drawingDoc);
                    fTools.RenameSheet("189_SideStiffener", Stiffener.Side.PartNo.Dynamic, drawingDoc);
                    fTools.RenameSheet("194_TopPanel", TopPanel.PartNo.Dynamic, drawingDoc);

                    mTools.OpenAssembly(Hood.FilePath);
                    (drawingDoc as ModelDoc2).Save3(1, 0, 0);

                    SaveEverything();

                    mTools.Close(desktopDrawing);
                }
                else
                {
                    Bank = fTools.AddNew_Bank();
                }

            } while (!bankExists);
            SW.ActivateDoc3(Hood.FilePath, false, 0, 0);
            mTools.Rebuild();
            SW.IActiveDoc2.ShowNamedView2("*Isometric", -1);
            SW.IActiveDoc2.ViewZoomtofit2();
        }
        public Hood(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            SelectionMgr selectionMgr = SW.IActiveDoc2.ISelectionManager;
            object selectedObj = selectionMgr.GetSelectedObject6(1, -1);

            // Get active doc where program is initiated
            string activeDocPath = SW.IActiveDoc2.GetPathName();

            string hood_Static = "3";
            string endPanel_Static = "157";
            string sidePanel_Static = "182";
            string cornerAngle_Static = "187";
            string endStiffener_Static = "188";
            string sideStiffener_Static = "189";
            string topPanel_Static = "194";

            // Dictionary mapping the static part numbers to their descriptions for debugging
            Dictionary<string, string> staticPartDescriptions = new Dictionary<string, string>
            {
                { hood_Static, "HOOD" },
                { endPanel_Static, "END-PANEL" },
                { sidePanel_Static, "SIDE-PANEL" },
                { cornerAngle_Static, "CORNER-PRL" },
                { endStiffener_Static, "STIFFENER_END" },
                { sideStiffener_Static, "STIFFENER_SIDE" },
                { topPanel_Static, "TOP-PANEL" }
            };

            // Itemized dictionary of platform components
            var hoodComponents = FilterComponents(selectedComponents, staticPartDescriptions);
            if (hoodComponents.Count == 0) { mTools.EnablePartUI(); return; }

            Hood.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, hood_Static) ?? null;
            EndPanel.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, endPanel_Static) ?? null;
            SidePanel.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, sidePanel_Static) ?? null;
            CornerAngle.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, cornerAngle_Static) ?? null;
            Stiffener.End.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, endStiffener_Static) ?? null;
            Stiffener.Side.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, sideStiffener_Static) ?? null;
            TopPanel.PartNo.Dynamic = GetDynamicPartNo(hoodComponents, topPanel_Static) ?? null;

            // Properties
            SetStrength();
            SetEndPanelProperties();
            SetSidePanelProperties();
            SetTopPanelProperties();
            SetStiffenerProperties();

            // Trig
            FindEndPanelAngle();
            FindSidePanelAngle();
            DefineHoodInterior();
            CalculateCornerAngle();

            // Post-trig
            AdjustEndStiffenerProperties();
            AdjustSideStiffenerProperties();
            AdjustEndPanelProperties();
            AdjustSidePanelProperties();

            // Only modify parts in the found in the dictionary
            var conditionsAndActions = new
                List<(Func<KeyValuePair<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>, bool> condition,
                      Action<Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>> action)>
            {
                (entry => entry.Value.StaticPartNo == endPanel_Static, ModifyEndPanel),
                (entry => entry.Value.StaticPartNo == sidePanel_Static, ModifySidePanel),
                (entry => entry.Value.StaticPartNo == cornerAngle_Static, ModifyCornerAngle),
                (entry => entry.Value.StaticPartNo == endStiffener_Static, ModifyEndStiffener),
                (entry => entry.Value.StaticPartNo == sideStiffener_Static, ModifySideStiffener),
                (entry => entry.Value.StaticPartNo == topPanel_Static, ModifyTopPanel)
            };
            foreach (var (condition, action) in conditionsAndActions)
            {
                if (hoodComponents.Any(condition))
                {
                    action(hoodComponents);
                }
            }

            // Only move selected parts in the assembly
            if (hoodComponents.Any(entry => entry.Value.StaticPartNo == hood_Static))
            {
                AssembleHood();

                if (activeDocPath != Hood.FilePath)
                {
                    Debug.WriteLine($"Original ActiveDoc: {activeDocPath}");
                    Debug.WriteLine($"Platform FilePath: {Hood.FilePath}");

                    mTools.Close(Hood.FilePath);
                }
            }
            else
            {
                mTools.Open(activeDocPath);
            }
            Debug.WriteLine("");
            mTools.Rebuild();

            if (selectedObj is Component2 comp)
            {
                SW.IActiveDoc2.Extension.SelectByID2(comp.Name2, "COMPONENT", 0, 0, 0, false, 0, null, 0);
            }
        }

        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            HoodUI hoodUI = new HoodUI();
            Application.Run(hoodUI);
        }

        // Set Pre-Trig Properties
        private void SetEndPanelProperties()
        {
            EndPanel.Height = Height - TopPanel.THK;
            EndPanel.TopLength = EndPanel.TopHole * 2 + SidePanel.FlangeLength - mTools.AssemblyClearance * 3;
            EndPanel.BottomLength = Width - SidePanel.FlangeLength - mTools.AssemblyClearance * 3;
            Console.WriteLine();
        }
        private void SetSidePanelProperties()
        {
            SidePanel.Height = Height - TopPanel.THK;
            SidePanel.TopLength = SidePanel.TopHole * 2 + EndPanel.FlangeLength - mTools.AssemblyClearance * 3;
            SidePanel.BottomLength = Length - EndPanel.FlangeLength - mTools.AssemblyClearance * 3;
        }
        private void SetTopPanelProperties()
        {
            TopPanel.Radius = FanDiameter / 2 + 0.25 + 0.125;
            TopPanel.Length = FanDiameter / 2 + SidePanel.FlangeLength - 0.125;

            double boltCircle = TopPanel.Radius + 1;
            double arcLength = FanHoles[fanDiameterInFeet].ArcLength;
            double angleRadians = arcLength / boltCircle;
            TopPanel.AngularSpacing = angleRadians * (180.0 / Math.PI);

            double holeToHoleDistance = (TopPanel.Length - 3.625 - 3);
            TopPanel.TopHoleQuantity = Math.Ceiling(holeToHoleDistance / 24);
            TopPanel.TopHoleSpacing = holeToHoleDistance / TopPanel.TopHoleQuantity;
        }
        private void SetStrength()
        {
            EndPanel.TopHole = FanDiameter / 2 + SidePanel.FlangeLength / 2;
            EndPanel.BottomHole = Length / 2;

            SidePanel.TopHole = FanDiameter / 2 + EndPanel.FlangeLength / 2;
            SidePanel.BottomHole = Width / 2;

            Point topEnd = new Point { x = 0, y = Height, z = EndPanel.TopHole + 1.5 };
            Point bottomEnd = new Point { x = 0, y = 0, z = EndPanel.BottomHole - 1.5 };
            EndPanel.WebHeight = CalculateDistance(topEnd, bottomEnd);
            Console.WriteLine(EndPanel.WebHeight);

            Point topSide = new Point { x = SidePanel.TopHole + 1.5, y = Height, z = 0 };
            Point bottomSide = new Point { x = SidePanel.BottomHole - 1.5, y = 0, z = 0 };
            SidePanel.WebHeight = CalculateDistance(topSide, bottomSide);

            Structural.SetPanelAndStiffenerStrength(Stiffener.End, EndPanel);
            Structural.SetPanelAndStiffenerStrength(Stiffener.Side, SidePanel);



            if (Stacks > 1)
            {
                TopPanel.THK = 0.1875;
                TopPanel.JDEnumber = "60015-HPC";
                TopPanel.Description = "PLATE_3/16\"_A572_50";
            }
            double maxValue = Math.Max(EndPanel.THK, Math.Max(SidePanel.THK, TopPanel.THK));
            if (EndPanel.THK == maxValue)
            {
                SidePanel.THK = EndPanel.THK;
                SidePanel.JDEnumber = EndPanel.JDEnumber;
                SidePanel.Description = EndPanel.Description;

                TopPanel.THK = EndPanel.THK;
                TopPanel.JDEnumber = EndPanel.JDEnumber;
                TopPanel.Description = EndPanel.Description;
            }
            else if (SidePanel.THK == maxValue)
            {
                EndPanel.THK = SidePanel.THK;
                EndPanel.JDEnumber = SidePanel.JDEnumber;
                EndPanel.Description = SidePanel.Description;

                TopPanel.THK = SidePanel.THK;
                TopPanel.JDEnumber = SidePanel.JDEnumber;
                TopPanel.Description = SidePanel.Description;
            }
            else
            {
                EndPanel.THK = TopPanel.THK;
                EndPanel.JDEnumber = TopPanel.JDEnumber;
                EndPanel.Description = TopPanel.Description;

                SidePanel.THK = TopPanel.THK;
                SidePanel.JDEnumber = TopPanel.JDEnumber;
                SidePanel.Description = TopPanel.Description;
            }


        }
        private void DefineHoodInterior()
        {
            // xFactor and zFactor use to determine hood interior
            if (EndPanel.SlopeAngle > 90)
            {
                double adjustedAngle = EndPanel.SlopeAngle - 90;
                mTools.AAS(adjustedAngle, out double height, out double length1, EndPanel.THK / 2);
                mTools.AAS(adjustedAngle, out double length2, height, out _);
                Hood.zFactor = length1 + length2;
            }
            else
            {
                Trig.AAS(true, EndPanel.SlopeAngle, EndPanel.THK / 2, out _, out double hypotenuse);
                Hood.zFactor = hypotenuse;
            }

            if (SidePanel.SlopeAngle > 90)
            {
                double adjustedAngle = SidePanel.SlopeAngle - 90;
                mTools.AAS(adjustedAngle, out double height, out double length1, SidePanel.THK / 2);
                mTools.AAS(adjustedAngle, out double length2, height, out _);
                Hood.xFactor = length1 + length2;
            }
            else
            {
                Trig.AAS(true, SidePanel.SlopeAngle, SidePanel.THK / 2, out _, out double hypotenuse);
                Hood.xFactor = hypotenuse;
            }

            Hood.InnerWidth.Bottom = Width / 2 - SidePanel.FlangeGage - Hood.xFactor;
            Hood.InnerLength.Bottom = Length / 2 - EndPanel.FlangeGage - Hood.zFactor;

            Hood.InnerWidth.Top = SidePanel.TopHole + SidePanel.FlangeGage - Hood.xFactor;
            Hood.InnerLength.Top = EndPanel.TopHole + EndPanel.FlangeGage - Hood.zFactor;
        }
        private void SetStiffenerProperties()
        {
            double endPanelLength = Math.Max(EndPanel.TopLength, EndPanel.BottomLength);
            Stiffener.End.Quantity = Math.Ceiling(endPanelLength / Stiffener.End.MaxSpacing);

            double sidePanelLength = Math.Max(SidePanel.TopLength, SidePanel.BottomLength);
            Stiffener.Side.Quantity = Math.Ceiling(sidePanelLength / Stiffener.Side.MaxSpacing);
        }


        // Trigonometry
        private void FindEndPanelAngle()
        {
            EndPanel.SlopeAngle = FindPanelAngle("End Panel", EndPanel);
        }
        private void FindSidePanelAngle()
        {
            SidePanel.SlopeAngle = FindPanelAngle("Side Panel", SidePanel);
        }
        private double FindPanelAngle(string panelName, IPanel panel)
        {
            double angle = CalculateAngleAndSides(panel);
            Debug.WriteLine
            (
                $"{panelName}" + "\n" +
                $"   angle = {angle}" + "\n"
            );

            return angle;
        }
        private double CalculateAngleAndSides(IPanel panel)
        {
            double angle;
            double a = panel.BottomHole - panel.TopHole - 2 * panel.FlangeGage;
            bool over90 = a <= 0;
            if (over90)
            {
                a = Math.Abs(a);
            }

            double b = panel.Height;
            double c = Math.Sqrt((a * a) + (b * b));
            angle = Math.Asin(b / c) * (180.0 / Math.PI);

            if (over90)
            {
                angle = 180 - angle;
            }

            return angle;
        }
        private void CalculateCornerAngle(bool drawSketch = false)
        {
            // Frustum of the hood interior
            SketchPoint[] btmRec = BottomRectangle(out Trig.Point r1, out Trig.Point r2, out Trig.Point r3, out Trig.Point r4, drawSketch);
            SketchPoint[] topRec = TopSquare(out Trig.Point s1, out _, out _, out Trig.Point s4, drawSketch);
            SketchSegment cornerLine1 = null;

            ///////////////////////////


            if (drawSketch)
            {
                cornerLine1 = mTools.ConnectPoints(false, false, btmRec[1], topRec[1]);
                mTools.ConnectPoints(false, false, btmRec[2], topRec[2]);
                mTools.ConnectPoints(false, false, btmRec[3], topRec[3]);
                mTools.ConnectPoints(false, false, btmRec[4], topRec[4]);
            }

            // Planar representations of hood panels
            Trig.Plane endPanelPlane = GetPlaneFromPoints(r1, r2, s1, "Plane157");
            Trig.Plane sidePanelPlane = GetPlaneFromPoints(r1, r3, s1, "Plane184");
            if (drawSketch)
            {
                mTools.CreatePlane("Plane157", btmRec[1], btmRec[2], topRec[1]);
                mTools.CreatePlane("Plane184", btmRec[1], btmRec[3], topRec[1]);
            }

            SetCornerAngleLength(r1, s1, drawSketch);


            //Mid plane between panels along seam
            Trig.Plane midPlane = GetMidPlane(endPanelPlane, sidePanelPlane, r1, Hood.S1);
            if (drawSketch)
            {
                Feature existingPlane1 = (SW.IActiveDoc2 as AssemblyDoc).FeatureByName(endPanelPlane.Name);
                Feature existingPlane2 = (SW.IActiveDoc2 as AssemblyDoc).FeatureByName(sidePanelPlane.Name);
                mTools.CreatePlane(existingPlane1, existingPlane2, true);

                cTools.Release(ref existingPlane1);
                cTools.Release(ref existingPlane2);
            }


            // Midplane intersection with bottom rectangle for corner angle alignment
            Hood.BtmRectangleIntersect = IntersectionOfLineWithPlane(r2, r4, midPlane, r2, r3);
            SketchPoint skR1 = null;
            if (drawSketch)
            {
                SketchPoint intersectionSketchPoint = mTools.CreatePoint(Hood.BtmRectangleIntersect.x, Hood.BtmRectangleIntersect.y, Hood.BtmRectangleIntersect.z);
                skR1 = mTools.CreatePoint(r1.x, r1.y, r1.z);
                cTools.Release(ref skR1);
                cTools.Release(ref intersectionSketchPoint);
            }


            // Rotate midplane 90 degrees and project bottom rectangle
            Trig.Plane perpendicularPlane = GetPerpendicularPlane(s1, r1);
            Point r2Projected = ProjectPointOntoPlane(r2, perpendicularPlane);
            Point r3Projected = ProjectPointOntoPlane(r3, perpendicularPlane);
            if (drawSketch)
            {
                //Feature midPlaneFeature = (SW.IActiveDoc2 as AssemblyDoc).FeatureByName(midPlane.Name);
                mTools.CreatePlane("PerpendicularPlane", btmRec[1], cornerLine1);
                SketchPoint P2 = mTools.CreatePoint(r2Projected.x, r2Projected.y, r2Projected.z);
                SketchPoint P3 = mTools.CreatePoint(r3Projected.x, r3Projected.y, r3Projected.z);
                SketchSegment projectedLine1 = mTools.ConnectPoints(false, true, btmRec[1], P2);
                SketchSegment projectedLine2 = mTools.ConnectPoints(false, true, btmRec[1], P3);
                mTools.AddDrivenDimension(projectedLine1, projectedLine2);

                cTools.Release(ref P2);
                cTools.Release(ref P3);
                cTools.Release(ref projectedLine1);
                cTools.Release(ref projectedLine2);
            }

            CornerAngle.Angle = ComputeAngleBetweenThreePoints(r1, r2Projected, r3Projected);

            foreach (var item in btmRec)
            {
                cTools.Release(item);
            }
            foreach (var item in topRec)
            {
                cTools.Release(item);
            }
            cTools.Release(ref cornerLine1);









        }
        private SketchPoint[] BottomRectangle(out Trig.Point r1, out Trig.Point r2, out Trig.Point r3, out Trig.Point r4, bool drawSketch = false)
        {
            double
                X = Hood.InnerWidth.Bottom,
                Y = 0,
                Z = Hood.InnerLength.Bottom;

            r1 = new Trig.Point(-X, Y, -Z);
            r2 = new Trig.Point(X, Y, -Z);
            r3 = new Trig.Point(-X, Y, Z);
            r4 = new Trig.Point(X, Y, Z);

            Hood.R1 = r1;

            SketchPoint R1, R2, R3, R4;
            R1 = R2 = R3 = R4 = null;
            if (drawSketch)
            {
                R1 = mTools.CreatePoint(r1.x, r1.y, r1.z);
                R2 = mTools.CreatePoint(r2.x, r2.y, r2.z);
                R3 = mTools.CreatePoint(r3.x, r3.y, r3.z);
                R4 = mTools.CreatePoint(r4.x, r4.y, r4.z);

                mTools.ConnectPoints(true, false, R1, R2, R4, R3);
            }

            return new SketchPoint[]
                {
                    null,
                    R1,
                    R2,
                    R3,
                    R4
                };
        }
        private SketchPoint[] TopSquare(out Trig.Point s1, out Trig.Point s2, out Trig.Point s3, out Trig.Point s4, bool drawSketch = false)
        {
            double
                x = SidePanel.TopHole + SidePanel.FlangeGage - Hood.xFactor,
                y = EndPanel.Height,
                z = EndPanel.TopHole + EndPanel.FlangeGage - Hood.zFactor;

            s1 = new Trig.Point(-x, y, -z);
            s2 = new Trig.Point(x, y, -z);
            s3 = new Trig.Point(-x, y, z);
            s4 = new Trig.Point(x, y, z);
            Hood.S1 = s1;

            SketchPoint T1, T2, T3, T4;
            T1 = T2 = T3 = T4 = null;
            if (drawSketch)
            {
                T1 = mTools.CreatePoint(s1.x, s1.y, s1.z);
                T2 = mTools.CreatePoint(s2.x, s2.y, s2.z);
                T3 = mTools.CreatePoint(s3.x, s3.y, s3.z);
                T4 = mTools.CreatePoint(s4.x, s4.y, s4.z);

                mTools.ConnectPoints(true, false, T1, T2, T4, T3);
            }

            return new SketchPoint[]
                {
                    null,
                    T1,
                    T2,
                    T3,
                    T4
                };
        }
        private void SetCornerAngleLength(Trig.Point r1, Trig.Point s1, bool drawSketch)
        {
            // Distance between two points
            double nominalLength =
                Math.Sqrt(Math.Pow(r1.x - s1.x, 2)
                + Math.Pow(r1.y - s1.y, 2)
                + Math.Pow(r1.z - s1.z, 2));

            // Local variables for length adjustments
            double topReduction1 = 0;
            double topReduction2 = 0;
            double bottomReduction = 0;

            // Conditionals for length adjustment
            if (EndPanel.SlopeAngle < 90)
            {
                Trig.Point p = new Trig.Point(s1.x, r1.y, r1.z);

                double angle1 = EndPanel.SlopeAngle;
                double angle2 = 90 - EndPanel.SlopeAngle;
                double angle3 = ComputeAngleBetweenThreePoints(s1, p, r1);

                mTools.AAS(angle1, EndPanel.THK, out double adjacentSide1, out double hypotenuse1);
                mTools.AAS(angle1, out double oppositeSide2, adjacentSide1, out _);
                mTools.AAS(angle2, out _, out double adjacentSide3, oppositeSide2);
                topReduction1 = hypotenuse1 + adjacentSide3;

                if (SidePanel.SlopeAngle >= 90)
                {
                    mTools.AAS(angle1, CornerAngle.Leg, out double adjacentSideA, out _);
                    bottomReduction = adjacentSideA;

                    mTools.AAS(angle3, out double oppositeSide4, CornerAngle.Leg, out _);
                    topReduction2 = oppositeSide4;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, bottomReduction - topReduction1 - topReduction2);
                }
                else if (SidePanel.SlopeAngle < 90)
                {
                    mTools.AAS(angle3, out double oppositeSide, CornerAngle.Leg, out _);
                    bottomReduction = oppositeSide;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, bottomReduction - topReduction1);
                }


            }
            else if (EndPanel.SlopeAngle > 90)
            {
                double angle1 = EndPanel.SlopeAngle - 90;

                if (SidePanel.SlopeAngle == 90)
                {
                    mTools.AAS(angle1, out double oppositeSide, CornerAngle.Leg, out _);
                    topReduction1 = oppositeSide;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, -topReduction1);
                }
                else if (SidePanel.SlopeAngle > 90)
                {
                    Trig.Point p;
                    if (EndPanel.SlopeAngle > SidePanel.SlopeAngle)
                    {
                        p = new Trig.Point(r1.x, r1.y, s1.z);
                    }
                    else
                    {
                        p = new Trig.Point(s1.x, r1.y, r1.z);
                    }
                    topReduction1 = 0;
                    double angle2 = ComputeAngleBetweenThreePoints(s1, p, r1);
                    mTools.AAS(angle2, out double oppositeSide, CornerAngle.Leg, out _);
                    topReduction2 = oppositeSide * 2;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, -topReduction2 - topReduction1);
                }
                else if (SidePanel.SlopeAngle < 90)
                {
                    Trig.Point p1 = new Trig.Point(s1.x, r1.y, r1.z);
                    double angle3 = ComputeAngleBetweenThreePoints(s1, p1, r1);
                    mTools.AAS(angle3, out double oppositeSide3, CornerAngle.Leg, out _);
                    bottomReduction = oppositeSide3;

                    Trig.Point p2 = new Trig.Point(r1.x, r1.y, s1.z);
                    double angle4 = ComputeAngleBetweenThreePoints(s1, p2, r1);
                    mTools.AAS(angle4, out _, mTools.BendTable[EndPanel.THK], out double hypotenuse);
                    topReduction1 = hypotenuse;

                    mTools.AAS(angle4, out double oppositeSide4, out _, CornerAngle.Leg);
                    topReduction2 = oppositeSide4;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, oppositeSide3 - topReduction1 - topReduction2);
                }
            }
            else if (EndPanel.SlopeAngle == 90)
            {
                if (SidePanel.SlopeAngle == 90)
                {
                    topReduction1 = EndPanel.THK + mTools.BendTable[EndPanel.THK];
                    bottomReduction = mTools.AssemblyClearance;
                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, -topReduction1 + bottomReduction);
                }
                else if (SidePanel.SlopeAngle < 90)
                {
                    mTools.AAS(SidePanel.SlopeAngle, out double oppositeSide1, out _, EndPanel.THK);
                    topReduction1 = oppositeSide1;

                    mTools.AAS(SidePanel.SlopeAngle, out double oppositeSide2, out _, mTools.BendTable[EndPanel.THK]);
                    topReduction2 = oppositeSide2;

                    mTools.AAS(SidePanel.SlopeAngle, CornerAngle.Leg, out double adjacentSide3, out _);
                    bottomReduction = adjacentSide3;

                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, -topReduction1 - topReduction2 + bottomReduction);
                }
                else if (SidePanel.SlopeAngle > 90)
                {
                    mTools.AAS(SidePanel.SlopeAngle - 90, out double oppositeSide, CornerAngle.Leg, out _);
                    topReduction1 = oppositeSide;
                    CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, -topReduction1);
                }
            }

            CornerAngle.CenterPoint = ShiftOffCenter(r1, s1, Shift);

            // Set length
            CornerAngle.Length = nominalLength - topReduction1 - bottomReduction - topReduction2 + Adjust;
        }


        // Set Post-Trig Properties
        private void AdjustEndPanelProperties()
        {
            double adjustedAngle = SidePanel.SlopeAngle;
            if (SidePanel.SlopeAngle > 90)
            {
                adjustedAngle -= 90;
            }
            Trig.AAS(true, adjustedAngle, SidePanel.THK / 2, out _, out double hypotenuse);

            if (adjustedAngle < 85 && adjustedAngle > 5)
            {
                EndPanel.BottomLength -= hypotenuse * 2;
                EndPanel.TopLength -= hypotenuse * 2;
            }

            EndPanel.BottomLength += .25;
            EndPanel.TopLength += .25;

            Console.WriteLine(  );
            // THK set in AdjustEndStiffenerProperties
        }
        private void AdjustSidePanelProperties()
        {
            double adjustedAngle = EndPanel.SlopeAngle;
            if (EndPanel.SlopeAngle > 90)
            {
                adjustedAngle -= 90;
            }
            Trig.AAS(true, adjustedAngle, EndPanel.THK / 2, out _, out double hypotenuse);

            if (adjustedAngle < 85 && adjustedAngle > 5)
            {
                SidePanel.BottomLength -= hypotenuse * 2;
                SidePanel.TopLength -= hypotenuse * 2;
            }

            SidePanel.BottomLength += .25;
            SidePanel.TopLength += .25;

            // THK set in AdjustSideStiffenerProperties
        }
        private void AdjustEndStiffenerProperties()
        {
            // Length
            double hypotenuse1 = CornerAngle.Length - Stiffener.End.HoleToEnd * 2;
            Point vert = new Point(Hood.R1.x, Hood.S1.y, Hood.S1.z);
            double angle = CalculateAngle(Hood.S1, Hood.R1, vert);
            mTools.AAS(angle, out _, out double adjacentSide, hypotenuse1);
            Stiffener.End.Length = adjacentSide + Stiffener.End.HoleToEnd * 2;


            // Lateral spacing
            double xSpan = Math.Abs(CornerAngle.CenterPoint.x * 2);
            double numberOfSpaces = Stiffener.End.Quantity + 1;
            Stiffener.End.Spacing = xSpan / numberOfSpaces;
            Stiffener.End.Xtranslation = xSpan / 2 - Stiffener.End.Spacing;


            // Center
            mTools.AAS(angle, out double oppositeSide2, out _, CornerAngle.HoleGage);
            if (SidePanel.SlopeAngle < 90)
            {
                oppositeSide2 *= -1;
            }
            Point topPoint = new Point
            {
                x = Stiffener.End.Xtranslation,
                y = EndPanel.Height,
                z = Hood.InnerLength.Top
            };
            Point bottomPoint = new Point
            {
                x = Stiffener.End.Xtranslation,
                y = 0,
                z = Hood.InnerLength.Bottom
            };
            Point midPoint = new Point
            {
                x = Stiffener.End.Xtranslation,
                y = CornerAngle.CenterPoint.y,
                z = -CornerAngle.CenterPoint.z
            };
            Stiffener.End.CenterPoint = ShiftOffCenter(midPoint, bottomPoint, topPoint, oppositeSide2);


            // Mid hole spacing
            mTools.AAS(angle, out _, out double adjacentSide3, CornerAngle.MidHoleSpacing);
            Stiffener.End.MidHoleSpacing = adjacentSide3;


        }
        private void AdjustSideStiffenerProperties()
        {
            // Length
            double hypotenuse1 = CornerAngle.Length - Stiffener.Side.HoleToEnd * 2;
            Point vert = new Point(Hood.S1.x, Hood.S1.y, Hood.R1.z);
            double angle = CalculateAngle(Hood.S1, Hood.R1, vert);
            mTools.AAS(angle, out _, out double adjacentSide, hypotenuse1);
            Stiffener.Side.Length = adjacentSide + Stiffener.Side.HoleToEnd * 2;


            // Lateral spacing
            double zSpan = Math.Abs(CornerAngle.CenterPoint.z * 2);
            double numberOfSpaces = Stiffener.Side.Quantity + 1;
            Stiffener.Side.Spacing = zSpan / numberOfSpaces;
            Stiffener.Side.Ztranslation = zSpan / 2 - Stiffener.Side.Spacing;


            // Center
            mTools.AAS(angle, out double oppositeSide2, out _, CornerAngle.HoleGage);
            if (EndPanel.SlopeAngle < 90)
            {
                oppositeSide2 *= -1;
            }
            Point topPoint = new Point
            {
                x = Hood.InnerWidth.Top,
                y = SidePanel.Height,
                z = Stiffener.Side.Ztranslation
            };
            Point bottomPoint = new Point
            {
                x = Hood.InnerWidth.Bottom,
                y = 0,
                z = Stiffener.Side.Ztranslation
            };
            Point midPoint = new Point
            {
                x = -CornerAngle.CenterPoint.x,
                y = CornerAngle.CenterPoint.y,
                z = Stiffener.Side.Ztranslation
            };
            Stiffener.Side.CenterPoint = ShiftOffCenter(midPoint, bottomPoint, topPoint, oppositeSide2);
        }

        // Modify
        private void ModifyEndPanel()
        {
            mTools.DisablePartUI();
            ModelDoc2 endPanel = mTools.Open(EndPanel.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("End Panel");

            // Job info
            fTools.SetProperty_Static("Project", Project, endPanel);
            fTools.SetProperty_Static("Bank", Bank, endPanel);
            fTools.SetProperty_Static("Customer", Customer, endPanel);
            fTools.SetProperty_Static("Client", Client, endPanel);
            fTools.SetProperty_Static("Location", Location, endPanel);
            fTools.SetProperty_Static("PO", PurchaseOrder, endPanel);
            fTools.SetProperty_Static("ItemNo", ItemNumber, endPanel);

            // JDE info
            fTools.SetProperty_Static("RMdesc", EndPanel.Description, endPanel);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + EndPanel.PartNo.Dynamic, endPanel);
            fTools.SetProperty_Static("RM", EndPanel.JDEnumber, endPanel);
            fTools.SetProperty_Static("Title", EndPanel.Title, endPanel);

            fTools.SetProperty_ConfigSpecific_Static("PartNo", "3" + Bank + "-" + EndPanel.PartNo.Dynamic + " (FP)", "FP", endPanel);
            fTools.SetProperty_ConfigSpecific_Static("Title", EndPanel.Title + "_FLAT", "FP", endPanel);

            mTools.EditDimension("TopLength", "sk:Panel", EndPanel.TopLength, endPanel);
            mTools.EditDimension("BottomLength", "sk:Panel", EndPanel.BottomLength, endPanel);
            mTools.EditDimension("Height", "sk:Panel", EndPanel.Height, endPanel);
            mTools.EditDimension("TopHole", "sk:Panel", EndPanel.TopHole, endPanel);
            mTools.EditDimension("BottomHole", "sk:Panel", EndPanel.BottomHole, endPanel);

            mTools.EditDimension("THK", "sk:Panel", EndPanel.THK, endPanel);
            mTools.EditDimension("THK", "Sheet-Metal", EndPanel.THK, endPanel);
            mTools.EditDimension("innerR", "sk:Panel", mTools.BendTable[EndPanel.THK], endPanel);
            mTools.EditDimension("innerR", "Sheet-Metal", mTools.BendTable[EndPanel.THK], endPanel);
            mTools.EditDimension("outerR", "sk:Panel", mTools.BendTable[EndPanel.THK] + EndPanel.THK, endPanel);

            mTools.EditDimension("Spacing", "TopHoles", TopPanel.TopHoleSpacing, endPanel);
            mTools.EditDimension("Quantity", "TopHoles", TopPanel.TopHoleQuantity + 1, endPanel);

            mTools.EditDimension("x", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.x), endPanel);
            mTools.EditDimension("y", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.y), endPanel);
            mTools.EditDimension("z", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.z), endPanel);
            mTools.EditDimension("Length", "sk:CornerHoles", CornerAngle.Length, endPanel);

            double span = EndPanel.BottomLength / 2 - 3 * 2;
            double maxSpacing = 24;
            double quantity = Math.Ceiling(span / maxSpacing) + 1;
            double spacing = span / (quantity - 1);
            if (span < maxSpacing)
            {
                spacing = span;
                quantity = 2;
            }
            mTools.EditDimension("Spacing", "sk:BottomHoles", spacing, endPanel);
            mTools.EditDimension("Quantity", "sk:BottomHoles", quantity, endPanel);

            mTools.EditDimension("Xtranslation", "sk:StiffenerHoles", Stiffener.End.Xtranslation, endPanel);
            mTools.EditDimension("Spacing", "StiffenerHolesPattern", Stiffener.End.Spacing, endPanel);
            mTools.EditDimension("Quantity", "StiffenerHolesPattern", Stiffener.End.Quantity, endPanel);

            cTools.Release(ref endPanel);
        }
        private void ModifyEndPanel(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifyEndPanel();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(EndPanel.FilePath);
            }
        }
        private void ModifySidePanel()
        {
            mTools.DisablePartUI();
            ModelDoc2 sidePanel = mTools.Open(SidePanel.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("Side Panel");

            // Job info
            fTools.SetProperty_Static("Project", Project, sidePanel);
            fTools.SetProperty_Static("Bank", Bank, sidePanel);
            fTools.SetProperty_Static("Customer", Customer, sidePanel);
            fTools.SetProperty_Static("Client", Client, sidePanel);
            fTools.SetProperty_Static("Location", Location, sidePanel);
            fTools.SetProperty_Static("PO", PurchaseOrder, sidePanel);
            fTools.SetProperty_Static("ItemNo", ItemNumber, sidePanel);

            // JDE info
            fTools.SetProperty_Static("RMdesc", SidePanel.Description, sidePanel);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + SidePanel.PartNo.Dynamic, sidePanel);
            fTools.SetProperty_Static("RM", SidePanel.JDEnumber, sidePanel);
            fTools.SetProperty_Static("Title", SidePanel.Title, sidePanel);

            fTools.SetProperty_ConfigSpecific_Static("PartNo", "3" + Bank + "-" + SidePanel.PartNo.Dynamic + " (FP)", "FP", sidePanel);
            fTools.SetProperty_ConfigSpecific_Static("Title", SidePanel.Title + "_FLAT", "FP", sidePanel);

            mTools.EditDimension("TopLength", "sk:Panel", SidePanel.TopLength, sidePanel);
            mTools.EditDimension("BottomLength", "sk:Panel", SidePanel.BottomLength, sidePanel);
            mTools.EditDimension("Height", "sk:Panel", SidePanel.Height, sidePanel);
            mTools.EditDimension("TopHole", "sk:Panel", FanDiameter / 2 + SidePanel.FlangeLength / 2, sidePanel);
            mTools.EditDimension("BottomHole", "sk:Panel", Width / 2, sidePanel);

            mTools.EditDimension("THK", "sk:Panel", SidePanel.THK, sidePanel);
            mTools.EditDimension("THK", "Sheet-Metal", SidePanel.THK, sidePanel);
            mTools.EditDimension("innerR", "sk:Panel", mTools.BendTable[SidePanel.THK], sidePanel);
            mTools.EditDimension("innerR", "Sheet-Metal", mTools.BendTable[SidePanel.THK], sidePanel);
            mTools.EditDimension("outerR", "sk:Panel", mTools.BendTable[SidePanel.THK] + SidePanel.THK, sidePanel);

            mTools.EditDimension("Spacing", "TopHoles", TopPanel.TopHoleSpacing, sidePanel);
            mTools.EditDimension("Quantity", "TopHoles", TopPanel.TopHoleQuantity + 1, sidePanel);

            mTools.EditDimension("x", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.x), sidePanel);
            mTools.EditDimension("y", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.y), sidePanel);
            mTools.EditDimension("z", "CornerAngleCenter", Math.Abs(CornerAngle.CenterPoint.z), sidePanel);
            mTools.EditDimension("Length", "sk:CornerHoles", CornerAngle.Length, sidePanel);

            double span = SidePanel.BottomLength / 2 - 3 * 2;
            double maxSpacing = 24;
            double quantity = Math.Ceiling(span / maxSpacing) + 1;
            double spacing = span / (quantity - 1);
            if (span < maxSpacing)
            {
                spacing = span;
                quantity = 2;
            }
            mTools.EditDimension("Spacing", "sk:BottomHoles", spacing, sidePanel);
            mTools.EditDimension("Quantity", "sk:BottomHoles", quantity, sidePanel);

            mTools.EditDimension("Ztranslation", "sk:StiffenerHoles", Stiffener.Side.Ztranslation, sidePanel);
            mTools.EditDimension("Spacing", "StiffenerHolesPattern", Stiffener.Side.Spacing, sidePanel);
            mTools.EditDimension("Quantity", "StiffenerHolesPattern", Stiffener.Side.Quantity, sidePanel);

            cTools.Release(ref sidePanel);
        }
        private void ModifySidePanel(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifySidePanel();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(SidePanel.FilePath);
            }
        }
        private void ModifyTopPanel()
        {
            mTools.DisablePartUI();
            ModelDoc2 topPanel = mTools.Open(TopPanel.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("Top Panel");

            // Job info
            fTools.SetProperty_Static("Project", Project, topPanel);
            fTools.SetProperty_Static("Bank", Bank, topPanel);
            fTools.SetProperty_Static("Customer", Customer, topPanel);
            fTools.SetProperty_Static("Client", Client, topPanel);
            fTools.SetProperty_Static("Location", Location, topPanel);
            fTools.SetProperty_Static("PO", PurchaseOrder, topPanel);
            fTools.SetProperty_Static("ItemNo", ItemNumber, topPanel);

            // JDE info
            fTools.SetProperty_Static("RMdesc", TopPanel.Description, topPanel);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + TopPanel.PartNo.Dynamic, topPanel);
            fTools.SetProperty_Static("RM", TopPanel.JDEnumber, topPanel);
            fTools.SetProperty_Static("Title", TopPanel.Title, topPanel);

            mTools.EditDimension("Length", "sk:Panel", TopPanel.Length, topPanel);
            mTools.EditDimension("Radius", "sk:Panel", TopPanel.Radius, topPanel);
            mTools.EditDimension("THK", "Panel", TopPanel.THK, topPanel);
            mTools.EditDimension("Spacing", "Holes", TopPanel.AngularSpacing, topPanel);
            mTools.EditDimension("Quantity", "Holes", FanHoles[fanDiameterInFeet].Quantity, topPanel);
            mTools.EditDimension("Spacing", "TopHoles1", TopPanel.TopHoleSpacing, topPanel);
            mTools.EditDimension("Quantity", "TopHoles1", TopPanel.TopHoleQuantity, topPanel);
            mTools.EditDimension("Spacing", "TopHoles2", TopPanel.TopHoleSpacing, topPanel);
            mTools.EditDimension("Quantity", "TopHoles2", TopPanel.TopHoleQuantity, topPanel);

            cTools.Release(ref topPanel);
        }
        private void ModifyTopPanel(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifyTopPanel();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(TopPanel.FilePath);
            }
        }
        private void ModifyCornerAngle()
        {
            mTools.DisablePartUI();
            ModelDoc2 cornerAngle = mTools.Open(CornerAngle.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("Corner Angle");

            // Job info
            fTools.SetProperty_Static("Project", Project, cornerAngle);
            fTools.SetProperty_Static("Bank", Bank, cornerAngle);
            fTools.SetProperty_Static("Customer", Customer, cornerAngle);
            fTools.SetProperty_Static("Client", Client, cornerAngle);
            fTools.SetProperty_Static("Location", Location, cornerAngle);
            fTools.SetProperty_Static("PO", PurchaseOrder, cornerAngle);
            fTools.SetProperty_Static("ItemNo", ItemNumber, cornerAngle);

            // JDE info
            fTools.SetProperty_Static("RMdesc", CornerAngle.Description, cornerAngle);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + CornerAngle.PartNo.Dynamic, cornerAngle);
            fTools.SetProperty_Static("RM", CornerAngle.JDEnumber, cornerAngle);
            fTools.SetProperty_Static("Title", CornerAngle.Title, cornerAngle);

            fTools.SetProperty_ConfigSpecific_Static("PartNo", "3" + Bank + "-" + CornerAngle.PartNo.Dynamic + " (FP)", "FP", cornerAngle);
            fTools.SetProperty_ConfigSpecific_Static("Title", CornerAngle.Title + "_FLAT", "FP", cornerAngle);

            mTools.EditDimension("Length", "PRL", CornerAngle.Length, cornerAngle);
            mTools.EditDimension("Angle", "sk:PRL", CornerAngle.Angle, cornerAngle);
            mTools.EditDimension("Leg", "sk:PRL", CornerAngle.Leg, cornerAngle);

            cTools.Release(ref cornerAngle);
        }
        private void ModifyCornerAngle(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifyCornerAngle();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(CornerAngle.FilePath);
            }
        }
        private void ModifyEndStiffener()
        {
            mTools.DisablePartUI();
            ModelDoc2 panelStiffener = mTools.Open(Stiffener.End.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("End Panel Stiffener");

            // Job info
            fTools.SetProperty_Static("Project", Project, panelStiffener);
            fTools.SetProperty_Static("Bank", Bank, panelStiffener);
            fTools.SetProperty_Static("Customer", Customer, panelStiffener);
            fTools.SetProperty_Static("Client", Client, panelStiffener);
            fTools.SetProperty_Static("Location", Location, panelStiffener);
            fTools.SetProperty_Static("PO", PurchaseOrder, panelStiffener);
            fTools.SetProperty_Static("ItemNo", ItemNumber, panelStiffener);

            // JDE info
            fTools.SetProperty_Static("RMdesc", Stiffener.End.Description, panelStiffener);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + Stiffener.End.PartNo.Dynamic, panelStiffener);
            fTools.SetProperty_Static("RM", Stiffener.End.JDEnumber, panelStiffener);
            fTools.SetProperty_Static("Title", Stiffener.End.Title, panelStiffener);

            mTools.EditDimension("Length", "L", Stiffener.End.Length, panelStiffener);
            mTools.EditDimension("Leg", "sk:L", Stiffener.End.Leg, panelStiffener);
            mTools.EditDimension("THK", "sk:L", Stiffener.End.THK, panelStiffener);
            mTools.EditDimension("K", "sk:L", Stiffener.End.R, panelStiffener);
            mTools.EditDimension("Gage", "sk:L", Stiffener.End.Gauge, panelStiffener);
            mTools.EditDimension("MidHoleSpacing", "sk:Holes", Stiffener.End.MidHoleSpacing, panelStiffener);

            cTools.Release(ref panelStiffener);
        }
        private void ModifyEndStiffener(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifyEndStiffener();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(Stiffener.End.FilePath);
            }
        }
        private void ModifySideStiffener()
        {
            mTools.DisablePartUI();
            ModelDoc2 panelStiffener = mTools.Open(Stiffener.Side.FilePath);
            mTools.EnablePartUI();
            Debug.WriteLine("Side Panel Stiffener");

            // Job info
            fTools.SetProperty_Static("Project", Project, panelStiffener);
            fTools.SetProperty_Static("Bank", Bank, panelStiffener);
            fTools.SetProperty_Static("Customer", Customer, panelStiffener);
            fTools.SetProperty_Static("Client", Client, panelStiffener);
            fTools.SetProperty_Static("Location", Location, panelStiffener);
            fTools.SetProperty_Static("PO", PurchaseOrder, panelStiffener);
            fTools.SetProperty_Static("ItemNo", ItemNumber, panelStiffener);

            // JDE info
            fTools.SetProperty_Static("RMdesc", Stiffener.Side.Description, panelStiffener);
            fTools.SetProperty_Static("PartNo", "3" + Bank + "-" + Stiffener.Side.PartNo.Dynamic, panelStiffener);
            fTools.SetProperty_Static("RM", Stiffener.Side.JDEnumber, panelStiffener);
            fTools.SetProperty_Static("Title", Stiffener.Side.Title, panelStiffener);

            mTools.EditDimension("Length", "L", Stiffener.Side.Length, panelStiffener);
            mTools.EditDimension("Leg", "sk:L", Stiffener.Side.Leg, panelStiffener);
            mTools.EditDimension("THK", "sk:L", Stiffener.Side.THK, panelStiffener);
            mTools.EditDimension("K", "sk:L", Stiffener.Side.R, panelStiffener);
            mTools.EditDimension("Gage", "sk:L", Stiffener.Side.Gauge, panelStiffener);

            cTools.Release(ref panelStiffener);
        }
        private void ModifySideStiffener(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            ModifySideStiffener();
            if (selectedComponents.Any(entry => entry.Value.FilePath == Hood.FilePath))
            {
                mTools.Close(Stiffener.Side.FilePath);
            }
        }


        // Assemble
        private void AssembleHood()
        {
            AssemblyDoc hoodAssembly = mTools.OpenAssembly(Hood.FilePath);

            // Record components with user defined locations
            List<Component2> unfixedComponents = ListUnfixedComponents(hoodAssembly);

            // Check assembly for existing component instances
            List<Component2> endPanelList = EndPanel.PartNo.Dynamic != null
                ? FindMatchingComponents(EndPanel.FilePath, hoodAssembly)
                : new List<Component2>();

            List<Component2> sidePanelList = SidePanel.PartNo.Dynamic != null
                ? FindMatchingComponents(SidePanel.FilePath, hoodAssembly)
                : new List<Component2>();

            List<Component2> topPanelList = TopPanel.PartNo.Dynamic != null
                ? FindMatchingComponents(TopPanel.FilePath, hoodAssembly)
                : new List<Component2>();

            List<Component2> cornerAngleList = CornerAngle.PartNo.Dynamic != null
                ? FindMatchingComponents(CornerAngle.FilePath, hoodAssembly)
                : new List<Component2>();

            List<Component2> endStiffenerList = Stiffener.End.PartNo.Dynamic != null
                ? FindMatchingComponents(Stiffener.End.FilePath, hoodAssembly)
                : new List<Component2>();

            List<Component2> sideStiffenerList = Stiffener.Side.PartNo.Dynamic != null
                ? FindMatchingComponents(Stiffener.Side.FilePath, hoodAssembly)
                : new List<Component2>();

            // Place components and close SOLIDWORKS file
            PlaceEndPanels(endPanelList, hoodAssembly);
            PlaceSidePanels(sidePanelList, hoodAssembly);
            PlaceTopPanels(topPanelList, hoodAssembly);
            PlaceCornerAngles(cornerAngleList, hoodAssembly);
            PlaceEndPanelStiffeners(endStiffenerList, hoodAssembly);
            PlaceSidePanelStiffeners(sideStiffenerList, hoodAssembly);
            PlaceStandardFanRingAndGuard(hoodAssembly);

            // Fix all components
            FixComponentLocations(hoodAssembly);

            // Float parts marked with a manually modified location
            UnfixComponentLocations(hoodAssembly, unfixedComponents);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            cTools.Release(ref hoodAssembly);
        }
        private void PlaceEndPanels(List<Component2> endPanelList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 frontPanel = GetInstance(endPanelList, 1); Debug.WriteLine(" (front end panel)");
            Component2 rearPanel = GetInstance(endPanelList, 2); Debug.WriteLine(" (rear end panel)");


            //---------------



            if (frontPanel == null && EndPanel.PartNo.Dynamic != null)
            {
                frontPanel = mTools.InsertComponent(EndPanel.FilePath, assemblyDoc); Debug.WriteLine(" (front end panel)");
                markedToClose = true;
            }


            if (frontPanel != null)
            {
                // Set location
                mTools.SetPosition(frontPanel);
                mTools.MakeTransParent(frontPanel);
            }

            // Release COM object
            cTools.Release(ref frontPanel);


            //---------------


            if (rearPanel == null && EndPanel.PartNo.Dynamic != null)
            {
                rearPanel = mTools.InsertComponent(EndPanel.FilePath, assemblyDoc); Debug.WriteLine(" (rear end panel)");
                markedToClose = true;
            }

            if (rearPanel != null)
            {
                // Set location
                mTools.Y_Rotation(180);
                mTools.SetPosition(rearPanel);
                mTools.MakeTransParent(rearPanel);
            }

            cTools.Release(ref rearPanel);


            if (markedToClose)
            {
                mTools.Close(EndPanel.FilePath);
            }


        }
        private void PlaceSidePanels(List<Component2> sidePanelList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 leftPanel = GetInstance(sidePanelList, 1); Debug.WriteLine(" (left side panel)");
            Component2 rightPanel = GetInstance(sidePanelList, 2); Debug.WriteLine(" (right side panel)");


            //---------------



            if (leftPanel == null && SidePanel.PartNo.Dynamic != null)
            {
                leftPanel = mTools.InsertComponent(SidePanel.FilePath, assemblyDoc); Debug.WriteLine(" (left side panel)");
                markedToClose = true;
            }


            if (leftPanel != null)
            {
                // Set location
                mTools.SetPosition(leftPanel);
                mTools.MakeTransParent(leftPanel);

            }
            cTools.Release(ref leftPanel);


            //---------------


            // If the model does not contain a left stringer, place one
            if (rightPanel == null && SidePanel.PartNo.Dynamic != null)
            {
                rightPanel = mTools.InsertComponent(SidePanel.FilePath, assemblyDoc); Debug.WriteLine(" (right side panel)");
                markedToClose = true;
            }

            if (rightPanel != null)
            {
                // Set location
                mTools.Y_Rotation(180);
                mTools.SetPosition(rightPanel);
                mTools.MakeTransParent(rightPanel);
            }

            cTools.Release(ref rightPanel);


            if (markedToClose)
            {
                mTools.Close(SidePanel.FilePath);
            }

            mTools.Close(SidePanel.FilePath);

        }
        private void PlaceTopPanels(List<Component2> topPanelList, AssemblyDoc assemblyDoc)
        {

            bool markedToClose = false;

            // Instances in stringerList
            Component2 panel1 = GetInstance(topPanelList, 1); Debug.WriteLine(" (top panel1)");
            Component2 panel2 = GetInstance(topPanelList, 2); Debug.WriteLine(" (top panel2)");
            Component2 panel3 = GetInstance(topPanelList, 3); Debug.WriteLine(" (top panel3)");
            Component2 panel4 = GetInstance(topPanelList, 4); Debug.WriteLine(" (top panel4)");


            //---------------



            if (panel1 == null && TopPanel.PartNo.Dynamic != null)
            {
                panel1 = mTools.InsertComponent(TopPanel.FilePath, assemblyDoc); Debug.WriteLine(" (top panel1)");
                markedToClose = true;
            }


            if (panel1 != null)
            {
                mTools.Y_Translation(EndPanel.Height);
                mTools.SetPosition(panel1);
                mTools.MakeTransParent(panel1);
            }


            //---------------



            if (panel2 == null && TopPanel.PartNo.Dynamic != null)
            {
                panel2 = mTools.InsertComponent(TopPanel.FilePath, assemblyDoc); Debug.WriteLine(" (top panel2)");
                markedToClose = true;
            }


            if (panel2 != null)
            {
                mTools.Y_Translation(EndPanel.Height);
                mTools.Y_Rotation(90);
                mTools.SetPosition(panel2);
                mTools.MakeTransParent(panel2);
            }



            //---------------



            if (panel3 == null && TopPanel.PartNo.Dynamic != null)
            {
                panel3 = mTools.InsertComponent(TopPanel.FilePath, assemblyDoc); Debug.WriteLine(" (top panel3)");
                markedToClose = true;
            }


            if (panel3 != null)
            {
                mTools.Y_Translation(EndPanel.Height);
                mTools.Y_Rotation(180);
                mTools.SetPosition(panel3);
                mTools.MakeTransParent(panel3);
            }



            //---------------



            if (panel4 == null && TopPanel.PartNo.Dynamic != null)
            {
                panel4 = mTools.InsertComponent(TopPanel.FilePath, assemblyDoc); Debug.WriteLine(" (top panel4)");
                markedToClose = true;
            }


            if (panel4 != null)
            {
                mTools.Y_Translation(EndPanel.Height);
                mTools.Y_Rotation(270);
                mTools.SetPosition(panel4);
                mTools.MakeTransParent(panel4);
            }



            //---------------


            cTools.Release(ref panel1);
            cTools.Release(ref panel2);
            cTools.Release(ref panel3);
            cTools.Release(ref panel4);


            if (markedToClose)
            {
                mTools.Close(TopPanel.FilePath);
            }



        }
        private void PlaceCornerAngles(List<Component2> cornerAngleList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;
            double[] matrix1 = new double[16];
            double[] matrix2 = new double[16];

            Component2 angle1 = GetInstance(cornerAngleList, 1); Debug.WriteLine(" (corner angle1)");
            Component2 angle2 = GetInstance(cornerAngleList, 2); Debug.WriteLine(" (corner angle2)");
            Component2 angle3 = GetInstance(cornerAngleList, 3); Debug.WriteLine(" (corner angle3)");
            Component2 angle4 = GetInstance(cornerAngleList, 4); Debug.WriteLine(" (corner angle4)");


            //---------------



            if (angle1 == null && CornerAngle.PartNo.Dynamic != null)
            {
                angle1 = mTools.InsertComponent(CornerAngle.FilePath, assemblyDoc); Debug.WriteLine(" (corner angle1)");
                markedToClose = true;
            }


            if (angle1 != null)
            {
                // Part vectors
                Vector3 partOrigin = new Vector3(0, 0, 0);
                mTools.AAS(EndPanel.SlopeAngle, EndPanel.Height, out _, out double hypotenuse);
                Vector3 partReferencePoint = new Vector3(0, (float)(hypotenuse / 2), 0);

                // X translation
                double adjustedSideSlope = SidePanel.SlopeAngle;
                float x;
                if (SidePanel.SlopeAngle > 90)
                {
                    adjustedSideSlope -= 90;
                    mTools.AAS(adjustedSideSlope, out double sidePanel_OppositeSide, SidePanel.Height / 2, out _);
                    if (SidePanel.SlopeAngle < 90) { sidePanel_OppositeSide *= -1; }
                    x = -(float)(SidePanel.BottomHole - SidePanel.FlangeGage + sidePanel_OppositeSide - Hood.xFactor);
                }
                else
                {
                    mTools.AAS(adjustedSideSlope, SidePanel.Height / 2, out double sidePanel_adjacentSide, out _);
                    x = -(float)(SidePanel.BottomHole - SidePanel.FlangeGage - sidePanel_adjacentSide - Hood.zFactor);
                }

                mTools.X_Translation(CornerAngle.CenterPoint.x);


                // Y translation
                float y = (float)EndPanel.Height / 2;
                mTools.Y_Translation(CornerAngle.CenterPoint.y);


                // Z translation
                double adjustedEndSlope = EndPanel.SlopeAngle;
                float z;
                if (EndPanel.SlopeAngle > 90)
                {
                    adjustedEndSlope -= 90;
                    mTools.AAS(adjustedEndSlope, out double endPanel_OppositeEnd, EndPanel.Height / 2, out _);
                    if (EndPanel.SlopeAngle < 90) { endPanel_OppositeEnd *= -1; }
                    z = -(float)(EndPanel.BottomHole - EndPanel.FlangeGage + endPanel_OppositeEnd - Hood.xFactor);
                }
                else
                {
                    mTools.AAS(adjustedEndSlope, EndPanel.Height / 2, out double endPanel_adjacentSide, out _);
                    z = -(float)(EndPanel.BottomHole - EndPanel.FlangeGage - endPanel_adjacentSide - Hood.zFactor);
                }

                mTools.Z_Translation(CornerAngle.CenterPoint.z);


                // Assembly Vectors
                Vector3 assemblyA = new Vector3(x, y, z);
                Vector3 assemblyB = new Vector3
                (
                    (float)Hood.S1.x,
                    (float)Hood.S1.y,
                    (float)Hood.S1.z
                );
                Vector3 btmRectangleIntersect = new Vector3
                     (
                     (float)Hood.BtmRectangleIntersect.x,
                     (float)Hood.BtmRectangleIntersect.y,
                     (float)Hood.BtmRectangleIntersect.z
                     );

                Matrix4x4 rotationMatrix = Trig.AlignPartToAssemblyPlane(partOrigin, partReferencePoint, assemblyA, assemblyB, btmRectangleIntersect);

                // Tilt around Y-axis by a specific angle
                float tiltAngleDegrees = (float)(90 - (CornerAngle.Angle / 2) + 180);
                float tiltAngleRadians = tiltAngleDegrees * (float)Math.PI / 180.0f;
                Matrix4x4 tiltMatrix = Matrix4x4.CreateRotationY(tiltAngleRadians);

                // Combine the rotation matrix with the tilt matrix
                Matrix4x4 finalMatrix = tiltMatrix * rotationMatrix;


                // Apply rotation
                if (EndPanel.SlopeAngle == 90 && SidePanel.SlopeAngle == 90)
                {
                    mTools.PositionMaxtrix[0] = 1;
                    mTools.PositionMaxtrix[1] = 0;
                    mTools.PositionMaxtrix[2] = 0;
                    mTools.PositionMaxtrix[3] = 0;
                    mTools.PositionMaxtrix[4] = 1;
                    mTools.PositionMaxtrix[5] = 0;
                    mTools.PositionMaxtrix[6] = 0;
                    mTools.PositionMaxtrix[7] = 0;
                    mTools.PositionMaxtrix[8] = 1;

                }
                else
                {
                    mTools.PositionMaxtrix[0] = finalMatrix.M11;
                    mTools.PositionMaxtrix[1] = finalMatrix.M12;
                    mTools.PositionMaxtrix[2] = finalMatrix.M13;
                    mTools.PositionMaxtrix[3] = finalMatrix.M21;
                    mTools.PositionMaxtrix[4] = finalMatrix.M22;
                    mTools.PositionMaxtrix[5] = finalMatrix.M23;
                    mTools.PositionMaxtrix[6] = finalMatrix.M31;
                    mTools.PositionMaxtrix[7] = finalMatrix.M32;
                    mTools.PositionMaxtrix[8] = finalMatrix.M33;
                }


                for (int i = 0; i < 16; i++)
                {
                    matrix1[i] = mTools.PositionMaxtrix[i];
                }

                mTools.SetPosition(angle1);
            }



            //---------------



            if (angle2 == null && CornerAngle.PartNo.Dynamic != null)
            {
                angle2 = mTools.InsertComponent(CornerAngle.FilePath, assemblyDoc); Debug.WriteLine(" (corner angle2)");
                markedToClose = true;
            }


            if (angle2 != null)
            {
                matrix2 = mTools.MirrorMatrix_YZplane(matrix1);
                for (int i = 0; i < 16; i++)
                {
                    mTools.PositionMaxtrix[i] = matrix2[i];
                }
                mTools.SetPosition(angle2);
            }



            //---------------



            if (angle3 == null && CornerAngle.PartNo.Dynamic != null)
            {
                angle3 = mTools.InsertComponent(CornerAngle.FilePath, assemblyDoc); Debug.WriteLine(" (corner angle3)");
                markedToClose = true;
            }


            if (angle3 != null)
            {
                double[] matrix3 = mTools.MirrorMatrix_XYplane(matrix1);
                for (int i = 0; i < 16; i++)
                {
                    mTools.PositionMaxtrix[i] = matrix3[i];
                }
                mTools.SetPosition(angle3);
            }



            //---------------



            if (angle4 == null && CornerAngle.PartNo.Dynamic != null)
            {
                angle4 = mTools.InsertComponent(CornerAngle.FilePath, assemblyDoc); Debug.WriteLine(" (corner angle4)"); ;
                markedToClose = true;
            }


            if (angle4 != null)
            {
                double[] matrix4 = mTools.MirrorMatrix_XYplane(matrix2);
                for (int i = 0; i < 16; i++)
                {
                    mTools.PositionMaxtrix[i] = matrix4[i];
                }
                mTools.SetPosition(angle4);
            }



            //---------------

            cTools.Release(ref angle1);
            cTools.Release(ref angle2);
            cTools.Release(ref angle3);
            cTools.Release(ref angle4);


            if (markedToClose)
            {
                mTools.Close(CornerAngle.FilePath);
            }


        }
        private void PlaceEndPanelStiffeners(List<Component2> endStiffenerList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;
            double x = Stiffener.End.Xtranslation;

            List<Component2> processedComponents = new List<Component2>();

            // Front
            for (int i = 0; i < Stiffener.End.Quantity; i++)
            {
                Component2 frontStiffener = GetInstance(endStiffenerList, i + 1);

                if (frontStiffener == null && Stiffener.End.PartNo.Dynamic != null)
                {
                    frontStiffener = mTools.InsertComponent(Stiffener.End.FilePath, assemblyDoc); Debug.WriteLine($" (front end stiffener{i + 1})");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (front end stiffener{i + 1})");
                    processedComponents.Add(frontStiffener);
                }

                if (frontStiffener != null)
                {
                    mTools.X_Translation(x);
                    x -= Stiffener.End.Spacing;
                    mTools.Y_Translation(Stiffener.End.CenterPoint.y);
                    mTools.Z_Translation(Stiffener.End.CenterPoint.z);
                    mTools.X_Axis_Rotate(-EndPanel.SlopeAngle - 180);
                    if (x < 0)
                    {
                        mTools.Z_Axis_Rotate(180);
                    }
                    mTools.SetPosition(frontStiffener);
                    cTools.Release(ref frontStiffener);
                }
            }



            // Rear
            x = -Stiffener.End.Xtranslation;
            for (int i = 0; i < Stiffener.End.Quantity; i++)
            {

                Component2 rearStiffener = GetInstance(endStiffenerList, i + 1 + (int)Stiffener.End.Quantity);

                if (rearStiffener == null && Stiffener.End.PartNo.Dynamic != null)
                {
                    rearStiffener = mTools.InsertComponent(Stiffener.End.FilePath, assemblyDoc); Debug.WriteLine($" (rear end stiffener{i + 1})");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (rear end stiffener{i + 1})");
                    processedComponents.Add(rearStiffener);
                }

                if (rearStiffener != null)
                {
                    mTools.X_Translation(x);
                    x += Stiffener.End.Spacing;
                    mTools.Y_Translation(Stiffener.End.CenterPoint.y);
                    mTools.Z_Translation(-Stiffener.End.CenterPoint.z);
                    mTools.X_Axis_Rotate(EndPanel.SlopeAngle - 180);
                    if (x > 0)
                    {
                        mTools.Rotate(180 - EndPanel.SlopeAngle + 90, 0, 180);
                    }
                    mTools.SetPosition(rearStiffener);
                    cTools.Release(ref rearStiffener);
                }

            }

            // Remove items in List that are not in processedComponents
            foreach (var component in endStiffenerList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Hood.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                mTools.Close(Stiffener.End.FilePath);
            }
        }
        private void PlaceSidePanelStiffeners(List<Component2> sideStiffenerList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;
            double z = -Stiffener.Side.Ztranslation;

            List<Component2> processedComponents = new List<Component2>();

            // Left
            for (int i = 0; i < Stiffener.Side.Quantity; i++)
            {

                Component2 leftStiffener = GetInstance(sideStiffenerList, i + 1);

                if (leftStiffener == null && Stiffener.Side.PartNo.Dynamic != null)
                {
                    leftStiffener = mTools.InsertComponent(Stiffener.Side.FilePath, assemblyDoc); Debug.WriteLine($" (left side stiffener{i + 1})");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (left side stiffener{i + 1})");
                    processedComponents.Add(leftStiffener);
                }

                if (leftStiffener != null)
                {
                    mTools.X_Translation(-Stiffener.Side.CenterPoint.x);
                    mTools.Y_Translation(Stiffener.Side.CenterPoint.y);
                    mTools.Z_Translation(z);
                    z += Stiffener.Side.Spacing;
                    if (z < 0)
                    {
                        mTools.Rotate(0, 90, -(SidePanel.SlopeAngle - 90));
                    }
                    else
                    {
                        mTools.Rotate(180, -90, -(SidePanel.SlopeAngle - 90));
                    }
                    mTools.SetPosition(leftStiffener);
                    cTools.Release(ref leftStiffener);
                }

            }


            // Right
            z = -Stiffener.Side.Ztranslation;

            for (int i = 0; i < Stiffener.Side.Quantity; i++)
            {

                Component2 rightStiffener = GetInstance(sideStiffenerList, i + 1 + (int)Stiffener.Side.Quantity);

                if (rightStiffener == null && Stiffener.Side.PartNo.Dynamic != null)
                {
                    rightStiffener = mTools.InsertComponent(Stiffener.Side.FilePath, assemblyDoc); Debug.WriteLine($" ");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (right side stiffener{i + 1})");
                    processedComponents.Add(rightStiffener);
                }

                if (rightStiffener != null)
                {
                    mTools.X_Translation(Stiffener.Side.CenterPoint.x);
                    mTools.Y_Translation(Stiffener.Side.CenterPoint.y);
                    mTools.Z_Translation(z);
                    z += Stiffener.Side.Spacing;
                    if (z < 0)
                    {
                        mTools.Rotate(180, -90, (SidePanel.SlopeAngle + 90));
                    }
                    else
                    {
                        mTools.Rotate(0, 90, (SidePanel.SlopeAngle + 90));
                    }
                    mTools.SetPosition(rightStiffener);
                    cTools.Release(ref rightStiffener);
                }

            }


            // Remove items in List that are not in processedComponents
            foreach (var component in sideStiffenerList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Hood.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                mTools.Close(Stiffener.Side.FilePath);
            }
        }
        private void PlaceStandardFanRingAndGuard(AssemblyDoc hoodAssembly)
        {
            // Local variables
            string ringString = null;
            string guardString = null;

            // Dictionary of supported fan rings
            var StandardParts_Dictionary = new Dictionary<Tuple<double, int>, StandardParts>
            {
                //             dia  depth                    ring     guard
                { Tuple.Create(5.0,  24), new StandardParts("03R25", "03G05") },
                { Tuple.Create(6.0,  24), new StandardParts("03R26", "03G06") },
                { Tuple.Create(7.0,  24), new StandardParts("03R27", "03G07") },
                { Tuple.Create(8.0,  24), new StandardParts("03R28", "03G08") },
                { Tuple.Create(9.0,  24), new StandardParts("03R29", "03G09") },
                { Tuple.Create(10.0, 24), new StandardParts("03R30", "03G10") },
                { Tuple.Create(11.0, 24), new StandardParts("03R31", "03G11") },
                { Tuple.Create(12.0, 24), new StandardParts("03R32", "03G12") },
                { Tuple.Create(13.0, 24), new StandardParts("03R33", "03G13") },
                { Tuple.Create(14.0, 24), new StandardParts("03R34", "03G14") },
                { Tuple.Create(15.0, 24), new StandardParts("03R35", "03G15") },
                { Tuple.Create(16.0, 24), new StandardParts("03R36", "03G16") },


                { Tuple.Create(5.0,  30), new StandardParts("02R25", "03G05") },
                { Tuple.Create(6.0,  30), new StandardParts("02R26", "03G06") },
                { Tuple.Create(7.0,  30), new StandardParts("02R27", "03G07") },
                { Tuple.Create(8.0,  30), new StandardParts("02R28", "03G08") },
                { Tuple.Create(9.0,  30), new StandardParts("02R29", "03G09") },
                { Tuple.Create(10.0, 30), new StandardParts("02R30", "03G10") },
                { Tuple.Create(11.0, 30), new StandardParts("02R31", "03G11") },
                { Tuple.Create(12.0, 30), new StandardParts("02R32", "03G12") },
                { Tuple.Create(13.0, 30), new StandardParts("02R33", "03G13") },
                { Tuple.Create(14.0, 30), new StandardParts("02R34", "03G14") },
                { Tuple.Create(15.0, 30), new StandardParts("02R35", "03G15") },
                { Tuple.Create(16.0, 30), new StandardParts("02R36", "03G16") }
            };

            Debug.WriteLine("\n");
            // Check the dictionary for a suitable fan ring and guard
            Tuple<double, int> fanKey = Tuple.Create(fanDiameterInFeet, Ring.Depth);
            StandardParts standardParts;
            if (StandardParts_Dictionary.TryGetValue(fanKey, out standardParts))
            {
                ringString = standardParts.RingString; Debug.WriteLine($"Standard Ring found [{ringString}]");
                guardString = standardParts.GuardString; Debug.WriteLine($"Standard Guard found [{guardString}]");
            }
            else
            {
                MessageBox.Show($"No standard part exists for fan diameter [{fanDiameterInFeet}ft] and depth [{Ring.Depth}in]");
            }

            // Create a list of all the standard parts that aren't needed
            var StandardParts_List = StandardParts_Dictionary.Values.ToList();
            var partsToRemove = StandardParts_List.Where(p => p.RingString == ringString || p.GuardString == guardString).ToList();
            foreach (var part in partsToRemove)
            {
                StandardParts_List.Remove(part);
            }

            object[] componentObjArray = hoodAssembly.GetComponents(true);
            List<Component2> ringComponents = new List<Component2>();
            Component2 guardComponent = null;

            foreach (var componentObj in componentObjArray)
            {
                Component2 component2 = componentObj as Component2;
                string compName = component2.Name2;

                // If unneeded standard ring or guard, delete component
                if (StandardParts_List.Any(part => compName.StartsWith(part.RingString) || compName.StartsWith(part.GuardString)))
                {
                    mTools.DeleteComponentByName(compName, hoodAssembly);

                    Marshal.ReleaseComObject(component2);
                    component2 = null;
                }

                // If needed ring, add to list
                else if (compName.StartsWith(ringString))
                {
                    ringComponents.Add(component2);
                }

                // If needed guard capture component state
                else if (compName.StartsWith(guardString))
                {
                    guardComponent = component2;
                }

                // Release component2 instance of non-ring/guard part
                else
                {
                    Marshal.ReleaseComObject(component2);
                    component2 = null;
                }
            }

            // sort list by instance found in component name
            ringComponents.Sort((x, y) => string.Compare(x.Name2, y.Name2));
            Debug.WriteLine($"Initial ring list count: {ringComponents.Count}");

            // clear excess instance if any
            if (ringComponents.Count > Stacks)
            {
                // Collect components to delete
                List<Component2> componentsToDelete = new List<Component2>();
                for (int i = Stacks; i < ringComponents.Count; i++)
                {
                    componentsToDelete.Add(ringComponents[i]);
                }

                // Delete and release components
                foreach (var component in componentsToDelete)
                {
                    if (component != null)
                    {
                        mTools.DeleteComponentByName(component.Name2, hoodAssembly);
                        Debug.WriteLine("Removed one ring from the list and assembly");

                        // Release the COM object
                        Marshal.ReleaseComObject(component);
                    }
                }

                // Clear the componentsToDelete list
                componentsToDelete.Clear();
            }
            else if (ringComponents.Count < Stacks)
            {
                string ringFolderPath = @"C:\AXC_VAULT\SolidWorks Common Data\Design Library\Hudson Library\Fan Rings";
                string ringFileName = ringString + ".SLDPRT";
                string ringFilePath = Path.Combine(ringFolderPath, ringFileName);

                if (File.Exists(ringFilePath))
                {
                    Debug.WriteLine("Connecting to vault...");
                    EdmVault5 vault = new EdmVault5();
                    vault.LoginAuto("AXC_VAULT", 0);
                    IEdmFolder5 folder = vault.GetFolderFromPath(ringFolderPath);
                    IEdmFile5 file = folder.GetFile(ringString + ".SLDPRT");
                    file.GetFileCopy(0);
                    Debug.WriteLine("Connected");

                    mTools.DisablePartUI();
                    mTools.Open(ringFilePath);
                    mTools.EnablePartUI();

                    SW.ActivateDoc((hoodAssembly as ModelDoc2).GetPathName());

                    cTools.Release(ref file);
                    cTools.Release(ref folder);
                    cTools.Release(ref vault);

                }
                else
                {
                    MessageBox.Show($"Could not find {ringFilePath}");
                }

                int j = ringComponents.Count;
                for (int i = j; i < Stacks; i++)
                {
                    Component2 component2 = mTools.InsertComponent(ringFilePath, hoodAssembly);
                    ringComponents.Add(component2);
                    Debug.WriteLine("Added one ring to the list and assembly");
                }

                mTools.Close(ringFilePath);

            }

            // locate parts
            for (int i = 0; i < Stacks; i++)
            {
                Component2 ringComponent = ringComponents[i];
                mTools.Y_Translation(EndPanel.Height + TopPanel.THK + Ring.Depth * i);
                mTools.SetPosition(ringComponent);
            }

            // locate guard
            if (guardString != null)
            {
                if (guardComponent == null)
                {
                    string guardFolderPath = @"C:\AXC_VAULT\SolidWorks Common Data\Design Library\Hudson Library\Fan Guards";
                    string guardFileName = guardString + ".SLDPRT";
                    string guardFilePath = Path.Combine(guardFolderPath, guardFileName);

                    if (File.Exists(guardFilePath))
                    {
                        Debug.WriteLine("Connecting to vault...");
                        EdmVault5 vault = new EdmVault5();
                        vault.LoginAuto("AXC_VAULT", 0);
                        IEdmFolder5 folder = vault.GetFolderFromPath(guardFolderPath);
                        IEdmFile5 file = folder.GetFile(guardString + ".SLDPRT");
                        file.GetFileCopy(0);
                        Debug.WriteLine("Connected");

                        mTools.DisablePartUI();
                        mTools.Open(guardFilePath);
                        mTools.EnablePartUI();

                        SW.ActivateDoc((hoodAssembly as ModelDoc2).GetPathName());

                        cTools.Release(ref file);
                        cTools.Release(ref folder);
                        cTools.Release(ref vault);

                    }
                    else
                    {
                        MessageBox.Show($"Could not find {guardFilePath}");
                    }

                    guardComponent = mTools.InsertComponent(guardFilePath, hoodAssembly);
                    Debug.WriteLine("Added fan guard to the assembly");

                    mTools.Close(guardFilePath);
                }

                mTools.Y_Translation(EndPanel.Height + TopPanel.THK + Ring.Depth * Stacks);
                mTools.SetPosition(guardComponent);

                cTools.Release(ref guardComponent);
            }

            for (int i = ringComponents.Count - 1; i >= 0; i--)
            {
                var component = ringComponents[i];
                cTools.Release(ref component);
                ringComponents.RemoveAt(i);
            }
        }



        // Edit
        private static Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> FilterComponents(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, Dictionary<string, string> staticPartDescriptions)
        {
            // Dictionary to add eligible components to
            var filteredComponents = new Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>();

            // Search itemized components
            var keys = selectedComponents.Keys.ToList();
            Debug.WriteLine("\n" + "Itemized list");
            for (int i = 0; i < keys.Count; i++)
            {
                var key = keys[i];
                var value = selectedComponents[key];

                // Use the dictionary to look up the description
                if (staticPartDescriptions.TryGetValue(value.StaticPartNo, out var description))
                {
                    filteredComponents.Add(key, value);
                    Debug.WriteLine($"  Item({i + 1}/{keys.Count}) {description} -->  {Path.GetFileNameWithoutExtension(key.GetPathName())} : {value.StaticPartNo} : {value.DynamicPartNo} : {value.Bank}");
                }
                else
                {
                    Debug.WriteLine($"  Item({i + 1}/{keys.Count}) discarded");
                }
                Debug.WriteLine("");
            }

            // Handle null tuple
            if (filteredComponents.Count == 0)
            {
                MessageBox.Show("No valid selections made.", "Component Selector", MessageBoxButtons.OK, MessageBoxIcon.Error);
                Debug.WriteLine("No valid selections made.");
                //System.Environment.Exit(0);
            }

            return filteredComponents;
        }
        private static string GetDynamicPartNo(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> components, params string[] staticPartNos)
        {
            return components
                .Where(entry => staticPartNos.Contains(entry.Value.StaticPartNo))
                .Select(entry => entry.Value.DynamicPartNo)
                .FirstOrDefault();
        }
        private static List<Component2> FindMatchingComponents(string filePath_OfTarget, AssemblyDoc assemblyDoc_OfSeachLocation)
        {
            // List to store matching components
            List<Component2> matchingComponents = new List<Component2>();

            // Extract the file name from the file path
            string fileName = GetFileNameFromPath(filePath_OfTarget);

            // Get all components in the assembly
            object[] allComponentsArray = assemblyDoc_OfSeachLocation.GetComponents(false);

            // Iterate through all components
            if (allComponentsArray != null)
            {
                foreach (var componentObj in allComponentsArray)
                {
                    Component2 component = componentObj as Component2;
                    string componentName = component.Name2;

                    // Check if the component name contains the file name and does not contain "/"
                    if (componentName.Contains(fileName) && !componentName.Contains("/"))
                    {
                        matchingComponents.Add(component);
                    }
                }
            }

            return matchingComponents;
        }
        private static string GetFileNameFromPath(string filePath)
        {
            string[] fileNameParts = Path.GetFileNameWithoutExtension(filePath).Split('.');
            return fileNameParts[0];
        }
        private static List<Component2> ListUnfixedComponents(AssemblyDoc assemblyDoc)
        {
            List<Component2> unfixedComponents = new List<Component2>();

            int componentCount = assemblyDoc.GetComponentCount(true);
            object[] components = (object[])assemblyDoc.GetComponents(true);
            for (int i = 0; i < componentCount; i++)
            {
                var component = (Component2)components[i];

                object[] mates = component.GetMates();

                if (component != null && component.IsFixed() == false && mates != null)
                {
                    unfixedComponents.Add(component);
                }
            }
            return unfixedComponents;
        }
        private static void FixComponentLocations(AssemblyDoc assemblyDoc)
        {
            int componentCount = assemblyDoc.GetComponentCount(true);
            object[] components = (object[])assemblyDoc.GetComponents(true);
            for (int i = 0; i < componentCount; i++)
            {
                var component = (Component2)components[i];
                if (component != null && !component.IsFixed())
                {
                    bool selected = component.Select2(true, -1);
                    if (selected)
                    {
                        (assemblyDoc as ModelDoc2).Extension.SelectAll();
                        assemblyDoc.FixComponent();
                        return;
                    }
                }
            }
        }
        private static void UnfixComponentLocations(AssemblyDoc assemblyDoc, List<Component2> unfixedComponents)
        {
            foreach (var component in unfixedComponents)
            {
                component.Select2(true, -1);
                assemblyDoc.UnfixComponent();
            }
        }
        private static void SaveEverything()
        {
            Debug.WriteLine("Saving...");
            object[] componentObj = (SW.IActiveDoc2 as AssemblyDoc).GetComponents(false);

            foreach (var obj in componentObj)
            {
                Component2 component2 = obj as Component2;
                ModelDoc2 modelDoc2 = component2.GetModelDoc2();
                modelDoc2.Save3(1, 0, 0);
                cTools.Release(ref modelDoc2);
            }

            SW.IActiveDoc2.Save3(1, 0, 0);
            Debug.WriteLine("...Save complete");
        }
        private static Component2 GetInstance(List<Component2> componentList, int instance)
        {
            if (instance < 1 || instance > componentList.Count)
            {
                // Invalid instance number, return null
                return null;
            }

            // Return the component at the specified instance (0-based index)
            Component2 swComponent = componentList[instance - 1];
            Debug.Write($"      Existing component found: {swComponent.Name2}");
            return swComponent;
        }

    }
}
