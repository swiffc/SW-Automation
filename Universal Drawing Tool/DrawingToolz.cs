using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Runtime.Remoting.Metadata.W3cXsd2001;
using System.Text.RegularExpressions;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.ToolTip;

namespace DrawingToolz
{
    public static class DrawingToolz
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        //---------------------------------------------------------------------------------------

        //---------------------------------------------------------------------------------------
        static void Main()
        {
            PositionAndScale();
        }




        // Buttons
        public static void ActivateNextSheet()
        {
            // Get the current active sheet
            Sheet activeSheet = (SW.IActiveDoc2 as DrawingDoc).GetCurrentSheet() as Sheet;

            // Get the name of the current active sheet
            string activeSheetName = activeSheet.GetName();

            // Get the names of all sheets in the drawing
            string[] allSheetNames = (SW.IActiveDoc2 as DrawingDoc).GetSheetNames() as string[];

            if (allSheetNames != null && allSheetNames.Length > 1)
            {
                // Find the index of the current active sheet
                int currentSheetIndex = Array.IndexOf(allSheetNames, activeSheetName);

                // Determine the index of the next sheet
                int nextSheetIndex = (currentSheetIndex + 1) % allSheetNames.Length;

                // Activate the next sheet
                (SW.IActiveDoc2 as DrawingDoc).ActivateSheet(allSheetNames[nextSheetIndex]);

                SW.IActiveDoc2.ViewZoomtofit2();

                CollapseDrawingTree();
            }
        }
        public static void ActivatePreviousSheet()
        {
            // Get the current active sheet
            Sheet activeSheet = (SW.IActiveDoc2 as DrawingDoc).GetCurrentSheet() as Sheet;

            // Get the name of the current active sheet
            string activeSheetName = activeSheet.GetName();

            // Get the names of all sheets in the drawing
            string[] allSheetNames = (SW.IActiveDoc2 as DrawingDoc).GetSheetNames() as string[];

            if (allSheetNames != null && allSheetNames.Length > 1)
            {
                // Find the index of the current active sheet
                int currentSheetIndex = Array.IndexOf(allSheetNames, activeSheetName);

                // Determine the index of the previous sheet
                int previousSheetIndex = currentSheetIndex - 1;

                // Handle wrapping around from the first sheet to the last
                if (previousSheetIndex < 0)
                    previousSheetIndex = allSheetNames.Length - 1;

                // Activate the previous sheet
                (SW.IActiveDoc2 as DrawingDoc).ActivateSheet(allSheetNames[previousSheetIndex]);

                SW.IActiveDoc2.ViewZoomtofit2();

                CollapseDrawingTree();
            }
        }
        public static void PositionAndScale()
        {
            DrawingDoc drawingDoc = GetDrawing();
            Sheet sheet = drawingDoc.GetCurrentSheet();
            PositionAndScale(sheet, drawingDoc);
        }
        public static void AutoBalloon()
        {
            if (HasBom())
            {
                DrawingDoc drawingDoc = GetDrawing();
                Sheet sheet = GetSheet();
                AssignViews(sheet, out View isoView, out View frontView, out View rightView, out View topView, out View bottomView, out View leftView);
                AutoBalloon(isoView, drawingDoc);
            }
        }
        public static void AlignDimensions()
        {
            Sheet sheet = GetSheet();
            AlignDimensions(sheet);
        }
        public static void DeleteDanglingAnnotations()
        {
            Sheet sheet = GetSheet();
            DeleteDanglingAnnotations(sheet);
        }
        public static void DrawingFileCleaner()
        {
            Sheet[] sheets = GetSheets();

            foreach (var sheet in sheets)
            {
                Debug.WriteLine($"Adjusting {sheet.GetName()}");
                ActivateSheet(sheet);
                AutoBalloon();
                PositionAndScale();
                DeleteDanglingAnnotations();
                AlignDimensions();
                ZoomToFit();
            }

            //Rebuild();
        }
        public static void SheetCleaner()
        {
            AutoBalloon();
            PositionAndScale();
            DeleteDanglingAnnotations();
            AlignDimensions();
            ZoomToFit();

            //Rebuild();
        }
        public static void SortSheetsInDrawing()
        {
            DrawingDoc drawingDoc = SW.IActiveDoc2 as DrawingDoc;

            SortSheetsInDrawing(drawingDoc);
        }
        


        // Helper Methods
        private static Sheet GetSheet()
        {
            return ((DrawingDoc)SW.IActiveDoc2).GetCurrentSheet();
        }
        private static Sheet[] GetSheets()
        {
            DrawingDoc drawingDoc = GetDrawing();
            string[] sheetNames = drawingDoc.GetSheetNames();
            Sheet[] sheets = new Sheet[sheetNames.Length];

            for (int i = 0; i < sheetNames.Length; i++)
            {
                sheets[i] = drawingDoc.Sheet[sheetNames[i]];
            }
            return sheets;
        }
        private static void ActivateSheet(Sheet sheet)
        {
            DrawingDoc drawingDoc = GetDrawing();
            drawingDoc.ActivateSheet(sheet.GetName());
        }
        private static DrawingDoc GetDrawing()
        {
            ModelDoc2 modelDoc2 = SW.ActiveDoc;
            return modelDoc2 as DrawingDoc;
        }
        private static void AssignViews(Sheet sheet, out View isoView, out View frontView, out View rightView, out View topView, out View bottomView, out View leftView)
        {
            isoView = null;
            frontView = null;
            rightView = null;
            topView = null;
            bottomView = null;
            leftView = null;
            List<View> otherViews = new List<View>();
            var views = sheet.GetViews();


            // assign views
            if (views != null)
            {
                foreach (View view in views)
                {
                    var prefix = view.Name.Split(':')[0] + ":";
                    if (prefix == "iso:")
                    {
                        isoView = view;
                    }
                    else if (prefix == "front:")
                    {
                        frontView = view;
                    }
                    else if (prefix == "right:")
                    {
                        rightView = view;
                    }
                    else if (prefix == "top:")
                    {
                        topView = view;
                    }
                    else if (prefix == "bottom:")
                    {
                        bottomView = view;
                    }
                    else if (prefix == "left:")
                    {
                        leftView = view;
                    }
                    else
                    {
                        otherViews.Add(view);
                    }
                }

                // filter other views
                foreach (var view in otherViews)
                {
                    if (rightView != null && frontView == null)
                    {
                        frontView = view;
                    }
                    else
                    {
                        //Debug.WriteLine($"   Unidentifiable View <{view}>");
                    }
                }
            }
            else
            {
                //Debug.WriteLine($"   Sheet <{sheet.GetName()}> Contains No Views");
            }
        }
        private static void DeleteBalloons_IsometricView(View isoView, DrawingDoc drawingDoc)
        {
            ModelDoc2 modelDoc2 = drawingDoc as ModelDoc2;

            Annotation swAnnotation = isoView.GetFirstAnnotation();
            while (swAnnotation != null)
            {
                if (swAnnotation.GetType() == (int)swAnnotationType_e.swNote)
                {
                    swAnnotation.Select2(false, -1);
                    modelDoc2.Extension.SelectAll();
                    modelDoc2.Extension.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);
                    break;
                }
                swAnnotation = swAnnotation.GetNext();
            }
        }
        private static void AlignDimensions(Sheet sheet)
        {
            ModelDoc2 modelDoc2 = SW.ActiveDoc;
            var views = sheet.GetViews();
            if (views == null)
            {
                return;
            }

            foreach (View view in views)
            {
                //try
                //{
                    var displayDimensions = view.GetDisplayDimensions();
                    if (displayDimensions != null)
                    {
                        foreach (DisplayDimension dim in displayDimensions)
                        {
                            string dimName = dim.GetNameForSelection();
                            modelDoc2.Extension.SelectByID2(dimName, "DIMENSION",
                                0, 0, 0, false, 0, null, 0);
                            modelDoc2.Extension.SelectAll();
                            modelDoc2.Extension.AlignDimensions(0, 1);
                            modelDoc2.ClearSelection();
                            break;
                        }
                    }
                //}
                //catch (Exception) { }
            }
        }
        private static void DeleteDanglingAnnotations(Sheet sheet)
        {
            ModelDoc2 modelDoc2 = SW.ActiveDoc;
            var views = sheet.GetViews();
            if (views == null)
            {
                return;
            }
            modelDoc2.ClearSelection();


            foreach (View view in views)
            {
                var annotations = view.GetAnnotations();
                if (annotations != null)
                {
                    foreach (Annotation annotation in annotations)
                    {
                        if (annotation.IsDangling())
                        {
                            annotation.Select(true);
                        }
                    }
                }

                if (modelDoc2.ISelectionManager.GetSelectedObjectCount() > 0)
                {
                    modelDoc2.Extension.DeleteSelection2(0);
                }

            }
        }
        private static void AutoBalloon(View isoView, DrawingDoc drawingDoc)
        {
            DeleteBalloons_IsometricView(isoView, drawingDoc);
            ModelDoc2 modelDoc2 = drawingDoc as ModelDoc2;
            modelDoc2.Extension.SelectByID2(
                isoView.Name, "DRAWINGVIEW",
                0, 0, 0, false, 0, null, 0);

            AutoBalloonOptions options = drawingDoc.CreateAutoBalloonOptions();

            //Component2 firstItem = options.FirstItem;

            options.Size = (int)swBalloonFit_e.swBF_Tightest;
            options.ItemOrder = (int)swBalloonItemNumbersOrder_e.swBalloonItemNumbers_FollowAssemblyOrder;
            options.Layout = (int)swBalloonLayoutType_e.swDetailingBalloonLayout_Circle;
            options.Style = (int)swBalloonStyle_e.swBS_Inspection;
            options.EditBalloonOption = (int)swEditBalloonOption_e.swEditBalloonOption_Resequence;
            options.UpperTextContent = (int)swBalloonTextContent_e.swBalloonTextCustomProperties;

            options.EditBalloons = true;
            options.IgnoreMultiple = true;
            options.InsertMagneticLine = false;
            options.LeaderAttachmentToFaces = false;
            options.ReverseDirection = false;

            options.ItemNumberStart = 1;
            options.ItemNumberIncrement = 1;

            options.LowerTextContent = 0;
            options.CustomSize = 0;
            options.Layername = null;
            options.UpperText = "PartNo";
            options.LowerText = null;

            double scale = .3;
            isoView.ScaleDecimal *= scale;
            drawingDoc.AutoBalloon5(options);
            isoView.ScaleDecimal /= scale;

            modelDoc2.ClearSelection2(true);

            //Release(firstItem);
            Release(options);
        }
        private static void Rebuild()
        {
            DrawingDoc drawingDoc = GetDrawing();
            drawingDoc.ForceRebuild();
        }
        private static bool HasBom()
        {
            return SW.IActiveDoc2.Extension.SelectByID2("", "ANNOTATIONTABLES", 0.419089290372227, 0.266590868969991, 0, false, 0, null, 0);
        }
        private static string TrimSheetName(string sheetName)
        {
            // Using regular expressions to match the pattern at the beginning of the string
            Match match = Regex.Match(sheetName, @"^[-|]*([\w\d_]+)_");
            if (match.Success && match.Groups.Count > 1)
            {
                return match.Groups[1].Value;
            }
            return sheetName; // Return the original string if no match found
        }
        private static void ResetAndApply_SheetScale(View view, Sheet sheet)
        {
            view.UseSheetScale = 1;
            sheet.SetScale(1, 1, false, false);
        }
        private static void ZoomToFit()
        {
            SW.IActiveDoc2.ViewZoomtofit2();
        }
        private static void UpdateViews_AssemblySheet(View isoView, Sheet sheet)
        {
            PoisitonIsometricView_TopLeft(isoView, sheet);
        }
        private static void UpdateViews_WeldmentSheet(View isoView, View frontView, View rightView, View bottomView, Sheet sheet, DrawingDoc drawingDoc)
        {
            PositionFrontView(frontView, bottomView, out double[] frontPosition, sheet);
            PositionRightView(rightView, frontPosition);

            PoisitonIsometricView_TopLeft(isoView, frontPosition, new double[] { 0, 0 }, sheet);
        }
        private static void UpdateViews_PartSheet(View isoView, View frontView, View rightView, View topView, View bottomView, Sheet sheet)
        {
            // required views
            PositionFrontView(frontView, bottomView, out double[] frontPosition, sheet);
            PositionRightView(rightView, frontPosition);
            PoisitonIsometricView_TopRight(isoView, frontView, topView, frontPosition, sheet);

            // optional views
            if (topView != null)
            {
                PositionTopView(topView, frontView, frontPosition);
            }
            if (bottomView != null)
            {
                PositionBottomView(bottomView, frontPosition);
            }
        }
        private static void PositionAndScale(Sheet sheet, DrawingDoc drawingDoc)
        {
            AssignViews(sheet, out View isoView, out View frontView, out View rightView, out View topView, out View bottomView, out View leftView);

            bool bomCheck = HasBom();
            Debug.WriteLine($"BoM found: {bomCheck}");

            // assembly sheet
            if (frontView == null && isoView != null && bomCheck == true)
            {
                UpdateViews_AssemblySheet(isoView, sheet);
            }

            // weldment sheet
            else if (frontView != null && bomCheck == true)
            {
                UpdateViews_WeldmentSheet(isoView, frontView, rightView, bottomView, sheet, drawingDoc);
            }

            // part sheet
            else if (frontView != null && bomCheck == false)
            {
                UpdateViews_PartSheet(isoView, frontView, rightView, topView, bottomView, sheet);
            }
        }
        private static void PoisitonIsometricView_TopLeft(View isoView, double[] frontPosition, double[] topPosition, Sheet sheet)
        {
            double viewWidth;
            double viewHeight;
            double sheetWidth;
            double sheetHeight;

            GetViewBoxDims(isoView, out viewWidth, out viewHeight);
            GetSheetDims(sheet, out sheetWidth, out sheetHeight);
            double customViewScale = isoView.ScaleDecimal;

            double maxViewWidth = sheetWidth - Border * 2 - BoM.Width;

            double value;
            switch (topPosition[1])
            {
                case 0:
                    value = frontPosition[1];
                    break;
                default:
                    value = topPosition[1];
                    break;
            }
            double maxViewHeight = sheetHeight - Border * 2 - TitleBlockHeight - value;

            double buffer = 0.90;
            double wScale = maxViewWidth / viewWidth;
            double hScale = maxViewHeight / viewHeight;

            double scaleCoefficient = Math.Min(wScale, hScale) * buffer;

            viewHeight *= scaleCoefficient;
            customViewScale *= scaleCoefficient;

            isoView.ScaleDecimal = customViewScale;

            double[] position =
            {
                (maxViewWidth / 2 ) * InchesToMeters / buffer,
                (sheetHeight - viewHeight / 2 - Border) * InchesToMeters * buffer
            };

            isoView.Position = position;
        }
        private static void PoisitonIsometricView_TopLeft(View isoView, Sheet sheet)
        {
            double[] frontPosition = { 0, 0 };
            double[] topPosition = { 0, 0 };
            PoisitonIsometricView_TopLeft(isoView, frontPosition, topPosition, sheet);
        }
        private static void PoisitonIsometricView_TopRight(View isoView, View frontView, View topView, double[] frontPosition, Sheet sheet)
        {
            GetViewBoxDims(isoView,
                out double isoViewWidth,
                out double isoViewHeight);

            GetViewBoxDims(frontView,
                out double frontViewWidth,
                out double frontViewHeight);

            GetSheetDims(sheet,
                out double sheetWidth,
                out double sheetHeight);

            // view size limits
            double availableWidth = sheetWidth - Border - frontPosition[0];
            double availableHeight = sheetHeight - Border - frontPosition[1];
            double buffer = 0.75;

            // handle long views
            if (frontViewWidth > frontViewHeight * 9 && topView == null)
            {
                availableWidth *= 2;
            }
            else if (frontViewWidth > frontViewHeight * 9 && topView != null)
            {
                availableWidth *= 1.5;
                availableHeight /= 1.375;
            }

            // handle tall views
            else if (frontViewHeight >= frontViewWidth * 1.5)
            {
                availableWidth *= 0.7;
                availableHeight *= 4;
                buffer = 0.85;
            }

            // handle square views
            else if (frontViewHeight >= frontViewWidth * 0.99)
            {
                availableWidth *= 0.65;
                availableHeight *= 2.5;
                buffer = 0.7;
            }

            // handle square-ish views
            else if (frontViewHeight >= frontViewWidth * 0.81)
            {
                availableWidth *= 0.75;
                availableHeight *= 2.5;
                buffer = 0.85;
            }

            // set custom scale
            double wScale = availableWidth / isoViewWidth;
            double hScale = availableHeight / isoViewHeight;
            isoView.ScaleDecimal *= Math.Min(wScale, hScale) * buffer;

            // set position
            double[] position =
            {
                (sheetWidth - Border - availableWidth/2) * InchesToMeters,
                (sheetHeight - Border - availableHeight/2) * InchesToMeters
            };
            isoView.Position = position;
        }
        private static void PositionFrontView(View frontView, View bottomView, out double[] frontPosition, Sheet sheet)
        {
            ResetAndApply_SheetScale(frontView, sheet);

            GetSheetDims(sheet,
                out double sheetWidth,
                out double sheetHeight);

            GetViewBoxDims(frontView,
                out double viewWidth,
                out double viewHeight);

            double widthFactor = 0.5;
            double heightFactor = 0.5;
            double scaleFactor = 1;
            double extraHeight = 0;

            // handle long views
            if (viewWidth > viewHeight * 10)
            {
                widthFactor = 0.68;
            }

            // handle tall views
            if (viewHeight >= viewWidth * 1.25)
            {
                heightFactor = 0.75;
                scaleFactor = 0.9;
            }

            // handle bottom views
            if (bottomView != null)
            {
                extraHeight = Border * 2;
            }

            // view size limits
            double availableWidth = (sheetWidth - Border * 4) * widthFactor;
            double availableHeight = (sheetHeight - Border * 2 - TitleBlockHeight) * heightFactor;

            // set sheet scale
            double wScale = availableWidth / viewWidth;
            double hScale = availableHeight / viewHeight;
            double scale = Math.Min(wScale, hScale) * scaleFactor;
            viewHeight *= scale;
            viewWidth *= scale;
            sheet.SetScale(scale, 1, false, false);

            // set position
            double x = (availableWidth / 2 + Border * 3.2) * InchesToMeters;
            double y = (viewHeight / 2 + Border * 4 + TitleBlockHeight + extraHeight) * InchesToMeters;
            if (viewHeight >= viewWidth * 1.25)
            {
                y = (availableHeight / 2 + Border * 2 + TitleBlockHeight) * InchesToMeters;
            }
            frontPosition = new double[] { x, y };
            frontView.Position = frontPosition;

            // out parameter in Inches
            x *= MetersToInches; y *= MetersToInches;

            x += viewWidth / 2;
            y += viewHeight / 2;

            frontPosition[0] = x; frontPosition[1] = y;
        }
        private static void PositionRightView(View rightView, double[] frontPosition)
        {
            GetViewBoxDims(rightView,
                out double viewWidth,
                out double viewHeight);

            double offsetFactor = Border * 4;
            // handle wide views
            if (viewWidth > viewHeight * 2)
            {
                offsetFactor = Border * 2;
            }
            double x = (frontPosition[0] + viewWidth / 2 + offsetFactor) * InchesToMeters;
            rightView.Position = new double[] { x, 0 };
        }
        private static void PositionTopView(View topView, View frontView, double[] frontPosition)
        {
            GetViewBoxDims(topView,
                out _,
                out double viewHeight);

            GetViewBoxDims(frontView,
                out double frontViewWidth,
                out double frontViewHeight);

            double y = (frontPosition[1] + viewHeight / 2 + Border * 4) * InchesToMeters;

            // handle square-ish views
            if (frontViewHeight >= frontViewWidth * 0.83)
            {
                y = (frontPosition[1] + viewHeight / 2 + Border) * InchesToMeters;
            }
            topView.Position = new double[] { 0, y };
        }
        private static void PositionBottomView(View bottomView, double[] frontPosition)
        {
            GetViewBoxDims(bottomView,
                out double viewWidth,
                out double viewHeight);

            double offsetFactor = Border * 3;
            double y = (frontPosition[1] - viewHeight / 2 - offsetFactor) * InchesToMeters;
            bottomView.Position = new double[] { 0, y };

        }
        private static void GetViewBoxDims(View view, out double viewWidth, out double viewHeight)
        {
            object outlineObj = view.GetOutline();
            double[] outline = (double[])outlineObj;

            double xMin = outline[0];
            double yMin = outline[1];
            double xMax = outline[2];
            double yMax = outline[3];

            viewWidth = (xMax - xMin) * MetersToInches;
            viewHeight = (yMax - yMin) * MetersToInches;
        }
        private static void GetSheetDims(Sheet sheet, out double sheetWidth, out double sheetHeight)
        {
            sheetWidth = 0;
            sheetHeight = 0;
            sheet.GetSize(ref sheetWidth, ref sheetHeight);

            sheetWidth = Math.Round(sheetWidth * MetersToInches, 1);
            sheetHeight = Math.Round(sheetHeight * MetersToInches, 1);
        }
        private static void Release(AutoBalloonOptions options)
        {
            while (Marshal.ReleaseComObject(options) > 0) { }
            options = null;
        }
        public static void SortSheetsInDrawing(DrawingDoc activeDrawing)
        {
            if (activeDrawing == null) return; // Exit if the active document is not a drawing.

            // 1. Retrieve all sheet names
            string[] allSheetNames = activeDrawing.GetSheetNames();

            // 2. Sort the sheet names
            var sortedSheetNames = allSheetNames
                .OrderBy(sheet => sheet.Equals("ReadMe") ? 0 : 1)
                .ThenBy(sheet =>
                {
                    char lastChar = sheet.Last();
                    if (char.IsDigit(lastChar))
                        return "0" + lastChar;
                    return "1" + lastChar;
                })
                .ToArray();

            // 3. Use ReorderSheets to rearrange the sheets based on the sorted names
            activeDrawing.ReorderSheets(sortedSheetNames);

            activeDrawing.ActivateSheet(allSheetNames[0]);
        }
        private static void CollapseDrawingTree()
        {
            ModelDoc2 activeDoc = (ModelDoc2)SW.ActiveDoc;
            if (activeDoc == null) return;

            Feature firstFeature = activeDoc.FirstFeature() as Feature;
            if (firstFeature == null) return;

            activeDoc.Extension.SelectByID2(firstFeature.Name, "SHEET", 0, 0, 0, false, 0, null, 0);

            SW.RunCommand(2555, null);
        }




        // Static Read-Only Properties
        private static double Border => 0.5;
        private static double TitleBlockHeight => 0.875;
        private static double RevTableHeight => 0.5;
        private static double MetersToInches => 39.3700787402;
        private static double InchesToMeters => 0.0254;
        private static BillOfMaterialsExtensions BoM { get; set; } = new BillOfMaterialsExtensions();
        private class BillOfMaterialsExtensions
        {
            private double PartNoWidth => 0.9375;
            private double QtyWidth => 0.4375;
            private double DescriptionWidth => 5.375;
            internal double Width => PartNoWidth + QtyWidth + DescriptionWidth;
        }
    }
}
