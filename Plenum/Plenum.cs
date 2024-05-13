using ModelTools;
using Plenum.Floor;
using Plenum.Floor.Derived;
using Plenum.Floor.Derived.Derived;
using Plenum.JohnsonBeam;
using Plenum.JohnsonBeam.Children;
using Plenum.StandardParts;
using Plenum.Stiffeners;
using Plenum.Walls;
using SolidWorks.Interop.sldworks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows.Forms;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using Ftools = FileTools.FileTools;
using mTools = Tools.ModelTools;
using static FileTools.StaticFileTools;
using Plenum.Structure;
using static FileTools.CommonData.CommonData;
using Plenum.Structure.Derived;
using static FileTools.Properties.Settings;

namespace Plenum
{
    public class Plenum
    {


        // Main method
        public static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            PlenumUI plenumUI = new PlenumUI();
            Application.Run(plenumUI);
        }


        // Protected methods
        protected void InitializePlenum(Design callerType)
        {
            bool bankExists;
            do
            {
                bankExists = File.Exists(FilePath);

                if (bankExists)
                {
                    AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, false);
                    var components = InstantiateComponents(callerType, StaticParentDictionary.Values.ToArray());
                        LocateComponents(components);

                    if (Default.Toggle_CreateDrawing)
                    {
                        CreateDrawing(components);
                        AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, false);
                    }

                    if (Default.Toggle_Save || Default.Toggle_DeleteFiles)
                        mTools.SaveEverything();

                    if (Default.Toggle_DeleteFiles)
                    {
                        mTools.CloseEverything();
                        mTools.DeleteUnusedFiles();
                        AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, false);
                    }

                    AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, false);
                    mTools.Rebuild(true);
                    TurnOffBendLines();
                }
                else
                {
                    Default.Bank = FTools.AddNew_Bank();
                }

            } while (!bankExists);

        }
        protected void UpdateFloor(Design callerType)
        {
            try
            {
                mTools.Lock();
            }
            catch (Exception) { }
            AssemblyDoc = FTools.OpenAssembly(FilePath, StaticPartNo, false);
            var components = InstantiateComponents(callerType, FloorDictionary.Values.ToArray());
            LocateComponents(components);
            mTools.ShowTopView();
            mTools.Unlock();
        }


        // Private methods
        private void CreateDrawing(List<IComponentInfo> components)
        {
            // Create drawing file
            bool drawingExists = FTools.CreateDrawing(StaticPartNo, out string drawingPath);
            if (drawingExists)
                return;


            AddIComponentsInfo(ref components, ChildInstances);

            // Remove duplicate instances based on PartNo
            components = components.GroupBy(comp => comp.PartNo)
                                   .Select(group => group.First())
                                   .ToList();

            // Replace references
            foreach (var component in components)
                FTools.ReplaceDrawingReference(component.PartNo, drawingPath, component.StaticPartNo);

            // Open drawing
            DrawingDoc drawingDoc = FTools.OpenDrawing(drawingPath);

            // Get all existing sheet names as an array
            string[] templateSheetNames = drawingDoc.GetSheetNames();

            // List sheets not to be deleted (include both StaticPartNo and StaticPartNo + "_FP")
            var sheetNamesRequired = new List<string>();
            foreach (var component in components)
            {
                sheetNamesRequired.Add(component.StaticPartNo);
                sheetNamesRequired.Add(component.StaticPartNo + "_FP"); // Include potential "_FP" sheets
            }

            // Determine sheets to delete by subtracting required sheets from existing ones
            var sheetsToDelete = new List<string>();
            foreach (var sheetName in templateSheetNames)
            {
                // If a sheet name is not required, and it's not an "_FP" version of a required sheet, add it to the delete list
                if (!sheetNamesRequired.Contains(sheetName) && !sheetNamesRequired.Contains(sheetName.Replace("_FP", "")))
                {
                    sheetsToDelete.Add(sheetName);
                }
            }

            // Delete sheets
            foreach (var sheetName in sheetsToDelete)
                DeleteSheet(sheetName, drawingDoc);

            // Rename sheets
            foreach (var component in components)
            {
                string staticPartNo = component.StaticPartNo;
                string compPartNo = component.PartNo;

                if (templateSheetNames.Contains(staticPartNo))
                    FTools.RenameSheet(staticPartNo, compPartNo, drawingDoc);

                string fpVersion = staticPartNo + "_FP";
                if (templateSheetNames.Contains(fpVersion))
                    FTools.RenameSheet(fpVersion, compPartNo + "_FP", drawingDoc);
            }

            // Sort sheets
            mTools.SortSheetsInDrawing();

            // Save and close drawing
            (drawingDoc as ModelDoc2).Save3(1, 0, 0);
            //mTools.Close(drawingPath);
            cTools.Release(ref drawingDoc);

        }
        private static void DeleteSheet(string sheetName, DrawingDoc drawingDoc)
        {
            ModelDoc2 modelDoc2 = drawingDoc as ModelDoc2;
            //drawingDoc.ActivateSheet(sheetName);
            bool checkSelection = modelDoc2.Extension.SelectByID2(sheetName, "SHEET", 0, 0, 0, false, 0, null, 0);
            bool checkDeletion = modelDoc2.Extension.DeleteSelection2(0);
            Debug.WriteLine($"   Sheet {sheetName} deleted: {checkDeletion}");
        }
        private List<IComponentInfo> InstantiateComponents(Design callerType, params Type[] componentTypes)
        {
            var components = new List<IComponentInfo>();

            foreach (var type in componentTypes)
            {
                // Get the 'Enabled' property
                var enabledProperty = type.GetProperty("Enabled", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
                if (enabledProperty != null && (bool)enabledProperty.GetValue(null))
                {
                    // Create an instance if 'Enabled' is true
                    var instance = Activator.CreateInstance(type, callerType) as IComponentInfo;
                    if (instance != null)
                    {
                        components.Add(instance);
                    }
                }
            }

            return components;
        }
        private void AddIComponentsInfo(ref List<IComponentInfo> components, List<IComponentInfo> instances)
        {
            foreach (var child in instances)
                components.Add(child);

            instances.Clear();
        }
        private void LocateComponents(List<IComponentInfo> components)
        {
            Assembly.RemoveUnneededSubComponents(AssemblyDoc);
            Component2[] userLocatedComponents = aTools.UnfixedComponentsArray(AssemblyDoc);
            foreach (var component in components)
            {
                aTools.PlaceComponent(component, AssemblyDoc);
            }
            aTools.FixComponentLocations(userLocatedComponents, AssemblyDoc);

            cTools.Release(AssemblyDoc);
        }


        // Public properties
        public static AssemblyDoc AssemblyDoc { get; set; }


        // Constants
        internal const string AssemblyName = "Plenum";
        protected const string StaticPartNo = "5";


        // Dictionaries
        internal static Dictionary<string, Type> StaticParentDictionary = new Dictionary<string, Type>
        {
            { "113", typeof(JohnsonBeamWld) },
            { "116", typeof(MidColumn) },
            { "117", typeof(EndColumn) },
            { "140", typeof(BackingPlate) },
            { "142", typeof(KneeClipBent_L) },
            { "144", typeof(KneeClipBent_R) },
            { "146", typeof(KneeClipFlat) },
            { "156", typeof(EndPanel) },
            { "166", typeof(DividerPanel) },
            { "176P", typeof(DividerFlange) },
            { "178", typeof(PlanBraceEnd) },
            { "179", typeof(PlanBraceMiddle) },
            { "180", typeof(PlanBraceHorizontal) },
            { "181", typeof(SidePanel) },
            { "181L", typeof(SidePanelLeft) },
            { "181R", typeof(SidePanelRight) },
            { "185", typeof(DividerAngle) },
            { "186", typeof(CornerAngle) },
            { "186S", typeof(WallStiffener) },
            { "191", typeof(OuterFloorPanelLeft) },
            { "192", typeof(FloorSplice) },
            { "193", typeof(OuterFloorPanelRight) },
            { "195", typeof(InnerFloorPanelLeft) },
            { "196", typeof(FanRing) },
            { "197", typeof(InnerFloorPanelRight) },
            { "201", typeof(FloorStiffener) },
            { "206", typeof(OuterFloorExtension) },
            { "207", typeof(InnerFloorExtension) },
            { "266", typeof(MotorBeamWld) },
            { $"FG{FanDiameter}", typeof(FanGuard) },
        };
        internal static Dictionary<string, Type> StaticChildDictionary = new Dictionary<string, Type>
        {
            { "103", typeof(CapPlate) },
            { "112", typeof(JohnsonBeamPlate) },
            { "113P", typeof(JohnsonBeamPart) },
            { "114", typeof(FlatSeal) },
            { "115", typeof(JohnsonTopPlate) },
            { "116P", typeof(MidBeam) },
            { "117P", typeof(EndBeam) },
            { "266", typeof(MotorBeamPart) },
            { "267", typeof(MotorBeamPlate) },
        };
        internal static Dictionary<string, Type> FloorDictionary = new Dictionary<string, Type>
        {
            { "191", typeof(OuterFloorPanelLeft) },
            { "193", typeof(OuterFloorPanelRight) },
            { "195", typeof(InnerFloorPanelLeft) },
            { "197", typeof(InnerFloorPanelRight) },
            { "201", typeof(FloorStiffener) },
        };
        internal static List<IComponentInfo> ParentInstances = new List<IComponentInfo>();
        internal static List<IComponentInfo> ChildInstances = new List<IComponentInfo>();
        internal static List<Sheet> SheetsToDelete = new List<Sheet>();


        // Internal static properties
        internal static Ftools FTools
        {
            get
            {
                return new Ftools
                    (AssemblyName, Default.Project, Default.Bank, StaticPartNo,
                    Default.Initials, Default.Customer, Default.Client,
                    Default.PlantLocation, Default.PurchaseOrder, Default.ItemNumber);
            }
        }


        // Private static properties
        private static string FilePath
        {
            get
            {
                return $@"{FTools.DesktopFolderPath}\{Default.Project}-{StaticPartNo}{Default.Bank}.SLDASM";
            }
        }
    }
}
