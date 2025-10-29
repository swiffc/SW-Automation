using SolidWorks.Interop.sldworks;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using static FileTools.FileTools;
using cTools = ModelTools.ReleaseCOM;

namespace Hood
{
    public class HoodData
    {
        // User Inputs
        public static char Bank { get; set; } = 'A';
        public static string Project { get; set; } = "M000";
        public static double Length { get; set; } = 117;
        public static double Width { get; set; } = 117;
        public static double Height { get; set; } = 36;
        public static double fanDiameterInFeet = 9;
        public static int Stacks { get; set; } = 1;
        public static double WindLoad { get; set; } = 30;
        public static double Shift { get; set; } = 0;
        public static double Adjust { get; set; } = 0;
        public static string Initials { get; set; } = "AM";
        public static string Customer { get; set; } = "Customer";
        public static string Client { get; set; } = "Client";
        public static string Location { get; set; } = "Location";
        public static string PurchaseOrder { get; set; } = "PurchaseOrder";
        public static string ItemNumber { get; set; } = "ItemNumber";

        internal static FanRingExtensions Ring { get; set; } = new FanRingExtensions();
        internal class FanRingExtensions
        {
            public int Depth { get; set; } = 24;
            public string FilePath { get; set; }
        }


        public static Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> SelectedComponents()
        {
            // Get selection
            SelectionMgr selectionMgr = SW.IActiveDoc2.ISelectionManager;
            var selectedComponents = new Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)>();

            // Iterate through all selected items
            int numSelected = selectionMgr.GetSelectedObjectCount2(-1);
            Debug.WriteLine("\n" + "Selection");
            for (int i = 1; i <= numSelected; i++)
            {
                object selectedObj = selectionMgr.GetSelectedObject6(i, -1);

                Component2 component = null;

                if (selectedObj is Component2 comp)
                {
                    component = comp;
                }
                else if (selectedObj is IFace2 face)  // If the selected object is a face
                {
                    IEntity entity = (IEntity)face;
                    component = entity.GetComponent() as Component2;
                }

                // Itemize and add to the dictionary
                if (component != null)
                {
                    string fileName = component.GetPathName();
                    string staticPartNo = SW.GetConfigurationNames(fileName)[0];  // Assuming SW.GetConfigurationNames is your custom method
                    string dynamicPartNo = Path.GetFileNameWithoutExtension(fileName).Split('-').Last();
                    char? bank = Path.GetFileNameWithoutExtension(fileName).Split('-').ElementAtOrDefault(1)?.ElementAtOrDefault(2);

                    selectedComponents[component] = (fileName, staticPartNo, dynamicPartNo, bank);
                    Debug.WriteLine($"   Selection({i}/{numSelected})  -->  {Path.GetFileNameWithoutExtension(fileName)}");
                }
                else
                {
                    Debug.WriteLine($"   Selection({i}/{numSelected}) discarded");
                }
            }

            // Add children to the dictionary
            AddChildrenToSelection(selectedComponents);

            cTools.Release(ref selectionMgr);

            return selectedComponents;
        }
        private static void AddChildrenToSelection(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents)
        {
            bool newEntriesAdded;

            do
            {
                newEntriesAdded = false;  // Reset the flag at the start of each loop iteration

                var newEntries = new Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>();

                if (selectedComponents.Values.Any(value =>
                    value.StaticPartNo == "3" ||
                    value.StaticPartNo == "157" ||
                    value.StaticPartNo == "182" ||
                    value.StaticPartNo == "187" ||
                    value.StaticPartNo == "188" ||
                    value.StaticPartNo == "189" ||
                    value.StaticPartNo == "194"))

                {
                    foreach (var componentEntry in selectedComponents)
                    {
                        Component2 component = componentEntry.Key;

                        object[] childComponents = component.GetChildren();

                        if (childComponents != null)
                        {
                            foreach (Component2 childComponent in childComponents)
                            {
                                // Avoid processing already processed components
                                if (!selectedComponents.ContainsKey(childComponent) && !newEntries.ContainsKey(childComponent))
                                {
                                    string fileName = childComponent.GetPathName();
                                    string dynamicPartNo = Path.GetFileNameWithoutExtension(fileName).Split('-').Last();
                                    string staticPartNo = SW.GetConfigurationNames(fileName)[0];
                                    char? bank = Path.GetFileNameWithoutExtension(fileName).Split('-').ElementAtOrDefault(1)?.ElementAtOrDefault(2);

                                    // Store the child component data in the newEntries dictionary
                                    newEntries[childComponent] = (fileName, staticPartNo, dynamicPartNo, bank);
                                    Debug.WriteLine($"      Child of parent {Path.GetFileNameWithoutExtension(componentEntry.Value.FileName)}  -->  {Path.GetFileNameWithoutExtension(fileName)}");

                                    newEntriesAdded = true;  // Set the flag to true as new entries are being added
                                }
                            }
                        }
                    }
                }

                // Merge the new entries with the selectedComponents dictionary inside the loop
                foreach (var entry in newEntries)
                {
                    selectedComponents[entry.Key] = entry.Value;
                }

            } while (newEntriesAdded);  // Continue looping as long as new entries are being added
        }




        public static string TemplateFolderPath { get; set; } = @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Hood";
        public interface IPanel
        {
            double BottomHole { get; }
            double TopHole { get; }
            double FlangeGage { get; }
            double Height { get; }
            double THK { get; set; }
            double WebHeight { get; set; }
            string Description { get; set; }
            string JDEnumber { get; set; }
        }

        public interface IStiffener
        {
            double Leg { get; set; }
            double THK { get; set; }
            double MaxSpacing { get; set; }
            double R { get; set; }
            double Gauge { get; set; }
            string Description { get; set; }
            string JDEnumber { get; set; }
        }

        internal static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");


        internal static double FanDiameter
        {
            get
            {
                return fanDiameterInFeet * 12;
            }
            set
            {
                fanDiameterInFeet = value;
            }
        }

        internal static string DesktopFolderPath
        {
            get
            {
                string desktopPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop);
                return Path.Combine(desktopPath, $"{Project}-3{Bank}");
            }
        }

        internal static HoodExtensions Hood { get; set; } = new HoodExtensions();
        internal class HoodExtensions
        {
            public InnerSubExtensions InnerWidth { get; set; } = new InnerSubExtensions();
            public InnerSubExtensions InnerLength { get; set; } = new InnerSubExtensions();
            public double xFactor { get; set; }
            public double zFactor { get; set; }
            public Trig.Point BtmRectangleIntersect { get; set; }
            public Trig.Point S1 { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}.SLDASM";
            public Trig.Point R1 { get; set; }
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public static string AssemblyName { get; set; } = "Hood";
            public static string AssemblyNumber { get; set; } = "3";
        }
        internal class InnerSubExtensions
        {
            public double Top { get; set; }
            public double Bottom { get; set; }
        }

        internal static EndPanelExtensions EndPanel { get; set; } = new EndPanelExtensions();
        internal class EndPanelExtensions : IPanel
        {
            public double Height { get; set; }
            public double TopLength { get; set; }
            public double BottomLength { get; set; }
            public double FlangeLength => 3;
            public double FlangeGage => 1.5;
            public double THK { get; set; }
            public double BottomHole { get; set; }
            public double TopHole { get; set; }
            internal double SlopeAngle { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public double WebHeight { get; set; }
            public string AssemblyNumber { get; set; } = "157";
            public string Description { get; set; }
            public string JDEnumber { get; set; }
            public string Title { get; set; } = "END-PANEL_HOOD";

        }
        internal static SidePanelExtensions SidePanel { get; set; } = new SidePanelExtensions();
        internal class SidePanelExtensions : IPanel
        {
            public double Height { get; set; }
            public double TopLength { get; set; }
            public double BottomLength { get; set; }
            public double FlangeLength => 3;
            public double FlangeGage => 1.5;
            public double THK { get; set; }
            public double BottomHole { get; set; }
            internal double SlopeAngle { get; set; }
            public double TopHole { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public double WebHeight { get; set; }
            public string AssemblyNumber { get; set; } = "182";
            public string Description { get; set; }
            public string JDEnumber { get; set; }
            public string Title { get; set; } = "SIDE-PANEL_HOOD";

        }
        internal static TopPanelExtensions TopPanel { get; set; } = new TopPanelExtensions();
        internal class TopPanelExtensions
        {
            public double Radius { get; set; }
            public double THK { get; set; }
            public double Length { get; set; }
            public double HoleGauge { get; set; } = 1.125;
            public double AngularSpacing { get; set; }
            public double TopHoleSpacing { get; set; }
            public double TopHoleQuantity { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public string AssemblyNumber { get; set; } = "194";
            public string Description { get; set; }
            public string JDEnumber { get; set; }
            public string Title { get; set; } = "TOP-PANEL_HOOD";
        }


        internal static FanGuardExtensions Guard { get; set; } = new FanGuardExtensions();
        internal class FanGuardExtensions
        {
            public string FilePath { get; set; }
        }

        internal static CornerPrlExtensions CornerAngle { get; set; } = new CornerPrlExtensions();
        internal class CornerPrlExtensions
        {
            public double Length { get; set; }
            public double Angle { get; set; }
            public double Leg { get; set; } = 2;
            public double HoleGage { get; set; } = 1.125;
            public Trig.Point CenterPoint { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public double MidHoleSpacing { get; set; } = 2.5;
            public string AssemblyNumber { get; set; } = "187";
            public string Description { get; set; } = "PLATE_3/16\"_A572_50";
            public string JDEnumber { get; set; } = "60015-HPC";
            public string Title { get; set; } = "CORNER-PRL_HOOD";
        }
        internal static PanelStiffenerExtensions Stiffener { get; set; } = new PanelStiffenerExtensions();
        internal class PanelStiffenerExtensions
        {
            public EndStiffenerSubExtensions End { get; set; } = new EndStiffenerSubExtensions();
            public SideStiffenerSubExtensions Side { get; set; } = new SideStiffenerSubExtensions();
        }
        internal class EndStiffenerSubExtensions : IStiffener
        {
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double Length { get; set; }
            public double HoleToEnd { get; set; } = 2.5;
            public Trig.Point CenterPoint { get; set; }
            public double MidHoleSpacing { get; set; }
            public double Xtranslation { get; set; }
            public double Quantity { get; set; }
            public double Spacing { get; set; }
            public double Leg { get; set; }
            public double THK { get; set; }
            public double R { get; set; }
            public double MaxSpacing { get; set; }
            public double Gauge { get; set; }
            public string AssemblyNumber { get; set; } = "188";
            public string Description { get; set; }
            public string JDEnumber { get; set; }
            public string Title { get; set; } = "END-PANEL-STIFFENER_HOOD";
        }
        internal class SideStiffenerSubExtensions : IStiffener
        {
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public string FilePath => $@"{DesktopFolderPath}\{Project}-3{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double Length { get; set; }
            public double HoleToEnd { get; set; } = 1.125;
            public Trig.Point CenterPoint { get; set; }
            public double Ztranslation { get; set; }
            public double Quantity { get; set; }
            public double Spacing { get; set; }
            public double Leg { get; set; }
            public double THK { get; set; }
            public double R { get; set; }
            public double MaxSpacing { get; set; }
            public double Gauge { get; set; }
            public string AssemblyNumber { get; set; } = "187";
            public string Description { get; set; }
            public string JDEnumber { get; set; }
            public string Title { get; set; } = "SIDE-PANEL-STIFFENER_HOOD";
        }

        internal class PartNoSubExtensions
        {
            public string Static { get; set; }
            public string Dynamic { get; set; }
        }


    }
}
