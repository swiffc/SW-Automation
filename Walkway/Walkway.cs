
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Windows;
using System.Windows.Forms;
using Walkway.Tools;

namespace Walkway
{
    public class Walkway
    {

        // Static Read-Write Properties (User Inputs)
        public static string Project { get; set; } = "N001";
        public static string Customer { get; set; } = "Customer";
        public static string Client { get; set; } = "Client";
        public static string Location { get; set; } = "Town, State";
        public static string PurchaseOrder { get; set; } = "PO000001";
        public static string ItemNumber { get; set; } = "AC-1";

        // Model Inputs
        public static char Bank { get; set; } = 'A';
        public static string AcheColumnSize { get; set; } = "W6x15";
        public static double RailHeight { get; set; } = 42;
        public static double FloorHeight { get; set; } = 1.25;
        public static double OffsetFromColumnCenter { get; set; } = 24;
        public static double AcheColumnCenterToCenterWidth { get; set; } = 188.125;
        public static double Width { get; set; } = 30;
        public static double Length { get; set; } = 200;
        public static int MinStringerSize { get; set; } = 0;
        public static double EndToSupportCenter { get; set; } = 5.9375;
        public static bool UpdateLocation { get; set; } = true;

        // Drawing Inputs
        public static string Initials { get; set; } = "XX";

        // SolidWorks COM connection
        private static SldWorks _sw;
        private static readonly object _swLock = new object();

        /// <summary>
        /// Gets the SolidWorks application instance (lazy-initialized)
        /// </summary>
        internal static SldWorks SW
        {
            get
            {
                if (_sw == null)
                {
                    lock (_swLock)
                    {
                        if (_sw == null)
                        {
                            try
                            {
                                _sw = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
                            }
                            catch (COMException ex)
                            {
                                throw new InvalidOperationException(
                                    "SolidWorks is not running. Please start SolidWorks and try again.", ex);
                            }
                        }
                    }
                }
                return _sw;
            }
        }

        /// <summary>
        /// Disconnect from SolidWorks and release COM object
        /// </summary>
        public static void DisconnectSolidWorks()
        {
            if (_sw != null)
            {
                Marshal.ReleaseComObject(_sw);
                _sw = null;
            }
        }

        public static void Create_Standard_EndWalkway(char bank, double width, double height, double floorHeight, int minimumStringerSize, double offsetFromColumnCenter, string columnSize, double plenumCenterWidth, double supportCenterToWalkwayEnd)
        {
            // Set global property
            Bank = bank; Debug.WriteLine("\n" + $"Bank set to: {bank}" + "\n");

            bool bankExists;
            do
            {
                // Check if bank exists
                string desktopBankFile = $@"{DesktopFolderPath}\{Project}-28{Bank}.SLDASM";
                bankExists = System.IO.File.Exists(desktopBankFile);

                if (bankExists)
                {
                    double length = plenumCenterWidth + supportCenterToWalkwayEnd * 2;

                    AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);

                    // Contruct new walkway
                    WalkwayPlatform platform = new WalkwayPlatform(bank, length, width, floorHeight, minimumStringerSize, supportCenterToWalkwayEnd, false);

                    List<Component2> platformList = FindMatchingComponents(platform.Platform.FilePath, bankAssembly);
                    PlacePlatform(platformList, platform, bankAssembly);

                    // Construct new handrail
                    HandRail rail = new HandRail(bank, length, height, floorHeight, minimumStringerSize, false);

                    List<Component2> railList = FindMatchingComponents(rail.Rail.FilePath, bankAssembly);
                    PlaceHandRails(railList, rail, bankAssembly, width, height);

                    // Construct new support
                    Support support = new Support(bank, minimumStringerSize, width, offsetFromColumnCenter, columnSize, length, false);

                    List<Component2> supportList = FindMatchingComponents(support.Reference.FilePath, bankAssembly);
                    PlaceSupportBeams(supportList, support, bankAssembly, floorHeight, plenumCenterWidth);

                    // Fix all components
                    FixComponentLocations(bankAssembly);

                    Close(platform.Platform.FilePath);
                    Close(rail.Rail.FilePath);
                    Close(support.Reference.FilePath);

                    SaveEverything();

                    Optimize.Release(ref bankAssembly);
                }
                else
                {
                    AddNew_Bank();
                }
            } while (!bankExists);
        }
        public static void Modify_Walkway(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, double walkway_Width, double rail_Height, double floor_Height, int minimumStringerSize, double offsetFromColumnCenter, string columnSize, double plenumCenterWidth, double supportCenterToWalkwayEnd, bool updateLocation)
        {
            double walkway_Length = plenumCenterWidth + supportCenterToWalkwayEnd * 2;

            string desktopBankFile = $@"{DesktopFolderPath}\{Project}-28{Bank}.SLDASM";

            string platformPath = null;
            string railPath = null;
            string supportPath = null;

            // Dictionary mapping the static part numbers to their descriptions for debugging
            Dictionary<string, string> staticPartDescriptions = new Dictionary<string, string>
            {
                { Platform_PartNo_Static, "PLATFORM" },
                { Handrail_PartNo_Static, "RAIL WELDMENT"},
                { SupportAssembly_PartNo_Static, "Support assembly (reference only)" }
            };

            // Itemized dictionary of support components
            var wwComponents = FilterComponents(selectedComponents, staticPartDescriptions);
            if (wwComponents.Count == 0) { DevTools.EnablePartUI(); return; }

            // Update locations option
            if (updateLocation) { AddParentToSelection(wwComponents); }

            // Assign bank if ONLY one bank is found. Otherwise, exit the program.
            GetBankFrom(wwComponents);

            // Contruct class instances using selected parts
            if (wwComponents.Any(entry => entry.Value.StaticPartNo == Platform_PartNo_Static))
            {
                WalkwayPlatform platform = new WalkwayPlatform(selectedComponents, walkway_Length, walkway_Width, floor_Height, minimumStringerSize, supportCenterToWalkwayEnd, updateLocation);

                AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);

                List<Component2> platformList = FindMatchingComponents(platform.Platform.FilePath, bankAssembly);
                PlacePlatform(platformList, platform, bankAssembly);

                platformPath = platform.Platform.FilePath;
            }

            if (wwComponents.Any(entry => entry.Value.StaticPartNo == Handrail_PartNo_Static))
            {
                HandRail rail = new HandRail(selectedComponents, walkway_Length, rail_Height, floor_Height, minimumStringerSize, updateLocation);

                AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);

                List<Component2> handrailList = FindMatchingComponents(rail.Rail.FilePath, bankAssembly);
                PlaceHandRails(handrailList, rail, bankAssembly, walkway_Width, rail_Height);

                railPath = rail.Rail.FilePath;
            }

            if (wwComponents.Any(entry => entry.Value.StaticPartNo == SupportAssembly_PartNo_Static))
            {
                Support support = new Support(selectedComponents, minimumStringerSize, walkway_Width, offsetFromColumnCenter, columnSize, walkway_Length, updateLocation);

                AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);

                List<Component2> supportList = FindMatchingComponents(support.Reference.FilePath, bankAssembly);
                PlaceSupportBeams(supportList, support, bankAssembly, floor_Height, plenumCenterWidth);

                supportPath = support.Reference.FilePath;
            }
        }
        private static void PlacePlatform(List<Component2> componentList, WalkwayPlatform platform, AssemblyDoc assemblyDoc)
        {
            Component2 platformComponent = GetInstance(componentList, 1); Debug.WriteLine(" (platform)");

            if (platformComponent == null && platform.Platform.PartNo.Dynamic != null)
            {
                platformComponent = InsertComponent(platform.Platform.FilePath, assemblyDoc);
            }
            
            if (platformComponent != null)
            {
                // Set location
                ApplyPositionInformation(platformComponent);

                // Release COM object
                Optimize.Release(ref platformComponent);
            }
        }
        private static void PlaceHandRails(List<Component2> componentList, HandRail rail, AssemblyDoc assemblyDoc, double walkwayWidth, double railHeight)
        {
            Component2 leftRail = GetInstance(componentList, 1); Debug.WriteLine(" (left rail)");
            Component2 rightRail = GetInstance(componentList, 2); Debug.WriteLine(" (right rail)");

            if (leftRail == null && rail.Rail.PartNo.Dynamic != null)
            {
                leftRail = InsertComponent(rail.Rail.FilePath, assemblyDoc);
            }

            if (leftRail != null)
            {
                // Set location
                X_Translation(-walkwayWidth / 2);
                Y_Translation(railHeight);
                ApplyPositionInformation(leftRail);

                // Release COM object
                Optimize.Release(ref leftRail);
            }




            if (rightRail == null && rail.Rail.PartNo.Dynamic != null)
            {
                rightRail = InsertComponent(rail.Rail.FilePath, assemblyDoc);
            }

            if (rightRail != null)
            {
                // Set location
                X_Translation(walkwayWidth / 2);
                Y_Translation(railHeight);
                Y_Axis_180_Degree_Rotate();
                ApplyPositionInformation(rightRail);

                // Release COM object
                Optimize.Release(ref rightRail);
            }
        }
        private static void PlaceSupportBeams(List<Component2> componentList, Support support, AssemblyDoc assemblyDoc, double floorHeight, double plenumCenterWidth)
        {
            Component2 leftBeam = GetInstance(componentList, 1); Debug.WriteLine(" (left beam)");
            Component2 rightBeam = GetInstance(componentList, 2); Debug.WriteLine(" (right beam)");

            if (leftBeam == null && support.Reference.PartNo.Dynamic != null)
            {
                leftBeam = InsertComponent(support.Reference.FilePath, assemblyDoc);
            }

            if (leftBeam != null)
            {
                // Set location
                Y_Translation(-floorHeight - support.Reference.StringerDepth);
                Z_Translation(plenumCenterWidth / 2);
                ApplyPositionInformation(leftBeam);

                // Release COM object
                Optimize.Release(ref leftBeam);
            }




            if (rightBeam == null && support.Reference.PartNo.Dynamic != null)
            {
                rightBeam = InsertComponent(support.Reference.FilePath, assemblyDoc);
            }

            if (rightBeam != null)
            {
                // Set location
                Y_Translation(-floorHeight - support.Reference.StringerDepth);
                Z_Translation(-plenumCenterWidth / 2);
                ApplyPositionInformation(rightBeam);

                // Release COM object
                Optimize.Release(ref rightBeam);
            }

        }



        // Selection Tools
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
            AddChildrenToSelection(selectedComponents);  // Assuming AddChildren is your custom method

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
                        value.StaticPartNo == Platform_PartNo_Static ||
                        value.StaticPartNo == "Bank" ||
                        value.StaticPartNo == Handrail_PartNo_Static ||
                        value.StaticPartNo == SupportAssembly_PartNo_Static ||
                        value.StaticPartNo == SupportBeamWeldment_PartNo_Static))
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
        internal static void AddParentToSelection(Dictionary<Component2, (string FilePath, string StaticPartNo, string DynamicPartNo, char? Bank)> filteredComponents)
        {
            var updatedComponents = new Dictionary<Component2, (string, string, string, char?)>();
            var distinctValues = new HashSet<(string, string, string, char?)>();

            foreach (var entry in filteredComponents)
            {
                Component2 component = entry.Key.GetParent();

                if (component == null)
                {
                    SelectionMgr selectionMgr = SW.IActiveDoc2.ISelectionManager;

                    SW.IActiveDoc2.Extension.SelectByID2(
                        Path.GetFileName(SW.IActiveDoc2.GetPathName()),
                        "COMPONENT", 0, 0, 0, false, 0, null, 0);

                    object selectedObj = selectionMgr.GetSelectedObject6(1, -1);
                    component = selectedObj as Component2;
                }

                string fileName = component.GetPathName();
                string staticPartNo = SW.GetConfigurationNames(fileName)[0];
                string dynamicPartNo = Path.GetFileNameWithoutExtension(fileName).Split('-').Last();
                char? bankChar = Path.GetFileNameWithoutExtension(fileName).Split('-').ElementAtOrDefault(1)?.ElementAtOrDefault(2);

                var currentValue = (fileName, staticPartNo, dynamicPartNo, bankChar);

                if (distinctValues.Add(currentValue))
                {
                    updatedComponents[component] = currentValue;
                    Debug.WriteLine($"updateLocation selected. Adding {Path.GetFileNameWithoutExtension(fileName)}");
                }
            }

            // Update the main dictionary with the new distinct values
            foreach (var entry in updatedComponents)
            {
                filteredComponents[entry.Key] = entry.Value;
            }
        }
        internal static char GetBankFrom(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            char bank;

            bool allSameBank = platformComponents.Values.Select(v => v.Bank).Distinct().Count() == 1;
            if (allSameBank)
            {
                bank = (char)platformComponents.Values.First().Bank;
                return bank;
            }
            else
            {
                Debug.WriteLine("Multi-bank selection not supported. Exiting the program.");
                System.Environment.Exit(0);
                return ' ';
            }
        }
        internal static Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> FilterComponents(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, Dictionary<string, string> staticPartDescriptions)
        {
            // Dictionary to add elidgable components to
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
            }

            // Handle null tuple
            if (filteredComponents.Count == 0)
            {
                System.Windows.MessageBox.Show("No valid selections made.", "Component Selector", (MessageBoxButton)MessageBoxButtons.OK, (MessageBoxImage)MessageBoxIcon.Error);
                Debug.WriteLine("No valid selections made. Exiting the program.");
                //System.Environment.Exit(0);
            }

            return filteredComponents;
        }


        // File Creation
        internal static AssemblyDoc AddNew_Bank()
        {
            string assemblyFileName = $"{Project}-28{Bank}.SLDASM";
            string drawingFileName = $"{Project}-28{Bank}.SLDDRW";

            string assemblyTemplateFile = $@"{TemplateFolderPath}\JOBNO-28A.SLDASM";
            string drawingTemplateFile = $@"{TemplateFolderPath}\JOBNO-28A.SLDDRW";

            string assemblyDesktopPath = $@"{DesktopFolderPath}\{assemblyFileName}";
            string drawingDesktopPath = $@"{DesktopFolderPath}\{drawingFileName}";

            // Determine next available bank
            while (System.IO.File.Exists(assemblyDesktopPath))
            {
                Debug.WriteLine($"{assemblyDesktopPath} already exists");
                Bank++;
                assemblyDesktopPath = $@"{DesktopFolderPath}\{assemblyFileName}";
            }

            // Create working folder on desktop
            Directory.CreateDirectory(DesktopFolderPath);

            // Create bank assembly
            CopyAsReadWrite(assemblyTemplateFile, assemblyDesktopPath);
            Debug.WriteLine($"Created {assemblyFileName}");

            // Create bank drawing
            CopyAsReadWrite(drawingTemplateFile, drawingDesktopPath);
            Debug.WriteLine($"Created {drawingFileName}");

            // Open bank assembly
            return Open(assemblyDesktopPath) as AssemblyDoc;
        }
        internal static string CreateNew_ComponentFile(string staticPartNo)
        {
            string templateASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string templatePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string template = System.IO.File.Exists(templateASM) ? templateASM : templatePRT;

            string extension = Path.GetExtension(template);

            if (extension == ".SLDASM")
            {
                return HandleAssemblyFile(template).ToString();
            }
            else if (extension == ".SLDPRT")
            {
                return HandlePartFile(template).ToString();
            }
            else
            {
                Debug.WriteLine("File type not recognized");
                return "-1";
            }
        }
        private static int HandlePartFile(string templateFile)
        {
            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Project}-28{Bank}-{partNo}.SLDPRT";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"Created {destinationFile} from template file {templateFile}");
            }
            return partNo;
        }
        private static int HandleAssemblyFile(string templateFile)
        {
            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Project}-28{Bank}-{partNo}.SLDASM";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"Created {destinationFile} from template file {templateFile}");
            }
            return partNo;
        }
        internal static string CreateNew_SubComponentFile(string staticPartNo)
        {
            string templateFile = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";

            char partNo1 = 'A';
            char partNo2 = ' ';

            while (true)
            {
                partNo1 = SkipInvalidChars(partNo1);
                partNo2 = SkipInvalidChars(partNo2);

                string fileName = $"{partNo1}{(partNo2 != ' ' ? partNo2.ToString() : "")}.SLDPRT";
                string desktopFile = $@"{DesktopFolderPath}\{Project}-28{Bank}-{fileName}";

                if (!System.IO.File.Exists(desktopFile))
                {
                    CopyAsReadWrite(templateFile, desktopFile);
                    Debug.WriteLine($"Created {desktopFile} from template file {templateFile}");
                    return $"{partNo1}{(partNo2 != ' ' ? partNo2.ToString() : "")}";
                }

                (partNo1, partNo2) = IncrementPartNo(partNo1, partNo2);
            }
        }
        private static (char, char) IncrementPartNo(char partNo1, char partNo2)
        {
            if (partNo1 == 'Z')
            {
                return ('A', 'A');
            }
            else if (partNo2 != ' ' && partNo2 != 'Z')
            {
                return (partNo1, (char)(partNo2 + 1));
            }
            else if (partNo2 == 'Z')
            {
                return ((char)(partNo1 + 1), 'A');
            }
            else
            {
                return ((char)(partNo1 + 1), ' ');
            }
        }
        private static char SkipInvalidChars(char partNo)
        {
            while (partNo == 'I' || partNo == 'O')
            {
                partNo++;
            }
            return partNo;
        }
        private static int GetUniquePartNo()
        {
            int partNo = 1;

            while (true)
            {
                string desktopFile_Part = $@"{DesktopFolderPath}\{Project}-28{Bank}-{partNo}.SLDPRT";
                string desktopFile_Assembly = $@"{DesktopFolderPath}\{Project}-28{Bank}-{partNo}.SLDASM";

                if (!System.IO.File.Exists(desktopFile_Part) && !System.IO.File.Exists(desktopFile_Assembly))
                {
                    return partNo;
                }
                partNo++;
            }
        }
        internal static void CopyAsReadWrite(string sourceFile, string destinationFile)
        {
            System.IO.File.Copy(sourceFile, destinationFile);
            FileAttributes attributes = System.IO.File.GetAttributes(destinationFile);
            attributes &= ~FileAttributes.ReadOnly;
            System.IO.File.SetAttributes(destinationFile, attributes);
        }


        // Data Management
        internal static List<Component2> FindMatchingComponents(string filePath_OfTarget, AssemblyDoc assemblyDoc_OfSeachLocation)
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
        internal static string GetDynamicPartNo(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> components, params string[] staticPartNos)
        {
            return components
                .Where(entry => staticPartNos.Contains(entry.Value.StaticPartNo))
                .Select(entry => entry.Value.DynamicPartNo)
                .FirstOrDefault();
        }


        // Drawing Creation
        internal static void ReplaceDrawingReference(string dynamicPartNo, string drawingToBeModified, string staticPartNo)
        {
            string oldReferenceASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string oldReferencePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string oldReference = System.IO.File.Exists(oldReferenceASM) ? oldReferenceASM : oldReferencePRT;

            string newReferenceASM = $@"{DesktopFolderPath}\{Project}-28{Bank}-{dynamicPartNo}.SLDASM";
            string newReferencePRT = $@"{DesktopFolderPath}\{Project}-28{Bank}-{dynamicPartNo}.SLDPRT";
            string newReference = System.IO.File.Exists(newReferenceASM) ? newReferenceASM : newReferencePRT;

            bool platform_Check = false;

            try
            {
                platform_Check = SW.ReplaceReferencedDocument(drawingToBeModified, oldReference, newReference);
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"Exception when replacing reference: {ex.Message}");
            }

            Debug.WriteLine($"   Replaced sheet reference to {Project}-28{Bank}-{dynamicPartNo}: {platform_Check}");
            if (platform_Check == false)
            {
                bool check1 = System.IO.File.Exists(oldReference) ? true : false;
                bool check2 = System.IO.File.Exists(drawingToBeModified) ? true : false;
                bool check3 = System.IO.File.Exists(newReference) ? true : false;
                Debug.WriteLine($"Could not replace reference:" + "\n" +
                                $"                            {oldReference} " +
                                "\n" + $"                               File exists: {check1}" + "\n" + "\n" +
                                $"                                 in drawing " + "\n" +
                                $"                            {drawingToBeModified} " +
                                "\n" + $"                               File exists: {check2}" + "\n" + "\n" +
                                $"                                 with reference " + "\n" +
                                $"                            {newReference}" +
                                "\n" + $"                               File exists: {check3}" + "\n" + "\n");

            }
        }
        internal static void MigrateSheetsToBankDrawing(DrawingDoc tempDrawingDoc)
        {
            // Accessor for ModelDoc2 interface
            ModelDoc2 tempModelDoc = tempDrawingDoc as ModelDoc2;

            // Dictionary of sheetNames and their associated sheetViewNames
            Dictionary<string, List<string>> listOf_SavedViews_PerSheet = new Dictionary<string, List<string>>();

            // Get all sheets in temporary platform drawing
            string[] sheetNames_PlatformDrawing = tempDrawingDoc.GetSheetNames();
            foreach (var sheetName in sheetNames_PlatformDrawing)
            {
                // Instantiate a new list of view names
                List<string> viewNames = new List<string>();

                // Collect all views
                object[] views = tempDrawingDoc.Sheet[sheetName].GetViews();
                foreach (var viewObj in views)
                {
                    // Add each view to the list
                    IView view = viewObj as IView;
                    string readName = view.GetName2();
                    viewNames.Add(readName);
                    Debug.WriteLine($"      Added view {readName} to sheet {sheetName}");
                    Optimize.Release(ref view);
                }

                // Add sheet to dictioanry with assoicated views
                listOf_SavedViews_PerSheet[sheetName] = viewNames;

                // Select the sheet to prepare for copy-paste
                tempModelDoc.Extension.SelectByID2(sheetName, "SHEET", 0, 0, 0, true, 0, null, 0);
            }

            // Copy (all) selected sheets
            tempModelDoc.EditCopy();

            // Delete temporary platform drawing
            string tempPath = tempModelDoc.GetPathName();
            Optimize.Release(ref tempModelDoc);
            Optimize.Release(ref tempDrawingDoc);
            Close(tempPath);
            System.IO.File.Delete(tempPath);







            // Open bank drawing and paste copied sheets
            string bank_DesktopDrawingPath = $@"{DesktopFolderPath}\{Project}-28{Bank}.SLDDRW";
            DrawingDoc bank_DesktopDrawing = OpenDrawing(bank_DesktopDrawingPath);
            ModelDoc2 bank_ModelDoc2 = bank_DesktopDrawing as ModelDoc2;
            bank_ModelDoc2.Paste();

            // All sheets in bank desktop drawing
            string[] read_SheetNames = bank_DesktopDrawing.GetSheetNames();

            // Only copied sheets in bank desktop drawing
            List<string> copied_SheetNames = new List<string>();

            // Iterate through all sheets in bank desktop drawing 
            for (int i = 0; i < read_SheetNames.Length; i++)
            {
                // Copied sheets will be suffixed with: (2)
                int indexOfParenthesis = read_SheetNames[i].IndexOf('(');

                // For all sheets containing this suffix...
                if (indexOfParenthesis != -1)
                {
                    // Trim suffix from sheet name
                    string write_Sheetname = read_SheetNames[i].Substring(0, indexOfParenthesis).Trim();
                    RenameSheet(bank_DesktopDrawing, read_SheetNames[i], write_Sheetname);

                    // Add sheet to list of copied sheets
                    copied_SheetNames.Add(write_Sheetname);
                }
            }

            // Iterate through only sheets that have been copied
            foreach (var sheetName in copied_SheetNames)
            {
                // Check if the sheet for the given sheetName exists
                var sheet = bank_DesktopDrawing.Sheet[sheetName];
                if (sheet == null)
                {
                    Debug.WriteLine($"Sheet {sheetName} does not exist in bank_DesktopDrawing.");
                    continue; // Skip the rest of the loop for this sheetName
                }

                // Collect all views on each sheet
                object[] views = sheet.GetViews();

                // Verify if the sheet exists in the original drawing and views count is the same
                if (listOf_SavedViews_PerSheet.ContainsKey(sheetName) && views.Length == listOf_SavedViews_PerSheet[sheetName].Count)
                {
                    for (int i = 0; i < views.Length; i++)
                    {
                        // Assign the original name back to the copied sheet views
                        string savedViewName = listOf_SavedViews_PerSheet[sheetName][i];

                        // trim everything to the right of the ":" but keep the colon
                        int indexOfColon = savedViewName.IndexOf(":");
                        if (indexOfColon != -1)
                        {
                            savedViewName = savedViewName.Substring(0, indexOfColon + 1);
                        }
                        string uniqueViewName = $"{savedViewName}{sheetName}";

                        IView view = views[i] as IView;
                        view.SetName2(uniqueViewName);
                        Debug.WriteLine($"      View name {view.GetName2()} on sheet {sheetName} set to {uniqueViewName}");
                    }
                }
                else
                {
                    Debug.WriteLine($"Could not find sheet {sheetName} in listOf_SavedViews_PerSheet");
                    Debug.WriteLine($"Printing listOf_SavedViews_PerSheet:");
                    foreach (var item in listOf_SavedViews_PerSheet)
                    {
                        Debug.WriteLine($"Key: {item.Key}");

                        foreach (var viewName in item.Value)
                        {
                            Debug.WriteLine($"   Value: {viewName}");
                        }
                    }
                }
            }

            // Save and close bank desktop drawing
            Debug.WriteLine("");
            //DrawingToolz.DrawingToolz.SortSheetsInDrawing();
            SaveDrawing(bank_ModelDoc2);
            Optimize.Release(ref bank_ModelDoc2);
            Optimize.Release(ref bank_DesktopDrawing);
            Close(bank_DesktopDrawingPath);
        }
        private static void SaveDrawing(ModelDoc2 modelDoc2)
        {
            modelDoc2.Save3((int)swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0);
            string docName = modelDoc2.GetTitle();
            string[] partNumber = docName.Split('-');
            Debug.WriteLine($"Saved file {partNumber[1]}");
        }
        private static void RenameSheet(DrawingDoc drawingDoc, string read_SheetName, string write_SheetName)
        {
            drawingDoc.Sheet[read_SheetName].SetName(write_SheetName);
            Debug.WriteLine($"   Sheet: {read_SheetName}, has been renamed to {write_SheetName}");
        }
        internal static void RenameSheet(string sheetName, string dynamicPartNo, DrawingDoc drawingDoc)
        {
            drawingDoc.Sheet[sheetName].SetName($"{Project}-28{Bank}-{dynamicPartNo}");
            Debug.WriteLine($"   Sheet {sheetName} has been renamed to {Project}-28{Bank}-{dynamicPartNo}");
        }
        internal static void DeleteSheet(string sheetName, DrawingDoc drawingDoc)
        {
            ModelDoc2 modelDoc2 = drawingDoc as ModelDoc2;
            //drawingDoc.ActivateSheet(sheetName);
            bool checkSelection = modelDoc2.Extension.SelectByID2(sheetName, "SHEET", 0, 0, 0, false, 0, null, 0);
            bool checkDeletion = modelDoc2.Extension.DeleteSelection2(0);
            Debug.WriteLine($"   Sheet {sheetName} deleted: {checkDeletion}");
        }
        internal static string CreateTemporary_PlatformDesktopDrawing(string templatePartNo, string platform_PartNo)
        {
            string fileName = $"{Project}-28{Bank}-{platform_PartNo}.SLDDRW";

            string platform_TemplateDrawing = $@"{TemplateFolderPath}\JOBNO-{templatePartNo}.SLDDRW";
            string platform_DesktopDrawing = $@"{DesktopFolderPath}\{fileName}";

            System.IO.File.Copy(platform_TemplateDrawing, platform_DesktopDrawing);
            Debug.WriteLine($"Created {fileName}");

            FileAttributes attributes = System.IO.File.GetAttributes(platform_DesktopDrawing);
            attributes &= ~FileAttributes.ReadOnly;
            System.IO.File.SetAttributes(platform_DesktopDrawing, attributes);

            return platform_DesktopDrawing;
        }
        internal static DrawingDoc OpenDrawing(string filePath)
        {
            ModelDoc2 modelDoc2 = Open(filePath);

            ModelView modelView = modelDoc2.ActiveView as ModelView;
            modelView.EnableGraphicsUpdate = false;

            IFeatureManager featureManager = modelDoc2.FeatureManager;
            featureManager.EnableFeatureTree = false;

            SetProperty("DrawnBy", Initials, modelDoc2);
            SetProperty("DrawnDate", DateTime.Now.ToString("M/d/yyyy"), modelDoc2);

            return modelDoc2 as DrawingDoc;
        }



        // Assembly Tools
        internal static Component2 GetInstance(List<Component2> componentList, int instance)
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
        internal static void ApplyPositionInformation(Component2 component)
        {
            DevTools.InchesToMeters(DevTools.PositionMaxtrix);
            MathTransform mathTransform = SW.GetMathUtility().CreateTransform(DevTools.PositionMaxtrix);
            component.Transform2 = mathTransform;

            DevTools.PositionMaxtrix = new double[16]
            {
                1,0,0,0,
                1,0,0,0,
                1,0,0,0,
                1,0,0,0
             };

            Debug.WriteLine($"Position set: {component.Name2}");

            Optimize.Release(ref mathTransform);
        }
        internal static void X_Translation(double value)
        {
            DevTools.PositionMaxtrix[9] = value;
        }
        internal static void Y_Translation(double value)
        {
            DevTools.PositionMaxtrix[10] = value;
        }
        internal static void Z_Translation(double value)
        {
            DevTools.PositionMaxtrix[11] = value;
        }
        internal static void Y_Axis_180_Degree_Rotate()
        {
            DevTools.PositionMaxtrix[0] = DevTools.PositionMaxtrix[0] * Math.Cos(Math.PI);  //  -1
            DevTools.PositionMaxtrix[1] = DevTools.PositionMaxtrix[1] * Math.Sin(Math.PI);  //   0
            DevTools.PositionMaxtrix[3] = DevTools.PositionMaxtrix[3] * Math.Sin(Math.PI);  //   0
            DevTools.PositionMaxtrix[4] = DevTools.PositionMaxtrix[4] * Math.Cos(0);     //   1
            DevTools.PositionMaxtrix[8] = DevTools.PositionMaxtrix[8] * Math.Cos(Math.PI);  //  -1
        }
        internal static void Z_Axis_Rotate(double degrees, int quadrant, char refAxis)
        {
            // inputs
            double radians = degrees * (Math.PI / 180);
            char axis = char.ToUpper(refAxis);

            int a = 1;
            int b = 1;
            int c = 1;
            int d = 1;

            switch (quadrant)
            {
                case 1:
                    a = -1; //  -
                    b = -1; //  -
                    c = 1;  //  +
                    d = -1; //  -
                    break;
                case 2:
                    a = 1;  //  +
                    b = -1; //  -
                    c = 1;  //  +
                    d = 1;  //  +
                    break;
                case 3:
                    a = 1;  //  +
                    b = 1;  //  +
                    c = -1; //  -
                    d = 1;  //  +
                    break;
                case 4:
                    a = -1; //  -
                    b = 1;  //  +
                    c = -1; //  -
                    d = -1; //  -
                    break;
                default:
                    Console.WriteLine("Invalid Quadrant Value. Quadrant must be between 1 and 4.");
                    break;
            }

            switch (axis)
            {
                case 'X':
                    DevTools.PositionMaxtrix[0] = a * Math.Cos(radians);
                    DevTools.PositionMaxtrix[1] = b * Math.Sin(radians);
                    DevTools.PositionMaxtrix[3] = c * Math.Sin(radians);
                    DevTools.PositionMaxtrix[4] = d * Math.Cos(radians);
                    break;
                case 'Y':
                    DevTools.PositionMaxtrix[0] = a * Math.Sin(radians);
                    DevTools.PositionMaxtrix[1] = b * Math.Cos(radians);
                    DevTools.PositionMaxtrix[3] = c * Math.Cos(radians);
                    DevTools.PositionMaxtrix[4] = d * Math.Sin(radians);
                    break;
                default:
                    Console.WriteLine("Invalid Axis. Axis must be between either X or Y");
                    break;
            }
        }
        internal static AssemblyDoc OpenAssembly(string filePath)
        {
            ModelDoc2 modelDoc2 = Open(filePath);
            SW.ActivateDoc3(filePath, false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
            return modelDoc2 as AssemblyDoc;
        }
        internal static Component2 InsertComponent(string filePath, AssemblyDoc assemblyDoc)
        {
            Component2 component2 = assemblyDoc.AddComponent5
                (filePath,
                (int)swAddComponentConfigOptions_e.swAddComponentConfigOptions_CurrentSelectedConfig, null,
                false, "Default",
                0, 0, 0);
            Debug.Write($"      Inserted new component:    {component2.Name2}");
            return component2;
        }
        internal static void FixComponentLocations(AssemblyDoc assemblyDoc)
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
        internal static void UnfixComponentLocations(AssemblyDoc assemblyDoc, List<Component2> unfixedComponents)
        {
            foreach (var component in unfixedComponents)
            {
                component.Select2(true, -1);
                assemblyDoc.UnfixComponent();
            }
        }
        internal static List<Component2> ListUnfixedComponents(AssemblyDoc assemblyDoc)
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




        // File Management
        internal static ModelDoc2 Open(string filePath, bool silent = false)
        {
            Debug.WriteLine("\n");
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
                null,
                errors, warnings);

            if (!silent)
            {
                Debug.WriteLine($"{GetPartNumber(filePath)}{extension} opened");
            }

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
        private static string GetPartNumber(string filePath)
        {
            string fileName = Path.GetFileNameWithoutExtension(filePath);
            return fileName.Substring(fileName.IndexOf("-") + 1);
        }
        internal static bool EditDimension(string dimensionName, string treeName, double newValue, ModelDoc2 modelDoc2)
        {
            // Initialize as false. Will be set to true if edit is successful.
            bool editSuccessful = false;

            if (modelDoc2 == null)
            {
                // Log the error instead of throwing an exception
                Debug.WriteLine("Error: ModelDoc2 is null.");
                return false;  // Return false to indicate failure
            }

            string equationName = $"{dimensionName}@{treeName}";
            Dimension dimension = modelDoc2.Parameter(equationName);

            if (dimension != null)
            {
                dimension.SetValue3(newValue, (int)swSetValueInConfiguration_e.swSetValue_UseCurrentSetting, null);
                Debug.WriteLine($"   Dimension {dimension.Name} modified");
                editSuccessful = true;  // Set to true on successful edit
            }
            else
            {
                // Log the error
                Debug.WriteLine($"Error: Dimension {equationName} not found.");
            }

            Optimize.Release(ref dimension);
            return editSuccessful;  // Return the success status
        }
        internal static void EditDimension(string dimensionName, string treeName, double newValue, AssemblyDoc assemblyDoc)
        {
            EditDimension(dimensionName, treeName, newValue, assemblyDoc as ModelDoc2);
        }
        internal static void SetProperty(string property, string value, ModelDoc2 modelDoc2)
        {
            string fileName = modelDoc2.GetPathName();
            string staticPartNo;
            try
            {
                staticPartNo = SW.GetConfigurationNames(fileName)[0];
            }
            catch
            {
                staticPartNo = "";
            }

            CustomPropertyManager customPropertyManager = modelDoc2.Extension.CustomPropertyManager[staticPartNo];

            int messageID = customPropertyManager.Add3(
                property,
                (int)swCustomInfoType_e.swCustomInfoText,
                value,
                (int)swCustomPropertyAddOption_e.swCustomPropertyReplaceValue);

            switch (messageID)
            {
                case 0:
                    Debug.WriteLine($"   Property {property}: Success");
                    break;
                case 1:
                    Debug.WriteLine($"   Property {property}: Failed to add the custom property");
                    break;
                case 2:
                    Debug.WriteLine($"   Property {property}: Existing custom property with the same name has a different type");
                    break;
                case 3:
                    Debug.WriteLine($"   Property {property}: Specified value of the custom property does not match the specified type");
                    break;
            }
        }
        internal static void SetProperty(string property, char value, ModelDoc2 modelDoc2)
        {
            SetProperty(property, value.ToString(), modelDoc2);
        }
        internal static void SetProperty(string property, string value, AssemblyDoc assemblyDoc)
        {
            SetProperty(property, value, assemblyDoc as ModelDoc2);
        }
        internal static void SetProperty(string property, char value, AssemblyDoc assemblyDoc)
        {
            SetProperty(property, value.ToString(), assemblyDoc as ModelDoc2);
        }
        internal static void Close(string filePath)
        {
            SW.CloseDoc(filePath);
            string fileName = Path.GetFileNameWithoutExtension(filePath);
            string partNumber = fileName.Substring(fileName.IndexOf("-") + 1);
            Debug.WriteLine($"Closed {partNumber}");
        }
        internal static List<string> SaveAndCloseAllDocuments(out string originalActivePath)
        {
            List<string> openDocPaths = new List<string>();
            ModelDoc2 activeDoc = SW.ActiveDoc;

            originalActivePath = activeDoc.GetPathName();

            Debug.WriteLine("\n");
            while (activeDoc != null)
            {
                // Record the file path of the open document
                openDocPaths.Add(activeDoc.GetPathName());
                Debug.WriteLine($"Added {Path.GetFileNameWithoutExtension(activeDoc.GetPathName())} to the list of open docs");

                // Save the document
                activeDoc.Save();

                // Close the document
                Close(activeDoc.GetPathName());

                // Move to the next open document
                activeDoc = SW.ActiveDoc;
            }

            return openDocPaths;
        }
        public static void SaveEverything()
        {
            object[] componentObj = (SW.IActiveDoc2 as AssemblyDoc).GetComponents(false);

            foreach (var obj in componentObj)
            {
                Component2 component2 = obj as Component2;
                ModelDoc2 modelDoc2 = component2.GetModelDoc2();
                modelDoc2.Save3(1, 0, 0);
                Optimize.Release(ref modelDoc2);
            }

            SW.IActiveDoc2.Save3(1, 0, 0);
        }


        // Band-aids
        public static void AddNew(string filePath)
        {
            AssemblyDoc assemblyDoc = SW.IActiveDoc2 as AssemblyDoc;

            DevTools.DisablePartUI();
            Open(filePath);
            DevTools.EnablePartUI();

            Component2 component2 = InsertComponent(filePath, assemblyDoc);

            Close(filePath);
            Optimize.Release(ref component2);
            Optimize.Release(ref assemblyDoc);
        }
        public static void AddNewComponent(string partNumber)
        {
            string filePath = $@"{DesktopFolderPath}\{Project}-28{Bank}-{CreateNew_ComponentFile(partNumber)}.SLDPRT";

            AddNew(filePath);
        }
        public static void AddNewSubComponent(string partNumber)
        {
            string filePath = $@"{DesktopFolderPath}\{Project}-28{Bank}-{CreateNew_SubComponentFile(partNumber)}.SLDPRT";

            AddNew(filePath);
        }

        //---------------------//


        // Static Read-Only Properties
        internal static string TemplateFolderPath => @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Walkway";
        internal static string DesktopFolderPath
        {
            get
            {
                string desktopPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop);
                return Path.Combine(desktopPath, $"{Project}-28{Bank}");
            }
        }
        internal static double WeldGap => 0.0625;
        internal static double AssemblyClearance => 0.125;
        internal static double Clearance => 0.25;
        internal static double HoleDiameter => 0.75;
        internal static double InterUnitGap => 6;

        internal static string Platform_PartNo_Static => "1336";
        internal static string StringerL_PartNo_Static => "1336L";
        internal static string StringerC_PartNo_Static => "1336C";
        internal static string EndAngle_PartNo_Static => "1337L";
        internal static string EndChannel_PartNo_Static => "1337C";
        internal static string GratingPrimary_PartNo_Static => "1338";
        internal static string GratingFiller_PartNo_Static => "1338F";
        internal static string Strut_PartNo_Static => "1339";

        internal static string Handrail_PartNo_Static => "1341";
        internal static string Rail_Top_PartNo_Static => "1341P";
        internal static string Rail_Mid_PartNo_Static => "1342";
        internal static string Post_Left_PartNo_Static => "1343L";
        internal static string Post_Right_PartNo_Static => "1343R";
        internal static string Post_Mid_PartNo_Static => "1344";
        internal static string TopPlate_PartNo_Static => "1371";

        internal static string SupportAssembly_PartNo_Static => "1326A";
        internal static string SupportBeamWeldment_PartNo_Static => "1326";
        internal static string SupportBeamPart_PartNo_Static => "1326P";
        internal static string EndPlate_PartNo_Static => "1327";
        internal static string BraceClip_PartNo_Static => "1328";
        internal static string KneeBrace_PartNo_Static => "1331";
        internal static string BraceSpacer_PartNo_Static => "132";
    }
}