using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using mTools = Tools.ModelTools;
using aTools = ModelTools.AssemblyTools;
using cTools = ModelTools.ReleaseCOM;
using ModelTools;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.Remoting.Metadata.W3cXsd2001;
using static FileTools.Base.Part;

namespace FileTools
{
    public class FileTools
    {
        public FileTools(string assemblyName, string project, char bank, string assemblyNumber, string initials)
        {
            SetCoreValues(assemblyName, project, bank, assemblyNumber, initials);
        }
        public FileTools(string assemblyName, string project, char bank, string assemblyNumber, string initials,
                         string customer, string client, string location, string purchaseOrder, string itemNumber)
        {
            SetCoreValues(assemblyName, project, bank, assemblyNumber, initials);
            Customer = customer;
            Client = client;
            Location = location;
            PurchaseOrder = purchaseOrder;
            ItemNumber = itemNumber;
        }
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        private string AssemblyName { get; set; }
        public string DesktopFolderPath { get; set; }
        private string TemplateFolderPath { get; set; }
        public string Project { get; set; }
        public char Bank { get; set; }
        public string AssemblyNumber { get; set; }
        public string Initials { get; set; }
        private string Customer { get; set; }
        private string Client { get; set; }
        private string Location { get; set; }
        private string PurchaseOrder { get; set; }
        private string ItemNumber { get; set; }
        private string PartNo { get; set; }


        private void SetCoreValues(string assemblyName, string project, char bank, string assemblyNumber, string initials)
        {
            AssemblyName = assemblyName;
            Project = project;
            Bank = bank;
            AssemblyNumber = assemblyNumber;
            DesktopFolderPath =
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop) +
                $@"\{Project}-{AssemblyNumber}{Bank}";
            TemplateFolderPath =
                $@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\{AssemblyName}".ToLower();
            Initials = initials;
        }

        public string GetCurrentDisplayStateName(ModelDoc2 modelDoc)
        {
            ConfigurationManager configManager = modelDoc.ConfigurationManager;
            Configuration activeConfig = configManager.ActiveConfiguration;

            string[] displayStateNames = activeConfig.GetDisplayStates() as string[];

            // Assuming you want the first display state name.
            // Modify as needed if looking for a specific state.
            string currentDisplayStateName = displayStateNames[0];

            Debug.WriteLine($"Current Display State Name: {currentDisplayStateName}");

            return currentDisplayStateName;
        }
        public string ExtractPartNumber(string input)
        {
            int start = input.IndexOf('-') + 1;
            int end = input.LastIndexOf('.');
            if (start > 0 && end > start)
            {
                return input.Substring(start, end - start);
            }
            return input;
        }

        public ModelDoc2 OpenSilent(string filePath, string configurationName)
        {
            ModelDoc2 modelDoc2 = OpenDocument(filePath, configurationName);

            SetProperty("PartNo", ExtractPartNumber(Path.GetFileName(filePath)), modelDoc2);
            SetProperty("Title", GetConfigurationTitle(modelDoc2), modelDoc2);

            return modelDoc2;
        }



        public enum RawMaterial
        {
            Plate,
            Beam,
            Angle
        }
        public enum MaterialSpec
        {
            A36,
            A572_50,
            A1011_33,
            A992
        };

        public void AddRawMaterialInfo(RawMaterial shape, Spec material, string size, ModelDoc2 modelDoc2)
        {
            if (shape == RawMaterial.Beam)
                material = Spec.A992;

            if (shape == RawMaterial.Plate && size == "0.1344")
                material = Spec.A1011_33;


            string number;
            string description;
            if (JDE.TryGetValue((size, material), out var value))
            {
                number = value.RM;
                description = value.Description;
            }
            else
            {
                number = "N/A";
                description = $"{shape}_{size}_{material}";
            }
            SetProperty("RM", number, modelDoc2);
            SetProperty("RMDesc", description, modelDoc2);
            SetMaterial("Galvanized Steel", modelDoc2);

        }

        Dictionary<(string THK, Spec), (string RM, string Description)> JDE = new Dictionary<(string THK, Spec), (string RM, string Description)>
        {
            // Sheet
            { ("0.1344",            Spec.A1011_33),     ("11110-HPC",    "SHEET_10GA_A1011-33") },


            // Plate
            { ("0.1875",            Spec.A36),          ("11399",       "PLATE_3/16\"_A36") },
            { ("0.1875",            Spec.A572_50),      ("60015-HPC",   "PLATE_3/16\"_A572_50") },

            { ("0.25",              Spec.A36),          ("11449-HPC",   "PLATE_1/4\"_A36_72\"WIDE") },
            { ("0.25",              Spec.A572_50),      ("60038",       "PLATE_1/4\"_A572_50") },

            { ("0.3125",            Spec.A36),          ("11519",       "PLATE_5/16\"_A36") },
            { ("0.3125",            Spec.A572_50),      ("54877-HPC",   "PLATE_5/16\"_A572_50") },

            { ("0.375",             Spec.A36),          ("11569",       "PLATE_3/8\"_A36") },
            { ("0.375",             Spec.A572_50),      ("59500",       "PLATE_3/8\"_A572_50") },

            { ("0.5",               Spec.A36),          ("11619",       "PLATE_1/2\"_A36") },
            { ("0.5",               Spec.A572_50),      ("54397",       "PLATE_1/2\"_A572_50") },


            // Beam
            { ("W6x15",             Spec.A992),         ("13011-HPC",   "BEAM_W_6x15_A992") },
            { ("W6x20",             Spec.A992),         ("13012-HPC",   "BEAM_W_6x20_A992") },
            { ("W6x25",             Spec.A992),         ("13015-HPC",   "BEAM_W_6x25_A992") },
            { ("W8x31",             Spec.A992),         ("13027-HPC",   "BEAM_W_8x31_A992") },


            // Angle
            { ("L2.5x2.5x0.1875",   Spec.A36),          ("12021",       "ANGLE_2-1/2\"x2-1/2\"x3/16\"_A36") },
            { ("L2.5x2.5x0.1875",   Spec.A572_50),      ("30592",       "ANGLE_2-1/2\"x1-1/2\"x3/16\"_A572 50") },

            { ("L3x2x0.1875",       Spec.A36),          ("12132",       "ANGLE_3\"x2\"x3/16\"_A36") },
            { ("L3x2x0.1875",       Spec.A572_50),      ("60014-HPC",   "ANGLE_3\"x2\"x3/16\"_A572-50") },

            { ("L3x3x0.1875",       Spec.A36),          ("12026",       "ANGLE_3\"x3\"x3/16\"_A36") },
            { ("L3x3x0.1875",       Spec.A572_50),      ("54949",       "ANGLE_3\"x3\"x3/16\"_A572-50") },
        };


        public void SetMaterial(string material, ModelDoc2 modelDoc2)
        {
            PartDoc partDoc = modelDoc2 as PartDoc;

            MaterialVisualPropertiesData swMatVisPrps = partDoc.GetMaterialVisualProperties();
            swMatVisPrps.ApplyAppearance = false;
            partDoc.SetMaterialVisualProperties(swMatVisPrps, (int)swInConfigurationOpts_e.swAllConfiguration, "");

            string configName = modelDoc2.GetConfigurationNames()[0];
            object[] bodiesObj = partDoc.GetBodies2((int)swBodyType_e.swAllBodies, false);
            if (bodiesObj != null)
            {
                foreach (var bodyObj in bodiesObj)
                {
                    Body2 body = (Body2)bodyObj;
                    body.Select2(false, null);
                    int message = body.SetMaterialProperty(configName, "solidworks materials.sldmat", material);
                    modelDoc2.ClearSelection2(true);
                }
            }
        }











        public AssemblyDoc OpenAssembly(string filePath, string configurationName, bool silent = false)
        {
            ModelDoc2 modelDoc2 = OpenDocument(filePath, configurationName);
            SW.ActivateDoc3(filePath, false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
            if (silent)
            {
                modelDoc2.Visible = false;
            }

            SetProperty("PartNo", ExtractPartNumber(Path.GetFileName(filePath)), modelDoc2);
            SetProperty("Title", GetConfigurationTitle(modelDoc2), modelDoc2);
            SetProperty("RMdesc", GetConfigurationTitle(modelDoc2), modelDoc2);
            // RM# and Desc not implemented

            return modelDoc2 as AssemblyDoc;
        }

        private ModelDoc2 OpenDocument(string filePath, string configurationName)
        {
            mTools.DisablePartUI();
            ModelDoc2 modelDoc2 = mTools.Open(filePath, configurationName);
            mTools.EnablePartUI();

            if (modelDoc2 != null)
            {
                Configuration config = modelDoc2.ConfigurationManager.ActiveConfiguration;
                Debug.WriteLine($"{config.Name} - {GetConfigurationTitle(modelDoc2)}");

                // Job info
                SetProperty("Project", Project, modelDoc2);
                SetProperty("Bank", Bank, modelDoc2);
                SetProperty("Customer", Customer, modelDoc2);
                SetProperty("Client", Client, modelDoc2);
                SetProperty("Location", Location, modelDoc2);
                SetProperty("PO", PurchaseOrder, modelDoc2);
                SetProperty("ItemNo", ItemNumber, modelDoc2);

                return modelDoc2;
            }
            else
            {
                Debug.WriteLine($"Attempting to open {Path.GetFileNameWithoutExtension(filePath)} returns null ModelDoc2");
                return null;
            }

        }

        private string GetConfigurationTitle(ModelDoc2 modelDoc2)
        {
            Configuration config = modelDoc2.ConfigurationManager.ActiveConfiguration;
            return config.GetDisplayStates()[0];
        }

        public string GetFilePath(string partNo, string fileType)
        {
            return $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{partNo}.{fileType}";
        }

        public static void CopyAsReadWrite(string sourceFile, string destinationFile)
        {
            File.Copy(sourceFile, destinationFile);
            FileAttributes attributes = File.GetAttributes(destinationFile);
            attributes &= ~FileAttributes.ReadOnly;
            File.SetAttributes(destinationFile, attributes);
        }

        public static string GetFileNameFromPath(string filePath)
        {
            string[] fileNameParts = Path.GetFileNameWithoutExtension(filePath).Split('.');
            return fileNameParts[0];
        }
        public string CreateNew_SubComponentFile(string staticPartNo)
        {
            Debug.WriteLine($"Creating new {staticPartNo} from {TemplateFolderPath}");
            string templateFile = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";

            char partNo1 = 'A';
            char partNo2 = ' ';

            while (true)
            {
                partNo1 = SkipInvalidChars(partNo1);
                partNo2 = SkipInvalidChars(partNo2);

                string fileName = $"{partNo1}{(partNo2 != ' ' ? partNo2.ToString() : "")}.SLDPRT";
                string desktopFile = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{fileName}";

                if (!File.Exists(desktopFile))
                {
                    CopyAsReadWrite(templateFile, desktopFile);
                    //Debug.WriteLine($"Created {desktopFile} from template file {templateFile}");
                    return $"{partNo1}{(partNo2 != ' ' ? partNo2.ToString() : "")}";
                }

                (partNo1, partNo2) = IncrementPartNo(partNo1, partNo2);
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
        public string CreateNew_ComponentFile(string staticPartNo)
        {
            string templateASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string templatePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string template = File.Exists(templateASM) ? templateASM : templatePRT;

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
        private int HandlePartFile(string templateFile)
        {
            string DesktopFolderPath =
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop) +
                $"{Project}-{AssemblyNumber}{Bank}";

            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{partNo}.SLDPRT";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"   Created new {destinationFile} as {Path.GetFileNameWithoutExtension(destinationFile)}");
            }
            return partNo;
        }
        private int HandleAssemblyFile(string templateFile)
        {
            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{partNo}.SLDASM";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"   Created new {destinationFile} as {Path.GetFileNameWithoutExtension(destinationFile)}");
            }
            return partNo;
        }
        public char AddNew_Bank()
        {
            string assemblyFileName = $"{Project}-{AssemblyNumber}{Bank}.SLDASM";
            string drawingFileName = $"{Project}-{AssemblyNumber}{Bank}.SLDDRW";

            string TemplateFolderPath =
                $@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\{AssemblyName}";

            string assemblyTemplateFile = $@"{TemplateFolderPath}\JOBNO-{AssemblyNumber}.SLDASM";
            string drawingTemplateFile = $@"{TemplateFolderPath}\JOBNO-{AssemblyNumber}.SLDDRW";

            string assemblyDesktopPath = $@"{DesktopFolderPath}\{assemblyFileName}";
            string drawingDesktopPath = $@"{DesktopFolderPath}\{drawingFileName}";

            bool fileExists = File.Exists(assemblyDesktopPath);
            // Determine next available bank
            while (fileExists)
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
            //CopyAsReadWrite(drawingTemplateFile, drawingDesktopPath);
            //Debug.WriteLine($"Created {drawingFileName}");

            // Open bank assembly
            mTools.Open(assemblyDesktopPath);

            return Bank;
        }
        private int GetUniquePartNo()
        {
            int partNo = 1;

            while (true)
            {
                string desktopFile_Part = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{partNo}.SLDPRT";
                string desktopFile_Assembly = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{partNo}.SLDASM";

                if (!File.Exists(desktopFile_Part) && !File.Exists(desktopFile_Assembly))
                {
                    return partNo;
                }
                partNo++;
            }
        }
        public string GetPartNoFromAssembly(string staticPartNo, AssemblyDoc assemblyDoc)
        {
            string assemblyDocPath = (assemblyDoc as ModelDoc2).GetPathName();
            string assemblyName = Path.GetFileNameWithoutExtension(assemblyDocPath);
            Debug.WriteLine($"   Looking for part {staticPartNo} in {assemblyName}...");

            object[] componentObjs = assemblyDoc.GetComponents(true);
            if (componentObjs != null)
            {
                foreach (var componentObj in componentObjs)
                {
                    Component2 component = componentObj as Component2;

                    // Skip the component if it's suppressed
                    if (component.GetSuppression() == (int)swComponentSuppressionState_e.swComponentSuppressed)
                    {
                        //Debug.WriteLine($"Skipped suppressed component {component.Name2} in assembly {assemblyName}");
                        ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
                        bool isSelected = modelDoc2.Extension.SelectByID2($"{component.Name2}@{Path.GetFileNameWithoutExtension(modelDoc2.GetPathName())}", "COMPONENT", 0, 0, 0, false, 0, null, 0);
                        if (isSelected)
                        {
                            modelDoc2.Extension.DeleteSelection2(0);
                        }
                        continue;
                    }

                    string componentPath = component.GetPathName();
                    string config = component.ReferencedConfiguration;

                    // Check if the component is not in the TemplateFolderPath
                    if (!componentPath.StartsWith(TemplateFolderPath, StringComparison.OrdinalIgnoreCase))
                    {
                        if (config == staticPartNo)
                        {
                            string partName = Path.GetFileNameWithoutExtension(componentPath);
                            string partNo = aTools.ExtractPartNo(component);
                            Debug.WriteLine($"   Found part {partName} in assembly {assemblyName}");
                            return partNo;
                        }
                    }
                    else
                    {
                        //Debug.WriteLine($"Skipped component {Path.GetFileName(componentPath)} because it is in the template folder.");
                    }
                }
            }

            return null;
        }

        public void PlaceComponent(IComponentInfo component, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            object[] allComponentsArray = assemblyDoc.GetComponents(true);
            if (allComponentsArray != null)
            {
                foreach (var componentObj in allComponentsArray)
                {
                    Component2 assemblyComp = componentObj as Component2;
                    string assemblyCompPath = assemblyComp.GetPathName().ToLower();
                    if (assemblyCompPath.Contains(TemplateFolderPath))
                    {
                        string configName = assemblyComp.ReferencedConfiguration;
                        if (configName == component.StaticPartNo)
                        {
                            SW.ActivateDoc3(modelDoc2.GetPathName(), false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
                            bool check = modelDoc2.Extension.SelectByID2(assemblyComp.Name2 + $"@{Path.GetFileNameWithoutExtension(modelDoc2.GetPathName())}", "COMPONENT", 0, 0, 0, false, 0, null, 0);
                            bool check2 = assemblyDoc.ReplaceComponents2(component.FilePath, component.StaticPartNo, true, 1, false);
                            modelDoc2.Visible = false;
                        }
                    }
                    cTools.Release(ref assemblyComp);
                }
            }

            // Search assembly for existing component instances
            List<Component2> componentList = component.PartNo != null
                ? FindMatchingComponents(component.FilePath, assemblyDoc)
                : new List<Component2>();
            bool insertNew = componentList.Count == 0 && component.PartNo != null;
            componentList.Sort((c1, c2) => c1.Name2.CompareTo(c2.Name2));

            // Add to list or remove components from assembly
            while (componentList.Count < component.Position.Count)
            {
                componentList.Add(null);
            }
            while (componentList.Count > component.Position.Count)
            {
                aTools.DeleteComponentByName(componentList[componentList.Count - 1].Name2, assemblyDoc);
                componentList.RemoveAt(componentList.Count - 1);
            }

            // Insert or get existing components
            for (int i = 0; i < component.Position.Count; i++)
            {
                componentList[i] = insertNew || componentList[i] == null
                    ? mTools.InsertComponent(component.FilePath, assemblyDoc)
                    : GetInstance(componentList, i);
            }

            // Locate components
            for (int i = 0; i < component.Position.Count; i++)
            {
                if (componentList[i] != null)
                {
                    mTools.X_Translation(component.Position[i].TranslationX);
                    mTools.Y_Translation(component.Position[i].TranslationY);
                    mTools.Z_Translation(component.Position[i].TranslationZ);
                    mTools.Rotate(
                        component.Position[i].RotationX,
                        component.Position[i].RotationY,
                        component.Position[i].RotationZ
                    );
                    mTools.SetPosition(componentList[i]);
                }
            }

            // Memory management
            foreach (var item in componentList)
            {
                cTools.Release(item);
            }
            componentList.Clear();
            mTools.Close(component.FilePath);
        }
        public void RenameSheet(string sheetName, string dynamicPartNo, DrawingDoc drawingDoc)
        {
            string newName = $"{Project}-{AssemblyNumber}{Bank}-{dynamicPartNo}";
            Sheet sheet = drawingDoc.Sheet[sheetName];
            sheet.SetName(newName);

            Debug.WriteLine($"   Sheet {sheetName} has been renamed to {Project}-{AssemblyNumber}{Bank}-{dynamicPartNo}");
        }



        public static List<Component2> FindMatchingComponents(string filePath_OfTarget, AssemblyDoc assemblyDoc_OfSearchLocation)
        {
            // List to store matching components
            List<Component2> matchingComponents = new List<Component2>();

            // Extract the file name from the file path
            string[] fileNameParts = Path.GetFileNameWithoutExtension(filePath_OfTarget).Split('.');

            string fileName = fileNameParts[0];

            // Get all components in the assembly
            object[] allComponentsArray = assemblyDoc_OfSearchLocation.GetComponents(false);

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
        public static Component2 GetInstance(List<Component2> componentList, int instance)
        {
            Component2 swComponent = componentList[instance];
            Debug.WriteLine($"Existing component found: {swComponent.Name2} at instance {instance}");
            return swComponent;
        }


        public string CreateDrawing(string templatePartNo)
        {
            string fileName = $"{Project}-{AssemblyNumber}{Bank}.SLDDRW";

            string templateDrawing = $@"{TemplateFolderPath}\JOBNO-{templatePartNo}.SLDDRW";
            string desktopDrawing = $@"{DesktopFolderPath}\{fileName}";

            try
            {
                File.Copy(templateDrawing, desktopDrawing);
                Debug.WriteLine($"Created {fileName}");
                FileAttributes attributes = System.IO.File.GetAttributes(desktopDrawing);
                attributes &= ~FileAttributes.ReadOnly;
                System.IO.File.SetAttributes(desktopDrawing, attributes);
            }
            catch (Exception)
            {
                Debug.WriteLine($"File {fileName} already exists");
            }


            return desktopDrawing;
        }
        public bool CreateDrawing(string templatePartNo, out string desktopDrawing)
        {
            string fileName = $"{Project}-{AssemblyNumber}{Bank}.SLDDRW";

            string templateDrawing = $@"{TemplateFolderPath}\JOBNO-{templatePartNo}.SLDDRW";
            desktopDrawing = $@"{DesktopFolderPath}\{fileName}";

            try
            {
                File.Copy(templateDrawing, desktopDrawing);
                Debug.WriteLine($"Created {fileName}");
                FileAttributes attributes = System.IO.File.GetAttributes(desktopDrawing);
                attributes &= ~FileAttributes.ReadOnly;
                System.IO.File.SetAttributes(desktopDrawing, attributes);
            }
            catch (Exception)
            {
                Debug.WriteLine($"File {fileName} already exists");
                return true;
            }

            return false;
        }


        public bool ReplaceDrawingReference(string dynamicPartNo, string drawingToBeModified, string staticPartNo)
        {
            string oldReferenceASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string oldReferencePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string oldReference = System.IO.File.Exists(oldReferenceASM) ? oldReferenceASM : oldReferencePRT;

            string newReferenceASM;
            if (dynamicPartNo == null)
            {
                newReferenceASM = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}.SLDASM";
            }
            else
            {
                newReferenceASM = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{dynamicPartNo}.SLDASM";
            }
            string newReferencePRT = $@"{DesktopFolderPath}\{Project}-{AssemblyNumber}{Bank}-{dynamicPartNo}.SLDPRT";
            string newReference = System.IO.File.Exists(newReferenceASM) ? newReferenceASM : newReferencePRT;

            bool check = false;

            try
            {
                check = SW.ReplaceReferencedDocument(drawingToBeModified, oldReference, newReference);
            }
            catch (Exception ex)
            {
                Debug.WriteLine($"Exception when replacing reference: {ex.Message}");
            }

            Debug.WriteLine($"   Replaced sheet reference to {Project}-{AssemblyNumber}{Bank}-{dynamicPartNo}: {check}");
            if (check == false)
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
            return check;
        }



        public DrawingDoc OpenDrawing(string filePath)
        {
            ModelDoc2 modelDoc2 = mTools.Open(filePath);

            ModelView modelView = modelDoc2.ActiveView as ModelView;
            modelView.EnableGraphicsUpdate = false;

            IFeatureManager featureManager = modelDoc2.FeatureManager;
            featureManager.EnableFeatureTree = false;

            SetProperty_Static("DrawnBy", Initials, modelDoc2);
            SetProperty_Static("DrawnDate", DateTime.Now.ToString("M/d/yyyy"), modelDoc2);

            return modelDoc2 as DrawingDoc;
        }

        public static void SetProperty_Static(string property, string value, ModelDoc2 modelDoc2)
        {
            string fileName = modelDoc2.GetPathName();
            string[] configurationNames = SW.GetConfigurationNames(fileName);
            if (configurationNames != null)
            {

                for (int i = 0; i < configurationNames.Length; i++)
                {
                    SetProperty_ConfigSpecific_Static(property, value, configurationNames[i], modelDoc2);
                }

            }
            else // .SLDDRW
            {
                SetProperty_ConfigSpecific_Static(property, value, "", modelDoc2);
            }

        }
        public static void SetProperty_ConfigSpecific_Static(string property, string value, string configuration, ModelDoc2 modelDoc2)
        {
            CustomPropertyManager customPropertyManager = modelDoc2.Extension.CustomPropertyManager[configuration];

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
        public static void SetProperty_Static(string property, char value, ModelDoc2 modelDoc2)
        {
            SetProperty_Static(property, value.ToString(), modelDoc2);
        }
        public static void SetProperty_Static(string property, string value, AssemblyDoc assemblyDoc)
        {
            SetProperty_Static(property, value, assemblyDoc as ModelDoc2);
        }
        public static void SetProperty_Static(string property, char value, AssemblyDoc assemblyDoc)
        {
            SetProperty_Static(property, value.ToString(), assemblyDoc as ModelDoc2);
        }


        public void SetProperty(string property, string value, ModelDoc2 modelDoc2)
        {
            string fileName = modelDoc2.GetPathName();
            string[] configurationNames = SW.GetConfigurationNames(fileName);
            if (configurationNames != null)
            {

                for (int i = 0; i < configurationNames.Length; i++)
                {
                    if (property == "PartNo" && configurationNames[i] == "FP")
                        SetProperty_ConfigSpecific(property, value + " FP", configurationNames[i], modelDoc2);
                    else
                        SetProperty_ConfigSpecific(property, value, configurationNames[i], modelDoc2);

                }

            }
            else // .SLDDRW
            {
                SetProperty_ConfigSpecific(property, value, "", modelDoc2);
            }

        }
        public void SetProperty_ConfigSpecific(string property, string value, string configuration, ModelDoc2 modelDoc2)
        {
            CustomPropertyManager customPropertyManager = modelDoc2.Extension.CustomPropertyManager[configuration];

            int messageID = customPropertyManager.Add3(
                property,
                (int)swCustomInfoType_e.swCustomInfoText,
                value,
                (int)swCustomPropertyAddOption_e.swCustomPropertyReplaceValue);

            //switch (messageID)
            //{
            //    case 0:
            //        Debug.WriteLine($"   Property {property}: Success");
            //        break;
            //    case 1:
            //        Debug.WriteLine($"   Property {property}: Failed to add the custom property");
            //        break;
            //    case 2:
            //        Debug.WriteLine($"   Property {property}: Existing custom property with the same name has a different type");
            //        break;
            //    case 3:
            //        Debug.WriteLine($"   Property {property}: Specified value of the custom property does not match the specified type");
            //        break;
            //}
        }
        public void SetProperty(string property, char value, ModelDoc2 modelDoc2)
        {
            SetProperty(property, value.ToString(), modelDoc2);
        }
        public void SetProperty(string property, string value, AssemblyDoc assemblyDoc)
        {
            SetProperty(property, value, assemblyDoc as ModelDoc2);
        }
        public void SetProperty(string property, char value, AssemblyDoc assemblyDoc)
        {
            SetProperty(property, value.ToString(), assemblyDoc as ModelDoc2);
        }
    }
}
