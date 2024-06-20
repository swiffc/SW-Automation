using FileTools.Base;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using static FileTools.Base.Part;
using static FileTools.RawMaterial;
using static ModelTools.ReleaseCOM;
using static System.Net.WebRequestMethods;
using static Tools.ModelTools;
using static FileTools.Properties.Settings;
using static FileTools.Base.MainAssembly;
using static FileTools.Base.SW_Assembly;
using System.ComponentModel;
using ModelTools;
using System.Security.AccessControl;

namespace FileTools
{
    public static class StaticFileTools
    {
        // Public methods
        public static string GetFilePath(string partNo, string fileType)
        {
            return $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{partNo}.{fileType}";
        }
        public static char AddNew_Bank()
        {
            string assemblyFileName = $"{Default.Project}-{AssemblyNumber}{Default.Bank}.SLDASM";
            string drawingFileName = $"{Default.Project}-{AssemblyNumber}{Default.Bank}.SLDDRW";

            string assemblyTemplateFile = $@"{TemplateFolderPath}\JOBNO-{AssemblyNumber}.SLDASM";
            string drawingTemplateFile = $@"{TemplateFolderPath}\JOBNO-{AssemblyNumber}.SLDDRW";

            string assemblyDesktopPath = $@"{DesktopFolderPath}\{assemblyFileName}";
            string drawingDesktopPath = $@"{DesktopFolderPath}\{drawingFileName}";

            bool fileExists = System.IO.File.Exists(assemblyDesktopPath);

            // Determine next available bank
            while (fileExists)
            {
                Debug.WriteLine($"{assemblyDesktopPath} already exists");
                Default.Bank++;
                assemblyDesktopPath = $@"{DesktopFolderPath}\{assemblyFileName}";
            }

            // Create working folder on desktop
            Directory.CreateDirectory(DesktopFolderPath);

            // Create bank assembly
            CopyAsReadWrite(assemblyTemplateFile, assemblyDesktopPath);
            Debug.WriteLine($"Created new [{AssemblyNumber}] as <{Path.GetFileNameWithoutExtension(assemblyFileName)}>");

            // Open bank assembly
            Open(assemblyDesktopPath);

            return Default.Bank;
        }
        public static AssemblyDoc OpenAssembly(string filePath, string configurationName, bool silent = false)
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
        public static string GetPartNoFromAssembly(string staticPartNo, SW_Assembly swAssembly)
        {
            string assemblyDocPath = (swAssembly.AssemblyDoc as ModelDoc2).GetPathName();
            string assemblyName = Path.GetFileNameWithoutExtension(assemblyDocPath);
            Debug.WriteLine($"   [{staticPartNo}] {LastType.Name}.cs was reflected in ({swAssembly.Config})");
            Debug.WriteLine($"      Looking for [{staticPartNo}] in assembly ({swAssembly.Config})...");

            if (swAssembly.ComponentArray != null)
            {
                foreach (var component in swAssembly.ComponentArray)
                {
                    // Skip the component if it's suppressed
                    if (component.GetSuppression() == (int)swComponentSuppressionState_e.swComponentSuppressed)
                    {
                        //Debug.WriteLine($"Skipped suppressed component {component.Name2} in assembly {assemblyName}");
                        ModelDoc2 modelDoc2 = swAssembly.AssemblyDoc as ModelDoc2;
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
                            string partNo = ExtractPartNo(component);
                            Debug.WriteLine($"         Found [{staticPartNo}] in ({swAssembly.Config})");
                            Debug.WriteLine("");
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
        private static List<Component2> AssemblyComponents = new List<Component2>();
        public static string CreateNew_ComponentFile(string staticPartNo)
        {
            string templateASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string templatePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string template;
            if (System.IO.File.Exists(templateASM))
                template = templateASM;
            else if (System.IO.File.Exists(templatePRT))
                template = templatePRT;
            else throw new Exception
                    ("Could not find:" + "\n" +
                    $"{templateASM}" + "\n" +
                    $"{templatePRT}");

            string extension = Path.GetExtension(template);

            if (extension == ".SLDASM")
            {
                return HandleAssemblyFile(template, staticPartNo).ToString();
            }
            else if (extension == ".SLDPRT")
            {
                return HandlePartFile(template, staticPartNo).ToString();
            }
            else
            {
                Debug.WriteLine("File type not recognized");
                return "-1";
            }
        }
        public static string CreateNew_SubComponentFile(string staticPartNo, SW_Assembly swAssembly, string over_ride = null)
        {
            string templateFile = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";

            char partNo1 = 'A';
            char partNo2 = ' ';

            while (true)
            {
                partNo1 = SkipInvalidChars(partNo1);
                partNo2 = SkipInvalidChars(partNo2);

                string fileNameWithoutExtension;

                if (over_ride != null)
                {
                    fileNameWithoutExtension = $"{over_ride}";

                }
                else
                {
                    fileNameWithoutExtension = $"{partNo1}{(partNo2 != ' ' ? partNo2.ToString() : "")}";
                }

                string desktopFile = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{fileNameWithoutExtension}.SLDPRT";

                if (!System.IO.File.Exists(desktopFile))
                {
                    CopyAsReadWrite(templateFile, desktopFile);
                    Debug.WriteLine($"            Created new [{staticPartNo}] as <{Path.GetFileNameWithoutExtension(desktopFile)}>");
                    Debug.WriteLine($"");
                    AssignedComponentPaths.Add(desktopFile);
                    return fileNameWithoutExtension;
                }

                (partNo1, partNo2) = IncrementPartNo(partNo1, partNo2);
            }
        }
        public static string GetPartNoFromDirectory(string staticPartNo, SW_Assembly swAssembly)
        {
            ModelDoc2 modelDoc = swAssembly.AssemblyDoc as ModelDoc2;
            string parentPath = modelDoc.GetPathName();
            Debug.WriteLine($"         Looking for [{staticPartNo}] in directory of ({swAssembly.Config})...");

            // Components in directory
            List<string> directoryComponentPaths = new List<string>();
            string assemblyDirectory = Path.GetDirectoryName(parentPath);
            string[] filePaths = Directory.GetFiles(assemblyDirectory);

            foreach (string filePath in filePaths)
            {
                string extension = Path.GetExtension(filePath).ToLower();
                string fileName = Path.GetFileName(filePath);

                // Check if the file is not temporary and has the right extension
                if (!fileName.StartsWith("~$") && (extension == ".sldprt" || extension == ".sldasm"))
                {
                    directoryComponentPaths.Add(filePath);
                    //Console.WriteLine(" " + filePath);
                }
            }


            // Components loaded into memory
            HashSet<string> loadedComponentPaths = new HashSet<string>
            {
                parentPath
            };

            // Add components loaded via the assembly
            if (swAssembly.ComponentArray != null)
            {
                foreach (var component in swAssembly.ComponentArray)
                {
                    loadedComponentPaths.Add(component.GetPathName());
                }
            }

            // Add components previously assigned in this method
            foreach (var assignedComponentPath in AssignedComponentPaths)
            {
                loadedComponentPaths.Add(assignedComponentPath);
            }

            // Components not loaded into memory
            List<string> unloadedComponentPaths = new List<string>();
            foreach (string directoryComponentPath in directoryComponentPaths)
            {
                if (!loadedComponentPaths.Contains(directoryComponentPath))
                {
                    unloadedComponentPaths.Add(directoryComponentPath);
                }
            }

            // Load unloaded components
            DisablePartUI();
            string partNo = null;
            bool exitOuterLoop = false;

            foreach (string unloadedComponentPath in unloadedComponentPaths)
            {
                ModelDoc2 temp = Open(unloadedComponentPath);
                object[] configsArray = temp.GetConfigurationNames();

                foreach (var configObj in configsArray)
                {
                    string configName = configObj as string;
                    if (configName == "FP")
                        continue;

                    if (configName == staticPartNo)
                    {
                        Debug.WriteLine($"            [{configName}] found");
                        partNo = ExtractPartNoFromPath(unloadedComponentPath);
                        exitOuterLoop = true;
                        break;
                    }

                    Debug.WriteLine($"            ...loaded {configName} from directory");
                }
                Release(ref temp);
                if (exitOuterLoop)
                {
                    AssignedComponentPaths.Add(unloadedComponentPath);
                    break;
                }

            }


            EnablePartUI();

            if (partNo != null)
                Debug.WriteLine("");

            return partNo;
        }
        public static void LocateComponents(List<IComponentInfo2> components, SW_Assembly swAssembly)
        {
            Debug.WriteLine($"------------Locating components in ({swAssembly.Config})-------------" + "\n");

            if (ClassesToIsolate.Count == 0)
                RemoveUnneededComponents(swAssembly);

            Component2[] userLocatedComponents = UnfixedComponentsArray(swAssembly.ComponentArray);

            foreach (var component in components)
                PlaceComponent(component, swAssembly);

            swAssembly.ClearComponentArray();
            FixComponentLocations(userLocatedComponents, swAssembly.AssemblyDoc, swAssembly.ComponentArray);

            //Release(swAssembly.AssemblyDoc);
            Debug.WriteLine($"---------------------------------------------------" + "\n");
        }
        public static void PlaceComponent(IComponentInfo2 component, SW_Assembly sW_Assembly)
        {
            // Remove or replace template files
            var templateReplacements = new List<string>();
            ModelDoc2 modelDoc2 = sW_Assembly.AssemblyDoc as ModelDoc2;
            if (sW_Assembly.ComponentArray != null)
            {
                foreach (var assemblyComp in sW_Assembly.ComponentArray)
                {
                    if (assemblyComp != null)
                    {
                        string assemblyCompPath = assemblyComp.GetPathName().ToLower();
                        if (assemblyCompPath.Contains(TemplateFolderPath.ToLower()))
                        {
                            string configName = assemblyComp.ReferencedConfiguration;
                            if (configName == component.StaticPartNo)
                            {
                                SW.ActivateDoc3(modelDoc2.GetPathName(), false, (int)swRebuildOnActivation_e.swDontRebuildActiveDoc, 0);
                                bool check = modelDoc2.Extension.SelectByID2(assemblyComp.Name2 + $"@{Path.GetFileNameWithoutExtension(modelDoc2.GetPathName())}", "COMPONENT", 0, 0, 0, false, 0, null, 0);
                                bool check2 = sW_Assembly.AssemblyDoc.ReplaceComponents2(component.FilePath, component.StaticPartNo, true, 1, false);
                                Debug.WriteLine($"   Replaced template file <{Path.GetFileNameWithoutExtension(assemblyCompPath.ToUpper())}> with [{component.StaticPartNo}] in ({sW_Assembly.Config})");
                                templateReplacements.Add(component.FilePath);
                                modelDoc2.Visible = false;
                            }
                        }
                    }
                    //Release(assemblyComp); 
                }
            }

            // Search assembly for existing component instances
            List<Component2> componentList;
            if (component.PartNo != null)
            {
                componentList = FindMatchingComponents(component.FilePath, sW_Assembly.ComponentArray);
            }
            else
            {
                componentList = new List<Component2>();
            }

            // Determine if a new component should be inserted based on the list being empty and the component having a PartNo
            bool insertNew = componentList.Count == 0 && component.PartNo != null;

            // Sort the component list by the Name2 property
            componentList.Sort((c1, c2) => c1.Name2.CompareTo(c2.Name2));


            // Add to list or remove components from assembly
            while (componentList.Count < component.Position.Count)
            {
                componentList.Add(null);
            }
            while (componentList.Count > component.Position.Count)
            {
                DeleteComponentByName(componentList[componentList.Count - 1].Name2, sW_Assembly.AssemblyDoc);
                componentList.RemoveAt(componentList.Count - 1);
            }

            var insertedComponents = new List<Component2>();
            // Insert or get existing components
            for (int i = 0; i < component.Position.Count; i++)
            {
                string internalID = component.StaticPartNo;
                if (insertNew || componentList[i] == null)
                {
                    componentList[i] = InsertComponent2(component.FilePath, i, sW_Assembly);
                    insertedComponents.Add(componentList[i]);
                }
                else
                    componentList[i] = GetInstance(componentList, i, sW_Assembly);
            }

            var componentsToFix = new List<Component2>(componentList);

            // Delete all mates
            for (int i = 0; i < componentList.Count; i++)
            {
                var comp = componentList[i];
                if (templateReplacements.Contains(comp.GetPathName()))
                    DeleteAllMatesAndFix(comp, sW_Assembly.AssemblyDoc);

                if (!comp.IsFixed())
                    componentsToFix.Remove(comp);
            }

            // Locate components
            for (int i = 0; i < component.Position.Count; i++)
            {
                if (componentList[i] != null)
                {
                    X_Translation(component.Position[i].TranslationX);
                    Y_Translation(component.Position[i].TranslationY);
                    Z_Translation(component.Position[i].TranslationZ);
                    Rotate(
                        component.Position[i].RotationX,
                        component.Position[i].RotationY,
                        component.Position[i].RotationZ
                    );
                    SetPosition2(componentList[i], i, sW_Assembly);
                }
            }


            if (componentList.Count > 0)
            {
                ModelDoc2 modelDoc_ParentAssembly = sW_Assembly.AssemblyDoc as ModelDoc2;
                if (modelDoc_ParentAssembly != null)
                {
                    // Fix components
                    modelDoc_ParentAssembly.ClearSelection2(true);
                    componentsToFix.AddRange(insertedComponents);
                    if (componentsToFix.Count > 0)
                    {
                        if (modelDoc_ParentAssembly != null)
                        {
                            foreach (var comp in componentsToFix)
                            {
                                comp.Select4(true, null, false);
                            }
                            sW_Assembly.AssemblyDoc.FixComponent();
                            modelDoc_ParentAssembly.ClearSelection2(true);
                        }
                    }
                }
            }


            // Memory management
            foreach (var item in componentList)
            {
                //Release(item);
            }
            componentList.Clear();
            Close2(component);
        }
        public static ModelDoc2 OpenSilent(Part part)
        {
            ModelDoc2 modelDoc2 = OpenDocument(part.FilePath, part.StaticPartNo);

            SetProperty("PartNo", ExtractPartNumber(Path.GetFileName(part.FilePath)), modelDoc2);
            SetProperty("Title", GetConfigurationTitle(modelDoc2), modelDoc2);
            AddRawMaterialInfo(part.RawMaterialShape, part.MaterialSpec, part.SizeOrThickness, modelDoc2);

            return modelDoc2;
        }
        public static void PlaceSubComponents(List<IComponentInfo2> subComponents, SW_Assembly parentAssembly)
        {
            foreach (var instantiatedComponent in subComponents)
            {
                if (instantiatedComponent.Enabled)
                {
                    PlaceComponent(instantiatedComponent, parentAssembly);
                }
                else
                {
                    if (parentAssembly.ComponentArray != null)
                    {
                        var componentsToDelete = new List<Component2>();
                        foreach (var foundComponent in parentAssembly.ComponentArray)
                        {
                            if (foundComponent.ReferencedConfiguration == instantiatedComponent.StaticPartNo)
                            {
                                componentsToDelete.Add(foundComponent);
                            }
                        }
                        if (componentsToDelete.Count > 0)
                        {
                            foreach (var component in componentsToDelete)
                            {

                                RemoveComponent(component, parentAssembly.AssemblyDoc);
                            }
                        }
                    }
                }
            }
        }
        public static List<IComponentInfo2> InstantiateComponents(SW_Assembly swAssembly)
        {
            Debug.WriteLine($"\n" + $"----------Instantiating components in ({swAssembly.Config})----------" + "\n");
            string targetNamespace = swAssembly.GetType().Namespace;

            // Get the assembly containing the type of mainAssembly
            Assembly assembly = Assembly.GetAssembly(swAssembly.GetType());
            Type[] types = assembly.GetTypes();
            var typesList = types.ToList();

            Predicate<Type> hasStaticPartNo = type =>
            {
                // Get the property named 'StaticPartNo' that is public and an instance member
                PropertyInfo staticPartNoProperty = type.GetProperty("StaticPartNo", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);

                // Check if the property exists and is a string
                if (staticPartNoProperty != null && staticPartNoProperty.PropertyType == typeof(string))
                {
                    // Check if the property is an override
                    MethodInfo getMethod = staticPartNoProperty.GetGetMethod();
                    if (getMethod != null)
                    {
                        return getMethod.IsVirtual && getMethod.GetBaseDefinition().DeclaringType != type;
                    }
                }
                return false;
            };

            // Filter the list to remove any types that don't have the required property
            typesList.RemoveAll(type => !hasStaticPartNo(type));

            // Sort types by priority
            typesList = typesList.OrderByDescending(type =>
            {
                var priorityProperty = type.GetProperty("Priority", BindingFlags.Public | BindingFlags.Static);
                return priorityProperty != null ? (int)priorityProperty.GetValue(null) : 0;
            }).ToList();

            var componentsToBeAdded = new List<IComponentInfo2>();

            for (int i = 0; i < typesList.Count; i++)
            {
                Type type = typesList[i];
                LastType = type;

                if (!ClassesToIsolate.Any() || ClassesToIsolate.Contains(type))
                {
                    // Check if the type's namespace starts with the target namespace and is assignable to SubAssembly
                    if (type.Namespace != null && type.Namespace.StartsWith(targetNamespace) && typeof(IComponentInfo2).IsAssignableFrom(type))
                    {
                        // Ensure the type has a constructor that accepts a MainAssembly instance
                        ConstructorInfo constructor = type.GetConstructor(new Type[] { typeof(MainAssembly) });
                        if (constructor != null)
                        {
                            // Instantiate the type using the provided MainAssembly instance
                            IComponentInfo2 component = (IComponentInfo2)Activator.CreateInstance(type, swAssembly);
                            ComponentRegistry.RegisterComponent(component);

                            // Check if the component is enabled before adding it to the list
                            if (component != null && component.Enabled)
                            {
                                componentsToBeAdded.Add(component);
                            }
                        }
                    }
                }
            }

            AssignedComponentPaths.Clear();

            Debug.WriteLine($"---------------------------------------------------");
            return componentsToBeAdded;
        }
        public static void CreateDrawing(List<IComponentInfo2> components, SW_Assembly swAssembly)
        {
            // Create drawing file
            bool drawingExists = TryCreateDrawing(out string drawingPath);
            if (drawingExists)
                return;


            AddGrandChildren(ref components, swAssembly.GrandChildren);

            // Remove duplicate instances based on PartNo
            components = components.Where(comp => !string.IsNullOrEmpty(comp.PartNo))
                                   .GroupBy(comp => comp.PartNo)
                                   .Select(group => group.First())
                                   .ToList();


            // Replace references
            foreach (var component in components)
                ReplaceDrawingReference(component.PartNo, drawingPath, component.StaticPartNo);

            // Open drawing
            DrawingDoc drawingDoc = OpenDrawing(drawingPath);

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
                    RenameSheet(staticPartNo, compPartNo, drawingDoc);

                string fpVersion = staticPartNo + "_FP";
                if (templateSheetNames.Contains(fpVersion))
                    RenameSheet(fpVersion, compPartNo + "_FP", drawingDoc);
            }

            // Sort sheets
            SortSheetsInDrawing();

            // Save and close drawing
            (drawingDoc as ModelDoc2).Save3(1, 0, 0);
            Close(drawingPath);
            Release(ref drawingDoc);

        }
        public static void SetProperty(string property, string value, ModelDoc2 modelDoc2)
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
        public static List<IComponentInfo2> InstantiateSubComponents(Type baseType, SubAssembly parentSubAssembly, out List<IComponentInfo2> subComponentsToRemove)
        {
            subComponentsToRemove = new List<IComponentInfo2>();

            var instantiatedComponents = new List<IComponentInfo2>();
            var baseNamespace = baseType.Namespace;
            var assembly = Assembly.GetAssembly(baseType);

            // Find all types in the assembly that are in a sub-namespace of the provided base namespace
            var componentTypes = assembly.GetTypes()
                .Where(t => t.Namespace != null
                            && t.Namespace.StartsWith(baseNamespace)
                            && t.IsClass
                            && !t.IsAbstract
                            && t.IsSubclassOf(typeof(Part)));

            List<Type> componentTypesList = componentTypes.ToList();

            // Sort types by priority
            componentTypesList = componentTypesList.OrderByDescending(type =>
            {
                var priorityProperty = type.GetProperty("Priority", BindingFlags.Public | BindingFlags.Static);
                return priorityProperty != null ? (int)priorityProperty.GetValue(null) : 0;
            }).ToList();



            for (int i = 0; i < componentTypesList.Count; i++)
            {
                var type = componentTypesList[i];

                if (!ClassesToIsolate.Any() || ClassesToIsolate.Contains(type))
                {
                    // Attempt to find a constructor that takes a SubAssembly as a parameter
                    var constructor = type.GetConstructor(new Type[] { typeof(SubAssembly) });
                    if (constructor != null)
                    {
                        // Instantiate the component
                        var componentInstance = constructor.Invoke(new object[] { parentSubAssembly }) as IComponentInfo2;
                        if (componentInstance != null)
                        {
                            if (componentInstance.Enabled)
                                instantiatedComponents.Add(componentInstance);
                            else
                                subComponentsToRemove.Add(componentInstance);
                        }
                    }
                }
            }

            return instantiatedComponents;
        }
        public static void TurnOffBendLines()
        {
            SW.IActiveDoc2.SetUserPreferenceToggle((int)swUserPreferenceToggle_e.swDisplayBendLines, false);
        }
        public static void FoldModel()
        {
            SW.IActiveDoc2.SetBendState((int)swSMBendState_e.swSMBendStateFolded);
        }
        public static void UnFoldModel()
        {
            SW.IActiveDoc2.SetBendState((int)swSMBendState_e.swSMBendStateFlattened);
        }
        public static void ActivateModelDoc(ModelDoc2 modelDoc2)
        {
            SW.ActivateDoc3(modelDoc2.GetPathName(), false, 0, 0);
        }
        public static string GetConfigurationTitle(ModelDoc2 modelDoc2)
        {
            Configuration config = modelDoc2.ConfigurationManager.ActiveConfiguration;
            return config.GetDisplayStates()[0];
        }
        public static void SetPosition2(Component2 component, int instance, SW_Assembly sW_Assembly)
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

            Debug.WriteLine($"         [{component.ReferencedConfiguration}:{instance + 1}] position set in ({sW_Assembly.Config})");

            Release(ref mathTransform);
        }
        public static void Close2(IComponentInfo2 component)
        {
            SW.CloseDoc(component.FilePath);
            string fileName = Path.GetFileNameWithoutExtension(component.FilePath);
            Debug.WriteLine($"            [{component.StaticPartNo}] closed");
            Debug.WriteLine($"");
        }
        public static Component2 InsertComponent2(string filePath, int instance, SW_Assembly sWAssembly)
        {
            Component2 component2 = sWAssembly.AssemblyDoc.AddComponent5
                (filePath,
                (int)swAddComponentConfigOptions_e.swAddComponentConfigOptions_CurrentSelectedConfig, null,
                false, null,
                0, 0, 0);
            Debug.WriteLine($"   New [{component2.ReferencedConfiguration}:{instance + 1}] inserted into ({sWAssembly.Config})");
            return component2;
        }
        public static void ForceRebuild(AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            modelDoc2.ForceRebuild3(false);
            if (Developer)
            {
                Debug.WriteLine(@"
DDDDDDDDDDDDD              OOOOOOOOO      NNNNNNNN        NNNNNNNN EEEEEEEEEEEEEEEEEEEEEE
D::::::::::::DDD         OO:::::::::OO    N:::::::N       N::::::N E::::::::::::::::::::E
D:::::::::::::::DD     OO:::::::::::::OO  N::::::::N      N::::::N E::::::::::::::::::::E
DDD:::::DDDDD:::::D   O:::::::OOO:::::::O N:::::::::N     N::::::N EE::::::EEEEEEEEE::::E
  D:::::D    D:::::D  O::::::O   O::::::O N::::::::::N    N::::::N   E:::::E       EEEEEE
  D:::::D     D:::::D O:::::O     O:::::O N:::::::::::N   N::::::N   E:::::E             
  D:::::D     D:::::D O:::::O     O:::::O N:::::::N::::N  N::::::N   E::::::EEEEEEEEEE   
  D:::::D     D:::::D O:::::O     O:::::O N::::::N N::::N N::::::N   E:::::::::::::::E   
  D:::::D     D:::::D O:::::O     O:::::O N::::::N  N::::N:::::::N   E:::::::::::::::E   
  D:::::D     D:::::D O:::::O     O:::::O N::::::N   N:::::::::::N   E::::::EEEEEEEEEE   
  D:::::D     D:::::D O:::::O     O:::::O N::::::N    N::::::::::N   E:::::E             
  D:::::D    D:::::D  O::::::O   O::::::O N::::::N     N:::::::::N   E:::::E       EEEEEE
DDD:::::DDDDD:::::D   O:::::::OOO:::::::O N::::::N      N::::::::N EE::::::EEEEEEEE:::::E
D:::::::::::::::DD     OO:::::::::::::OO  N::::::N       N:::::::N E::::::::::::::::::::E
D::::::::::::DDD         OO:::::::::OO    N::::::N        N::::::N E::::::::::::::::::::E
DDDDDDDDDDDDD              OOOOOOOOO      NNNNNNNN         NNNNNNN EEEEEEEEEEEEEEEEEEEEEE
                ");
            }
            else
            {
                modelDoc2.ShowNamedView2("*Isometric", -1);
                modelDoc2.ViewZoomtofit2();
                System.Windows.Forms.MessageBox.Show("Automation tool has finished executing", "Automation Guy", System.Windows.Forms.MessageBoxButtons.OK, System.Windows.Forms.MessageBoxIcon.Information);
            }
        }
        public static ModelDoc2 OpenDocument(string filePath, string configurationName)
        {
            DisablePartUI();
            ModelDoc2 modelDoc2 = Open(filePath, configurationName);
            EnablePartUI();

            if (modelDoc2 != null)
            {
                Configuration config = modelDoc2.ConfigurationManager.ActiveConfiguration;
                Debug.WriteLine($"({config.Name}) {GetConfigurationTitle(modelDoc2)} opened");

                // Job info
                SetProperty("Project", Default.Project, modelDoc2);
                SetProperty("Bank", Default.Bank.ToString(), modelDoc2);
                SetProperty("Customer", Default.Customer, modelDoc2);
                SetProperty("Client", Default.Client, modelDoc2);
                SetProperty("Location", Default.PlantLocation, modelDoc2);
                SetProperty("PO", Default.PurchaseOrder, modelDoc2);
                SetProperty("ItemNo", Default.ItemNumber, modelDoc2);

                return modelDoc2;
            }
            else
            {
                Debug.WriteLine($"Attempting to open {Path.GetFileNameWithoutExtension(filePath)} returns null ModelDoc2");
                return null;
            }

        }
        public static void RemoveUnneededComponents(SW_Assembly swAssembly)
        {
            if (swAssembly.ComponentArray != null)
            {
                var subComponentStaticNumbers = AssignConfigToComponent(swAssembly.ComponentArray);
                var subComponentTypes = DetermineComponentTypes(subComponentStaticNumbers, swAssembly);
                var componentsToDelete = IdentifyComponentsToDelete(subComponentTypes, swAssembly.ComponentArray);

                RemoveComponents(componentsToDelete, swAssembly.AssemblyDoc);
            }
        }
        public static void RemoveDisabledSubComponents(List<IComponentInfo2> subComponentsToRemove, SW_Assembly parentAssembly)
        {
            {
                foreach (var comp in subComponentsToRemove)
                {
                    if (!comp.Enabled)
                    {
                        if (parentAssembly.ComponentArray != null)
                        {
                            var componentsToDelete = new List<Component2>();
                            foreach (var foundComponent in parentAssembly.ComponentArray)
                            {
                                if (foundComponent.ReferencedConfiguration == comp.StaticPartNo)
                                {
                                    componentsToDelete.Add(foundComponent);
                                }
                            }
                            if (componentsToDelete.Count > 0)
                            {
                                foreach (var component in componentsToDelete)
                                {

                                    RemoveComponent(component, parentAssembly.AssemblyDoc);
                                }
                            }
                        }
                    }
                }
            }

        }






        // Private methods
        private static void CopyAsReadWrite(string sourceFile, string destinationFile)
        {
            System.IO.File.Copy(sourceFile, destinationFile);
            FileAttributes attributes = System.IO.File.GetAttributes(destinationFile);
            attributes &= ~FileAttributes.ReadOnly;
            System.IO.File.SetAttributes(destinationFile, attributes);
        }
        private static void SetProperty_ConfigSpecific(string property, string value, string configuration, ModelDoc2 modelDoc2)
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
        private static string ExtractPartNumber(string input)
        {
            int start = input.IndexOf('-') + 1;
            int end = input.LastIndexOf('.');
            if (start > 0 && end > start)
            {
                return input.Substring(start, end - start);
            }
            return input;
        }
        private static string ExtractPartNoFromPath(string path)
        {
            string fileName = Path.GetFileNameWithoutExtension(path);

            // Find the last occurrence of '-' or '_'
            int lastDash = fileName.LastIndexOf('-');
            int lastUnderscore = fileName.LastIndexOf('_');

            // Determine which of the two delimiters comes last
            int lastDelimiter = Math.Max(lastDash, lastUnderscore);

            // If a delimiter was found, extract the substring after it; otherwise, use the entire input
            string result = lastDelimiter >= 0 ? fileName.Substring(lastDelimiter + 1) : fileName;

            return result;
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
        private static int HandlePartFile(string templateFile, string staticPartNo)
        {
            string DesktopFolderPath =
                System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop) +
                $"{Default.Project}-{AssemblyNumber}{Default.Bank}";

            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{partNo}.SLDPRT";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"            Created new [{staticPartNo}] as <{Path.GetFileNameWithoutExtension(destinationFile)}>");
                Debug.WriteLine("");
                AssignedComponentPaths.Add(destinationFile);
            }
            return partNo;
        }
        private static int HandleAssemblyFile(string templateFile, string staticPartNo)
        {
            int partNo = GetUniquePartNo();
            if (partNo != -1)
            {
                string destinationFile = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{partNo}.SLDASM";
                CopyAsReadWrite(templateFile, destinationFile);
                Debug.WriteLine($"            Created new [{staticPartNo}] as <{Path.GetFileNameWithoutExtension(destinationFile)}>");
                Debug.WriteLine("");
                AssignedComponentPaths.Add(destinationFile);
            }
            return partNo;
        }
        private static int GetUniquePartNo()
        {
            int partNo = 1;

            while (true)
            {
                string desktopFile_Part = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{partNo}.SLDPRT";
                string desktopFile_Assembly = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{partNo}.SLDASM";

                if (!System.IO.File.Exists(desktopFile_Part) && !System.IO.File.Exists(desktopFile_Assembly))
                {
                    return partNo;
                }
                partNo++;
            }
        }
        private static List<Component2> FindMatchingComponents(string filePath_OfTarget, Component2[] componentsArray)
        {
            // List to store matching components
            List<Component2> matchingComponents = new List<Component2>();

            // Extract the file name from the file path
            string[] fileNameParts = Path.GetFileNameWithoutExtension(filePath_OfTarget).Split('.');

            string fileName = fileNameParts[0];

            // Iterate through all components
            if (componentsArray != null)
            {
                foreach (var component in componentsArray)
                {
                    if (component != null)
                    {
                        string componentName = component.Name2;

                        // Check if the component name contains the file name and does not contain "/"
                        if (componentName.Contains(fileName) && !componentName.Contains("/"))
                        {
                            matchingComponents.Add(component);
                        }
                    }
                }
            }

            return matchingComponents;
        }
        private static Component2 GetInstance(List<Component2> componentList, int instance, SW_Assembly sW_Assembly)
        {
            Component2 swComponent = componentList[instance];
            Debug.WriteLine($"      [{swComponent.ReferencedConfiguration}:{instance + 1}] found in ({sW_Assembly.Config})");
            return swComponent;
        }
        private static string ExtractPartNo(Component2 component2)
        {
            string pathName = component2.GetPathName();
            return ExtractPartNoFromPath(pathName);
        }
        private static Dictionary<Component2, string> AssignConfigToComponent(Component2[] componentArray)
        {
            var dictionary = new Dictionary<Component2, string>();

            foreach (var component in componentArray)
            {
                if (component != null)
                {
                    string staticPartNo = component.ReferencedConfiguration;
                    dictionary[component] = staticPartNo;
                }
            }

            return dictionary;
        }
        private static Dictionary<Component2, Type> DetermineComponentTypes(Dictionary<Component2, string> staticNumbers, SW_Assembly swAssembly)
        {
            var dictionary = new Dictionary<Component2, Type>();
            Assembly assembly = Assembly.GetAssembly(swAssembly.GetType());

            foreach (var kvp in staticNumbers)
            {
                var componentKey = kvp.Key;
                var staticNumber = kvp.Value;

                // First, try to find an existing component by its StaticPartNo
                var existingComponent = ComponentRegistry.GetComponentByPartNo(staticNumber);

                if (existingComponent != null)
                {
                    // If found, use the existing component's type
                    dictionary.Add(componentKey, existingComponent.GetType());
                    continue; // Skip to the next kvp in staticNumbers
                }

                if (componentKey.GetPathName().Contains("Hudson Library"))
                {
                    dictionary.Add(componentKey, null);
                    continue;
                }







                //// If not found, proceed to potentially instantiate a new component
                //var matchingType = assembly.GetTypes()
                //    .Where(type => type.IsClass && !type.IsAbstract)
                //    .FirstOrDefault(type =>
                //    {
                //        var propInfo = type.GetProperty("StaticPartNo", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
                //        if (propInfo != null && propInfo.GetMethod != null && propInfo.GetMethod.IsVirtual)
                //        {
                //            // Since no existing instance was found, instantiate a new one for comparison
                //            object instance = null;
                //            var constructorInfo = type.GetConstructor(new Type[] { typeof(MainAssembly) });
                //            if (constructorInfo != null)
                //            {
                //                instance = constructorInfo.Invoke(new object[] { swAssembly });
                //            }

                //            if (instance != null)
                //            {
                //                var value = propInfo.GetValue(instance)?.ToString();
                //                return value == staticNumber;
                //            }
                //        }
                //        return false;
                //    });

                //if (matchingType != null)
                //{
                //    dictionary.Add(componentKey, matchingType);
                //}
            }

            return dictionary;
        }
        private static List<Component2> IdentifyComponentsToDelete(Dictionary<Component2, Type> componentTypes, Component2[] componentArray)
        {
            var componentsToDelete = new List<Component2>();

            foreach (var kvp in componentTypes)
            {
                Component2 component = kvp.Key;
                Type componentType = kvp.Value;

                // Attempt to get the type directly from the registry using known StaticPartNo
                Type registeredType = ComponentRegistry.GetComponentTypeByPartNo(component.ReferencedConfiguration);

                if (registeredType != null && registeredType == componentType)
                {
                    // If the type matches and you need to check if it's enabled, retrieve the instance
                    var instance = ComponentRegistry.GetComponentByPartNo(component.ReferencedConfiguration);

                    // Handles local part instances
                    if (instance != null && !instance.Enabled)
                    {
                        componentsToDelete.Add(component);
                    }

                    // Handles standard parts in the vault
                    if (instance != null && component.ReferencedConfiguration != instance.StaticPartNo)
                    {
                        componentsToDelete.Add(component);
                    }
                }
                else
                {
                    componentsToDelete.Add(component);
                }
            }

            // Mark components for deletion in componentArray by setting them to null
            for (int i = 0; i < componentArray.Length; i++)
            {
                if (componentsToDelete.Contains(componentArray[i]))
                {
                    // Mark the component as "deleted" by setting it to null
                    componentArray[i] = null;
                }
            }

            return componentsToDelete;
        }
        private static void RemoveComponents(List<Component2> componentsToDelete, AssemblyDoc assemblyDoc)
        {
            foreach (var component in componentsToDelete)
            {
                string filePath = component.GetPathName();
                RemoveComponent(component, assemblyDoc);
                //Release(component);

                if (!FilesToBeDeleted.Contains(filePath))
                    FilesToBeDeleted.Add(filePath);
            }
        }
        private static Component2[] UnfixedComponentsArray(Component2[] componentArray)
        {
            List<Component2> unfixedComponents = new List<Component2>();

            for (int i = 0; i < componentArray.Length; i++)
            {
                var component = componentArray[i];

                if (component != null)
                {
                    object[] mates = component.GetMates();

                    if (!component.IsFixed() && mates != null)
                    {
                        unfixedComponents.Add(component);
                    }
                }
            }
            return unfixedComponents.ToArray();
        }
        private static void FixComponentLocations(Component2[] userLocatedComponents, AssemblyDoc assemblyDoc, Component2[] componentArray)
        {
            HashSet<Component2> locatedComponentsSet = new HashSet<Component2>(userLocatedComponents);
            List<Component2> componentsToFix = new List<Component2>();

            // Collect components that need fixing
            for (int i = 0; i < componentArray.Length; i++)
            {
                var component = componentArray[i];
                if (component != null && !component.IsFixed() && !locatedComponentsSet.Contains(component))
                {
                    componentsToFix.Add(component);
                }
            }

            // Select and fix all at once
            if (componentsToFix.Count > 0)
            {
                ModelDoc2 modelDoc = assemblyDoc as ModelDoc2;
                if (modelDoc != null)
                {
                    foreach (var comp in componentsToFix)
                    {
                        comp.Select4(true, null, false);
                    }
                    assemblyDoc.FixComponent();
                    modelDoc.ClearSelection2(true);
                }
            }
        }
        private static bool TryCreateDrawing(out string desktopDrawing)
        {
            string fileName = $"{Default.Project}-{AssemblyNumber}{Default.Bank}.SLDDRW";

            string templateDrawing = $@"{TemplateFolderPath}\JOBNO-{AssemblyNumber}.SLDDRW";
            desktopDrawing = $@"{DesktopFolderPath}\{fileName}";

            try
            {
                System.IO.File.Copy(templateDrawing, desktopDrawing);
                Debug.WriteLine($"Created {fileName}");
                FileAttributes attributes = System.IO.File.GetAttributes(desktopDrawing);
                attributes &= ~FileAttributes.ReadOnly;
                System.IO.File.SetAttributes(desktopDrawing, attributes);
            }
            catch (Exception)
            {
                Debug.WriteLine($"Failed to create {fileName}");
                return true;
            }

            return false;
        }
        private static bool ReplaceDrawingReference(string dynamicPartNo, string drawingToBeModified, string staticPartNo)
        {
            string oldReferenceASM = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDASM";
            string oldReferencePRT = $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
            string oldReference = System.IO.File.Exists(oldReferenceASM) ? oldReferenceASM : oldReferencePRT;

            string newReferenceASM;
            if (dynamicPartNo == null)
            {
                newReferenceASM = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}.SLDASM";
            }
            else
            {
                newReferenceASM = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{dynamicPartNo}.SLDASM";
            }
            string newReferencePRT = $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}-{dynamicPartNo}.SLDPRT";
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

            Debug.WriteLine($"   Replaced sheet reference to {Default.Project}-{AssemblyNumber}{Default.Bank}-{dynamicPartNo}: {check}");
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
        private static DrawingDoc OpenDrawing(string filePath)
        {
            ModelDoc2 modelDoc2 = Open(filePath);

            ModelView modelView = modelDoc2.ActiveView as ModelView;
            modelView.EnableGraphicsUpdate = false;

            IFeatureManager featureManager = modelDoc2.FeatureManager;
            featureManager.EnableFeatureTree = false;

            SetProperty("DrawnBy", Default.Initials, modelDoc2);
            SetProperty("DrawnDate", DateTime.Now.ToString("M/d/yyyy"), modelDoc2);

            return modelDoc2 as DrawingDoc;
        }
        private static void SetProperty_ConfigSpecific_Static(string property, string value, string configuration, ModelDoc2 modelDoc2)
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
        private static void DeleteSheet(string sheetName, DrawingDoc drawingDoc)
        {
            ModelDoc2 modelDoc2 = drawingDoc as ModelDoc2;
            //drawingDoc.ActivateSheet(sheetName);
            bool checkSelection = modelDoc2.Extension.SelectByID2(sheetName, "SHEET", 0, 0, 0, false, 0, null, 0);
            bool checkDeletion = modelDoc2.Extension.DeleteSelection2(0);
            Debug.WriteLine($"   Sheet {sheetName} deleted: {checkDeletion}");
        }
        private static void RenameSheet(string sheetName, string dynamicPartNo, DrawingDoc drawingDoc)
        {
            string newName = $"{Default.Project}-{AssemblyNumber}{Default.Bank}-{dynamicPartNo}";
            Sheet sheet = drawingDoc.Sheet[sheetName];
            sheet.SetName(newName);

            Debug.WriteLine($"   Sheet {sheetName} has been renamed to {Default.Project}-{AssemblyNumber}{Default.Bank}-{dynamicPartNo}");
        }
        private static void AddGrandChildren(ref List<IComponentInfo2> components, List<IComponentInfo2> grandChildren)
        {
            foreach (var child in grandChildren)
                components.Add(child);

            grandChildren.Clear();
        }
        private static void DeleteAllMatesAndFix(Component2 component, AssemblyDoc assemblyDoc)
        {
            ModelDoc2 modelDoc2 = assemblyDoc as ModelDoc2;
            SW.ActivateDoc3(modelDoc2.GetPathName(), false, 0, 0);
            string assemblyName = Path.GetFileNameWithoutExtension(modelDoc2.GetPathName());
            string componentName = Path.GetFileNameWithoutExtension(component.GetPathName());

            object[] matesObj = component.GetMates();
            if (matesObj == null)
                return;

            Mate2[] mates = new Mate2[matesObj.Length];

            for (int i = 0; i < mates.Length; i++)
            {
                mates[i] = matesObj[i] as Mate2;
            }

            modelDoc2.Extension.MultiSelect2(mates, false, null);
            bool matesDeleted = modelDoc2.Extension.DeleteSelection2((int)swDeleteSelectionOptions_e.swDelete_Absorbed);

            bool isSelected = modelDoc2.Extension.SelectByID2(component.Name2 + "@" + assemblyName, "COMPONENT", 0, 0, 0, false, 0, null, 0);
            assemblyDoc.FixComponent();
        }


        // Public properties
        public static string DesktopFolderPath => $"{System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop)}\\{Default.Project}-{AssemblyNumber}{Default.Bank}";
        public static int AssemblyNumber { get; set; }
        public static string AssemblyDesc { get; set; }
        public static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
        public static Spec StaticMaterialSpec { get; set; } = Spec.A36;
        public static string TemplateFolderPath =>
                $@"C:\AXC_VAULT\Active\_Automation Tools\Hudson_\Drafting\Certified\{AssemblyDesc}";
        public static AssemblyDoc MainAssemblyDoc { get; set; }
        public static string AssemblyPath => $@"{DesktopFolderPath}\{Default.Project}-{AssemblyNumber}{Default.Bank}.SLDASM";
        public static List<string> AssignedComponentPaths = new List<string>();
        public static Type LastType { get; set; }
        public static bool Developer => System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop).ToLower().Contains("acmurr") ? true : false;

    }
}
