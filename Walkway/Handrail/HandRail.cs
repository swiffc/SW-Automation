using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swcommands;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar;

namespace Walkway
{
    internal class HandRail : Walkway
    {
        // Create a new handrail
        public HandRail(char bank, double length, double height, double floorHeight, int minimumStringerSize, bool handrailOnly)
        {
            // Set global property
            Bank = bank; Debug.WriteLine("\n" + $"Bank set to: {bank}" + "\n");

            bool bankExists;
            do
            {
                // Check if bank exists
                string desktopBankFile = $@"{DesktopFolderPath}\{Project}-28{Bank}.SLDASM";
                bankExists = File.Exists(desktopBankFile);

                // Set class properties
                if (bankExists)
                {
                    // Set properties
                    PassUserInputs(length, height, floorHeight, minimumStringerSize);
                    SetStaticPartNumbers();
                    SetPostProperties();
                    SetRailProperties();

                    // Create desktop instances of platform files with unique part numbers
                    Rail.PartNo.Dynamic = CreateNew_ComponentFile(Rail.PartNo.Static);
                    Rail.Top.PartNo.Dynamic = CreateNew_SubComponentFile(Rail.Top.PartNo.Static);
                    Rail.Mid.PartNo.Dynamic = CreateNew_SubComponentFile(Rail.Mid.PartNo.Static);
                    Post.Left.PartNo.Dynamic = CreateNew_SubComponentFile(Post.Left.PartNo.Static);
                    Post.Right.PartNo.Dynamic = CreateNew_SubComponentFile(Post.Right.PartNo.Static);
                    Post.Mid.PartNo.Dynamic = CreateNew_SubComponentFile(Post.Mid.PartNo.Static);
                    if (Reference.StringerShape != 'L')
                    {
                        ToePlate.PartNo.Dynamic = CreateNew_SubComponentFile(ToePlate.PartNo.Static);
                    }
                    

                    // Load part files into memory and pass properties
                    ModifyParts();

                    // Place instances of loaded part files into platform assembly file
                    AssembleHandrail();

                    // Create temporary platform drawing
                    string tempDrawingPath = CreateTemporary_PlatformDesktopDrawing(Rail.PartNo.Static, Rail.PartNo.Dynamic);

                    // Replace template references with desktop references
                    ReplaceDrawingReference(Rail.PartNo.Dynamic, tempDrawingPath, Rail.PartNo.Static);
                    ReplaceDrawingReference(Rail.Top.PartNo.Dynamic, tempDrawingPath, Rail.Top.PartNo.Static);
                    ReplaceDrawingReference(Rail.Mid.PartNo.Dynamic, tempDrawingPath, Rail.Mid.PartNo.Static);
                    ReplaceDrawingReference(Post.Left.PartNo.Dynamic, tempDrawingPath, Post.Left.PartNo.Static);
                    ReplaceDrawingReference(Post.Right.PartNo.Dynamic, tempDrawingPath, Post.Right.PartNo.Static);
                    ReplaceDrawingReference(Post.Mid.PartNo.Dynamic, tempDrawingPath, Post.Mid.PartNo.Static);
                    if (ToePlate.PartNo.Dynamic != null)
                    {
                        ReplaceDrawingReference(ToePlate.PartNo.Dynamic, tempDrawingPath, ToePlate.PartNo.Static);
                    }
                    

                    // Open drawing
                    DrawingDoc handrail_DesktopDrawingDoc = OpenDrawing(tempDrawingPath);

                    // Rename sheets
                    RenameSheet("1341_Handrail", Rail.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    RenameSheet("1341P_TopRail", Rail.Top.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    RenameSheet("1342_MidRail", Rail.Mid.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    RenameSheet("1343R_EndPost(R)", Post.Right.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    RenameSheet("1343L_EndPost(L)", Post.Left.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    RenameSheet("1344_MidPost", Post.Mid.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    if (ToePlate.PartNo.Dynamic != null)
                    {
                        RenameSheet("1371_ToePlate", ToePlate.PartNo.Dynamic, handrail_DesktopDrawingDoc);
                    }
                    

                    // Migrate sheets
                    MigrateSheetsToBankDrawing(handrail_DesktopDrawingDoc);

                    // Open bank and open Insert Component property manager page
                    AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);


                    //-------------------------------------------------------


                    if (handrailOnly)
                    {
                        // Initiate InsertComponents command
                        SW.RunCommand((int)swCommands_e.swCommands_InsertComponents, null);

                        // Release COM reference to bank and close platform weldment
                        Optimize.Release(ref bankAssembly);
                        Close(Rail.FilePath);
                    }
                    
                }
                else
                {
                    AddNew_Bank();
                }
            } while (!bankExists);

            // Import static part numbers
            







        }

        // Modify an existing handrail
        public HandRail(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, double length, double height, double floorHeight, int minimumStringerSize, bool updateLocations)
        {
            // Get active doc where program is initiated
            string activeDocPath = SW.IActiveDoc2.GetPathName();

            // Dictionary mapping the static part numbers to their descriptions for debugging
            Dictionary<string, string> staticPartDescriptions = new Dictionary<string, string>
            {
                { Handrail_PartNo_Static, "HANDRAIL_WELDMENT" },
                { Rail_Top_PartNo_Static, "TOP-RAIL"},
                { Rail_Mid_PartNo_Static, "MID-RAIL" },
                { Post_Left_PartNo_Static, "END-POST" },
                { Post_Right_PartNo_Static, "END-POST_MIR" },
                { Post_Mid_PartNo_Static, "MID-POST" },
                { TopPlate_PartNo_Static, "TOE-PLATE" }
            };

            // Itemized dictionary of components
            var handrailComponents = FilterComponents(selectedComponents, staticPartDescriptions); Debug.WriteLine("Handrail");
            if (handrailComponents.Count == 0) { DevTools.EnablePartUI(); return; }

            // Update locations option
            if (updateLocations) { AddParentToSelection(handrailComponents); }

            // Assign exisitng dynamic part numbers to class
            Rail.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Handrail_PartNo_Static);
            Rail.Top.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Rail_Top_PartNo_Static);
            Rail.Mid.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Rail_Mid_PartNo_Static);
            Post.Left.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Post_Left_PartNo_Static);
            Post.Right.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Post_Right_PartNo_Static);
            Post.Mid.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, Post_Mid_PartNo_Static);
            ToePlate.PartNo.Dynamic = GetDynamicPartNo(handrailComponents, TopPlate_PartNo_Static);

            // Assign properties
            char bank = GetBankFrom(handrailComponents); // Assign bank if ONLY one bank is found. Otherwise, exit the program.
            PassUserInputs(length, height, floorHeight, minimumStringerSize);
            SetStaticPartNumbers();

            GetParameters_SelectedComponents(handrailComponents);

            bool toePlate = false;
            // Swap structural shapes
            if (Post.HoleToEndDistance != 1.25 && Reference.StringerSize < 6 && ToePlate.PartNo.Dynamic != null) // delete toe plate
            {
                RemoveExistingToePlate();
                toePlate = false;
                ToePlate.PartNo.Dynamic = null;
            }
            else if (Post.HoleToEndDistance == 1.25 && Reference.StringerSize >= 6) // add toe plate
            {
                ToePlate.PartNo.Dynamic = CreateNew_SubComponentFile(ToePlate.PartNo.Static);
                toePlate = true;
            }
            else if (Reference.StringerSize < 6)
            {
                toePlate = false;
            }
            else
            {
                toePlate = true;
            }

            SetPostProperties();
            SetRailProperties();

            // Only modify parts in the found in the dictionary
            var conditionsAndActions = new
                List<(Func<KeyValuePair<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>, bool> condition,
                      Action<Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>> action)>
            {
                (entry => entry.Value.StaticPartNo == Post_Right_PartNo_Static, ModifyRightPost),
                (entry => entry.Value.StaticPartNo == Post_Left_PartNo_Static, ModifyLeftPost),
                (entry => entry.Value.StaticPartNo == Post_Mid_PartNo_Static, ModifyMidPost),
                (entry => entry.Value.StaticPartNo == Rail_Top_PartNo_Static, ModifyTopRail),
                (entry => entry.Value.StaticPartNo == Rail_Mid_PartNo_Static, ModifyMidRail),
                (entry => toePlate == true, ModifyToePlate)
            };
            foreach (var (condition, action) in conditionsAndActions)
            {
                if (handrailComponents.Any(condition))
                {
                    action(handrailComponents);
                }
            }

            // Only move selected parts in the assembly
            if (handrailComponents.Any(entry => entry.Value.StaticPartNo == Handrail_PartNo_Static))
            {
                AssembleHandrail();

                if (activeDocPath != Rail.FilePath)
                {
                    Close(Rail.FilePath);
                }
            }
            Debug.WriteLine("");
        }


        public ReferencePlatform Reference { get; set; } = new ReferencePlatform();
        public class ReferencePlatform
        {
            public double FloorHeight { get; set; }
            public int StringerSize { get; set; }
            public char StringerShape { get; set; }
        }
         
        // extensions
        public HandRailExtensions Rail { get; set; } = new HandRailExtensions();
        public class HandRailExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDASM";
            public string Title { get; set; } = "HANDRAIL_WELDMENT_WW";
            public string Description { get; set; } = "HANDRAIL_WELDMENT_WW";
            public double Length { get; set; }
            public double Height { get; set; }
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public RailSubExtensions Top { get; set; } = new RailSubExtensions();
            public RailSubExtensions Mid { get; set; } = new RailSubExtensions();
        }
        public class RailSubExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public string Title { get; set; }
            public string Description { get; set; } = "ANGLE_2-1/2\"x2-1/2\"x1/4\"_A572-50";
            public string JDEnumber { get; set; } = "53499";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public PostExtensions Post { get; set; } = new PostExtensions();
        public class PostExtensions
        {
            public double Length { get; set; }
            public double MaxSpacing { get; set; } = 80;
            public double HoleToEndDistance { get; set; }
            public double HoleToHoleSpacing { get; set; }
            public double RailingToPostDistance { get; set; } = 0.5;
            public double Gage { get; set; } = 1.375;
            public double ToePlateHole1 { get; set; }
            public double ToePlateHole2 { get; set; }
            public PostSubExtensions Right { get; set; } = new PostSubExtensions();
            public PostSubExtensions Left { get; set; } = new PostSubExtensions();
            public PostSubExtensions Mid { get; set; } = new PostSubExtensions();
        }
        public class PostSubExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public int Count { get; set; }
            public double Spacing { get; set; }
            public string Title { get; set; }
            public string Description { get; set; } = "ANGLE_2-1/2\"x2-1/2\"x1/4\"_A572-50";
            public string JDEnumber { get; set; } = "53499";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public ToePlateExtensions ToePlate { get; set; } = new ToePlateExtensions();
        public class ToePlateExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double Height { get; set; } = 5;
            public string Title { get; set; } = "TOE-PLATE_WW";
            public string Description { get; set; } = "PLATE_1/4\"_A572_50";
            public string JDEnumber { get; set; } = "60038";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public class PartNoSubExtensions
        {
            public string Static { get; set; }
            public string Dynamic { get; set; }
        }


        // Properties
        private void RemoveExistingToePlate()
        {
            string activeDocPath = SW.IActiveDoc2.GetPathName();

            ModelDoc2 modelDoc2 = Open(Rail.FilePath);

            object[] componentObjects = (modelDoc2 as AssemblyDoc).GetComponents(false);

            if (componentObjects != null)
            {
                foreach (var componentObject in componentObjects)
                {
                    Component2 component = componentObject as Component2;
                    if (component != null && component.Name2.Contains(Path.GetFileNameWithoutExtension(ToePlate.FilePath)))
                    {
                        modelDoc2.ClearSelection2(true);
                        modelDoc2.Extension.SelectByID2(component.Name2, "COMPONENT", 0, 0, 0, true, 0, null, 0);
                        modelDoc2.Extension.DeleteSelection2(0);
                    }
                }
            }

            List<string> openDocPaths = SaveAndCloseAllDocuments(out string _);

            File.Delete(ToePlate.FilePath);

            foreach (var path in openDocPaths)
            {
                ModelDoc2 openedDoc = Open(path, true);

                if (openedDoc != null)
                {
                    Debug.WriteLine($"Opened: {System.IO.Path.GetFileNameWithoutExtension(path)}");
                }
                else
                {
                    Debug.WriteLine($"Failed to Open: {path}");
                }
            }

            Open(activeDocPath);
        }
        internal void GetParameters_SelectedComponents(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> components)
        {
            var post = components.FirstOrDefault(entry => entry.Value.StaticPartNo == Post_Left_PartNo_Static ||
                                                          entry.Value.StaticPartNo == Post_Right_PartNo_Static ||
                                                          entry.Value.StaticPartNo == Post_Mid_PartNo_Static);

            if (post.Key != null) // Check if a matching component was found
            {
                Debug.WriteLine($"Component Name: {post.Key.Name2 ?? "Unknown"}"); // Safely print the name
                ModelDoc2 modelDoc2 = post.Key.GetModelDoc2();

                if (modelDoc2 != null) // Check if a ModelDoc2 was returned
                {
                    Dimension dimension = modelDoc2.Parameter("HoleToEndDistance@holes");

                    if (dimension != null) // Check if a valid Dimension was returned
                    {
                        Post.HoleToEndDistance = dimension.GetValue3(1, null)[0];
                        Debug.WriteLine($"Post.HoleToEndDistance = {Post.HoleToEndDistance}");
                    }
                    else
                    {
                        Debug.WriteLine("Dimension 'HoleToEndDistance@holes' not found.");
                    }
                }
                else
                {
                    Debug.WriteLine("No associated ModelDoc2 found for the component.");
                }
            }
            else
            {
                Debug.WriteLine("No matching component found.");
            }
        }
        private void PassUserInputs(double length, double height, double floorHeight, int minimumStringerSize)
        {
            // Pass user inputs
            Rail.Length = length;
            Debug.WriteLine($"WRITE length: {length}");

            Rail.Height = height;
            Debug.WriteLine($"WRITE height: {height}");

            Reference.FloorHeight = floorHeight;
            Debug.WriteLine($"REF floor height: {floorHeight}");


            if (Rail.Length < 132 && minimumStringerSize < 6)
            {
                Reference.StringerSize = 0;
                Reference.StringerShape = 'L';
            }
            else if (Rail.Length < 168 && minimumStringerSize == 6)
            {
                Reference.StringerSize = 6;
                Reference.StringerShape = 'C';
            }
            else if (Rail.Length < 198 && minimumStringerSize <= 8)
            {
                Reference.StringerSize = 8;
                Reference.StringerShape = 'C';
            }
            else
            {
                Reference.StringerSize = 10;
                Reference.StringerShape = 'C';
            }
            Debug.WriteLine($"REF stringer size: {Reference.StringerSize}");
        }
        private void SetStaticPartNumbers()
        {
            Rail.PartNo.Static = "1341";
            Rail.Top.PartNo.Static = "1341P";
            Rail.Mid.PartNo.Static = "1342";
            Post.Left.PartNo.Static = "1343L";
            Post.Right.PartNo.Static = "1343R";
            Post.Mid.PartNo.Static = "1344";
            ToePlate.PartNo.Static = "1371";
        }
        private void SetPostProperties()
        {
            // Right post
            Post.Right.Title = "END-POST_WW";

            // Left post
            Post.Left.Title = "END-POST_MIR_WW";

            // Mid post
            Post.Mid.Title = "MID-POST_WW";
            Post.Mid.Count = (int)(Rail.Length / Post.MaxSpacing);
            Post.Mid.Spacing = (Rail.Length - Post.Gage * 2) / (Post.Mid.Count + 1);

            double value = 3;
            string C = null;

            if (Reference.StringerSize == 6)
            {
                C = "C6x8.2";
            }
            else if (Reference.StringerSize > 6 && Reference.StringerSize <= 8)
            {
                C = "C8x11.5";
            }
            else if (Reference.StringerSize >= 8)
            {
                C = "C10x15.3";
            }
            else // L-stringer
            {
                value = value + Post.Length + 3;
                Post.ToePlateHole1 = Rail.Height + Reference.FloorHeight
                    + 0.375     // Stringer.LegTHK
                    - 2.25      // Stringer.TopLegGage1
                    - 1.25;     // Stringer.TopLegGage1 / 2
                Post.ToePlateHole2 = value;

                Post.HoleToEndDistance = 1.25;
                Post.HoleToHoleSpacing = 2.5;

                Post.Length =
                    +Rail.Height
                    - Post.RailingToPostDistance
                    + Reference.FloorHeight
                    + 0.375     // Stringer.LegTHK
                    - 2.25      // Stringer.TopLegGage1
                    + Post.HoleToEndDistance;
            }

            switch (C != null)
            {
                case true:
                    Post.ToePlateHole1 =
                        +Rail.Height
                        - Clearance
                        - 2.5   // ToePlate.Height / 2
                        - value / 2;
                    Post.ToePlateHole2 = value;

                    Post.HoleToEndDistance = SteelBook.Channel[C].WebGage;
                    Post.HoleToHoleSpacing =
                        +SteelBook.Channel[C].Depth
                        - SteelBook.Channel[C].WebGage * 2;

                    Post.Length =
                        +Rail.Height
                        - Post.RailingToPostDistance
                        + Reference.FloorHeight
                        + SteelBook.Channel[C].Depth;
                    break;
            }
        }
        private void SetRailProperties()
        {
            // Top rail
            Rail.Top.Title = "TOP-RAIL_WW";

            //Mid rail
            Rail.Mid.Title = "MID-RAIL_WW";
        }


        // Modify
        private void ModifyParts()
        {
            ModifyRightPost();
            ModifyLeftPost();
            ModifyMidPost();
            ModifyTopRail();
            if (Rail.Mid.PartNo.Dynamic != null) { ModifyMidRail(); }
            if (ToePlate.PartNo.Dynamic != null) { ModifyToePlate(); }
        }
        private AssemblyDoc ModifyHandRail()
        {
            // Open SOLIDWORKS file and obtain COM reference
            AssemblyDoc railAssembly = OpenAssembly(Rail.FilePath);
            Debug.WriteLine("Handrail Weldment");

            // Job info
            SetProperty("Project", Project, railAssembly);
            SetProperty("Bank", Bank, railAssembly);
            SetProperty("Customer", Customer, railAssembly);
            SetProperty("Client", Client, railAssembly);
            SetProperty("Location", Location, railAssembly);
            SetProperty("PO", PurchaseOrder, railAssembly);
            SetProperty("ItemNo", ItemNumber, railAssembly);

            // JDE info
            SetProperty("RMdesc", Rail.Description, railAssembly);
            SetProperty("PartNo", "28" + Bank + "-" + Rail.PartNo.Dynamic, railAssembly);
            SetProperty("Title", Rail.Title, railAssembly);

            // Drawing helper sketches
            EditDimension("height", "frontSketch", Post.Length + Post.RailingToPostDistance, railAssembly);
            EditDimension("length", "rightSketch", Rail.Length, railAssembly);
            EditDimension("spacing", "rightSketch", Post.Mid.Spacing, railAssembly);

            double adjustedCount = Post.Mid.Count;
            if (adjustedCount < 2)
            {
                adjustedCount = 2;
            }
            EditDimension("count", "rightSketch", adjustedCount, railAssembly);

            return railAssembly;
        }
        private void ModifyRightPost()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 rightPost = Open(Post.Right.FilePath);
            Debug.WriteLine("Right Post");

            // Job info
            SetProperty("Project", Project, rightPost);
            SetProperty("Bank", Bank, rightPost);
            SetProperty("Customer", Customer, rightPost);
            SetProperty("Client", Client, rightPost);
            SetProperty("Location", Location, rightPost);
            SetProperty("PO", PurchaseOrder, rightPost);
            SetProperty("ItemNo", ItemNumber, rightPost);

            // JDE info
            SetProperty("RMdesc", Post.Right.Description, rightPost);
            SetProperty("PartNo", "28" + Bank + "-" + Post.Right.PartNo.Dynamic, rightPost);
            SetProperty("RM", Post.Right.JDEnumber, rightPost);
            SetProperty("Title", Post.Right.Title, rightPost);

            //             dimName          treeName           newValue
            EditDimension("hole1", "sk:EndHoles", Post.ToePlateHole1, rightPost);
            EditDimension("hole2", "sk:EndHoles", Post.ToePlateHole2, rightPost);
            EditDimension("MidHole", "sk:EndHoles", Rail.Height / 2, rightPost);
            EditDimension("Diameter", "sk:EndHoles", HoleDiameter, rightPost);
            EditDimension("length", "body", Post.Length, rightPost);
            EditDimension("HoleToEndDistance", "holes", Post.HoleToEndDistance, rightPost);
            EditDimension("HoleToHole", "holes", Post.HoleToHoleSpacing, rightPost);


            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref rightPost);
        }
        private void ModifyRightPost(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyRightPost();
            if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            {
                Close(Post.Right.FilePath);
            }
        }
        private void ModifyLeftPost()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 leftPost = Open(Post.Left.FilePath);
            Debug.WriteLine("Left Post");

            // Job info
            SetProperty("Project", Project, leftPost);
            SetProperty("Bank", Bank, leftPost);
            SetProperty("Customer", Customer, leftPost);
            SetProperty("Client", Client, leftPost);
            SetProperty("Location", Location, leftPost);
            SetProperty("PO", PurchaseOrder, leftPost);
            SetProperty("ItemNo", ItemNumber, leftPost);

            // JDE info
            SetProperty("RMdesc", Post.Left.Description, leftPost);
            SetProperty("PartNo", "28" + Bank + "-" + Post.Left.PartNo.Dynamic, leftPost);
            SetProperty("RM", Post.Left.JDEnumber, leftPost);
            SetProperty("Title", Post.Left.Title, leftPost);

            //             dimName          treeName           newValue
            EditDimension("hole1", "sk:EndHoles", Post.ToePlateHole1, leftPost);
            EditDimension("hole2", "sk:EndHoles", Post.ToePlateHole2, leftPost);
            EditDimension("MidHole", "sk:EndHoles", Rail.Height / 2, leftPost);
            EditDimension("Diameter", "sk:EndHoles", HoleDiameter, leftPost);
            EditDimension("length", "body", Post.Length, leftPost);
            EditDimension("HoleToEndDistance", "holes", Post.HoleToEndDistance, leftPost);
            EditDimension("HoleToHole", "holes", Post.HoleToHoleSpacing, leftPost);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref leftPost);
        }
        private void ModifyLeftPost(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyLeftPost();
            if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            {
                Close(Post.Left.FilePath);
            }
        }
        private void ModifyMidPost()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 midPost = Open(Post.Mid.FilePath);
            Debug.WriteLine("Mid Post");

            // Job info
            SetProperty("Project", Project, midPost);
            SetProperty("Bank", Bank, midPost);
            SetProperty("Customer", Customer, midPost);
            SetProperty("Client", Client, midPost);
            SetProperty("Location", Location, midPost);
            SetProperty("PO", PurchaseOrder, midPost);
            SetProperty("ItemNo", ItemNumber, midPost);

            // JDE info
            SetProperty("RMdesc", Post.Mid.Description, midPost);
            SetProperty("PartNo", "28" + Bank + "-" + Post.Mid.PartNo.Dynamic, midPost);
            SetProperty("RM", Post.Mid.JDEnumber, midPost);
            SetProperty("Title", Post.Mid.Title, midPost);

            //             dimName          treeName           newValue
            EditDimension("Diameter", "holes", HoleDiameter, midPost);
            EditDimension("length", "body", Post.Length, midPost);
            EditDimension("HoleToEndDistance", "holes", Post.HoleToEndDistance, midPost);
            EditDimension("HoleToHole", "holes", Post.HoleToHoleSpacing, midPost);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref midPost);
        }
        private void ModifyMidPost(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyMidPost();
            if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            {
                Close(Post.Mid.FilePath);
            }
        }
        private void ModifyTopRail()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 topRail = Open(Rail.Top.FilePath);
            Debug.WriteLine("Top Rail");

            // Job info
            SetProperty("Project", Project, topRail);
            SetProperty("Bank", Bank, topRail);
            SetProperty("Customer", Customer, topRail);
            SetProperty("Client", Client, topRail);
            SetProperty("Location", Location, topRail);
            SetProperty("PO", PurchaseOrder, topRail);
            SetProperty("ItemNo", ItemNumber, topRail);

            // JDE info
            SetProperty("RMdesc", Rail.Top.Description, topRail);
            SetProperty("PartNo", "28" + Bank + "-" + Rail.Top.PartNo.Dynamic, topRail);
            SetProperty("RM", Rail.Top.JDEnumber, topRail);
            SetProperty("Title", Rail.Top.Title, topRail);

            //             dimName          treeName           newValue
            EditDimension("length", "body", Rail.Length, topRail);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref topRail);
        }
        private void ModifyTopRail(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyTopRail();
            if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            {
                Close(Rail.Top.FilePath);
            }
        }
        private void ModifyMidRail()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 midRail = Open(Rail.Mid.FilePath);
            Debug.WriteLine("Mid Rail");

            // Job info
            SetProperty("Project", Project, midRail);
            SetProperty("Bank", Bank, midRail);
            SetProperty("Customer", Customer, midRail);
            SetProperty("Client", Client, midRail);
            SetProperty("Location", Location, midRail);
            SetProperty("PO", PurchaseOrder, midRail);
            SetProperty("ItemNo", ItemNumber, midRail);

            // JDE info
            SetProperty("RMdesc", Rail.Mid.Description, midRail);
            SetProperty("PartNo", "28" + Bank + "-" + Rail.Mid.PartNo.Dynamic, midRail);
            SetProperty("RM", Rail.Mid.JDEnumber, midRail);
            SetProperty("Title", Rail.Mid.Title, midRail);

            //             dimName          treeName           newValue
            EditDimension("length", "body", Rail.Length, midRail);
            EditDimension("MidCount", "sk:Cope", Post.Mid.Count + 1, midRail);
            EditDimension("MidSpacing", "sk:Cope", Post.Mid.Spacing, midRail);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref midRail);
        }
        private void ModifyMidRail(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyMidRail();
            if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            {
                Close(Rail.Mid.FilePath);
            }
        }
        private void ModifyToePlate()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 toePlate = Open(ToePlate.FilePath);
            Debug.WriteLine("Toe Plate");

            // Job info
            SetProperty("Project", Project, toePlate);
            SetProperty("Bank", Bank, toePlate);
            SetProperty("Customer", Customer, toePlate);
            SetProperty("Client", Client, toePlate);
            SetProperty("Location", Location, toePlate);
            SetProperty("PO", PurchaseOrder, toePlate);
            SetProperty("ItemNo", ItemNumber, toePlate);

            // JDE info
            SetProperty("RMdesc", ToePlate.Description, toePlate);
            SetProperty("PartNo", "28" + Bank + "-" + ToePlate.PartNo.Dynamic, toePlate);
            SetProperty("RM", ToePlate.JDEnumber, toePlate);
            SetProperty("Title", ToePlate.Title, toePlate);

            //             dimName          treeName           newValue
            EditDimension("Length", "sk:Body", Rail.Length, toePlate);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref toePlate);
        }
        private void ModifyToePlate(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> handrailComponents)
        {
            ModifyToePlate();
            //if (handrailComponents.Any(entry => entry.Value.FileName == Rail.FilePath))
            //{
            //    Close(ToePlate.FilePath);
            //}
        }


        // Assemble
        private void AssembleHandrail()
        {
            AssemblyDoc railWeldment = ModifyHandRail();

            // Record components with user defined locations
            List<Component2> unfixedComponents = ListUnfixedComponents(railWeldment);

            // Check assembly for existing component instances
            List<Component2> topRailList = Rail.Top.PartNo.Dynamic != null
                ? FindMatchingComponents(Rail.Top.FilePath, railWeldment)
                : new List<Component2>();

            List<Component2> midRailList = Rail.Mid.PartNo.Dynamic != null
                ? FindMatchingComponents(Rail.Mid.FilePath, railWeldment)
                : new List<Component2>();

            List<Component2> rightPostList = Post.Right.PartNo.Dynamic != null
                ? FindMatchingComponents(Post.Right.FilePath, railWeldment)
                : new List<Component2>();

            List<Component2> leftPostList = Post.Left.PartNo.Dynamic != null
                ? FindMatchingComponents(Post.Left.FilePath, railWeldment)
                : new List<Component2>();

            List<Component2> midtPostList = Post.Mid.PartNo.Dynamic != null
                ? FindMatchingComponents(Post.Mid.FilePath, railWeldment)
                : new List<Component2>();

            List<Component2> toePlateList = ToePlate.PartNo.Dynamic != null
                ? FindMatchingComponents(ToePlate.FilePath, railWeldment)
                : new List<Component2>();

            // Place components and close SOLIDWORKS file
            PlaceTopRail(topRailList, railWeldment);
            PlaceMidRail(midRailList, railWeldment);
            PlaceRightEndPost(rightPostList, railWeldment);
            PlaceLeftEndPost(leftPostList, railWeldment);
            PlaceMidPosts(midtPostList, railWeldment);
            if (ToePlate.PartNo.Dynamic != null) { PlaceToePlate(toePlateList, railWeldment); }

            // Fix all components
            FixComponentLocations(railWeldment);

            // Float parts marked with a manually modifed location
            UnfixComponentLocations(railWeldment, unfixedComponents);

            SaveEverything();

            Optimize.Release(ref railWeldment);
        }
        private void PlaceRightEndPost(List<Component2> componentList, AssemblyDoc assembly)
        {
            bool markedToClose = false;

            Component2 rightPost = GetInstance(componentList, 1); Debug.WriteLine(" (right post)");

            if (rightPost == null && Post.Right.PartNo.Dynamic != null)
            {
                rightPost = InsertComponent(Post.Right.FilePath, assembly);
                markedToClose = true;
            }

            if (rightPost != null)
            {
                // Set location
                Z_Translation(-Rail.Length / 2);
                ApplyPositionInformation(rightPost);

                // Release COM object
                Optimize.Release(ref rightPost);
            }

            if (markedToClose)
            {
                Close(Post.Right.FilePath);
            }
        }
        private void PlaceLeftEndPost(List<Component2> componentList, AssemblyDoc assembly)
        {
            bool markedToClose = false;

            Component2 leftPost = GetInstance(componentList, 1); Debug.WriteLine(" (left post)");

            if (leftPost == null && Post.Left.PartNo.Dynamic != null)
            {
                leftPost = InsertComponent(Post.Left.FilePath, assembly);
                markedToClose = true;
            }

            if (leftPost != null)
            {
                // Set location
                Z_Translation(Rail.Length / 2);
                ApplyPositionInformation(leftPost);

                // Release COM object
                Optimize.Release(ref leftPost);
            }

            if (markedToClose)
            {
                Close(Post.Left.FilePath);
            }
            
        }
        private void PlaceMidPosts(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            List<Component2> processedComponents = new List<Component2>();

            for (int i = 1; i < Post.Mid.Count + 1; i++)
            {
                Component2 midPost = GetInstance(componentList, i); 

                if (midPost == null && Post.Mid.PartNo.Dynamic != null)
                {
                    midPost = InsertComponent(Post.Mid.FilePath, assemblyDoc); Debug.WriteLine($" (mid post{i + 1})");
                    if (i == 1)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (mid post{i + 1})");
                    processedComponents.Add(midPost);
                }

                if (midPost != null)
                {
                    // Set location
                    Z_Translation(Rail.Length / 2 - Post.Gage - Post.Mid.Spacing * i);
                    ApplyPositionInformation(midPost);

                    // Release COM object
                    Optimize.Release(ref midPost);
                }
            }

            // Remove items in List that are not in processedComponents
            foreach (var component in componentList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Rail.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                Close(Post.Mid.FilePath);
            }
        }
        private void PlaceTopRail(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 topRail = GetInstance(componentList, 1); Debug.WriteLine(" (top rail)");

            if (topRail == null && Rail.Top.PartNo.Dynamic != null)
            {
                topRail = InsertComponent(Rail.Top.FilePath, assemblyDoc);
                markedToClose = true;
            }

            if (topRail != null)
            {
                ApplyPositionInformation(topRail);

                Optimize.Release(ref topRail);
            }

            if (markedToClose)
            {
                Close(Rail.Top.FilePath);
            }

        }
        private void PlaceMidRail(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 midRail = GetInstance(componentList, 1); Debug.WriteLine(" (mid rail)");

            if (midRail == null && Rail.Mid.PartNo.Dynamic != null)
            {
                midRail = InsertComponent(Rail.Mid.FilePath, assemblyDoc);
                markedToClose = true;
            }

            if (midRail != null)
            {
                Y_Translation(-Rail.Height / 2);
                ApplyPositionInformation(midRail);

                Optimize.Release(ref midRail);
            }

            if (markedToClose)
            {
                Close(Rail.Mid.FilePath);
            }
        }
        private void PlaceToePlate(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 toePlate = GetInstance(componentList, 1); Debug.WriteLine(" (toeplate)");

            if (toePlate == null && ToePlate.PartNo.Dynamic != null)
            {
                toePlate = InsertComponent(ToePlate.FilePath, assemblyDoc);
                markedToClose = true;
            }

            if (toePlate != null)
            {
                // Set location
                Y_Translation(-Rail.Height + ToePlate.Height / 2 + Clearance);
                ApplyPositionInformation(toePlate);

                // Release COM object
                Optimize.Release(ref toePlate);
            }

            if (markedToClose)
            {
                Close(ToePlate.FilePath);
            }
        }
    }
}
