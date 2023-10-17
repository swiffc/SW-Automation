using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swcommands;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Markup;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar;

namespace Walkway
{
    internal class Support : Walkway
    {
        // Create a new support
        public Support(char bank, int minimumStringerSize, double walkwayWidth, double offsetFromColumnCenter, string columnSize, double walkwayLength, bool supportOnly)
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
                    PassUserInputs(minimumStringerSize, walkwayWidth, offsetFromColumnCenter, columnSize, walkwayLength);
                    SetStaticPartNumbers();
                    SetBeamProperties();
                    SetBraceProperties();     

                    // Create desktop instances of platform files with unique part numbers
                    Beam.PartNo.Dynamic = CreateNew_ComponentFile(Beam.PartNo.Static);
                    Brace.PartNo.Dynamic = CreateNew_ComponentFile(Brace.PartNo.Static);
                    Spacer.PartNo.Dynamic = CreateNew_ComponentFile(Spacer.PartNo.Static);

                    Reference.PartNo.Dynamic =  CreateHelperAssembly("_");

                    Beam.Part.PartNo.Dynamic = CreateNew_SubComponentFile(Beam.Part.PartNo.Static);
                    EndPlate.PartNo.Dynamic = CreateNew_SubComponentFile(EndPlate.PartNo.Static);
                    BraceClip.PartNo.Dynamic = CreateNew_SubComponentFile(BraceClip.PartNo.Static);

                    // Load part files into memory and pass properties
                    ModifyBeam();
                    ModifyEndPlate();
                    ModifyBraceClip();
                    ModifyBrace();
                    ModifySpacer();

                    // Create weldment
                    AssembleBeamWeldment();

                    // Create helper assembly
                    AssembleSupport();

                    // Create temporary drawing
                    string tempDrawingPath = CreateTemporary_PlatformDesktopDrawing(Beam.PartNo.Static, Beam.PartNo.Dynamic);

                    // Replace template references with desktop references
                    ReplaceDrawingReference(Beam.PartNo.Dynamic, tempDrawingPath, Beam.PartNo.Static);
                    ReplaceDrawingReference(Beam.Part.PartNo.Dynamic, tempDrawingPath, Beam.Part.PartNo.Static);
                    ReplaceDrawingReference(EndPlate.PartNo.Dynamic, tempDrawingPath, EndPlate.PartNo.Static);
                    ReplaceDrawingReference(BraceClip.PartNo.Dynamic, tempDrawingPath, BraceClip.PartNo.Static);
                    ReplaceDrawingReference(Brace.PartNo.Dynamic, tempDrawingPath, Brace.PartNo.Static);
                    ReplaceDrawingReference(Spacer.PartNo.Dynamic, tempDrawingPath, Spacer.PartNo.Static);

                    // Open drawing
                    DrawingDoc temp_DesktopDrawingDoc = OpenDrawing(tempDrawingPath);

                    // Rename sheets
                    RenameSheet("132_Spacer", Spacer.PartNo.Dynamic, temp_DesktopDrawingDoc);
                    RenameSheet("1326_SupportBeam", Beam.PartNo.Dynamic, temp_DesktopDrawingDoc);
                    RenameSheet("1326P_Beam", Beam.Part.PartNo.Dynamic, temp_DesktopDrawingDoc);
                    RenameSheet("1327_EndPlate", EndPlate.PartNo.Dynamic, temp_DesktopDrawingDoc);
                    RenameSheet("1328_BraceClip", BraceClip.PartNo.Dynamic, temp_DesktopDrawingDoc);
                    RenameSheet("1331_Brace", Brace.PartNo.Dynamic, temp_DesktopDrawingDoc);

                    // Migrate sheets
                    MigrateSheetsToBankDrawing(temp_DesktopDrawingDoc);

                    // Open bank and open Insert Component property manager page
                    AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);


                    //-------------------------------------------------------


                    if (supportOnly)
                    {
                        // Initiate InsertComponents command
                        SW.RunCommand((int)swCommands_e.swCommands_InsertComponents, null);

                        // Release COM reference to bank and close top level assembly
                        Optimize.Release(ref bankAssembly);
                        Close(Reference.FilePath);
                    }
                    
                }
                else
                {
                    AddNew_Bank();
                }
            } while (!bankExists);

        }

        // Modify existing support
        public Support(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, int minimumStringerSize, double walkwayWidth, double offsetFromColumnCenter, string columnSize, double walkwayLength, bool updateLocations)
        {
            // Get active doc where program is initiated
            string activeDocPath = SW.IActiveDoc2.GetPathName();

            // Dictionary mapping the static part numbers to their descriptions for debugging
            Dictionary<string, string> staticPartDescriptions = new Dictionary<string, string>
            {
                { SupportAssembly_PartNo_Static, "Support assembly (reference only)" },
                { SupportBeamWeldment_PartNo_Static, "SUPPORT-BEAM_WELDMENT" },
                { SupportBeamPart_PartNo_Static, "SUPPORT-BEAM" },
                { EndPlate_PartNo_Static, "BEAM-END-PLATE" },
                { BraceClip_PartNo_Static, "KNEE-BRACE-CLIP" },
                { KneeBrace_PartNo_Static, "KNEE-BRACE" },
                { BraceSpacer_PartNo_Static, "KNEE-BRACE-SPACER" }
            };

            // Itemized dictionary of support components
            var supportComponents = FilterComponents(selectedComponents, staticPartDescriptions); Debug.WriteLine("Support");
            if (supportComponents.Count == 0) { DevTools.EnablePartUI(); return; }

            // Update locations option
            if (updateLocations) { AddParentToSelection(supportComponents); }

            // Assign exisitng dynamic part numbers to class
            Beam.PartNo.Dynamic = GetDynamicPartNo(supportComponents, SupportBeamWeldment_PartNo_Static);
            Beam.Part.PartNo.Dynamic = GetDynamicPartNo(supportComponents, SupportBeamPart_PartNo_Static);
            EndPlate.PartNo.Dynamic = GetDynamicPartNo(supportComponents, EndPlate_PartNo_Static);
            BraceClip.PartNo.Dynamic = GetDynamicPartNo(supportComponents, BraceClip_PartNo_Static);
            Brace.PartNo.Dynamic = GetDynamicPartNo(supportComponents, KneeBrace_PartNo_Static);
            Spacer.PartNo.Dynamic = GetDynamicPartNo(supportComponents, BraceSpacer_PartNo_Static);

            Reference.FilePath = $@"{DesktopFolderPath}\{Project}-28{Bank}-{Beam.PartNo.Dynamic}_.SLDASM";

            // Assign properties
            char bank = GetBankFrom(supportComponents); // Assign bank if ONLY one bank is found. Otherwise, exit the program.
            PassUserInputs(minimumStringerSize, walkwayWidth, offsetFromColumnCenter, columnSize, walkwayLength);
            SetStaticPartNumbers();
            SetBeamProperties();
            SetBraceProperties();

            // Only modify parts in the found in the dictionary
            var conditionsAndActions = new
                List<(Func<KeyValuePair<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>, bool> condition,
                      Action<Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>> action)>
            {
                (entry => entry.Value.StaticPartNo == SupportBeamPart_PartNo_Static, ModifyBeam),
                (entry => entry.Value.StaticPartNo == EndPlate_PartNo_Static, ModifyEndPlate),
                (entry => entry.Value.StaticPartNo == BraceClip_PartNo_Static, ModifyBraceClip),
                (entry => entry.Value.StaticPartNo == KneeBrace_PartNo_Static, ModifyBrace),
                (entry => entry.Value.StaticPartNo == BraceSpacer_PartNo_Static, ModifySpacer)
            };
            foreach (var (condition, action) in conditionsAndActions)
            {
                if (supportComponents.Any(condition))
                {
                    action(supportComponents);
                }
            }

            // Only move selected parts in the assembly
            if (supportComponents.Any(entry => entry.Value.StaticPartNo == SupportBeamWeldment_PartNo_Static))
            {
                AssembleBeamWeldment();

                if (activeDocPath != Beam.FilePath)
                {
                    Close(Beam.FilePath);
                }
            }

            if (supportComponents.Any(entry => entry.Value.StaticPartNo == SupportAssembly_PartNo_Static))
            {
                AssembleSupport();

                if (activeDocPath != Reference.FilePath)
                {
                    Close(Reference.FilePath);
                }
            }

            Debug.WriteLine("");

        }


        // Extensions
        public HelperAssemblyExtensions Reference { get; set; } = new HelperAssemblyExtensions();
        public class HelperAssemblyExtensions
        {
            public string FilePath { get; set; }
            public int StringerSize { get; set; }
            public double StringerDepth { get; set; }
            public double WalkwayWidth { get; set; }
            public double WalkwayLength { get; set; }
            public double OffsetFromColumnCenter { get; set; }
            public string ColumnSize { get; set; }
            public double FloorHeight => 1.25;
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public BeamExtensions Beam { get; set; } = new BeamExtensions();
        public class BeamExtensions
        {
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public string Title         { get; set; } = "SUPPORT-BEAM_WELDMENT_WW";
            public string Description   { get; set; } = "SUPPORT-BEAM_WELDMENT_WW";
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDASM";
            public BeamSubExtensions Part { get; set; } = new BeamSubExtensions();
            public double Height { get; set; } = 6;
        }
        public class BeamSubExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double Length { get; set; }
            public string Title { get; set; } = "SUPPORT-BEAM_WW";
            public string Description { get; set; } = "BEAM_W_6x15_A992";
            public string JDEnumber { get; set; } = "13011";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public EndPlateExtensions EndPlate { get; set; } = new EndPlateExtensions();
        public class EndPlateExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double THK { get; set; } = 0.5;
            public double Height { get; set; } = 6;
            public string Title { get; set; } = "BEAM-END-PLATE_WW";
            public string Description { get; set; } = "PLATE_1/4\"_A572_50";
            public string JDEnumber { get; set; } = "60038";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public BraceClipExtensions BraceClip { get; set; } = new BraceClipExtensions();
        public class BraceClipExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double HoleToEdge { get; set; } = 2.5;
            public double THK { get; set; } = 0.25;
            public string Title { get; set; } = "KNEE-BRACE-CLIP_WW";
            public string Description { get; set; } = "PLATE_1/4\"_A572_50";
            public string JDEnumber { get; set; } = "60038";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public KneeBraceExtensions Brace { get; set; } = new KneeBraceExtensions();
        public class KneeBraceExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public double Length { get; set; }
            public double HoleToEnd { get; set; } = 1.125;
            public double HoleToHole { get; set; } = 2.5;
            public double IntermediateSpan { get; set; }
            public int IntermediateCount { get; set; }
            public double IntermediateSpacing { get; set; }
            public string Title { get; set; } = "KNEE-BRACE_WW";
            public string Description { get; set; } = "ANGLE_3\"x3\"x1/4\"_A572-50";
            public string JDEnumber { get; set; } = "54986";
        }
        public BraceSpacerExtensions Spacer { get; set; } = new BraceSpacerExtensions();
        public class BraceSpacerExtensions
        {
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public string Title { get; set; } = "KNEE-BRACE-SPACER_FIELD";
            public string Description { get; set; } = "PLATE_1/4\"_A572_50";
            public string JDEnumber { get; set; } = "60038";
        }
        public class PartNoSubExtensions
        {
            public string Static { get; set; }
            public string Dynamic { get; set; }
        }


        // Properties
        private void PassUserInputs(int minimumStringerSize, double walkwayWidth, double offsetFromColumnCenter, string columnSize, double walkwayLength)
        {
            Reference.WalkwayLength = walkwayLength;
            Debug.WriteLine($"REF walkway length: {walkwayLength}");

            if (Reference.WalkwayLength < 132 && minimumStringerSize < 6)
            {
                Reference.StringerSize = 0;
                Reference.StringerDepth = 0.375;
            }
            else if (Reference.WalkwayLength < 168 && minimumStringerSize == 6)
            {
                Reference.StringerSize = 6;
                Reference.StringerDepth = SteelBook.Channel["C6x8.2"].Depth;
            }
            else if (Reference.WalkwayLength < 198 && minimumStringerSize <= 8)
            {
                Reference.StringerSize = 8;
                Reference.StringerDepth = SteelBook.Channel["C8x11.5"].Depth;
            }
            else // C10x15.3
            {
                Reference.StringerSize = 10;
                Reference.StringerDepth = SteelBook.Channel["C10x15.3"].Depth;
            }
            Debug.WriteLine($"REF stringer size: {Reference.StringerSize}");

            Reference.WalkwayWidth = walkwayWidth;
            Debug.WriteLine($"REF walkway width: {walkwayWidth}");

            Reference.OffsetFromColumnCenter = offsetFromColumnCenter;
            Debug.WriteLine($"REF offset from column center: {offsetFromColumnCenter}");

            Reference.ColumnSize = columnSize;
            Debug.WriteLine($"REF column size: {columnSize}");
        }
        private void SetStaticPartNumbers()
        {
            Reference.PartNo.Static = SupportAssembly_PartNo_Static;
            Beam.PartNo.Static = SupportBeamWeldment_PartNo_Static;
            Beam.Part.PartNo.Static = SupportBeamPart_PartNo_Static;
            EndPlate.PartNo.Static = EndPlate_PartNo_Static;
            BraceClip.PartNo.Static = BraceClip_PartNo_Static;
            Brace.PartNo.Static = KneeBrace_PartNo_Static;
            Spacer.PartNo.Static = BraceSpacer_PartNo_Static;
        }
        private string CreateHelperAssembly(string suffix)
        {
            Reference.FilePath = $@"{DesktopFolderPath}\{Project}-28{Bank}-{Beam.PartNo.Dynamic}{suffix}.SLDASM";
            CopyAsReadWrite($@"{TemplateFolderPath}\JOBNO-{Reference.PartNo.Static}.SLDASM", Reference.FilePath);

            return $"{Beam.PartNo.Dynamic}{suffix}";
        }

        private void SetBeamProperties()
        {
            Beam.Part.Length = Reference.OffsetFromColumnCenter + Reference.WalkwayWidth - SteelBook.W_Shape[Reference.ColumnSize].Depth / 2 - EndPlate.THK * 2;
        }
        private void SetBraceProperties()
        {
            Brace.Length =
                // horizontal distance from column clip hole to beam clip hole
                (Beam.Part.Length + EndPlate.THK * 2 - BraceClip.HoleToEdge * 2)

                // hole to hole hypotenuse at 45 degs
                * Math.Sqrt(2)

                // far end of brace
                + Brace.HoleToEnd * 2;

            Brace.IntermediateSpan = Brace.Length - Brace.HoleToEnd * 2 - Brace.HoleToHole * 2;
            Brace.IntermediateCount = (int)Brace.IntermediateSpan / 24;
            Brace.IntermediateSpacing = Brace.IntermediateSpan / (Brace.IntermediateCount + 1);
        }


        // Modify
        private AssemblyDoc ModifySupportBeam()
        {
            // Open SOLIDWORKS file and obtain COM reference
            AssemblyDoc beamWeldment = OpenAssembly(Beam.FilePath);

            // Job info
            SetProperty("Project", Project, beamWeldment);
            SetProperty("Bank", Bank, beamWeldment);
            SetProperty("Customer", Customer, beamWeldment);
            SetProperty("Client", Client, beamWeldment);
            SetProperty("Location", Location, beamWeldment);
            SetProperty("PO", PurchaseOrder, beamWeldment);
            SetProperty("ItemNo", ItemNumber, beamWeldment);

            // JDE info
            SetProperty("RMdesc", Beam.Description, beamWeldment);
            SetProperty("PartNo", "28" + Bank + "-" + Beam.PartNo.Dynamic, beamWeldment);
            SetProperty("Title", Beam.Title, beamWeldment);

            // Drawing helper sketches
            EditDimension("length", "frontSketch", Beam.Part.Length, beamWeldment);

            return beamWeldment;
        }
        private void ModifyBeam()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 beam = Open(Beam.Part.FilePath);
            Debug.WriteLine("Beam Part");

            // Job info
            SetProperty("Project", Project, beam);
            SetProperty("Bank", Bank, beam);
            SetProperty("Customer", Customer, beam);
            SetProperty("Client", Client, beam);
            SetProperty("Location", Location, beam);
            SetProperty("PO", PurchaseOrder, beam);
            SetProperty("ItemNo", ItemNumber, beam);

            // JDE info
            SetProperty("RMdesc", Beam.Part.Description, beam);
            SetProperty("PartNo", "28" + Bank + "-" + Beam.Part.PartNo.Dynamic, beam);
            SetProperty("RM", Beam.Part.JDEnumber, beam);
            SetProperty("Title", Beam.Part.Title, beam);

            double gage;

            if (Reference.StringerSize == 6)
            {
                gage = SteelBook.Channel["C6x8.2"].FlangeGage;
            }
            else if (Reference.StringerSize > 6 && Reference.StringerSize <= 8)
            {
                gage = SteelBook.Channel["C8x11.5"].FlangeGage;
            }
            else if (Reference.StringerSize > 8)
            {
                gage = SteelBook.Channel["C10x15.3"].FlangeGage;
            }
            else // L-stringer
            {
                gage = 2.5; // Stringer.BottomLegGage
            }

            //             dimName   treeName        newValue
            EditDimension("length", "body", Beam.Part.Length, beam);
            EditDimension("Diameter", "holes", HoleDiameter, beam);
            EditDimension("gage", "holes", gage, beam);
            EditDimension("Hole2Hole", "holes", Reference.WalkwayWidth - gage * 2, beam);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref beam);
        }
        private void ModifyBeam(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> supportComponents)
        {
            ModifyBeam();
            if (supportComponents.Any(entry => entry.Value.FileName == Beam.FilePath))
            {
                Close(Beam.Part.FilePath);
            }
        }
        private void ModifyEndPlate()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 endPlate = Open(EndPlate.FilePath);
            Debug.WriteLine("End Plate");

            // Job info
            SetProperty("Project", Project, endPlate);
            SetProperty("Bank", Bank, endPlate);
            SetProperty("Customer", Customer, endPlate);
            SetProperty("Client", Client, endPlate);
            SetProperty("Location", Location, endPlate);
            SetProperty("PO", PurchaseOrder, endPlate);
            SetProperty("ItemNo", ItemNumber, endPlate);

            // JDE info
            SetProperty("RMdesc", EndPlate.Description, endPlate);
            SetProperty("PartNo", "28" + Bank + "-" + EndPlate.PartNo.Dynamic, endPlate);
            SetProperty("RM", EndPlate.JDEnumber, endPlate);
            SetProperty("Title", EndPlate.Title, endPlate);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref endPlate);
        }
        private void ModifyEndPlate(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> supportComponents)
        {
            ModifyEndPlate();
            if (supportComponents.Any(entry => entry.Value.FileName == Beam.FilePath))
            {
                Close(EndPlate.FilePath);
            }
        }
        private void ModifyBraceClip()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 clip = Open(BraceClip.FilePath);
            Debug.WriteLine("Brace Clip");

            // Job info
            SetProperty("Project", Project, clip);
            SetProperty("Bank", Bank, clip);
            SetProperty("Customer", Customer, clip);
            SetProperty("Client", Client, clip);
            SetProperty("Location", Location, clip);
            SetProperty("PO", PurchaseOrder, clip);
            SetProperty("ItemNo", ItemNumber, clip);

            // JDE info
            SetProperty("RMdesc", BraceClip.Description, clip);
            SetProperty("PartNo", "28" + Bank + "-" + BraceClip.PartNo.Dynamic, clip);
            SetProperty("RM", BraceClip.JDEnumber, clip);
            SetProperty("Title", BraceClip.Title, clip);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref clip);
        }
        private void ModifyBraceClip(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> supportComponents)
        {
            ModifyBraceClip();
            if (supportComponents.Any(entry => entry.Value.FileName == Beam.FilePath))
            {
                Close(BraceClip.FilePath);
            }
        }
        private void ModifyBrace()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 brace = Open(Brace.FilePath);
            Debug.WriteLine("Knee Brace");

            // Job info
            SetProperty("Project", Project, brace);
            SetProperty("Bank", Bank, brace);
            SetProperty("Customer", Customer, brace);
            SetProperty("Client", Client, brace);
            SetProperty("Location", Location, brace);
            SetProperty("PO", PurchaseOrder, brace);
            SetProperty("ItemNo", ItemNumber, brace);

            // JDE info
            SetProperty("RMdesc", Brace.Description, brace);
            SetProperty("PartNo", "28" + Bank + "-" + Brace.PartNo.Dynamic, brace);
            SetProperty("RM", Brace.JDEnumber, brace);
            SetProperty("Title", Brace.Title, brace);

            double a = 0;
            double b = 0;
            int adjustedCount = Brace.IntermediateCount;
            if (Brace.IntermediateCount < 1)
            {
                b = Brace.IntermediateSpan;
            }
            if (Brace.IntermediateCount < 2)
            {
                adjustedCount = 2;
                a = Brace.IntermediateSpan;
            }
            EditDimension("length", "body", Brace.Length - Brace.HoleToEnd, brace);
            EditDimension("spacing", "holes", Brace.IntermediateSpacing + b, brace);
            EditDimension("spacing2", "holes", Brace.IntermediateSpacing + a, brace);
            EditDimension("count", "holes", adjustedCount, brace);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref brace);
        }
        private void ModifyBrace(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> supportComponents)
        {
            ModifyBrace();
            if (supportComponents.Any(entry => entry.Value.FileName == Reference.FilePath))
            {
                Close(Brace.FilePath);
            }
        }
        private void ModifySpacer()
        {
            ModelDoc2 spacerPart = Open(Spacer.FilePath);
            Debug.WriteLine("Brace Spacer");

            SetProperty("Project", Project, spacerPart);
            SetProperty("Bank", Bank, spacerPart);
            SetProperty("Customer", Customer, spacerPart);
            SetProperty("Client", Client, spacerPart);
            SetProperty("Location", Location, spacerPart);
            SetProperty("PO", PurchaseOrder, spacerPart);
            SetProperty("ItemNo", ItemNumber, spacerPart);

            SetProperty("RMdesc", Spacer.Description, spacerPart);
            SetProperty("PartNo", "28" + Bank + "-" + Spacer.PartNo.Dynamic, spacerPart);
            SetProperty("RM", Spacer.JDEnumber, spacerPart);
            SetProperty("Title", Spacer.Title, spacerPart);

            Optimize.Release(ref spacerPart);
        }
        private void ModifySpacer(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> supportComponents)
        {
            ModifySpacer();
            if (supportComponents.Any(entry => entry.Value.FileName == Reference.FilePath))
            {
                Close(Spacer.FilePath);
            }
        }


        // Assemble
        private void AssembleBeamWeldment()
        {
            AssemblyDoc beamWeldment = ModifySupportBeam();

            // Record components with user defined locations
            List<Component2> unfixedComponents = ListUnfixedComponents(beamWeldment);

            // Check assembly for existing component instances
            List<Component2> beamList = Beam.Part.PartNo.Dynamic != null
                ? FindMatchingComponents(Beam.Part.FilePath, beamWeldment)
                : new List<Component2>();

            List<Component2> endPlateList = EndPlate.PartNo.Dynamic != null
                ? FindMatchingComponents(EndPlate.FilePath, beamWeldment)
                : new List<Component2>();

            List<Component2> braceClipList = BraceClip.PartNo.Dynamic != null
                ? FindMatchingComponents(BraceClip.FilePath, beamWeldment)
                : new List<Component2>();

            // Place components and close SOLIDWORKS file
            PlaceBeam(beamList, beamWeldment);
            PlaceEndPlate(endPlateList, beamWeldment);
            PlaceBraceClip(braceClipList, beamWeldment);

            // Fix all components
            FixComponentLocations(beamWeldment);

            // Float parts marked with a manually modifed location
            UnfixComponentLocations(beamWeldment, unfixedComponents);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref beamWeldment);            
        }
        private void AssembleSupport()
        {
            // Open SOLIDWORKS file and obtain COM reference
            AssemblyDoc supportAssembly = OpenAssembly(Reference.FilePath);

            // Record components with user defined locations
            List<Component2> unfixedComponents = ListUnfixedComponents(supportAssembly);

            // Check assembly for existing component instances
            List<Component2> beamList = Beam.PartNo.Dynamic != null
                ? FindMatchingComponents(Beam.FilePath, supportAssembly)
                : new List<Component2>();

            List<Component2> braceList = Brace.PartNo.Dynamic != null
                ? FindMatchingComponents(Brace.FilePath, supportAssembly)
                : new List<Component2>();

            List<Component2> spacerList = Spacer.PartNo.Dynamic != null
                ? FindMatchingComponents(Spacer.FilePath, supportAssembly)
                : new List<Component2>();

            PlaceBeamWeldment(beamList, supportAssembly);
            PlaceBraces(braceList, supportAssembly);
            PlaceSpacers(spacerList, supportAssembly);

            // Fix all components
            FixComponentLocations(supportAssembly);

            // Float parts marked with a manually modifed location
            UnfixComponentLocations(supportAssembly, unfixedComponents);

            SaveEverything();

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref supportAssembly);
        }
        private void PlaceBeamWeldment(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            // Instance in componentList
            Component2 beamWeldment = GetInstance(componentList, 1); Debug.WriteLine(" (beam weldment)");

            // If the model does not contain component, place it
            if (beamWeldment == null && Beam.PartNo.Dynamic != null)
            {
                beamWeldment = InsertComponent(Beam.FilePath, assemblyDoc); Debug.WriteLine(" (beam weldment)");
                markedToClose = true;
            }

            if (beamWeldment != null)
            {
                X_Translation(Reference.WalkwayWidth / 2);
                ApplyPositionInformation(beamWeldment);
                Optimize.Release(ref assemblyDoc);
            }

            if (markedToClose)
            {
                Close(Beam.FilePath);
            }
        }
        private void PlaceBeam(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            // Instance in componentList
            Component2 beamPart = GetInstance(componentList, 1); Debug.WriteLine(" (beam part)");

            // If the model does not contain component, place it
            if (beamPart == null && Beam.Part.PartNo.Dynamic != null)
            {
                beamPart = InsertComponent(Beam.Part.FilePath, assemblyDoc); Debug.WriteLine(" (beam part)");
                markedToClose = true;
            }

            if (beamPart != null)
            {
                // Set location
                ApplyPositionInformation(beamPart);

                // Release COM object
                Optimize.Release(ref beamPart);
            }

            if (markedToClose)
            {
                Close(Beam.Part.FilePath);
            }
        }
        private void PlaceEndPlate(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 endPlate = GetInstance(componentList, 1); Debug.WriteLine(" (End plate)");

            if (endPlate == null && EndPlate.PartNo.Dynamic != null)
            {
                endPlate = InsertComponent(EndPlate.FilePath, assemblyDoc); Debug.WriteLine(" (End plate)");
                markedToClose = true;
            }

            if (endPlate != null)
            {
                X_Translation(-Beam.Part.Length);
                Y_Translation(-EndPlate.Height / 2);
                ApplyPositionInformation(endPlate);

                // Release COM object
                Optimize.Release(ref endPlate);
            }

            if (markedToClose)
            {
                Close(EndPlate.FilePath);
            }
        }
        private void PlaceBraceClip(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 braceClip = GetInstance(componentList, 1); Debug.WriteLine(" (brace clip)");

            if (braceClip == null && BraceClip.PartNo.Dynamic != null)
            {
                braceClip = InsertComponent(BraceClip.FilePath, assemblyDoc); Debug.WriteLine(" (brace clip)");
                markedToClose = true;
            }

            if (braceClip != null)
            {
                // Set location
                Y_Translation(-Beam.Height);
                ApplyPositionInformation(braceClip);

                // Release COM object
                Optimize.Release(ref braceClip);
            }

            if (markedToClose)
            {
                Close(BraceClip.FilePath);
            }
            
        }
        private void PlaceBraces(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            Component2 leftBrace = GetInstance(componentList, 1); Debug.WriteLine(" (left brace)");
            Component2 rightBrace = GetInstance(componentList, 2); Debug.WriteLine(" (right brace)");

            if (leftBrace == null && Brace.PartNo.Dynamic != null)
            {
                leftBrace = InsertComponent(Brace.FilePath, assemblyDoc); Debug.WriteLine(" (left brace)");
                markedToClose = true;
            }

            if (leftBrace != null)
            {
                // Set location
                X_Translation(Reference.WalkwayWidth / 2 - BraceClip.HoleToEdge);
                Y_Translation(
                    -Beam.Height - BraceClip.HoleToEdge);
                Z_Translation(BraceClip.THK / 2);
                Z_Axis_Rotate(45, 3, 'x');
                ApplyPositionInformation(leftBrace);

                // Release COM object
                Optimize.Release(ref leftBrace);
            }



            if (rightBrace == null && Brace.PartNo.Dynamic != null)
            {
                rightBrace = InsertComponent(Brace.FilePath, assemblyDoc); Debug.WriteLine(" (right brace)");
                markedToClose = true;
            }

            if (rightBrace != null)
            {
                // Set location
                X_Translation(-Reference.WalkwayWidth / 2 - Reference.OffsetFromColumnCenter
                    + SteelBook.W_Shape[Reference.ColumnSize].Depth / 2 + BraceClip.HoleToEdge);
                Y_Translation(
                    -Beam.Height - BraceClip.HoleToEdge
                    - (Beam.Part.Length + EndPlate.THK * 2 - BraceClip.HoleToEdge * 2));
                Z_Translation(-BraceClip.THK / 2);
                DevTools.PositionMaxtrix[0] = -Math.Cos(Math.PI / 4);
                DevTools.PositionMaxtrix[1] = -Math.Sin(Math.PI / 4);
                DevTools.PositionMaxtrix[3] = -Math.Sin(Math.PI / 4);
                DevTools.PositionMaxtrix[4] = Math.Cos(Math.PI / 4);
                DevTools.PositionMaxtrix[8] = -1;
                ApplyPositionInformation(rightBrace);

                // Release COM object
                Optimize.Release(ref rightBrace);
            }

            if (markedToClose)
            {
                Close(Brace.FilePath);
            }
            
        }
        private void PlaceSpacers(List<Component2> componentList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            List<Component2> processedComponents = new List<Component2>();

            for (int i = 1; i < Brace.IntermediateCount + 1; i++)
            {
                Component2 spacer = GetInstance(componentList, i); Debug.WriteLine($" (brace spacer{i})");

                if (spacer == null && Spacer.PartNo.Dynamic != null)
                {
                    spacer = InsertComponent(Spacer.FilePath, assemblyDoc); Debug.WriteLine($" (brace spacer{i})");
                    if (i == 1)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (brace spacer{i})");
                    processedComponents.Add(spacer);
                }

                if (spacer != null)
                {
                    double x =
                                        // to brace clip hole
                                        Reference.WalkwayWidth / 2 - BraceClip.HoleToEdge

                                        // to brace clip slot
                                        - Brace.HoleToHole / Math.Sqrt(2)

                                        // to first spacer hole
                                        - Brace.IntermediateSpacing / Math.Sqrt(2) * i;

                    double y =
                         // to brace clip hole
                         -Beam.Height - BraceClip.HoleToEdge

                        // to brace clip slot
                        - Brace.HoleToHole / Math.Sqrt(2)

                        // to first spacer hole
                        - Brace.IntermediateSpacing / Math.Sqrt(2) * i;

                    X_Translation(x);
                    Y_Translation(y);
                    Z_Axis_Rotate(45, 1, 'x');
                    ApplyPositionInformation(spacer);

                    Optimize.Release(ref spacer);
                }
            }

            // Remove items in componentList that are not in processedComponents
            foreach (var component in componentList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Reference.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                Close(Spacer.FilePath);
            }

        }
    }
}
