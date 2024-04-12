using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swcommands;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows.Markup;
using System.Windows.Shapes;
using Walkway.Tools;
using static System.Windows.Forms.VisualStyles.VisualStyleElement.Rebar;

namespace Walkway
{
    internal class WalkwayPlatform : Walkway
    {
        // Create a new platform
        public WalkwayPlatform(char bank, double length, double width, double floorHeight, int minimumStringerSize, double supportCenterToWalkwayEnd, bool platformOnly)
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
                    PassUserInputs(length, width, floorHeight, minimumStringerSize, supportCenterToWalkwayEnd);
                      SetStructuralMembers();
                      SetStaticPartNumbers();
                     SetStringerProperties();
                    SetEndMemberProperties(); 
                      SetGratingProperties();
                        SetStrutProperties(); 

                    // Create desktop instances of platform files with unique part numbers
                           Platform.PartNo.Dynamic =    CreateNew_ComponentFile        (Platform.PartNo.Static);
                           Stringer.PartNo.Dynamic = CreateNew_SubComponentFile        (Stringer.PartNo.Static);
                          EndMember.PartNo.Dynamic = CreateNew_SubComponentFile       (EndMember.PartNo.Static);
                    Grating.Primary.PartNo.Dynamic = CreateNew_SubComponentFile (Grating.Primary.PartNo.Static);
                     Grating.Filler.PartNo.Dynamic = CreateNew_SubComponentFile  (Grating.Filler.PartNo.Static);
                              Strut.PartNo.Dynamic = CreateNew_SubComponentFile           (Strut.PartNo.Static);

                    // Load part files into memory and pass properties
                    ModifyParts();

                    // Place instances of loaded part files into platform assembly file
                    AssemblePlatform();

                    // Create temporary platform drawing
                    string tempDrawingPath = CreateTemporary_PlatformDesktopDrawing(Platform.PartNo.Static, Platform.PartNo.Dynamic);

                    // Replace template references with desktop references
                    ReplaceDrawingReference(Platform.PartNo.Dynamic,        tempDrawingPath,       Platform.PartNo.Static);
                    ReplaceDrawingReference(Stringer.PartNo.Dynamic,        tempDrawingPath,       Stringer.PartNo.Static);
                    ReplaceDrawingReference(EndMember.PartNo.Dynamic,       tempDrawingPath,      EndMember.PartNo.Static);
                    ReplaceDrawingReference(Grating.Primary.PartNo.Dynamic, tempDrawingPath, Grating.Primary.PartNo.Static);
                    ReplaceDrawingReference(Grating.Filler.PartNo.Dynamic,  tempDrawingPath,  Grating.Filler.PartNo.Static);
                    ReplaceDrawingReference(Strut.PartNo.Dynamic,           tempDrawingPath,          Strut.PartNo.Static);

                    // Open drawing
                    DrawingDoc platform_DesktopDrawingDoc = OpenDrawing(tempDrawingPath);

                    // Rename sheets
                    RenameSheet("1336_Platform",        Platform.PartNo.Dynamic,        platform_DesktopDrawingDoc);
                    string sheet_StringerL = "1336L_Stringer";
                    string sheet_StringerC = "1336C_Stringer";
                    string sheet_EndL = "1337L_EndAngle";
                    string sheet_EndC = "1337C_EndChannel";
                    if (Stringer.Shape == 'L')
                    {
                        RenameSheet(sheet_StringerL, Stringer.PartNo.Dynamic, platform_DesktopDrawingDoc);
                        DeleteSheet(sheet_StringerC, platform_DesktopDrawingDoc);

                        RenameSheet(sheet_EndL, EndMember.PartNo.Dynamic, platform_DesktopDrawingDoc);
                        DeleteSheet(sheet_EndC, platform_DesktopDrawingDoc);
                    }
                    else
                    {
                        RenameSheet(sheet_StringerC, Stringer.PartNo.Dynamic, platform_DesktopDrawingDoc);
                        DeleteSheet(sheet_StringerL, platform_DesktopDrawingDoc);

                        RenameSheet(sheet_EndC, EndMember.PartNo.Dynamic, platform_DesktopDrawingDoc);
                        DeleteSheet(sheet_EndL, platform_DesktopDrawingDoc);
                    }
                    RenameSheet("1338_Grating",     Grating.Primary.PartNo.Dynamic, platform_DesktopDrawingDoc);
                    RenameSheet("1338F_Grating",    Grating.Filler.PartNo.Dynamic,  platform_DesktopDrawingDoc);
                    RenameSheet("1339_Strut",       Strut.PartNo.Dynamic,           platform_DesktopDrawingDoc);

                    // Migrate sheets
                    MigrateSheetsToBankDrawing(platform_DesktopDrawingDoc);

                    // Open bank and open Insert Component property manager page
                    AssemblyDoc bankAssembly = OpenAssembly(desktopBankFile);


                    //-------------------------------------------------------


                    if (platformOnly)
                    {
                        // Initiate InsertComponents command
                        SW.RunCommand((int)swCommands_e.swCommands_InsertComponents, null);

                        // Release COM reference to bank and close platform weldment
                        Optimize.Release(ref bankAssembly);
                        Close(Platform.FilePath);
                    }
                    
                }
                else
                {
                    AddNew_Bank();
                }
            } while (!bankExists);
        }

        // Modify an existing platform
        public WalkwayPlatform(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> selectedComponents, double length, double width, double floorHeight, int minimumStringerSize, double supportCenterToWalkwayEnd, bool updateLocations)
        {
            // Get active doc where program is initiated
            string activeDocPath = SW.IActiveDoc2.GetPathName();

            // Dictionary mapping the static part numbers to their descriptions for debugging
            Dictionary<string, string> staticPartDescriptions = new Dictionary<string, string>
            {
                { Platform_PartNo_Static, "PLATFORM" },
                { StringerL_PartNo_Static, "L-STRINGER" },
                { StringerC_PartNo_Static, "C-STRINGER" },
                { EndAngle_PartNo_Static, "END-ANGLE" },
                { EndChannel_PartNo_Static, "END-CHANNEL" },
                { GratingPrimary_PartNo_Static, "GRATING-PRIMARY" },
                { GratingFiller_PartNo_Static, "GRATING-FILLER" },
                { Strut_PartNo_Static, "STRUT" }
            };

            // Itemized dictionary of platform components
            var platformComponents = FilterComponents(selectedComponents, staticPartDescriptions);
            if (platformComponents.Count == 0) { DevTools.EnablePartUI(); return; }

            // Update locations option
            if (updateLocations) { AddParentToSelection(platformComponents); }

            // Assign existing dynamic part numbers to class
            Platform.PartNo.Dynamic = GetDynamicPartNo(platformComponents, Platform_PartNo_Static) ?? null;
            Stringer.PartNo.Dynamic = GetDynamicPartNo(platformComponents, StringerL_PartNo_Static, StringerC_PartNo_Static) ?? null;
            EndMember.PartNo.Dynamic = GetDynamicPartNo(platformComponents, EndAngle_PartNo_Static, EndChannel_PartNo_Static) ?? null;
            Grating.Primary.PartNo.Dynamic = GetDynamicPartNo(platformComponents, GratingPrimary_PartNo_Static) ?? null;
            Grating.Filler.PartNo.Dynamic = GetDynamicPartNo(platformComponents, GratingFiller_PartNo_Static) ?? null;
            Strut.PartNo.Dynamic = GetDynamicPartNo(platformComponents, Strut_PartNo_Static) ?? null;

            // Read parameters
            GetParameters_SelectedComponents(platformComponents, out double read_Length, out double read_Width, out double read_FloorHeight, out int read_MinimumStringerSize);

            // Assign properties
            char bank = GetBankFrom(platformComponents); // Assign bank if ONLY one bank is found. Otherwise, exit the program.
            PassUserInputs(length, width, floorHeight, minimumStringerSize, supportCenterToWalkwayEnd);
            SetStructuralMembers();
            SetStaticPartNumbers();
            SetStringerProperties();
            SetEndMemberProperties();
            SetGratingProperties();
            SetStrutProperties();

            // Swap structural shapes
            if (read_MinimumStringerSize >= 6 && Stringer.Size < 6) // delete C replace with L
            {
                if (platformComponents.Values.Any(value => value.StaticPartNo == StringerC_PartNo_Static))
                {
                    ProcessStringerShape(GetTemplatePath_SLDPRT(StringerL_PartNo_Static));
                }

                if (platformComponents.Values.Any(value => value.StaticPartNo == EndChannel_PartNo_Static))
                {
                    ProcessEndMemberShape(GetTemplatePath_SLDPRT(EndAngle_PartNo_Static));
                }
            }
            else if (read_MinimumStringerSize < 6 && Stringer.Size >= 6) // delete L, replace with C
            {
                if (platformComponents.Values.Any(value => value.StaticPartNo == StringerL_PartNo_Static))
                {
                    ProcessStringerShape(GetTemplatePath_SLDPRT(StringerC_PartNo_Static));
                }

                if (platformComponents.Values.Any(value => value.StaticPartNo == EndAngle_PartNo_Static))
                {
                    ProcessEndMemberShape(GetTemplatePath_SLDPRT(EndChannel_PartNo_Static));
                }
            }

            // Only modify parts in the found in the dictionary
            var conditionsAndActions = new
                List<(Func<KeyValuePair<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>, bool> condition,
                      Action<Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)>> action)>
            {
                (entry => entry.Value.StaticPartNo == StringerL_PartNo_Static || entry.Value.StaticPartNo == StringerC_PartNo_Static, ModifyStringer),
                (entry => entry.Value.StaticPartNo == EndAngle_PartNo_Static || entry.Value.StaticPartNo == EndChannel_PartNo_Static, ModifyEndMember),
                (entry => entry.Value.StaticPartNo == GratingPrimary_PartNo_Static, ModifyPrimaryGrating),
                (entry => entry.Value.StaticPartNo == GratingFiller_PartNo_Static, ModifyFillerGrating),
                (entry => entry.Value.StaticPartNo == Strut_PartNo_Static, ModifyStrut)
            };
            foreach (var (condition, action) in conditionsAndActions)
            {
                if (platformComponents.Any(condition))
                {
                    action(platformComponents);
                }
            }

            // Only move selected parts in the assembly
            if (platformComponents.Any(entry => entry.Value.StaticPartNo == Platform_PartNo_Static))
            {
                AssemblePlatform();

                if (activeDocPath != Platform.FilePath)
                {
                    Debug.WriteLine($"Original ActiveDoc: {activeDocPath}");
                    Debug.WriteLine($"Platform FilePath: {Platform.FilePath}");

                    Close(Platform.FilePath);
                }
            }
            else
            {
                Open(activeDocPath);
            }
            Debug.WriteLine("");
        }


        // Extensions
        public PlatformExtensions Platform { get; set; } = new PlatformExtensions();
        public class PlatformExtensions
        {
            public string Title { get; set; } = "PLATFORM_WELDMENT_WW";
            public string Description => Title;
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDASM";
            public double Width { get; set; }
            public double SupportCenterToWalkwayEnd { get; set; }
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }



        public       StringerExtensions Stringer { get; set; } = new StringerExtensions();
        public class StringerExtensions
        {
            public double TopLegGage1 => 2.25;
            public double TopLegGage2 => 2.5;
            public string SizeDesc { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public char Shape { get; set; }
            public double Depth { get; set; }
            public double WebTHK { get; set; }
            public double WebGage { get; set; }
            public int Size { get; set; }
            public double FlangeWidth { get; set; }
            public double AverageFlangeTHK { get; set; }
            public double FlangeGage { get; set; }
            public double K { get; set; }
            public double Length { get; set; }
            public double BottomLegGage => 2.5;
            public double LegTHK => 0.375;
            public double Leg1 => 6;
            public double Leg2 => 4;
            public string Title { get; set; }
            public string Description { get; set; }
            public string JDEnumber { get; set; } 
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }

        private EndShapeExtensions EndMember { get; set; } = new EndShapeExtensions();
        private class EndShapeExtensions
        {
            public char Shape { get; set; }
            public string Size { get; set; }
            public double Length { get; set; }
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
            public double Leg1 => 4;
            public double Gage => 2.5;
            public string Title { get; set; } 
            public string Description { get; set; } 
            public string JDEnumber { get; set; } 
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public double CopeWidth { get; set; }
            public double CopeDepth { get; set; }
            public double Depth { get; set; }
            public double WebTHK { get; set; }
            public double WebGage { get; set; }
            public double FlangeWidth { get; set; }
            public double AverageFlangeTHK { get; set; }
            public double FlangeGage { get; set; }
            public double K { get; set; }
        }


        public GratingExtensions Grating { get; set; } = new GratingExtensions();
        public class GratingExtensions
        {
            public string Description  { get; set; } = "GRATING_1-1/4x3/16_GALV_SERR";
            public string JDEnumber    { get; set; } = "14441";
            public double Span         { get; set; }
            public double Gap          { get; set; }
            public double Height { get; set; }
            public List<double> StandardWidths { get; set; } = new List<double>
            {
                35.8125, 34.625, 33.4375, 32.25, 31.0625, 29.875, 28.6875,
                27.5, 26.3125, 25.125, 23.9375, 22.75, 21.5625, 20.375, 19.1875,
                18, 16.8125, 15.625, 14.4375, 13.25, 12.0625, 10.875,
                9.6875, 8.5, 7.3125, 6.125, 4.9375
            };
            public GratingSubExtensions Primary { get; set; } = new GratingSubExtensions();
            public GratingSubExtensions Filler { get; set; } = new GratingSubExtensions();
            public GratingSubExtensions Modified { get; set; } = new GratingSubExtensions();
        }
        public class GratingSubExtensions
        {
            public double Width        { get; set; }
            public int    Count        { get; set; }
            public string Title        { get; set; }
            public string FilePath     => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }

        public StrutExtensions Strut { get; set; } = new StrutExtensions();
        public class StrutExtensions
        {
            public double TopLeg       { get; set; } = 4;
            public string Title        { get; set; } = "STRUT_WW";
            public string Description  { get; set; } = "ANGLE_4\"x3\"x3/8\"_A572-50";
            public string JDEnumber    { get; set; } = "60129";
            public double Length       { get; set; } 
            public int Count { get; set; }
            public double Spacing { get; set; }
            public string FilePath => $@"{DesktopFolderPath}\{Project}-28{Bank}-{PartNo.Dynamic}.SLDPRT";
            public PartNoSubExtensions PartNo { get; set; } = new PartNoSubExtensions();
        }
        public class PartNoSubExtensions
        {
            public string Static { get; set; }
            public string Dynamic { get; set; }
        }


        // Properties
        private void PassUserInputs(double length, double width, double floorHeight, int minimumStringerSize, double supportCenterToWalkwayEnd)
        {
            // Pass user inputs
            Stringer.Length = length;
            Debug.WriteLine($"WRITE length: {length}");

            Grating.Height = floorHeight;
            Debug.WriteLine($"WRITE floor height: {floorHeight}");

            if (Stringer.Length < 132 && minimumStringerSize < 6)
            {
                Stringer.Size = 0;
            }
            else if (Stringer.Length < 168 && minimumStringerSize == 6)
            {
                Stringer.Size = 6;
            }
            else if (Stringer.Length < 198 && minimumStringerSize <= 8)
            {
                Stringer.Size = 8;
            }
            else
            {
                Stringer.Size = 10;
            }
            Debug.WriteLine($"WRITE minimum stringer size: {Stringer.Size}");

            Platform.Width = width;
            if (Stringer.Length < 132 && Stringer.Size < 6)
            { EndMember.Length = Platform.Width; }
            else
            { Grating.Span = Platform.Width; }
            Debug.WriteLine($"WRITE width: {width}");

            Platform.SupportCenterToWalkwayEnd = supportCenterToWalkwayEnd;
        }
        private void SetStructuralMembers()
        {
            if (Stringer.Length < 132 && Stringer.Size < 6)
            {
                Stringer.SizeDesc = "L6x4x3/8";
                EndMember.Size = "L4x3x3/8";
            }
            else if (Stringer.Length < 168 && Stringer.Size == 6)
            {
                Stringer.SizeDesc = "C6x8.2";
            }
            else if (Stringer.Length < 198 && Stringer.Size <= 8)
            {
                Stringer.SizeDesc = "C8x11.5";
            }
            else
            {
                Stringer.SizeDesc = "C10x15.3";
            }

            Stringer.Shape = Stringer.SizeDesc[0];
            EndMember.Shape = Stringer.Shape;
        }
        private void SetStaticPartNumbers()
        {
            Platform.PartNo.Static = "1336";
            Stringer.PartNo.Static = (Stringer.Shape == 'L') ? "1336L" : "1336C";
            EndMember.PartNo.Static = (EndMember.Shape == 'L') ? "1337L" : "1337C";
            Grating.Primary.PartNo.Static = "1338";
            Grating.Filler.PartNo.Static = "1338F";
            Strut.PartNo.Static = "1339";
        }
        private static string GetTemplatePath_SLDPRT(string staticPartNo)
        {
            return $@"{TemplateFolderPath}\JOBNO-{staticPartNo}.SLDPRT";
        }
        private static void GetParameters_SelectedComponents(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents, out double read_length, out double read_width, out double read_floorHeight, out int read_minimumStringerSize)
        {
            Debug.WriteLine("");

            // Set length
            read_length = GetLengthFrom(platformComponents);

            // Set floor height
            read_floorHeight = GetFloorHeightFrom(platformComponents);

            // Set minimum stringer size
            read_minimumStringerSize = GetMinStringerSizeFrom(platformComponents);

            // Set width
            read_width = GetWidthFrom(platformComponents);
        }
        private static double GetWidthFrom(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            double width = 0;
            bool dimensionFound = false;

            var potentialWidthComponents = platformComponents
                .Where(entry =>
                    entry.Value.StaticPartNo == EndAngle_PartNo_Static ||
                    entry.Value.StaticPartNo == EndChannel_PartNo_Static ||
                    entry.Value.StaticPartNo == GratingPrimary_PartNo_Static ||
                    entry.Value.StaticPartNo == GratingFiller_PartNo_Static ||
                    entry.Value.StaticPartNo == Strut_PartNo_Static)
                .Select(entry => entry.Key)
                .ToList();

            foreach (var component in potentialWidthComponents)
            {
                ModelDoc2 modelDoc2 = component.GetModelDoc2();

                // Attempt to get the relevant dimension using a lambda function
                Dimension dimension = new List<Func<Dimension>>
                {
                    () => modelDoc2.Parameter("Length@Body"),
                    () => modelDoc2.Parameter("Span@BearingBar")
                }
                .Select(func => func())
                .FirstOrDefault(dim => dim != null);

                if (dimension != null)
                {
                    double value = dimension.GetValue3(1, null)[0];
                    width = Math.Max(width, value);
                    dimensionFound = true;
                }
            }

            if (dimensionFound)
            {
                Debug.WriteLine($"READ width: {width}");
            }
            else
            {
                Debug.WriteLine("Could not determine walkway width");
            }

            return width;
        }
        private static double GetFloorHeightFrom(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            double floorHeight;

            bool containsGrating = platformComponents.Any(entry =>
                entry.Value.StaticPartNo == GratingPrimary_PartNo_Static ||
                entry.Value.StaticPartNo == GratingFiller_PartNo_Static);

            if (containsGrating)
            {
                var grating = platformComponents.First(entry =>
                    entry.Value.StaticPartNo == GratingPrimary_PartNo_Static ||
                    entry.Value.StaticPartNo == GratingFiller_PartNo_Static);

                ModelDoc2 modelDoc2 = grating.Key.GetModelDoc2();
                Dimension dimension = modelDoc2.Parameter("Height@sk:BearingBar");
                floorHeight = dimension.GetValue3(1, null)[0];

                Debug.WriteLine($"READ floor height: {floorHeight}");
            }
            else
            {
                floorHeight = 0;
                Debug.WriteLine("Could not determine walkway floor height");
            }

            return floorHeight;
        }
        private static int GetMinStringerSizeFrom(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            int minimumStringerSize = 0;

            bool contains_C_Stringer = platformComponents.Any(entry => entry.Value.StaticPartNo == StringerC_PartNo_Static);
            bool contains_EndChannel = platformComponents.Any(entry => entry.Value.StaticPartNo == EndChannel_PartNo_Static);

            if (contains_C_Stringer)
            {
                var stringerC = platformComponents.First(entry => entry.Value.StaticPartNo == StringerC_PartNo_Static);

                ModelDoc2 modelDoc2 = stringerC.Key.GetModelDoc2();
                Dimension dimension = modelDoc2.Parameter("Depth@sk:Body");
                minimumStringerSize = (int)dimension.GetValue3(1, null)[0];
            }
            else if (contains_EndChannel)
            {
                var endChannel = platformComponents.First(entry => entry.Value.StaticPartNo == EndChannel_PartNo_Static);

                ModelDoc2 modelDoc2 = endChannel.Key.GetModelDoc2();
                Dimension dimension = modelDoc2.Parameter("Depth@sk:Body");
                minimumStringerSize = (int)dimension.GetValue3(1, null)[0];
            }
            else
            {
                minimumStringerSize = 0;
            }
            Debug.WriteLine($"READ minimum stringer size: {minimumStringerSize}");

            return minimumStringerSize;
        }
        private static double GetLengthFrom(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            double length;

            bool containsStringer = platformComponents.Any(entry =>
                entry.Value.StaticPartNo == StringerL_PartNo_Static ||
                entry.Value.StaticPartNo == StringerC_PartNo_Static);

            if (containsStringer)
            {
                var stringer = platformComponents.First(entry =>
                    entry.Value.StaticPartNo == StringerL_PartNo_Static ||
                    entry.Value.StaticPartNo == StringerC_PartNo_Static);

                ModelDoc2 modelDoc2 = stringer.Key.GetModelDoc2();
                Dimension dimension = modelDoc2.Parameter("Length@Body");
                length = dimension.GetValue3(1, null)[0];

                Debug.WriteLine($"READ length: {length}");
            }
            else
            {
                length = 0;
                Debug.WriteLine("Could not determine walkway length");
            }

            return length;
        }
        private void ProcessStringerShape(string stringerTemplatePath)
        {
            List<string> openDocPaths = SaveAndCloseAllDocuments(out string activeDocPath);

            File.Delete(Stringer.FilePath);

            CopyAsReadWrite(stringerTemplatePath, Stringer.FilePath);
            Debug.WriteLine($"Created: {System.IO.Path.GetFileNameWithoutExtension(Stringer.FilePath)}" + "\n");

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
        private void ProcessEndMemberShape(string endMemberTemplatePath)
        {
            List<string> openDocPaths = SaveAndCloseAllDocuments(out string activeDocPath);

            File.Delete(EndMember.FilePath);

            CopyAsReadWrite(endMemberTemplatePath, EndMember.FilePath);
            Debug.WriteLine($"Created: {System.IO.Path.GetFileNameWithoutExtension(Stringer.FilePath)}" + "\n");

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

        public void SetStringerProperties()
        {
            Stringer.Title = $"STRINGER-{Stringer.Shape}_WW";

            if (Stringer.SizeDesc == "L6x4x3/8")
            {
                Stringer.K = 0.875;
                Stringer.JDEnumber = "73799";
                Stringer.Description = "ANGLE_6\"x4\"x3/8\"_A572-50";
            }
            else if (Stringer.SizeDesc == "C6x8.2")
            {
                Stringer.Depth = 6;
                Stringer.WebTHK = 0.1875;
                Stringer.FlangeWidth = 1.875;
                Stringer.AverageFlangeTHK = 0.3125;
                Stringer.K = 0.8125;
                Stringer.FlangeGage = 1.125;
                Stringer.WebGage = 2.25;
                Stringer.JDEnumber = "54948";
                Stringer.Description = "CHANNEL_6x8.2_A572_50";
            }
            else if (Stringer.SizeDesc == "C8x11.5")
            {
                Stringer.Depth = 8;
                Stringer.WebTHK = 0.25;
                Stringer.FlangeWidth = 2.25;
                Stringer.AverageFlangeTHK = 0.375;
                Stringer.K = 0.9375;
                Stringer.FlangeGage = 1.375;
                Stringer.WebGage = 2.5;
                Stringer.JDEnumber = "54420";
                Stringer.Description = "CHANNEL_8x11.5_A572-50";
            }
            else // (Stringer.Size == "C10x15.3")
            {
                Stringer.Depth = 10;
                Stringer.WebTHK = 0.25;
                Stringer.FlangeWidth = 2.625;
                Stringer.AverageFlangeTHK = 0.4375;
                Stringer.K = 1;
                Stringer.FlangeGage = 1.5;
                Stringer.WebGage = 2.5;
                Stringer.JDEnumber = "60635";
                Stringer.Description = "CHANNEL_10x15.3_A572-50";
            }
        }
        private void SetEndMemberProperties()
        {
            if (EndMember.Shape == 'L')
            {
                EndMember.Title = "END-ANGLE_WW";
                EndMember.Description = "ANGLE_4\"x3\"x3/8\"_A572-50";
                EndMember.JDEnumber = "60129";
            }
            else // (EndMember.Shape == 'C')
            {
                EndMember.Length = Platform.Width - (Stringer.WebTHK * 2) - (WeldGap * 2);
                EndMember.CopeWidth = Stringer.FlangeWidth - Stringer.WebTHK;
                EndMember.CopeDepth = Stringer.K;
                EndMember.Title = "END-CHANNEL_WW";
                EndMember.Description = Stringer.Description;
                EndMember.JDEnumber = Stringer.JDEnumber;
                EndMember.Depth = Stringer.Depth;
                EndMember.WebTHK = Stringer.WebTHK;
                EndMember.WebGage = Stringer.WebGage;
                EndMember.FlangeWidth = Stringer.FlangeWidth;
                EndMember.AverageFlangeTHK = Stringer.AverageFlangeTHK;
                EndMember.FlangeGage = Stringer.FlangeGage;
                EndMember.K = Stringer.K;
                EndMember.Length = Platform.Width - Stringer.WebTHK * 2 - WeldGap * 2;
            }
        }
        private void SetGratingProperties()
        {
            Grating.Primary.Title = "GRATING_WW";
            Grating.Filler.Title = "GRATING_FILLER_WW";
            Grating.Span = Stringer.Shape == 'L' ? Platform.Width - (Stringer.K * 2) - (Clearance * 2) : Grating.Span;



            bool conditionMet = false;

            // Iterate over standard grating widths to find a suitable configuration.
            foreach (double standardWidth in Grating.StandardWidths)
            {
                Grating.Primary.Count = (int)(Stringer.Length / standardWidth);
                double remainder = Stringer.Length - (Grating.Primary.Count * standardWidth);
                int gapCount = Math.Max(0, Grating.Primary.Count - 1);
                double remainderUpperBound = remainder - 0.125 * gapCount;
                double remainderLowerBound = remainder - 1 * gapCount;

                // Determine an appropriate filler width based on the remainder after primary gratings.
                Grating.Filler.Width = Grating.StandardWidths
                    .Where(p => p <= remainderUpperBound && p >= remainderLowerBound).LastOrDefault();

                double gap = (Stringer.Length - Grating.Primary.Count * standardWidth - Grating.Filler.Width) / (Grating.Primary.Count);

                // Check validity of calculated grating configuration.
                if (Grating.Filler.Width != 0 && gap >= 0.125 && gap <= 1)
                {
                    Grating.Primary.Width = standardWidth;
                    Grating.Gap = gap;
                    Grating.Filler.Count = 1;
                    conditionMet = true;
                    break;
                }
            }

            // Default configuration if no valid grating configuration is found.
            if (!conditionMet)
            {
                Debug.WriteLine("GratingFloor_Algorithm returned 0 solutions");
                Grating.Primary.Width = 35.8125;
                Grating.Primary.Count = 1;
                Grating.Gap = 0.125;
                Grating.Filler.Width = 4.9375;
            }
        }
        private void SetStrutProperties()
        {
            Strut.Count = (int)(Stringer.Length / 48) - 1;
            Strut.Spacing = (Stringer.Length / (Strut.Count + 1));
            Strut.Length = EndMember.Length;
        }



        // Modify
        private void ModifyParts()
        {
            ModifyStringer();
            ModifyEndMember();
            ModifyPrimaryGrating();
            ModifyFillerGrating();
            if (Strut.Count != 0) ModifyStrut();
        }
        private void ModifyStringer()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 stringer = Open(Stringer.FilePath);
            Debug.WriteLine("Stringer");

            // Job info
            SetProperty("Project", Project, stringer);
            SetProperty("Bank", Bank, stringer);
            SetProperty("Customer", Customer, stringer);
            SetProperty("Client", Client, stringer);
            SetProperty("Location", Location, stringer);
            SetProperty("PO", PurchaseOrder, stringer);
            SetProperty("ItemNo", ItemNumber, stringer);

            // JDE info
            SetProperty("RMdesc", Stringer.Description, stringer);
            SetProperty("PartNo", "28" + Bank + "-" + Stringer.PartNo.Dynamic, stringer);
            SetProperty("RM", Stringer.JDEnumber, stringer);
            SetProperty("Title", Stringer.Title, stringer);

            // Estimate mid post location(s) based on SmithCo end walkway standards
            Estimate_PostHoleLocations(out int estimatedCount, out double estimatedSpacing);

            //             dimName          treeName            newValue            modelDoc2
            EditDimension("Length", "Body", Stringer.Length, stringer);
            EditDimension("Diameter", "sk:RailHole", HoleDiameter, stringer);
            EditDimension($"Count", "sk:RailHole", estimatedCount + 1, stringer);
            EditDimension($"Spacing", "sk:RailHole", estimatedSpacing, stringer);
            EditDimension("Offset", "sk:SupportHoles", Platform.SupportCenterToWalkwayEnd, stringer);

            if (Stringer.Shape == 'C')
            {
                //             dimName          treeName            newValue                    modelDoc2
                EditDimension("FlangeWidth", "sk:Body", Stringer.FlangeWidth, stringer);
                EditDimension("WebTHK", "sk:Body", Stringer.WebTHK, stringer);
                EditDimension("Depth", "sk:Body", Stringer.Depth, stringer);
                EditDimension("FlangeGage", "sk:Body", Stringer.FlangeGage, stringer);
                EditDimension("AvgFlgTHK", "sk:Body", Stringer.AverageFlangeTHK, stringer);
                EditDimension("K", "sk:Body", Stringer.K, stringer);
                EditDimension("WebGage", "sk:RailHole", Stringer.WebGage, stringer);
                EditDimension("FlangeGage", "sk:SupportHoles", Stringer.FlangeGage, stringer);
            }

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref stringer);
        }
        private void ModifyStringer(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            ModifyStringer();
            if (platformComponents.Any(entry => entry.Value.FileName == Platform.FilePath))
            {
                Close(Stringer.FilePath);
            }
        }
        private void Estimate_PostHoleLocations(out int estimatedCount, out double estimatedSpacing)
        {
            double estimated_Post_MaxSpacing = 80;
            double estimated_Post_Gage = 1.375;
            estimatedCount = (int)(Stringer.Length / estimated_Post_MaxSpacing);
            estimatedSpacing = (Stringer.Length - estimated_Post_Gage * 2) / (estimatedCount + 1);

            // Handle SOLIDWORKS count limitation
            if (estimatedCount < 1)
            {
                estimatedCount = 2;
                estimatedSpacing = Stringer.Length;
            }
        }
        private AssemblyDoc ModifyPlatform()
        {
            // Open SOLIDWORKS file and obtain COM reference
            AssemblyDoc platformWeldment = OpenAssembly(Platform.FilePath);
            Debug.WriteLine("Platform Weldment");

            // Job info
            SetProperty("Project",  Project,         platformWeldment);
            SetProperty("Bank",     Bank,            platformWeldment);
            SetProperty("Customer", Customer,        platformWeldment);
            SetProperty("Client",   Client,          platformWeldment);
            SetProperty("Location", Location,        platformWeldment);
            SetProperty("PO",       PurchaseOrder,   platformWeldment);
            SetProperty("ItemNo",   ItemNumber,      platformWeldment);

            // JDE info
            SetProperty("RMdesc", Platform.Description,     platformWeldment);
            SetProperty("PartNo", "28" + Bank + "-" + Platform.PartNo.Dynamic,  platformWeldment);
            SetProperty("Title", Platform.Title,            platformWeldment);

            // Drawing helper sketches
            double aboveFloorHeight = -Stringer.LegTHK + Grating.Height + Stringer.Leg1 + 0.001;
            double belowFloorHeight = Stringer.Depth + (Stringer.Shape == 'L' ? EndMember.Leg1 : 0);
            int sketchStrutCount = Strut.Count + 1 < 2 ? 2 : Strut.Count + 1;

            //             dimName              treeName        newValue
            EditDimension("Width",              "FrontSketch",  Platform.Width,          platformWeldment);
            EditDimension("Floor",              "FrontSketch",  Grating.Height,          platformWeldment);
            EditDimension("Gage",               "FrontSketch",  EndMember.Gage,          platformWeldment);
            EditDimension("AboveFloorHeight",   "FrontSketch",  aboveFloorHeight,        platformWeldment);
            EditDimension("BelowFloorHeight",   "FrontSketch",  belowFloorHeight,        platformWeldment);
            EditDimension("Length",             "RightSketch",  Stringer.Length,         platformWeldment);
            EditDimension("Spacing",            "RightSketch",  Strut.Spacing,           platformWeldment);
            EditDimension("Count",              "RightSketch",  sketchStrutCount,        platformWeldment);

            return platformWeldment;
        }
        private void ModifyEndMember()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 endMember = Open(EndMember.FilePath);
            Debug.WriteLine(EndMember.Shape == 'L' ? "End Angle" : "End Channel");

            // Job info
            SetProperty("Project", Project, endMember);
            SetProperty("Bank", Bank, endMember);
            SetProperty("Customer", Customer, endMember);
            SetProperty("Client", Client, endMember);
            SetProperty("Location", Location, endMember);
            SetProperty("PO", PurchaseOrder, endMember);
            SetProperty("ItemNo", ItemNumber, endMember);

            // JDE info
            SetProperty("RMdesc", EndMember.Description, endMember);
            SetProperty("PartNo", "28" + Bank + "-" + EndMember.PartNo.Dynamic, endMember);
            SetProperty("RM", EndMember.JDEnumber, endMember);
            SetProperty("Title", EndMember.Title, endMember);

            //             dimName      treeName         newValue           modelDoc2
            EditDimension("Length", "Body", EndMember.Length, endMember);

            if (EndMember.Shape == 'C')
            {
                bool editSuccessful;
                int i = 1;
                do
                {

                    //             dimName          treeName        newValue                    modelDoc2
                    editSuccessful =
                    EditDimension("FlangeWidth", "sk:Body", EndMember.FlangeWidth, endMember);
                    EditDimension("WebTHK", "sk:Body", EndMember.WebTHK, endMember);
                    EditDimension("Depth", "sk:Body", EndMember.Depth, endMember);
                    EditDimension("FlangeGage", "sk:Body", EndMember.FlangeGage, endMember);
                    EditDimension("AvgFlgTHK", "sk:Body", EndMember.AverageFlangeTHK, endMember);
                    EditDimension("K", "sk:Body", EndMember.K, endMember);
                    EditDimension("Depth", "sk:Cope", EndMember.CopeDepth, endMember);
                    EditDimension("Width", "sk:Cope", EndMember.CopeWidth, endMember);

                    if (editSuccessful == false)
                    {
                        // Save open stringer file
                        IModelDoc2 stringer = (IModelDoc2)SW.GetOpenDocumentByName(Stringer.FilePath);
                        stringer.Save3((int)swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0);

                        // Delete L for C
                        ProcessEndMemberShape(GetTemplatePath_SLDPRT(EndChannel_PartNo_Static));
                        endMember = Open(EndMember.FilePath);

                        EditDimension("Length", "Body", EndMember.Length, endMember);
                    }

                    i++;

                } while (editSuccessful == false && i < 3);

            }

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref endMember);
        }
        private void ModifyEndMember(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            ModifyEndMember();
            if (platformComponents.Any(entry => entry.Value.FileName == Platform.FilePath))
            {
                Close(EndMember.FilePath);
            }
        }
        private void ModifyPrimaryGrating()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 primaryGrating = Open(Grating.Primary.FilePath);
            Debug.WriteLine("Primary Grating");

            // Job info
            SetProperty("Project", Project, primaryGrating);
            SetProperty("Bank", Bank, primaryGrating);
            SetProperty("Customer", Customer, primaryGrating);
            SetProperty("Client", Client, primaryGrating);
            SetProperty("Location", Location, primaryGrating);
            SetProperty("PO", PurchaseOrder, primaryGrating);
            SetProperty("ItemNo", ItemNumber, primaryGrating);

            // JDE info
            SetProperty("RMdesc", Grating.Description, primaryGrating);
            SetProperty("PartNo", "28" + Bank + "-" + Grating.Primary.PartNo.Dynamic, primaryGrating);
            SetProperty("RM", Grating.JDEnumber, primaryGrating);
            SetProperty("Title", Grating.Primary.Title, primaryGrating);

            //             dimName   treeName        newValue
            EditDimension("Span", "BearingBar", Grating.Span, primaryGrating);
            EditDimension("Span", "TopSketch", Grating.Span, primaryGrating);
            EditDimension("Height", "sk:BearingBar", Grating.Height, primaryGrating);
            EditDimension("Width", "sk:BearingBar", Grating.Primary.Width, primaryGrating);
            EditDimension("Width", "TopSketch", Grating.Primary.Width, primaryGrating);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref primaryGrating);
        }
        private void ModifyPrimaryGrating(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            ModifyPrimaryGrating();
            if (platformComponents.Any(entry => entry.Value.FileName == Platform.FilePath))
            {
                Close(Grating.Primary.FilePath);
            }
        }
        private void ModifyFillerGrating()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 fillerGrating = Open(Grating.Filler.FilePath);
            Debug.WriteLine("Filler Grating");

            // Job info
            SetProperty("Project", Project, fillerGrating);
            SetProperty("Bank", Bank, fillerGrating);
            SetProperty("Customer", Customer, fillerGrating);
            SetProperty("Client", Client, fillerGrating);
            SetProperty("Location", Location, fillerGrating);
            SetProperty("PO", PurchaseOrder, fillerGrating);
            SetProperty("ItemNo", ItemNumber, fillerGrating);

            // JDE info
            SetProperty("RMdesc", Grating.Description, fillerGrating);
            SetProperty("PartNo", "28" + Bank + "-" + Grating.Filler.PartNo.Dynamic, fillerGrating);
            SetProperty("RM", Grating.JDEnumber, fillerGrating);
            SetProperty("Title", Grating.Filler.Title, fillerGrating);

            //             dimName   treeName        newValue
            EditDimension("Span", "BearingBar", Grating.Span, fillerGrating);
            EditDimension("Span", "TopSketch", Grating.Span, fillerGrating);
            EditDimension("Height", "sk:BearingBar", Grating.Height, fillerGrating);
            EditDimension("Width", "sk:BearingBar", Grating.Filler.Width, fillerGrating);
            EditDimension("Width", "TopSketch", Grating.Filler.Width, fillerGrating);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref fillerGrating);
        }
        private void ModifyFillerGrating(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            ModifyFillerGrating();
            if (platformComponents.Any(entry => entry.Value.FileName == Platform.FilePath))
            {
                Close(Grating.Filler.FilePath);
            }
        }
        private void ModifyModifiedGrating() // not implemented
        {
            //// Open SOLIDWORKS file and obtain COM reference
            //ModelDoc2 modifiedGrating = Open(Grating.Filler.FilePath);
            //Debug.WriteLine("Modified Grating");

            //// Job info
            //SetProperty("Project",  Project,              modifiedGrating);
            //SetProperty("Bank",     Bank,                 modifiedGrating);
            //SetProperty("Customer", Customer,             modifiedGrating);
            //SetProperty("Client",   Client,               modifiedGrating);
            //SetProperty("Location", Location,             modifiedGrating);
            //SetProperty("PO",       PurchaseOrder,        modifiedGrating);
            //SetProperty("ItemNo",   ItemNumber,           modifiedGrating);

            //// JDE info
            //SetProperty("RMdesc",   Grating.Description,     modifiedGrating);
            //SetProperty("PartNo",   Grating.Modified.PartNo, modifiedGrating);
            //SetProperty("RM",       Grating.JDEnumber,       modifiedGrating);
            //SetProperty("Title",    Grating.Modified.Title,  modifiedGrating);

            ////             dimName   treeName        newValue
            //EditDimension("Span",   "BearingBar",    Grating.Span,              modifiedGrating);
            //EditDimension("Height", "sk:BearingBar", floorHeight,            modifiedGrating);
            //EditDimension("Width",  "sk:BearingBar", Grating.Modified.Width,    modifiedGrating);

            //// Release COM object, but leave file open in SOLIDWORKS for assembly placement
            //Release(ref modifiedGrating);
        }
        private void ModifyStrut()
        {
            // Open SOLIDWORKS file and obtain COM reference
            ModelDoc2 strut = Open(Strut.FilePath);
            Debug.WriteLine("Strut");

            // Job info
            SetProperty("Project", Project, strut);
            SetProperty("Bank", Bank, strut);
            SetProperty("Customer", Customer, strut);
            SetProperty("Client", Client, strut);
            SetProperty("Location", Location, strut);
            SetProperty("PO", PurchaseOrder, strut);
            SetProperty("ItemNo", ItemNumber, strut);

            // JDE info
            SetProperty("RMdesc", Strut.Description, strut);
            SetProperty("PartNo", "28" + Bank + "-" + Strut.PartNo.Dynamic, strut);
            SetProperty("RM", Strut.JDEnumber, strut);
            SetProperty("Title", Strut.Title, strut);

            //             dimName   treeName   newValue
            EditDimension("Length", "Body", Strut.Length, strut);

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref strut);
        }
        private void ModifyStrut(Dictionary<Component2, (string FileName, string StaticPartNo, string DynamicPartNo, char? Bank)> platformComponents)
        {
            ModifyStrut();
            if (platformComponents.Any(entry => entry.Value.FileName == Platform.FilePath))
            {
                Close(Strut.FilePath);
            }
        }



        //Assemble
        private void AssemblePlatform()
        {
            AssemblyDoc platformWeldment = ModifyPlatform();

            // Record components with user defined locations
            List<Component2> unfixedComponents = ListUnfixedComponents(platformWeldment);

            // Check assembly for existing component instances
            List<Component2> stringerList = Stringer.PartNo.Dynamic != null
                ? FindMatchingComponents(Stringer.FilePath, platformWeldment)
                : new List<Component2>();

            List<Component2> endMemberList = EndMember.PartNo.Dynamic != null
                ? FindMatchingComponents(EndMember.FilePath, platformWeldment)
                : new List<Component2>();

            List<Component2> primaryGratingList = Grating.Primary.PartNo.Dynamic != null
                ? FindMatchingComponents(Grating.Primary.FilePath, platformWeldment)
                : new List<Component2>();

            List<Component2> fillerGratingList = Grating.Filler.PartNo.Dynamic != null
                ? FindMatchingComponents(Grating.Filler.FilePath, platformWeldment)
                : new List<Component2>();

            List<Component2> strutList = Strut.PartNo.Dynamic != null
                ? FindMatchingComponents(Strut.FilePath, platformWeldment)
                : new List<Component2>();


            // Place components and close SOLIDWORKS file
            PlaceStringers(stringerList, platformWeldment);
            PlaceEndMembers(endMemberList, platformWeldment);
            PlacePrimaryGrating(primaryGratingList, platformWeldment);
            PlaceFillerGrating(fillerGratingList, platformWeldment);
            if (Strut.Count > 0) { PlaceStruts(strutList, platformWeldment); }

            // Fix all components
            FixComponentLocations(platformWeldment);

            // Float parts marked with a manually modifed location
            UnfixComponentLocations(platformWeldment, unfixedComponents);

            SaveEverything();

            // Release COM object, but leave file open in SOLIDWORKS for assembly placement
            Optimize.Release(ref platformWeldment);
        }
        private void PlaceStringers(List<Component2> stringerList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            // Instances in stringerList
            Component2 rightStringer = GetInstance(stringerList, 1); Debug.WriteLine(" (right stringer)");
            Component2 leftStringer = GetInstance(stringerList, 2); Debug.WriteLine(" (left stringer)");


            //---------------


            
            if (rightStringer == null && Stringer.PartNo.Dynamic != null)
            {
                rightStringer = InsertComponent(Stringer.FilePath, assemblyDoc); Debug.WriteLine(" (right stringer)");
                markedToClose = true;
            }


            if (rightStringer != null)
            {
                // Set location
                X_Translation(Platform.Width / 2);
                Y_Translation(Stringer.Shape == 'L' ? -(Grating.Height + 0.375) : -Grating.Height);
                ApplyPositionInformation(rightStringer);

                // Release COM object
                Optimize.Release(ref rightStringer);
            }
            


            //---------------


            // If the model does not contain a left stringer, place one
            if (leftStringer == null && Stringer.PartNo.Dynamic != null)
            {
                leftStringer = InsertComponent(Stringer.FilePath, assemblyDoc); Debug.WriteLine(" (left stringer)");
                markedToClose = true;
            }

            if (leftStringer != null)
            {
                // Set location
                Y_Axis_180_Degree_Rotate();
                X_Translation(-Platform.Width / 2);
                Y_Translation(Stringer.Shape == 'L' ? -(Grating.Height + 0.375) : -Grating.Height);
                ApplyPositionInformation(leftStringer);
            }


            // Release COM object & unload file from memory
            Optimize.Release(ref leftStringer);


            if (markedToClose)
            {
                Close(Stringer.FilePath);
            }
            
        }
        private void PlaceEndMembers(List<Component2> endMemberList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            // Instances in endMemberList
            Component2 frontEndMember = GetInstance(endMemberList, 1); Debug.WriteLine(" (front end member)");
            Component2 rearEndMember = GetInstance(endMemberList, 2); Debug.WriteLine(" (rear end member)");


            //---------------


            // If the model does not contain a front, end member, place one
            if (frontEndMember == null && EndMember.PartNo.Dynamic != null)
            {
                frontEndMember = InsertComponent(EndMember.FilePath, assemblyDoc); Debug.WriteLine(" (front end member)");
                markedToClose = true;
            }

            if (frontEndMember != null)
            {
                // Set location
                Y_Translation(-Grating.Height);
                Z_Translation(-Stringer.Length / 2);
                ApplyPositionInformation(frontEndMember);

                // Release COM object
                Optimize.Release(ref frontEndMember);
            }
            


            //---------------


            // If the model does not contain a rear, end member, place one
            if (rearEndMember == null && EndMember.PartNo.Dynamic != null)
            {
                rearEndMember = InsertComponent(EndMember.FilePath, assemblyDoc); Debug.WriteLine(" (rear end member)");
                markedToClose = true;
            }

            if (rearEndMember != null)
            {
                // Set location
                Y_Axis_180_Degree_Rotate();
                Y_Translation(-Grating.Height);
                Z_Translation(Stringer.Length / 2);
                ApplyPositionInformation(rearEndMember);

                // Release COM object & unload file from memory
                Optimize.Release(ref rearEndMember);
            }
            
            if (markedToClose)
            {
                Close(EndMember.FilePath);
            }
        }
        private void PlacePrimaryGrating(List<Component2> gratingList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            List<Component2> processedComponents = new List<Component2>();

            for (int i = 0; i < Grating.Primary.Count; i++)
            {

                Component2 primaryGrating = GetInstance(gratingList, i + 1);

                if (primaryGrating == null && Grating.Primary.PartNo.Dynamic != null)
                {
                    primaryGrating = InsertComponent(Grating.Primary.FilePath, assemblyDoc); Debug.WriteLine($" (grating{i + 1})");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (grating{i + 1})");
                    processedComponents.Add(primaryGrating);
                }

                if (primaryGrating != null)
                {
                    Z_Translation(-Grating.Primary.Width / 2 + Stringer.Length / 2 - Grating.Gap * i - Grating.Primary.Width * i);
                    ApplyPositionInformation(primaryGrating);


                    // Release COM object
                    Optimize.Release(ref primaryGrating);
                }
                
            }

            // Remove items in gratingList that are not in processedComponents
            foreach (var component in gratingList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Platform.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                Close(Grating.Primary.FilePath);
            }
            
        }
        private void PlaceFillerGrating(List<Component2> fillerGratingList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            // Instance in fillerGratingList
            Component2 fillerGrating = GetInstance(fillerGratingList, 1); Debug.WriteLine(" (grating-filler)");

            // If the model does not contain filler grating, place it
            if (fillerGrating == null && Grating.Filler.PartNo.Dynamic != null)
            {
                fillerGrating = InsertComponent(Grating.Filler.FilePath, assemblyDoc); Debug.WriteLine(" (grating-filler)");
                markedToClose = true;
            }

            if (fillerGrating != null)
            {
                // Set location
                Z_Translation(Grating.Filler.Width / 2 - Stringer.Length / 2);
                ApplyPositionInformation(fillerGrating);

                // Release COM object & unload file from memory
                Optimize.Release(ref fillerGrating);
            }
            
            if (markedToClose)
            {
                Close(Grating.Filler.FilePath);
            }
            
        }
        private void PlaceModifiedGrating(AssemblyDoc assemblyDoc) {} // not implemented
        private void PlaceStruts(List<Component2> strutList, AssemblyDoc assemblyDoc)
        {
            bool markedToClose = false;

            List <Component2> processedComponents = new List<Component2>();

            for (int i = 0; i < Strut.Count; i++)
            {
                Component2 strut = GetInstance(strutList, i + 1); 

                if (strut == null && Strut.PartNo.Dynamic != null)
                {
                    strut = InsertComponent(Strut.FilePath, assemblyDoc); Debug.WriteLine($" (strut{i + 1})");
                    if (i == 0)
                    {
                        markedToClose = true;
                    }
                }
                else
                {
                    Debug.WriteLine($" (strut{i + 1})");
                    processedComponents.Add(strut);
                }


                if (strut != null)
                {
                    // Set location
                    switch (Stringer.Shape)
                    {
                        case 'C':
                            Y_Translation
                                (-Grating.Height - 1);
                            break;
                        case 'L':
                            Y_Translation
                                (-Grating.Height - 0.375);
                            break;
                    }
                    Z_Translation(Stringer.Length / 2 - (Stringer.Length / (Strut.Count + 1) * (i + 1)) + Strut.TopLeg / 2);
                    ApplyPositionInformation(strut);

                    // Release COM object
                    Optimize.Release(ref strut);
                }
                
            }

            // Remove items in strutList that are not in processedComponents
            foreach (var component in strutList)
            {
                if (!processedComponents.Contains(component))
                {
                    (assemblyDoc as ModelDoc2).Extension.SelectByID2($"{component.Name2}@{Project}-{Platform.PartNo.Dynamic}", "COMPONENT", 0, 0, 0, true, 0, null, 0);
                    Debug.WriteLine($"         Deleted: {component.Name2}");
                    (assemblyDoc as ModelDoc2).Extension.DeleteSelection2(0);
                }
            }

            if (markedToClose)
            {
                Close(Strut.FilePath);
            }
            
        }
    }
}
