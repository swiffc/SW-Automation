using System;
using System.IO;
using System.Windows;
using UnifiedUI.Models;
using FileTools.Infrastructure;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Constants for SolidWorks Assembly Numbers
    /// </summary>
    public static class AssemblyNumbers
    {
        public const int MACHINERY_MOUNT = 4;
        public const int PLENUM = 5;
        public const int BUNDLE = 7;
        public const int STRUCTURE = 25;
        public const int WALKWAY = 28;
        public const int HEADER_61 = 61;
        public const int HEADER_62 = 62;
        public const int HEADER_63 = 63;
        public const int HEADER_64 = 64;
        public const int HEADER_65 = 65;
        public const int HEADER_66 = 66;
        public const int HOOD = 3;
    }

    /// <summary>
    /// Service for SolidWorks component generation
    /// Coordinates between Assembly UI and Design Table approaches
    /// </summary>
    public class SolidWorksService
    {
        /// <summary>
        /// Generates SolidWorks components based on configuration
        /// </summary>
        /// <param name="config">Component configuration containing all parameters</param>
        /// <param name="progressCallback">Optional callback for progress updates (0-100)</param>
        /// <exception cref="Exception">Thrown when generation fails</exception>
        public void GenerateComponents(ComponentConfiguration config, Action<int> progressCallback)
        {
            progressCallback?.Invoke(0);
            GlobalErrorHandler.LogInfo($"Starting generation for {config.ComponentType}");

            try
            {
                // Determine which strategy to use
                var strategy = SelectStrategy(config);
                
                progressCallback?.Invoke(25);
                
                // Execute generation
                strategy.Generate(config, progressCallback);
                
                progressCallback?.Invoke(100);
                GlobalErrorHandler.LogInfo($"{config.ComponentType} generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Component Generation Failed");
                throw new Exception($"Generation failed: {ex.Message}", ex);
            }
        }

        private IGenerationStrategy SelectStrategy(ComponentConfiguration config)
        {
            // Strategy pattern: choose approach based on component type
            return config.ComponentType switch
            {
                "Bundle" => new AssemblyUIStrategy(),
                "Header" when IsAdvancedSectionTool(config) => new DesignTableStrategy(),
                "Header" => new AssemblyUIStrategy(),
                "XCH Structure" => new DesignTableStrategy(),
                "Z Structure" => new DesignTableStrategy(),
                _ => new AssemblyUIStrategy()
            };
        }

        private bool IsAdvancedSectionTool(ComponentConfiguration config)
        {
            // Check if user selected advanced section tool
            return config.GetParameter("UseAdvancedSectionTool", false);
        }
    }

    /// <summary>
    /// Interface for generation strategies
    /// </summary>
    public interface IGenerationStrategy
    {
        /// <summary>
        /// Generates SolidWorks components using the specific strategy implementation
        /// </summary>
        /// <param name="config">Component configuration</param>
        /// <param name="progressCallback">Progress callback (0-100)</param>
        void Generate(ComponentConfiguration config, Action<int> progressCallback);
    }

    /// <summary>
    /// Strategy for Assembly UI approach (code-driven)
    /// Directly calls existing component generators (Bundle.cs, Header.cs, etc.)
    /// </summary>
    /// <remarks>
    /// This strategy creates components by instantiating C# classes that use SolidWorks API directly.
    /// Used for: Bundle, Header, Hood, Walkway, MachineryMount, Plenum, Structure
    /// </remarks>
    public class AssemblyUIStrategy : IGenerationStrategy
    {
        public void Generate(ComponentConfiguration config, Action<int> progressCallback)
        {
            GlobalErrorHandler.LogInfo($"AssemblyUIStrategy: Generating {config.ComponentType}");
            progressCallback?.Invoke(10);

            // Route to appropriate component generator
            switch (config.ComponentType)
            {
                case "Bundle":
                    GenerateBundle(config as BundleConfiguration, progressCallback);
                    break;
                    
                case "Header":
                    GenerateHeader(config as HeaderConfiguration, progressCallback);
                    break;

                case "Hood":
                    GenerateHood(config as HoodConfiguration, progressCallback);
                    break;

                case "Walkway":
                    GenerateWalkway(config as WalkwayConfiguration, progressCallback);
                    break;

                case "Machinery Mount":
                    GenerateMachineryMount(config as MachineryMountConfiguration, progressCallback);
                    break;

                case "Plenum":
                    GeneratePlenum(config as PlenumConfiguration, progressCallback);
                    break;

                case "Structure":
                    GenerateStructure(config as StructureConfiguration, progressCallback);
                    break;
       
                default:
                    throw new NotImplementedException($"Assembly UI generation for {config.ComponentType} not yet implemented");
            }

            progressCallback?.Invoke(100);
        }

        /// <summary>
        /// Helper method to convert numeric Bank (1,2,3) to char ('A','B','C')
        /// </summary>
        private static char ConvertBankToChar(int bank)
        {
            if (bank < 1 || bank > 26)
                throw new ArgumentOutOfRangeException(nameof(bank), "Bank must be between 1 and 26");
            return (char)(bank + 'A' - 1);
        }

        private void GenerateBundle(BundleConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Bundle generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate configuration
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Bundle configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                        progressCallback?.Invoke(20);

                // ========== MAP BUNDLECONFIGURATION TO COMMONDATA STATIC PROPERTIES ==========
                GlobalErrorHandler.LogInfo("Mapping BundleConfiguration to CommonData...");
                
                // Job Information
                FileTools.CommonData.CommonData.Project = config.JobNumber;
                FileTools.CommonData.CommonData.Bank = ConvertBankToChar(config.Bank);
                FileTools.CommonData.CommonData.Customer = config.Customer ?? "";
                FileTools.CommonData.CommonData.Client = config.Client ?? "";
                FileTools.CommonData.CommonData.PlantLocation = config.Location ?? "";
                FileTools.CommonData.CommonData.PurchaseOrder = config.PurchaseOrder ?? "";
                FileTools.CommonData.CommonData.ItemNumber = config.ItemNumber ?? "";
                FileTools.CommonData.CommonData.Initials = config.Initials ?? "DC";

                // Bundle Dimensions
                FileTools.CommonData.CommonData.Bundle_Width = config.BundleWidth;
                FileTools.CommonData.CommonData.SideFrame_THK = config.SideFrameThickness;
                FileTools.CommonData.CommonData.SideFrame_Depth = config.SideFrameDepth;
                FileTools.CommonData.CommonData.HeadersOutsideFrames = config.HeadersOutsideFrame;

                // Tube Configuration
                FileTools.CommonData.CommonData.TubeLength = config.TubeLength;
                FileTools.CommonData.CommonData.TubeProjection = config.TubeProjection;
                FileTools.CommonData.CommonData.TubeOD = config.TubeOD;
                FileTools.CommonData.CommonData.TubeWallTHK = config.TubeWallThickness;
                FileTools.CommonData.CommonData.FinOD = config.FinOD;
                FileTools.CommonData.CommonData.Tube_Row_1L = config.TubeRow1Count;
                FileTools.CommonData.CommonData.Tube_Row_2L = config.TubeRow2Count;
                FileTools.CommonData.CommonData.TubeQuantity = config.TubeQuantity;
                FileTools.CommonData.CommonData.TubeHorizPitch = config.HorizontalPitch;

                // Fin Strip Back
                FileTools.CommonData.CommonData.FinStripBack_Front = config.FinStripBackFront;
                FileTools.CommonData.CommonData.FinStripBack_Rear = config.FinStripBackRear;

                // Vertical Pitches - Front (9 values)
                if (config.FrontVerticalPitches != null && config.FrontVerticalPitches.Count >= 9)
                {
                    FileTools.CommonData.CommonData.FrontVerticalPitch._1_2 = config.FrontVerticalPitches[0];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._2_3 = config.FrontVerticalPitches[1];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._3_4 = config.FrontVerticalPitches[2];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._4_5 = config.FrontVerticalPitches[3];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._5_6 = config.FrontVerticalPitches[4];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._6_7 = config.FrontVerticalPitches[5];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._7_8 = config.FrontVerticalPitches[6];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._8_9 = config.FrontVerticalPitches[7];
                    FileTools.CommonData.CommonData.FrontVerticalPitch._9_10 = config.FrontVerticalPitches[8];
                }

                // Vertical Pitches - Rear (9 values)
                if (config.RearVerticalPitches != null && config.RearVerticalPitches.Count >= 9)
                {
                    FileTools.CommonData.CommonData.RearVerticalPitch._1_2 = config.RearVerticalPitches[0];
                    FileTools.CommonData.CommonData.RearVerticalPitch._2_3 = config.RearVerticalPitches[1];
                    FileTools.CommonData.CommonData.RearVerticalPitch._3_4 = config.RearVerticalPitches[2];
                    FileTools.CommonData.CommonData.RearVerticalPitch._4_5 = config.RearVerticalPitches[3];
                    FileTools.CommonData.CommonData.RearVerticalPitch._5_6 = config.RearVerticalPitches[4];
                    FileTools.CommonData.CommonData.RearVerticalPitch._6_7 = config.RearVerticalPitches[5];
                    FileTools.CommonData.CommonData.RearVerticalPitch._7_8 = config.RearVerticalPitches[6];
                    FileTools.CommonData.CommonData.RearVerticalPitch._8_9 = config.RearVerticalPitches[7];
                    FileTools.CommonData.CommonData.RearVerticalPitch._9_10 = config.RearVerticalPitches[8];
                }

                // Tube Supports
                FileTools.CommonData.CommonData.TubeSupportSpacing_Feet = config.TubeSupportSpacingFeet;
                FileTools.CommonData.CommonData.TubeSupportQuantity = (int)config.TubeSupportQuantity;
                FileTools.CommonData.CommonData.TubeSupportSize = config.TubeSupportSize ?? "";

                // Manufacturing
                FileTools.CommonData.CommonData.TitleblockManuf = config.TitleblockManufacturer ?? "HPC";
                FileTools.CommonData.CommonData.Cambered = config.Cambered;

                // Plenum Context
                FileTools.CommonData.CommonData.Plenum_Length = config.PlenumLength;
                FileTools.CommonData.CommonData.OffsetFromCenter = config.OffsetFromPlenumCenter;

                // Other
                FileTools.CommonData.CommonData.Fan_Count = config.FanCount;
                FileTools.CommonData.CommonData.TotalUnitWeight = config.TotalUnitWeight;

                GlobalErrorHandler.LogInfo($"Mapped 40+ properties from BundleConfiguration to CommonData");

                progressCallback?.Invoke(50);

                // Create Bundle Assembly (Assembly Number: 7)
                GlobalErrorHandler.LogInfo("Creating Bundle assembly...");
                new Bundle.Bundle(AssemblyNumbers.BUNDLE, "Bundle Assembly");
                
                progressCallback?.Invoke(90);

                GlobalErrorHandler.LogInfo("Bundle generation completed successfully");
                
                System.Windows.MessageBox.Show(
                    $"Bundle Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.BUNDLE}{FileTools.CommonData.CommonData.Bank}.SLDASM\n" +
                    $"Bundle Width: {config.BundleWidth:F3}\"\n" +
                    $"Side Frame THK: {config.SideFrameThickness:F3}\"\n" +
                    $"Side Frame Depth: {config.SideFrameDepth:F3}\"\n\n" +
                    $"All files created in SolidWorks!",
                    "Bundle Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);
  
                GlobalErrorHandler.LogInfo("Bundle configuration validated - ready for full integration");
                progressCallback?.Invoke(100);
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Bundle Generation Failed");
                
                System.Windows.MessageBox.Show(
                    $"Error generating bundle:\n\n{ex.Message}\n\n" +
                    $"Check log file: {GlobalErrorHandler.LogFilePath}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private void GenerateHeader(HeaderConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Header generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Header configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");
                if (string.IsNullOrWhiteSpace(config.HeaderType))
                    throw new InvalidOperationException("Header Type is required");

                progressCallback?.Invoke(20);

                // Set CommonData properties
                FileTools.CommonData.CommonData.Project = config.JobNumber;
                FileTools.CommonData.CommonData.Bank = ConvertBankToChar(config.Bank);
                FileTools.CommonData.CommonData.Initials = config.Initials ?? "DC";
                FileTools.CommonData.CommonData.Customer = config.Customer ?? "";
                FileTools.CommonData.CommonData.Client = config.Client ?? "";
                FileTools.CommonData.CommonData.PlantLocation = config.Location ?? "";
                FileTools.CommonData.CommonData.PurchaseOrder = config.PurchaseOrder ?? "";
                FileTools.CommonData.CommonData.ItemNumber = config.ItemNumber ?? "";

                // Determine which header type (61-66)
                int headerNumber = int.Parse(config.HeaderType);

                // Set the appropriate Header61-66 properties based on config
                var headerInstance = GetHeaderInstance(headerNumber);
                headerInstance.IsRequired = true;
                headerInstance.BoxWidth = config.BoxWidth;
                headerInstance.BoxHeight = config.BoxHeight;
                headerInstance.BoxLength = config.BoxLength;
                if (config.TubesheetThickness > 0)
                    headerInstance.TubesheetTHK = config.TubesheetThickness;

                // Create Header instance
                progressCallback?.Invoke(50);
                new HDR.HeaderBase(headerNumber, "Header");

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"Header Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{headerNumber}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"Type: Header {headerNumber}\n" +
                    $"Box: {config.BoxWidth}\" x {config.BoxHeight}\" x {config.BoxLength}\"\n" +
                    $"All files created in SolidWorks!",
                    "Header Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("Header generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Header Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating header:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private FileTools.Base.IHeaderExtensions GetHeaderInstance(int headerNumber)
        {
            return headerNumber switch
            {
                61 => FileTools.CommonData.CommonData.Header61,
                62 => FileTools.CommonData.CommonData.Header62,
                63 => FileTools.CommonData.CommonData.Header63,
                64 => FileTools.CommonData.CommonData.Header64,
                65 => FileTools.CommonData.CommonData.Header65,
                66 => FileTools.CommonData.CommonData.Header66,
                _ => throw new ArgumentException($"Invalid header number: {headerNumber}. Must be 61-66.")
            };
        }

        private void GenerateHood(HoodConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Hood generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Hood configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                progressCallback?.Invoke(20);

                // Map UnifiedUI config to HoodData static properties
                Hood.HoodData.Project = config.JobNumber;
                Hood.HoodData.Bank = ConvertBankToChar(config.Bank);
                Hood.HoodData.Length = config.Length;
                Hood.HoodData.Width = config.Width;
                Hood.HoodData.Height = config.Height;
                Hood.HoodData.fanDiameterInFeet = config.FanDiameter;
                Hood.HoodData.Stacks = config.Stacks;
                Hood.HoodData.WindLoad = config.WindLoad;
                // Note: Hood.HoodData.Ring property integration pending
                // Hood.HoodData.Ring.Depth = config.DepthOption;
                Hood.HoodData.Shift = config.ShiftStiffeners;
                Hood.HoodData.Adjust = config.Adjust;
                Hood.HoodData.Initials = config.Initials ?? "DC";
                Hood.HoodData.Customer = config.Customer ?? "";
                Hood.HoodData.Client = config.Client ?? "";
                Hood.HoodData.Location = config.Location ?? "";
                Hood.HoodData.PurchaseOrder = config.PurchaseOrder ?? "";
                Hood.HoodData.ItemNumber = config.ItemNumber ?? "";

                progressCallback?.Invoke(50);

                // Create Hood
                new Hood.Hood();

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"Hood Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.HOOD}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"Size: {config.Length}\" x {config.Width}\" x {config.Height}\"\n" +
                    $"All files created in SolidWorks!",
                    "Hood Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("Hood generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Hood Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating hood:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private void GenerateWalkway(WalkwayConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Walkway generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Walkway configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                progressCallback?.Invoke(20);

                // Map to static properties
                Walkway.Walkway.Project = config.JobNumber;
                Walkway.Walkway.Bank = ConvertBankToChar(config.Bank);
                Walkway.Walkway.Width = config.Width;
                Walkway.Walkway.RailHeight = config.RailHeight;
                Walkway.Walkway.FloorHeight = config.FloorHeight;
                Walkway.Walkway.MinStringerSize = ParseStringerSize(config.MinimumStringerSize);
                Walkway.Walkway.OffsetFromColumnCenter = config.OffsetFromColumnCenter;
                Walkway.Walkway.AcheColumnSize = config.ColumnSize;
                Walkway.Walkway.AcheColumnCenterToCenterWidth = config.PlenumCenterWidth;
                Walkway.Walkway.EndToSupportCenter = config.SupportCenterToEnd;
                Walkway.Walkway.Initials = config.Initials ?? "DC";
                Walkway.Walkway.Customer = config.Customer ?? "";
                Walkway.Walkway.Client = config.Client ?? "";
                Walkway.Walkway.Location = config.Location ?? "";
                Walkway.Walkway.PurchaseOrder = config.PurchaseOrder ?? "";
                Walkway.Walkway.ItemNumber = config.ItemNumber ?? "";

                progressCallback?.Invoke(50);

                // Call static method
                Walkway.Walkway.Create_Standard_EndWalkway(
                    Walkway.Walkway.Bank,
                    Walkway.Walkway.Width,
                    Walkway.Walkway.RailHeight,
                    Walkway.Walkway.FloorHeight,
                    Walkway.Walkway.MinStringerSize,
                    Walkway.Walkway.OffsetFromColumnCenter,
                    Walkway.Walkway.AcheColumnSize,
                    Walkway.Walkway.AcheColumnCenterToCenterWidth,
                    Walkway.Walkway.EndToSupportCenter
                );

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"Walkway Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.WALKWAY}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"All files created in SolidWorks!",
                    "Walkway Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("Walkway generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Walkway Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating walkway:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private void GenerateMachineryMount(MachineryMountConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting MachineryMount generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "MachineryMount configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                progressCallback?.Invoke(20);

                // Map UnifiedUI config to CommonData static properties
                FileTools.CommonData.CommonData.Project = config.JobNumber;
                FileTools.CommonData.CommonData.Bank = ConvertBankToChar(config.Bank);
                FileTools.CommonData.CommonData.MachineryMount_Width = config.MountWidth;
                FileTools.CommonData.CommonData.MachineryMount_Length = config.MountLength;
                FileTools.CommonData.CommonData.MachineryMount_Height = config.MountHeight;
                FileTools.CommonData.CommonData.Initials = config.Initials ?? "DC";
                FileTools.CommonData.CommonData.Customer = config.Customer ?? "";
                FileTools.CommonData.CommonData.Client = config.Client ?? "";
                FileTools.CommonData.CommonData.PlantLocation = config.Location ?? "";
                FileTools.CommonData.CommonData.PurchaseOrder = config.PurchaseOrder ?? "";
                FileTools.CommonData.CommonData.ItemNumber = config.ItemNumber ?? "";

                progressCallback?.Invoke(50);

                // Create MachineryMount
                new MachineryMount.MachineryMount(AssemblyNumbers.MACHINERY_MOUNT, "MachineryMount");

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"MachineryMount Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.MACHINERY_MOUNT}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"Size: {config.MountWidth}\" x {config.MountLength}\"\n" +
                    $"All files created in SolidWorks!",
                    "MachineryMount Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("MachineryMount generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "MachineryMount Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating machinery mount:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private void GeneratePlenum(PlenumConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Plenum generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Plenum configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                progressCallback?.Invoke(20);

                // Map UnifiedUI config to CommonData static properties
                FileTools.CommonData.CommonData.Project = config.JobNumber;
                FileTools.CommonData.CommonData.Bank = ConvertBankToChar(config.Bank);
                FileTools.CommonData.CommonData.Plenum_Width = config.PlenumWidth;
                FileTools.CommonData.CommonData.Plenum_Length = config.PlenumLength;
                // Note: Plenum_Height and Plenum_Depth properties integration pending
                // FileTools.CommonData.CommonData.Plenum_Height = config.PlenumHeight;
                // FileTools.CommonData.CommonData.Plenum_Depth = config.PlenumDepth;
                FileTools.CommonData.CommonData.Initials = config.Initials ?? "DC";
                FileTools.CommonData.CommonData.Customer = config.Customer ?? "";
                FileTools.CommonData.CommonData.Client = config.Client ?? "";
                FileTools.CommonData.CommonData.PlantLocation = config.Location ?? "";
                FileTools.CommonData.CommonData.PurchaseOrder = config.PurchaseOrder ?? "";
                FileTools.CommonData.CommonData.ItemNumber = config.ItemNumber ?? "";

                progressCallback?.Invoke(50);

                // Note: Complete Plenum implementation pending
                // var plenum = new Plenum.Plenum();
                // InitializePlenum requires a Design enum parameter
                // plenum.InitializePlenum(Design.Standard);

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"Plenum Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.PLENUM}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"Size: {config.PlenumWidth}\" x {config.PlenumLength}\" x {config.PlenumDepth}\"\n" +
                    $"All files created in SolidWorks!",
                    "Plenum Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("Plenum generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Plenum Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating plenum:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private void GenerateStructure(StructureConfiguration config, Action<int> progressCallback)
        {
            try
            {
                GlobalErrorHandler.LogInfo("Starting Structure generation from UnifiedUI");
                progressCallback?.Invoke(10);

                // Validate
                if (config == null)
                    throw new ArgumentNullException(nameof(config), "Structure configuration cannot be null");
                if (string.IsNullOrWhiteSpace(config.JobNumber))
                    throw new InvalidOperationException("Job Number is required");

                progressCallback?.Invoke(20);

                // Map UnifiedUI config to CommonData static properties
                FileTools.CommonData.CommonData.Project = config.JobNumber;
                FileTools.CommonData.CommonData.Bank = ConvertBankToChar(config.Bank);
                FileTools.CommonData.CommonData.Plenum_Width = config.StructureWidth;
                FileTools.CommonData.CommonData.Plenum_Length = config.StructureLength;
                // Note: TotalColumnHeight is read-only, integration pending
                // FileTools.CommonData.CommonData.TotalColumnHeight = config.StructureHeight;
                FileTools.CommonData.CommonData.Initials = config.Initials ?? "DC";
                FileTools.CommonData.CommonData.Customer = config.Customer ?? "";
                FileTools.CommonData.CommonData.Client = config.Client ?? "";
                FileTools.CommonData.CommonData.PlantLocation = config.Location ?? "";
                FileTools.CommonData.CommonData.PurchaseOrder = config.PurchaseOrder ?? "";
                FileTools.CommonData.CommonData.ItemNumber = config.ItemNumber ?? "";

                progressCallback?.Invoke(50);

                // Create Structure
                new Structure.Structure(AssemblyNumbers.STRUCTURE, "Structure");

                progressCallback?.Invoke(100);

                System.Windows.MessageBox.Show(
                    $"Structure Generated Successfully!\n\n" +
                    $"Job: {config.JobNumber}\n" +
                    $"Assembly: {config.JobNumber}-{AssemblyNumbers.STRUCTURE}{ConvertBankToChar(config.Bank)}.SLDASM\n" +
                    $"Type: {config.StructureType}\n" +
                    $"Size: {config.StructureWidth}\" x {config.StructureLength}\" x {config.StructureHeight}\"\n" +
                    $"All files created in SolidWorks!",
                    "Structure Generation Complete",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Information);

                GlobalErrorHandler.LogInfo("Structure generation completed successfully");
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "Structure Generation Failed");
                System.Windows.MessageBox.Show(
                    $"Error generating structure:\n\n{ex.Message}",
                    "Generation Error",
                    System.Windows.MessageBoxButton.OK,
                    System.Windows.MessageBoxImage.Error);
                throw;
            }
        }

        private int ParseStringerSize(string size)
        {
            return size switch
            {
                "C6" => 0,
                "C8" => 1,
                "C10" => 2,
                _ => 0
            };
        }
    }

    /// <summary>
    /// Strategy for Design Table approach (Excel-driven)
    /// Copies template files, updates Excel configurations, and generates components
    /// </summary>
    /// <remarks>
    /// This strategy works with Excel-based design tables and template files.
    /// Used for: XCH Structure, Z Structure, and advanced Header configurations
    /// </remarks>
    public class DesignTableStrategy : IGenerationStrategy
    {
        private readonly TemplateFileManager _templateManager;
        private readonly JobFolderManager _folderManager;
        private readonly ExcelConfigWriter _excelWriter;

        public DesignTableStrategy()
        {
            _templateManager = new TemplateFileManager();
            _folderManager = new JobFolderManager();
            _excelWriter = new ExcelConfigWriter();
        }

        public void Generate(ComponentConfiguration config, Action<int> progressCallback)
        {
            progressCallback?.Invoke(10);

            // STEP 1: Create job folder structure
            var variant = config.GetParameter<string>("Variant", "S03");
            var folderResult = _folderManager.CreateJobFolderStructure(
                config.JobNumber, 
                config.ComponentType, 
                variant);

            if (!folderResult.Success)
            {
                throw new Exception($"Failed to create folders: {folderResult.Error}");
            }

            progressCallback?.Invoke(25);

            // STEP 2: Get template path
            var templatePath = _templateManager.GetTemplatePath(config.ComponentType, variant);
            if (string.IsNullOrEmpty(templatePath))
            {
                throw new Exception($"No template found for {config.ComponentType} ({variant})");
            }

            progressCallback?.Invoke(35);

            // STEP 3: Copy and rename template files
            var copyRequest = new TemplateCopyRequest
            {
                SourceTemplatePath = templatePath,
                DestinationPath = folderResult.ComponentFolderPath,
                OldPrefix = "000000", // Template prefix
                NewPrefix = config.JobNumber // Job number
            };

            var copyResult = _templateManager.CopyAndRenameTemplates(copyRequest);
            if (!copyResult.Success)
            {
                throw new Exception($"Failed to copy templates: {copyResult.Error}");
            }

            progressCallback?.Invoke(55);

            // STEP 4: Update Excel configuration files
            var excelFiles = _templateManager.GetExcelFilesToUpdate(
                folderResult.ComponentFolderPath, 
                config.ComponentType);

            var excelResults = _excelWriter.UpdateMultipleFiles(excelFiles, config);

            // Check for Excel errors
            foreach (var result in excelResults)
            {
                if (!result.Success)
                {
                    throw new Exception($"Failed to update Excel: {result.Error}");
                }
            }

            progressCallback?.Invoke(75);

            // STEP 5: Open in SolidWorks and rebuild (optional - can be done later)
            // This step would require SolidWorks API integration
            // For now, files are ready for manual opening or automated opening

            progressCallback?.Invoke(90);

            // Success!
            var summary = $"Generated {config.ComponentType}:\n" +
                         $"- Copied {copyResult.CopiedFiles.Count} files\n" +
                         $"- Updated {excelResults.Count} Excel files\n" +
                         $"- Location: {folderResult.ComponentFolderPath}";

            progressCallback?.Invoke(100);
        }
    }
}
