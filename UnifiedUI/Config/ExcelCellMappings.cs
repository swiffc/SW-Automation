using System.Collections.Generic;

namespace UnifiedUI.Config
{
    /// <summary>
    /// Excel Cell Mappings - Maps engineering parameters to specific Excel cells
    /// Based on analysis of S25140-Prego1.xlsm
    /// </summary>
    public static class ExcelCellMappings
    {
        /// <summary>
        /// Bundle parameter mappings
        /// Primary source: RAGU sheet (most consolidated data)
        /// Backup source: Input sheet
        /// </summary>
        public static class Bundle
        {
            // Job Information
            public const string SHEET_JOB = "RAGU";
            public const string CELL_JOB_NUMBER = "G4";          // Job number
            
            // Bundle Dimensions
            public const string SHEET_DIMENSIONS = "RAGU";
            public const string CELL_BUNDLE_WIDTH = "G17";       // Bundle overall width
            public const string CELL_BUNDLE_DEPTH = "J84";       // Bundle depth
            public const string CELL_SIDE_FRAME_THK = "G20";     // Side frame thickness (estimate)
            public const string CELL_SIDE_FRAME_DEPTH = "G21";   // Side frame depth (estimate)
            
            // Tube Specifications
            public const string SHEET_TUBES = "RAGU";
            public const string CELL_TUBE_OD = "G3";             // Tube outer diameter
            public const string CELL_TUBE_WALL = "G7";           // Tube wall thickness
            public const string CELL_TUBE_LENGTH = "G6";         // Tube length
            public const string CELL_TUBE_PROJECTION = "G8";     // Tube projection (estimate)
            public const string CELL_FIN_OD = "G9";              // Fin outer diameter (if finned)
            
            // Tube Layout
            public const string SHEET_LAYOUT = "RAGU";
            public const string CELL_TUBE_COUNT = "G72";         // Total tube count
            public const string CELL_ROW_COUNT = "J53";          // Number of rows
            public const string CELL_HORIZ_PITCH = "G56";        // Horizontal tube pitch
            public const string CELL_VERT_PITCH = "G57";         // Vertical tube pitch (estimate)
            public const string CELL_ROW_1_COUNT = "J54";        // Tubes in row 1 (estimate)
            public const string CELL_ROW_2_COUNT = "J55";        // Tubes in row 2 (estimate)
            
            // Materials
            public const string SHEET_MATERIAL = "RAGU";
            public const string CELL_TUBE_MATERIAL = "J99";      // Tube material specification
            
            // Backup mappings from Input sheet
            public static class InputSheet
            {
                public const string SHEET = "Input";
                public const string CELL_JOB_NUMBER = "G2";
                public const string CELL_BUNDLE_WIDTH = "E12";
                public const string CELL_TUBE_OD = "K10";
                public const string CELL_TUBE_WALL = "K14";
                public const string CELL_TUBE_LENGTH = "K15";
                public const string CELL_TUBE_COUNT = "H13";
                public const string CELL_HORIZ_PITCH = "AJ4";
            }
        }

        /// <summary>
        /// Header parameter mappings
        /// Primary source: RAGU sheet
        /// Backup source: Input sheet
        /// </summary>
        public static class Header
        {
            // Job Information
            public const string SHEET_JOB = "RAGU";
            public const string CELL_JOB_NUMBER = "G4";
            
            // Header Box Dimensions
            public const string SHEET_DIMENSIONS = "RAGU";
            public const string CELL_HEADER_LENGTH = "G6";       // Header length (same as tube length + clearances)
            public const string CELL_BOX_WIDTH = "G18";          // Header box width (estimate)
            public const string CELL_BOX_HEIGHT = "G19";         // Header box height (estimate)
            
            // Tubesheet
            public const string SHEET_TUBESHEET = "RAGU";
            public const string CELL_TUBESHEET_THK = "G7";       // Tubesheet thickness
            public const string CELL_TUBE_HOLE_DIA = "G10";      // Tube hole diameter (estimate)
            public const string CELL_TUBE_PROJECTION = "G11";    // Tube projection into header (estimate)
            
            // Plates
            public const string SHEET_PLATES = "RAGU";
            public const string CELL_COVER_PLATE_THK = "G12";    // Cover plate thickness (estimate)
            public const string CELL_SHELL_THK = "G13";          // Shell thickness (estimate)
            
            // Pressure Design
            public const string SHEET_PRESSURE = "Input";
            public const string CELL_DESIGN_PRESSURE = "A9";     // Design pressure
            public const string CELL_MAWP = "H32";               // Maximum Allowable Working Pressure
            
            // Materials
            public const string SHEET_MATERIAL = "RAGU";
            public const string CELL_SHELL_MATERIAL = "J100";    // Shell material (estimate)
            public const string CELL_TUBESHEET_MATERIAL = "J101"; // Tubesheet material (estimate)
            
            // Backup mappings from Input sheet
            public static class InputSheet
            {
                public const string SHEET = "Input";
                public const string CELL_HEADER_WIDTH = "AC42";
                public const string CELL_TUBESHEET_THK = "AC49";
                public const string CELL_DESIGN_PRESSURE = "A9";
                public const string CELL_MAWP = "H32";
            }
        }

        /// <summary>
        /// Common parameters shared across components
        /// </summary>
        public static class Common
        {
            // Job & Customer Info (from Inputs_Calcs)
            public const string SHEET_JOB_INFO = "Inputs_Calcs";
            public const string CELL_JOB_NUMBER = "Q44";
            public const string CELL_CUSTOMER = "Q45";           // Customer name (estimate)
            public const string CELL_PROJECT = "Q46";            // Project name (estimate)
            
            // Design Conditions (from Input)
            public const string SHEET_DESIGN = "Input";
            public const string CELL_DESIGN_PRESSURE = "A9";
            public const string CELL_DESIGN_TEMP = "A10";        // Design temperature (estimate)
            public const string CELL_MAWP = "H32";
            
            // Materials (from Input)
            public const string SHEET_MATERIAL = "Input";
            public const string CELL_MATERIAL_SPEC = "K6";
            
            // Calculations (from Inputs_Calcs)
            public const string SHEET_CALCS = "Inputs_Calcs";
            public const string CELL_TUBE_OD = "J2";
            public const string CELL_TUBE_LENGTH = "AA5";
            public const string CELL_TUBE_WALL = "AP48";
        }

        /// <summary>
        /// SolidWorks export sheet - Already formatted for SolidWorks!
        /// This sheet (Prego_to_Sw) may have the cleanest data
        /// </summary>
        public static class SolidWorksExport
        {
            public const string SHEET = "Prego_to_Sw";
            public const string CELL_BUNDLE_WIDTH = "C7";
            public const string CELL_DESIGN_PRESSURE = "A13";
            public const string CELL_TUBE_WALL = "C13";
            public const string CELL_TUBE_LENGTH = "C25";
            public const string CELL_HEADER_LENGTH = "C25";
            public const string CELL_TUBESHEET_THK = "C38";
            public const string CELL_TUBE_OD = "C100";
        }

        /// <summary>
        /// Get preferred cell mapping for a parameter
        /// Uses primary source, falls back to backup if needed
        /// </summary>
        public static (string Sheet, string Cell) GetCellMapping(string componentType, string parameterName)
        {
            // This method can be enhanced to provide intelligent fallback logic
            // For now, it serves as documentation of the mapping strategy
            
            if (componentType == "Bundle")
            {
                return parameterName switch
                {
                    "JobNumber" => (Bundle.SHEET_JOB, Bundle.CELL_JOB_NUMBER),
                    "BundleWidth" => (Bundle.SHEET_DIMENSIONS, Bundle.CELL_BUNDLE_WIDTH),
                    "TubeOD" => (Bundle.SHEET_TUBES, Bundle.CELL_TUBE_OD),
                    "TubeLength" => (Bundle.SHEET_TUBES, Bundle.CELL_TUBE_LENGTH),
                    "TubeCount" => (Bundle.SHEET_LAYOUT, Bundle.CELL_TUBE_COUNT),
                    _ => (string.Empty, string.Empty)
                };
            }
            else if (componentType == "Header")
            {
                return parameterName switch
                {
                    "JobNumber" => (Header.SHEET_JOB, Header.CELL_JOB_NUMBER),
                    "TubesheetThickness" => (Header.SHEET_TUBESHEET, Header.CELL_TUBESHEET_THK),
                    "DesignPressure" => (Header.SHEET_PRESSURE, Header.CELL_DESIGN_PRESSURE),
                    "MAWP" => (Header.SHEET_PRESSURE, Header.CELL_MAWP),
                    _ => (string.Empty, string.Empty)
                };
            }
            
            return (string.Empty, string.Empty);
        }

        /// <summary>
        /// Engineering notes and warnings
        /// </summary>
        public static class Notes
        {
            public const string RAGU_SHEET_PURPOSE = "RAGU sheet contains consolidated, calculated values - preferred source";
            public const string INPUT_SHEET_PURPOSE = "Input sheet contains user inputs - backup source";
            public const string PREGO_TO_SW_PURPOSE = "Prego_to_Sw sheet is pre-formatted for SolidWorks - may be most reliable";
            public const string CELL_VERIFICATION = "All cell addresses must be verified with actual Excel file before production use";
            public const string FORMULA_HANDLING = "Some cells contain formulas - read calculated value, not formula text";
        }
    }
}
