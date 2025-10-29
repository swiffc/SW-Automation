using System;
using System.Collections.Generic;
using System.IO;
using UnifiedUI.Models;
using UnifiedUI.Config;
// NOTE: Add reference to Microsoft.Office.Interop.Excel via COM references
// using Excel = Microsoft.Office.Interop.Excel;

namespace UnifiedUI.Services
{
    /// <summary>
    /// Imports configuration data from Excel template files (like S25140-Prego1.xlsm)
    /// This is OPTIONAL - users can also fill forms manually
    /// </summary>
    public class ExcelTemplateImporter
    {
        /// <summary>
        /// Import Bundle configuration from Excel template
        /// </summary>
        public BundleConfiguration ImportBundleFromExcel(string excelFilePath)
        {
            if (!File.Exists(excelFilePath))
            {
                throw new FileNotFoundException($"Excel file not found: {excelFilePath}");
            }

            var config = new BundleConfiguration();

            try
            {
                // STUB VERSION (no Office Interop yet)
                // Extracts job number and sets defaults
                config.JobNumber = ExtractJobNumberFromFileName(excelFilePath);
                config.ComponentType = "Bundle";
                config.BundleWidth = 48.5;
                config.SideFrameThickness = 0.375;
                config.SideFrameDepth = 4.0;
                config.TubeLength = 84.0;
                config.TubeOD = 1.0;
                config.TubeWallThickness = 0.049;
                
                // TODO: Uncomment when Office Interop is added
                /*
                Excel.Application excelApp = null;
                Excel.Workbook workbook = null;
                
                try
                {
                    excelApp = new Excel.Application();
                    excelApp.Visible = false;
                    excelApp.DisplayAlerts = false;
                    
                    workbook = excelApp.Workbooks.Open(excelFilePath);
                    
                    // Read from RAGU sheet (primary source)
                    config.JobNumber = ReadCellValue(workbook, ExcelCellMappings.Bundle.SHEET_JOB, 
                                                     ExcelCellMappings.Bundle.CELL_JOB_NUMBER);
                    
                    config.BundleWidth = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_DIMENSIONS, 
                                                        ExcelCellMappings.Bundle.CELL_BUNDLE_WIDTH);
                    
                    config.BundleDepth = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_DIMENSIONS, 
                                                        ExcelCellMappings.Bundle.CELL_BUNDLE_DEPTH);
                    
                    config.TubeOD = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_TUBES, 
                                                   ExcelCellMappings.Bundle.CELL_TUBE_OD);
                    
                    config.TubeWallThickness = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_TUBES, 
                                                              ExcelCellMappings.Bundle.CELL_TUBE_WALL);
                    
                    config.TubeLength = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_TUBES, 
                                                       ExcelCellMappings.Bundle.CELL_TUBE_LENGTH);
                    
                    config.TubeRow1Count = ReadCellInt(workbook, ExcelCellMappings.Bundle.SHEET_LAYOUT, 
                                                       ExcelCellMappings.Bundle.CELL_ROW_1_COUNT);
                    
                    config.TubeRow2Count = ReadCellInt(workbook, ExcelCellMappings.Bundle.SHEET_LAYOUT, 
                                                       ExcelCellMappings.Bundle.CELL_ROW_2_COUNT);
                    
                    config.HorizontalPitch = ReadCellDouble(workbook, ExcelCellMappings.Bundle.SHEET_LAYOUT, 
                                                            ExcelCellMappings.Bundle.CELL_HORIZ_PITCH);
                    
                    config.ComponentType = "Bundle";
                }
                finally
                {
                    if (workbook != null)
                    {
                        workbook.Close(false);
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(workbook);
                    }
                    if (excelApp != null)
                    {
                        excelApp.Quit();
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(excelApp);
                    }
                    GC.Collect();
                    GC.WaitForPendingFinalizers();
                }
                */
                
                return config;
            }
            catch (Exception ex)
            {
                throw new Exception($"Error importing from Excel: {ex.Message}", ex);
            }
        }

        /// <summary>
        /// Import Header configuration from Excel template
        /// </summary>
        public HeaderConfiguration ImportHeaderFromExcel(string excelFilePath)
        {
            if (!File.Exists(excelFilePath))
            {
                throw new FileNotFoundException($"Excel file not found: {excelFilePath}");
            }

            var config = new HeaderConfiguration();

            try
            {
                // STUB VERSION
                config.JobNumber = ExtractJobNumberFromFileName(excelFilePath);
                config.ComponentType = "Header";
                config.HeaderType = "61";
                config.BoxWidth = 24.0;
                config.BoxHeight = 18.0;
                
                // TODO: Uncomment when Office Interop is added
                /*
                Excel.Application excelApp = null;
                Excel.Workbook workbook = null;
                
                try
                {
                    excelApp = new Excel.Application();
                    excelApp.Visible = false;
                    excelApp.DisplayAlerts = false;
                    
                    workbook = excelApp.Workbooks.Open(excelFilePath);
                    
                    config.JobNumber = ReadCellValue(workbook, ExcelCellMappings.Header.SHEET_JOB, 
                                                     ExcelCellMappings.Header.CELL_JOB_NUMBER);
                    
                    config.TubesheetThickness = ReadCellDouble(workbook, ExcelCellMappings.Header.SHEET_TUBESHEET, 
                                                               ExcelCellMappings.Header.CELL_TUBESHEET_THK);
                    
                    config.DesignPressure = ReadCellDouble(workbook, ExcelCellMappings.Header.SHEET_PRESSURE, 
                                                          ExcelCellMappings.Header.CELL_DESIGN_PRESSURE);
                    
                    config.MAWP = ReadCellDouble(workbook, ExcelCellMappings.Header.SHEET_PRESSURE, 
                                                 ExcelCellMappings.Header.CELL_MAWP);
                    
                    config.ComponentType = "Header";
                }
                finally
                {
                    if (workbook != null)
                    {
                        workbook.Close(false);
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(workbook);
                    }
                    if (excelApp != null)
                    {
                        excelApp.Quit();
                        System.Runtime.InteropServices.Marshal.ReleaseComObject(excelApp);
                    }
                    GC.Collect();
                    GC.WaitForPendingFinalizers();
                }
                */
                
                return config;
            }
            catch (Exception ex)
            {
                throw new Exception($"Error importing from Excel: {ex.Message}", ex);
            }
        }

        /// <summary>
        /// Detect component type from Excel file
        /// </summary>
        public string DetectComponentType(string excelFilePath)
        {
            // Look at filename or sheet names to detect type
            var fileName = Path.GetFileName(excelFilePath).ToLower();
            
            if (fileName.Contains("bundle")) return "Bundle";
            if (fileName.Contains("header")) return "Header";
            if (fileName.Contains("hood")) return "Hood";
            if (fileName.Contains("plenum")) return "Plenum";
            if (fileName.Contains("structure")) return "Structure";
            
            // Default: assume it's a Bundle template
            return "Bundle";
        }

        /// <summary>
        /// Extract job number from filename (e.g., S25140-Prego1.xlsm -> S25140)
        /// </summary>
        private string ExtractJobNumberFromFileName(string filePath)
        {
            try
            {
                var fileName = Path.GetFileNameWithoutExtension(filePath);
                
                // Look for pattern like "S25140" or "S2____"
                var parts = fileName.Split('-', '_');
                foreach (var part in parts)
                {
                    if (part.StartsWith("S") && part.Length >= 5)
                    {
                        return part.Substring(0, Math.Min(6, part.Length));
                    }
                }
                
                // If no pattern found, return first part
                return parts[0];
            }
            catch
            {
                return "S2____";
            }
        }

        /// <summary>
        /// Get list of recently used template files
        /// </summary>
        public List<string> GetRecentTemplates()
        {
            var recentFiles = new List<string>();
            
            // Look in common locations
            var searchPaths = new[]
            {
                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "CascadeProjects", "Solidworks_Automation"),
                @"C:\Jobs",
                Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
            };

            foreach (var path in searchPaths)
            {
                if (Directory.Exists(path))
                {
                    var files = Directory.GetFiles(path, "*.xlsm", SearchOption.TopDirectoryOnly);
                    recentFiles.AddRange(files);
                    
                    // Limit to 10 most recent
                    if (recentFiles.Count >= 10) break;
                }
            }

            return recentFiles;
        }
        
        // Helper methods for Excel reading (uncomment when Office Interop is added)
        /*
        private string ReadCellValue(Excel.Workbook workbook, string sheetName, string cellAddress)
        {
            try
            {
                Excel.Worksheet sheet = (Excel.Worksheet)workbook.Sheets[sheetName];
                Excel.Range cell = sheet.Range[cellAddress];
                var value = cell.Value2;
                
                System.Runtime.InteropServices.Marshal.ReleaseComObject(cell);
                System.Runtime.InteropServices.Marshal.ReleaseComObject(sheet);
                
                return value?.ToString() ?? "";
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine($"Error reading cell {sheetName}!{cellAddress}: {ex.Message}");
                return "";
            }
        }
        
        private double ReadCellDouble(Excel.Workbook workbook, string sheetName, string cellAddress)
        {
            try
            {
                var valueStr = ReadCellValue(workbook, sheetName, cellAddress);
                if (double.TryParse(valueStr, out double result))
                    return result;
                return 0.0;
            }
            catch
            {
                return 0.0;
            }
        }
        
        private int ReadCellInt(Excel.Workbook workbook, string sheetName, string cellAddress)
        {
            try
            {
                var valueStr = ReadCellValue(workbook, sheetName, cellAddress);
                if (int.TryParse(valueStr, out int result))
                    return result;
                return 0;
            }
            catch
            {
                return 0;
            }
        }
        */
    }

    /// <summary>
    /// Result of Excel import operation
    /// </summary>
    public class ExcelImportResult
    {
        public bool Success { get; set; }
        public string Message { get; set; }
        public string Error { get; set; }
        public ComponentConfiguration Configuration { get; set; }
        public int FieldsImported { get; set; }
    }
}
