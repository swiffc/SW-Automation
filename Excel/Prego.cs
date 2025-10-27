using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.Office.Interop.Excel;
using static FileTools.CommonData.CommonData;
using Application = Microsoft.Office.Interop.Excel.Application;
using AXC_Vault;
using SplashScreen;
using FileTools.CommonData;
using System.ComponentModel;
using EPDM.Interop.epdm;
using System.Runtime.InteropServices;
using static FileTools.StaticFileTools;
using FileTools.Infrastructure;

namespace Excel
{
    public static class Prego
    {
        // Static COM manager for tracking Excel objects
        private static ComObjectManager _comManager = new ComObjectManager();

        // Static constructor
        static Prego()
        {
      CommonData.PropertyChanged += CommonData_PropertyChanged;
          GlobalErrorHandler.LogInfo("Prego initialized");
     }

        // Static properties
        static Application ExcelApp
   {
            get
     {
        if (_excel == null)
      {
    try
      {
     GlobalErrorHandler.LogInfo("Connecting to Excel...");
             _excel = _comManager.Track(
        (Application)Marshal.GetActiveObject("Excel.Application"));
        GlobalErrorHandler.LogInfo("Connected to existing Excel instance");
       }
   catch (COMException ex)
     {
   GlobalErrorHandler.LogInfo("Excel not running, creating new instance");
     try
        {
_excel = _comManager.Track(new Application());
         GlobalErrorHandler.LogInfo("New Excel instance created");
     }
     catch (Exception innerEx)
      {
       GlobalErrorHandler.LogError(innerEx, "Create Excel Instance");
            throw new InvalidOperationException(
     "Failed to create Excel instance. Please ensure Excel is installed.", innerEx);
     }
               }
    catch (Exception ex)
     {
            GlobalErrorHandler.LogError(ex, "Excel Connection");
        throw new InvalidOperationException(
        "Failed to connect to Excel. Please ensure Excel is installed.", ex);
        }
                }
  return _excel;
      }
        }

        public static Workbook PregoDoc
        {
            get
            {
      if (_pregoDoc == null)
      {
   try
          {
           GlobalErrorHandler.LogInfo("Opening Prego document");
       
        string expectedFolder = $@"C:\AXC_VAULT\Active\{Project}\Drafting\Headers\~Archive\";
         string expectedFileName = $"{Project}-prego{Bank - 'A' + 1}.xlsm";
  string expectedFilePath = Path.Combine(expectedFolder, expectedFileName);

        bool localFileExists = File.Exists(expectedFilePath);

        PleaseWait.Start("Connecting to AXC_VAULT");
     
            if (Developer)
            {
    ExcelApp.Visible = true;
    }
         else if (Vault.FileExists(expectedFilePath, out IEdmFile5 file))
       {
        if (!localFileExists)
                  {
       GlobalErrorHandler.LogInfo($"Downloading from vault: {expectedFilePath}");
       Vault.DownloadFile(file);
      }
        }

           if (localFileExists)
{
  PleaseWait.Hide();
                DialogResult result = MessageBox.Show(
        "Would you like to import data from Prego found at:" + "\n" + expectedFilePath,
     "Import Data", MessageBoxButtons.YesNo, MessageBoxIcon.Question);

if (result == DialogResult.Yes)
    {
           PleaseWait.Show($"Loading {expectedFileName}");
  FilePath = expectedFilePath;
             _pregoDoc = _comManager.Track(ExcelApp.Workbooks.Open(expectedFilePath));
GlobalErrorHandler.LogInfo($"Prego opened: {expectedFilePath}");
        }
   }

    if (_pregoDoc == null)
{
      if (!Developer)
   PleaseWait.Show($"Loading...");

 if (Developer)
    {
   expectedFolder = Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
                 PleaseWait.Hide();
           }

          OpenFileDialog openFileDialog = new OpenFileDialog
     {
        Title = "Select Prego file",
            Filter = "Excel files (*.xlsm)|*.xlsm",
    FilterIndex = 1,
       InitialDirectory = expectedFolder,
};

      if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
   PleaseWait.Show($"Loading {openFileDialog.FileName}");
        FilePath = openFileDialog.FileName;
     _pregoDoc = _comManager.Track(ExcelApp.Workbooks.Open(openFileDialog.FileName));
             GlobalErrorHandler.LogInfo($"Prego opened (manual selection): {FilePath}");
       }
             }

        PleaseWait.Hide();
        }
 catch (Exception ex)
            {
      GlobalErrorHandler.LogError(ex, "Open Prego Document");
            PleaseWait.Stop();
    throw;
         }
                }
      return _pregoDoc;
    }
  }

        public static Worksheet InputSheet
        {
            get
            {
      if (_inputSheet == null && PregoDoc != null)
                {
       try
             {
      _inputSheet = _comManager.Track((Worksheet)PregoDoc.Sheets["Input"]);
    GlobalErrorHandler.LogInfo("InputSheet accessed");
         }
   catch (Exception ex)
     {
           GlobalErrorHandler.LogError(ex, "Access InputSheet");
  throw new InvalidOperationException("Failed to access 'Input' sheet in Prego file.", ex);
      }
     }
       return _inputSheet;
  }
        }

     public static Worksheet SketchCalcsSheet
        {
    get
      {
     if (_sketchCalcsSheet == null && PregoDoc != null)
   {
  try
      {
          _sketchCalcsSheet = _comManager.Track((Worksheet)PregoDoc.Sheets["Sketch_Calcs"]);
         }
         catch (Exception ex)
          {
          GlobalErrorHandler.LogError(ex, "Access SketchCalcsSheet");
        throw new InvalidOperationException("Failed to access 'Sketch_Calcs' sheet.", ex);
     }
      }
 return _sketchCalcsSheet;
     }
        }

   public static Worksheet InputsCalcsSheet
        {
            get
 {
             if (_inputsCalcsSheet == null && PregoDoc != null)
                {
         try
      {
               _inputsCalcsSheet = _comManager.Track((Worksheet)PregoDoc.Sheets["Inputs_Calcs"]);
         }
            catch (Exception ex)
           {
 GlobalErrorHandler.LogError(ex, "Access InputsCalcsSheet");
      throw new InvalidOperationException("Failed to access 'Inputs_Calcs' sheet.", ex);
           }
   }
         return _inputsCalcsSheet;
            }
        }

    public static Worksheet PregoToMikeySheet
        {
            get
          {
     if (_pregoToMikeySheet == null && PregoDoc != null)
           {
           try
    {
             _pregoToMikeySheet = _comManager.Track((Worksheet)PregoDoc.Sheets["Prego_to_Mikey"]);
         }
catch (Exception ex)
        {
    GlobalErrorHandler.LogError(ex, "Access PregoToMikeySheet");
             throw new InvalidOperationException("Failed to access 'Prego_to_Mikey' sheet.", ex);
         }
     }
    return _pregoToMikeySheet;
            }
     }

     public static Worksheet InventorSheet
        {
            get
        {
           if (_inventor == null && PregoDoc != null)
      {
   try
             {
        _inventor = _comManager.Track((Worksheet)PregoDoc.Sheets["Prego_to_Inv"]);
    }
   catch (Exception ex)
       {
             GlobalErrorHandler.LogError(ex, "Access InventorSheet");
       throw new InvalidOperationException("Failed to access 'Prego_to_Inv' sheet.", ex);
  }
         }
   return _inventor;
            }
        }

        public static Worksheet BomInputSheet
        {
            get
   {
           if (_bomInput == null && PregoDoc != null)
  {
             try
          {
  _bomInput = _comManager.Track((Worksheet)PregoDoc.Sheets["BOM_Input"]);
     }
        catch (Exception ex)
     {
             GlobalErrorHandler.LogError(ex, "Access BomInputSheet");
 throw new InvalidOperationException("Failed to access 'BOM_Input' sheet.", ex);
         }
         }
              return _bomInput;
   }
        }

        public static int Version
        {
 get
            {
   try
          {
             string versionString = CellString(InputSheet, "J2");
         if (!string.IsNullOrEmpty(versionString))
     {
         string numericVersion = versionString.TrimStart('V').Replace(".", "");

                 while (numericVersion.Length < 4)
              {
            numericVersion += "0";
       }

     if (int.TryParse(numericVersion, out int versionInt))
  {
  return versionInt;
   }
               }
       throw new FormatException("The version string is not in the expected format.");
        }
     catch (Exception ex)
            {
              GlobalErrorHandler.LogError(ex, "Get Prego Version");
             throw;
          }
      }
        }

        public static string FilePath { get; set; }

        // Public methods
        public static List<double> CellDoubleList(Worksheet sheet, params string[] cellNames)
        {
       var list = new List<double>();

            foreach (var cellName in cellNames)
            {
   double cellValue = CellDouble(sheet, cellName);
                if (cellValue != 0)
    list.Add(cellValue);
        }

    return list;
        }

 public static List<string> CellStringList(Worksheet sheet, params string[] cellNames)
        {
     var list = new List<string>();

       foreach (var cellName in cellNames)
            {
          string cellString = CellString(sheet, cellName);
     if (cellString != null)
        list.Add(cellString);
      }

   return list;
        }

      public static string[] CellNameColumnArray(string firstCell, string lastCell)
        {
            var columnMatch = Regex.Match(firstCell, @"[A-Za-z]+");
          var firstRowMatch = Regex.Match(firstCell, @"\d+");
            var lastRowMatch = Regex.Match(lastCell, @"\d+");

            if (!columnMatch.Success || !firstRowMatch.Success || !lastRowMatch.Success)
            {
                throw new ArgumentException("Invalid cell name format.");
            }

       string column = columnMatch.Value;
        int firstRow = int.Parse(firstRowMatch.Value);
            int lastRow = int.Parse(lastRowMatch.Value);

            if (lastRow < firstRow)
        {
    throw new ArgumentException("The last cell must be after the first cell in the sequence.");
            }

var cellNames = new List<string>();
      for (int row = firstRow; row <= lastRow; row++)
         {
                cellNames.Add($"{column}{row}");
      }

          return cellNames.ToArray();
        }

        public static string CellString(Worksheet sheet, params string[] cellNames)
        {
        for (int i = 0; i < cellNames.Length; i++)
      {
        var cellValue = CellValue(i, sheet, cellNames);

       if (cellValue is string)
                {
      if (cellValue != null && cellValue != "")
    return cellValue;
    }
    }

    return null;
      }

        public static double CellDouble(Worksheet sheet, params string[] cellNames)
  {
            for (int i = 0; i < cellNames.Length; i++)
  {
            var cellValue = CellValue(i, sheet, cellNames);

                if (cellValue != null)
  {
       if (cellValue is double != true)
           {
           if (cellValue == "" && i == cellNames.Length)
     return 0;

            bool success = TryParseCellValue(cellValue, out double? result);
  if (success)
     return result.Value;
         }
            else
 return cellValue;
                }
  }
   return 0;
        }

        public static void CleanUp(bool fullCleanUp = false)
        {
          try
{
GlobalErrorHandler.LogInfo($"Excel cleanup (full: {fullCleanUp})");

         if (fullCleanUp)
       {
     // Close workbook if open
     if (_pregoDoc != null)
       {
             try
            {
          _pregoDoc.Close(false);
  GlobalErrorHandler.LogInfo("Prego document closed");
    }
       catch (Exception ex)
          {
      GlobalErrorHandler.LogWarning($"Error closing Prego: {ex.Message}");
        }
    }

        // Quit Excel if we created it
         if (_excel != null)
       {
             try
  {
          _excel.Quit();
        GlobalErrorHandler.LogInfo("Excel application quit");
         }
      catch (Exception ex)
    {
              GlobalErrorHandler.LogWarning($"Error quitting Excel: {ex.Message}");
       }
            }

             // Release all tracked COM objects
          _comManager.ReleaseAll();

        // Clear references
        _excel = null;
           _pregoDoc = null;
  _inputSheet = null;
             _sketchCalcsSheet = null;
     _inputsCalcsSheet = null;
    _pregoToMikeySheet = null;
        _inventor = null;
        _bomInput = null;

              GlobalErrorHandler.LogInfo("Full Excel cleanup complete");
     }
     else
    {
       // Partial cleanup - just release worksheet references
 if (_inputSheet != null)
 {
   _comManager.Release(ref _inputSheet);
   }
         if (_sketchCalcsSheet != null)
  {
           _comManager.Release(ref _sketchCalcsSheet);
          }
        if (_inputsCalcsSheet != null)
                    {
            _comManager.Release(ref _inputsCalcsSheet);
          }
if (_pregoToMikeySheet != null)
  {
       _comManager.Release(ref _pregoToMikeySheet);
             }
   if (_inventor != null)
   {
     _comManager.Release(ref _inventor);
       }
    if (_bomInput != null)
      {
     _comManager.Release(ref _bomInput);
   }

 GlobalErrorHandler.LogInfo("Partial Excel cleanup complete");
   }
            }
            catch (Exception ex)
        {
        GlobalErrorHandler.LogError(ex, "Excel Cleanup");
         }
        }

        // Private methods
        static bool TryParseCellValue(string cellValue, out double? result)
     {
   Match match = Regex.Match(cellValue, @"-?\d+\.?\d*");
            if (match.Success)
   {
          result = double.Parse(match.Value);
            return true;
      }
       else
    {
    result = null;
            return false;
       }
        }

        static dynamic CellValue(int i, Worksheet sheet, params string[] cellNames)
        {
      string column = Regex.Match(cellNames[i], @"[A-Za-z]+").Value;
       int row = int.Parse(Regex.Match(cellNames[i], @"\d+").Value);

            sheet.Activate();
   Range cell = (Range)sheet.Cells[row, column];
            return cell.Value2;
        }

        // Event handlers
   private static void CommonData_PropertyChanged(object sender, PropertyChangedEventArgs e)
     {
            if (e.PropertyName == nameof(CommonData.Bank) || e.PropertyName == nameof(CommonData.Project))
    {
          if (ClearPregoOnJobChanged)
                {
         GlobalErrorHandler.LogInfo("Job changed, clearing Prego references");
         _pregoDoc = null;
_inputSheet = null;
          _sketchCalcsSheet = null;
       _inputsCalcsSheet = null;
          _pregoToMikeySheet = null;
         }
        }
        }

        public static bool ClearPregoOnJobChanged { get; set; } = true;

        // Backing fields
        static Application _excel;
        static public Workbook _pregoDoc;
        static Worksheet _inputSheet;
static Worksheet _sketchCalcsSheet;
    static Worksheet _inputsCalcsSheet;
        static Worksheet _pregoToMikeySheet;
        static Worksheet _inventor;
        static Worksheet _bomInput;
    }
}