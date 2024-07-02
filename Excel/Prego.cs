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

namespace Excel
{
    public static class Prego
    {
        // Static constructor
        static Prego()
        {
            CommonData.PropertyChanged += CommonData_PropertyChanged;
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
                        _excel = (Application)Marshal.GetActiveObject("Excel.Application");
                    }
                    catch (Exception)
                    {
                        _excel = new Application();
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
                    string expectedFolder = $@"C:\AXC_VAULT\Active\{Project}\Drafting\Headers\~Archive\";
                    string expectedFileName = $"{Project}-prego{Bank - 'A' + 1}.xlsm";
                    string expectedFilePath = Path.Combine(expectedFolder, expectedFileName);

                    // Guess the desired file

                    bool localFileExists = File.Exists(expectedFilePath);

                    PleaseWait.Start("Connecting to AXC_VAULT");
                    if (Developer)
                    {

                    }
                    else if (Vault.FileExists(expectedFilePath, out IEdmFile5 file))
                    {
                        if (!localFileExists)
                            Vault.DownloadFile(file);
                    }

                    if (localFileExists)
                    {
                        // User to confirm
                        PleaseWait.Hide();
                        DialogResult result = MessageBox.Show(
                            "Would you like to import data from Prego found at:" + "\n" + expectedFilePath,
                            "Import Data", MessageBoxButtons.YesNo, MessageBoxIcon.Question);

                        if (result == DialogResult.Yes)
                        {
                            PleaseWait.Show($"Loading {expectedFileName}");
                            _pregoDoc = ExcelApp.Workbooks.Open(expectedFilePath);
                        }
                    }

                    // User to manually select
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
                            _pregoDoc = ExcelApp.Workbooks.Open(openFileDialog.FileName);
                        }

                    }

                    PleaseWait.Hide();
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
                    _inputSheet = (Worksheet)PregoDoc.Sheets["Input"];
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
                    _sketchCalcsSheet = (Worksheet)PregoDoc.Sheets["Sketch_Calcs"];
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
                    _inputsCalcsSheet = (Worksheet)PregoDoc.Sheets["Inputs_Calcs"];
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
                    _pregoToMikeySheet = (Worksheet)PregoDoc.Sheets["Prego_to_Mikey"];
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
                    _inventor = (Worksheet)PregoDoc.Sheets["Prego_to_Inv"];
                }
                return _inventor;
            }
        }

        public static int Version
        {
            get
            {
                string versionString = CellString(InputSheet, "J2");
                if (!string.IsNullOrEmpty(versionString))
                {
                    // Remove the 'V' prefix and all '.' characters
                    string numericVersion = versionString.TrimStart('V').Replace(".", "");

                    // Ensure the numeric version is four digits by appending '0's if necessary
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
        }


        // Public methods
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
                        if (cellValue == "")
                            return 0;

                        bool success = TryParseCellValue(cellValue, out double? result);
                        if (success)
                            return result.Value;
                    }
                    else
                        return cellValue;
                }
            }
            throw new FormatException("The cell value does not contain a valid number.");
        }


        // Private methods
        static bool TryParseCellValue(string cellValue, out double? result)
        {
            // Use regular expression to match any number, including decimal and negative
            Match match = Regex.Match(cellValue, @"-?\d+\.?\d*");
            if (match.Success)
            {
                // If a match was found, parse it to double
                result = double.Parse(match.Value);
                return true;
            }
            else
            {
                result = null;
                return false;
            }
        }
        static void CleanUp()
        {
            if (_inputSheet != null)
            {
                System.Runtime.InteropServices.Marshal.ReleaseComObject(_inputSheet);
                _inputSheet = null;
            }

            if (_pregoDoc != null)
            {
                _pregoDoc.Close(false);
                System.Runtime.InteropServices.Marshal.ReleaseComObject(_pregoDoc);
                _pregoDoc = null;
            }

            if (_excel != null)
            {
                //_excel.Quit();
                System.Runtime.InteropServices.Marshal.ReleaseComObject(_excel);
                _excel = null;
            }

            GC.Collect();
            GC.WaitForPendingFinalizers();
        }
        static dynamic CellValue(int i, Worksheet sheet, params string[] cellNames)
        {
            string column = Regex.Match(cellNames[i], @"[A-Za-z]+").Value;
            int row = int.Parse(Regex.Match(cellNames[i], @"\d+").Value);

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
        static Workbook _pregoDoc;
        static Worksheet _inputSheet;
        static Worksheet _sketchCalcsSheet;
        static Worksheet _inputsCalcsSheet;
        static Worksheet _pregoToMikeySheet;
        static Worksheet _inventor;
    }
}