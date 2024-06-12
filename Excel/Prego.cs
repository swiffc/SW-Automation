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
        static Workbook PregoDoc
        {
            get
            {
                if (_pregoDoc == null)
                {
                    string expectedFolder = $@"C:\AXC_VAULT\Active\{Project}\Drafting\Headers\~Archive\";
                    string expectedFileName = $"{Project}-prego{Bank - 'A' + 1}.xlsm";
                    string expectedFilePath = Path.Combine(expectedFolder, expectedFileName);

                    // Guess the desired file

                    PleaseWait.Start("Connecting to AXC_VAULT");
                    if (Vault.FileExists(expectedFilePath, out IEdmFile5 file))
                    {
                        if (!File.Exists(expectedFilePath))
                            Vault.DownloadFile(file);

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
                        PleaseWait.Show($"Loading...");
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


        // Public methods
        public static string CellString(Worksheet sheet, params string[] cellNames)
        {
            for (int i = 0; i < cellNames.Length; i++)
            {
                string column = Regex.Match(cellNames[i], @"[A-Za-z]+").Value;
                int row = int.Parse(Regex.Match(cellNames[i], @"\d+").Value);

                Range cell = (Range)sheet.Cells[row, column];
                string cellValue = cell.Value2;

                if (cellValue != null && cellValue != "")
                    return cellValue;
            }

            return null;
        }
        public static double CellDouble(Worksheet sheet, params string[] cellNames)
        {
            for (int i = 0; i < cellNames.Length; i++)
            {
                string column = Regex.Match(cellNames[i], @"[A-Za-z]+").Value;
                int row = int.Parse(Regex.Match(cellNames[i], @"\d+").Value);

                Range cell = (Range)sheet.Cells[row, column];
                var cellValue = cell.Value2;

                if (cellValue != null)
                {
                    if (cellValue is double != true)
                        return ParseCellValue(cellValue);
                    else
                        return cellValue;
                }
            }
            throw new FormatException("The cell value does not contain a valid number.");
        }


        // Private methods
        static double ParseCellValue(string cellValue)
        {
            // Use regular expression to match any number, including decimal and negative
            Match match = Regex.Match(cellValue, @"-?\d+\.?\d*");
            if (match.Success)
            {
                // If a match was found, parse it to double
                return double.Parse(match.Value);
            }
            else
            {
                // If no match was found, throw an exception or return a default value
                throw new FormatException("The cell value does not contain a valid number.");
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


        // Event handlers
        private static void CommonData_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == nameof(CommonData.Bank) || e.PropertyName == nameof(CommonData.Project))
            {
                _pregoDoc = null;
                _inputSheet = null;
            }
        }


        // Backing fields
        static Application _excel;
        static Workbook _pregoDoc;
        static Worksheet _inputSheet;

    }
}










