using Excel;
using Microsoft.Office.Interop.Excel;
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using SplashScreen;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using static Excel.Prego;
using static FileTools.StaticFileTools;
using static ModelTools.ReleaseCOM;
using static FileTools.CommonData.CommonData;

namespace HDR.Drawings
{
    public class TitleBlock
    {
        // Public methods
        public static void ImportTitleBlockInfo()
        {
            // TryGet active drawing document
            ModelDoc2 slddrw = GetActiveDoc(); if (slddrw == null) return;

            // Get Prego version if supported 
            string version = GetCurrentOrDefaultVersion();
            PleaseWait.Show("NOW LOADING!");

            // Add/edit custom properties loop
            if (version != null)
                SetCustomDrawingProperties(version);
            slddrw.ForceRebuild3(false);

            // Release resources
            CleanUp(slddrw);

            // Notify the user
            if (version != null)
                MessageBox.Show(
                    $"Successfully imported data from {Path.GetFileNameWithoutExtension(Prego.FilePath)}",
                    "Import Successful", MessageBoxButtons.OK, MessageBoxIcon.Information);
            else
                MessageBox.Show(
                    $"Could not import Prego data.",
                    "Import Unsuccessful", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }


        // Private methods
        static string GetValueFromExcel(Worksheet sheet, string cellName)
        {
            string column = Regex.Match(cellName, @"[A-Za-z]+").Value;
            int row = int.Parse(Regex.Match(cellName, @"\d+").Value);

            sheet.Activate();
            Range cell = (Range)sheet.Cells[row, column];


            if (cell.Value2 == null) return "";
            else return cell.Value2.ToString();
        }
        static void SetCustomTextProperty(string name, string value)
        {
            int result = _customPropertyManager.Add3(name, (int)swCustomInfoType_e.swCustomInfoText, value, (int)swCustomPropertyAddOption_e.swCustomPropertyReplaceValue);

            // Interpret the result code
            string message;
            switch (result)
            {
                case (int)swCustomInfoAddResult_e.swCustomInfoAddResult_AddedOrChanged:
                    message = "Success: The custom property was added or updated.";
                    break;
                case (int)swCustomInfoAddResult_e.swCustomInfoAddResult_GenericFail:
                    message = $"Error: Failed to add the custom property {name}.";
                    break;
                case (int)swCustomInfoAddResult_e.swCustomInfoAddResult_MismatchAgainstExistingType:
                    message = $"Error: Existing custom property named {name} has a different type.";
                    break;
                case (int)swCustomInfoAddResult_e.swCustomInfoAddResult_MismatchAgainstSpecifiedType:
                    message = $"Error: Specified value {value} of the custom property {name} does not match the specified type.";
                    break;
                default:
                    message = "Unknown error occurred.";
                    break;
            }

            if (result != (int)swCustomInfoAddResult_e.swCustomInfoAddResult_AddedOrChanged)
            {
                MessageBox.Show(message, "Custom Property Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
        static ModelDoc2 GetActiveDoc()
        {
            // Get active document
            ModelDoc2 modelDoc2 = SW.IActiveDoc2;

            // Check if null or not a drawing
            if (modelDoc2 == null || modelDoc2.GetType() != (int)swDocumentTypes_e.swDocDRAWING)
            {
                MessageBox.Show("Please open a Solidworks drawing document and try again.", "SLDDRW Not Found", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return null;
            }
            else _customPropertyManager = modelDoc2.Extension.CustomPropertyManager[""];

            return modelDoc2;
        }
        static string GetCurrentOrDefaultVersion()
        {
            // Initialize Prego and get current Prego version
            var load = InputSheet; if (load == null) return null;
            string version = GetValueFromExcel(InputSheet, "J2");

            // Set version to default if not found
            if (!VersionedExcelDataMap.ContainsKey(version))
            {
                MessageBox.Show(
                    $"The current version of Prego, {version}, is not directly supported by this application." + "\n" +
                    "Unexpected results possible",
                    "Version Not Found", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                version = _defaultVersion;
            }

            return version;
        }
        static void SetCustomDrawingProperties(string version)
        {
            // Dictionary loop
            foreach (var dataMap in VersionedExcelDataMap[version])
            {
                string property = dataMap.Key;
                string cellName = dataMap.Value.CellName;
                Worksheet sheet = dataMap.Value.Sheet;

                string value = GetValueFromExcel(sheet, cellName);

                SetCustomTextProperty(property, value);
            }

            // Exceptions
            Bank = GetValueFromExcel(InputsCalcsSheet, "BGV7")[0];
            SetCustomTextProperty("DWGPartNo", $"6{Bank}");
            SetCustomTextProperty("DrawnBy", Initials);
            SetCustomTextProperty("DrawnDate", DateTime.Now.ToString("MM/dd/yyyy"));

            // Adjustments
            string coatSuffix = "shop coat(s) at";
            TryAddSuffix("PrimerCoats", coatSuffix, "Primer");
            TryAddSuffix("IntermediateCoats", coatSuffix, "Intermediate");
            TryAddSuffix("FinishCoats", coatSuffix, "Finish");
            string dftSuffix = "DFT";
            TryAddSuffix("PrimerDFT", dftSuffix, "Primer");
            TryAddSuffix("IntermediateDFT", dftSuffix, "Intermediate");
            TryAddSuffix("FinishDFT", dftSuffix, "Finish");
        }
        static void TryAddSuffix(string targetPropertyName, string suffix, string parentPropertyName)
        {
            _customPropertyManager.Get6(parentPropertyName, false, out string parentValue, out _, out _, out _);
            if (parentValue != "")
                SetCustomTextProperty(targetPropertyName, parentValue + $" {suffix}");
            else
                SetCustomTextProperty(targetPropertyName, "");
        }
        static void CleanUp(ModelDoc2 slddrw)
        {
            try { PleaseWait.Stop(); } catch (Exception) { }
            Release(ref slddrw);
            Release(ref _customPropertyManager);
            Prego.CleanUp();
            _versionedExcelDataMap = null;
        }


        // Private fields
        static CustomPropertyManager _customPropertyManager;
        static Dictionary<string, Dictionary<string, (string CellName, Worksheet Sheet)>> _versionedExcelDataMap;
        static string _defaultVersion = "V3.3.2";


        // Excel data mapping
        public static Dictionary<string, Dictionary<string, (string CellName, Worksheet Sheet)>> VersionedExcelDataMap
        {
            get
            {
                if (_versionedExcelDataMap == null)
                {
                    _versionedExcelDataMap = new Dictionary<string, Dictionary<string, (string CellName, Worksheet Sheet)>>
                    {
                        { "V3.3.2", new Dictionary<string, (string CellName, Worksheet Sheet)>
                        {
                            {          "TubesPerPass1" , ( "AGR45", InputsCalcsSheet  ) }, 
                            {          "TubesPerPass2" , ( "AGS45", InputsCalcsSheet  ) }, 
                            {          "TubesPerPass3" , ( "AGT45", InputsCalcsSheet  ) }, 
                            {          "TubesPerPass4" , ( "AGU45", InputsCalcsSheet  ) },
                            {          "TubesPerPass5" , ( "AGV45", InputsCalcsSheet  ) },
                            {          "TubesPerPass6" , ( "AGW45", InputsCalcsSheet  ) },
                            {          "TubesPerPass7" , ( "AGX45", InputsCalcsSheet  ) },
                            {          "TubesPerPass8" , ( "AGY45", InputsCalcsSheet  ) },
                            {          "TubesPerPass9" , ( "AGZ45", InputsCalcsSheet  ) },
                            {         "TubesPerPass10" , ( "AHA45", InputsCalcsSheet  ) },
                            {         "TubesPerPass11" , ( "AHB45", InputsCalcsSheet  ) },
                            {         "TubesPerPass12" , ( "AHC45", InputsCalcsSheet  ) },
                            {         "TubesPerPass13" , ( "AHD45", InputsCalcsSheet  ) },
                            {         "TubesPerPass14" , ( "AHE45", InputsCalcsSheet  ) },
                            {         "TubesPerPass15" , ( "AHF45", InputsCalcsSheet  ) },
                            {         "TubesPerPass16" , ( "AHG45", InputsCalcsSheet  ) },
                            {                "FinType" , ( "AQ42" , InputsCalcsSheet  ) },
                            {            "FinsPerInch" , ( "BB47" , InputsCalcsSheet  ) },
                            {      "FinStripBackInlet" , ( "X39"  , PregoToMikeySheet ) },
                            {      "FinStripBackOther" , ( "Y39"  , PregoToMikeySheet ) },
                            { "TubeLength_decimalFeet" , ( "AY40" , InputsCalcsSheet  ) },
                            {                 "TubeOD" , ( "AQ39" , InputsCalcsSheet  ) },
                            {                  "FinOD" , ( "BB20" , InputsCalcsSheet  ) },
                            {            "BundleCount" , ( "BGV8" , InputsCalcsSheet  ) },
                            {              "PassCount" , ( "D48"  , InputsCalcsSheet  ) },
                            {               "RowCount" , ( "BB16" , InputsCalcsSheet  ) },
                            {              "TubeCount" , ( "BB43" , InputsCalcsSheet  ) },
                            {             "TubePitchX" , ( "BB6"  , InputsCalcsSheet  ) },
                            {             "TubePitchY" , ( "BB11" , InputsCalcsSheet  ) },
                            {             "Radiograph" , ( "X34"  , PregoToMikeySheet ) },
                            {          "HeatTreatment" , ( "R71"  , InputsCalcsSheet  ) },
                            {      "TubeWallReduction" , ( "BGV59", InputsCalcsSheet  ) },
                            {            "TubeEndWeld" , ( "X35"  , PregoToMikeySheet ) },
                            {           "TestPressure" , ( "P139" , PregoToMikeySheet ) },
                            {    "Corrosion Allowance" , ( "C12"  , InputsCalcsSheet  ) },
                            {                   "ASME" , ( "X31"  , PregoToMikeySheet ) },
                            {                "ApiYear" , ( "C9"   , InputsCalcsSheet  ) },
                            {                "Service" , ( "P5"   , PregoToMikeySheet ) },
                            {         "DesignPressure" , ( "P14"  , PregoToMikeySheet ) },
                            {                   "MAWP" , ( "P101" , PregoToMikeySheet ) },
                            {                   "MDMT" , ( "AB18" , InputsCalcsSheet  ) },
                            {               "Customer" , ( "P4"   , PregoToMikeySheet ) },
                            {      "Customer Location" , ( "P6"   , PregoToMikeySheet ) },
                            {                   "Bank" , ( "BGV7" , InputsCalcsSheet  ) },
                            {                   "Item" , ( "P10"  , PregoToMikeySheet ) },
                            {         "Purchase Order" , ( "B5"   , InputSheet        ) },
                            {                 "Primer" , ( "BW48" , InputSheet        ) },
                            {             "PrimerName" , ( "BW49" , InputSheet        ) },
                            {            "PrimerColor" , ( "BW50" , InputSheet        ) },
                            {              "PrimerDFT" , ( "BDX47", InputsCalcsSheet  ) },
                            {            "PrimerCoats" , ( "BDX48", InputsCalcsSheet  ) },
                            {           "Intermediate" , ( "BW54" , InputSheet        ) },
                            {       "IntermediateName" , ( "BW55" , InputSheet        ) },
                            {      "IntermediateColor" , ( "BW56" , InputSheet        ) },
                            {        "IntermediateDFT" , ( "BDX49", InputsCalcsSheet  ) },
                            {      "IntermediateCoats" , ( "BDX50", InputsCalcsSheet  ) },
                            {                 "Finish" , ( "BW60" , InputSheet        ) },
                            {             "FinishName" , ( "BW61" , InputSheet        ) },
                            {            "FinishColor" , ( "BW62" , InputSheet        ) },
                            {              "FinishDFT" , ( "BDX51", InputsCalcsSheet  ) },
                            {            "FinishCoats" , ( "BDX52", InputsCalcsSheet  ) },
                            {              "JobNumber" , ( "H2"   , InputSheet        ) },
                            {             "DesignTemp" , ( "X22"  , PregoToMikeySheet ) },
                            {               "MawpTemp" , ( "X25"  , PregoToMikeySheet ) },
                            {         "TubeEndCoating" , ( "BW1"  , InputSheet        ) },
                        }
                        },
                    };
                }
                return _versionedExcelDataMap;
            }
        }
    }
}
