using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace DrawingToolz
{
    public class DrawingFileManager
    {
        private static SldWorks SW = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");

        //[STAThread]
        //static void Main()
        //{
        //    //SplitActiveDrawing();
        //    AddToActiveDrawing();
        //}
        public static void SplitActiveDrawing()
        {
            ModelDoc2 sourceModelDoc2 = SW.IActiveDoc2;
            DrawingDoc sourceDrawingDoc = sourceModelDoc2 as DrawingDoc;
            string jobNumber = GetJobNumber(Path.GetFileNameWithoutExtension(sourceModelDoc2.GetPathName()), out char delimiter);
            string sourceFilePath = sourceModelDoc2.GetPathName();
            string destinationFolder;

            try
            {
                destinationFolder = Path.GetDirectoryName(sourceFilePath);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Save the drawing file to your desktop." + "\n\n" + $"{ex.Message}",
                                "No File Path Assigned", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return; 
            }

            if (sourceFilePath.Contains("AXC_VAULT") || destinationFolder.Contains("AXC_VAULT"))
            {
                MessageBox.Show("You can not split drawing files saved to the vault. Save a copy on your desktop before running this command.",
                                "Vault File", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return;
            }


            string[] sourceSheetNames = sourceDrawingDoc.GetSheetNames();
            foreach (var sourceSheetName in sourceSheetNames)
            {
                // Create and open new file for single sheet
                string destinationFilePath = destinationFolder + @"\" + jobNumber + delimiter + sourceSheetName + ".SLDDRW";
                try
                {
                    File.Copy(sourceFilePath, destinationFilePath);
                }
                catch (Exception)
                {
                    destinationFilePath = destinationFolder + @"\" + sourceSheetName + ".SLDDRW";
                    File.Copy(sourceFilePath, destinationFilePath);
                }

                ModelDoc2 destinationModelDoc2 = OpenDrawing(destinationFilePath);

                // Remove all sheets except one
                string[] destinationSheetNames = (destinationModelDoc2 as DrawingDoc).GetSheetNames();
                foreach (var destinationSheetName in destinationSheetNames)
                {
                    if (destinationSheetName != sourceSheetName)
                    {
                        destinationModelDoc2.Extension.SelectByID2(destinationSheetName, "SHEET", 0, 0, 0, true, 0, null, 0);
                    }
                }
                destinationModelDoc2.Extension.DeleteSelection2(0);
                destinationModelDoc2.Save3(1, 0, 0);
                SW.CloseDoc(destinationFilePath);

                // Remove copied sheet from source
                sourceModelDoc2.Extension.SelectByID2(sourceSheetName, "SHEET", 0, 0, 0, false, 0, null, 0);
                sourceModelDoc2.Extension.DeleteSelection2(0);
            }
            SW.CloseDoc(sourceFilePath);
            File.Delete(sourceFilePath);

        }
        public static void AddToActiveDrawing()
        {
            string baseDrawing = SW.IActiveDoc2.GetPathName();

            if (baseDrawing.Contains("AXC_VAULT"))
            {
                MessageBox.Show("You can not merge drawing files saved to the vault. Save a copy on your desktop before running this command.",
                                "Vault File", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                return;
            }

            OpenFileDialog openFileDialog = new OpenFileDialog
            {
                Multiselect = true,
                Filter = "SolidWorks Drawings (*.slddrw)|*.slddrw",
                Title = "Select Drawings to Add",
                InitialDirectory = Path.GetDirectoryName(baseDrawing)
            };

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                string jobNumber = GetJobNumber(Path.GetFileNameWithoutExtension(baseDrawing), out _);
                string folderPath = Path.GetDirectoryName(baseDrawing);
                ModelDoc2 mergedDoc = OpenDrawing(baseDrawing);

                // Dictionary to hold original sheet names and their new names
                Dictionary<string, string> sheetNamesMap = new Dictionary<string, string>();

                foreach (string fileName in openFileDialog.FileNames)
                {
                    if (fileName == baseDrawing) { continue; }

                    if (fileName.Contains("AXC_VAULT"))
                    {
                        MessageBox.Show("You can not merge drawing files saved to the vault. Save a copy on your desktop before running this command.",
                                        "Vault File", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                        return;
                    }

                    ModelDoc2 sheetDoc = OpenDrawing(fileName);
                    DrawingDoc sheetDwg = sheetDoc as DrawingDoc;

                    // Get the names of all sheets in the current document
                    string[] sheetNames = sheetDwg.GetSheetNames();

                    foreach (string sheetName in sheetNames)
                    {
                        // Check if the sheet name is already in the map to avoid duplicates in case of same sheet names across drawings
                        if (sheetNamesMap.ContainsKey(sheetName))
                        {
                            // Handle the case where the sheet name already exists (you could rename it or skip copying it)
                            // For now, let's just add a suffix to make it unique
                            sheetNamesMap[sheetName] = sheetName + "_copy";
                        }
                        else
                        {
                            sheetNamesMap[sheetName] = null;
                        }

                        // Select and copy the sheet
                        sheetDwg.ActivateSheet(sheetName);
                        sheetDoc.Extension.SelectByID2(sheetName, "SHEET", 0, 0, 0, false, 0, null, 0);
                        sheetDoc.EditCopy();

                        // Open and paste into the merged drawing
                        mergedDoc = OpenDrawing(baseDrawing);
                        mergedDoc.Paste();
                    }

                    // Close and delete the source document after its sheets have been copied
                    SW.CloseDoc(fileName);
                    File.Delete(fileName);
                }

                // After all sheets have been pasted, rename them if necessary
                foreach (var originalSheetName in sheetNamesMap.Keys.ToList())
                {
                    string newSheetName = FindPastedSheetName(mergedDoc, originalSheetName);
                    if (newSheetName != originalSheetName)
                    {
                        RenameSheet(mergedDoc, newSheetName, originalSheetName);
                    }
                }
                SortSheetsInDrawing();
                mergedDoc.Save3((int)swSaveAsOptions_e.swSaveAsOptions_Silent, 0, 0);
            }
        }

        public static void SortSheetsInDrawing()
        {
            DrawingDoc activeDrawing = SW.IActiveDoc2 as DrawingDoc;

            if (activeDrawing == null) return; // Exit if the active document is not a drawing.

            // 1. Retrieve all sheet names
            string[] allSheetNames = activeDrawing.GetSheetNames();

            // 2. Sort the sheet names
            var sortedSheetNames = allSheetNames
                .OrderBy(sheet => sheet.Equals("ReadMe") ? 0 : 1)
                .ThenBy(sheet =>
                {
                    char lastChar = sheet.Last();
                    if (char.IsDigit(lastChar))
                        return "0" + lastChar;
                    return "1" + lastChar;
                })
                .ToArray();

            // 3. Use ReorderSheets to rearrange the sheets based on the sorted names
            activeDrawing.ReorderSheets(sortedSheetNames);

            activeDrawing.ActivateSheet(allSheetNames[0]);
        }
        private static void RenameSheet(ModelDoc2 doc, string oldName, string newName)
        {
            // Assuming doc is a DrawingDoc and oldName, newName are the sheet names
            DrawingDoc drawing = doc as DrawingDoc;
            bool result = drawing.ActivateSheet(oldName);
            if (result)
            {
                // If the sheet was activated successfully, rename it
                Sheet sheet = drawing.GetCurrentSheet();
                sheet.SetName(newName);
            }
            // Consider adding error handling if renaming fails
        }
        private static string FindPastedSheetName(ModelDoc2 doc, string originalName)
        {
            DrawingDoc drawing = doc as DrawingDoc;
            string[] sheetNames = drawing.GetSheetNames();
            foreach (string sheetName in sheetNames)
            {
                if (sheetName.StartsWith(originalName))
                {
                    return sheetName; // Found a sheet with the original name, possibly with a suffix
                }
            }
            return originalName; // If not found, return the original name
        }
        private static dynamic OpenDrawing(string filePath, bool asDrawingDoc = false)
        {
            ModelDoc2 modelDoc2 = SW.OpenDoc6
                    (
                    filePath,
                    (int)swDocumentTypes_e.swDocDRAWING,
                    (int)swOpenDocOptions_e.swOpenDocOptions_Silent,
                    null,
                    0,
                    0
                    );

            if (asDrawingDoc)
            {
                return modelDoc2 as DrawingDoc;
            }
            else
            {
                return modelDoc2;
            }
        }
        static string GetJobNumber(string fileName, out char delimiter)
        {
            delimiter = '\0';

            int separatorIndex = fileName.IndexOfAny(new char[] { '-', '_' });

            if (separatorIndex >= 0)
            {
                delimiter = fileName[separatorIndex];
                return fileName.Substring(0, separatorIndex);
            }
            else
            {
                return fileName;
            }
        }

    }
}
