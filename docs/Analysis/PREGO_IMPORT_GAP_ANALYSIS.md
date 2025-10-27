# ?? PREGO IMPORT GAP ANALYSIS

**Date**: October 27, 2025  
**Issue**: UnifiedUI missing critical Prego import functionality from old UIs  
**Severity**: HIGH - Core feature not implemented

---

## ?? PROBLEM IDENTIFIED

### What the Old UIs Have:
Both `BundleUI` and `HeaderUI` have an **"Import Prego" button** (`bImportPrego`) that:
1. Opens the Prego Excel file from AXC_VAULT
2. Reads specific cell values using `Excel.Prego` system
3. Populates ALL fields with data from Prego

### What UnifiedUI Currently Has:
- ? NO "Import Prego" functionality
- ? NO Excel.Prego system integration
- ??  Basic `ImportFromExcel` that uses `ExcelTemplateImporter` (different system)

---

## ?? OLD UI PREGO IMPORT DETAILS

### Bundle UI - bImportPrego_Click Method

**Location**: `Bundle/BundleUI.cs` lines 189-288

**What It Does**:
```csharp
private void bImportPrego_Click(object sender, EventArgs e)
{
    if (PregoDoc != null) // Excel.Prego.PregoDoc
    {
        // Read Job Info from Prego
        Project = LoadPregoValue<string>(job_Box, InputSheet, "H2");
        Bank = (char)(CellDouble(InputSheet, "C7") + 64);
        Customer = LoadPregoValue<string>(customer_Box, InputSheet, "B2");
        Client = LoadPregoValue<string>(client_Box, InputSheet, "B3");
        PlantLocation = LoadPregoValue<string>(location_Box, InputSheet, "B4");
        PurchaseOrder = LoadPregoValue<string>(purchaseOrder_Box, InputSheet, "B5");
        ItemNumber = LoadPregoValue<string>(itemNumber_Box, InputSheet, "H3");

        // Read Bundle dimensions
        Bundle_Width = LoadPregoDouble(tBundleWidth, InputSheet, "BQ45", "F12");
        TubeLength = LoadPregoDouble_FeetToInches(tTubeLength, InputSheet, "L15", "N15");
        TubeProjection = LoadPregoDouble(tTubeProjection, InputSheet, 0.25, 0.125);
        
        // Read Tube specifications
        TubeOD = LoadPregoDouble(tTubeOD, InputSheet, "L10");
        TubeWallTHK = LoadPregoDouble(tTubeWallTHK, InputSheet, "N14", "L14");
        FinOD = LoadPregoDouble(tFinOD, InputSheet, "N19", "L19");
        
        // Read Tube layout (rows)
        Tube_Row_1L = LoadPregoInt(tTubes_Row_1L, InputSheet, "AW39", "AU39");
        Tube_Row_2L = LoadPregoInt(tTubes_Row_2L, InputSheet, "AW42", "AU42");
        // ... all 10 rows
        
        // Read Vertical Pitches (front)
        FrontVerticalPitch._1_2 = LoadPregoDouble(tFrontVerticalPitch_1_2, SketchCalcsSheet, "DF58");
        FrontVerticalPitch._2_3 = LoadPregoDouble(tFrontVerticalPitch_2_3, SketchCalcsSheet, "DF59");
        // ... all front pitches
        
        // Read Vertical Pitches (rear)
        RearVerticalPitch._1_2 = LoadPregoDouble(tRearVerticalPitch_1_2, SketchCalcsSheet, "DF70");
        // ... all rear pitches
        
        // Read Side Frame
        SideFrame_Depth = LoadPregoDouble(tDepth, InputsCalcsSheet, "BGM26");
        SideFrame_THK = LoadPregoValue<double>(cSideFrameTHK, InputSheet, "CG32", "CF32");
        
        // Import Header data
        ImportHeaderData_FromPrego();
        
        MessageBox.Show("Data imported from Prego successfully");
    }
}
```

**Cell Mappings** (50+ cells read):
| Data | Sheet | Cell(s) | Notes |
|------|-------|---------|-------|
| Project (Job#) | InputSheet | H2 | |
| Bank | InputSheet | C7 | Converts number to letter |
| Customer | InputSheet | B2 | |
| Client | InputSheet | B3 | |
| Location | InputSheet | B4 | |
| PO | InputSheet | B5 | |
| Item# | InputSheet | H3 | |
| Bundle Width | InputSheet | BQ45, F12 | Converts feet to inches if needed |
| Tube Length | InputSheet | L15, N15 | Converts feet to inches |
| Tube OD | InputSheet | L10 | |
| Tube Wall | InputSheet | N14, L14 | |
| Fin OD | InputSheet | N19, L19 | |
| Tube Rows (1-10) | InputSheet | AW39-AW78, AU39-AU78 | 10 rows per side |
| Front VPitch (1-9) | SketchCalcsSheet | DF58-DF66 | 9 vertical pitches |
| Rear VPitch (1-9) | SketchCalcsSheet | DF70-DF78 | 9 vertical pitches |
| Side Frame Depth | InputsCalcsSheet | BGM26 | |
| Side Frame THK | InputSheet | CG32, CF32, CG30, CF30 | |
| Tube Support Spacing | InputsCalcsSheet | BGF12 | |
| Tube Support Qty | InputsCalcsSheet | BGF20 | |
| Tube Support Size | InputSheet | CG28, CF28, CG26, CF26 | |

---

### Header UI - bImportPrego_Click Method

**Location**: `Header/PartialUI/HeaderUI.General.cs` lines 283-303

**What It Does**:
```csharp
private void bImportPrego_Click(object sender, EventArgs e)
{
    if (PregoDoc != null)
    {
        // Automated imports for Double values
        Header_DataManager.ImportHeaderData_FromPrego();
        Connection_DataManager.ImportConnectionData_FromPrego();

        // Manual imports for non-Double values
        ImportHeaderData_FromPrego_Manual();

        CleanUp(); // Excel cleanup

        MessageBox.Show("Data imported from Prego successfully");
    }
}

private void ImportHeaderData_FromPrego_Manual()
{
    // Import boolean flags
    LoadPregoBool(cIsBusted61, InputsCalcsSheet, "AAI45");
    LoadPregoBool(cIsBusted63, InputsCalcsSheet, "AAJ45");
    LoadPregoBool(cIsBusted65, InputsCalcsSheet, "AAK45");
    LoadPregoBool(cIsBusted62, InputsCalcsSheet, "ADC45");
    LoadPregoBool(cIsBusted64, InputsCalcsSheet, "ADD45");
    LoadPregoBool(cIsBusted66, InputsCalcsSheet, "ADE45");

    // Import connection types
    LoadPregoString(cExtensionType_Inlet, InputsCalcsSheet, "CV17");
    LoadPregoString(cExtensionType_Outlet, InputsCalcsSheet, "DX18");

    // Import titleblock info
    LoadPregoString(cTileblockManuf, InputsCalcsSheet, "R49");
    LoadPregoBool(cHeadersOutsideFrames, InputsCalcsSheet, "R41");

    // Import part numbers
    ImportPartStiffPartNumbers();
    ImportConnectionPartNumbers();
}
```

**Additional Imports**:
- `Header_DataManager.ImportHeaderData_FromPrego()` - Imports 100+ header dimensions
- `Connection_DataManager.ImportConnectionData_FromPrego()` - Imports flange/connection data

---

## ?? EXCEL.PREGO SYSTEM

### Key Classes

#### 1. Excel.Prego (Prego.cs)
**Location**: `Excel/Prego.cs`

**Static Properties**:
```csharp
public static Workbook PregoDoc { get; } // Main Prego workbook
public static Worksheet InputSheet { get; } // Main input sheet
public static Worksheet InputsCalcsSheet { get; } // Calculations sheet
public static Worksheet SketchCalcsSheet { get; } // Sketch calcs sheet
public static Worksheet PregoToMikeySheet { get; } // BOM sheet
public static Worksheet BomInputSheet { get; } // BOM input sheet
```

**Key Methods**:
```csharp
public static void CleanUp() // Release COM objects
```

**File Location Logic**:
```csharp
string expectedFolder = $@"C:\AXC_VAULT\Active\{Project}\Drafting\Headers\~Archive\";
string expectedFileName = $"{Project}-prego{Bank - 'A' + 1}.xlsm";
string expectedFilePath = Path.Combine(expectedFolder, expectedFileName);
```

#### 2. Excel.StaticHelpers (StaticHelpers.cs)
**Location**: `Excel/StaticHelpers.cs`

**Helper Methods**:
```csharp
public static double CellDouble(Worksheet sheet, string cellAddress)
public static string GetValueFromExcel(Worksheet sheet, string cellAddress)
public static T LoadPregoValue<T>(Control control, Worksheet sheet, params string[] cellAddresses)
public static double LoadPregoDouble(TextBox textBox, Worksheet sheet, params string[] cellAddresses)
public static double LoadPregoDouble_FeetToInches(TextBox textBox, Worksheet sheet, params string[] cellAddresses)
public static int LoadPregoInt(TextBox textBox, Worksheet sheet, params string[] cellAddresses)
public static bool LoadPregoBool(CheckBox checkBox, Worksheet sheet, params string[] cellAddresses)
public static void LoadPregoString(ComboBox comboBox, Worksheet sheet, params string[] cellAddresses)
```

---

## ? WHAT'S MISSING IN UNIFIEDUI

### Current UnifiedUI Import:
```csharp
public void ImportFromExcel(string filePath)
{
    // Uses ExcelTemplateImporter - different system!
    var componentType = _excelTemplateImporter.DetectComponentType(filePath);
    
    if (componentType == "Bundle")
    {
        var bundleConfig = _excelTemplateImporter.ImportBundleFromExcel(filePath);
        // ... updates config
    }
}
```

**Problems**:
1. ? Doesn't use `Excel.Prego` system
2. ? Doesn't read from specific Prego cells
3. ? Doesn't auto-locate Prego file from AXC_VAULT
4. ? Doesn't import Header data
5. ? Doesn't import all 50+ Bundle fields
6. ? No "Import Prego" button in UI

---

## ? WHAT NEEDS TO BE ADDED

### 1. Add Prego Import to UnifiedUI ViewModel

**File**: `UnifiedUI/ViewModels/MainViewModel.cs`

**New Method**:
```csharp
/// <summary>
/// Import from Prego Excel file (replicates old UI "Import Prego" button)
/// </summary>
public void ImportFromPrego()
{
    try
    {
        if (Excel.Prego.PregoDoc != null)
        {
            // Import based on active tab
            if (CurrentConfiguration is BundleConfiguration bundleConfig)
            {
                ImportBundleFromPrego(bundleConfig);
            }
            else if (CurrentConfiguration is HeaderConfiguration headerConfig)
            {
                ImportHeaderFromPrego(headerConfig);
            }
            
            MessageBox.Show("Data imported from Prego successfully!");
        }
        else
        {
            MessageBox.Show("Prego file not found. Please ensure it's accessible.");
        }
    }
    catch (Exception ex)
    {
        GlobalErrorHandler.LogError(ex, "ImportFromPrego");
        throw;
    }
}

private void ImportBundleFromPrego(BundleConfiguration config)
{
    // Read Job Info
    GlobalJobNumber = Excel.StaticHelpers.GetValueFromExcel(
        Excel.Prego.InputSheet, "H2");
    
    // Read Bundle dimensions
    BundleWidth = Excel.StaticHelpers.CellDouble(
        Excel.Prego.InputSheet, "BQ45");
    if (BundleWidth < 16) BundleWidth *= 12; // Convert feet to inches
    
    SideFrameDepth = Excel.StaticHelpers.CellDouble(
        Excel.Prego.InputsCalcsSheet, "BGM26");
    
    // Read all 50+ Prego cells...
    // (Copy from BundleUI.bImportPrego_Click)
}

private void ImportHeaderFromPrego(HeaderConfiguration config)
{
    // Call existing Header import logic
    // Header_DataManager.ImportHeaderData_FromPrego();
    // Connection_DataManager.ImportConnectionData_FromPrego();
}
```

### 2. Add "Import Prego" Button to UI

**File**: `UnifiedUI/MainWindow.xaml`

**Add Toolbar Button**:
```xml
<Button Name="ImportPregoButton" Click="ImportPregoButton_Click" 
        ToolTip="Import from Prego Excel">
    <StackPanel Orientation="Horizontal">
        <TextBlock Text="??" FontSize="16" Margin="0,0,5,0"/>
        <TextBlock Text="Import Prego"/>
    </StackPanel>
</Button>
```

**Add Event Handler** (`MainWindow.xaml.cs`):
```csharp
private void ImportPregoButton_Click(object sender, RoutedEventArgs e)
{
    try
    {
        _viewModel.ImportFromPrego();
    }
    catch (Exception ex)
    {
        MessageBox.Show($"Error importing from Prego:\n\n{ex.Message}",
            "Import Error", MessageBoxButton.OK, MessageBoxImage.Error);
    }
}
```

### 3. Update Menu Item

**File**: `UnifiedUI/MainWindow.xaml`

**Change**:
```xml
<!-- OLD -->
<MenuItem Header="Import Excel..." Click="ImportButton_Click"/>

<!-- NEW -->
<MenuItem Header="Import from Prego..." Click="ImportPregoButton_Click"/>
<MenuItem Header="Import from File..." Click="ImportButton_Click"/>
```

---

## ?? IMPLEMENTATION PRIORITY

### HIGH Priority (Critical):
1. ? Add `ImportFromPrego()` method to MainViewModel
2. ? Add `ImportBundleFromPrego()` with all 50+ cell mappings
3. ? Add "Import Prego" button to toolbar
4. ? Test with real Prego file

### MEDIUM Priority (Important):
5. ??  Add `ImportHeaderFromPrego()` with Header_DataManager integration
6. ??  Add import for other components (Hood, Walkway, etc.)
7. ??  Add error handling for missing Prego file

### LOW Priority (Nice to Have):
8. ??  Add "Recent Prego Files" menu
9. ??  Add Prego file validation
10. ??  Add import preview before applying

---

## ?? RECOMMENDATION

**IMMEDIATE ACTION REQUIRED**:
1. Add proper Prego import functionality to UnifiedUI
2. Copy cell mapping logic from old UIs
3. Add "Import Prego" button
4. Test with real Prego files

**WHY THIS MATTERS**:
- Prego import is a **core workflow** in the old UIs
- Users expect to import data from Prego Excel files
- Without it, UnifiedUI is missing critical functionality
- All 7 components need Prego import support

---

## ?? CELL MAPPING REFERENCE

### Bundle - Complete Cell Map

| Field | Sheet | Primary Cell | Fallback Cell(s) | Notes |
|-------|-------|--------------|------------------|-------|
| Job Number | InputSheet | H2 | | |
| Bank | InputSheet | C7 | | Convert number to char |
| Customer | InputSheet | B2 | | |
| Client | InputSheet | B3 | | |
| Location | InputSheet | B4 | | |
| PO | InputSheet | B5 | | |
| Item Number | InputSheet | H3 | | |
| Bundle Width | InputSheet | BQ45 | F12 | Convert ft?in if < 16 |
| Tube Length | InputSheet | L15 | N15 | Convert ft?in |
| Tube Projection | InputSheet | 0.25 | 0.125 | Default values |
| Tube OD | InputSheet | L10 | | |
| Tube Wall | InputSheet | N14 | L14 | |
| Fin OD | InputSheet | N19 | L19 | |
| Tube Row 1L | InputSheet | AW39 | AU39 | |
| Tube Row 2L | InputSheet | AW42 | AU42 | |
| ... (rows 3-10) | InputSheet | AW45-AW78 | AU45-AU78 | |
| Front VPitch 1-2 | SketchCalcsSheet | DF58 | | |
| Front VPitch 2-3 | SketchCalcsSheet | DF59 | | |
| ... (pitches 3-9) | SketchCalcsSheet | DF60-DF66 | | |
| Rear VPitch 1-2 | SketchCalcsSheet | DF70 | | |
| ... (pitches 2-9) | SketchCalcsSheet | DF71-DF78 | | |
| Side Frame Depth | InputsCalcsSheet | BGM26 | | |
| Side Frame THK | InputSheet | CG32 | CF32, CG30, CF30 | |
| Tube Support Spacing | InputsCalcsSheet | BGF12 | | Feet |
| Tube Support Qty | InputsCalcsSheet | BGF20 | | |
| Tube Support Size | InputSheet | CG28 | CF28, CG26, CF26 | Parse size |

**Total**: 50+ cells for Bundle alone

---

## ?? CONCLUSION

**The Prego import system is CRITICAL and currently MISSING from UnifiedUI.**

UnifiedUI won't be production-ready until this is implemented. Users rely on this workflow daily.

**Status**: ?? **GAP IDENTIFIED - NEEDS IMPLEMENTATION**

