# ?? PREGO EXCEL MAPPING - Bundle to UnifiedUI

## Overview
This document maps ALL Excel Prego cells used by the original BundleUI to their corresponding properties in UnifiedUI.

**Source:** `Bundle/BundleUI.cs` ? `bImportPrego_Click()` method
**Destination:** `UnifiedUI/ViewModels/MainViewModel.cs` ? `ImportBundleDataFromPrego()` method

---

## ?? CELL MAPPING TABLE

### Job Information
| Property | Excel Sheet | Cell(s) | Method | Notes |
|----------|------------|---------|--------|-------|
| **Project** (Job Number) | InputSheet | H2 | `LoadPregoValue<string>` | Primary job identifier |
| **Bank** | InputSheet | C7 | `CellDouble` | Convert: `(char)(value + 64)` ? 1='A', 2='B' |
| **Customer** | InputSheet | B2 | `LoadPregoValue<string>` | |
| **Client** | InputSheet | B2 | `LoadPregoValue<string>` | Same as Customer |
| **PlantLocation** | InputSheet | B4 | `LoadPregoValue<string>` | |
| **PurchaseOrder** | InputSheet | B5 | `LoadPregoValue<string>` | |
| **ItemNumber** | InputSheet | H3 | `LoadPregoValue<string>` | |
| **TitleblockManuf** | InputSheet | G27, F27 | `LoadPregoValue<string>` | Fallback cells |

---

### Bundle Dimensions
| Property | Excel Sheet | Cell(s) | Method | Notes |
|----------|------------|---------|--------|-------|
| **Bundle_Width** | InputSheet | BQ45, F12 | `LoadPregoDouble` | Convert if < 16: `value *= 12` (feet?inches) |
| **SideFrame_Depth** | InputsCalcsSheet | BGM26 | `LoadPregoDouble` | Side frame depth |
| **SideFrame_THK** | InputSheet | CG32, CF32, CG30, CF30 | `LoadPregoValue<double>` | 4 fallback cells |
| **HeadersOutsideFrames** | InputSheet | G15, F15 | `LoadPregoBool` | Boolean checkbox |

---

### Tube Configuration
| Property | Excel Sheet | Cell(s) | Method | Notes |
|----------|------------|---------|--------|-------|
| **TubeLength** | InputSheet | L15, N15 | `LoadPregoDouble_FeetToInches` | Architectural feet (5'-6") or decimal feet |
| **TubeProjection** | *(hardcoded)* | N/A | `LoadPregoDouble` | 0.25 (Smithco) or 0.125 (HPC) |
| **TubeOD** | InputSheet | L10 | `LoadPregoDouble` | Tube outer diameter |
| **TubeWallTHK** | InputSheet | N14, L14 | `LoadPregoDouble` | Tube wall thickness |
| **FinOD** | InputSheet | N19, L19 | `LoadPregoDouble` | Fin outer diameter |
| **Tube_Row_1L** | InputSheet | AW39, AU39 | `LoadPregoInt` | Row 1 tube count (left) |
| **Tube_Row_2L** | InputSheet | AW42, AU42 | `LoadPregoInt` | Row 2 tube count (left) |
| **TubeHorizPitch** | InputSheet | BO47 | `LoadPregoDouble` | Horizontal pitch between tubes |
| **TubeQuantity** | InputSheet | N20, L20 | `LoadPregoInt` | Total tube count |

---

### Fin Strip Back
| Property | Excel Sheet | Cell(s) | Method | Notes |
|----------|------------|---------|--------|-------|
| **FinStripBack_Front** | PregoToMikeySheet | X39 | `LoadPregoDouble` | Front fin strip back distance |
| **FinStripBack_Rear** | PregoToMikeySheet | Y39 | `LoadPregoDouble` | Rear fin strip back distance |

---

### Vertical Pitch - FRONT (9 rows)
| Property | Excel Sheet | Cell | Method | Notes |
|----------|------------|------|--------|-------|
| **FrontVerticalPitch._1_2** | SketchCalcsSheet | DF58 | `LoadPregoDouble` | Row 1 to 2 |
| **FrontVerticalPitch._2_3** | SketchCalcsSheet | DF59 | `LoadPregoDouble` | Row 2 to 3 |
| **FrontVerticalPitch._3_4** | SketchCalcsSheet | DF60 | `LoadPregoDouble` | Row 3 to 4 |
| **FrontVerticalPitch._4_5** | SketchCalcsSheet | DF61 | `LoadPregoDouble` | Row 4 to 5 |
| **FrontVerticalPitch._5_6** | SketchCalcsSheet | DF62 | `LoadPregoDouble` | Row 5 to 6 |
| **FrontVerticalPitch._6_7** | SketchCalcsSheet | DF63 | `LoadPregoDouble` | Row 6 to 7 |
| **FrontVerticalPitch._7_8** | SketchCalcsSheet | DF64 | `LoadPregoDouble` | Row 7 to 8 |
| **FrontVerticalPitch._8_9** | SketchCalcsSheet | DF65 | `LoadPregoDouble` | Row 8 to 9 |
| **FrontVerticalPitch._9_10** | SketchCalcsSheet | DF66 | `LoadPregoDouble` | Row 9 to 10 |

---

### Vertical Pitch - REAR (9 rows)
| Property | Excel Sheet | Cell | Method | Notes |
|----------|------------|------|--------|-------|
| **RearVerticalPitch._1_2** | SketchCalcsSheet | DF70 | `LoadPregoDouble` | Row 1 to 2 |
| **RearVerticalPitch._2_3** | SketchCalcsSheet | DF71 | `LoadPregoDouble` | Row 2 to 3 |
| **RearVerticalPitch._3_4** | SketchCalcsSheet | DF72 | `LoadPregoDouble` | Row 3 to 4 |
| **RearVerticalPitch._4_5** | SketchCalcsSheet | DF73 | `LoadPregoDouble` | Row 4 to 5 |
| **RearVerticalPitch._5_6** | SketchCalcsSheet | DF74 | `LoadPregoDouble` | Row 5 to 6 |
| **RearVerticalPitch._6_7** | SketchCalcsSheet | DF75 | `LoadPregoDouble` | Row 6 to 7 |
| **RearVerticalPitch._7_8** | SketchCalcsSheet | DF76 | `LoadPregoDouble` | Row 7 to 8 |
| **RearVerticalPitch._8_9** | SketchCalcsSheet | DF77 | `LoadPregoDouble` | Row 8 to 9 |
| **RearVerticalPitch._9_10** | SketchCalcsSheet | DF78 | `LoadPregoDouble` | Row 9 to 10 |

---

### Tube Supports
| Property | Excel Sheet | Cell(s) | Method | Notes |
|----------|------------|---------|--------|-------|
| **TubeSupportSpacing_Feet** | InputsCalcsSheet | BGF12 | `LoadPregoDouble` | Spacing in feet |
| **TubeSupportQuantity** | InputsCalcsSheet | BGF20 | `LoadPregoInt` | Number of supports |
| **TubeSupportSize** | InputSheet | CG28, CF28, CG26, CF26 | `LoadPregoValue<string>` | Extract first word: `.Split(' ')[0]` |

---

### Camber Logic
| Property | Condition | Notes |
|----------|-----------|-------|
| **Cambered** | `!IsSmithco && Tube.SlopesPerFootList[Tube.RowCount] == 0` | Auto-set to `true` if not Smithco and no slopes |

---

## ?? EXCEL SHEET REFERENCES

The old BundleUI uses these Excel worksheet references from `Excel.Prego`:

```csharp
using static Excel.Prego;

// Available worksheets:
PregoDoc              // The workbook
InputSheet            // Main input sheet
SketchCalcsSheet      // Sketch calculations  
InputsCalcsSheet      // Inputs & calculations
PregoToMikeySheet     // Prego to Mikey transfer sheet
Inventor              // (not used in Bundle)
BOMInput              // (not used in Bundle)
```

---

## ?? HELPER METHODS USED

### From `Excel.StaticHelpers`:
```csharp
LoadPregoValue<T>(Control control, Worksheet worksheet, params string[] cellNames)
LoadPregoDouble(TextBox textBox, Worksheet worksheet, params string[] cellNames)
LoadPregoInt(TextBox textBox, Worksheet worksheet, params string[] cellNames)
LoadPregoBool(CheckBox checkBox, Worksheet worksheet, params string[] cellNames)
LoadPregoDouble_FeetToInches(TextBox textBox, Worksheet worksheet, params string[] cellNames)
LoadPregoDouble(TextBox textBox, Worksheet worksheet, double valueIfSmithco, double valueIfHPC)
```

### From `Excel.Prego`:
```csharp
CellDouble(Worksheet sheet, params string[] cellNames)  
CellString(Worksheet sheet, params string[] cellNames)
```

---

## ? IMPLEMENTATION STATUS IN UNIFIEDUI

### ? Already Implemented (Partial):
- Job Number (H2)
- Bundle Width (BQ45, F12) with feet?inches conversion
- Side Frame Depth (BGM26) 
- Side Frame Thickness (CG32, CF32, CG30, CF30)

### ? MISSING - Need to Add:
- Bank (C7) - Not imported yet
- Customer, Client, Location, PO, Item Number (B2, B4, B5, H3)
- TitleblockManuf (G27, F27)
- Headers Outside Frame (G15, F15)
- TubeLength with feet converter (L15, N15)
- TubeProjection (Smithco vs HPC logic)
- FinStripBack Front/Rear (X39, Y39)
- TubeOD, TubeWallTHK, FinOD (L10, N14, L14, N19, L19)
- Tube Row counts (AW39, AU39, AW42, AU42)
- TubeHorizPitch (BO47)
- TubeQuantity (N20, L20)
- **ALL 18 Vertical Pitch values** (DF58-DF66, DF70-DF78)
- Tube Support data (BGF12, BGF20, CG28, CF28, CG26, CF26)
- Camber auto-detection logic
- Header data import (`ImportHeaderData_FromPrego()`)

---

## ?? UNIFIEDUI IMPLEMENTATION PLAN

### Step 1: Update `BundleConfiguration` Model
Add missing properties to match all CommonData fields:
- `TubeProjection`
- `FinStripBack_Front`, `FinStripBack_Rear`  
- `TubeWallThickness`, `FinOD`, `TubeRow1Count`, `TubeRow2Count`
- `TubeQuantity`, `TubeSupportSpacing`, `TubeSupportQuantity`, `TubeSupportSize`
- `Cambered` (bool)
- List<double> for all 18 vertical pitches

### Step 2: Update `ImportBundleDataFromPrego()` in MainViewModel
Add ALL missing cell reads following the exact mapping above.

### Step 3: Update Property Bindings in XAML
Ensure all new properties are bound to UI controls.

### Step 4: Test with Real Prego File
Verify all data imports correctly and matches old BundleUI behavior.

---

## ?? CURRENT UNIFIEDUI CODE GAPS

```csharp
// CURRENT (only 4 fields):
var bundleWidth = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "BQ45");
var sideFrameDepth = Excel.Prego.CellDouble(Excel.Prego.InputsCalcsSheet, "BGM26");
var sideFrameThk = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "CG32");
var jobNumber = Excel.Prego.CellString(Excel.Prego.InputSheet, "H2");

// NEEDED (40+ more fields based on old BundleUI mapping above)
```

---

## ?? CONVERSION FORMULAS

### Feet to Inches (Bundle Width):
```csharp
if (bundleWidth < 16)  // Assume feet if less than 16
{
    bundleWidth *= 12;  // Convert to inches
}
```

### Bank Letter Conversion:
```csharp
var bankNumber = Excel.Prego.CellDouble(Excel.Prego.InputSheet, "C7");
config.Bank = (int)(bankNumber);  // 1='A', 2='B', etc.
```

### Tube Projection (Manufacturer-specific):
```csharp
var titleblockManuf = Excel.Prego.CellString(Excel.Prego.InputSheet, "G27", "F27");
config.TubeProjection = (titleblockManuf == "Smithco") ? 0.25 : 0.125;
```

### Tube Support Size Extraction:
```csharp
var tubeSupportFull = Excel.Prego.CellString(Excel.Prego.InputSheet, "CG28", "CF28", "CG26", "CF26");
config.TubeSupportSize = tubeSupportFull?.Split(' ')[0];  // Extract first word
```

---

## ?? NOTES

1. **Method Signatures**: The old UI uses `StaticHelpers.LoadPregoXXX()` which take a Control as first parameter and auto-populate it. UnifiedUI needs to use the underlying `Prego.CellDouble()` / `Prego.CellString()` directly.

2. **Multiple Cell Fallbacks**: Many properties try multiple cells in order (e.g., BQ45 then F12). Use the `params string[]` feature of `CellDouble()`.

3. **Header Import**: The old UI calls `ImportHeaderData_FromPrego()` separately - this is in `Excel.Header_DataManager` and handles all 6 header types (61-66).

4. **Vertical Pitches**: These are stored in `FrontVerticalPitch` and `RearVerticalPitch` objects with properties like `._1_2`, `._2_3`, etc. UnifiedUI should use a `List<double>`.

5. **Worksheet Access**: Must ensure `Excel.Prego.PregoDoc != null` before accessing sheets.

---

**Last Updated:** Current session  
**Source Files:**
- `Bundle/BundleUI.cs` lines 189-296
- `Excel/StaticHelpers.cs`  
- `Excel/Prego.cs`


