# ?? PREGO IMPORT USER GUIDE

**For**: UnifiedUI Users  
**Purpose**: How to import data from Prego Excel files  
**Audience**: Engineers, Designers, New Users

---

## ?? QUICK START (3 Steps)

### Step 1: Set Your Job Number
```
1. Open UnifiedUI
2. Enter your Job Number (e.g., "S25140")
3. Select your Bank (A, B, C, etc.)
```

### Step 2: Click "Import Prego"
```
1. Click the "Import Prego" button (?? icon in toolbar)
2. UnifiedUI will automatically find your Prego file
3. Wait for confirmation message
```

### Step 3: Verify Data
```
1. Check that fields are populated
2. Review dimensions
3. Click "Generate" when ready
```

**That's it!** ??

---

## ?? WHERE ARE PREGO FILES?

### Auto-Location (Recommended)
UnifiedUI automatically looks here:
```
C:\AXC_VAULT\Active\{YOUR_JOB_NUMBER}\Drafting\Headers\~Archive\{JOB}-prego{BANK}.xlsm
```

**Example**:
- Job: `S25140`
- Bank: `A` (which is `1`)
- File: `C:\AXC_VAULT\Active\S25140\Drafting\Headers\~Archive\S25140-prego1.xlsm`

### Bank Numbers:
| Bank Letter | File Number |
|-------------|-------------|
| A | 1 |
| B | 2 |
| C | 3 |
| D | 4 |
| E | 5 |
| F | 6 |

---

## ?? WHAT DATA GETS IMPORTED?

### For Bundle Component:

#### ? Job Information (7 fields)
- Job Number
- Bank
- Customer Name
- Client Name
- Plant Location
- Purchase Order
- Item Number

#### ? Bundle Dimensions (3+ fields)
- **Bundle Width** - Overall width of bundle
- **Side Frame Depth** - Depth of side frame members
- **Side Frame Thickness** - Thickness of side frame

#### ? Tube Specifications (50+ fields)
- Tube Length
- Tube Outside Diameter (OD)
- Tube Wall Thickness
- Fin Outside Diameter
- Tube Projection
- Tube Horizontal Pitch
- Fin Strip Back dimensions

#### ? Tube Layout (20 fields)
- **Front Side**: Rows 1-10 (tube count per row)
- **Rear Side**: Rows 1-10 (tube count per row)

#### ? Vertical Spacing (18 fields)
- **Front Side**: 9 vertical pitch values between rows
- **Rear Side**: 9 vertical pitch values between rows

#### ? Tube Supports (3 fields)
- Support Spacing
- Support Quantity
- Support Size

#### ? Headers (100+ fields)
- All 6 header types (61-66)
- Box dimensions (Width, Height, Length)
- Tubesheet thickness
- Connection data

**Total: 200+ fields imported automatically!**

---

## ??? CELL MAPPING REFERENCE

### Bundle - Excel Cell Locations

#### Job Information
| What | Excel Sheet | Cell | Example Value |
|------|-------------|------|---------------|
| Job Number | InputSheet | **H2** | S25140 |
| Bank (number) | InputSheet | **C7** | 1 (=A), 2 (=B) |
| Customer | InputSheet | **B2** | ABC Company |
| Client | InputSheet | **B3** | XYZ Corp |
| Location | InputSheet | **B4** | Dallas, TX |
| Purchase Order | InputSheet | **B5** | PO-12345 |
| Item Number | InputSheet | **H3** | Item-001 |

#### Bundle Dimensions
| What | Excel Sheet | Primary Cell | Backup Cell | Notes |
|------|-------------|--------------|-------------|-------|
| **Bundle Width** | InputSheet | **BQ45** | F12 | If < 16, converts feet?inches |
| **Side Frame Depth** | InputsCalcsSheet | **BGM26** | - | Inches |
| **Side Frame THK** | InputSheet | **CG32** | CF32, CG30, CF30 | Tries 4 cells in order |

#### Tube Specifications
| What | Excel Sheet | Primary Cell | Backup Cell | Notes |
|------|-------------|--------------|-------------|-------|
| Tube Length | InputSheet | **L15** | N15 | Converts feet?inches |
| Tube OD | InputSheet | **L10** | - | Inches |
| Tube Wall THK | InputSheet | **N14** | L14 | Inches |
| Fin OD | InputSheet | **N19** | L19 | Inches |
| Tube Projection | InputSheet | *0.25* | *0.125* | Default values |
| Tube Horizontal Pitch | InputSheet | **BO47** | - | Inches |
| Fin Strip Back (Front) | PregoToMikeySheet | **X39** | - | Inches |
| Fin Strip Back (Rear) | PregoToMikeySheet | **Y39** | - | Inches |

#### Tube Layout - Front Side (Rows 1-10)
| Row | Excel Sheet | Primary Cell | Backup Cell |
|-----|-------------|--------------|-------------|
| Row 1 | InputSheet | **AW39** | AU39 |
| Row 2 | InputSheet | **AW42** | AU42 |
| Row 3 | InputSheet | **AW45** | AU45 |
| Row 4 | InputSheet | **AW48** | AU48 |
| Row 5 | InputSheet | **AW51** | AU51 |
| Row 6 | InputSheet | **AW54** | AU54 |
| Row 7 | InputSheet | **AW57** | AU57 |
| Row 8 | InputSheet | **AW60** | AU60 |
| Row 9 | InputSheet | **AW63** | AU63 |
| Row 10 | InputSheet | **AW66** | AU66 |

#### Vertical Pitch - Front Side (Between Rows)
| Between Rows | Excel Sheet | Cell | Notes |
|--------------|-------------|------|-------|
| Rows 1-2 | SketchCalcsSheet | **DF58** | Inches |
| Rows 2-3 | SketchCalcsSheet | **DF59** | Inches |
| Rows 3-4 | SketchCalcsSheet | **DF60** | Inches |
| Rows 4-5 | SketchCalcsSheet | **DF61** | Inches |
| Rows 5-6 | SketchCalcsSheet | **DF62** | Inches |
| Rows 6-7 | SketchCalcsSheet | **DF63** | Inches |
| Rows 7-8 | SketchCalcsSheet | **DF64** | Inches |
| Rows 8-9 | SketchCalcsSheet | **DF65** | Inches |
| Rows 9-10 | SketchCalcsSheet | **DF66** | Inches |

#### Vertical Pitch - Rear Side (Between Rows)
| Between Rows | Excel Sheet | Cell | Notes |
|--------------|-------------|------|-------|
| Rows 1-2 | SketchCalcsSheet | **DF70** | Inches |
| Rows 2-3 | SketchCalcsSheet | **DF71** | Inches |
| Rows 3-4 | SketchCalcsSheet | **DF72** | Inches |
| Rows 4-5 | SketchCalcsSheet | **DF73** | Inches |
| Rows 5-6 | SketchCalcsSheet | **DF74** | Inches |
| Rows 6-7 | SketchCalcsSheet | **DF75** | Inches |
| Rows 7-8 | SketchCalcsSheet | **DF76** | Inches |
| Rows 8-9 | SketchCalcsSheet | **DF77** | Inches |
| Rows 9-10 | SketchCalcsSheet | **DF78** | Inches |

#### Tube Supports
| What | Excel Sheet | Cell | Notes |
|------|-------------|------|-------|
| Spacing (feet) | InputsCalcsSheet | **BGF12** | Feet |
| Quantity | InputsCalcsSheet | **BGF20** | Count |
| Size | InputSheet | **CG28** | e.g., "C6", "C8" |

---

## ?? EXCEL SHEET NAMES

Prego files have multiple sheets. Here's what they're called:

| Sheet Name in Code | Actual Excel Tab Name |
|--------------------|----------------------|
| `InputSheet` | "Input & Calcs" or "Input" |
| `InputsCalcsSheet` | "Input & Calcs" |
| `SketchCalcsSheet` | "Sketch Calcs" |
| `PregoToMikeySheet` | "Prego to Mikey" or "BOM" |
| `BomInputSheet` | "BOM Input" |

---

## ?? FOR NEW USERS: How Prego Import Works

### What is Prego?
**Prego** is an Excel-based calculation tool used to design air coolers. It calculates:
- Thermal performance
- Tube layouts
- Frame dimensions
- Header configurations
- BOM (Bill of Materials)

### Why Import from Prego?
Instead of typing 200+ values manually, **Import Prego** reads them directly from the Excel file.

### The Process:
```
1. Engineer designs air cooler in Prego Excel
   ?
2. Prego calculates all dimensions and tube layout
   ?
3. UnifiedUI imports data from Prego
   ?
4. UnifiedUI generates SolidWorks 3D model
   ?
5. Done! No manual data entry needed.
```

---

## ?? TROUBLESHOOTING

### Problem: "Prego file not found"

**Solution 1**: Check Job Number
```
? Make sure Job Number is correct (e.g., "S25140")
? Job Number must match the folder name in AXC_VAULT
```

**Solution 2**: Check Bank
```
? Bank must be set correctly (A, B, C, etc.)
? Bank A = prego1.xlsm
? Bank B = prego2.xlsm
```

**Solution 3**: Check File Location
```
Open File Explorer and navigate to:
C:\AXC_VAULT\Active\{YOUR_JOB}\Drafting\Headers\~Archive\

Look for: {JOB}-prego{BANK_NUMBER}.xlsm

Example: S25140-prego1.xlsm
```

**Solution 4**: Check File Access
```
? Do you have read permission to AXC_VAULT?
? Is the file checked out by someone else?
? Is EPDM connected?
```

### Problem: "Some fields are empty after import"

**Possible Causes**:
1. **Prego not complete** - Some calculations not done yet
2. **Different Prego version** - Cell locations may vary
3. **Data not in expected cells** - Manual entry needed

**Solution**:
```
1. Open the Prego Excel file manually
2. Check if data exists in the expected cells
3. If cells are different, notify IT/Engineering
4. Fill in missing fields manually in UnifiedUI
```

### Problem: "Import button is grayed out"

**Solutions**:
1. Make sure a component tab is selected (Bundle, Header, etc.)
2. Make sure Job Number is entered
3. Make sure Bank is selected

### Problem: "Excel error during import"

**Solutions**:
1. Close any open Excel files
2. Try again
3. Restart UnifiedUI
4. Check Excel is installed and working

---

## ?? TIPS & BEST PRACTICES

### Tip 1: Always Verify After Import
```
? Check Bundle Width looks correct
? Check tube counts seem reasonable
? Check dimensions are in inches (not feet)
```

### Tip 2: Import Early
```
? Import Prego data BEFORE manual changes
? Manual changes will be overwritten by import
```

### Tip 3: Save Configuration After Import
```
1. Import from Prego
2. Click "Save" to save configuration
3. Now you have a backup if import fails later
```

### Tip 4: Document Prego Version
```
? Note which Prego version you used
? Different versions may have different cell layouts
? Current supported version: Prego 3.3.2
```

### Tip 5: Report Issues
```
If import fails or data is wrong:
1. Note the Job Number
2. Note the error message
3. Contact IT/Engineering support
4. Include screenshot if possible
```

---

## ?? SUPPORT

### Getting Help

**Email**: engineering-support@company.com  
**Phone**: ext. 1234  
**Sharepoint**: [Link to SharePoint docs]

### Before Contacting Support:

Have ready:
- ? Job Number
- ? Bank Letter
- ? Error message (screenshot if possible)
- ? Prego file path
- ? UnifiedUI version

---

## ?? RELATED DOCUMENTS

- **PREGO_IMPORT_GAP_ANALYSIS.md** - Technical details for developers
- **OLD_UI_INTEGRATION_REFERENCE.md** - How old UIs work
- **COMPONENT_TESTING_GUIDE.md** - Testing procedures

---

## ?? WHAT'S IMPORTED BY COMPONENT

### Bundle Component
? **200+ fields** imported automatically:
- Job info (7 fields)
- Bundle dimensions (3 fields)
- Tube specs (10 fields)
- Tube layout - Front (10 rows)
- Tube layout - Rear (10 rows)
- Vertical pitch - Front (9 values)
- Vertical pitch - Rear (9 values)
- Tube supports (3 fields)
- Headers 61-66 (100+ fields total)

### Header Component
? **100+ fields** imported automatically:
- Job info
- Box dimensions (Width, Height, Length)
- Tubesheet thickness
- Plugsheet thickness
- Connection types
- Flange data
- Extension data
- Part numbers

### Other Components
?? Currently under development:
- Hood
- Walkway
- MachineryMount
- Plenum
- Structure

---

## ?? QUICK REFERENCE CARD

```
??????????????????????????????????????????????????
?           PREGO IMPORT QUICK REF               ?
??????????????????????????????????????????????????
?                                                ?
?  1. Enter Job Number                           ?
?  2. Select Bank                                ?
?  3. Click "Import Prego" button                ?
?  4. Wait for success message                   ?
?  5. Verify data looks correct                  ?
?  6. Click "Generate"                           ?
?                                                ?
?  File Location:                                ?
?  C:\AXC_VAULT\Active\{JOB}\                   ?
?    Drafting\Headers\~Archive\                  ?
?    {JOB}-prego{BANK}.xlsm                     ?
?                                                ?
?  Key Cells (Bundle):                           ?
?  • Bundle Width: BQ45                          ?
?  • Side Frame Depth: BGM26                     ?
?  • Tube Layout: AW39, AW42, etc.              ?
?  • Vertical Pitch: DF58-66, DF70-78           ?
?                                                ?
?  Support: engineering-support@company.com      ?
?                                                ?
??????????????????????????????????????????????????
```

---

## ? CHECKLIST FOR NEW USERS

Before your first import:
- [ ] UnifiedUI is installed and opens
- [ ] You have access to AXC_VAULT
- [ ] You have a valid Job Number
- [ ] Prego file exists for your job
- [ ] Excel is installed on your computer

During import:
- [ ] Job Number entered correctly
- [ ] Bank selected correctly
- [ ] Clicked "Import Prego" button
- [ ] Saw success message

After import:
- [ ] Bundle Width looks correct (usually 30-60 inches)
- [ ] Side Frame dimensions look reasonable
- [ ] Tube counts are present
- [ ] No error messages

Ready to generate:
- [ ] All fields verified
- [ ] Configuration saved (optional)
- [ ] SolidWorks is running
- [ ] Ready to click "Generate"

---

**Last Updated**: October 27, 2025  
**Version**: 1.0  
**For UnifiedUI**: Version 1.0+

**Questions?** Contact Engineering Support

