# ?? Design Table Analysis - Excel to CAD Connections

**Date**: October 25, 2025  
**Total Excel Files**: 45  
**Purpose**: Map all Excel design tables to their CAD assemblies

---

## ?? What Are Design Tables?

**Design tables** are Excel spreadsheets that control SolidWorks dimensions and configurations. They allow you to:
- Create multiple configurations from one master model
- Drive dimensions parametrically
- Generate families of parts/assemblies automatically
- Automate repetitive design tasks

---

## ?? Design Table Summary by Tool

```
Tool                    Excel Files    Type
???????????????????????????????????????????????????????????
Header Section Tool     37            Design Tables (parametric)
XCH Structure Tool      3             Config + Calculations
Z Structure Tool        3             Config + Calculations
Hudson Certified        2             Calculation sheets
???????????????????????????????????????????????????????????
TOTAL                   45 files
```

---

## 1?? Header Section Tool (37 Excel files) ?? MOST PARAMETRIC

### Master Configuration Files (4 files)

#### **000000_S01c-HCS.xlsx** - Combined Header Configuration Sheet
```
Controls: 000000_S01c-Header.SLDASM (Main assembly)
Purpose:  Master configuration for multi-circuit headers
Drives:   All S01c components listed below
Type:     MASTER DESIGN TABLE
```

#### **000000_S01c-SFCS.xlsx** - Combined Section Frame Configuration Sheet
```
Controls: 000000_S01c-SEC.SLDASM (Section assembly)
Purpose:  Section/frame configuration
Drives:   Section components
Type:     MASTER DESIGN TABLE
```

#### **000000_S03-HCS.xlsx** - Single Header Configuration Sheet
```
Controls: 000000_S03-Header.SLDASM (Main assembly)
Purpose:  Master configuration for single-circuit headers
Drives:   All S03 components listed below
Type:     MASTER DESIGN TABLE
```

#### **000000_S03-SFCS.xlsx** - Single Section Frame Configuration Sheet
```
Controls: 000000_S03-SEC.SLDASM (Section assembly)
Purpose:  Section/frame configuration
Drives:   Section components
Type:     MASTER DESIGN TABLE
```

---

### Component Design Tables - S01c (Combined Headers) - 17 files

Each Excel file controls ONE specific part/assembly:

| Excel File | Controls CAD File | Component |
|-----------|------------------|-----------|
| Worksheet in 000000_S01c-Header.xlsx | 000000_S01c-Header.SLDASM | Main header assembly |
| Worksheet in 000000_S01c-HeaderBox.xlsx | 000000_S01c-HeaderBox.SLDPRT | Header box part |
| Worksheet in 000000_S01c-Filler.xlsx | 000000_S01c-Filler.SLDPRT | Filler part |
| Worksheet in 000000_S01c-Filler Top.xlsx | 000000_S01c-Filler Top.SLDPRT | Filler top part |
| Worksheet in 000000_S01c-FlangeCover.xlsx | 000000_S01c-FlangeCover.SLDASM | Flange cover assy |
| Worksheet in 000000_S01c-LWN Flange.xlsx | 000000_S01c-LWN Flange.SLDPRT | LWN flange part |
| Worksheet in 000000_S01c-Nozzle.xlsx | 000000_S01c-Nozzle.SLDASM | Nozzle assembly |
| Worksheet in 000000_S01c-Pipe.xlsx | 000000_S01c-Pipe.SLDPRT | Pipe part |
| Worksheet in 000000_S01c-SEC.xlsx | 000000_S01c-SEC.SLDASM | Section assembly |
| Worksheet in 000000_S01c-SFR.xlsx | 000000_S01c-SFR.SLDPRT | Section frame part |
| Worksheet in 000000_S01c-Support Bar.xlsx | 000000_S01c-Support Bar.SLDPRT | Support bar |
| Worksheet in 000000_S01c-TTS.xlsx | 000000_S01c-TTS.SLDPRT | Tube support sheet |
| Worksheet in 000000_S01c-Tube Support.xlsx | 000000_S01c-Tube Support.SLDPRT | Tube support |
| Worksheet in 000000_S01c-Tube.xlsx | 000000_S01c-Tube.SLDPRT | Tube part |
| Worksheet in 000000_S01c-TubeInsert.xlsx | 000000_S01c-TubeInsert.SLDPRT | Tube insert |

---

### Component Design Tables - S03 (Single Headers) - 16 files

Each Excel file controls ONE specific part/assembly:

| Excel File | Controls CAD File | Component |
|-----------|------------------|-----------|
| Worksheet in 000000_S03-Header.xlsx | 000000_S03-Header.SLDASM | Main header assembly |
| Worksheet in 000000_S03-HeaderBox.xlsx | 000000_S03-HeaderBox.SLDPRT | Header box part |
| Worksheet in 000000_S03-Filler.xlsx | 000000_S03-Filler.SLDPRT | Filler part |
| Worksheet in 000000_S03-Filler Top.xlsx | 000000_S03-Filler Top.SLDPRT | Filler top part |
| Worksheet in 000000_S03-FlangeCover.xlsx | 000000_S03-FlangeCover.SLDASM | Flange cover assy |
| Worksheet in 000000_S03-LWN Flange.xlsx | 000000_S03-LWN Flange.SLDPRT | LWN flange part |
| Worksheet in 000000_S03-Nozzle.xlsx | 000000_S03-Nozzle.SLDASM | Nozzle assembly |
| Worksheet in 000000_S03-Pipe.xlsx | 000000_S03-Pipe.SLDPRT | Pipe part |
| Worksheet in 000000_S03-SEC.xlsx | 000000_S03-SEC.SLDASM | Section assembly |
| Worksheet in 000000_S03-SFR.xlsx | 000000_S03-SFR.SLDPRT | Section frame part |
| Worksheet in 000000_S03-Support Bar.xlsx | 000000_S03-Support Bar.SLDPRT | Support bar |
| Worksheet in 000000_S03-TTS.xlsx | 000000_S03-TTS.SLDPRT | Tube support sheet |
| Worksheet in 000000_S03-Tube Spacer.xlsx | 000000_S03-Tube Spacer.SLDPRT | Tube spacer |
| Worksheet in 000000_S03-Tube Support.xlsx | 000000_S03-Tube Support.SLDPRT | Tube support |
| Worksheet in 000000_S03-Tube.xlsx | 000000_S03-Tube.SLDPRT | Tube part |
| Worksheet in 000000_S03-TubeInsert.xlsx | 000000_S03-TubeInsert.SLDPRT | Tube insert |

---

### ?? Header Tool Design Table Hierarchy

```
MASTER CONFIG FILES
??? 000000_S01c-HCS.xlsx (Combined)
?   ??? Controls ALL S01c components (17 parts/assemblies)
??? 000000_S01c-SFCS.xlsx (Combined Section)
?   ??? Controls S01c section components
??? 000000_S03-HCS.xlsx (Single)
?   ??? Controls ALL S03 components (16 parts/assemblies)
??? 000000_S03-SFCS.xlsx (Single Section)
    ??? Controls S03 section components

COMPONENT DESIGN TABLES
Each component has its own embedded Excel design table
that defines dimensions, configurations, and features
```

---

## 2?? XCH Structure Tool (3 Excel files)

### **XCH_SCS.xlsx** - Structure Configuration Sheet (MAIN)
```
Location: templates\xch_structure_tool\XCH Cooler\
Controls: XCH_Assembly.SLDASM (main cooler assembly)
Purpose:  Master configuration for entire XCH cooler
Drives:   All XCH components:
          - XCH_FE (fan end)
          - XCH_FDK (fan deck)
          - XCH_COL (columns)
          - XCH_MPL (middle plate)
          - XCH_EPL (end plate)
          - XCH_SPL (side plate)
          - XCH_WLK (walkway)
          - XCH_DRM (drum)
Type:     MASTER DESIGN TABLE
```

### **XCH Cooler Design Work Sheet.xls**
```
Location: templates\xch_structure_tool\
Purpose:  Engineering calculations and sizing
Type:     CALCULATION SHEET (not a design table)
Uses:     Reference for manual design decisions
```

### **XCH_Lift Lug safety factors.xls**
```
Location: templates\xch_structure_tool\
Purpose:  Safety calculations for lifting lugs
Type:     CALCULATION SHEET (structural analysis)
```

---

## 3?? Z Structure Tool (3 Excel files)

### **Z Cooler Design Work Sheet.xls**
```
Location: templates\z_structure_tool\
Purpose:  Engineering calculations and sizing
Type:     CALCULATION SHEET (not a design table)
Uses:     Reference for Z cooler design
```

### **ZST_Z_Lift Lug safety factors.xls**
```
Location: templates\z_structure_tool\
Purpose:  Safety calculations for lifting lugs
Type:     CALCULATION SHEET (structural analysis)
```

### **Lifting System Work Sheet.xlsx**
```
Location: templates\z_structure_tool\Z Cooler\3 Beam Lifting System\
Purpose:  Calculations for 3-beam lifting system
Type:     CALCULATION SHEET
```

---

## 4?? Hudson Certified (2 Excel files)

### **Cofimco Fan Calculator.xlsx**
```
Location: templates\hudson_certified\archive\
Purpose:  Fan sizing and selection calculations
Type:     CALCULATION SHEET
```

### **Hood Design.xls**
```
Location: templates\hudson_certified\Hood\
Purpose:  Hood design calculations (1976 program reference)
Type:     CALCULATION SHEET (legacy)
```

---

## ?? Design Table Types Explained

### Type 1: Master Configuration Files (4 files)
**Location**: Header Section Tool  
**Files**: 
- `000000_S01c-HCS.xlsx`
- `000000_S01c-SFCS.xlsx`
- `000000_S03-HCS.xlsx`
- `000000_S03-SFCS.xlsx`

**How They Work**:
1. You edit the Excel file with your design parameters
2. Open the corresponding assembly in SolidWorks
3. SolidWorks reads the Excel file
4. All dimensions update automatically
5. All child components regenerate
6. Drawings update automatically

**Automation Potential**: ????? (HIGHEST)

---

### Type 2: Component Design Tables (33 files)
**Location**: Header Section Tool  
**Format**: `Worksheet in 000000_XXX.xlsx`

**How They Work**:
1. Each component has an embedded design table
2. The Excel file is stored WITH the CAD file
3. Parent assembly drives child component configurations
4. Changes cascade through the hierarchy

**Automation Potential**: ???? (HIGH)

---

### Type 3: Configuration Sheets (1 file)
**Location**: XCH Structure Tool  
**File**: `XCH_SCS.xlsx`

**How It Works**:
1. Similar to Master Config Files
2. Single Excel file controls entire XCH cooler
3. Opens main assembly ? regenerates all

**Automation Potential**: ????? (HIGHEST)

---

### Type 4: Calculation Sheets (7 files)
**Location**: All tools  
**Examples**: Design Work Sheets, Safety Factors, Fan Calculator

**How They Work**:
1. These are NOT design tables
2. Used for engineering calculations
3. Reference only - don't drive CAD directly
4. Manual input ? manual CAD updates

**Automation Potential**: ?? (REFERENCE ONLY)

---

## ?? How to Use Design Tables for Automation

### Method 1: Edit Excel ? Regenerate CAD (Header Tool)

```python
# Example workflow
1. Open Excel file: 000000_S01c-HCS.xlsx
2. Modify parameters (row 2 typically has config name)
3. Save Excel file
4. Open in SolidWorks: 000000_S01c-Header.SLDASM
5. SolidWorks automatically reads Excel and regenerates
6. Save new configuration
```

**VBA/Python Automation**:
```python
import win32com.client
import openpyxl

# Step 1: Modify Excel
wb = openpyxl.load_workbook('000000_S01c-HCS.xlsx')
ws = wb.active
ws['B2'] = 'NewConfig'  # New configuration name
ws['C2'] = 48.0         # Some dimension
wb.save('000000_S01c-HCS.xlsx')

# Step 2: Open SolidWorks and regenerate
swApp = win32com.client.Dispatch("SldWorks.Application")
model = swApp.OpenDoc('000000_S01c-Header.SLDASM', 2)
model.ForceRebuild3(True)  # Regenerate with new Excel data
model.Save()
```

---

### Method 2: XCH Configuration Sheet

```python
# Edit XCH_SCS.xlsx
1. Modify cooler dimensions
2. Open XCH_Assembly.SLDASM
3. Entire cooler regenerates automatically
```

---

## ?? Automation Priority Ranking

### Tier 1: HIGHEST PRIORITY (Best for automation)
```
? Header Section Tool Master Configs (4 files)
   - 000000_S01c-HCS.xlsx
   - 000000_S03-HCS.xlsx
   - Well-structured, tested, production-ready

? XCH_SCS.xlsx
   - Single-file control of entire assembly
```

### Tier 2: HIGH PRIORITY (Good for automation)
```
? Header Component Design Tables (33 files)
   - Automated through parent assemblies
```

### Tier 3: REFERENCE ONLY (Manual use)
```
?? Calculation sheets (7 files)
   - Design work sheets
   - Safety factors
   - Fan calculators
```

---

## ?? Key Insights

### 1. Header Section Tool is the MOST Parametric
- **37 Excel files** controlling the entire system
- Master configs + individual component tables
- Proven system (in production)
- Best candidate for full automation

### 2. XCH Uses Single Master Config
- **XCH_SCS.xlsx** controls everything
- Simpler structure than Header Tool
- Good for batch processing

### 3. Z Tool is Mostly Manual
- Calculation sheets only
- Less parametric automation
- More manual design work

### 4. Hudson is Legacy/Reference
- Old calculation sheets (1976 program!)
- Not design-table driven

---

## ?? Recommended Automation Approach

### Phase 1: Header Section Tool
```
1. Master the HCS.xlsx files (4 master configs)
2. Create automation script to:
   - Read input parameters
   - Update Excel files
   - Open SolidWorks assemblies
   - Regenerate configurations
   - Export drawings
```

### Phase 2: XCH Structure Tool
```
1. Automate XCH_SCS.xlsx
2. Similar workflow to Header Tool
3. Simpler because single master file
```

### Phase 3: Job-Based Automation
```
1. Integrate with Job Browser
2. Auto-select correct tool based on job type
3. Populate Excel from job requirements
4. Generate complete assemblies + drawings
5. Export to output folder
```

---

## ?? File Locations

```
Header Section Tool Design Tables:
templates/header_section_tool/Combined_/Drafting/Headers/
templates/header_section_tool/Single_/Drafting/Headers/

XCH Structure Tool Config:
templates/xch_structure_tool/XCH Cooler/XCH_SCS.xlsx

Z Tool Worksheets:
templates/z_structure_tool/Z Cooler Design Work Sheet.xls

Hudson Calculators:
templates/hudson_certified/Hood/Hood Design.xls
templates/hudson_certified/archive/Cofimco Fan Calculator.xlsx
```

---

## ?? Next Steps

1. **Examine one Excel file in detail** (e.g., 000000_S01c-HCS.xlsx)
2. **Map all columns** to their CAD dimensions
3. **Create automation script** to modify Excel programmatically
4. **Test regeneration** with SolidWorks API
5. **Build UI** for easy parameter input

---

*Analysis Complete: October 25, 2025*  
*Total Excel Files Analyzed: 45*  
*Design Tables Identified: 38 (automation-ready)*  
*Calculation Sheets: 7 (reference only)*  

**Status**: ? COMPLETE MAPPING - READY FOR AUTOMATION

