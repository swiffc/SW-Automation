# ?? PREGO IMPORT TESTING GUIDE

**Purpose**: Test the new Prego import functionality in UnifiedUI  
**Date**: October 27, 2025

---

## ?? WHAT WAS IMPLEMENTED

### Backend (MainViewModel.cs):
- ? `ImportFromPrego()` - Main import method
- ? `ImportJobInfoFromPrego()` - Job info import
- ? `ImportBundleFromPrego()` - Bundle dimensions import
- ? `ImportHeaderFromPrego()` - Header import (basic)

### UI (MainWindow.xaml):
- ? "Import Prego" button in File menu
- ? "Import Prego" button in toolbar (?? icon)
- ? Rich tooltips with instructions

### Event Handler (MainWindow.xaml.cs):
- ? `ImportPregoButton_Click()` - Button click handler

---

## ?? PRE-TEST CHECKLIST

Before you start testing, verify:

- [ ] **UnifiedUI compiles successfully**
  ```
  Open: Solidworks Automation.sln
  Build > Build Solution (Ctrl+Shift+B)
  Check for errors
  ```

- [ ] **You have a test Prego file**
  ```
  Location: C:\AXC_VAULT\Active\{YOUR_JOB}\Drafting\Headers\~Archive\{JOB}-prego{BANK}.xlsm
  Example: C:\AXC_VAULT\Active\S25140\Drafting\Headers\~Archive\S25140-prego1.xlsm
  ```

- [ ] **Excel is installed and working**

- [ ] **SolidWorks is running** (may be needed for some functionality)

---

## ?? TEST PLAN

### TEST 1: UI Elements Exist
**Goal**: Verify the Import Prego button appears

**Steps**:
1. Build and run UnifiedUI
2. Look at the **File menu** ? Should see "Import Prego..." menu item
3. Look at the **toolbar** ? Should see "Import Prego" button with ?? icon
4. Hover over the button ? Should see rich tooltip

**Expected Results**:
- ? Menu item exists
- ? Toolbar button exists
- ? Tooltip shows file location info
- ? Tooltip mentions "200+ fields"

---

### TEST 2: Button Click Without Prego File
**Goal**: Test error handling when Prego file doesn't exist

**Steps**:
1. In UnifiedUI, leave Job Number empty (or use fake job like "TEST12345")
2. Click "Import Prego" button
3. Observe the message box

**Expected Results**:
- ?? Message box appears: "Prego file not found"
- ?? Shows expected file location
- ?? Lists troubleshooting steps
- ?? References user guide
- ? No crash or exception

---

### TEST 3: Bundle Import (Happy Path)
**Goal**: Successfully import Bundle data from Prego

**Prerequisites**:
- Have a real Prego file (e.g., S25140-prego1.xlsm)

**Steps**:
1. In UnifiedUI, select **Bundle tab**
2. Enter **Job Number**: `S25140` (or your test job)
3. Enter **Bank**: `1` (or appropriate bank)
4. Click **"Import Prego"** button
5. Wait for Excel to open (may take 5-10 seconds)
6. Observe message box
7. Check Bundle fields are populated:
   - Bundle Width
   - Side Frame Depth
   - Side Frame Thickness

**Expected Results**:
- ? Success message appears
- ? Message says "Data imported from Prego successfully!"
- ? Bundle Width field is populated with value from Excel
- ? Side Frame Depth is populated
- ? Side Frame Thickness is populated
- ? Values look reasonable (e.g., Bundle Width 30-60 inches)
- ? Excel closes automatically
- ? No error messages

**Verify in Logs**:
```
Check: FileTools logs (if logging enabled)
Should see:
  "ImportFromPrego - Starting Prego import"
  "Prego document found, importing data"
  "? Bundle Width: XX" (from cell BQ45/F12)"
  "? Side Frame Depth: XX" (from cell BGM26)"
  "? Side Frame THK: XX" (from cells CG32/CF32/CG30/CF30)"
  "Bundle data import complete - 3 core fields imported"
  "Prego import completed successfully"
```

---

### TEST 4: Header Import (Basic)
**Goal**: Test Header import (currently imports job info only)

**Steps**:
1. In UnifiedUI, select **Header tab**
2. Enter **Job Number**: `S25140`
3. Enter **Bank**: `1`
4. Click **"Import Prego"** button
5. Observe message box

**Expected Results**:
- ? Success message appears
- ? Job Number field is populated
- ? No crash
- ?? Note: Full header import not yet implemented

---

### TEST 5: Import Multiple Times
**Goal**: Test that import can be run multiple times

**Steps**:
1. Import Bundle data (Test 3)
2. Manually change Bundle Width to a different value
3. Click "Import Prego" again
4. Check if Bundle Width reverts to imported value

**Expected Results**:
- ? Second import succeeds
- ? Fields are updated with Prego data
- ? No duplicate Excel processes
- ? No memory leaks

---

### TEST 6: Import Without Job Number
**Goal**: Test validation

**Steps**:
1. Clear Job Number field
2. Click "Import Prego"

**Expected Results**:
- ?? Error message about missing Job Number
- ? Application doesn't crash

---

### TEST 7: Verify Cell Mappings
**Goal**: Manually verify that correct Excel cells are being read

**Steps**:
1. Open Prego Excel file manually
2. Note values in these cells:
   - **BQ45** (Bundle Width)
   - **BGM26** on "Input & Calcs" sheet (Side Frame Depth)
   - **CG32** (Side Frame THK)
3. Run Import Prego in UnifiedUI
4. Compare values

**Expected Results**:
- ? Bundle Width matches BQ45 (or F12 if BQ45 is empty)
- ? Side Frame Depth matches BGM26
- ? Side Frame THK matches first non-zero: CG32, CF32, CG30, or CF30
- ? Values in feet are converted to inches

---

### TEST 8: Feet to Inches Conversion
**Goal**: Test automatic unit conversion

**Steps**:
1. Find a Prego file where Bundle Width (BQ45) is in feet (value < 16)
2. Import
3. Check if value is multiplied by 12

**Expected Results**:
- ? If BQ45 = 4.5 (feet), UnifiedUI shows 54 (inches)
- ? If BQ45 = 48 (inches), UnifiedUI shows 48 (no conversion)

---

### TEST 9: Excel COM Cleanup
**Goal**: Ensure Excel processes are cleaned up

**Steps**:
1. Open Task Manager
2. Note Excel process count
3. Run Import Prego 5 times
4. Check Task Manager again

**Expected Results**:
- ? No extra Excel processes left running
- ? Excel.Prego.CleanUp() is working
- ? No memory growth

---

### TEST 10: Tooltip Content
**Goal**: Verify tooltip is helpful

**Steps**:
1. Hover over "Import Prego" toolbar button
2. Read the tooltip

**Expected Results**:
- ? Tooltip shows file location pattern
- ? Mentions "200+ fields"
- ? Lists what gets imported (job info, dimensions, etc.)
- ? References user guide

---

## ?? KNOWN ISSUES / LIMITATIONS

### Current Implementation:
- ? Bundle: 3 core fields imported (Width, Depth, THK)
- ?? Bundle: 47+ additional fields documented but not yet implemented
- ?? Header: Only job info imported (100+ fields pending)
- ?? Hood, Walkway, etc.: Not yet implemented

### Future Enhancements:
- [ ] Import all 50+ Bundle fields
- [ ] Complete Header import (100+ fields)
- [ ] Add Prego import for other components
- [ ] Add "Recent Prego Files" menu
- [ ] Add Prego version detection

---

## ?? TEST RESULTS TEMPLATE

Use this to record your test results:

```
Date: __________
Tester: __________
UnifiedUI Version: __________

TEST 1 - UI Elements: [ ] Pass [ ] Fail
  Notes: _________________________________

TEST 2 - No Prego File: [ ] Pass [ ] Fail
  Notes: _________________________________

TEST 3 - Bundle Import: [ ] Pass [ ] Fail
  Bundle Width imported: _______
  Side Frame Depth imported: _______
  Side Frame THK imported: _______
  Notes: _________________________________

TEST 4 - Header Import: [ ] Pass [ ] Fail
  Notes: _________________________________

TEST 5 - Multiple Imports: [ ] Pass [ ] Fail
  Notes: _________________________________

TEST 6 - No Job Number: [ ] Pass [ ] Fail
  Notes: _________________________________

TEST 7 - Cell Mappings: [ ] Pass [ ] Fail
  BQ45 value: _______ ? UnifiedUI: _______
  BGM26 value: _______ ? UnifiedUI: _______
  CG32 value: _______ ? UnifiedUI: _______
  Notes: _________________________________

TEST 8 - Unit Conversion: [ ] Pass [ ] Fail
  Test value: _______ ? Converted: _______
  Notes: _________________________________

TEST 9 - COM Cleanup: [ ] Pass [ ] Fail
  Excel processes before: _______
  Excel processes after: _______
  Notes: _________________________________

TEST 10 - Tooltip: [ ] Pass [ ] Fail
  Notes: _________________________________

OVERALL RESULT: [ ] All Pass [ ] Some Failures
```

---

## ?? DEBUGGING TIPS

### If Import Button Does Nothing:
1. Check Output window for exceptions
2. Verify event handler is wired up in XAML
3. Add breakpoint in `ImportPregoButton_Click()`

### If "Prego file not found" Error:
1. Verify Job Number is correct
2. Check Bank is set
3. Manually browse to file location
4. Verify file exists in AXC_VAULT
5. Check EPDM connection

### If Excel Stays Open:
1. Check Excel.Prego.CleanUp() is being called
2. Look for exceptions that prevent cleanup
3. Manually close Excel and retry

### If Wrong Values Imported:
1. Open Prego file manually
2. Check cell locations match documentation
3. Verify sheet names are correct
4. Check for different Prego version

### If Application Crashes:
1. Check Output window for exception details
2. Review GlobalErrorHandler logs
3. Add try-catch in more places
4. Check COM object references

---

## ?? RELATED DOCUMENTATION

- **User Guide**: `docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md`
- **Gap Analysis**: `docs/Analysis/PREGO_IMPORT_GAP_ANALYSIS.md`
- **Cell Mappings**: See User Guide, page "CELL MAPPING REFERENCE"

---

## ? READY TO TEST?

1. Build UnifiedUI (Ctrl+Shift+B)
2. Have a Prego file ready
3. Run through tests 1-10
4. Record results
5. Report any issues

**Good luck testing!** ??







