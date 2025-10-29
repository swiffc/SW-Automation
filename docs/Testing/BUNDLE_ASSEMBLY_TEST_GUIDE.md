# ?? BUNDLE ASSEMBLY - COMPLETE TESTING GUIDE

**Date**: October 27, 2025  
**Component**: Bundle Assembly Automation  
**Status**: ? Built and Ready to Test

---

## ?? QUICK START - TEST NOW! (5 Minutes)

### Option 1: Test WITHOUT SolidWorks (Quickest)
```powershell
# Navigate to the Bundle executable
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug"

# Launch the Bundle UI
.\Bundle.exe
```

**What to verify:**
1. ? Application launches without errors
2. ? UI form displays with all fields
3. ? Try clicking "Create Bundle" (should show SolidWorks not available dialog)
4. ? Check logs created: `%AppData%\BundleApp\Logs\`

---

### Option 2: Test WITH SolidWorks (Full Test)
```powershell
# 1. Start SolidWorks first
# (Wait for it to fully load - not just splash screen)

# 2. Launch Bundle
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug"
.\Bundle.exe

# 3. Enter test configuration (see below)

# 4. Click "Create Bundle"

# 5. Watch SolidWorks generate 21 files!
```

---

## ?? TEST CONFIGURATION

### Basic Test Values
Use these values for your first test:

| Field | Value | Notes |
|-------|-------|-------|
| **Job Number** | `S25TEST` | Test job number |
| **Bundle Width** | `48.500` | inches |
| **Side Frame THK** | `0.375` | inches |
| **Side Frame Depth** | `4.000` | inches |
| **Tube Length** | `96.000` | inches |
| **Tube OD** | `1.000` | inch |
| **Tube Wall THK** | `0.035` | inch |
| **Tube Row 1** | `8` | tubes |
| **Tube Row 2** | `7` | tubes |
| **Tube Horiz Pitch** | `2.500` | inches |

Leave other fields at their default values.

---

## ??? WHAT FILES WILL BE CREATED

When you click "Create Bundle", the system will generate **21 files**:

### Main Assembly
- `S25TEST-7.SLDASM` - Main bundle assembly
- `S25TEST-7.SLDDRW` - Drawing (3.6 MB)

### Side Frames (4 files)
- `S25TEST-1011.SLDPRT` - Side frame left
- `S25TEST-1011W.SLDASM` - Side frame left weldment
- `S25TEST-1012.SLDPRT` - Side frame right
- `S25TEST-1012W.SLDASM` - Side frame right weldment

### Air Seals (4 files)
- `S25TEST-1013.SLDPRT` - Top front air seal
- `S25TEST-1014.SLDPRT` - Top rear air seal
- `S25TEST-1015.SLDPRT` - Bottom front air seal
- `S25TEST-1016.SLDPRT` - Bottom rear air seal

### Tube Supports (5 files)
- `S25TEST-1504W.SLDASM` - Tube support weldment
- `S25TEST-1504B.SLDPRT` - Tube support base
- `S25TEST-1504P.SLDPRT` - Tube support part
- `S25TEST-1560.SLDASM` - Mounting angle assembly
- `S25TEST-1560P.SLDPRT` - Mounting angle part

### Tube Keepers (3 files)
- `S25TEST-1561L.SLDPRT` - Tube keeper bent
- `S25TEST-1561P.SLDPRT` - Tube keeper pipe
- `S25TEST-1505.SLDPRT` - Tube keeper end plate

### Miscellaneous (4 files)
- `S25TEST-Tube.SLDPRT` - Tube template
- `S25TEST-Pstrip.SLDPRT` - P-strip template
- `S25TEST-1300HPC.SLDPRT` - Lifting lug (if required)

**Total: 21 CAD files created from templates!**

---

## ?? TEMPLATE LOCATION

Your templates are located at:
```
C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Bundle\
```

**21 Template Files Available:**
? HUD_JOBNO-7.SLDASM (Main)
? HUD_JOBNO-7.SLDDRW (Drawing)
? HUD_JOBNO-1011.SLDPRT (Side frame L)
? HUD_JOBNO-1011W.SLDASM (Side frame L weldment)
? HUD_JOBNO-1012.SLDPRT (Side frame R)
? HUD_JOBNO-1012W.SLDASM (Side frame R weldment)
? HUD_JOBNO-1013.sldprt (Air seal TF)
? HUD_JOBNO-1014.sldprt (Air seal TR)
? HUD_JOBNO-1015.sldprt (Air seal BF)
? HUD_JOBNO-1016.sldprt (Air seal BR)
? HUD_JOBNO-1300HPC.sldprt (Lifting lug)
? HUD_JOBNO-1504B.sldprt (TS base)
? HUD_JOBNO-1504P.sldprt (TS part)
? HUD_JOBNO-1504W.SLDASM (TS weldment)
? HUD_JOBNO-1505.sldprt (TK end plate)
? HUD_JOBNO-1560.SLDASM (Mounting angle)
? HUD_JOBNO-1560P.sldprt (Mounting part)
? HUD_JOBNO-1561L.sldprt (TK bent)
? HUD_JOBNO-1561P.sldprt (TK pipe)
? HUD_JOBNO-Pstrip.sldprt (P-strip)
? HUD_JOBNO-Tube.sldprt (Tube)

---

## ?? TESTING SCENARIOS

### Test 1: UI Launch (No SolidWorks) ? EASIEST
**Purpose**: Verify the application starts correctly

**Steps:**
1. Close SolidWorks (if running)
2. Launch `Bundle.exe`
3. Observe the UI

**Expected Results:**
- ? Form displays with all input fields
- ? No crash or error messages
- ? All fields are editable
- ? Buttons are clickable

**Pass/Fail**: Application launches without errors

---

### Test 2: SolidWorks Detection (SW Not Running) ? IMPORTANT
**Purpose**: Verify graceful error handling

**Steps:**
1. Ensure SolidWorks is **NOT** running
2. Launch `Bundle.exe`
3. Click "Create Bundle" button
4. Observe the response

**Expected Results:**
- ? Dialog appears: "SolidWorks is not currently running..."
- ? Shows "Retry" and "Cancel" buttons
- ? Application does NOT crash
- ? Can click "Cancel" and continue using UI

**Pass/Fail**: Graceful error dialog (not crash)

---

### Test 3: Log File Creation ? CRITICAL
**Purpose**: Verify logging system works

**Steps:**
1. Launch `Bundle.exe`
2. Navigate to: `%AppData%\BundleApp\Logs\`
   - Or: `C:\Users\DCornealius\AppData\Roaming\BundleApp\Logs\`
3. Check for log files

**Expected Results:**
- ? Log directory exists
- ? Log file created with timestamp name
- ? File contains timestamped entries
- ? Shows "Application Started" entry

**Pass/Fail**: Log file exists and is readable

---

### Test 4: Import Prego (Excel Integration) ? IMPORTANT
**Purpose**: Verify Excel automation works

**Steps:**
1. Close Excel (if running)
2. Launch `Bundle.exe`
3. Click "Import Prego" button
4. Select an example Prego file (if available)
5. Check Task Manager for Excel processes

**Expected Results:**
- ? Excel starts automatically (if needed)
- ? Data imports successfully
- ? Fields populate in UI
- ? No errors
- ? When you close Bundle, Excel closes properly
- ? No zombie EXCEL.EXE processes remain

**Pass/Fail**: Excel integration works and cleans up

---

### Test 5: Bundle Generation (WITH SolidWorks) ??? FULL TEST
**Purpose**: Verify complete automation workflow

**Prerequisites:**
- ? SolidWorks 2022+ installed
- ? Templates exist in `templates/hudson_certified/Bundle/`
- ? SolidWorks is fully loaded (not just splash screen)

**Steps:**
1. **Start SolidWorks**
   - Wait until fully loaded
   
2. **Launch Bundle.exe**
   ```powershell
   cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug"
   .\Bundle.exe
   ```

3. **Enter Configuration:**
   - Job Number: `S25TEST`
   - Bundle Width: `48.500`
   - Side Frame THK: `0.375`
   - Side Frame Depth: `4.000`
   - Tube Length: `96.000`
   - Tube OD: `1.000`
   - Tube Wall THK: `0.035`
   - Tube Row 1: `8`
   - Tube Row 2: `7`
   - Tube Horiz Pitch: `2.500`

4. **Click "Create Bundle"**

5. **Watch the Process:**
   - Progress indicator (if visible)
   - SolidWorks opens template files
   - Files are modified
   - Assembly builds

6. **Verify Results:**
   - Check output folder
   - Verify 21 files created
   - Open main assembly (S25TEST-7.SLDASM)
   - Check dimensions

**Expected Results:**
- ? Process completes without errors
- ? All 21 files created with correct names
- ? Assembly opens in SolidWorks
- ? Dimensions match input values
- ? All components properly constrained
- ? Drawing updates correctly
- ? No SolidWorks crash
- ? Log file shows success

**Expected Time:** 5-15 minutes (depending on computer speed)

**Pass/Fail**: All 21 files created correctly

---

### Test 6: Validation & Edge Cases ? ADVANCED
**Purpose**: Test error handling and validation

**Test Cases:**

#### 6a. Empty Job Number
- Leave Job Number blank
- Click "Create Bundle"
- **Expected**: Error message or validation warning

#### 6b. Invalid Dimensions
- Enter negative number for Bundle Width
- Click "Create Bundle"
- **Expected**: Validation error

#### 6c. Zero Tubes
- Enter `0` for Tube Row 1
- Enter `0` for Tube Row 2
- **Expected**: Warning or error

#### 6d. Missing Templates
- Temporarily rename template folder
- Try to create bundle
- **Expected**: Clear error message about missing templates

---

## ?? SUCCESS CRITERIA

### ? ALL TESTS PASS IF:
1. ? Application launches without crashing
2. ? SolidWorks detection works (shows retry dialog)
3. ? Log files are created
4. ? Excel integration works (no zombies)
5. ? Bundle generation creates all 21 files
6. ? Assembly opens correctly in SolidWorks
7. ? Dimensions are accurate
8. ? No errors in log files

### ?? PARTIAL PASS IF:
- UI works but SolidWorks generation has minor issues
- Most files created but 1-2 have problems
- Dimensions slightly off (may need calibration)

### ? FAIL IF:
- Application crashes on startup
- No error handling (crashes on missing SW)
- No log files created
- Excel zombies remain
- Generation fails completely
- Major errors in log files

---

## ?? TROUBLESHOOTING

### Problem: "Bundle.exe not found"
**Solution:**
```powershell
# Build the project first
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation"
& "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" Bundle\Bundle.csproj /p:Configuration=Debug
```

### Problem: Application crashes on startup
**Solution:**
1. Check log files: `%AppData%\BundleApp\Logs\`
2. Look for specific error message
3. Verify all dependencies (FileTools.dll, Excel.dll, etc.)
4. Try running as Administrator

### Problem: "SolidWorks API not available"
**Solution:**
1. Ensure SolidWorks is **fully loaded** (not just splash screen)
2. Check SolidWorks version (needs 2022+)
3. Verify SolidWorks API is enabled
4. Try launching SolidWorks as Administrator

### Problem: Templates not found
**Solution:**
1. Verify templates exist:
   ```powershell
   Test-Path "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Bundle\HUD_JOBNO-7.SLDASM"
   ```
2. Check template path in code
3. Verify folder permissions

### Problem: Excel doesn't close
**Solution:**
1. Check if Excel was already running (it won't close pre-existing instances)
2. Verify `ComObjectManager.ReleaseAll()` is called
3. Check log for cleanup messages
4. Manually close Excel and try again

### Problem: Files created in wrong location
**Solution:**
1. Check output path configuration
2. Look for files in Desktop or Documents
3. Check log file for actual save location
4. Verify write permissions to target folder

---

## ?? TEST REPORT TEMPLATE

After testing, document your results:

```markdown
# Bundle Assembly Test Report

**Date**: [Date]
**Tester**: [Your Name]
**SolidWorks Version**: [Version]

## Test Results

| Test # | Test Name | Result | Notes |
|--------|-----------|--------|-------|
| 1 | UI Launch | ? PASS / ? FAIL | |
| 2 | SW Detection | ? PASS / ? FAIL | |
| 3 | Log Creation | ? PASS / ? FAIL | |
| 4 | Excel Import | ? PASS / ? FAIL | |
| 5 | Bundle Generation | ? PASS / ? FAIL | |
| 6 | Validation | ? PASS / ? FAIL | |

## Files Created
- [ ] All 21 files created
- [ ] Correct naming (S25TEST-*.*)
- [ ] Assembly opens correctly
- [ ] Dimensions accurate

## Issues Found
[List any problems encountered]

## Overall Status
? READY FOR PRODUCTION
?? MINOR ISSUES (list below)
? NEEDS WORK (list issues)

## Recommendations
[Your recommendations]
```

---

## ?? QUICK COMMANDS

### Launch Bundle
```powershell
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Bundle\bin\Debug"
.\Bundle.exe
```

### Check if SolidWorks Running
```powershell
Get-Process -Name "SLDWORKS" -ErrorAction SilentlyContinue
```

### View Latest Log
```powershell
$logPath = "$env:APPDATA\BundleApp\Logs"
$latestLog = Get-ChildItem $logPath -Filter "*.log" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
notepad $latestLog.FullName
```

### Open Log Directory
```powershell
Start-Process "$env:APPDATA\BundleApp\Logs"
```

### Kill Zombie Excel
```powershell
Get-Process -Name "EXCEL" -ErrorAction SilentlyContinue | Stop-Process -Force
```

### Check Templates Exist
```powershell
Get-ChildItem "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\Bundle\" | Measure-Object
# Should show 21 files
```

---

## ?? RELATED DOCUMENTATION

- **Quick Reference**: `macros/csharp/Solidworks-Automation/docs/Testing/TESTING_QUICK_REFERENCE.md`
- **Full Testing Guide**: `macros/csharp/Solidworks-Automation/docs/Testing/TESTING_GUIDE.md`
- **Integration Guide**: `macros/csharp/Solidworks-Automation/UNIFIEDUI_BUNDLE_INTEGRATION_GUIDE.md`
- **Architecture**: `macros/csharp/Solidworks-Automation/docs/Architecture/BUNDLE_DUAL_APPROACH_ANALYSIS.md`

---

## ?? NEXT STEPS AFTER SUCCESSFUL TEST

### If All Tests Pass:
1. ? **Production Ready!** Start using for real projects
2. ?? **Document** any customizations needed
3. ?? **Train** other team members
4. ?? **Integrate** with other components (Header, Hood, etc.)
5. ?? **Track** time savings and ROI

### If Tests Partially Pass:
1. ?? **Review** log files for specific errors
2. ??? **Fix** identified issues
3. ?? **Re-test** failed scenarios
4. ?? **Document** workarounds if needed

### If Tests Fail:
1. ?? **Collect** all error messages and logs
2. ?? **Review** troubleshooting section
3. ?? **Ask for help** with specific error details
4. ??? **Debug** one issue at a time

---

## ? FINAL CHECKLIST

Before declaring success, verify:

- [ ] Application launches without errors
- [ ] UI displays correctly
- [ ] SolidWorks detection works
- [ ] Retry dialog appears (when SW not running)
- [ ] Log files created and readable
- [ ] Excel integration works
- [ ] Excel cleanup works (no zombies)
- [ ] With SolidWorks: All 21 files created
- [ ] Assembly opens correctly
- [ ] Dimensions match inputs
- [ ] No errors in log files
- [ ] Tested with sample configuration
- [ ] Validated edge cases
- [ ] Documented any issues

---

## ?? SUCCESS!

If all tests pass, congratulations! You have a **production-ready Bundle assembly automation system** that:

? **Automates** 21 file creation process  
? **Validates** inputs and handles errors gracefully  
? **Logs** everything for troubleshooting  
? **Integrates** with SolidWorks and Excel  
? **Saves** massive amounts of time  

**Estimated Time Savings**: 70-85% reduction in bundle creation time!

---

**Test Guide Version**: 1.0  
**Last Updated**: October 27, 2025  
**Component Status**: ? Built and Ready  
**Documentation Status**: ? Complete

---

**READY TO TEST? START WITH TEST 1! ??**





