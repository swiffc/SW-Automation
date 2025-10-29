# ?? Bug Fix Workflow Template

**Purpose:** Step-by-step checklist for fixing bugs systematically  
**Last Updated:** October 28, 2025

---

## ?? **PHASE 1: REPRODUCE**

### Gather Information
- [ ] **Error Message:** _______________
- [ ] **Component:** [UnifiedUI / Bundle / Header / Other]
- [ ] **When:** [Startup / Button Click / During Generation / Other]
- [ ] **Frequency:** [Always / Sometimes / Once]

### Find Log File
```powershell
# Check most recent log
Get-ChildItem "$env:APPDATA\*App\Logs" -Recurse -Filter "*.log" | 
    Sort-Object LastWriteTime -Descending | 
    Select-Object -First 1
```

- [ ] **Log File Location:** _______________
- [ ] **Relevant Error Lines:** (copy here)

### Steps to Reproduce
1. _______________
2. _______________
3. _______________

**Expected Behavior:** _______________  
**Actual Behavior:** _______________

---

## ?? **PHASE 2: DIAGNOSE**

### Search Codebase
```powershell
# Find error message in code
Select-String -Path *.cs -Pattern "<error-text>" -Recurse -Context 3
```

- [ ] **File:** _______________
- [ ] **Line:** _______________
- [ ] **Method:** _______________

### Check Recent Changes
```bash
# See recent commits to this file
git log --oneline --follow -n 10 -- <file-path>
```

- [ ] **Last Modified:** _______________
- [ ] **Recent Changes:** _______________

### Identify Root Cause
- [ ] **Null reference** (missing null check)
- [ ] **COM error** (SolidWorks/Excel not available)
- [ ] **File not found** (path incorrect)
- [ ] **Invalid cast** (type mismatch)
- [ ] **Logic error** (wrong calculation)
- [ ] **Other:** _______________

---

## ?? **PHASE 3: FIX**

### Create Branch
```bash
git checkout -b fix/<short-description>
```

- [ ] **Branch Name:** `fix/_______________`

### Apply Minimal Fix
Choose appropriate pattern:

#### **Pattern 1: Add Null Check**
```csharp
// BEFORE
var result = someObject.Property;

// AFTER
if (someObject == null)
{
    GlobalErrorHandler.LogWarning("Object is null");
    return; // or default value
}
var result = someObject.Property;
```

#### **Pattern 2: Add Error Handling**
```csharp
// BEFORE
var result = RiskyOperation();

// AFTER
try
{
    var result = RiskyOperation();
}
catch (Exception ex)
{
    GlobalErrorHandler.LogError(ex, "RiskyOperation context");
    MessageBox.Show($"Error: {ex.Message}\n\nSee log for details.", 
        "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
}
```

#### **Pattern 3: Fix Logic Error**
```csharp
// BEFORE (wrong calculation)
var total = quantity * price / discount;

// AFTER (correct calculation)
var total = (quantity * price) * (1 - discount);
```

#### **Pattern 4: Add COM Safety**
```csharp
// BEFORE
var swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");

// AFTER
if (!IsSolidWorksAvailable())
{
    MessageBox.Show("Please start SolidWorks", "Error");
    return;
}
var swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
```

### Update Documentation
- [ ] Add XML comments if method is public
- [ ] Update relevant .md file if behavior changed
- [ ] Add to error_solutions.md if new error pattern

### Add Logging
```csharp
GlobalErrorHandler.LogInfo($"Starting {operation}...");
// ... operation ...
GlobalErrorHandler.LogInfo($"? {operation} completed");
```

---

## ? **PHASE 4: VERIFY**

### Build Verification
```powershell
cd macros\csharp\Solidworks-Automation
& "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" `
  UnifiedUI\UnifiedUI.csproj /p:Configuration=Debug /v:minimal /nologo
```

- [ ] **Build Status:** [? Success / ? Failed]
- [ ] **Errors:** [0 / _____ errors]
- [ ] **Warnings:** [_____ warnings - acceptable]

### Manual Testing
1. [ ] Stop all running instances
```powershell
Stop-Process -Name "UnifiedUI","Bundle","Header" -Force
```

2. [ ] Launch application
3. [ ] Reproduce original bug scenario
4. [ ] Verify fix works

- [ ] **Bug Fixed:** [Yes / No]
- [ ] **No New Issues:** [Yes / No]

### Log File Verification
```powershell
# Check new log file
Get-ChildItem "$env:APPDATA\UnifiedUIApp\Logs" -Filter "*.log" | 
    Sort-Object LastWriteTime -Descending | 
    Select-Object -First 1 | 
    Get-Content -Tail 20
```

- [ ] No unexpected errors in log
- [ ] Operation logged correctly
- [ ] Error messages clear and helpful

---

## ?? **PHASE 5: COMMIT**

### Pre-Commit Checklist
- [ ] Build succeeds (0 errors)
- [ ] Manual test passed
- [ ] Log file reviewed
- [ ] No debug code left (Console.WriteLine, etc.)
- [ ] No commented-out code (unless explanation)
- [ ] Files organized (none in root)

### Commit Message Format
```
fix: <what-was-broken> (<component>)

<detailed-explanation>

Root Cause: <why-it-broke>
Solution: <how-you-fixed-it>
Impact: <what-components-affected>

Fixes #<issue-number>
```

**Example:**
```
fix: NullReferenceException in Prego import (UnifiedUI)

Added null check for HeaderAppData before calling 
ImportHeaderData_FromPrego(). Now gracefully skips 
header import if Prego isn't initialized.

Root Cause: Prego Excel file not opened before import
Solution: Check PregoDoc != null before import
Impact: UnifiedUI bundle generation

Fixes #123
```

### Create Commit
```bash
git add <files>
git commit -m "fix: <message>"
```

- [ ] **Commit SHA:** _______________

---

## ?? **PHASE 6: PULL REQUEST**

### PR Template
```markdown
## ?? Bug Fix: <Short Description>

### Problem
Describe the bug that was fixed.

### Root Cause
Explain why the bug occurred.

### Solution
Describe how you fixed it.

### Testing
- [x] Build succeeds (0 errors)
- [x] Manual test passed
- [x] Log file verified
- [ ] Tested by: _______________

### Impact
- **Components Affected:** UnifiedUI, Bundle, etc.
- **Breaking Changes:** None
- **Risk Level:** Low / Medium / High

### Screenshots
(if applicable)

### Checklist
- [x] Code follows .cursorrules guidelines
- [x] Error handling added
- [x] Logging updated
- [x] Documentation updated
- [x] No hardcoded paths
- [x] COM objects properly disposed
```

### Push and Create PR
```bash
git push origin fix/<short-description>
# Then create PR on GitHub
```

- [ ] **PR Number:** #_______________
- [ ] **Reviewer:** _______________

---

## ?? **VERIFICATION MATRIX**

| Check | Status | Notes |
|-------|--------|-------|
| Build succeeds | [ ] | 0 errors required |
| Manual test passed | [ ] | Original scenario works |
| Log file clean | [ ] | No unexpected errors |
| No new warnings | [ ] | (or acceptable) |
| Documentation updated | [ ] | If behavior changed |
| PR created | [ ] | With test instructions |

---

## ?? **SUCCESS CRITERIA**

**Bug is considered FIXED when:**
1. ? Original error no longer occurs
2. ? Build succeeds with 0 errors
3. ? Manual test confirms fix
4. ? Log file shows no new errors
5. ? No regressions introduced
6. ? Code follows project standards

---

**Template Version:** 1.0  
**Last Updated:** October 28, 2025  
**Status:** ? Production Ready

