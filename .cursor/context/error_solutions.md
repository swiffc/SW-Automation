# ?? Error Solutions Database

**Purpose:** Instant solutions for common errors AI agents encounter  
**Last Updated:** October 28, 2025

---

## ?? **CRITICAL BUILD ERRORS**

### ? **"Type library not registered"**
```
Error CS0009: Metadata file '...' could not be opened
-- 'Type library not registered'
```

**Instant Fix:**
1. Close ALL Visual Studio instances
2. Run Visual Studio 2022 as **Administrator**
3. Clean Solution (Build > Clean Solution)
4. Rebuild Solution (Build > Rebuild Solution)

**Root Cause:** SolidWorks Interop DLLs require admin privileges to register.

---

### ? **"File locked by process"**
```
Error: Cannot access file '...\UnifiedUI.exe'
because it is being used by another process
```

**Instant Fix:**
```powershell
Stop-Process -Name "UnifiedUI","Bundle","Header" -Force
# Wait 2 seconds
Start-Sleep -Seconds 2
# Rebuild
```

**Root Cause:** Previous instance still running in background.

---

### ? **"CS0122: 'Bundle' is inaccessible due to its protection level"**
```
Error CS0122: 'Bundle.Bundle' is inaccessible
due to its protection level
```

**Instant Fix:**
```csharp
// Change in Bundle/Bundle.cs
internal class Bundle  // ? WRONG
public class Bundle    // ? CORRECT
```

**Root Cause:** Class is `internal` but needs to be `public` for cross-project access.

---

### ? **"CS0103: The name 'GlobalErrorHandler' does not exist"**
```
Error CS0103: The name 'GlobalErrorHandler' does not exist
in the current context
```

**Instant Fix:**
```csharp
// Add to top of file
using FileTools.Infrastructure;
```

**Root Cause:** Missing using directive.

---

## ?? **RUNTIME ERRORS**

### ? **NullReferenceException in Excel.dll**
```
System.NullReferenceException: Object reference not set
at Excel.Header_DataManager.ImportHeaderData_FromPrego()
```

**Instant Fix:**
```csharp
// BEFORE (crashes if Prego not open)
Excel.Header_DataManager.ImportHeaderData_FromPrego();

// AFTER (safe check)
if (Excel.Prego.PregoDoc != null && 
    Excel.Header_DataManager.HeaderAppData != null)
{
    Excel.Header_DataManager.ImportHeaderData_FromPrego();
}
else
{
    GlobalErrorHandler.LogWarning("Prego not initialized - skipping header import");
}
```

**Root Cause:** Prego Excel file not opened before attempting import.

---

### ? **"SolidWorks application not running"**
```
COMException: "Cannot get SolidWorks application"
```

**Instant Fix:**
```csharp
// BEFORE (crashes if SW not running)
var swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");

// AFTER (safe check)
if (!IsSolidWorksAvailable())
{
    MessageBox.Show("Please start SolidWorks first", "Error");
    return;
}
```

**Root Cause:** SolidWorks not running when component tries to connect.

---

### ? **"TargetInvocationException" in Bundle components**
```
System.Reflection.TargetInvocationException: 
Exception has been thrown by the target of an invocation
```

**Instant Fix:**
```csharp
// Add try-catch around instantiation
try
{
    var component = new SomeComponent(parent, config);
}
catch (Exception ex)
{
    GlobalErrorHandler.LogError(ex, "Component instantiation");
    // Continue with other components
}
```

**Root Cause:** Component constructor failed (often due to null parameters).

---

## ?? **XAML ERRORS**

### ? **MC3000: "Invalid character in the given encoding"**
```
Error MC3000: 'Invalid character in the given encoding.
Line 54 Position 20.'
```

**Instant Fix:**
```xml
<!-- BEFORE (emojis cause encoding issues) -->
<TextBlock Text="? Success"/>

<!-- AFTER (use text or HTML entities) -->
<TextBlock Text="Success"/>
<!-- OR -->
<TextBlock Text="&#x2705; Success"/>
```

**Root Cause:** XAML file saved with wrong encoding (not UTF-8).

**Alternative Fix:**
1. Open XAML in VS Code
2. Click encoding in bottom right
3. Select "Save with Encoding"
4. Choose "UTF-8 with BOM"

---

### ? **"Cannot locate resource"**
```
IOException: Cannot locate resource 'mainwindow.xaml'
```

**Instant Fix:**
1. Check Build Action in Properties
2. Should be: **Page** (not Embedded Resource)
3. Rebuild project

---

## ?? **COM ERRORS**

### ? **"Excel.exe remains in Task Manager"**
```
Problem: EXCEL.EXE still running after closing app
```

**Instant Fix:**
```csharp
// ENSURE THIS PATTERN:
Excel.Application excelApp = null;
Workbook workbook = null;
try
{
    excelApp = new Excel.Application();
    workbook = excelApp.Workbooks.Open(filePath);
    // Use workbook...
}
finally
{
    // CRITICAL: Release in reverse order
    if (workbook != null)
    {
        workbook.Close(false);
        Marshal.ReleaseComObject(workbook);
    }
    if (excelApp != null)
    {
        excelApp.Quit();
        Marshal.ReleaseComObject(excelApp);
    }
    GC.Collect();
    GC.WaitForPendingFinalizers();
}
```

**Root Cause:** COM objects not properly released.

---

### ? **"Memory leak detected"**
```
Problem: Memory usage grows continuously
```

**Instant Fix:**
```csharp
// Use ComObjectManager pattern
using (var comManager = new ComObjectManager())
{
    var swApp = comManager.GetSolidWorksApplication();
    // All COM objects auto-released when disposed
}
```

**Root Cause:** COM objects not tracked and released.

---

## ?? **DATA/CONFIGURATION ERRORS**

### ? **"Cell address not found"**
```
Exception: Cell 'DF32' not found in worksheet
```

**Instant Fix:**
```csharp
// Use fallback cells
var value = Excel.Prego.CellDouble(
    Excel.Prego.InputSheet,
    "DF32",  // Primary
    "DE32",  // Fallback 1
    "DF30"   // Fallback 2
);
```

**Root Cause:** Excel schema changed or cell doesn't exist.

---

### ? **"Path not found: C:\AXC_VAULT\..."**
```
DirectoryNotFoundException: Could not find path 'C:\AXC_VAULT\...'
```

**Instant Fix:**
Replace ALL hardcoded paths:
```csharp
// BEFORE (hardcoded production path)
var path = @"C:\AXC_VAULT\Active\{Job}\...";

// AFTER (use project output folder)
var basePath = @"C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\output";
var path = Path.Combine(basePath, job, "Drafting", "Headers", ...);
```

**Root Cause:** Development environment doesn't have access to production vault.

---

## ?? **GIT/BUILD ERRORS**

### ? **"Pre-commit hook failed"**
```
Error: pre-commit hook failed
File in root should be in subdirectory
```

**Instant Fix:**
```powershell
# Run organization script
.\ROOT-Organize.ps1
# Then try commit again
```

**Root Cause:** File placed in root but should be in subdirectory.

---

### ? **"Large file rejected by GitHub"**
```
Error: File size exceeds GitHub's 100 MB limit
```

**Instant Fix:**
1. Add to `.gitignore`:
```
# Large binary files
output/
templates/**/*.SLDASM
templates/**/*.SLDPRT
```
2. Remove from staging:
```bash
git rm --cached path/to/large/file
```

**Root Cause:** Trying to commit CAD template files (binary, large).

---

## ?? **QUICK DIAGNOSTIC COMMANDS**

### Check Build Status
```powershell
cd macros\csharp\Solidworks-Automation
& "C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\MSBuild.exe" `
  UnifiedUI\UnifiedUI.csproj /p:Configuration=Debug /v:q /nologo 2>&1 | 
  Select-String "error|Build succeeded"
```

### Check Running Processes
```powershell
Get-Process | Where-Object { 
    $_.ProcessName -in @("UnifiedUI","Bundle","Header","SLDWORKS","EXCEL") 
} | Format-Table ProcessName, Id, StartTime
```

### Find Recent Log File
```powershell
Get-ChildItem "$env:APPDATA\UnifiedUIApp\Logs" -Filter "*.log" | 
    Sort-Object LastWriteTime -Descending | 
    Select-Object -First 1 | 
    Get-Content -Tail 50
```

### Search for Error Pattern
```powershell
Select-String -Path *.cs -Pattern "TODO|FIXME|HACK|XXX" -Recurse
```

---

## ?? **ERROR PREVENTION CHECKLIST**

Before committing code, verify:
- [ ] Build succeeds (0 errors)
- [ ] All using statements present
- [ ] GlobalErrorHandler.LogError() for all exceptions
- [ ] Null checks before accessing objects
- [ ] COM objects properly disposed
- [ ] No hardcoded AXC_VAULT paths
- [ ] No emojis in XAML without UTF-8 encoding
- [ ] Files organized (not in root)

---

**Last Updated:** October 28, 2025  
**Maintainer:** AI Agents  
**Status:** ? Active Reference

