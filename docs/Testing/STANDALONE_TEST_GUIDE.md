# Standalone Testing Guide - Automation Guy Add-In

## Yes, You Can Test Without the Vault! ?

Your project is now **fully standalone** and ready to test locally without any connection to `C:\AXC_VAULT\`.

## Quick Start

### 1. Clean and Rebuild
```powershell
# Navigate to solution folder
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation"

# Delete old DLL (from previous D.X.C. rename)
Remove-Item "Solidworks Add-In\bin\Debug\D.X.C..dll" -ErrorAction SilentlyContinue

# Clean and rebuild
msbuild "Solidworks Automation.sln" /t:Clean
msbuild "Solidworks Automation.sln" /t:"Solidworks Add-In" /p:Configuration=Debug
```

### 2. Verify Build Output
After successful build, check for:
```
Solidworks Add-In\bin\Debug\Automation Guy.dll
```

### 3. Register the Add-In with SolidWorks

#### Option A: Manual Registration (Recommended for Testing)
1. Open SolidWorks
2. Go to **Tools** > **Add-Ins**
3. Click **Browse** and navigate to:
   ```
   C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\Solidworks Add-In\bin\Debug\Automation Guy.dll
   ```
4. Select the add-in and check both boxes:
   - ? Load at startup
   - ? Active

#### Option B: Automatic Registry Registration (Run as Administrator)
The add-in has post-build events that should register it automatically, but you can also run:
```powershell
# Run Visual Studio as Administrator, then build
# The post-build event calls regasm to register the COM add-in
```

### 4. Test the Add-In

#### Basic Functionality Test:
1. **Open SolidWorks** (must be installed on your machine)
2. **Look for the taskpane** - Should show "Automation Guy" title
3. **Try creating a simple component**:
   - The add-in should now reference local templates from:
     ```
     C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\templates\hudson_certified\
     ```

#### Expected Behavior:
- ? Add-in loads without vault connection errors
- ? Templates read from local `templates/` folder
- ? Outputs save to local `output/` folder
- ? No modifications to `C:\AXC_VAULT\`

## What Will Work Standalone

### ? WILL WORK:
- **Template-based automation** (Walkway, Hood, MachineryMount modules)
  - Uses templates from `templates/hudson_certified/`
- **Header Section Tool**
  - Reads from `templates/header_section_tool/`
- **Structure Tools** (XCH, Z)
  - Reads from `templates/xch_structure_tool/`, `templates/z_structure_tool/`
- **File operations**
  - All file I/O goes to local `output/` folders
- **Configuration**
  - All settings in `config.json` point to local paths

### ?? MAY NEED SETUP:
- **Design Library Components** (Bearings, Fan Guards, etc.)
  - Code now looks in `templates/common_library/`
  - You need to copy these files from the vault manually (one-time setup)
  - See "Design Library Setup" below

### ? WON'T WORK:
- **Vault-specific features** (PDM check-in/check-out)
  - The `AXC_Vault.dll` module may have vault dependencies
  - You can disable or skip vault-related operations

## Design Library Setup (Optional)

If you need bearing, fan guard, or sensor parts:

```powershell
# Create local design library structure
New-Item -ItemType Directory -Force -Path "templates\common_library\Bearings"
New-Item -ItemType Directory -Force -Path "templates\common_library\Fan Guards"
New-Item -ItemType Directory -Force -Path "templates\common_library\Fan Rings"
New-Item -ItemType Directory -Force -Path "templates\common_library\Vibration Sensors"

# Then manually copy .SLDPRT files from vault to these folders
# (You'll need read access to the vault to copy files)
```

## Troubleshooting

### Build Fails
**Error**: "D.X.C..dll" still exists
```powershell
Remove-Item "Solidworks Add-In\bin\Debug\D.X.C..dll" -Force
msbuild "Solidworks Automation.sln" /t:Clean
msbuild "Solidworks Automation.sln" /t:Rebuild /p:Configuration=Debug
```

### Add-In Doesn't Load
**Issue**: SolidWorks doesn't show the add-in

1. **Check registry** (run as Admin):
   ```powershell
   Get-ItemProperty "HKLM:\SOFTWARE\SolidWorks\AddIns\*" | Where-Object {$_.Title -like "*Automation*"}
   ```

2. **Re-register manually**:
   ```powershell
   cd "Solidworks Add-In\bin\Debug"
   & "$env:SystemRoot\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe" "Automation Guy.dll" /codebase
   ```

### Missing Templates
**Error**: "Template not found"

1. Verify templates exist:
   ```powershell
   Get-ChildItem "templates\hudson_certified" -Recurse -Include "*.SLDPRT","*.SLDASM","*.SLDDRW"
   ```

2. Check `config.json` paths match your local structure

### Missing Design Library Parts
**Error**: "File not found: C:\Users\...\Bearings\..."

- This is expected if you haven't copied design library files
- Either copy the files (see "Design Library Setup") or comment out the code that uses them

## Testing Checklist

- [ ] Build succeeds without errors
- [ ] `Automation Guy.dll` exists in bin\Debug
- [ ] SolidWorks loads the add-in
- [ ] Taskpane appears with "Automation Guy" title
- [ ] Can browse local templates folder
- [ ] Config.json loads successfully
- [ ] No vault connection attempts
- [ ] Outputs save to local `output/` folder

## Performance Notes

**Standalone mode is FASTER** than vault mode because:
- No network PDM vault connection overhead
- No check-in/check-out operations
- Direct local file system access
- No vault API calls

## Next Steps After Testing

1. **Commit your changes** (vault isolation updates)
2. **Document which features you use** most
3. **Create design library backup** (copy needed parts once)
4. **Set up automated tests** for your most common workflows

---
**Generated**: 2025-10-28  
**Status**: Ready for standalone testing  
**Dependencies**: SolidWorks, Visual Studio 2022, .NET Framework 4.8
