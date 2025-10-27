# Getting Started with this SolidWorks Automation Repository

This is a comprehensive SolidWorks automation suite with multiple specialized add-ins and tools.

---

## ?? What's Included

### Core Add-in
- **SolidWorks Add-In** - Main add-in that loads into SolidWorks with task pane integration

### Specialized Tools (20 Projects Total)
1. **Walkway** - Automated walkway design
2. **Hood** - Hood design and automation
3. **Plenum** - Plenum structure automation
4. **Structure** - Structural elements (columns, braces)
5. **Bundle** - Bundle design (air seals, side frames, tube supports)
6. **Header** - Header design (box, connections, foot)
7. **MachineryMount** - Machinery mounting systems
8. **Bounty** - Bounty-specific automation
9. **Fork** - Configuration management
10. **Walkway** - Walkway systems

### Utilities & Libraries
- **ModelTools** - SolidWorks model manipulation utilities
- **FileTools** - File management and operations
- **UserInterface** - UI components and helpers
- **Excel** - Excel integration tools
- **Vault** - PDM vault integration (EPDM)

### Support Tools
- **AddinInstaller** - WPF installer for the add-in
- **AddInUpdater** - Auto-update functionality
- **AddInDllVersionControl** - Version control for DLLs
- **SplashScreen** - Loading splash screen
- **Universal Drawing Tool** - Drawing automation
- **Testing** - Test harness

---

## ?? Quick Start

### Step 1: Open in Visual Studio

1. **Open Visual Studio 2022**
2. **File ? Open ? Project/Solution**
3. Navigate to: `C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\`
4. Open **`Solidworks Automation.sln`**

### Step 2: Restore NuGet Packages

1. **Right-click on Solution** in Solution Explorer
2. Select **Restore NuGet Packages**
3. Wait for packages to download

### Step 3: Update SolidWorks Interop References

The repository includes SolidWorks interop DLLs in `Libraries/Solidworks Interop/` folder.

**If you have a different SolidWorks version:**
1. Right-click each project ? **Manage NuGet Packages**
2. Update to your SolidWorks version:
   - `SolidWorks.Interop.sldworks`
   - `SolidWorks.Interop.swconst`
   - `SolidWorks.Interop.swpublished`

### Step 4: Build the Solution

1. **Set Configuration** to **Debug | Any CPU** (or x64 if needed)
2. **Build ? Build Solution** (Ctrl+Shift+B)
3. Watch for errors in Output window

**Note:** Some projects may not build initially due to:
- Missing references specific to the original development environment
- Custom library paths
- EPDM vault dependencies

### Step 5: Focus on Main Projects

Start with these core projects that are most likely to build successfully:

1. **ModelTools** - Utility library (should build cleanly)
2. **FileTools** - File operations (should build cleanly)
3. **SolidWorks Add-In** - Main add-in (requires SolidWorks)

---

## ?? Configuration

### Setting Up Debug Launch

For the **SolidWorks Add-In** project:

1. Right-click **SolidWorks Add-In** project ? **Properties**
2. **Debug** tab:
   - Start external program: `C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe`
   - (Adjust path for your SolidWorks installation)
3. **Build** tab:
   - Platform target: **x64** (if using 64-bit SolidWorks)
   - Uncheck "Prefer 32-bit"

### Post-Build Registration

The projects should have post-build events that register the DLLs with COM:
```cmd
"$(FrameworkDir64)v4.0.30319\RegAsm.exe" /codebase "$(TargetPath)"
```

If this fails, you may need to run Visual Studio as **Administrator**.

---

## ?? Key Projects to Explore

### 1. SolidWorks Add-In (Main Entry Point)
Location: `Solidworks Add-In/`

This is the main add-in that loads into SolidWorks. Key files:
- **TaskpaneIntegration.cs** - Task pane setup
- **TaskpaneHostUI.cs** - UI host
- **DevUI.cs** - Development UI

### 2. ModelTools (Core Utilities)
Location: `ModelTools/`

Essential utilities for working with SolidWorks models:
- **AssemblyTools.cs** - Assembly manipulation
- **BendTable.cs** - Bend table operations
- **SteelBook.cs** - Steel specifications
- **ReleaseCOM.cs** - COM object cleanup

### 3. FileTools (File Operations)
Location: `FileTools/`

File management and operations:
- **StaticFileTools.cs** - File utilities
- **CommonData/** - Shared data structures
- **Base/** - Base classes

### 4. Specialized Design Tools
- **Hood/** - Hood design automation
- **Plenum/** - Plenum structures
- **Bundle/** - Bundle systems
- **Structure/** - Structural elements

---

## ??? Building Strategy

### Phase 1: Build Core Libraries (No SolidWorks Required)
Try building these in order:
1. ? **ModelTools**
2. ? **FileTools**
3. ? **UserInterface**

### Phase 2: Build Specialized Tools
4. **Hood**
5. **Plenum**
6. **Bundle**
7. **Structure**

### Phase 3: Build Main Add-in
8. **SolidWorks Add-In**

### Phase 4: Utilities
9. **AddinInstaller**
10. **Testing**

---

## ?? Common Issues

### Issue: Missing References
**Error:** "Could not load file or assembly..."

**Solution:**
- Check `Libraries/` folder for required DLLs
- Update references to point to correct paths
- Some projects may reference each other

### Issue: EPDM/Vault Errors
**Error:** References to `EdmLib` or vault operations fail

**Solution:**
- If you don't use PDM Vault, you can exclude these projects:
  - `AXC_Vault`
  - `Vault`
- Or install EPDM API from SolidWorks installation

### Issue: COM Registration Fails
**Error:** Post-build event fails with "Access denied"

**Solution:**
- Run Visual Studio as **Administrator**
- Or disable post-build event temporarily

### Issue: Different SolidWorks Version
**Error:** Interop version mismatch

**Solution:**
- Replace interop DLLs with your version from:
  `C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\api\redist\`
- Or use NuGet packages for your specific version

---

## ?? Testing the Add-in

### Step 1: Build Successfully
Make sure the **SolidWorks Add-In** project builds without errors.

### Step 2: Register the Add-in
- Build runs RegAsm automatically (if running as admin)
- Or manually: `RegAsm.exe /codebase "path\to\SolidWorks Add-In.dll"`

### Step 3: Open SolidWorks
1. Launch SolidWorks
2. **Tools ? Add-Ins**
3. Look for the add-in in the list
4. Check both boxes (load now and at startup)

### Step 4: Verify Loading
- Look for task pane or toolbar
- Check Tools menu for new commands
- Watch Visual Studio Output window for debug messages

---

## ?? Understanding the Architecture

### Add-in Structure
```
SolidWorks Add-In (Main)
??? Loads into SolidWorks
??? Creates Task Pane
??? References specialized tools:
?   ??? Hood
?   ??? Plenum
?   ??? Bundle
?   ??? etc.
??? Uses utilities:
    ??? ModelTools
    ??? FileTools
    ??? UserInterface
```

### Data Flow
1. User interacts with Task Pane UI
2. UI calls specialized tool (e.g., Hood.cs)
3. Tool uses ModelTools for SolidWorks operations
4. FileTools handles file I/O
5. Results displayed in UI or SolidWorks

---

## ?? Next Steps

1. ? **Build core libraries first** (ModelTools, FileTools)
2. ? **Study the code** - Look at ModelTools and FileTools to understand the architecture
3. ? **Pick one specialized tool** - Start with Hood or Structure
4. ? **Build incrementally** - Fix one project at a time
5. ? **Test in SolidWorks** - Once SolidWorks Add-In builds

---

## ?? Tips for Success

1. **Start Small** - Don't try to build everything at once
2. **Check Dependencies** - Projects reference each other; build in order
3. **Read the Code** - This is a great learning resource
4. **Backup First** - Make commits before making changes
5. **Document Issues** - Keep notes on build problems and solutions

---

## ?? Related Files

- **Standards/** - Contains standard parts and templates
- **commit_push.bat** - Git automation script
- **packages/** - NuGet package cache
- **Libraries/** - External DLL dependencies

---

## ?? Getting Help

If you encounter issues:

1. **Check Output Window** in Visual Studio for specific errors
2. **Review Error List** for compilation errors
3. **Check References** in each project's References folder
4. **Verify SolidWorks is running** when debugging the add-in
5. **Run Visual Studio as Administrator** for COM registration

---

**Good luck! This is a professional-grade automation suite that will teach you a LOT about SolidWorks API!** ??

