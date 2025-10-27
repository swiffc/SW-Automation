# ?? How to Run Everything - Complete Guide

**Date**: October 25, 2025  
**Status**: Integration Strategy

---

## ?? Current Project Structure

```
macros/csharp/Solidworks-Automation/
??? Solidworks Automation.sln        ? Main solution (20 projects)
??? Solidworks Add-In/               ? Main add-in (loads in SolidWorks)
??? Header/                          ? Header automation
??? Hood/                            ? Hood automation
??? FileTools/                       ? File utilities
??? [17 other projects]              ? Various tools
```

---

## ?? Two Systems Explained

### System 1: Existing SolidWorks Add-in (Current)
**Location**: `macros/csharp/Solidworks-Automation/`  
**Type**: SolidWorks Add-in (DLL)  
**How it runs**: Loads INSIDE SolidWorks as a taskpane

```
User Launches SolidWorks
    ?
SolidWorks loads add-in DLL
    ?
Add-in appears as taskpane in SolidWorks
    ?
User interacts with taskpane UI
    ?
Add-in controls SolidWorks directly
```

**Pros**:
? Integrated with SolidWorks
? Direct API access
? Appears as built-in tool

**Cons**:
? Requires SolidWorks running
? Complex to debug
? Harder to develop UI

---

### System 2: New Unified UI (To Build)
**Location**: `macros/csharp/SolidWorksAutomationUI/` (to create)  
**Type**: Standalone WPF Application  
**How it runs**: Independent application that CONTROLS SolidWorks

```
User Launches Standalone App
    ?
App shows unified UI (9 tabs)
    ?
User configures parameters
    ?
Click "Generate"
    ?
App launches/connects to SolidWorks
    ?
App sends commands to SolidWorks via API
    ?
SolidWorks generates the parts
```

**Pros**:
? Easy to develop and debug
? Can run without SolidWorks open
? Modern, clean UI
? Works on multiple machines

**Cons**:
? Requires SolidWorks installed
? Must connect via COM API

---

## ?? Integration Strategy - Best of Both Worlds

### Recommended Approach: **Hybrid System**

```
???????????????????????????????????????????????????
?  Option A: Standalone App (Primary)            ?
?  ????????????????????????????????????????????  ?
?  ?  Your New Unified UI (WPF App)           ?  ?
?  ?  - 9 Component Tabs                      ?  ?
?  ?  - Excel Integration                     ?  ?
?  ?  - Parameter Input                       ?  ?
?  ????????????????????????????????????????????  ?
?               ?                                 ?
?         SolidWorks COM API                      ?
?               ?                                 ?
?  ????????????????????????????????????????????  ?
?  ?  SolidWorks (running in background)      ?  ?
?  ????????????????????????????????????????????  ?
???????????????????????????????????????????????????

???????????????????????????????????????????????????
?  Option B: Add-in Taskpane (Alternative)       ?
?  ????????????????????????????????????????????  ?
?  ?  SolidWorks                               ?  ?
?  ?  ??????????????????????????????????????  ?  ?
?  ?  ? Your UI as Taskpane                ?  ?  ?
?  ?  ? (embedded in SolidWorks)           ?  ?  ?
?  ?  ??????????????????????????????????????  ?  ?
?  ????????????????????????????????????????????  ?
???????????????????????????????????????????????????
```

**Recommendation**: Build **Option A first**, then add **Option B later**

---

## ??? How to Build & Run

### Step 1: Build Existing Add-in (Current System)

```powershell
# Open in Visual Studio 2022
cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation
start "Solidworks Automation.sln"

# In Visual Studio:
1. Right-click solution ? Restore NuGet Packages
2. Build ? Build Solution (Ctrl+Shift+B)
3. Check output for "Build succeeded"
```

**Result**: Creates `Solidworks Add-In.dll` in `bin/Debug/`

**To Load in SolidWorks**:
```
1. SolidWorks ? Tools ? Add-ins
2. Check "SolidWorks Add-In"
3. Click OK
4. Taskpane appears in SolidWorks
```

---

### Step 2: Create New Unified UI (New System)

```powershell
# Create new WPF project
1. Visual Studio 2022 ? New Project
2. Search "WPF App"
3. Name: SolidWorksAutomationUI
4. Location: C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\
5. Framework: .NET 6.0
6. Create
```

**Project Structure**:
```
SolidWorksAutomationUI/
??? App.xaml
??? MainWindow.xaml          ? Main UI window
??? Services/
?   ??? ExcelService.cs      ? Read/write Excel
?   ??? SolidWorksService.cs ? Control SolidWorks
?   ??? ValidationService.cs ? Validate inputs
??? ViewModels/
?   ??? MainViewModel.cs
?   ??? BundleViewModel.cs
?   ??? HeaderViewModel.cs
??? Views/
?   ??? BundleTab.xaml
?   ??? HeaderTab.xaml
??? Models/
    ??? ComponentConfig.cs
```

**NuGet Packages to Install**:
```
Install-Package EPPlus                          # Excel
Install-Package CommunityToolkit.Mvvm           # MVVM
Install-Package SolidWorks.Interop.sldworks     # SolidWorks API
```

**To Run**:
```
F5 in Visual Studio ? Standalone app launches
```

---

## ?? How They Work Together

### Scenario 1: User Uses Standalone App
```
1. User launches SolidWorksAutomationUI.exe
2. Fills in Bundle parameters
3. Clicks "Generate"
4. App updates Excel file
5. App launches SolidWorks (if not running)
6. App tells SolidWorks: "Open this assembly"
7. SolidWorks regenerates from Excel
8. App saves result to output folder
```

**Code Example**:
```csharp
// In SolidWorksService.cs
public void GenerateBundle(BundleConfig config)
{
    // 1. Update Excel
    _excelService.WriteToExcel("Bundle.xlsx", config);
    
    // 2. Connect to SolidWorks
    var swApp = ConnectToSolidWorks();
    
    // 3. Open assembly
    var model = swApp.OpenDoc("HUD_JOBNO-7.SLDASM", 2);
    
    // 4. Rebuild
    model.ForceRebuild3(true);
    
    // 5. Save
    model.SaveAs("output/S2XXXX-7.SLDASM");
}
```

---

### Scenario 2: User Uses Add-in Taskpane
```
1. User opens SolidWorks
2. Add-in loads automatically
3. Taskpane shows same UI
4. User fills in parameters
5. Clicks "Generate"
6. Add-in directly controls SolidWorks (no COM needed)
```

---

## ?? Comparison Matrix

| Feature | Standalone App | Add-in Taskpane |
|---------|----------------|-----------------|
| **Development** | ? Easy | ?? Complex |
| **Debugging** | ? Easy | ? Hard |
| **UI Design** | ? Full control | ?? Limited space |
| **SolidWorks Access** | ?? Via COM | ? Direct |
| **Deployment** | ? Simple .exe | ?? DLL + registry |
| **User Experience** | ? Modern | ?? Feels old |
| **Run Without SW** | ? Yes (for config) | ? No |

---

## ?? Recommended Workflow

### Phase 1: Build Standalone App (Weeks 1-3)
```
? Easier to develop
? Faster iteration
? Better UI/UX
? Can test without SolidWorks constantly running
```

### Phase 2: Wrap as Add-in (Week 4)
```
? Take working standalone app
? Create thin add-in wrapper
? Embed UI as taskpane
? Users get both options
```

---

## ?? Quick Start - Standalone App

### Create Project Now:

```powershell
# 1. Create directory
cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp
mkdir SolidWorksAutomationUI

# 2. Create solution
dotnet new wpf -n SolidWorksAutomationUI -o SolidWorksAutomationUI
cd SolidWorksAutomationUI

# 3. Add NuGet packages
dotnet add package EPPlus
dotnet add package CommunityToolkit.Mvvm

# 4. Open in Visual Studio
start SolidWorksAutomationUI.sln
```

### First Code - Connect to SolidWorks:

```csharp
// SolidWorksService.cs
using SolidWorks.Interop.sldworks;
using System;
using System.Runtime.InteropServices;

public class SolidWorksService
{
    private SldWorks _swApp;
    
    public bool ConnectToSolidWorks()
    {
        try
        {
            // Try to get running instance
            _swApp = (SldWorks)Marshal.GetActiveObject("SldWorks.Application");
            return true;
        }
        catch
        {
            // Not running, start it
            Type swType = Type.GetTypeFromProgID("SldWorks.Application");
            _swApp = (SldWorks)Activator.CreateInstance(swType);
            _swApp.Visible = true;
            return true;
        }
    }
    
    public void OpenAndRebuild(string filePath)
    {
        if (_swApp == null)
            ConnectToSolidWorks();
            
        // Open document
        int errors = 0, warnings = 0;
        var model = _swApp.OpenDoc6(filePath, 
            (int)swDocumentTypes_e.swDocASSEMBLY,
            (int)swOpenDocOptions_e.swOpenDocOptions_Silent,
            "", ref errors, ref warnings);
        
        // Rebuild
        model.ForceRebuild3(true);
        
        // Save
        model.Save3(0, ref errors, ref warnings);
    }
}
```

---

## ?? UI Integration Options

### Option 1: Pure Standalone (Recommended First)
```
Launch: Double-click SolidWorksAutomationUI.exe
Location: Desktop, Start Menu, or quick launch
Uses: COM to control SolidWorks
```

### Option 2: SolidWorks Add-in
```
Launch: Automatic when SolidWorks starts
Location: SolidWorks taskpane
Uses: Direct SolidWorks API
```

### Option 3: Hybrid (Best Long-term)
```
Standalone App:
- For batch processing
- For configuration/planning
- When SolidWorks not needed

Add-in:
- For live modeling
- For integrated workflow
- When working in SolidWorks
```

---

## ?? Action Plan

### Today:
```
1. ? Understand current system
2. ? Decide: Standalone vs Add-in first
3. ? Create new WPF project
4. ? Test SolidWorks connection
```

### This Week:
```
1. Build main window with tabs
2. Implement Bundle tab
3. Connect to Excel
4. Test with SolidWorks
```

### Next Week:
```
1. Add more tabs
2. Implement validation
3. Add preview panel
4. Polish UI
```

---

## ?? Development Environment

### Required:
- ? Visual Studio 2022 (installed)
- ? .NET 6.0 SDK (installed)
- ? SolidWorks (installed)
- ? NuGet packages (to install)

### Optional but Helpful:
- Git (version control)
- SolidWorks API Documentation
- WPF tutorials/references

---

## ? FAQ

**Q: Can I use the existing add-in code?**  
A: Yes! Reuse the FileTools, DataManagers, and other utilities from the existing solution.

**Q: Will the old add-in still work?**  
A: Yes, both can coexist. Old add-in for legacy, new UI for modern workflow.

**Q: Do I need both systems?**  
A: No. Start with standalone app. Add add-in version later if needed.

**Q: Can the standalone app work offline?**  
A: Yes for configuration. Need SolidWorks for generation.

**Q: Which is easier to develop?**  
A: Standalone app is MUCH easier.

---

## ?? Recommendation: START HERE

```
1. Create standalone WPF app
2. Build one tab (Bundle)
3. Test Excel integration
4. Test SolidWorks connection
5. If it works ? expand to all 9 tabs
6. Later ? wrap as add-in if needed
```

**Estimated Time**:
- Standalone MVP: 1 week
- Full standalone: 3 weeks  
- Add-in wrapper: 1 week

**Total**: 4 weeks (matches your spec!)

---

*Guide Created: October 25, 2025*  
*Recommended: Standalone WPF App First*  
*Status: Ready to Start Building* ??

