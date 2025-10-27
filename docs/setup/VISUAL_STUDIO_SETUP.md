# Visual Studio Setup for SolidWorks Automation

Quick guide to get the cloned GitHub repository working in Visual Studio.

---

## ? What's Already Done

- ? Visual Studio 2022 installed
- ? GitHub repository cloned to: `macros/csharp/Solidworks-Automation/`
- ? Solution file ready: `Solidworks Automation.sln`

---

## ?? Next Steps (Do These Now)

### Step 1: Open the Solution

1. **Launch Visual Studio 2022**
2. Click **"Open a project or solution"**
3. Navigate to:
   ```
   C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation\
   ```
4. Open **`Solidworks Automation.sln`**
5. Visual Studio will load all 20 projects

### Step 2: Restore NuGet Packages

When the solution opens, you should see a banner about restoring packages:
1. Click **"Restore"** in the banner
2. Or: Right-click **Solution** ? **"Restore NuGet Packages"**
3. Wait for completion (may take 1-2 minutes)

### Step 3: Try Building Core Libraries

Let's start by building the utility libraries (they don't need SolidWorks running):

1. **In Solution Explorer**, right-click **"ModelTools"** project
2. Select **"Build"**
3. Check **Output** window (View ? Output)
4. Look for "Build succeeded" or error messages

**Then try:**
- Right-click **"FileTools"** ? Build
- Right-click **"UserInterface"** ? Build

### Step 4: Review Build Results

**If builds succeed:** ? Great! You're ready to explore the code.

**If you see errors:** Common issues and fixes below.

---

## ?? Common Build Issues & Fixes

### Issue 1: "Could not find SolidWorks Interop assemblies"

**Fix Option A - Use Included DLLs:**
1. The repo includes DLLs in `Libraries/Solidworks Interop/`
2. Each project should reference these
3. If missing, manually add references:
   - Right-click project ? Add ? Reference
   - Browse to `Libraries/Solidworks Interop/`
   - Add the DLL files

**Fix Option B - Use NuGet (Recommended):**
1. Right-click project ? **Manage NuGet Packages**
2. Click **Browse** tab
3. Search and install:
   - `SolidWorks.Interop.sldworks`
   - `SolidWorks.Interop.swconst`
   - `SolidWorks.Interop.swpublished`
4. Match the version to your SolidWorks (e.g., 30.5.0 for SW 2023)

### Issue 2: "The type or namespace name 'EdmLib' could not be found"

This affects vault-related projects (AXC_Vault, Vault).

**Fix:**
- If you don't use PDM Vault: Right-click these projects ? **Unload Project**
- If you do use PDM: Install EPDM API from SolidWorks installation media

### Issue 3: Platform target mismatch

**Fix:**
1. **Build ? Configuration Manager**
2. Set Platform to **x64** (if you have 64-bit SolidWorks)
3. Or set to **Any CPU** and ensure "Prefer 32-bit" is unchecked

### Issue 4: COM registration fails during post-build

**Fix:**
1. Close Visual Studio
2. Right-click **Visual Studio** ? **Run as Administrator**
3. Re-open the solution and build again

---

## ?? Recommended Build Order

Try building in this order to resolve dependencies:

1. **ModelTools** (no dependencies)
2. **FileTools** (may depend on ModelTools)
3. **UserInterface** (UI components)
4. **Structure** (uses ModelTools)
5. **Hood** (uses ModelTools)
6. **Plenum** (uses ModelTools + FileTools)
7. **Bundle** (uses multiple libraries)
8. **SolidWorks Add-In** (main add-in, depends on most others)

---

## ?? Configure for Debugging

### For "SolidWorks Add-In" Project:

1. In Solution Explorer, right-click **"SolidWorks Add-In"**
2. Select **Properties**
3. Go to **Debug** tab
4. Set:
   - **Start Action**: "Start external program"
   - **Program**: `C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe`
   - (Adjust path if SolidWorks is installed elsewhere)

5. Now you can press **F5** to debug:
   - Visual Studio will launch SolidWorks
   - Your add-in will load automatically
   - You can set breakpoints and debug!

---

## ?? Understanding the Solution

### Project Count: 20

**Core Add-in:**
- SolidWorks Add-In (main entry point)

**Design Automation Tools (8):**
- Bundle, Header, Hood, MachineryMount, Plenum, Structure, Walkway, Bounty

**Utilities (3):**
- ModelTools, FileTools, UserInterface

**Support (5):**
- AddinInstaller, AddInUpdater, AddInDllVersionControl, SplashScreen, Testing

**Integration (4):**
- Excel, Vault, Fork, Universal Drawing Tool

---

## ?? Quick Test

### Build a Simple Project:

1. Right-click **"ModelTools"**
2. Click **"Build"**
3. Check Output window

**Success looks like:**
```
Build started...
1>------ Build started: Project: ModelTools, Configuration: Debug Any CPU ------
1>ModelTools -> C:\...\bin\Debug\ModelTools.dll
========== Build: 1 succeeded, 0 failed, 0 up-to-date, 0 skipped ==========
```

**Error looks like:**
```
Error CS0246: The type or namespace name 'SldWorks' could not be found
```
? Fix: Add SolidWorks Interop references (see Issue 1 above)

---

## ?? Learning Path

### Week 1: Explore
1. ? Get solution to build
2. ?? Read code in **ModelTools** - see how they interact with SolidWorks API
3. ?? Read code in **FileTools** - see file operations
4. ?? Study one design tool (start with **Hood** or **Structure**)

### Week 2: Modify
1. Make small changes to UI text
2. Add debug messages
3. Understand event flow

### Week 3: Extend
1. Add new feature to existing tool
2. Create your own variation
3. Build and test in SolidWorks

---

## ?? Pro Tips

1. **Build Incrementally**: Don't try to build all 20 projects at once. Build one at a time.

2. **Start with Libraries**: Build ModelTools and FileTools first. Other projects depend on them.

3. **Exclude Problem Projects**: If a project won't build, right-click ? Unload Project. Come back to it later.

4. **Use Output Window**: View ? Output shows detailed build information and errors.

5. **Check Error List**: View ? Error List shows all compilation errors at once.

6. **Run as Admin**: COM registration requires admin rights. Run VS as Administrator from the start.

---

## ?? File Locations

- **Solution**: `macros/csharp/Solidworks-Automation/Solidworks Automation.sln`
- **Detailed Guide**: `macros/csharp/Solidworks-Automation/GETTING_STARTED.md`
- **Standards**: `macros/csharp/Solidworks-Automation/Standards/`
- **Libraries**: `macros/csharp/Solidworks-Automation/Libraries/`

---

## ? Success Checklist

- [ ] Visual Studio 2022 is open
- [ ] Solution `Solidworks Automation.sln` is loaded
- [ ] NuGet packages are restored
- [ ] At least one project builds successfully (try ModelTools first)
- [ ] SolidWorks installation path is configured for debugging
- [ ] Visual Studio is running as Administrator (if needed for COM registration)

---

## ?? Ready to Start!

Once you can build **ModelTools** successfully, you're ready to explore this professional SolidWorks automation suite!

**Current Status:**
- ? Repository cloned
- ? Visual Studio installed
- ? Next: Open solution and restore packages

**Open the solution now and let's see what happens!** ??

---

Need help? Check `macros/csharp/Solidworks-Automation/GETTING_STARTED.md` for more detailed information.

