# Installation Checklist

Use this checklist to ensure your SolidWorks automation environment is properly set up.

---

## ? 1. SolidWorks Configuration

### Install SolidWorks API SDK
- [ ] Download from SolidWorks Customer Portal
- [ ] Install API SDK package
- [ ] Verify API Help is accessible (Help ? API Help)

### Enable Macros
- [ ] Open SolidWorks
- [ ] Go to **Tools ? Options ? System Options**
- [ ] **General tab**: Check "Enable macros on startup"
- [ ] **Security tab**: Set macro security to "Medium"
- [ ] Enable Macro toolbar (**Tools ? Customize ? Commands**)

### Verify API is Working
- [ ] Create simple test macro in SolidWorks
- [ ] Record a basic operation
- [ ] Successfully run recorded macro

---

## ? 2. VBA Setup (Built-in)

### Access VBA Editor
- [ ] Can open VBA editor (**Tools ? Macro ? Edit**)
- [ ] VBA editor loads without errors

### Configure VBA Editor
- [ ] **Tools ? Options** configured:
  - [ ] Auto Syntax Check enabled
  - [ ] Require Variable Declaration enabled
  - [ ] Auto List Members enabled
- [ ] **Tools ? References** configured:
  - [ ] SolidWorks type library referenced
  - [ ] SolidWorks constants library referenced

### Test VBA
- [ ] Create new macro
- [ ] Successfully connect to `Application.SldWorks`
- [ ] Can access active document

---

## ? 3. Visual Studio Setup

### Install Visual Studio
- [ ] Download Visual Studio 2022 Community
- [ ] Install with these workloads:
  - [ ] .NET desktop development
  - [ ] Office/SharePoint development (optional)

### Install Components
- [ ] .NET Framework 4.8 SDK installed
- [ ] .NET Framework 4.8 targeting pack installed
- [ ] NuGet package manager installed

### Configure for SolidWorks
- [ ] Can create Class Library (.NET Framework) project
- [ ] Can install SolidWorks NuGet packages:
  - [ ] `SolidWorks.Interop.sldworks`
  - [ ] `SolidWorks.Interop.swconst`
  - [ ] `SolidWorks.Interop.swpublished`

### Test C# Connection
- [ ] Create simple console app
- [ ] Successfully connect using `Type.GetTypeFromProgID("SldWorks.Application")`
- [ ] Can get SolidWorks version number

---

## ? 4. VSCode Setup

### Install VSCode
- [ ] Download and install from https://code.visualstudio.com/
- [ ] VSCode opens successfully

### Install Extensions
- [ ] **VBA** extension installed (by local-smart)
- [ ] **C#** extension installed (by Microsoft)
- [ ] **Python** extension installed (by Microsoft)
- [ ] **Todo Tree** installed (optional)
- [ ] **GitLens** installed (optional)

### Configure VSCode
- [ ] `.vscode/settings.json` created
- [ ] `.vscode/extensions.json` created
- [ ] VBA files (*.swp) open with VBA syntax highlighting

---

## ? 5. Python Setup

### Install Python
- [ ] Python 3.11+ installed from python.org
- [ ] "Add Python to PATH" was checked during install
- [ ] Can run `python --version` in terminal

### Setup Virtual Environment
- [ ] Virtual environment created (`python -m venv venv`)
- [ ] Can activate venv (`.\venv\Scripts\Activate.ps1`)
- [ ] Activation policy allows script execution

### Install Packages
- [ ] pip upgraded (`python -m pip install --upgrade pip`)
- [ ] Requirements installed (`pip install -r requirements.txt`)
- [ ] Packages imported successfully:
  - [ ] `import win32com.client` works
  - [ ] `import pythoncom` works

### Test Python Connection
- [ ] Run `python test_connection.py`
- [ ] Successfully connects to SolidWorks
- [ ] Can get SolidWorks version
- [ ] Can check for active document

---

## ? 6. Workspace Organization

### Folder Structure
- [ ] `macros/vba/` directory exists
- [ ] `macros/csharp/` directory exists
- [ ] `macros/python/` directory exists
- [ ] `templates/` directory exists
- [ ] `output/` directory exists
- [ ] `docs/` directory exists

### Configuration Files
- [ ] `.gitignore` exists
- [ ] `requirements.txt` exists
- [ ] `README.md` exists
- [ ] `SETUP_GUIDE.md` exists

### Template Files
- [ ] VBA template macro exists
- [ ] Python helper module exists
- [ ] Documentation files exist

---

## ? 7. Git Setup (Optional)

### Install Git
- [ ] Git installed from git-scm.com
- [ ] Can run `git --version`

### Initialize Repository
- [ ] Repository initialized (`git init`)
- [ ] Remote repository connected (if applicable)
- [ ] `.gitignore` properly configured

### First Commit
- [ ] Files staged (`git add .`)
- [ ] Initial commit made
- [ ] Changes pushed to remote (if applicable)

---

## ? 8. Testing & Validation

### VBA Test
- [ ] Open SolidWorks
- [ ] Open a simple part file
- [ ] Run VBA template macro successfully
- [ ] No errors in macro execution

### Python Test
- [ ] SolidWorks is running
- [ ] Activate Python venv
- [ ] Run `python test_connection.py`
- [ ] All 4 tests pass
- [ ] Can access active document

### C# Test
- [ ] Create simple test project in Visual Studio
- [ ] Add SolidWorks references
- [ ] Build project successfully (no errors)
- [ ] Can connect to SolidWorks instance

---

## ? 9. Documentation Review

### Read Core Docs
- [ ] Read `SETUP_GUIDE.md` completely
- [ ] Read `README.md`
- [ ] Skimmed `VBA_QUICK_REFERENCE.md`
- [ ] Skimmed `CSHARP_ADDIN_GUIDE.md` (if using C#)

### Bookmark Resources
- [ ] SolidWorks API Help bookmarked
- [ ] SolidWorks API forum bookmarked
- [ ] CodeStack website bookmarked

---

## ? 10. First Project

### Plan First Automation
- [ ] Identified a task to automate
- [ ] Determined best tool (VBA/Python/C#)
- [ ] Created project folder
- [ ] Started coding!

---

## Common Issues Solved

### Execution Policy Error (PowerShell)
**Error**: "cannot be loaded because running scripts is disabled"

**Solution**:
```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

### COM Connection Fails
**Error**: Cannot connect to SolidWorks

**Solutions**:
- [ ] SolidWorks is running
- [ ] Run IDE/script as Administrator
- [ ] Re-register SolidWorks: `SLDWORKS.exe /regserver` (as admin)

### Type Library Not Found (VBA)
**Error**: "User-defined type not defined"

**Solution**:
- [ ] Open VBA Editor
- [ ] Tools ? References
- [ ] Check SolidWorks type libraries
- [ ] If missing, browse to SolidWorks installation folder

### Python Package Install Fails
**Error**: pip install fails

**Solutions**:
- [ ] Virtual environment is activated
- [ ] pip is upgraded: `python -m pip install --upgrade pip`
- [ ] Run terminal as Administrator
- [ ] Check internet connection

---

## Sign Off

When all items are checked, your environment is ready! ??

**Date Completed**: _______________

**Notes**:
```
[Add any specific notes about your setup here]
```

---

**Need help?** See `SETUP_GUIDE.md` for detailed instructions on each step.

