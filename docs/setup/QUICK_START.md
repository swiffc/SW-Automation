# Quick Start Guide

Get up and running with SolidWorks automation in 5 minutes!

---

## ?? Super Quick Start

### For Python Users (Easiest)

1. **Run the setup script** (in PowerShell):
```powershell
.\setup_python.ps1
```

2. **Test your connection**:
```powershell
python test_connection.py
```

That's it! You're ready to automate with Python.

---

### For VBA Users (Simplest)

1. **Open SolidWorks**
2. **Tools ? Macro ? Edit**
3. **Open**: `macros/vba/Template_Macro.swp`
4. **Press F5** to run

Done! Start modifying the template for your needs.

---

### For C# Developers (Most Powerful)

1. **Open Visual Studio 2022**
2. **File ? New ? Project ? Class Library (.NET Framework)**
3. **Install NuGet packages**:
   - `SolidWorks.Interop.sldworks`
   - `SolidWorks.Interop.swconst`
4. **Follow**: `docs/CSHARP_ADDIN_GUIDE.md`

---

## ?? What Got Set Up

### Project Structure
```
Solidworks_Automation/
??? ?? .vscode/                    VSCode settings
??? ?? macros/
?   ??? ?? vba/                    VBA macros (.swp files)
?   ??? ?? csharp/                 C# projects
?   ??? ?? python/                 Python scripts
??? ?? templates/                  SolidWorks templates
??? ?? output/                     Generated files
??? ?? docs/                       Documentation
?   ??? ?? VBA_QUICK_REFERENCE.md
?   ??? ?? CSHARP_ADDIN_GUIDE.md
?   ??? ?? INSTALLATION_CHECKLIST.md
??? ?? SETUP_GUIDE.md             Complete setup instructions
??? ?? README.md                   Project overview
??? ?? QUICK_START.md             This file
??? ?? test_connection.py          Test script
??? ?? requirements.txt            Python packages
??? ?? setup_python.ps1            Python setup script
??? ?? .gitignore                  Git ignore rules
```

---

## ? First Steps

### Step 1: Choose Your Tool

| Tool | Best For | Difficulty | Setup Time |
|------|----------|------------|------------|
| **VBA** | Simple macros, quick tasks | Easy | 0 min (built-in) |
| **Python** | Automation, batch processing | Medium | 5 min |
| **C#** | Add-ins, custom UI, complex apps | Advanced | 30 min |

### Step 2: Complete Setup

Pick one based on your chosen tool:

#### Option A: Python
```powershell
# Run this in PowerShell
.\setup_python.ps1

# Then test
python test_connection.py
```

#### Option B: VBA
1. Open SolidWorks
2. Tools ? Options ? System Options ? General
3. Check "Enable macros on startup"
4. Tools ? Macro ? Edit ? Open template

#### Option C: C#
1. Install Visual Studio 2022
2. Read `docs/CSHARP_ADDIN_GUIDE.md`
3. Follow step-by-step instructions

### Step 3: Run Your First Test

#### Python Test
```powershell
# Make sure SolidWorks is running
python test_connection.py
```

Expected output:
```
====================================================
SolidWorks API Connection Test
====================================================

[1/4] Testing module imports...
    ? win32com imported successfully

[2/4] Connecting to SolidWorks...
    ? Connected to SolidWorks

[3/4] Getting SolidWorks version...
    ? SolidWorks Version: 30.5.0.0...

[4/4] Checking for active document...
    ? Active document: Part1 (Part)

====================================================
? All tests passed! SolidWorks API is ready to use.
====================================================
```

#### VBA Test
```vba
' In VBA Editor, press F5 to run
Sub main()
    Dim swApp As SldWorks.SldWorks
    Set swApp = Application.SldWorks
    MsgBox "Connected to SolidWorks " & swApp.RevisionNumber()
End Sub
```

#### C# Test
```csharp
// Create console app and run
Type swType = Type.GetTypeFromProgID("SldWorks.Application");
SldWorks swApp = (SldWorks)Activator.CreateInstance(swType);
Console.WriteLine($"Connected to SolidWorks {swApp.RevisionNumber()}");
```

---

## ?? Learning Path

### Week 1: Basics
1. ? Complete setup and testing
2. ?? Read `SETUP_GUIDE.md`
3. ?? Explore `VBA_QUICK_REFERENCE.md`
4. ?? Modify template macro for simple task

### Week 2: First Project
1. ?? Identify automation need
2. ?? Plan approach
3. ?? Write code
4. ?? Test thoroughly
5. ?? Save and document

### Week 3: Advanced Topics
1. ?? Study SolidWorks API Help
2. ?? Create custom UI (if using C#)
3. ?? Add event handlers
4. ?? Integrate with external systems

---

## ?? Help & Troubleshooting

### Problem: Script execution disabled (PowerShell)
```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

### Problem: Cannot connect to SolidWorks
**Solutions**:
- Make sure SolidWorks is running
- Run script/IDE as Administrator
- Re-register: `SLDWORKS.exe /regserver` (as admin)

### Problem: Python packages won't install
```powershell
# Activate venv first
.\venv\Scripts\Activate.ps1

# Upgrade pip
python -m pip install --upgrade pip

# Try again
pip install -r requirements.txt
```

### Problem: VBA type not defined
- Open VBA Editor
- Tools ? References
- Check "SolidWorks [version] type library"
- Check "SolidWorks constant type library"

### Problem: Visual Studio can't find SolidWorks
1. Install NuGet packages:
   - `SolidWorks.Interop.sldworks`
   - `SolidWorks.Interop.swconst`
2. Or manually add references from:
   - `C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\api\redist`

---

## ?? Documentation Index

| Document | Purpose |
|----------|---------|
| **QUICK_START.md** (this file) | Get started in 5 minutes |
| **README.md** | Project overview and basics |
| **SETUP_GUIDE.md** | Complete installation guide |
| **VBA_QUICK_REFERENCE.md** | VBA code snippets and examples |
| **CSHARP_ADDIN_GUIDE.md** | C# add-in development guide |
| **INSTALLATION_CHECKLIST.md** | Verify your setup is complete |

---

## ?? Next Steps

1. **Complete your setup** using the appropriate method above
2. **Run the test** to verify everything works
3. **Check the checklist**: `docs/INSTALLATION_CHECKLIST.md`
4. **Start coding**! Begin with the templates provided

---

## ?? Pro Tips

1. **Always save before running macros** - Test on simple parts first
2. **Use version control** - Git is your friend
3. **Read the API docs** - Help ? API Help in SolidWorks
4. **Start simple** - Master basics before complex automation
5. **Ask for help** - SolidWorks API forum is active and helpful

---

## ?? Quick Links

- **SolidWorks API Forum**: https://forum.solidworks.com/community/api
- **CodeStack Examples**: https://www.codestack.net/solidworks-api/
- **API Help**: Help ? API Help (in SolidWorks)
- **Python win32com**: https://pypi.org/project/pywin32/

---

**Ready to automate? Let's go! ??**

Questions? Check `SETUP_GUIDE.md` for detailed help.

