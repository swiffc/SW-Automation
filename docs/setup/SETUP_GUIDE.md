# SolidWorks Automation Setup Guide

Complete guide for setting up your PC for SolidWorks automation using VBA, Visual Studio, and VSCode.

---

## 1. SolidWorks Setup

### Enable SolidWorks API and Macros
1. Open **SolidWorks**
2. Go to **Tools → Options → System Options → General**
   - Check **Enable macros on startup**
3. Go to **Tools → Options → System Options → Security**
   - Set macro security to **Medium** (allows you to enable/disable macros)
4. Go to **Tools → Customize → Commands**
   - Enable the **Macro** toolbar for easy access

### SolidWorks API Help
- Access from: **SolidWorks → Help → API Help**
- Or download from: SolidWorks Customer Portal → Downloads → API SDK

---

## 2. VBA Setup (Built into SolidWorks)

### Access VBA Editor
- **Tools → Macro → New** (creates new macro)
- **Tools → Macro → Edit** (edits existing macro)
- Or press **Alt + F11** (if hotkey enabled)

### VBA Editor Configuration
1. In VBA Editor, go to **Tools → Options**:
   - **Editor Tab**:
     - ☑ Auto Syntax Check
     - ☑ Require Variable Declaration
     - ☑ Auto List Members
     - ☑ Auto Quick Info
   - **Editor Format Tab**: Choose comfortable colors/fonts
   
2. Set up **References** (Tools → References):
   - ☑ SolidWorks 2.x type library (your version)
   - ☑ SolidWorks constant type library
   - ☑ Microsoft Scripting Runtime (for file operations)
   - ☑ Microsoft Visual Basic for Applications Extensibility

### VBA Project Structure
```
MyMacro.swp
├── Module1 (General utilities)
├── Module2 (Specific functions)
└── ThisApplication
```

---

## 3. Visual Studio Setup (For VB.NET/C# Add-ins)

### Download & Install
1. **Download Visual Studio 2022 Community** (free):
   - https://visualstudio.microsoft.com/downloads/
   
2. **Required Workloads** during installation:
   - ☑ **.NET desktop development**
   - ☑ **Office/SharePoint development** (optional, for advanced features)

3. **Individual Components** to add:
   - ☑ .NET Framework 4.8 SDK
   - ☑ .NET Framework 4.8 targeting pack
   - ☑ NuGet package manager

### Configure Visual Studio for SolidWorks

#### Install SolidWorks.Interop Assemblies
After installing Visual Studio:

**Option A: NuGet Package (Recommended)**
1. Create new project: **File → New → Project → Class Library (.NET Framework)**
2. Right-click project → **Manage NuGet Packages**
3. Search and install:
   - `SolidWorks.Interop.sldworks`
   - `SolidWorks.Interop.swconst`
   - `SolidWorks.Interop.swpublished`

**Option B: Reference DLLs Directly**
1. Right-click **References** in Solution Explorer
2. Click **Add Reference → Browse**
3. Navigate to SolidWorks installation folder (usually):
   ```
   C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\api\redist
   ```
4. Add these DLLs:
   - `SolidWorks.Interop.sldworks.dll`
   - `SolidWorks.Interop.swconst.dll`
   - `SolidWorks.Interop.swpublished.dll`
   - `SolidWorks.Interop.swcommands.dll`

#### Project Configuration
1. Right-click project → **Properties**:
   - **Target Framework**: .NET Framework 4.8
   - **Platform Target**: x64 (if using 64-bit SolidWorks)
   - **Build** → Uncheck "Prefer 32-bit"

2. For Add-ins, set:
   - **Debug** → **Start external program**: 
     ```
     C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe
     ```

---

## 4. VSCode Setup (For Macro Editing & Scripting)

### Download & Install
1. **Download VSCode**: https://code.visualstudio.com/
2. Install for Windows

### Essential Extensions

#### For VBA Development:
```
Name: VBA
Publisher: local-smart
ID: local-smart.vba
```

#### For General Development:
1. **C#** (by Microsoft) - If writing C# add-ins
   - ID: `ms-dotnettools.csharp`

2. **Python** (by Microsoft) - If using Python for automation
   - ID: `ms-python.python`

3. **SolidWorks Macros Syntax** (if available)
   - Search marketplace for "SolidWorks"

4. **Prettier - Code formatter**
   - ID: `esbenp.prettier-vscode`

5. **Todo Tree** - Track TODO comments
   - ID: `Gruntfuggly.todo-tree`

6. **GitLens** - Git integration
   - ID: `eamodio.gitlens`

### VSCode Configuration for SolidWorks

Create `.vscode/settings.json` in your workspace:

```json
{
  "files.associations": {
    "*.swp": "vba",
    "*.swb": "vba"
  },
  "editor.tabSize": 4,
  "editor.insertSpaces": true,
  "editor.formatOnSave": false,
  "editor.minimap.enabled": true,
  "files.encoding": "windows1252",
  "files.eol": "\r\n"
}
```

Create `.vscode/extensions.json`:

```json
{
  "recommendations": [
    "local-smart.vba",
    "ms-dotnettools.csharp",
    "ms-python.python"
  ]
}
```

---

## 5. Python Setup (Optional - For API Automation)

### Install Python
1. Download Python 3.11+: https://www.python.org/downloads/
2. During installation:
   - ☑ **Add Python to PATH**
   - ☑ Install for all users

### Install Required Packages
Open PowerShell in your workspace:

```powershell
# Create virtual environment
python -m venv venv

# Activate virtual environment
.\venv\Scripts\Activate.ps1

# Install packages
pip install pywin32          # For COM automation
pip install comtypes         # Alternative COM library
pip install pythonnet        # .NET interop
```

### Python Script for SolidWorks
Example connection test:

```python
import win32com.client

# Connect to SolidWorks
sw = win32com.client.Dispatch("SldWorks.Application")
print(f"Connected to SolidWorks {sw.RevisionNumber()}")

# Get active document
model = sw.ActiveDoc
if model:
    print(f"Active document: {model.GetTitle()}")
```

---

## 6. Workspace Organization

### Recommended Folder Structure
```
Solidworks_Automation/
├── macros/
│   ├── vba/                    # VBA macros (.swp, .swb)
│   ├── csharp/                 # C# projects
│   └── python/                 # Python scripts
├── templates/                  # SolidWorks templates
├── output/                     # Generated files
├── docs/                       # Documentation
├── .vscode/                    # VSCode settings
└── README.md
```

---

## 7. Quick Start Testing

### Test 1: VBA Macro
1. Open SolidWorks
2. **Tools → Macro → New**
3. Paste this code:

```vba
Option Explicit

Sub main()
    Dim swApp As SldWorks.SldWorks
    Dim swModel As SldWorks.ModelDoc2
    
    Set swApp = Application.SldWorks
    Set swModel = swApp.ActiveDoc
    
    If swModel Is Nothing Then
        MsgBox "Please open a document first!", vbExclamation
        Exit Sub
    End If
    
    MsgBox "Connected! Document: " & swModel.GetTitle(), vbInformation
End Sub
```

4. Click **Run** (F5)

### Test 2: Visual Studio C# Add-in
Create new Class Library project and add this code:

```csharp
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swconst;
using System;
using System.Runtime.InteropServices;

namespace SolidWorksTest
{
    public class Program
    {
        static void Main(string[] args)
        {
            // Connect to SolidWorks
            Type swType = Type.GetTypeFromProgID("SldWorks.Application");
            SldWorks swApp = (SldWorks)Activator.CreateInstance(swType);
            swApp.Visible = true;
            
            Console.WriteLine($"Connected to SolidWorks {swApp.RevisionNumber()}");
            Console.ReadLine();
        }
    }
}
```

### Test 3: Python Script
Save as `test_connection.py`:

```python
import win32com.client
import pythoncom

try:
    sw = win32com.client.Dispatch("SldWorks.Application")
    sw.Visible = True
    print(f"✓ Connected to SolidWorks {sw.RevisionNumber()}")
    
    # Get active document
    model = sw.ActiveDoc
    if model:
        print(f"✓ Active document: {model.GetTitle()}")
    else:
        print("⚠ No active document")
        
except Exception as e:
    print(f"✗ Error: {e}")
```

Run: `python test_connection.py`

---

## 8. Useful Resources

### Documentation
- **SolidWorks API Help**: Help → API Help (within SolidWorks)
- **API SDK**: https://www.solidworks.com/support/downloads
- **Developer Forums**: https://forum.solidworks.com/community/api

### Learning Resources
- **SolidWorks API Tutorial**: Built into API Help
- **GitHub Examples**: Search "SolidWorks API examples"
- **CAD Python**: https://www.codestack.net/solidworks-api/

### Debugging Tools
- **SolidWorks Macro Feature Debugger**: Tools → Macro → Debug
- **Visual Studio Debugger**: Attach to SLDWORKS.exe process
- **Python**: Use `print()` statements or `pdb` debugger

---

## 9. Common Issues & Solutions

### Issue: "Cannot connect to SolidWorks"
**Solution**:
- Ensure SolidWorks is running
- Run IDE as Administrator
- Check Windows Firewall settings

### Issue: "Type library not found"
**Solution**:
- Re-register SolidWorks: Run as Admin
  ```cmd
  "C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe" /regserver
  ```

### Issue: "Access denied" in Python
**Solution**:
- Run script as Administrator
- Use early binding with `EnsureDispatch`:
  ```python
  sw = win32com.client.gencache.EnsureDispatch("SldWorks.Application")
  ```

### Issue: VBA "User-defined type not defined"
**Solution**:
- Check **Tools → References** in VBA Editor
- Ensure SolidWorks type libraries are checked

---

## 10. Next Steps

1. ✓ Install all required software
2. ✓ Configure development environments
3. ✓ Run test scripts to verify connections
4. ✓ Organize your workspace folders
5. ✓ Start with simple automation tasks
6. ✓ Explore SolidWorks API documentation

---

## Quick Reference Commands

### PowerShell (in VSCode terminal)
```powershell
# Run Python script
python macros/python/script.py

# Activate Python venv
.\venv\Scripts\Activate.ps1

# Build C# project
dotnet build macros/csharp/MyProject.csproj
```

### SolidWorks Macro Shortcuts
- **Alt + F11**: Open VBA Editor (if enabled)
- **F5**: Run current macro
- **F8**: Step through macro (debug)
- **Ctrl + Break**: Stop running macro

---

**Good luck with your SolidWorks automation journey!** 🚀

