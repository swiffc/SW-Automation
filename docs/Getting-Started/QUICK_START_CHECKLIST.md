# ? SolidWorks Automation - Quick Start Checklist

**Use this for rapid deployment**. See `LAUNCH_PLAN.md` for detailed instructions.

---

## ?? Today's Tasks (Get Running in 2-3 Hours)

### ? Task 1: Build the C# Add-In (30 min)
```powershell
# Open Visual Studio 2022 AS ADMINISTRATOR
# Open: macros\csharp\Solidworks-Automation\Solidworks Automation.sln
# Build ? Build Solution (Ctrl+Shift+B)
# Verify: 0 errors, 0 warnings
```
**Success**: Add-in appears in SolidWorks ? Tools ? Add-Ins

---

### ? Task 2: Run Setup Scripts (15 min)
```powershell
# Open PowerShell AS ADMINISTRATOR
cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation

.\SETUP_TEMPLATE_INTEGRATION.ps1
.\SETUP_HEADER_SECTION_TOOL.ps1

# Verify output folders created
ls output\
```
**Success**: Folders created, no errors

---

### ? Task 3: Test Add-In in SolidWorks (30 min)
```
1. Open SolidWorks
2. Go to: Tools ? Add-Ins
3. Check the box next to "Solidworks Automation"
4. Click OK
5. Verify add-in toolbar/menu appears
```
**Success**: Add-in loads, UI visible

---

### ? Task 4: Run First Automation (30 min)
```
In SolidWorks:
1. Use the add-in to run Bundle or Header automation
2. Select a template from templates/hudson_certified/
3. Generate output
4. Check output folder for generated files
```
**Success**: Files generated successfully

---

### ? Task 5: Test AI Assistant (15 min)
```powershell
# Ask for help with next steps
python utilities\python\ai_repo_assistant.py "What should I test next?"

# Get module-specific guidance
python utilities\python\ai_repo_assistant.py "How do I use the Header automation module?"
```
**Success**: AI responds with helpful guidance

---

## ?? This Week's Goals

### Week 1: Core Infrastructure
- [?] C# add-in built and registered
- [?] Templates accessible
- [?] Output folders created
- [?] All 7 automation modules tested:
  - [?] Bundle
  - [?] Header
  - [?] Hood
  - [?] Machinery Mount
  - [?] Plenum
  - [?] Structure
  - [?] Walkway

---

## ?? Quick Fixes

### Add-In Won't Load?
```powershell
# Re-register
cd "macros\csharp\Solidworks-Automation\bin\Release"
C:\Windows\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe /codebase "SolidworksAutomation.dll"
```

### Build Errors?
```powershell
# Restore NuGet packages
# In Visual Studio: Tools ? NuGet Package Manager ? Restore
```

### Excel Issues?
```powershell
# Validate file first
python utilities\python\ai_excel_validator.py "path\to\file.xlsx"
```

### Need Help?
```powershell
python utilities\python\ai_repo_assistant.py "I'm stuck on [describe issue]"
```

---

## ?? Quick Reference

**Documentation**:
- Full plan: `LAUNCH_PLAN.md`
- AI setup: `OPENAI_QUICKSTART.md`
- Agents guide: `AGENTS.md`

**AI Commands**:
```powershell
# Interactive help
python utilities\python\ai_repo_assistant.py

# Validate Excel
python utilities\python\ai_excel_validator.py "file.xlsx"

# Quick Excel preview
python utilities\python\excel_summary.py "file.xlsx"
```

---

## ? Done for the Day When...

- [?] Add-in builds successfully
- [?] Add-in loads in SolidWorks
- [?] At least 1 automation module tested
- [?] Output files generated
- [?] AI assistant working

**Next**: Move to Week 2 tasks in `LAUNCH_PLAN.md`

---

**Start here**: Task 1 ? Build the add-in ??
