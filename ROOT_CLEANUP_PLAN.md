# Root Folder Cleanup Plan

**Current Issue**: Files scattered in `Solidworks_Automation/` root  
**Goal**: Organize into proper structure before GitHub commit

---

## ?? Current Root Files (Need Organization)

### Python Scripts (Analysis/Utilities)
```
analyze_excel_structure.py
capture_everything.py
excel_cell_inspector.py
find_file_references_in_excel.py
quick_excel_check.py
read_excel_template.py
scan_all_excel_files.py
scan_all_excel_sheets.py
test_connection.py
```
**Where they should go**: `utilities/python/` or `scripts/python/`

---

### PowerShell Scripts (Setup/Copy)
```
COPY_CAD_FILES.ps1
COPY_HUDSON_TEMPLATES.ps1
COPY_Z_STRUCTURE_TOOL.ps1
SETUP_HEADER_SECTION_TOOL.ps1
SETUP_TEMPLATE_INTEGRATION.ps1
setup_python.ps1
```
**Where they should go**: `scripts/setup/` or `scripts/utilities/`

---

### Excel Analysis Results (Data Files)
```
CELL_MAPPING_FINDINGS.json
CELL_MAPPING_REPORT.md
complete_excel_analysis.json
COMPLETE_EXCEL_CAPTURE.json
COMPLETE_EXCEL_REPORT.md
COMPLETE_EXCEL_SUMMARY.md
excel_scan_results.txt
```
**Where they should go**: 
- Move to `analysis/excel/` or `data/excel-analysis/`
- OR delete if no longer needed (they're analysis artifacts)

---

### Documentation Files (Markdown)
```
BUNDLE_DUAL_APPR... (likely BUNDLE_DUAL_APPROACH_ANALYSIS.md)
PROJECT_SCAN_REPO... (likely PROJECT_SCAN_REPORT.md - duplicate?)
```
**Where they should go**: 
- BUNDLE_DUAL_APPROACH_ANALYSIS.md ? `macros/csharp/Solidworks-Automation/docs/Architecture/`
- Check if duplicates exist in proper location

---

### Configuration Files
```
config.json
requirements.txt (Python dependencies)
```
**Where they should go**: 
- config.json ? Keep in root OR move to `config/`
- requirements.txt ? Keep in root (standard for Python projects)

---

### Data Files
```
S25140-Prego1.xlsm (Excel workbook - 1.4 MB)
```
**Where it should go**: 
- `examples/` or `test-data/` if it's sample data
- OR delete if it's not needed for the project

---

## ? Files That Are Correctly Located

```
.gitignore ?
README.md ?
macros/ ?
output/ ?
solidworks-api/ ?
templates/ ?
docs/ ? (if it exists)
```

---

## ?? Recommended Structure

```
Solidworks_Automation/
??? README.md                          ? Keep here
??? .gitignore                         ? Keep here
??? requirements.txt                   ? Keep here (Python deps)
??? config.json                        ? Keep here OR move to config/
?
??? macros/                            ? Already good
?   ??? csharp/
?   ?   ??? Solidworks-Automation/     ? Main C# project
?   ??? python/                        ? Move Python scripts here
?   ??? vba/
?
??? templates/                         ? Already good
?   ??? header_section_tool/
?   ??? hudson_certified/
?   ??? xch_structure_tool/
?   ??? z_structure_tool/
?
??? scripts/                           ? Create this
?   ??? setup/                         ? Move SETUP_*.ps1 here
?   ?   ??? SETUP_HEADER_SECTION_TOOL.ps1
?   ?   ??? SETUP_TEMPLATE_INTEGRATION.ps1
?   ?   ??? setup_python.ps1
?   ?
?   ??? utilities/                     ? Move COPY_*.ps1 here
?       ??? COPY_CAD_FILES.ps1
?       ??? COPY_HUDSON_TEMPLATES.ps1
?       ??? COPY_Z_STRUCTURE_TOOL.ps1
?
??? utilities/                         ? Create this
?   ??? python/                        ? Move Python scripts here
?       ??? analyze_excel_structure.py
?       ??? capture_everything.py
?       ??? excel_cell_inspector.py
?       ??? find_file_references_in_excel.py
?       ??? quick_excel_check.py
?       ??? read_excel_template.py
?       ??? scan_all_excel_files.py
?       ??? scan_all_excel_sheets.py
?       ??? test_connection.py
?
??? analysis/                          ? Create this (OR DELETE FILES)
?   ??? excel/                         ? Excel analysis artifacts
?       ??? CELL_MAPPING_FINDINGS.json
?       ??? CELL_MAPPING_REPORT.md
?       ??? complete_excel_analysis.json
?       ??? COMPLETE_EXCEL_CAPTURE.json
?       ??? COMPLETE_EXCEL_REPORT.md
?       ??? COMPLETE_EXCEL_SUMMARY.md
?       ??? excel_scan_results.txt
?
??? examples/                          ? Create this (OR DELETE)
?   ??? S25140-Prego1.xlsm            ? Sample Excel file
?
??? docs/                              ? Exists (in macros/csharp/...)
??? output/                            ? Already good
```

---

## ?? Action Plan

### Option 1: Comprehensive Cleanup (Recommended)

1. **Create folder structure**:
   ```powershell
   mkdir scripts\setup
   mkdir scripts\utilities  
   mkdir utilities\python
   mkdir analysis\excel
   mkdir examples
   ```

2. **Move Python scripts**:
   ```powershell
   Move-Item *.py utilities\python\
   ```

3. **Move PowerShell setup scripts**:
   ```powershell
   Move-Item SETUP_*.ps1 scripts\setup\
   Move-Item setup_python.ps1 scripts\setup\
   ```

4. **Move PowerShell utility scripts**:
   ```powershell
   Move-Item COPY_*.ps1 scripts\utilities\
   ```

5. **Move analysis artifacts**:
   ```powershell
   Move-Item *CELL_MAPPING* analysis\excel\
   Move-Item *COMPLETE_EXCEL* analysis\excel\
   Move-Item excel_scan_results.txt analysis\excel\
   ```

6. **Move sample data**:
   ```powershell
   Move-Item S25140-Prego1.xlsm examples\
   ```

7. **Move documentation**:
   ```powershell
   Move-Item BUNDLE_DUAL_*.md macros\csharp\Solidworks-Automation\docs\Architecture\
   ```

---

### Option 2: Minimal Cleanup (Faster)

Just move the most important files and delete the rest:

1. **Move scripts to one folder**:
   ```powershell
   mkdir scripts
   Move-Item *.ps1 scripts\
   Move-Item *.py scripts\
   ```

2. **Delete analysis artifacts** (if not needed):
   ```powershell
   Remove-Item *CELL_MAPPING*, *COMPLETE_EXCEL*, excel_scan_results.txt
   ```

3. **Move or delete sample data**:
   ```powershell
   # Either:
   Move-Item S25140-Prego1.xlsm examples\
   # Or:
   Remove-Item S25140-Prego1.xlsm
   ```

---

## ?? Recommendation

**I recommend Option 1** for a professional GitHub repository:

1. Properly organized structure
2. Scripts logically grouped
3. Analysis artifacts preserved (but organized)
4. Easy to navigate
5. Professional appearance

**Time Required**: 10-15 minutes

---

## ?? Before You Start

### Decision Points:

1. **Excel Analysis Files** - Do you need these?
   - If NO ? Delete them (they're artifacts from analysis)
   - If YES ? Keep in `analysis/excel/`

2. **S25140-Prego1.xlsm** - Is this needed?
   - If it's a test file ? Move to `examples/`
   - If it's not needed ? Delete it

3. **Python Scripts** - Are these utilities still used?
   - If YES ? Organize in `utilities/python/`
   - If NO ? Consider deleting

4. **COPY_*.ps1 Scripts** - Are these for setup/deployment?
   - If YES ? Keep in `scripts/utilities/`
   - If NO ? Delete

---

## ?? What to Keep in Root

**Only these files should be in the root**:
```
? README.md
? .gitignore
? requirements.txt (Python dependencies)
? config.json (if used globally)
? LICENSE (to be added)
? CONTRIBUTING.md (to be added)
```

**Everything else should be in organized folders.**

---

## ?? Quick Commands

### Preview what will move:
```powershell
# See Python files
Get-ChildItem *.py

# See PowerShell files
Get-ChildItem *.ps1

# See JSON/Excel files
Get-ChildItem *.json, *.xlsm, *.txt
```

### Execute cleanup:
```powershell
# Run the organization script I created
cd macros\csharp\Solidworks-Automation
.\scripts\Organize-ForGitHub.ps1

# OR manually organize the root:
cd ..\..\..
# Then follow Option 1 or Option 2 above
```

---

## ? After Cleanup

Your root should look like:
```
Solidworks_Automation/
??? README.md
??? .gitignore
??? requirements.txt
??? config.json
??? macros/
??? templates/
??? scripts/
??? utilities/
??? analysis/
??? examples/
??? docs/
??? output/
```

**Clean, organized, professional!** ?

---

**Next Step**: Review this plan and decide Option 1 or Option 2, then execute!

