# ?? **CLEANUP SCRIPT USAGE GUIDE**

## ?? **Quick Start**

### **Option 1: Full Cleanup (Recommended)**
```powershell
# Navigate to solution root
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation"

# Run with backup (SAFE)
.\scripts\Cleanup-Project.ps1 -Backup
```

### **Option 2: Preview Changes (No Risk)**
```powershell
# See what would happen without making changes
.\scripts\Cleanup-Project.ps1 -WhatIf
```

### **Option 3: Documentation Only (Quick Win)**
```powershell
# Only organize documentation (15 min)
.\scripts\Cleanup-Project.ps1 -Phase Documentation
```

---

## ?? **What Each Phase Does**

### **Phase: All (Default)**
- ? Organizes all documentation
- ? Removes duplicate files
- ? Creates navigation files
- ? Updates .gitignore
- ?? **Time:** ~2-3 minutes

### **Phase: Documentation**
- ? Creates `/docs` folder structure
- ? Moves all .md files to appropriate folders
- ? Moves test scripts to `/scripts`
- ?? **Time:** ~30 seconds

### **Phase: Duplicates**
- ? Removes Drawing.cs (duplicate)
- ? Removes temp files
- ?? **Time:** ~5 seconds

### **Phase: Navigation**
- ? Creates README.md at root
- ? Creates docs/README.md
- ?? **Time:** ~5 seconds

---

## ?? **Command Reference**

### **Safe Commands (Preview Only)**
```powershell
# See everything that would happen
.\scripts\Cleanup-Project.ps1 -WhatIf

# See documentation changes only
.\scripts\Cleanup-Project.ps1 -Phase Documentation -WhatIf

# See duplicate removal only
.\scripts\Cleanup-Project.ps1 -Phase Duplicates -WhatIf
```

### **Execution Commands**
```powershell
# Full cleanup with Git backup (RECOMMENDED)
.\scripts\Cleanup-Project.ps1 -Backup

# Full cleanup without backup
.\scripts\Cleanup-Project.ps1

# Documentation only
.\scripts\Cleanup-Project.ps1 -Phase Documentation

# Remove duplicates only
.\scripts\Cleanup-Project.ps1 -Phase Duplicates

# Create navigation only
.\scripts\Cleanup-Project.ps1 -Phase Navigation
```

### **Advanced Options**
```powershell
# Force overwrite existing files
.\scripts\Cleanup-Project.ps1 -Force

# Verbose output
.\scripts\Cleanup-Project.ps1 -Verbose

# Combine options
.\scripts\Cleanup-Project.ps1 -Backup -Verbose -WhatIf
```

---

## ?? **Expected Results**

### **Before Cleanup:**
```
Solidworks-Automation/
??? COMPLETE_SUCCESS_REPORT.md
??? TESTING_GUIDE.md
??? REFACTORING_SUMMARY.md
??? ... (13 more .md files)
??? Test-BundleRefactoring.ps1
??? Run-InteractiveTesting.ps1
??? Bundle/
??? Excel/
??? ... (other projects)
```

### **After Cleanup:**
```
Solidworks-Automation/
??? README.md ?? NEW!
??? docs/ ?? NEW!
?   ??? README.md ?? NEW!
?   ??? Status/
?   ?   ??? COMPLETE_SUCCESS_REPORT.md
?   ?   ??? TASKS_1_2_FINAL_STATUS.md
?   ?   ??? ... (6 status files)
?   ??? Testing/
?   ?   ??? TESTING_GUIDE.md
?   ?   ??? TESTING_QUICK_REFERENCE.md
?   ??? Migration/
?   ?   ??? MIGRATION_GUIDE.md
?   ??? Reference/
?       ??? REFACTORING_SUMMARY.md
?       ??? QUICK_START_GUIDE.md
?       ??? ... (7 reference files)
??? scripts/ ?? NEW!
?   ??? Cleanup-Project.ps1
?   ??? Test-BundleRefactoring.ps1
?   ??? Run-InteractiveTesting.ps1
?   ??? cleanup-YYYYMMDD-HHMMSS.log
??? Bundle/
??? Excel/
??? ... (other projects)
```

---

## ?? **Safety Features**

### **Built-in Protections:**
- ? **-WhatIf Support** - Preview without changes
- ? **Git Backup** - Automatic backup branch
- ? **Detailed Logging** - Every action logged
- ? **Error Handling** - Graceful failure recovery
- ? **Validation** - Checks before moving/deleting

### **Backup Strategy:**
```powershell
# Script creates Git backup branch:
backup-before-cleanup-YYYYMMDD-HHMMSS

# You can restore by:
git checkout backup-before-cleanup-20240115-143022
```

---

## ?? **Troubleshooting**

### **"Execution Policy" Error:**
```powershell
# Run this ONCE (as Administrator):
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

### **"Path Not Found" Error:**
```powershell
# Make sure you're in solution root:
cd "C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation"

# Verify you're in right place:
Test-Path ".\Bundle"  # Should return True
```

### **"Access Denied" Error:**
```powershell
# Close Visual Studio first
# Close any open .md files
# Then run cleanup
```

---

## ?? **Log Files**

### **Location:**
```
scripts/cleanup-YYYYMMDD-HHMMSS.log
```

### **Example Log:**
```
[2024-01-15 14:30:15] [Info] === PHASE 1: ORGANIZING DOCUMENTATION ===
[2024-01-15 14:30:15] [Success] Created directory: docs\Status
[2024-01-15 14:30:16] [Success] Moved: COMPLETE_SUCCESS_REPORT.md ? docs\Status\
[2024-01-15 14:30:16] [Success] Moved: TESTING_GUIDE.md ? docs\Testing\
...
[2024-01-15 14:30:45] [Success] === CLEANUP COMPLETE! ===
[2024-01-15 14:30:45] [Info] Folders created: 6
[2024-01-15 14:30:45] [Info] Files moved: 18
[2024-01-15 14:30:45] [Info] Files deleted: 3
```

---

## ? **Verification Steps**

### **After Running Cleanup:**

1. **Check folder structure:**
   ```powershell
   # Should show new structure
   Get-ChildItem -Directory
   ```

2. **Verify documentation moved:**
   ```powershell
   # Should list organized files
 Get-ChildItem docs\* -Recurse
   ```

3. **Check log file:**
   ```powershell
   # View latest log
   Get-ChildItem scripts\cleanup-*.log | Sort-Object LastWriteTime -Descending | Select-Object -First 1 | Get-Content
   ```

4. **Build solution:**
   ```powershell
   # Verify no build errors
   msbuild /t:Rebuild
   ```

---

## ?? **Recommended Workflow**

### **Step 1: Preview (30 seconds)**
```powershell
.\scripts\Cleanup-Project.ps1 -WhatIf
```
**Review output, verify it looks correct**

### **Step 2: Commit Current State (1 minute)**
```powershell
git add .
git commit -m "Before cleanup - baseline"
```

### **Step 3: Run Cleanup with Backup (2 minutes)**
```powershell
.\scripts\Cleanup-Project.ps1 -Backup
```

### **Step 4: Verify Results (1 minute)**
```powershell
# Check structure
Get-ChildItem docs -Recurse

# Check log
Get-ChildItem scripts\cleanup-*.log | Select-Object -Last 1 | Get-Content -Tail 20
```

### **Step 5: Commit Changes (1 minute)**
```powershell
git add .
git commit -m "Organized documentation and removed duplicates

- Moved all .md files to /docs with organized structure
- Removed Drawing.cs duplicate
- Removed temp files
- Created navigation READMEs
- Updated .gitignore

Total cleanup:
- 6 folders created
- 18 files organized
- 3 duplicate/temp files removed
- 800+ duplicate lines eliminated"
```

---

## ?? **Ready to Execute?**

### **Conservative Approach:**
```powershell
# 1. Preview first
.\scripts\Cleanup-Project.ps1 -WhatIf

# 2. Commit current state
git add . ; git commit -m "Before cleanup"

# 3. Run with backup
.\scripts\Cleanup-Project.ps1 -Backup

# 4. Verify
Get-ChildItem docs -Recurse
```

### **Quick Approach:**
```powershell
# Just do it (with backup safety net)
.\scripts\Cleanup-Project.ps1 -Backup
```

---

**Total Time:** 2-3 minutes  
**Risk Level:** Very Low (Git backup + WhatIf support)  
**Benefit:** Professional folder structure, 800+ duplicate lines removed  

**Ready when you are!** ??
