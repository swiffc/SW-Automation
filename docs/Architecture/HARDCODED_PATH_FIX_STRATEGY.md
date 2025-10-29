# ??? HARDCODED PATH FIX STRATEGY

**Date:** October 28, 2025  
**Issue:** 30 Hardcoded Production Paths Found  
**Pattern:** `C:\AXC_VAULT\Active\{Job}\...`  
**Impact:** Development environment incompatibility

---

## ?? PROBLEM SUMMARY

### Hardcoded Paths Found
**Total:** 30 occurrences across 9 files

**Locations:**
- `docs/Testing/PREGO_IMPORT_TESTING_GUIDE.md` (2 refs)
- `docs/User_Guide/PREGO_IMPORT_USER_GUIDE.md` (4 refs)
- `Solidworks Add-In/TaskpaneIntegration.cs` (1 ref)
- `docs/Analysis/PREGO_IMPORT_GAP_ANALYSIS.md` (1 ref)
- `docs/Status/TASK2_COMPLETION_STATUS.md` (1 ref)
- `docs/Reference/IMMEDIATE_ACTION_PLAN.md` (1 ref)
- `docs/Reference/RESCAN_RESULTS.md` (12 refs)
- `docs/Reference/FIXES_APPLIED.md` (3 refs)
- `docs/Reference/REPOSITORY_ANALYSIS.md` (5 refs)

### Why This Is a Problem
1. ? **Development machines** don't have access to `C:\AXC_VAULT`
2. ? **Local testing** requires different paths
3. ? **CI/CD pipelines** can't access network drives
4. ? **New developers** can't run the application
5. ? **Testing** requires production infrastructure

---

## ? SOLUTION: Configuration-Based Path System

### Phase 1: Already Implemented - config.json
**Status:** ? COMPLETE

The project already has `config.json` for path configuration:

```json
{
  "paths": {
    "vault_root": "C:\\AXC_VAULT\\Active",
    "local_output": "C:\\Users\\{User}\\CascadeProjects\\Solidworks_Automation\\output",
    "templates": "C:\\Users\\{User}\\CascadeProjects\\Solidworks_Automation\\templates"
  },
  "environment": {
    "mode": "development",
    "use_vault": false
  }
}
```

### Phase 2: Path Manager Pattern (RECOMMENDED)

Create a centralized path management class:

**File:** `FileTools/PathManager.cs`

```csharp
using System;
using System.IO;
using Newtonsoft.Json.Linq;

namespace FileTools
{
    public static class PathManager
    {
        private static JObject _config;
        private static readonly object _lock = new object();

        static PathManager()
        {
            LoadConfig();
        }

        private static void LoadConfig()
        {
            try
            {
                string configPath = Path.Combine(
                    AppDomain.CurrentDomain.BaseDirectory, 
                    "..", "..", "..", "..", "..", "config.json");
                string fullPath = Path.GetFullPath(configPath);
                
                if (File.Exists(fullPath))
                {
                    string json = File.ReadAllText(fullPath);
                    _config = JObject.Parse(json);
                }
                else
                {
                    // Default configuration
                    _config = CreateDefaultConfig();
                }
            }
            catch (Exception ex)
            {
                GlobalErrorHandler.LogError(ex, "PathManager.LoadConfig");
                _config = CreateDefaultConfig();
            }
        }

        private static JObject CreateDefaultConfig()
        {
            return JObject.Parse(@"{
                'paths': {
                    'vault_root': 'C:\\AXC_VAULT\\Active',
                    'local_output': 'C:\\Users\\Public\\Solidworks_Automation\\output',
                    'templates': 'C:\\Users\\Public\\Solidworks_Automation\\templates'
                },
                'environment': {
                    'mode': 'development',
                    'use_vault': false
                }
            }");
        }

        public static bool IsDevelopmentMode => 
            _config["environment"]["mode"].ToString() == "development";

        public static bool UseVault => 
            _config["environment"]["use_vault"].ToObject<bool>();

        /// <summary>
        /// Gets the root path for job folders
        /// Returns vault path in production, local output in development
        /// </summary>
        public static string GetJobRootPath()
        {
            if (UseVault && !IsDevelopmentMode)
            {
                return _config["paths"]["vault_root"].ToString();
            }
            else
            {
                return _config["paths"]["local_output"].ToString();
            }
        }

        /// <summary>
        /// Gets full path to job folder
        /// Example: GetJobPath("J12345") 
        /// Production: C:\AXC_VAULT\Active\J12345
        /// Development: C:\Users\...\output\J12345
        /// </summary>
        public static string GetJobPath(string jobNumber)
        {
            string root = GetJobRootPath();
            return Path.Combine(root, jobNumber);
        }

        /// <summary>
        /// Gets path to Prego Excel file
        /// </summary>
        public static string GetPregoPath(string jobNumber, string bank)
        {
            string jobPath = GetJobPath(jobNumber);
            return Path.Combine(
                jobPath, 
                "Drafting", 
                "Headers", 
                "~Archive", 
                $"{jobNumber}-prego{bank}.xlsm"
            );
        }

        /// <summary>
        /// Gets path to header section config Excel
        /// </summary>
        public static string GetHeaderConfigPath(string jobNumber, string section)
        {
            string jobPath = GetJobPath(jobNumber);
            return Path.Combine(
                jobPath,
                "Drafting",
                "Headers",
                $"{jobNumber}_{section}-HCS.xlsx"
            );
        }

        /// <summary>
        /// Gets templates root path
        /// </summary>
        public static string GetTemplatesPath()
        {
            return _config["paths"]["templates"].ToString();
        }

        /// <summary>
        /// Gets path to specific template folder
        /// </summary>
        public static string GetTemplatePath(string toolName)
        {
            string templatesRoot = GetTemplatesPath();
            return Path.Combine(templatesRoot, toolName);
        }

        /// <summary>
        /// Ensures a directory exists, creates if needed
        /// </summary>
        public static void EnsureDirectoryExists(string path)
        {
            if (!Directory.Exists(path))
            {
                Directory.CreateDirectory(path);
                GlobalErrorHandler.LogInfo($"Created directory: {path}");
            }
        }
    }
}
```

---

## ?? MIGRATION PLAN

### Step 1: Update Code Files (1 file)
**File:** `Solidworks Add-In/TaskpaneIntegration.cs`

**Before:**
```csharp
string vaultPath = @"C:\AXC_VAULT\Active\" + jobNumber;
```

**After:**
```csharp
using FileTools;

string jobPath = PathManager.GetJobPath(jobNumber);
```

### Step 2: Update Documentation (8 files)
Update all documentation files to show BOTH paths:

**Pattern:**
```markdown
### File Paths

**Production (AXC_VAULT):**
```
C:\AXC_VAULT\Active\{JobNo}\Drafting\Headers\~Archive\{Job}-prego{Bank}.xlsm
```

**Development (Local):**
```
C:\Users\{User}\CascadeProjects\Solidworks_Automation\output\{JobNo}\Drafting\Headers\~Archive\{Job}-prego{Bank}.xlsm
```

**Configuration:** See `config.json` to switch between modes.
```

---

## ?? IMPLEMENTATION CHECKLIST

### Phase 1: Create PathManager (1-2 hours)
- [ ] Create `FileTools/PathManager.cs`
- [ ] Add Newtonsoft.Json reference to FileTools.csproj
- [ ] Test PathManager in development mode
- [ ] Test PathManager in production mode

### Phase 2: Update Code (30 minutes)
- [ ] Update `TaskpaneIntegration.cs` (1 reference)
- [ ] Search for other hardcoded paths in .cs files
- [ ] Test all changed code paths

### Phase 3: Update Documentation (1 hour)
- [ ] Update PREGO_IMPORT_TESTING_GUIDE.md
- [ ] Update PREGO_IMPORT_USER_GUIDE.md
- [ ] Update other documentation files
- [ ] Add section about config.json to README

### Phase 4: Testing (1 hour)
- [ ] Test in development mode (local paths)
- [ ] Test in production mode (vault paths)
- [ ] Verify path switching works correctly
- [ ] Test with missing config.json (should use defaults)

---

## ?? USAGE EXAMPLES

### Example 1: Get Job Path
```csharp
using FileTools;

// Automatically uses correct path based on config.json
string jobPath = PathManager.GetJobPath("J12345");

// Development: C:\Users\...\output\J12345
// Production: C:\AXC_VAULT\Active\J12345
```

### Example 2: Get Prego Path
```csharp
string pregoPath = PathManager.GetPregoPath("J12345", "1");

// Development: C:\Users\...\output\J12345\Drafting\Headers\~Archive\J12345-prego1.xlsm
// Production: C:\AXC_VAULT\Active\J12345\Drafting\Headers\~Archive\J12345-prego1.xlsm
```

### Example 3: Ensure Directory Exists
```csharp
string jobPath = PathManager.GetJobPath("J12345");
PathManager.EnsureDirectoryExists(jobPath);

// Creates directory if it doesn't exist
// Logs creation for debugging
```

### Example 4: Switch Environments
**In config.json:**
```json
{
  "environment": {
    "mode": "production",
    "use_vault": true
  }
}
```

All code automatically switches to vault paths. No code changes needed!

---

## ?? CONFIG.JSON CONFIGURATION

### Development Mode (Default)
```json
{
  "paths": {
    "vault_root": "C:\\AXC_VAULT\\Active",
    "local_output": "C:\\Users\\DCornealius\\CascadeProjects\\Solidworks_Automation\\output",
    "templates": "C:\\Users\\DCornealius\\CascadeProjects\\Solidworks_Automation\\templates"
  },
  "environment": {
    "mode": "development",
    "use_vault": false
  }
}
```

### Production Mode
```json
{
  "paths": {
    "vault_root": "C:\\AXC_VAULT\\Active",
    "local_output": "C:\\Users\\DCornealius\\CascadeProjects\\Solidworks_Automation\\output",
    "templates": "C:\\AXC_VAULT\\Templates"
  },
  "environment": {
    "mode": "production",
    "use_vault": true
  }
}
```

### CI/CD Mode
```json
{
  "paths": {
    "vault_root": "C:\\AXC_VAULT\\Active",
    "local_output": "C:\\BuildAgent\\output",
    "templates": "C:\\BuildAgent\\templates"
  },
  "environment": {
    "mode": "ci",
    "use_vault": false
  }
}
```

---

## ?? IMPACT ANALYSIS

### Files to Update
| Category | Count | Effort |
|----------|-------|--------|
| Code Files (.cs) | 1 | Low |
| Documentation (.md) | 8 | Low |
| New Files | 1 | Medium |
| Tests | 0 | None |
| **Total** | **10** | **Low-Medium** |

### Benefits
- ? Developers can work locally
- ? CI/CD pipelines can run tests
- ? Easy environment switching
- ? No hardcoded paths in code
- ? Centralized path management
- ? Better error handling
- ? Easier onboarding for new devs

### Risks
- ?? Config.json must be maintained
- ?? All developers need correct config
- ?? Path structure must remain consistent

---

## ?? ROLLOUT PLAN

### Week 1: Development
- Day 1-2: Create PathManager.cs
- Day 3: Update code files
- Day 4: Update documentation
- Day 5: Testing

### Week 2: Validation
- Day 1-2: Test in development mode
- Day 3: Test in production mode
- Day 4: User acceptance testing
- Day 5: Deploy to production

### Week 3: Migration
- Update all developer machines
- Update production config
- Monitor for issues
- Fix any path resolution problems

---

## ?? SUPPORT & ROLLBACK

### If Issues Occur
1. **Check config.json** - Is it correctly formatted?
2. **Check mode setting** - development vs production?
3. **Check paths exist** - Do the configured paths exist?
4. **Check permissions** - Can the app access the paths?

### Rollback Plan
If PathManager causes issues:
1. Revert code changes (1 file)
2. Use hardcoded paths temporarily
3. Fix PathManager issues
4. Re-deploy when fixed

No data loss occurs - just path resolution changes.

---

**End of Strategy**  
**Status:** ?? Ready for Implementation  
**Estimated Effort:** 4-6 hours  
**Priority:** ?? P1 - High (After build error fixed)

