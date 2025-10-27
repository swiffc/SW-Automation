# Quick Fix Guide

Apply these fixes to make the repository buildable on your system.

---

## ?? CRITICAL FIX #1: Change Base Class

**File:** `Solidworks Add-In/TaskpaneIntegration.cs`

**Line 20 - Change:**
```csharp
public class TaskpaneIntegration : SwAddin
```

**To:**
```csharp
public class TaskpaneIntegration : ISwAddin
```

---

## ?? CRITICAL FIX #2: Disable Version Control

**File:** `Solidworks Add-In/TaskpaneIntegration.cs`

**Lines 78-87 - Replace this:**
```csharp
// Version control
if (!Developer)
{
    VersionControl();
    SignInitials();
}
else
{
    ClearConfigFileCache();
}
```

**With this:**
```csharp
// Version control - DISABLED for development
// The original version control connects to company-specific PDM Vault
// Comment back in if you have access to AXC_VAULT
/*
if (!Developer)
{
    VersionControl();
    SignInitials();
}
else
{
    ClearConfigFileCache();
}
*/

// For now, just clear the cache
try 
{ 
    ClearConfigFileCache(); 
} 
catch 
{ 
    // Ignore if method doesn't exist
}
```

---

## ?? HIGH PRIORITY FIX #3: Enable Developer Mode

**File:** `FileTools/StaticFileTools.cs`

**Line 1489 - Change:**
```csharp
public static bool Developer => System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop).ToLower().Contains("acmurr") && DevMode ? true : false;
```

**To (Option A - Always Developer):**
```csharp
public static bool Developer => true; // Force developer mode for testing
```

**Or To (Option B - Match Your Username):**
```csharp
public static bool Developer => System.Environment.GetFolderPath(System.Environment.SpecialFolder.Desktop).ToLower().Contains("dcornealius") ? true : false;
```

---

## ?? OPTIONAL FIX #4: Unload Vault Projects

If you don't have PDM Professional installed:

1. In **Solution Explorer**
2. Right-click **"AXC_Vault"** ? **Unload Project**
3. Right-click **"Vault"** ? **Unload Project**

Then edit `Solidworks Add-In/SolidWorks Add-In.csproj`:

**Remove lines 142-145:**
```xml
<ProjectReference Include="..\Vault\AXC_Vault.csproj">
  <Project>{2521C1CB-33C8-4247-BA36-8AE3B5A505F4}</Project>
  <Name>AXC_Vault</Name>
</ProjectReference>
```

---

## ?? OPTIONAL FIX #5: Add Post-Build Registration

**File:** `Solidworks Add-In/SolidWorks Add-In.csproj`

**Before line 154 (before `</Project>`), add:**
```xml
<PropertyGroup>
  <PostBuildEvent>"$(FrameworkDir64)v4.0.30319\RegAsm.exe" /codebase "$(TargetPath)"</PostBuildEvent>
</PropertyGroup>
```

---

## ? Verification Checklist

After applying fixes:

- [ ] TaskpaneIntegration inherits from ISwAddin (not SwAddin)
- [ ] VersionControl() is disabled/commented out
- [ ] Developer mode is enabled for your system
- [ ] Vault projects unloaded (if you don't have PDM)
- [ ] Solution builds without errors

---

## ?? Test Build Order

1. **ModelTools** (should build clean)
2. **FileTools** (depends on ModelTools)
3. **SolidWorks Add-In** (main add-in)

If these three build successfully, the fixes worked!

---

## ?? Known Remaining Issues

Even after fixes, you may see:

1. **Missing methods** - Some methods like `SignInitials()` may not exist
   - Solution: Add empty method stubs or comment out calls

2. **EPDM references** - May show warnings
   - Solution: Unload vault projects (see Fix #4)

3. **Company-specific features** - Some features reference AXC infrastructure
   - Solution: These won't work but shouldn't prevent compilation

---

## ?? Ready to Build!

Once these fixes are applied:

```
1. Open: Solidworks Automation.sln
2. Restore NuGet packages
3. Build ModelTools
4. Build FileTools  
5. Build SolidWorks Add-In
6. Press F5 to test in SolidWorks
```

Good luck! ??

