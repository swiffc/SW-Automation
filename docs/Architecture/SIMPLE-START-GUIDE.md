# ?? SIMPLE START GUIDE

**For**: Building and using the SolidWorks Automation Suite  
**Time**: 2 minutes to build, ready to use!

---

## ? QUICK START (3 Steps)

### STEP 1: Build Everything (30 seconds)

```powershell
# From project root:
.\BUILD-AND-DEPLOY.ps1
```

**That's it!** This script:
- ? Builds UnifiedUI (includes ALL components)
- ? Copies files to `deploy\UnifiedUI`
- ? Ready to use!

---

### STEP 2: Launch SolidWorks

Just open SolidWorks normally.

---

### STEP 3: Run UnifiedUI

```powershell
cd deploy\UnifiedUI
.\UnifiedUI.exe
```

**OR** double-click `deploy\UnifiedUI\UnifiedUI.exe`

---

## ?? WHAT YOU GET

### One Window, 4 Tools:

1. **Header Section Tool** (main tool)
   - Bundle, Header, Hood, Structure, etc.
   - 37 Excel configurations
   
2. **XCH Structure Tool**
   - Cross-flow structures
   
3. **Z Structure Tool**
   - Vertical structures
   
4. **Hudson Certified**
   - Certified drawings

### How to Use:

1. **Select tool** from dropdown at top
2. **Pick component** from tabs
3. **Enter parameters** or import from Prego
4. **Click Generate**
5. **Done!** SolidWorks creates your parts

---

## ?? DEVELOPMENT (If You Want to Modify)

### Open in Visual Studio:

```
macros\csharp\Solidworks-Automation\Solidworks Automation.sln
```

### Build Order (Automatic):

1. FileTools builds first (shared by all)
2. Components build next (Bundle, Header, etc.)
3. UnifiedUI builds last (includes everything)

### Just Press F5:

- Builds in correct order
- Launches UnifiedUI
- Ready to debug

---

## ?? DAILY WORKFLOW

### Morning:
```powershell
# Get latest code
git pull

# Rebuild
.\BUILD-AND-DEPLOY.ps1
```

### Working:
```powershell
# Open in Visual Studio
cd macros\csharp\Solidworks-Automation
start "Solidworks Automation.sln"

# Make changes, press F5 to test
```

### End of Day:
```powershell
# Commit changes
git add -A
git commit -m "feat: your changes"
git push
```

---

## ? COMMON QUESTIONS

**Q: Do I need to build all 22 projects separately?**  
A: No! Just run `BUILD-AND-DEPLOY.ps1` - it builds everything in one go.

**Q: Which .exe do I run?**  
A: `UnifiedUI.exe` - it has all components built-in.

**Q: What if I only want to build Bundle?**  
A: You can't - UnifiedUI needs all components. But don't worry, build is fast (30 seconds).

**Q: Where are my templates?**  
A: In `templates\` folder - script doesn't touch them (they're 2.5 GB!).

**Q: How do I add a new component?**  
A: Add project to the solution, reference FileTools, add to UnifiedUI. That's it!

**Q: What about the old UIs (BundleUI.exe, HeaderUI.exe)?**  
A: Legacy - UnifiedUI replaces them all. But they still work if needed.

---

## ?? THAT'S IT!

**To build and use:**
1. `.\BUILD-AND-DEPLOY.ps1`
2. Open SolidWorks
3. Run `deploy\UnifiedUI\UnifiedUI.exe`

**Everything else is optional!**

---

## ?? NEED HELP?

**Check logs:**
- UnifiedUI creates logs in `C:\Temp\UnifiedUI_YYYYMMDD_HHMMSS.log`
- Look for errors if something fails

**Build fails?**
- Run Visual Studio as Administrator
- Close any running UnifiedUI.exe
- Check that SolidWorks is installed

**Components not working?**
- Make sure SolidWorks is running first
- Check that templates exist in `templates\` folder
- Verify Excel is installed (for Prego import)

---

**Last Updated**: October 28, 2025  
**Status**: ? Production Ready - Keep It Simple!

