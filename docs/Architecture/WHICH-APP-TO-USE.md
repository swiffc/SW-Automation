# ? WHICH APP TO USE - SIMPLE ANSWER

**Last Updated**: October 28, 2025

---

## ?? THE SIMPLE ANSWER:

**Use ONLY:** `UnifiedUI.exe`

**Location:** `deploy\UnifiedUI\UnifiedUI.exe`

---

## ? Why Do I Have Multiple Apps?

You're seeing **TWO sets** of applications:

### ? OLD APPS (Legacy - IGNORE THESE):

```
?? Bundle.exe           ? Makes bundles only
?? Header.exe           ? Makes headers only
?? Hood.exe             ? Makes hoods only
?? MachineryMount.exe   ? Makes machinery mounts only
?? Plenum.exe           ? Makes plenums only
?? Structure.exe        ? Makes structures only
?? Walkway.exe          ? Makes walkways only
```

**Status**: Legacy (old WinForms apps from years ago)  
**Do I use these?**: NO ?  
**Why do they exist?**: Backup, code libraries, developer reference  

---

### ? NEW APP (Modern - USE THIS):

```
?? UnifiedUI.exe        ? Makes EVERYTHING!
   ? Bundle
   ? Header
   ? Hood
   ? Machinery Mount
   ? Plenum
   ? Structure
   ? Walkway
   ? Tool selector for different projects
```

**Status**: Modern (WPF, what we built today)  
**Do I use this?**: YES ?  
**Why?**: One app for everything, modern interface, tool selector  

---

## ?? COMPARISON:

| Feature | Old Apps (7 separate) | UnifiedUI (1 app) |
|---------|----------------------|-------------------|
| **How many apps?** | 7 different .exe files | 1 .exe file |
| **Interface** | Old WinForms | Modern WPF |
| **Tool selector** | ? No | ? Yes |
| **Easy to use** | ? Switch between apps | ? One window |
| **Maintained** | ? No | ? Yes |
| **Recommended** | ? No | ? YES! |

---

## ?? WHAT TO DO:

### For Daily Work:

1. **Run ONLY:** `deploy\UnifiedUI\UnifiedUI.exe`
2. **Use tool selector** at top to switch between:
   - Header Section Tool (main tool)
   - XCH Structure Tool
   - Z Structure Tool
   - Hudson Certified (coming in Phase 2)
3. **Pick component** from tabs
4. **Generate!**

### Quick Build & Deploy:

```powershell
.\BUILD-AND-DEPLOY.ps1
```

This builds UnifiedUI and puts it in `deploy\UnifiedUI\`

---

## ?? WHY DO OLD APPS STILL EXIST?

**In the Solution Explorer, you see all projects because:**

1. **UnifiedUI USES their code**
   - Bundle.dll contains the logic for generating bundles
   - Header.dll contains the logic for generating headers
   - Etc.

2. **UnifiedUI is the WRAPPER**
   - It provides the modern interface
   - It adds the tool selector
   - It combines everything in one place

3. **Think of it like:**
   - ?? UnifiedUI = Your smartphone
   - ?? Bundle.dll = Camera app code
   - ?? Header.dll = Calculator app code
   - ?? Hood.dll = Music app code
   
   Your phone (UnifiedUI) USES the apps (DLLs) but you only run the phone!

---

## ? FAQ

**Q: Can I delete the old apps?**  
A: Not from the solution - UnifiedUI needs their code (as .dll files).  
   But you don't need to RUN them!

**Q: What if I need a feature from the old apps?**  
A: UnifiedUI should have everything. If something's missing, let us know!

**Q: Do I need to keep Bundle.exe, Header.exe, etc.?**  
A: They exist as backups, but you should use UnifiedUI.

**Q: Why does BUILD-AND-DEPLOY.ps1 build all projects?**  
A: Because UnifiedUI USES their code. They all get compiled into .dll files that UnifiedUI loads.

---

## ? FINAL ANSWER:

**You need ONLY ONE app:**

```
deploy\UnifiedUI\UnifiedUI.exe
```

**Everything else exists for:**
- Code libraries (UnifiedUI uses them)
- Backup (if needed)
- Developer reference

**You should:**
- ? Use UnifiedUI.exe daily
- ? Ignore the old standalone apps
- ? Run BUILD-AND-DEPLOY.ps1 to build
- ? Enjoy the new tool selector! ??

---

**Still confused?** Just remember:

**ONE APP = UnifiedUI.exe** ?

That's all you need!


