# ?? QUICK START GUIDE - Updated SolidWorks Automation

## For End Users

### Starting the Application
1. **Optional:** Start SolidWorks first (recommended but not required)
2. Run `Bundle.exe`
3. If SolidWorks is not running, you'll get a friendly message when you try to use SolidWorks features

### If You Get an Error
1. **Read the error message** - it will tell you exactly what to do
2. **Check the log file** - path is shown in the error dialog
3. **Try again** - most errors have a "Retry" option

### Finding Log Files
- **Windows Key + R**
- Type: `%AppData%\BundleApp\Logs`
- Press Enter
- Open the most recent `.log` file

---

## For Developers

### New Code Added

#### 1. Global Error Handler
```csharp
// Log an error
GlobalErrorHandler.LogError(exception, "Feature Name");

// Log info
GlobalErrorHandler.LogInfo("Operation started");

// Log warning
GlobalErrorHandler.LogWarning("Something unusual happened");

// Open log file
GlobalErrorHandler.OpenLogFile();
```

#### 2. COM Object Manager
```csharp
using (var comManager = new ComObjectManager())
{
    var excel = comManager.Track((Application)Marshal.GetActiveObject("Excel.Application"));
    var workbook = comManager.Track(excel.Workbooks.Open(path));
    
    // Work with COM objects...
    
} // Everything cleaned up automatically!
```

#### 3. Safe SolidWorks Access
```csharp
// Check before using
if (IsSolidWorksAvailable())
{
    var sw = SW; // Safe to use
}
else
{
    MessageBox.Show("Please start SolidWorks");
    return;
}

// Cleanup
DisconnectSolidWorks();

// Retry
ResetConnection();
```

#### 4. New Header System
```csharp
// Old way - 6 separate classes
public class Header_61 : IHeaderExtensions { /* 500 lines */ }

// New way - 1 base class
public static HeaderBase Header61 = new HeaderBase("61");
public static HeaderBase Header62 = new HeaderBase("62");
// ...all headers use same base class
```

### Key Changes to Your Code

#### Before:
```csharp
private void Button_Click(object sender, EventArgs e)
{
    // Direct call - crashes if error
    var result = SomeOperation();
}
```

#### After:
```csharp
private void Button_Click(object sender, EventArgs e)
{
    try
    {
GlobalErrorHandler.LogInfo("Button clicked");
    var result = SomeOperation();
        GlobalErrorHandler.LogInfo("Operation completed");
    }
    catch (Exception ex)
    {
        GlobalErrorHandler.LogError(ex, "Button_Click");
      MessageBox.Show($"Error: {ex.Message}");
    }
}
```

### Testing Your Changes

#### Unit Test Example:
```csharp
[TestMethod]
public void TestHeaderCreation()
{
    var header = new HeaderBase("61");
    Assert.IsNotNull(header);
    Assert.AreEqual("61", header.HeaderId);
}
```

#### Integration Test:
```csharp
[TestMethod]
public void TestSolidWorksConnection()
{
    if (IsSolidWorksAvailable())
    {
        var sw = SW;
        Assert.IsNotNull(sw);
  Assert.IsNotNull(sw.RevisionNumber());
    }
}
```

### Common Patterns

#### Pattern 1: Safe Operation
```csharp
public void DoSomethingWithSolidWorks()
{
    if (!IsSolidWorksAvailable())
    {
        MessageBox.Show("SolidWorks required");
        return;
    }
    
    try
{
      GlobalErrorHandler.LogInfo("Starting operation");
        // Your SolidWorks code here
        GlobalErrorHandler.LogInfo("Operation complete");
    }
    catch (Exception ex)
    {
        GlobalErrorHandler.LogError(ex, "DoSomethingWithSolidWorks");
        throw;
    }
}
```

#### Pattern 2: COM Cleanup
```csharp
public void WorkWithExcel()
{
    using (var comManager = new ComObjectManager())
    {
        try
        {
            var excel = comManager.Track(GetExcelApp());
            var workbook = comManager.Track(excel.Workbooks.Open(path));
         
  // Do work...
      }
        catch (Exception ex)
        {
  GlobalErrorHandler.LogError(ex, "WorkWithExcel");
            throw;
        }
  } // Automatic cleanup even if exception occurs
}
```

#### Pattern 3: Async Operation (Future)
```csharp
private async void Button_Click_Async(object sender, EventArgs e)
{
    try
    {
 await AsyncHelper.RunWithProgressAsync(
     () => LongRunningOperation(),
            "Processing... Please wait.",
            this);
        
 MessageBox.Show("Complete!");
    }
    catch (Exception ex)
    {
   GlobalErrorHandler.LogError(ex, "Async Operation");
     MessageBox.Show($"Error: {ex.Message}");
    }
}
```

### Debugging Tips

#### Enable Debug Logging:
```csharp
#if DEBUG
GlobalErrorHandler.LogDebug("Detailed debug info");
#endif
```

#### Track COM Object Counts:
```csharp
using (var comManager = new ComObjectManager())
{
    Debug.WriteLine($"Tracked objects: {comManager.TrackedObjectCount}");
    // ... work ...
    Debug.WriteLine($"Tracked objects: {comManager.TrackedObjectCount}");
}
```

#### Check Last Error:
```csharp
if (!IsSolidWorksAvailable())
{
    var lastError = LastConnectionError;
    GlobalErrorHandler.LogError(lastError, "Connection Check");
}
```

### Build and Deploy

#### Build Steps:
1. Clean Solution (Ctrl+Shift+Alt+L)
2. Rebuild Solution (Ctrl+Shift+B)
3. Check for warnings
4. Run tests (if available)
5. Test manually

#### Deploy Checklist:
- [ ] All files compiled successfully
- [ ] No warnings
- [ ] Tests pass
- [ ] Manual smoke test completed
- [ ] Log file location verified
- [ ] Error handling tested

### Troubleshooting

#### "GlobalErrorHandler not found"
```
Solution: Add using Bundle.Infrastructure;
```

#### "ComObjectManager not found"
```
Solution: Add using FileTools.Infrastructure;
```

#### "HeaderBase not found"
```
Solution: Already in FileTools.CommonData namespace
```

#### Build errors after refactoring:
```
1. Clean Solution
2. Close Visual Studio
3. Delete bin/ and obj/ folders
4. Reopen and Rebuild
```

### Performance Tips

1. **Always use `using` with ComObjectManager**
2. **Call `DisconnectSolidWorks()` when done**
3. **Don't call `IsSolidWorksAvailable()` repeatedly** - cache the result
4. **Use async methods for long operations** (coming in Phase 6)

### Code Review Checklist

When reviewing code:
- [ ] All exceptions are caught and logged
- [ ] COM objects are tracked and released
- [ ] SolidWorks availability is checked before use
- [ ] User gets helpful error messages
- [ ] Operations are logged for debugging
- [ ] Resources are properly disposed

---

## Emergency Contacts

**Critical Bug:** Create GitHub issue with log file attached  
**Questions:** Check code comments first, then ask team  
**SolidWorks API:** https://help.solidworks.com/API  

---

## Version History

**v1.0.0** - Initial refactoring
- Global error handler
- COM object manager
- Header base class
- Safe SolidWorks connection

**v1.1.0** - (Planned) Async operations
**v1.2.0** - (Planned) Dependency injection
**v2.0.0** - (Planned) Full MVVM architecture

---

## Quick Reference Card

| Task | Code |
|------|------|
| Log error | `GlobalErrorHandler.LogError(ex, "context")` |
| Log info | `GlobalErrorHandler.LogInfo("message")` |
| Track COM | `var obj = comManager.Track(comObject)` |
| Check SW | `if (IsSolidWorksAvailable()) { }` |
| Get header | `var h = Header61; h.BoxWidth = 10;` |
| Open logs | `GlobalErrorHandler.OpenLogFile()` |

---

**Last Updated:** 2024  
**Maintained By:** Development Team  
**Questions?** Check the full `REFACTORING_SUMMARY.md`
