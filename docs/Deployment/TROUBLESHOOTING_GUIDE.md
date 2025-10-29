# SolidWorks Automation Suite - Troubleshooting Guide

## Quick Diagnostics

### System Health Check

Run these checks first:

1. **SolidWorks Status**: Is SolidWorks installed and licensed?
2. **Connection Status**: Can the application connect to SolidWorks?
3. **File Access**: Can the application read/write to configured folders?
4. **Dependencies**: Is .NET Framework 4.8 installed?

### Diagnostic Information

To get diagnostic info:
```
Help → About → Copy System Info
```

This provides:
- Application version
- SolidWorks version
- .NET version
- OS version
- Memory usage
- Connected status

---

## Common Issues

### 1. Connection Issues

#### Problem: Cannot Connect to SolidWorks

**Error Messages**:
- "Failed to connect to SolidWorks"
- "SolidWorks not found"
- "COM Error: Class not registered"

**Symptoms**:
- Connection button stays disabled
- Application freezes when connecting
- Error dialog appears

**Solutions**:

**Solution 1: Restart Both Applications**
```
1. Close SolidWorks Automation
2. Close all SolidWorks instances
3. Open Task Manager → End any SW processes
4. Launch SolidWorks first
5. Launch SolidWorks Automation
6. Click "Connect"
```

**Solution 2: Run as Administrator**
```
1. Right-click application shortcut
2. Select "Run as administrator"
3. Allow UAC prompt
4. Try connecting again
```

**Solution 3: Repair SolidWorks Installation**
```
1. Control Panel → Programs and Features
2. Select SolidWorks
3. Click "Repair"
4. Complete repair process
5. Restart computer
```

**Solution 4: Check COM Registration**
```powershell
# Run in PowerShell as Admin
$swPath = "C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe"
& $swPath /regserver
```

---

### 2. Workflow Execution Issues

#### Problem: Workflow Fails Immediately

**Symptoms**:
- Workflow starts then stops
- No steps execute
- Error in execution log

**Solutions**:

**Check Workflow Validation**
```
1. Open workflow editor
2. Look for validation errors (red X icons)
3. Fix invalid steps
4. Save and try again
```

**Verify File Paths**
```
1. Check all file paths in workflow steps
2. Ensure files exist
3. Use absolute paths, not relative
4. Check for typos
```

**Increase Timeouts**
```
1. Edit workflow
2. For each step, increase timeout
3. Recommended: 300 seconds minimum
4. Save changes
```

#### Problem: Some Steps Fail But Others Succeed

**Symptoms**:
- Workflow partially completes
- Some steps marked as failed
- Error messages in step results

**Solutions**:

**Enable Continue on Error**
```
1. Edit failing step
2. Check "Continue on Error"
3. Save workflow
4. Re-run
```

**Add Retry Logic**
```
1. Edit failing step
2. Set "Retry Count" to 3
3. Save and re-run
```

**Check Step Dependencies**
```
1. Verify steps execute in correct order
2. Check "Depends On" relationships
3. Ensure prerequisites are met
```

---

### 3. Performance Issues

#### Problem: Application is Slow or Freezing

**Symptoms**:
- UI becomes unresponsive
- Operations take longer than expected
- High CPU or memory usage

**Solutions**:

**Close Unnecessary Documents**
```
1. In SolidWorks, close all documents
2. Keep only documents being processed
3. Use File → Close All
```

**Reduce Parallel Workflows**
```
Settings → Workflows
- Set "Max Parallel Workflows" to 1
- Disable "Enable Parallel Execution"
```

**Increase System Resources**
```
1. Close other applications
2. Check Task Manager for resource hogs
3. Consider upgrading RAM
```

**Clear Cache**
```
Settings → Performance
- Click "Clear Cache"
- Restart application
```

#### Problem: SolidWorks Becomes Unresponsive

**Symptoms**:
- SolidWorks window frozen
- "Not Responding" in title bar
- Cannot interact with SolidWorks

**Solutions**:

**Wait for Operation to Complete**
```
- Large assemblies can take time
- Check Windows Task Manager for activity
- Wait 5-10 minutes before forcing close
```

**Disable Automatic Rebuild**
```
SolidWorks → Tools → Options
- System Options → Performance
- Uncheck "Automatic rebuild"
```

---

### 4. File Access Issues

#### Problem: Cannot Open Documents

**Error Messages**:
- "File not found"
- "Access denied"
- "File is read-only"

**Solutions**:

**Check File Permissions**
```
1. Right-click file → Properties
2. Security tab
3. Ensure your user has Read/Write
4. If not, click Edit → Add permissions
```

**Check File is Not Open**
```
1. Look for file in SolidWorks
2. Close if open
3. Check Task Manager for hidden SolidWorks instances
```

**Verify Network Path**
```
1. If file on network, check connection
2. Try accessing via File Explorer
3. Map network drive if needed
```

#### Problem: Cannot Save Files

**Error Messages**:
- "Failed to save document"
- "Disk is full"
- "Path not found"

**Solutions**:

**Check Disk Space**
```
1. Open File Explorer
2. Right-click drive → Properties
3. Verify free space > 1 GB
4. Clean up if needed
```

**Check Output Folder Exists**
```
Settings → Paths
- Verify "Output Path" exists
- Create folder if missing
```

**Check Write Permissions**
```
1. Navigate to output folder
2. Try creating a test file
3. If fails, check permissions
```

---

### 5. Property Update Issues

#### Problem: Properties Not Updating

**Symptoms**:
- Set property operation succeeds
- But property value unchanged
- No error message

**Solutions**:

**Check Property Name**
```
1. Property names are case-sensitive
2. Verify exact spelling
3. No extra spaces
```

**Check Configuration**
```
1. Specify correct configuration name
2. Or use "" for file-level properties
3. Check configuration exists
```

**Force Rebuild**
```
1. After setting property
2. Add "Rebuild Document" step
3. Re-run workflow
```

**Manual Verification**
```
1. Open document in SolidWorks
2. File → Properties
3. Check property manually
4. If there, may be display issue
```

---

### 6. Export Issues

#### Problem: Export Fails

**Error Messages**:
- "Export failed"
- "Invalid format"
- "Cannot create file"

**Solutions**:

**Verify Format Support**
```
Supported formats:
- PDF, DXF, DWG (drawings)
- STEP, IGES, Parasolid (parts/assemblies)
- STL (3D printing)
```

**Check Output Path**
```
1. Ensure output folder exists
2. Check write permissions
3. Verify disk space
```

**Use Full File Path**
```
Wrong: "export.pdf"
Right: "C:\Output\export.pdf"
```

#### Problem: PDF Export is Blank

**Symptoms**:
- PDF file created
- But content is blank or black

**Solutions**:

**Update Graphics Drivers**
```
1. Visit GPU manufacturer website
2. Download latest drivers
3. Install and restart
```

**Change PDF Export Settings**
```
SolidWorks → Tools → Options
- System Options → Export
- PDF Options
- Try different settings
```

---

### 7. Installation Issues

#### Problem: Installation Fails

**Error Messages**:
- "Installation failed"
- ".NET Framework required"
- "Insufficient permissions"

**Solutions**:

See [Installation Guide](INSTALLATION_GUIDE.md) for detailed troubleshooting.

Quick fixes:
1. Run installer as Administrator
2. Install .NET Framework 4.8
3. Check disk space
4. Disable antivirus temporarily

---

### 8. Licensing Issues

#### Problem: "SolidWorks License Error"

**Symptoms**:
- Connection succeeds initially
- Operations fail with license error
- SolidWorks shows license warning

**Solutions**:

**Check SolidWorks License**
```
SolidWorks → Help → Check Licensing
```

**Restart License Service**
```
Services → SolidWorks FlexNet Licensing Service
- Right-click → Restart
```

**Contact IT Support**
```
- License may be expired
- May need license server update
- Check with IT department
```

---

## Advanced Troubleshooting

### Enable Debug Logging

To get detailed logs:

```
1. Open Settings
2. Navigate to Advanced → Logging
3. Set "Log Level" to "Debug"
4. Click "Apply"
5. Restart application
6. Reproduce issue
7. Check logs
```

Logs location: `%AppData%\SolidWorksAutomation\Logs\`

### Check Event Viewer

Windows Event Viewer may have additional errors:

```
1. Press Win+X → Event Viewer
2. Windows Logs → Application
3. Look for errors from "SolidWorksAutomation"
4. Note error codes and messages
```

### Clean Reinstall

If all else fails:

```
1. Export settings and workflows
2. Uninstall application completely
3. Delete %AppData%\SolidWorksAutomation
4. Restart computer
5. Reinstall application
6. Import settings
```

---

## Error Codes

Common error codes and meanings:

| Code | Meaning | Solution |
|------|---------|----------|
| E001 | SolidWorks not found | Install SolidWorks |
| E002 | Connection timeout | Increase timeout, restart SW |
| E003 | File not found | Check file path |
| E004 | Access denied | Check permissions |
| E005 | Invalid parameter | Check step configuration |
| E006 | Workflow validation failed | Fix validation errors |
| E007 | Operation timeout | Increase step timeout |
| E008 | COM error | Restart both applications |
| E009 | Disk full | Free up disk space |
| E010 | License error | Check SolidWorks license |

---

## Getting Help

### Before Contacting Support

Gather this information:

1. **System Info**: Help → About → Copy System Info
2. **Error Messages**: Screenshot or copy exact text
3. **Log Files**: From `%AppData%\SolidWorksAutomation\Logs\`
4. **Steps to Reproduce**: Exact steps that cause the issue
5. **Workflow File**: If workflow-related (export as JSON)

### Support Channels

**Email Support**:
- Email: support@example.com
- Include all gathered information
- Response time: 24-48 hours

**GitHub Issues**:
- https://github.com/swiffc/Solidworks-Automation/issues
- Search existing issues first
- Create new issue with template

**Community Forum**:
- Coming soon

---

## Known Issues

### Current Known Issues

1. **Large Assembly Performance**: Assemblies with 1000+ components may be slow
   - Workaround: Process in smaller batches
   
2. **PDF Export Blank**: Some graphics cards produce blank PDFs
   - Workaround: Update graphics drivers
   
3. **Network Files**: UNC paths may timeout
   - Workaround: Map network drive

### Planned Fixes

These issues will be addressed in upcoming releases:
- Improved error messages
- Better timeout handling
- Enhanced logging
- Performance optimizations

---

## Self-Help Resources

### Documentation
- [User Guide](../UserGuide/USER_GUIDE.md)
- [Installation Guide](INSTALLATION_GUIDE.md)
- API Documentation (Help → API Docs)

### Sample Files
- Sample workflows in: `C:\Program Files\SolidWorksAutomation\Workflows\Samples\`
- Example configurations in Settings

### Video Tutorials
- Coming soon

---

**Document Version**: 1.0.0
**Last Updated**: October 29, 2025
