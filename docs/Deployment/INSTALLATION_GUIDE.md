
# SolidWorks Automation Suite - Installation Guide

## Pre-Installation Checklist

Before installing, ensure you have:

- [ ] Windows 10/11 (64-bit) operating system
- [ ] Administrator privileges on the computer
- [ ] .NET Framework 4.8 or higher installed
- [ ] SolidWorks 2020 or higher installed and activated
- [ ] 500 MB free disk space (minimum)
- [ ] 8 GB RAM (16 GB recommended)
- [ ] Antivirus temporarily disabled (if causing conflicts)

---

## Installation Steps

### Step 1: Download the Installer

1. Download `SolidWorksAutomation.msi` from:
   - Internal network share: `\\server\Software\SolidWorksAutomation\`
   - Or download from provided link

2. Verify file integrity (optional but recommended):
   ```powershell
   Get-FileHash SolidWorksAutomation.msi -Algorithm SHA256
   ```
   Compare with provided hash value.

### Step 2: Run the Installer

1. **Right-click** `SolidWorksAutomation.msi`
2. Select **"Run as administrator"**
3. If prompted by User Account Control, click **"Yes"**

### Step 3: Welcome Screen

1. Read the welcome message
2. Click **"Next"** to continue

### Step 4: License Agreement

1. Read the End User License Agreement
2. Select **"I accept the terms in the License Agreement"**
3. Click **"Next"**

### Step 5: Choose Install Location

**Default Location**: `C:\Program Files\SolidWorksAutomation`

To change:
1. Click **"Change"**
2. Browse to desired location
3. Click **"OK"**
4. Click **"Next"**

**Recommended**: Use default location for proper functionality.

### Step 6: Select Features

Choose components to install:

✅ **Main Application** (Required)
- Core application files
- Dependencies
- Configuration files

✅ **Documentation** (Recommended)
- User guide
- API documentation
- Troubleshooting guide

□ **Sample Workflows** (Optional)
- Pre-built workflow examples
- Template workflows

Select features and click **"Next"**

### Step 7: Shortcuts

Choose shortcut locations:

- [x] Create Start Menu shortcut
- [ ] Create Desktop shortcut

Click **"Next"**

### Step 8: Ready to Install

1. Review installation summary
2. Click **"Install"** to begin
3. Wait for installation to complete (2-5 minutes)

### Step 9: Completion

1. Installation complete message appears
2. [ ] Launch SolidWorks Automation Suite
3. Click **"Finish"**

---

## Post-Installation Configuration

### First Launch Setup

When you first launch the application:

#### 1. Initial Connection

The application will attempt to connect to SolidWorks:

**If SolidWorks is Running**:
- Application attaches to running instance
- Connection status shows "Connected"

**If SolidWorks is Not Running**:
- Application prompts to start SolidWorks
- Click "Yes" to start
- Wait for connection (15-30 seconds)

#### 2. Configure Paths

1. Open Settings (Tools → Settings)
2. Navigate to "Paths" tab
3. Configure folders:

   **Templates Path**: Location of SolidWorks templates
   ```
   Recommended: C:\SolidWorks\Templates
   ```

   **Workflows Path**: Where workflows are saved
   ```
   Default: %AppData%\SolidWorksAutomation\Workflows
   ```

   **Output Path**: Generated files location
   ```
   Recommended: D:\SolidWorksOutput (or project-specific folder)
   ```

   **Logs Path**: Application logs
   ```
   Default: %AppData%\SolidWorksAutomation\Logs
   ```

4. Click "Apply" then "OK"

#### 3. Test Connection

1. Click "Connect to SolidWorks" button
2. Verify connection status shows "Connected"
3. Check SolidWorks version is displayed

---

## Verification

### Verify Installation

1. **Check Installation Folder**:
   ```
   C:\Program Files\SolidWorksAutomation\
   ```
   Should contain:
   - UnifiedUI.exe
   - Configuration files
   - Documentation folder
   - Templates folder (if selected)

2. **Check Start Menu**:
   - Windows Start
   - Search "SolidWorks Automation"
   - Shortcut should appear

3. **Check Registry**:
   Open Registry Editor (regedit) and verify:
   ```
   HKEY_CURRENT_USER\Software\SolidWorksAutomation
   ```

4. **Launch Application**:
   - Click Start Menu shortcut
   - Application should launch without errors
   - Main window appears within 5 seconds

### Test Basic Functionality

1. **Connection Test**:
   - Launch SolidWorks
   - Launch SolidWorks Automation
   - Verify connection indicator is green

2. **Document Test**:
   - Open a simple part in SolidWorks
   - In application, check that active document is detected

3. **Settings Test**:
   - Open Settings (Ctrl+,)
   - Verify all tabs are accessible
   - Try changing a setting and saving

---

## Network Installation

For deploying to multiple computers:

### Option 1: Group Policy Deployment

1. **Copy MSI** to network share:
   ```
   \\server\Software\SolidWorksAutomation\SolidWorksAutomation.msi
   ```

2. **Create GPO**:
   - Open Group Policy Management
   - Create new GPO
   - Edit → Computer Configuration → Software Settings → Software Installation
   - Right-click → New → Package
   - Browse to MSI file
   - Select "Assigned"

3. **Deploy**:
   - Link GPO to target OU
   - Force update: `gpupdate /force`
   - Installation occurs on next computer restart

### Option 2: Silent Installation

For automated deployment:

```cmd
msiexec /i SolidWorksAutomation.msi /quiet /norestart /log install.log
```

**With Parameters**:
```cmd
msiexec /i SolidWorksAutomation.msi /quiet /norestart ^
  INSTALLFOLDER="C:\CustomPath\SolidWorksAutomation" ^
  ADDLOCAL=MainApplication,Documentation ^
  /log install.log
```

**Parameters**:
- `/quiet`: No user interface
- `/norestart`: Don't restart computer
- `/log`: Create log file
- `INSTALLFOLDER`: Custom install location
- `ADDLOCAL`: Features to install

### Option 3: SCCM Deployment

For System Center Configuration Manager:

1. **Create Application**:
   - SCCM Console → Software Library → Application Management
   - Create Application → Manually specify application information

2. **Add Deployment Type**:
   - Type: Windows Installer
   - Content: Path to MSI
   - Installation program: `msiexec /i SolidWorksAutomation.msi /quiet`
   - Detection method: File system check for UnifiedUI.exe

3. **Deploy**:
   - Distribute content to distribution points
   - Deploy to device collection
   - Set deployment purpose (Required/Available)

---

## Upgrading

### Upgrade from Previous Version

The installer automatically:
1. Detects older version
2. Removes old version
3. Installs new version
4. Preserves user data (settings, workflows, logs)

**Manual Steps**:

1. **Backup User Data** (optional but recommended):
   ```
   %AppData%\SolidWorksAutomation\
   ```
   Copy to safe location

2. **Export Settings**:
   - Open application
   - Settings → Export Settings
   - Save to file

3. **Run New Installer**:
   - Follow installation steps above
   - Installer handles upgrade automatically

4. **Import Settings** (if needed):
   - Settings → Import Settings
   - Select exported file

5. **Verify**:
   - Check version: Help → About
   - Test workflows
   - Verify connections

---

## Uninstallation

### Method 1: Control Panel

1. Open **Control Panel**
2. Navigate to **Programs and Features**
3. Find **"SolidWorks Automation Suite"**
4. Click **"Uninstall"**
5. Follow prompts
6. Click **"Yes"** to confirm

### Method 2: Settings App

1. Open **Windows Settings** (Win+I)
2. Go to **Apps**
3. Search for **"SolidWorks Automation"**
4. Click application
5. Click **"Uninstall"**
6. Confirm removal

### Method 3: Silent Uninstallation

```cmd
msiexec /x {ProductGUID} /quiet /norestart /log uninstall.log
```

Or by MSI path:
```cmd
msiexec /x SolidWorksAutomation.msi /quiet /norestart /log uninstall.log
```

### What Gets Removed

The uninstaller removes:
- Application files
- Start Menu shortcuts
- Desktop shortcuts (if created)
- Registry entries

### What Gets Preserved

User data is **NOT** removed:
- Workflows: `%AppData%\SolidWorksAutomation\Workflows\`
- Settings: `%AppData%\SolidWorksAutomation\settings.json`
- Profiles: `%AppData%\SolidWorksAutomation\Profiles\`
- Logs: `%AppData%\SolidWorksAutomation\Logs\`

### Complete Removal

To remove all traces including user data:

1. Uninstall application (methods above)
2. Delete user data folder:
   ```powershell
   Remove-Item -Recurse -Force "$env:APPDATA\SolidWorksAutomation"
   ```
3. Clean registry (optional):
   ```
   HKEY_CURRENT_USER\Software\SolidWorksAutomation
   ```

---

## Troubleshooting Installation

### Issue: "Installation Failed" Error

**Cause**: Missing prerequisites or permissions

**Solution**:
1. Verify .NET Framework 4.8 is installed
2. Run installer as Administrator
3. Check disk space (need 500 MB free)
4. Disable antivirus temporarily
5. Check Windows Event Viewer for details

### Issue: ".NET Framework 4.8 Required" Error

**Solution**:
1. Download .NET Framework 4.8 from Microsoft
2. Install .NET Framework 4.8
3. Restart computer
4. Run installer again

### Issue: "SolidWorks Not Found" Warning

**Solution**:
- This is a warning, not an error
- Install SolidWorks 2020 or higher
- Or continue installation (you can install SolidWorks later)

### Issue: Installer Hangs or Freezes

**Solution**:
1. Wait 5-10 minutes (may be processing)
2. If still frozen:
   - Open Task Manager
   - End "msiexec.exe" process
   - Restart computer
   - Run installer again

### Issue: "Another Version Already Installed"

**Solution**:
1. Uninstall existing version first
2. Restart computer
3. Run new installer

---

## Advanced Configuration

### Custom Installation Paths

To install to custom location:

**During Installation**:
- Step 5: Click "Change"
- Browse to desired location
- Must have write permissions

**After Installation** (not recommended):
- Move files manually
- Update registry entries
- Update shortcuts

### Proxy Configuration

If behind corporate proxy:

1. Configure system proxy:
   ```
   Settings → Network & Internet → Proxy
   ```

2. Or configure in application:
   ```
   Settings → Network → Proxy Settings
   ```

### Firewall Configuration

Allow application through firewall:

1. Windows Firewall → Allow an app
2. Click "Change settings"
3. Click "Allow another app"
4. Browse to UnifiedUI.exe
5. Check both Private and Public
6. Click "Add"

---

## Support

### Installation Support

If you encounter issues:

1. **Check Installation Log**:
   ```
   %TEMP%\MSI*.log
   ```
   Sort by date, open most recent

2. **Contact Support**:
   - Email: support@example.com
   - Include: Installation log, error messages, system info

3. **Community Help**:
   - GitHub Issues: https://github.com/swiffc/Solidworks-Automation/issues
   - Search for similar issues first

---

**Document Version**: 1.0.0
**Last Updated**: October 29, 2025
