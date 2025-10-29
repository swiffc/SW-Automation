
# SolidWorks Automation Suite - Installer

## Overview

This directory contains the WiX (Windows Installer XML) configuration for creating the SolidWorks Automation Suite installer.

## Prerequisites

1. **WiX Toolset 3.11+**: Download from https://wixtoolset.org/
2. **Visual Studio 2022**: With WiX extension installed
3. **.NET Framework 4.8**: Must be installed on build machine
4. **SolidWorks 2020+**: For testing the installer

## Building the Installer

### Using Command Line

```cmd
# Navigate to installer directory
cd Installer

# Build the installer
candle Product.wxs
light Product.wixobj -out SolidWorksAutomation.msi
```

### Using Visual Studio

1. Add WiX project to solution
2. Build in Release configuration
3. Installer will be created in `bin\Release\`

## Installer Features

### Main Application
- Core UnifiedUI application
- All dependencies (Newtonsoft.Json, etc.)
- Configuration files
- Registry entries

### Documentation
- User guide
- API documentation
- Troubleshooting guide
- Sample workflows

### Templates
- SolidWorks part templates
- Assembly templates
- Drawing templates

### Workflows
- Pre-built workflow definitions
- Sample automation scripts

## Installation Options

- **Install Location**: Customizable (default: C:\Program Files\SolidWorksAutomation)
- **Start Menu Shortcuts**: Yes
- **Desktop Shortcut**: Optional
- **Documentation**: Optional

## System Requirements

- **OS**: Windows 10/11 (64-bit)
- **.NET Framework**: 4.8 or higher
- **SolidWorks**: 2020 or higher
- **Disk Space**: 500 MB minimum
- **RAM**: 8 GB recommended

## Post-Installation

After installation:
1. Launch "SolidWorks Automation" from Start Menu
2. Configure paths in Settings
3. Connect to SolidWorks
4. Import sample workflows

## Upgrade Process

The installer supports:
- **Major Upgrades**: Automatic removal of older versions
- **Minor Updates**: In-place updates
- **Rollback**: Automatic if installation fails

## Uninstallation

Uninstall via:
- Control Panel > Programs and Features
- Settings > Apps > SolidWorks Automation Suite

Uninstallation removes:
- Application files
- Shortcuts
- Registry entries

**Note**: User data (workflows, settings, logs) is preserved by default.

## Troubleshooting

### Build Errors

**Error**: "Cannot find Product.wxs"
- Ensure you're in the Installer directory
- Check file paths in Product.wxs

**Error**: ".NET Framework not found"
- Install .NET Framework 4.8 SDK
- Rebuild the application in Release mode

**Error**: "SolidWorks detection failed"
- SolidWorks detection is for runtime, not build time
- Installer will still build successfully

### Installation Errors

**Error**: "Installation failed - .NET Framework 4.8 required"
- Install .NET Framework 4.8 from Microsoft
- Restart and try again

**Error**: "SolidWorks not detected"
- Install SolidWorks 2020 or higher
- Or skip the check (advanced users only)

## Customization

### Changing Install Location
Edit Product.wxs:
```xml
<Directory Id="INSTALLFOLDER" Name="YourFolderName">
```

### Adding Features
1. Define new Feature in Product.wxs
2. Create ComponentGroup
3. Add files to ComponentGroup

### Custom Dialogs
Add WiX dialog references:
```xml
<UIRef Id="WixUI_InstallDir"/>
```

## Distribution

### MSI Package
- **File**: SolidWorksAutomation.msi
- **Size**: ~50 MB
- **Format**: Windows Installer

### Distribution Channels
- Internal network share
- USB drive
- Download portal
- Microsoft Store (future)

## Security

- **Code Signing**: Recommended (use SignTool.exe)
- **Hash Verification**: SHA256
- **Elevation**: Required (admin rights)

## Support

For installer issues:
- Check logs in `%TEMP%\`
- Review Windows Event Viewer
- Contact support team

## Version History

- **1.0.0**: Initial release
  - Core application
  - Basic features
  - Sample workflows

## Future Enhancements

- [ ] Silent installation support
- [ ] Custom action for SolidWorks plugin registration
- [ ] Automatic updates via ClickOnce
- [ ] Multi-language support
- [ ] Network deployment via Group Policy
