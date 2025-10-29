
# SolidWorks Automation Suite - User Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Main Features](#main-features)
4. [Workflow Management](#workflow-management)
5. [Settings & Configuration](#settings--configuration)
6. [Troubleshooting](#troubleshooting)
7. [FAQ](#faq)

---

## Introduction

Welcome to the SolidWorks Automation Suite! This comprehensive application enables you to automate repetitive SolidWorks tasks, create custom workflows, and boost productivity.

### Key Benefits

- **Automation**: Automate document operations, property editing, and feature manipulation
- **Workflows**: Create, save, and reuse complex automation sequences
- **Efficiency**: Process multiple files in batch operations
- **Consistency**: Ensure standardized processes across projects
- **Flexibility**: Customize workflows to match your specific needs

---

## Getting Started

### System Requirements

- **Operating System**: Windows 10/11 (64-bit)
- **.NET Framework**: 4.8 or higher
- **SolidWorks**: 2020 or higher
- **RAM**: 8 GB minimum (16 GB recommended)
- **Disk Space**: 500 MB for application + space for templates

### Installation

1. **Download** the installer: `SolidWorksAutomation.msi`
2. **Run** the installer with administrator privileges
3. **Follow** the installation wizard
4. **Select** features to install (Main Application, Documentation, Samples)
5. **Choose** installation location (default: `C:\Program Files\SolidWorksAutomation`)
6. **Complete** installation and launch the application

### First Launch

On first launch, the application will:
1. Create default folders for templates, workflows, and logs
2. Initialize default settings
3. Create a default settings profile
4. Attempt to connect to SolidWorks

**Important**: Ensure SolidWorks is closed before first launch for clean connection setup.

---

## Main Features

### 1. Component Generation

The application supports automated generation of:
- **Bundles**: Heat exchanger tube bundles
- **Headers**: Inlet/outlet headers (types 61-66)
- **Structures**: XCH and Z-type structures
- **Hoods**: Equipment enclosures
- **Plenums**: Air distribution chambers
- **Walkways**: Access platforms
- **Machinery Mounts**: Equipment mounting structures

#### Generating Components

1. **Select Component Type** from the navigation panel
2. **Enter Parameters**: Job number, dimensions, specifications
3. **Review Configuration**: Check all settings
4. **Generate**: Click "Generate" button
5. **Monitor Progress**: Watch the progress bar and status messages
6. **Completion**: Files are created in SolidWorks automatically

### 2. SolidWorks Connection

#### Connecting to SolidWorks

- **Auto-Connect**: Application attempts to connect on startup (if enabled)
- **Manual Connect**: Click "Connect to SolidWorks" button
- **Connection Status**: Indicator in the toolbar shows connection state

#### Connection Modes

- **Attach to Running Instance**: Connects to already open SolidWorks
- **Start New Instance**: Launches SolidWorks if not running
- **Keep Visible**: Keeps SolidWorks window visible during operations

### 3. Document Operations

#### Opening Documents
```
File → Open Document
- Browse to file location
- Select document type (Part, Assembly, Drawing)
- Click Open
```

#### Saving Documents
```
File → Save
- Saves active document
- Use Save As for new location
```

#### Batch Operations
```
Tools → Batch Operations
- Select multiple files
- Choose operation (Open, Save, Export, Close)
- Execute batch
```

### 4. Property Management

#### Setting Custom Properties

1. **Open** a document
2. **Navigate** to Properties panel
3. **Enter** property name and value
4. **Choose** scope (File-level or Configuration-specific)
5. **Click** "Set Property"

#### Batch Property Updates

1. **Create** a properties file (JSON or CSV)
2. **Load** the file in Batch Properties tool
3. **Select** target documents
4. **Apply** properties to all documents

### 5. Export Functionality

#### Supported Export Formats

- **PDF**: For drawings and documents
- **DXF/DWG**: For 2D geometry
- **STEP/IGES**: For 3D models
- **STL**: For 3D printing
- **Parasolid**: For advanced CAD exchange

#### Exporting Documents

1. **Open** document to export
2. **Select** File → Export
3. **Choose** format
4. **Configure** export options
5. **Select** output location
6. **Click** Export

---

## Workflow Management

### What are Workflows?

Workflows are sequences of automated steps that perform complex operations on SolidWorks documents. They can:
- Open and close documents
- Modify properties
- Edit features
- Create drawings
- Export files
- And much more!

### Creating a Workflow

#### Step 1: Create New Workflow
```
Workflows → New Workflow
- Enter workflow name
- Add description
- Choose execution mode (Sequential, Parallel, Conditional)
```

#### Step 2: Add Steps
```
Click "Add Step"
- Select step type (Open Document, Set Property, Export, etc.)
- Configure step parameters
- Set timeout and retry options
- Enable/disable step
```

#### Step 3: Configure Steps

Each step has:
- **Name**: Descriptive name for the step
- **Action**: What the step does
- **Parameters**: Input values (file paths, property names, etc.)
- **Timeout**: Maximum execution time
- **Retry Count**: Number of retry attempts on failure
- **Continue on Error**: Whether to continue if step fails

#### Step 4: Save Workflow
```
File → Save Workflow
- Workflow is saved as JSON file
- Can be edited, shared, and reused
```

### Running a Workflow

1. **Open** Workflows panel
2. **Select** workflow from list
3. **Click** "Run Workflow"
4. **Monitor** progress in real-time
5. **View** results and logs

### Workflow Templates

#### Built-in Templates

The application includes pre-built templates:

**Simple Document Processing**
- Opens a document
- Sets custom properties
- Saves the document
- Closes the document

**Batch Export to PDF**
- Opens documents
- Exports each to PDF
- Closes documents

#### Creating Custom Templates

1. Create a workflow
2. Test thoroughly
3. Save as template: `Workflow → Save as Template`
4. Template appears in Templates list for reuse

### Workflow Best Practices

✅ **DO**:
- Use descriptive names for workflows and steps
- Set appropriate timeouts
- Enable logging for debugging
- Test workflows on sample data first
- Save workflows regularly

❌ **DON'T**:
- Create extremely long workflows (break into smaller ones)
- Skip error handling
- Use hardcoded paths (use parameters instead)
- Run untested workflows on production files

---

## Settings & Configuration

### Accessing Settings
```
Tools → Settings
or
Press Ctrl+, (Ctrl+Comma)
```

### Settings Categories

#### 1. Paths Settings

Configure folder locations:
- **Templates Path**: Location of SolidWorks templates
- **Workflows Path**: Where workflows are saved
- **Output Path**: Default output location for generated files
- **Logs Path**: Application log files location
- **Backup Path**: Backup files location

#### 2. SolidWorks Settings

Control SolidWorks behavior:
- **Auto Start**: Start SolidWorks automatically
- **Keep Visible**: Keep SolidWorks window visible
- **Auto Save**: Save documents after processing
- **Close After Processing**: Close documents when done
- **Document Timeout**: Maximum time to wait for operations
- **Rebuild Timeout**: Maximum time for rebuilds

#### 3. Workflow Settings

Configure workflow execution:
- **Enable Parallel Execution**: Run steps in parallel when possible
- **Max Parallel Workflows**: Number of concurrent workflows
- **Enable Auto Retry**: Retry failed steps automatically
- **Default Retry Count**: Number of retry attempts
- **Save Workflow History**: Keep execution history

#### 4. Analytics Settings

Control tracking and reporting:
- **Enable Analytics**: Track workflow performance
- **Track Error Rates**: Monitor failure rates
- **Generate Reports**: Create performance reports
- **Report Retention**: How long to keep reports

#### 5. UI Settings

Customize the interface:
- **Theme**: Light or Dark mode
- **Window Size**: Default window dimensions
- **Remember Position**: Restore window position on startup
- **Show Tooltips**: Display helpful tooltips
- **Font Size**: Adjust text size

### Settings Profiles

#### Creating a Profile

1. **Configure** settings as desired
2. **Click** "Save as Profile"
3. **Enter** profile name and description
4. **Save**

#### Switching Profiles

1. **Open** Settings
2. **Click** "Profiles" dropdown
3. **Select** profile to load
4. **Apply**

#### Importing/Exporting Profiles

**Export**:
```
Settings → Profiles → Export
- Choose profile
- Select location
- Save as .json file
```

**Import**:
```
Settings → Profiles → Import
- Browse to .json file
- Load profile
- Apply settings
```

---

## Troubleshooting

### Common Issues

#### Issue: Cannot Connect to SolidWorks

**Symptoms**: 
- Connection button stays disabled
- Error message: "Failed to connect to SolidWorks"

**Solutions**:
1. Ensure SolidWorks is installed (2020 or higher)
2. Close all SolidWorks instances
3. Run application as Administrator
4. Check SolidWorks license is valid
5. Restart both SolidWorks and the application

#### Issue: Workflow Fails to Execute

**Symptoms**:
- Workflow starts but stops immediately
- Error in workflow log

**Solutions**:
1. Check workflow validation (red X indicates errors)
2. Verify all file paths exist
3. Ensure SolidWorks is connected
4. Check step timeouts aren't too short
5. Review log files for specific errors

#### Issue: Properties Not Updating

**Symptoms**:
- Set property operation completes but property unchanged

**Solutions**:
1. Verify document is open
2. Check property name is correct (case-sensitive)
3. Ensure configuration name is correct
4. Try rebuilding the document
5. Save and reopen the document

#### Issue: Slow Performance

**Symptoms**:
- Operations take longer than expected
- UI becomes unresponsive

**Solutions**:
1. Close unnecessary SolidWorks documents
2. Reduce parallel workflow count
3. Increase timeout values
4. Disable automatic saving (do manual saves)
5. Check system resources (RAM, CPU)
6. Close other applications

### Log Files

#### Viewing Logs

```
Help → View Logs
or
Navigate to: %AppData%\SolidWorksAutomation\Logs
```

#### Log Levels

- **INFO**: Normal operations
- **WARNING**: Non-critical issues
- **ERROR**: Operation failures
- **CRITICAL**: Application-level errors

#### Interpreting Logs

Look for:
- Timestamps of errors
- Step names that failed
- Error messages
- Stack traces for debugging

---

## FAQ

### General Questions

**Q: Do I need a SolidWorks license?**
A: Yes, a valid SolidWorks license is required as the application uses the SolidWorks API.

**Q: Can I run this on Mac or Linux?**
A: No, the application requires Windows and SolidWorks (Windows-only).

**Q: Is my data safe?**
A: Yes, the application only modifies files you explicitly tell it to. Always backup important files first.

**Q: Can I automate drawings?**
A: Yes, the application supports drawing creation, view insertion, and dimension placement.

### Workflows

**Q: How many steps can a workflow have?**
A: No hard limit, but we recommend keeping workflows under 50 steps for maintainability.

**Q: Can workflows call other workflows?**
A: Not currently, but this is a planned feature.

**Q: Can I share workflows with colleagues?**
A: Yes! Export workflows as JSON and share the files.

### Performance

**Q: How many files can I process at once?**
A: Depends on system resources. Start with batches of 10-20 files and adjust based on performance.

**Q: Will the application slow down SolidWorks?**
A: Minimal impact. The application uses standard SolidWorks API calls like manual operations.

### Technical

**Q: What .NET version is required?**
A: .NET Framework 4.8 or higher.

**Q: Can I extend the application with custom code?**
A: Yes! The application is extensible. Contact support for developer documentation.

**Q: Does it support SolidWorks PDM?**
A: Basic support. Advanced PDM integration is planned for future releases.

---

## Getting Help

### Support Resources

- **Documentation**: Included in installation
- **Email Support**: support@example.com
- **GitHub Issues**: https://github.com/swiffc/Solidworks-Automation/issues
- **Community Forum**: Coming soon

### Reporting Bugs

When reporting bugs, include:
1. Application version (Help → About)
2. SolidWorks version
3. Steps to reproduce
4. Error messages
5. Log files (if applicable)

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| Ctrl+N | New Workflow |
| Ctrl+O | Open Workflow |
| Ctrl+S | Save Workflow |
| Ctrl+, | Open Settings |
| F5 | Refresh/Reload |
| F12 | Open Developer Console |
| Ctrl+Q | Quit Application |

---

## Version History

**Version 1.0.0** (Current)
- Initial release
- Core workflow engine
- Component generation (Bundle, Header, etc.)
- Settings management
- Basic analytics

---

**Last Updated**: October 29, 2025
**Document Version**: 1.0.0
