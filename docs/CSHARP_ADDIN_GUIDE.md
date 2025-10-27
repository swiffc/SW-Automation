# C# Add-in Development Guide for SolidWorks

Guide for creating SolidWorks add-ins using C# and Visual Studio.

---

## Why Create an Add-in?

**Use VBA when:**
- Quick automation tasks
- Simple macros
- Recording and playback

**Use C# Add-in when:**
- Custom UI (ribbons, task panes, property pages)
- Background services and event handlers
- Complex logic and data structures
- Database integration
- Need to distribute to other users
- Better performance and maintainability

---

## Getting Started

### 1. Create New Project in Visual Studio

1. **File ? New ? Project**
2. Select **Class Library (.NET Framework)**
3. Name: `MyCompanySolidWorksAddin`
4. Framework: **.NET Framework 4.8**
5. Platform: **x64** (for 64-bit SolidWorks)

### 2. Install NuGet Packages

Right-click project ? **Manage NuGet Packages**:
- `SolidWorks.Interop.sldworks`
- `SolidWorks.Interop.swconst`
- `SolidWorks.Interop.swpublished`

### 3. Set Project Properties

**Build Tab:**
- Platform target: **x64**
- Uncheck "Prefer 32-bit"

**Debug Tab:**
- Start external program: `C:\Program Files\SOLIDWORKS Corp\SOLIDWORKS\SLDWORKS.exe`

**Build Events (Post-build):**
```cmd
"%windir%\Microsoft.NET\Framework64\v4.0.30319\RegAsm.exe" /codebase "$(TargetPath)"
```

---

## Basic Add-in Structure

### Main Add-in Class

```csharp
using SolidWorks.Interop.sldworks;
using SolidWorks.Interop.swpublished;
using SolidWorks.Interop.swconst;
using System;
using System.Runtime.InteropServices;
using System.Collections;

namespace MyCompanySolidWorksAddin
{
    [Guid("YOUR-GUID-HERE")]  // Generate with Tools ? Create GUID
    [ComVisible(true)]
    [ProgId("MyCompany.SolidWorksAddin")]
    public class SwAddin : ISwAddin
    {
        #region Private Members
        private ISldWorks iSwApp = null;
        private int addinID = 0;
        private BitmapHandler iBmp;
        #endregion

        #region Public Properties
        public ISldWorks SwApp
        {
            get { return iSwApp; }
        }
        #endregion

        #region SolidWorks Registration
        [ComRegisterFunction]
        public static void RegisterFunction(Type t)
        {
            // Get Custom Attribute: SwAddinAttribute
            SwAddinAttribute SWattr = null;
            Type type = typeof(SwAddin);

            foreach (System.Attribute attr in type.GetCustomAttributes(false))
            {
                if (attr is SwAddinAttribute)
                {
                    SWattr = attr as SwAddinAttribute;
                    break;
                }
            }

            try
            {
                Microsoft.Win32.RegistryKey hklm = Microsoft.Win32.Registry.LocalMachine;
                Microsoft.Win32.RegistryKey hkcu = Microsoft.Win32.Registry.CurrentUser;

                string keyname = "SOFTWARE\\SolidWorks\\Addins\\{" + t.GUID.ToString() + "}";
                Microsoft.Win32.RegistryKey addinkey = hklm.CreateSubKey(keyname);
                addinkey.SetValue(null, 0);
                addinkey.SetValue("Description", SWattr.Description);
                addinkey.SetValue("Title", SWattr.Title);

                keyname = "Software\\SolidWorks\\AddInsStartup\\{" + t.GUID.ToString() + "}";
                addinkey = hkcu.CreateSubKey(keyname);
                addinkey.SetValue(null, Convert.ToInt32(SWattr.LoadAtStartup), Microsoft.Win32.RegistryValueKind.DWord);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error registering: " + e.Message);
            }
        }

        [ComUnregisterFunction]
        public static void UnregisterFunction(Type t)
        {
            try
            {
                Microsoft.Win32.RegistryKey hklm = Microsoft.Win32.Registry.LocalMachine;
                Microsoft.Win32.RegistryKey hkcu = Microsoft.Win32.Registry.CurrentUser;

                string keyname = "SOFTWARE\\SolidWorks\\Addins\\{" + t.GUID.ToString() + "}";
                hklm.DeleteSubKey(keyname);

                keyname = "Software\\SolidWorks\\AddInsStartup\\{" + t.GUID.ToString() + "}";
                hkcu.DeleteSubKey(keyname);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error unregistering: " + e.Message);
            }
        }
        #endregion

        #region ISwAddin Implementation
        public bool ConnectToSW(object ThisSW, int Cookie)
        {
            iSwApp = (ISldWorks)ThisSW;
            addinID = Cookie;

            // Setup callbacks
            iSwApp.SetAddinCallbackInfo(0, this, addinID);

            // Setup UI
            SetupUI();

            // Setup event handlers
            AttachEventHandlers();

            return true;
        }

        public bool DisconnectFromSW()
        {
            // Cleanup UI
            RemoveUI();

            // Detach event handlers
            DetachEventHandlers();

            // Release COM objects
            Marshal.ReleaseComObject(iSwApp);
            iSwApp = null;
            GC.Collect();
            GC.WaitForPendingFinalizers();

            GC.Collect();
            GC.WaitForPendingFinalizers();

            return true;
        }
        #endregion

        #region UI Methods
        private void SetupUI()
        {
            // Create command group
            // Add menu items
            // Add toolbar buttons
        }

        private void RemoveUI()
        {
            // Remove command group
            // Remove menu items
            // Remove toolbar buttons
        }
        #endregion

        #region Event Handlers
        private void AttachEventHandlers()
        {
            // Attach to SolidWorks events
        }

        private void DetachEventHandlers()
        {
            // Detach from SolidWorks events
        }
        #endregion

        #region Event Handler Methods
        // Implement event handler methods here
        #endregion
    }

    // Add-in attribute
    [AttributeUsage(AttributeTargets.Class)]
    public class SwAddinAttribute : System.Attribute
    {
        public string Description { get; set; }
        public string Title { get; set; }
        public bool LoadAtStartup { get; set; }

        public SwAddinAttribute()
        {
            LoadAtStartup = true;
        }
    }
}
```

### Add Attribute to Class

```csharp
[SwAddin(
    Description = "My awesome SolidWorks add-in",
    Title = "My Company Add-in",
    LoadAtStartup = true
)]
public class SwAddin : ISwAddin
{
    // ... class implementation
}
```

---

## Adding Commands and UI

### Create Command Group

```csharp
private void SetupUI()
{
    ICommandGroup cmdGroup = iSwApp.CreateCommandGroup2(
        1,                      // User-defined command group ID
        "My Commands",          // Title
        "My command group",     // Tooltip
        "Command group hint",   // Hint string
        -1,                     // Position
        true,                   // ignorePrevious
        ref cmdGroupErr
    );

    // Add commands
    int cmdIndex0 = cmdGroup.AddCommandItem2(
        "Command 1",            // Name
        -1,                     // Position
        "My first command",     // Hint string
        "Command 1 tooltip",    // Tooltip
        0,                      // Image list index
        "OnCommand1",           // Callback function
        "EnableCommand1",       // Enable function
        1,                      // User ID
        (int)swCommandItemType_e.swMenuItem | (int)swCommandItemType_e.swToolbarItem
    );

    cmdGroup.HasToolbar = true;
    cmdGroup.HasMenu = true;
    cmdGroup.Activate();
}
```

### Command Callback

```csharp
public void OnCommand1(int cmdID)
{
    ModelDoc2 model = iSwApp.ActiveDoc as ModelDoc2;
    if (model == null)
    {
        iSwApp.SendMsgToUser("Please open a document first.");
        return;
    }

    // Your command logic here
    iSwApp.SendMsgToUser("Command 1 executed!");
}

public int EnableCommand1()
{
    // Return 1 to enable, 0 to disable
    return (iSwApp.ActiveDoc != null) ? 1 : 0;
}
```

---

## Event Handling

### Document Events

```csharp
private void AttachEventHandlers()
{
    // Attach to app-level events
    iSwApp.FileNewNotify2 += OnFileNew;
    iSwApp.ActiveDocChangeNotify += OnActiveDocChange;
    iSwApp.FileOpenPostNotify += OnFileOpen;
    
    // Attach to active document events
    AttachModelEventHandler();
}

private int OnFileNew(object newDoc, int docType, string templateName)
{
    ModelDoc2 model = newDoc as ModelDoc2;
    // Handle new file event
    return 0;
}

private int OnActiveDocChange()
{
    // Handle active document change
    AttachModelEventHandler();
    return 0;
}

private int OnFileOpen(string fileName)
{
    // Handle file open event
    return 0;
}
```

### Model-Specific Events

```csharp
private void AttachModelEventHandler()
{
    ModelDoc2 model = iSwApp.ActiveDoc as ModelDoc2;
    if (model == null) return;

    switch (model.GetType())
    {
        case (int)swDocumentTypes_e.swDocPART:
            PartDoc part = model as PartDoc;
            part.DestroyNotify2 += OnPartDestroy;
            part.AddItemNotify += OnPartAddItem;
            break;

        case (int)swDocumentTypes_e.swDocASSEMBLY:
            AssemblyDoc assy = model as AssemblyDoc;
            assy.DestroyNotify2 += OnAssemblyDestroy;
            assy.ComponentStateChangeNotify2 += OnComponentStateChange;
            break;
    }
}

private int OnPartAddItem(int entityType, string itemName)
{
    // Handle new feature added to part
    System.Diagnostics.Debug.WriteLine($"Feature added: {itemName}");
    return 0;
}
```

---

## Creating Task Pane

### Setup Task Pane

```csharp
using SolidWorksTools;
using SolidWorksTools.File;

private TaskpaneView taskpaneView;
private MyUserControl taskpaneControl;

private void CreateTaskPane()
{
    string imagePath = GetImagePath();
    
    taskpaneView = iSwApp.CreateTaskpaneView2(
        imagePath,
        "My Task Pane"
    );

    taskpaneControl = new MyUserControl();
    taskpaneView.AddControl(taskpaneControl);
}
```

### User Control (WinForms)

```csharp
using System.Windows.Forms;

public partial class MyUserControl : UserControl
{
    public MyUserControl()
    {
        InitializeComponent();
    }

    private void btnDoSomething_Click(object sender, EventArgs e)
    {
        // Button click handler
        MessageBox.Show("Button clicked!");
    }
}
```

---

## Property Manager Page

### Create PMPage

```csharp
using SolidWorks.Interop.swpublished;

private PropertyManagerPage2 pmp;

private void CreatePMPage()
{
    int options = (int)swPropertyManagerPageOptions_e.swPropertyManagerOptions_OkayButton |
                  (int)swPropertyManagerPageOptions_e.swPropertyManagerOptions_CancelButton;

    pmp = iSwApp.CreatePropertyManagerPage(
        "My Property Page",
        options,
        this,
        ref errors
    );

    // Add controls
    AddPMPageControls();
}

private void AddPMPageControls()
{
    IPropertyManagerPageGroup group = pmp.AddGroupBox(
        1,
        "Input Parameters",
        (int)swAddGroupBoxOptions_e.swGroupBoxOptions_Visible
    );

    // Add textbox
    IPropertyManagerPageTextbox textbox = group.AddControl(
        2,
        (short)swPropertyManagerPageControlType_e.swControlType_Textbox,
        "Width:",
        (short)swPropertyManagerPageControlLeftAlign_e.swControlAlign_LeftEdge,
        (int)swAddControlOptions_e.swControlOptions_Visible |
        (int)swAddControlOptions_e.swControlOptions_Enabled,
        "Enter width value"
    );
}
```

### PMPage Callbacks

```csharp
// Implement IPropertyManagerPage2Handler9
public void OnClose(int reason)
{
    // OK button = swPropertyManagerPageClose_Okay
    // Cancel = swPropertyManagerPageClose_Cancel
}

public void OnCheckboxCheck(int id, bool @checked)
{
    // Checkbox state changed
}

public void OnTextboxChanged(int id, string text)
{
    // Textbox value changed
}
```

---

## Best Practices

### 1. COM Object Management
```csharp
// Always release COM objects
if (comObject != null)
{
    Marshal.ReleaseComObject(comObject);
    comObject = null;
}
```

### 2. Error Handling
```csharp
try
{
    // Your code
}
catch (COMException ex)
{
    iSwApp.SendMsgToUser($"Error: {ex.Message}");
    System.Diagnostics.Debug.WriteLine(ex.StackTrace);
}
```

### 3. Thread Safety
```csharp
// Use Invoke for UI updates from background threads
if (control.InvokeRequired)
{
    control.Invoke(new Action(() => {
        control.Text = "Updated";
    }));
}
```

### 4. Configuration-Specific Data
```csharp
// Get configuration-specific custom properties
IConfiguration config = model.GetActiveConfiguration();
CustomPropertyManager cpm = config.CustomPropertyManager;
```

---

## Debugging

### Attach to Process
1. Start SolidWorks
2. In Visual Studio: **Debug ? Attach to Process**
3. Select **SLDWORKS.exe**
4. Set breakpoints
5. Trigger your add-in code

### Debug Output
```csharp
System.Diagnostics.Debug.WriteLine("Debug message");
```

View in: **View ? Output** window

---

## Distribution

### 1. Build Release Version
- Configuration: **Release**
- Platform: **x64**

### 2. Create Installer
Use tools like:
- WiX Toolset
- InstallShield
- Advanced Installer

### 3. Include Files
- DLL assembly
- Dependencies
- Registry entries
- Icons/images

---

## Resources

- **SolidWorks API Help**: Installed with SolidWorks SDK
- **CodeStack**: https://www.codestack.net/solidworks-api/
- **SolidWorks Forums**: https://forum.solidworks.com/community/api
- **GitHub Examples**: Search "SolidWorks API C#"

---

Happy coding! ??

