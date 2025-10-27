# OLD UI INTEGRATION REFERENCE

**Purpose**: Document how old UIs work to properly integrate them into UnifiedUI

---

## ?? Architecture Pattern

All old components follow this pattern:

1. **Static Data Class** - Stores all user inputs as static properties
2. **Main Class** - Implements generation logic, uses FileTools
3. **UI Form** - Sets static properties, calls main class

---

## ?? Component Details

### 1. Hood

**Files**:
- `Hood/HoodData.cs` - Static properties
- `Hood/Hood.cs` - Main generation class
- `Hood/HoodUI.cs` - WinForms UI

**How It Works**:
```csharp
// 1. UI sets static properties
HoodData.Project = "S25140";
HoodData.Bank = 'A';
HoodData.Length = 117;
HoodData.Width = 117;
HoodData.Height = 36;
HoodData.FanDiameter = 9 * 12; // Convert feet to inches
HoodData.Stacks = 1;
HoodData.WindLoad = 50;
HoodData.Initials = "DC";
HoodData.Customer = "Customer Name";
HoodData.Client = "Client Name";
// ... etc

// 2. Create Hood (constructor does all the work)
new Hood();
```

**Key Properties** (from HoodData.cs):
- `char Bank` - Bank letter (A, B, C, etc.)
- `string Project` - Job number
- `double Length` - Hood length
- `double Width` - Hood width  
- `double Height` - Hood height
- `double fanDiameterInFeet` - Fan diameter in feet
- `int Stacks` - Number of stacks
- `double WindLoad` - Wind load (30-100)
- `double Shift` - Shift stiffeners
- `double Adjust` - Adjustment value
- `int Ring.Depth` - Depth (24, 36, 42, 48)

**Constructor**:
```csharp
public Hood()
{
    var fTools = new FileTools.FileTools("Hood", Project, Bank, "3", Initials);
    // Creates JOBNO-3A.SLDASM
    // Generates all hood parts: 157, 182, 187, 188, 189, 194
    // Creates drawing: JOBNO-3.SLDDRW
}
```

---

### 2. Walkway

**Files**:
- `Walkway/Walkway.cs` - Static properties + methods
- `Walkway/WalkwayUI.cs` - WinForms UI

**How It Works**:
```csharp
// 1. Set static properties
Walkway.Project = "S25140";
Walkway.Bank = 'A';
Walkway.Width = 30;
Walkway.RailHeight = 42;
Walkway.FloorHeight = 1.25;
Walkway.MinStringerSize = 6; // C6
Walkway.OffsetFromColumnCenter = 24;
Walkway.AcheColumnSize = "W8x31";
Walkway.AcheColumnCenterToCenterWidth = 188.125;
Walkway.EndToSupportCenter = 5.9375;
Walkway.Length = 200;

// 2. Call static creation method
Walkway.Create_Standard_EndWalkway(
    Walkway.Bank,
    Walkway.Width,
    Walkway.RailHeight,
    Walkway.FloorHeight,
    Walkway.MinStringerSize,
    Walkway.OffsetFromColumnCenter,
    Walkway.AcheColumnSize,
    Walkway.AcheColumnCenterToCenterWidth,
    Walkway.EndToSupportCenter
);

// OR create individual components:
new WalkwayPlatform(bank, length, width, floorHeight, minStringerSize, endToSupport, true);
new HandRail(bank, length, railHeight, floorHeight, minStringerSize, true);
new Support(bank, minStringerSize, width, offset, columnSize, length, true);
```

**Key Properties**:
- `char Bank`
- `string Project`
- `double Width` - Walkway width
- `double RailHeight` - Handrail height (typically 42")
- `double FloorHeight` - Floor thickness
- `int MinStringerSize` - Minimum stringer (0=C6, 1=C8, 2=C10)
- `double OffsetFromColumnCenter`
- `string AcheColumnSize` - Column size (W6x15, W8x31, etc.)
- `double Length` - Platform length

**Generates**:
- JOBNO-28A.SLDASM (walkway assembly)
- JOBNO-28A.SLDDRW (walkway drawing)
- Multiple sub-components

---

### 3. MachineryMount

**Files**:
- `MachineryMount/MachineryMount.cs`
- `MachineryMount/MachineryMountUI.cs`

**Pattern**: Similar to Hood - static properties + constructor

**Generates**:
- JOBNO-4.SLDASM (machinery mount assembly)
- Multiple mounting bracket parts

---

### 4. Plenum

**Files**:
- `Plenum/Plenum.cs`
- `Plenum/PlenumUI.cs`

**How It Works**:
```csharp
// Uses CommonData pattern
FileTools.CommonData.CommonData.JobNumber = jobNumber;
// ... set properties

// Instantiate with Design enum
new Plenum.Plenum().InitializePlenum(Design.CallerType);
```

**Uses**: `FileTools.CommonData.CommonData` static class for properties

**Generates**:
- JOBNO-5.SLDASM (plenum assembly)
- Floor, walls, stiffeners, Johnson beams

---

### 5. Structure

**Files**:
- `Structure/Structure.cs`
- `Structure/StructureUI.cs`

**Pattern**: Similar to Plenum - uses CommonData

**Generates**:
- JOBNO-25.SLDASM (structure assembly)

---

## ?? Integration Pattern for UnifiedUI

### Step 1: Map Configuration to Static Properties

```csharp
private void GenerateHood(HoodConfiguration config, Action<int> progressCallback)
{
    try
    {
        // Map UnifiedUI config to HoodData static properties
        Hood.HoodData.Project = config.JobNumber;
        Hood.HoodData.Bank = (char)(config.Bank + 'A' - 1); // Convert 1->'A', 2->'B'
        Hood.HoodData.Length = config.Length;
        Hood.HoodData.Width = config.Width;
        Hood.HoodData.Height = config.Height;
        Hood.HoodData.fanDiameterInFeet = config.FanDiameter;
        Hood.HoodData.Stacks = config.Stacks;
        Hood.HoodData.WindLoad = config.WindLoad;
        Hood.HoodData.Ring.Depth = config.DepthOption;
        Hood.HoodData.Shift = config.ShiftStiffeners;
        Hood.HoodData.Adjust = config.Adjust;
        Hood.HoodData.Initials = config.Initials ?? "DC";
        Hood.HoodData.Customer = config.Customer ?? "";
        Hood.HoodData.Client = config.Client ?? "";
        Hood.HoodData.Location = config.Location ?? "";
        Hood.HoodData.PurchaseOrder = config.PurchaseOrder ?? "";
        Hood.HoodData.ItemNumber = config.ItemNumber ?? "";
        
        progressCallback?.Invoke(50);
        
        // Call Hood constructor - does all the work
        new Hood.Hood();
        
        progressCallback?.Invoke(100);
        
        System.Windows.MessageBox.Show(
            $"? Hood Generated Successfully!\n\n" +
            $"Job: {config.JobNumber}\n" +
            $"Assembly: {config.JobNumber}-3{(char)(config.Bank + 'A' - 1)}.SLDASM\n" +
            $"Size: {config.Length}\" x {config.Width}\" x {config.Height}\"\n" +
            $"All files created in SolidWorks!",
            "Success",
            System.Windows.MessageBoxButton.OK,
            System.Windows.MessageBoxImage.Information);
    }
    catch (Exception ex)
    {
        GlobalErrorHandler.LogError(ex, "Hood Generation Failed");
        throw;
    }
}
```

### Step 2: Walkway Integration

```csharp
private void GenerateWalkway(WalkwayConfiguration config, Action<int> progressCallback)
{
    try
    {
        // Map to static properties
        Walkway.Walkway.Project = config.JobNumber;
        Walkway.Walkway.Bank = (char)(config.Bank + 'A' - 1);
        Walkway.Walkway.Width = config.Width;
        Walkway.Walkway.RailHeight = config.RailHeight;
        Walkway.Walkway.FloorHeight = config.FloorHeight;
        Walkway.Walkway.MinStringerSize = ParseStringerSize(config.MinimumStringerSize);
        Walkway.Walkway.OffsetFromColumnCenter = config.OffsetFromColumnCenter;
        Walkway.Walkway.AcheColumnSize = config.ColumnSize;
        Walkway.Walkway.AcheColumnCenterToCenterWidth = config.PlenumCenterWidth;
        Walkway.Walkway.EndToSupportCenter = config.SupportCenterToEnd;
        Walkway.Walkway.Initials = config.Initials ?? "DC";
        // ... more properties
        
        progressCallback?.Invoke(50);
        
        // Call static method
        Walkway.Walkway.Create_Standard_EndWalkway(
            Walkway.Walkway.Bank,
            Walkway.Walkway.Width,
            Walkway.Walkway.RailHeight,
            Walkway.Walkway.FloorHeight,
            Walkway.Walkway.MinStringerSize,
            Walkway.Walkway.OffsetFromColumnCenter,
            Walkway.Walkway.AcheColumnSize,
            Walkway.Walkway.AcheColumnCenterToCenterWidth,
            Walkway.Walkway.EndToSupportCenter
        );
        
        progressCallback?.Invoke(100);
        
        System.Windows.MessageBox.Show(
            $"? Walkway Generated Successfully!\n\n" +
            $"Job: {config.JobNumber}\n" +
            $"Assembly: {config.JobNumber}-28{(char)(config.Bank + 'A' - 1)}.SLDASM",
            "Success",
            System.Windows.MessageBoxButton.OK,
            System.Windows.MessageBoxImage.Information);
    }
    catch (Exception ex)
    {
        GlobalErrorHandler.LogError(ex, "Walkway Generation Failed");
        throw;
    }
}

private int ParseStringerSize(string size)
{
    return size switch
    {
        "C6" => 0,
        "C8" => 1,
        "C10" => 2,
        _ => 0
    };
}
```

---

## ?? Property Mapping Tables

### Hood Properties

| UnifiedUI Config | HoodData Property | Type | Default |
|------------------|-------------------|------|---------|
| JobNumber | Project | string | "M000" |
| Bank | Bank | char | 'A' |
| Length | Length | double | 117 |
| Width | Width | double | 117 |
| Height | Height | double | 36 |
| FanDiameter | fanDiameterInFeet | double | 9 |
| Stacks | Stacks | int | 1 |
| WindLoad | WindLoad | double | 30 |
| DepthOption | Ring.Depth | int | 24 |
| ShiftStiffeners | Shift | double | 0 |
| Adjust | Adjust | double | 0 |
| Initials | Initials | string | "AM" |
| Customer | Customer | string | "" |
| Client | Client | string | "" |
| Location | Location | string | "" |
| PurchaseOrder | PurchaseOrder | string | "" |
| ItemNumber | ItemNumber | string | "" |

### Walkway Properties

| UnifiedUI Config | Walkway Property | Type | Default |
|------------------|------------------|------|---------|
| JobNumber | Project | string | "N001" |
| Bank | Bank | char | 'A' |
| Width | Width | double | 30 |
| RailHeight | RailHeight | double | 42 |
| FloorHeight | FloorHeight | double | 1.25 |
| MinimumStringerSize | MinStringerSize | int | 0 (C6) |
| OffsetFromColumnCenter | OffsetFromColumnCenter | double | 24 |
| ColumnSize | AcheColumnSize | string | "W6x15" |
| PlenumCenterWidth | AcheColumnCenterToCenterWidth | double | 188.125 |
| SupportCenterToEnd | EndToSupportCenter | double | 5.9375 |
| PlatformLength | Length | double | 200 |

---

## ? Implementation Checklist

For each component (Hood, Walkway, MachineryMount, Plenum, Structure):

- [x] **Step 1**: Add project reference to UnifiedUI.csproj
- [x] **Step 2**: Create Configuration class with all properties
- [ ] **Step 3**: Implement `Generate{Component}()` method in SolidWorksService.cs
- [ ] **Step 4**: Map Configuration properties to static Data class
- [ ] **Step 5**: Call appropriate constructor or static method
- [ ] **Step 6**: Add error handling and progress callbacks
- [ ] **Step 7**: Test with SolidWorks
- [ ] **Step 8**: Add data binding to UI panel (optional - can use basic TextBoxes initially)

---

## ?? Priority Order

1. **Hood** - Simplest (constructor-based)
2. **Walkway** - Medium (static methods)
3. **MachineryMount** - Similar to Hood
4. **Plenum** - Uses CommonData (different pattern)
5. **Structure** - Uses CommonData

---

**Status**: Reference document complete  
**Next**: Implement backend generation methods using these patterns

