# ?? Tool Selector Implementation Plan

**Created**: October 28, 2025  
**Purpose**: Add top-level tool selector to UnifiedUI based on research + actual project structure  
**Based On**: 
- Perplexity research on CAD UI best practices
- Full project scan (4,856 files analyzed)
- Current UnifiedUI MVVM architecture

---

## ?? Current Project Structure (Actual)

### Your 4 Tool Types

| Tool | Excel Configs | CAD Templates | Purpose | Status |
|------|---------------|---------------|---------|--------|
| **Header Section Tool** | 37 | 95 | Main tool, multiple configs | ?? 2 refs |
| **XCH Structure Tool** | 1 (XCH_SCS.xlsx) | 308 | Cross-flow structures | ? 1 ref |
| **Z Structure Tool** | 1 (Lifting System) | 1,198 | Vertical structures (HUGE) | ? 1 ref |
| **Hudson Certified** | 1 (Fan Calculator) | 172 | Separate project | ? 10 refs |

**TOTAL**: 40 Excel configs, 1,773 CAD templates, 2.5GB

---

## ?? Recommended UI Pattern (From Research)

### Pattern: **Two-Level Navigation**

```
????????????????????????????????????????????????????????????????
?  UnifiedUI - SolidWorks Automation                          ?
?  ????????????????????????????????????????????????????????   ?
?  ? Tool Selector:  [Header Section ?]                   ?   ? ? TOP LEVEL
?  ????????????????????????????????????????????????????????   ?
?  ????????????????????????????????????????????????????????   ?
?  ? ????????????????????????????????????????????????    ?   ?
?  ? ? Bundle ? Header ? Hood   ? Struct ? Walkway  ?    ?   ? ? COMPONENT LEVEL
?  ? ????????????????????????????????????????????????    ?   ?
?  ?                                                       ?   ?
?  ?  [Component configuration here]                      ?   ?
?  ????????????????????????????????????????????????????????   ?
????????????????????????????????????????????????????????????????
```

**Why This Pattern:**
- Autodesk Fusion 360 uses workspace selector
- Visual Studio uses solution/project selector
- Siemens NX uses ribbon workspace switching
- Clear hierarchy: Tool ? Component ? Configuration

---

## ??? Implementation Design

### Phase 1: Add Tool Selector (4 hours)

#### 1.1 Create Tool Model

**File**: `UnifiedUI/Models/ToolType.cs`

```csharp
namespace UnifiedUI.Models
{
    /// <summary>
    /// Represents a tool/project type in the application
    /// </summary>
    public class ToolType
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public string IconPath { get; set; }
        public string TemplatePath { get; set; }
        public string ExcelConfigPath { get; set; }
        public List<string> AvailableComponents { get; set; }
        
        // Tool types
        public static ToolType HeaderSectionTool => new ToolType
        {
            Id = "HeaderSection",
            Name = "Header Section Tool",
            Description = "Main tool with 37 Excel configurations",
            TemplatePath = @"templates\header_section_tool",
            ExcelConfigPath = @"templates\header_section_tool",
            AvailableComponents = new List<string> 
            { 
                "Bundle", "Header", "Hood", "Machinery Mount", 
                "Plenum", "Structure", "Walkway" 
            }
        };
        
        public static ToolType XCHStructureTool => new ToolType
        {
            Id = "XCHStructure",
            Name = "XCH Structure Tool",
            Description = "Cross-flow structures (XCH_SCS.xlsx)",
            TemplatePath = @"templates\xch_structure_tool",
            ExcelConfigPath = @"templates\xch_structure_tool\XCH_SCS.xlsx",
            AvailableComponents = new List<string> { "XCH Structure" }
        };
        
        public static ToolType ZStructureTool => new ToolType
        {
            Id = "ZStructure",
            Name = "Z Structure Tool",
            Description = "Vertical structures (1,198 templates)",
            TemplatePath = @"templates\z_structure_tool",
            ExcelConfigPath = @"templates\z_structure_tool\Lifting System Work Sheet.xlsx",
            AvailableComponents = new List<string> { "Z Structure" }
        };
        
        public static ToolType HudsonCertified => new ToolType
        {
            Id = "HudsonCertified",
            Name = "Hudson Certified",
            Description = "Certified drawings (separate project)",
            TemplatePath = @"templates\hudson_certified",
            ExcelConfigPath = @"templates\hudson_certified\Cofimco Fan Calculator.xlsx",
            AvailableComponents = new List<string> { "Certified Drawing" }
        };
        
        public static List<ToolType> AllTools => new List<ToolType>
        {
            HeaderSectionTool,
            XCHStructureTool,
            ZStructureTool,
            HudsonCertified
        };
    }
}
```

---

#### 1.2 Update MainViewModel

**File**: `UnifiedUI/ViewModels/MainViewModel.cs`

**Add properties**:

```csharp
private ToolType _selectedTool;
public ToolType SelectedTool
{
    get => _selectedTool;
    set
    {
        _selectedTool = value;
        OnPropertyChanged(nameof(SelectedTool));
        OnToolChanged();
    }
}

public ObservableCollection<ToolType> AvailableTools { get; set; }
public ObservableCollection<string> CurrentComponents { get; set; }

private void OnToolChanged()
{
    // Update available components based on selected tool
    CurrentComponents.Clear();
    foreach (var component in SelectedTool.AvailableComponents)
    {
        CurrentComponents.Add(component);
    }
    
    // Update template path
    TemplateManager.SetToolPath(SelectedTool.TemplatePath);
    
    // Update Excel config
    ExcelManager.SetConfigPath(SelectedTool.ExcelConfigPath);
    
    // Clear current configuration
    CurrentConfiguration = null;
    
    GlobalErrorHandler.LogInfo($"? Switched to: {SelectedTool.Name}");
    StatusMessage = $"Tool: {SelectedTool.Name}";
}
```

**In constructor**:

```csharp
public MainViewModel()
{
    // Initialize tools
    AvailableTools = new ObservableCollection<ToolType>(ToolType.AllTools);
    CurrentComponents = new ObservableCollection<string>();
    
    // Default to Header Section Tool
    SelectedTool = ToolType.HeaderSectionTool;
    
    // Rest of initialization...
}
```

---

#### 1.3 Update MainWindow.xaml

**Add Tool Selector at top**:

```xml
<DockPanel Background="{StaticResource DarkBackground}">
    <!-- NEW: Tool Selector Bar -->
    <Border DockPanel.Dock="Top" 
            Background="{StaticResource DarkPanel}" 
            BorderBrush="{StaticResource DarkBorder}" 
            BorderThickness="0,0,0,1" 
            Padding="15,10">
        <Grid>
            <Grid.ColumnDefinitions>
                <ColumnDefinition Width="Auto"/>
                <ColumnDefinition Width="*"/>
                <ColumnDefinition Width="Auto"/>
            </Grid.ColumnDefinitions>
            
            <!-- Tool Selector -->
            <StackPanel Grid.Column="0" Orientation="Horizontal">
                <TextBlock Text="Active Tool:" 
                          VerticalAlignment="Center" 
                          Margin="0,0,10,0" 
                          FontWeight="Bold"
                          FontSize="14"
                          Foreground="{StaticResource LightText}"/>
                
                <ComboBox Name="ToolSelector"
                         Width="250"
                         FontSize="14"
                         ItemsSource="{Binding AvailableTools}"
                         SelectedItem="{Binding SelectedTool}"
                         DisplayMemberPath="Name">
                    <ComboBox.ItemTemplate>
                        <DataTemplate>
                            <StackPanel>
                                <TextBlock Text="{Binding Name}" FontWeight="Bold"/>
                                <TextBlock Text="{Binding Description}" 
                                          FontSize="10" 
                                          Foreground="Gray"/>
                            </StackPanel>
                        </DataTemplate>
                    </ComboBox.ItemTemplate>
                </ComboBox>
            </StackPanel>
            
            <!-- Tool Info -->
            <StackPanel Grid.Column="1" 
                       Orientation="Horizontal" 
                       HorizontalAlignment="Center"
                       VerticalAlignment="Center">
                <TextBlock Text="Components: " 
                          Foreground="Gray" 
                          Margin="20,0,5,0"/>
                <TextBlock Text="{Binding SelectedTool.AvailableComponents.Count}" 
                          FontWeight="Bold"
                          Foreground="{StaticResource LightText}"/>
                <TextBlock Text=" | Templates: " 
                          Foreground="Gray" 
                          Margin="10,0,5,0"/>
                <TextBlock Text="{Binding SelectedTool.TemplatePath}" 
                          FontSize="10"
                          Foreground="Gray"/>
            </StackPanel>
            
            <!-- Quick Actions -->
            <StackPanel Grid.Column="2" Orientation="Horizontal">
                <Button Content="Load Excel Config" 
                       Click="LoadExcelConfig_Click"
                       Margin="5,0"/>
                <Button Content="Tool Settings" 
                       Click="ToolSettings_Click"
                       Margin="5,0"/>
            </StackPanel>
        </Grid>
    </Border>

    <!-- Existing Menu Bar -->
    <Menu DockPanel.Dock="Top" ...>
    ...
```

---

#### 1.4 Update Component Tabs Dynamically

**In MainWindow.xaml.cs**:

```csharp
private void InitializeComponentTabs()
{
    // Clear existing tabs
    ComponentTabs.Items.Clear();
    
    // Create tabs ONLY for components available in selected tool
    foreach (var componentType in _viewModel.SelectedTool.AvailableComponents)
    {
        var tabItem = new TabItem
        {
            Header = componentType,
            Content = CreateComponentPanel(componentType)
        };
        ComponentTabs.Items.Add(tabItem);
    }
}

// Call this when tool changes
private void OnToolChanged()
{
    InitializeComponentTabs();
}
```

---

### Phase 2: Tool-Specific Features (8 hours)

#### 2.1 Template Manager Integration

**Update**: `FileTools/TemplateManager.cs`

```csharp
public static class TemplateManager
{
    private static string _currentToolPath;
    
    public static void SetToolPath(string toolPath)
    {
        _currentToolPath = toolPath;
        GlobalErrorHandler.LogInfo($"? Template path: {toolPath}");
    }
    
    public static string GetTemplatePath(string templateName)
    {
        if (string.IsNullOrEmpty(_currentToolPath))
        {
            throw new InvalidOperationException("Tool path not set! Select a tool first.");
        }
        
        var fullPath = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            "CascadeProjects", "Solidworks_Automation",
            _currentToolPath,
            templateName
        );
        
        if (!File.Exists(fullPath))
        {
            throw new FileNotFoundException($"Template not found: {fullPath}");
        }
        
        TrackUsage(_currentToolPath, templateName);
        return fullPath;
    }
}
```

---

#### 2.2 Excel Config Manager

**Create**: `UnifiedUI/Services/ExcelConfigManager.cs`

```csharp
public class ExcelConfigManager
{
    private string _currentConfigPath;
    
    public void SetConfigPath(string configPath)
    {
        _currentConfigPath = configPath;
        GlobalErrorHandler.LogInfo($"? Excel config: {configPath}");
    }
    
    public Dictionary<string, string> LoadToolConfigurations()
    {
        var configs = new Dictionary<string, string>();
        
        // For Header Section Tool: 37 Excel files
        if (_currentConfigPath.Contains("header_section_tool"))
        {
            var dir = new DirectoryInfo(_currentConfigPath);
            foreach (var file in dir.GetFiles("*.xlsx"))
            {
                configs[file.Name] = file.FullName;
            }
        }
        // For other tools: single Excel file
        else if (File.Exists(_currentConfigPath))
        {
            configs[Path.GetFileName(_currentConfigPath)] = _currentConfigPath;
        }
        
        return configs;
    }
}
```

---

#### 2.3 Tool-Specific Workflows

**Handle Hudson Certified differently** (it's a separate project):

```csharp
private void OnToolChanged()
{
    if (SelectedTool.Id == "HudsonCertified")
    {
        // Hudson Certified has different workflow
        ShowHudsonCertifiedWorkflow();
    }
    else
    {
        // Standard component-based workflow
        ShowStandardWorkflow();
    }
}

private void ShowHudsonCertifiedWorkflow()
{
    // Load Hudson-specific UI
    // This could be a different View entirely
    ContentArea.Content = new HudsonCertifiedView();
}

private void ShowStandardWorkflow()
{
    // Standard component tabs
    InitializeComponentTabs();
}
```

---

### Phase 3: Polish & Integration (4 hours)

#### 3.1 Save/Load Tool Selection

```csharp
public class UserPreferences
{
    public string LastUsedTool { get; set; }
    public Dictionary<string, string> ToolSpecificSettings { get; set; }
    
    public static void SavePreferences(UserPreferences prefs)
    {
        var json = JsonConvert.SerializeObject(prefs, Formatting.Indented);
        File.WriteAllText("user_preferences.json", json);
    }
    
    public static UserPreferences LoadPreferences()
    {
        if (File.Exists("user_preferences.json"))
        {
            var json = File.ReadAllText("user_preferences.json");
            return JsonConvert.DeserializeObject<UserPreferences>(json);
        }
        return new UserPreferences { LastUsedTool = "HeaderSection" };
    }
}
```

---

#### 3.2 Tool-Specific Excel Import

```csharp
private void ImportButton_Click(object sender, RoutedEventArgs e)
{
    var configManager = new ExcelConfigManager();
    var configs = configManager.LoadToolConfigurations();
    
    if (configs.Count == 1)
    {
        // Single config (XCH, Z, Hudson) - load directly
        LoadExcelConfig(configs.First().Value);
    }
    else
    {
        // Multiple configs (Header Section) - show picker
        var picker = new ConfigPickerDialog(configs);
        if (picker.ShowDialog() == true)
        {
            LoadExcelConfig(picker.SelectedConfigPath);
        }
    }
}
```

---

#### 3.3 Visual Indicators

```xml
<!-- Show which tool is active with color coding -->
<Border Background="{Binding SelectedTool.AccentColor}">
    <TextBlock Text="{Binding SelectedTool.Name}"/>
</Border>
```

**Add to ToolType**:

```csharp
public Brush AccentColor => Id switch
{
    "HeaderSection" => new SolidColorBrush(Color.FromRgb(52, 152, 219)), // Blue
    "XCHStructure" => new SolidColorBrush(Color.FromRgb(46, 204, 113)),  // Green
    "ZStructure" => new SolidColorBrush(Color.FromRgb(155, 89, 182)),    // Purple
    "HudsonCertified" => new SolidColorBrush(Color.FromRgb(230, 126, 34)), // Orange
    _ => Brushes.Gray
};
```

---

## ?? Implementation Timeline

| Phase | Task | Hours | Priority |
|-------|------|-------|----------|
| **Phase 1** | Add Tool Selector | 4h | HIGH |
| | - Create ToolType model | 1h | |
| | - Update MainViewModel | 1h | |
| | - Update MainWindow UI | 1h | |
| | - Dynamic component tabs | 1h | |
| **Phase 2** | Tool-Specific Features | 8h | MEDIUM |
| | - Template Manager | 2h | |
| | - Excel Config Manager | 2h | |
| | - Hudson Certified workflow | 2h | |
| | - Testing & debugging | 2h | |
| **Phase 3** | Polish & Integration | 4h | LOW |
| | - Save/load preferences | 1h | |
| | - Tool-specific import | 2h | |
| | - Visual indicators | 1h | |
| **TOTAL** | | **16 hours** | |

---

## ?? Expected Benefits

### Before (Current State)
- ? No way to select tool
- ? All components mixed together
- ? Templates not organized by tool
- ? Excel configs scattered
- ? Hudson Certified treated same as others

### After (With Tool Selector)
- ? Clear tool selection
- ? Only relevant components shown
- ? Templates organized by tool
- ? Excel configs per tool
- ? Hudson Certified has unique workflow
- ? 37 Header Section configs accessible
- ? Users can't accidentally mix tools

---

## ?? Integration Points

### Files to Modify

| File | Changes | Lines |
|------|---------|-------|
| `UnifiedUI/Models/ToolType.cs` | NEW | ~100 |
| `UnifiedUI/ViewModels/MainViewModel.cs` | Add tool selection | +50 |
| `UnifiedUI/MainWindow.xaml` | Add tool selector bar | +60 |
| `UnifiedUI/MainWindow.xaml.cs` | Dynamic tabs | +40 |
| `FileTools/TemplateManager.cs` | Tool-aware paths | +30 |
| `UnifiedUI/Services/ExcelConfigManager.cs` | NEW | ~80 |

**Total**: ~360 lines of new/modified code

---

## ?? Quick Start (Week 1)

### Day 1 (4 hours): Phase 1
```powershell
# 1. Create ToolType.cs
# 2. Update MainViewModel.cs
# 3. Update MainWindow.xaml
# 4. Test tool switching
```

**Goal**: Tool selector visible and functional

### Day 2 (4 hours): Phase 2 Part 1
```powershell
# 1. Update TemplateManager.cs
# 2. Create ExcelConfigManager.cs
# 3. Test template loading per tool
```

**Goal**: Templates load from correct tool folder

### Day 3 (4 hours): Phase 2 Part 2
```powershell
# 1. Implement Hudson Certified workflow
# 2. Test all 4 tools
# 3. Debug any issues
```

**Goal**: All 4 tools work independently

### Day 4 (4 hours): Phase 3
```powershell
# 1. Add save/load preferences
# 2. Polish UI
# 3. Add visual indicators
# 4. Final testing
```

**Goal**: Production-ready!

---

## ?? UI Mockup

```
??????????????????????????????????????????????????????????????????????????
?  ????????????????????????????????????????????????????????????????????  ?
?  ?  Active Tool: [Header Section Tool ?]  | 7 components             ?  ?
?  ?               Templates: templates\header_section_tool            ?  ?
?  ?  [Load Excel Config]  [Tool Settings]                            ?  ?
?  ????????????????????????????????????????????????????????????????????  ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ?  File  Edit  Tools  Help                                        ?   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ?  [New] [Open] [Save] [Import Prego] [Generate] [Validate]      ?   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ?  ??????????????????????????????????????????????????????????   ?   ?
?  ?  ?Bundle ? Header ? Hood ? Machinery  ?Plenum  ? Structure?   ?   ?
?  ?  ?       ?        ?      ? Mount      ?        ?          ?   ?   ?
?  ?  ??????????????????????????????????????????????????????????   ?   ?
?  ?  ???????????????????????????????????????????????????????????? ?   ?
?  ?  ?  [Bundle Configuration Panel]                            ? ?   ?
?  ?  ?                                                           ? ?   ?
?  ?  ?  Width: [____]    Depth: [____]                         ? ?   ?
?  ?  ?  ...                                                      ? ?   ?
?  ?  ???????????????????????????????????????????????????????????? ?   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ??????????????????????????????????????????????????????????????????   ?
?  ?  Ready  |  Tool: Header Section  |  Validation: ? OK          ?   ?
?  ??????????????????????????????????????????????????????????????????   ?
??????????????????????????????????????????????????????????????????????????
```

---

## ?? Pro Tips

1. **Start with Header Section Tool** (most complex with 37 configs)
2. **Test tool switching thoroughly** (memory leaks in COM objects?)
3. **Hudson Certified may need separate window** if workflow is very different
4. **Save user's last tool** so they return to where they were
5. **Add tooltips** explaining each tool's purpose

---

## ?? Potential Issues & Solutions

| Issue | Solution |
|-------|----------|
| **Tool switching clears data** | Prompt user to save before switching |
| **Templates not found** | Validate paths on tool switch |
| **Excel configs conflict** | Each tool has isolated config manager |
| **COM object leaks** | Dispose properly on tool switch |
| **Hudson different workflow** | Create separate view/workflow |

---

## ?? References

- Research: `docs/Architecture/TOOL_SELECTOR_RESEARCH.md`
- Current UI: `macros/csharp/Solidworks-Automation/UnifiedUI/`
- Templates: `templates/` (4 folders, 1,773 CAD files)
- Best Practices: Fusion 360, Visual Studio, CATIA patterns

---

**Next Step**: Start with Phase 1 Day 1 (4 hours)  
**Expected Completion**: 4 days (16 hours total)  
**Impact**: Users can finally choose their tool!

---

**Version**: 1.0  
**Last Updated**: October 28, 2025  
**Status**: ? READY TO IMPLEMENT

