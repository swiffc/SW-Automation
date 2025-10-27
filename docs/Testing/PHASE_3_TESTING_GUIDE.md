# ?? **PHASE 3 TESTING GUIDE**

**Date:** October 27, 2025  
**Status:** ? **READY TO TEST**  
**Build Status:** ? **NO ERRORS**

---

## ? **PRE-TEST CHECKLIST**

- ? **XAML compiles** - No errors
- ? **ViewModel compiles** - No errors  
- ? **All 62 bindings added** - Complete
- ? **Naming verified** - All property names match
- ? **Binding modes correct** - TwoWay/OneWay as appropriate

---

## ?? **QUICK START TESTING**

### **Step 1: Build the Project**
```powershell
# Navigate to project directory
cd C:\Users\DCornealius\CascadeProjects\Solidworks_Automation\macros\csharp\Solidworks-Automation

# Build UnifiedUI
dotnet build UnifiedUI\UnifiedUI.csproj --configuration Debug
```

**Expected:** ? Build succeeds with 0 errors

---

### **Step 2: Run the Application**
```powershell
# Run from bin directory
.\UnifiedUI\bin\Debug\UnifiedUI.exe
```

**Expected:** Application launches, Bundle tab visible

---

## ?? **TEST CASES**

### **TEST 1: Two-Way Binding - Simple TextBox** ?? 30 seconds

**Steps:**
1. Navigate to Bundle tab
2. Find "Bundle Width" field (should show `48.500`)
3. Change value to `50.000`
4. Press Tab or click another field

**Expected Results:**
- ? Value changes in TextBox
- ? ViewModel property `BundleWidth` updates to 50.0
- ? No errors in console
- ? Calculated properties update (if they depend on Bundle Width)

**How to Verify:**
- Add breakpoint in `MainViewModel.cs` at `BundleWidth` setter
- Watch window: `this.BundleWidth` should show 50.0

---

### **TEST 2: Calculated Property Updates** ?? 1 minute

**Steps:**
1. Set `TubeRow1Left` = 10
2. Set `TubeRow2Left` = 9  
3. Observe "Total Tubes" in Calculated Summary section

**Expected Results:**
- ? txtCalcTubes displays `38` (formula: (10 + 9) × 2 = 38)
- ? Updates happen automatically (no button click needed)
- ? Read-only display (cannot edit txtCalcTubes directly)

**Formula Check:**
```csharp
CalculatedTubes = (TubeRow1Left + TubeRow2Left) * 2
// Expected: (10 + 9) * 2 = 38
```

---

### **TEST 3: ComboBox Binding (Double Value)** ?? 30 seconds

**Steps:**
1. Find "Side Frame THK" ComboBox
2. Select "0.500" from dropdown
3. Check ViewModel value

**Expected Results:**
- ? ComboBox shows "0.500" selected
- ? ViewModel `SideFrameThickness` = 0.5 (double)
- ? No conversion errors
- ? `UpdateBundleConfiguration()` called automatically

**How to Verify:**
- Add breakpoint in `SideFrameThickness` setter
- Watch: `this._sideFrameThickness` should be 0.5 (not string "0.500")

---

### **TEST 4: ComboBox Binding (String Value)** ?? 30 seconds

**Steps:**
1. Find "Titleblock" ComboBox
2. Select "Smithco"
3. Check ViewModel value

**Expected Results:**
- ? ComboBox shows "Smithco" selected
- ? ViewModel `Titleblock` = "Smithco" (string)
- ? No errors

---

### **TEST 5: CheckBox Binding** ?? 30 seconds

**Steps:**
1. Find "Headers Outside Frame" CheckBox
2. Check the box
3. Uncheck the box
4. Observe ViewModel

**Expected Results:**
- ? Checked: ViewModel `HeadersOutsideFrame` = true
- ? Unchecked: ViewModel `HeadersOutsideFrame` = false
- ? Immediate updates (TwoWay binding)

---

### **TEST 6: Read-Only Protection** ?? 15 seconds

**Steps:**
1. Find "Total Tubes" in Calculated Summary
2. Try to click and edit the value

**Expected Results:**
- ? Cannot edit (TextBlock, not TextBox)
- ? Value is display-only
- ? Updates automatically when TubeRow1Left/TubeRow2Left change

---

### **TEST 7: Vertical Pitches (18 Fields)** ?? 2 minutes

**Steps:**
1. Expand "Vertical Pitches (Advanced)" section
2. Change `txtFrontPitch12` to 2.000
3. Change `txtRearPitch12` to 2.500
4. Observe Calculated Height updates

**Expected Results:**
- ? Both TextBoxes accept values
- ? ViewModel properties update
- ? `CalculateTotals()` called
- ? CalculatedHeight changes

**Formula Check:**
```csharp
double totalFrontPitches = FrontPitch12 + FrontPitch23 + ... + FrontPitch910;
double totalRearPitches = RearPitch12 + RearPitch23 + ... + RearPitch910;
double avgPitches = (totalFrontPitches + totalRearPitches) / 2.0;
CalculatedHeight = avgPitches + (TubeOD * 2) + 2.0;
```

---

### **TEST 8: UpdateSourceTrigger=PropertyChanged** ?? 30 seconds

**Steps:**
1. Type in any TextBox (e.g., Bundle Width)
2. Don't press Tab or Enter
3. Just type and watch

**Expected Results:**
- ? ViewModel updates **IMMEDIATELY** after each keystroke
- ? No need to Tab out or click elsewhere
- ? Real-time synchronization

**Why:** All bindings use `UpdateSourceTrigger=PropertyChanged`

---

### **TEST 9: All Sections Expand/Collapse** ?? 1 minute

**Steps:**
1. Expand all Expanders:
   - Job Information ?
   - Bundle Dimensions ?
   - Tube Configuration ?
   - Tube Supports ?
   - Vertical Pitches ?
   - Structure & Plenum ?
   - Advanced Options ?
2. Scroll through entire form
3. Collapse all sections

**Expected Results:**
- ? No layout errors
- ? All controls visible when expanded
- ? Smooth scrolling
- ? No overlapping content

---

### **TEST 10: Default Values Match** ?? 2 minutes

**Steps:**
1. Fresh application start
2. Navigate to Bundle tab
3. Verify default values

**Expected Values:**
| Field | Expected Value | Location |
|-------|----------------|----------|
| GlobalJobNumber | "S2____" | ViewModel default |
| BundleWidth | 48.500 | Job Info section |
| SideFrameThickness | 0.375 | Bundle Dimensions |
| SideFrameDepth | 4.000 | Bundle Dimensions |
| TubeLength | 96.000 | Tube Configuration |
| TubeOD | 1.000 | Tube Configuration |
| TubeRow1Left | 8 | Tube Layout |
| TubeRow2Left | 7 | Tube Layout |
| CalculatedTubes | 30 | Calculated Summary (8+7)*2 |
| FanCount | 2 | Structure section |
| PlenumLength | 120.000 | Structure section |
| EnableCamber | false (unchecked) | Advanced Options |
| SaveFiles | true (checked) | Advanced Options |

---

## ?? **COMMON ISSUES & SOLUTIONS**

### **Issue 1: Binding Not Working**
**Symptom:** TextBox shows blank or doesn't update

**Debug Steps:**
1. Check Output window for binding errors
2. Verify property name matches exactly (case-sensitive)
3. Ensure `DataContext` is set in MainWindow
4. Check `RelativeSource` path is correct

**Solution:**
```xaml
<!-- CORRECT -->
Text="{Binding DataContext.GlobalJobNumber, 
       RelativeSource={RelativeSource AncestorType=Window}, 
     Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}"

<!-- WRONG - Missing DataContext -->
Text="{Binding GlobalJobNumber}"
```

---

### **Issue 2: ComboBox Shows Blank**
**Symptom:** ComboBox empty or shows wrong value

**Debug Steps:**
1. Check if using `SelectedItem` vs `Text` binding
2. Verify ComboBoxItem Content matches ViewModel value
3. For double values, ensure Text binding (not SelectedItem)

**Solution:**
```xaml
<!-- For String ComboBox (Titleblock) -->
<ComboBox SelectedItem="{Binding Titleblock}">
    <ComboBoxItem Content="Hudson"/>
    <ComboBoxItem Content="Smithco"/>
</ComboBox>

<!-- For Double ComboBox (SideFrameTHK) -->
<ComboBox Text="{Binding SideFrameThickness, UpdateSourceTrigger=PropertyChanged}">
    <ComboBoxItem Content="0.250"/>
    <ComboBoxItem Content="0.375"/>
</ComboBox>
```

---

### **Issue 3: Calculated Properties Not Updating**
**Symptom:** CalculatedTubes stays at 60 even after changing TubeRow1Left

**Debug Steps:**
1. Check if `CalculateTotals()` is called in property setters
2. Verify OneWay binding mode for display
3. Check calculation formula

**Solution:**
```csharp
// In MainViewModel.cs
public int TubeRow1Left
{
    get => _tubeRow1Left;
    set 
    { 
        if (_tubeRow1Left != value) 
   { 
         _tubeRow1Left = value; 
            OnPropertyChanged(); 
CalculateTotals(); // ? MUST BE HERE
     } 
}
}
```

---

### **Issue 4: "Cannot find DataContext" Error**
**Symptom:** Binding errors in Output window

**Solution:**
Ensure MainWindow.xaml.cs sets DataContext:
```csharp
public MainWindow()
{
    InitializeComponent();
    DataContext = new MainViewModel(); // ? REQUIRED
}
```

---

## ?? **TEST RESULTS TEMPLATE**

Copy this and fill in results:

```markdown
## TEST EXECUTION - [Date/Time]

**Tester:** [Your Name]  
**Build:** Debug  
**Platform:** Windows 10/11  

### Test Results Summary
- ? TEST 1: Two-Way Binding - PASS/FAIL - [Notes]
- ? TEST 2: Calculated Properties - PASS/FAIL - [Notes]
- ? TEST 3: ComboBox Double - PASS/FAIL - [Notes]
- ? TEST 4: ComboBox String - PASS/FAIL - [Notes]
- ? TEST 5: CheckBox - PASS/FAIL - [Notes]
- ? TEST 6: Read-Only - PASS/FAIL - [Notes]
- ? TEST 7: Vertical Pitches - PASS/FAIL - [Notes]
- ? TEST 8: UpdateSourceTrigger - PASS/FAIL - [Notes]
- ? TEST 9: Expand/Collapse - PASS/FAIL - [Notes]
- ? TEST 10: Default Values - PASS/FAIL - [Notes]

**Overall Status:** ? PASS / ? FAIL  
**Issues Found:** [Number]  
**Critical Bugs:** [Number]  

### Detailed Notes
[Add detailed observations here]

### Screenshots
[Attach screenshots if needed]
```

---

## ?? **SUCCESS CRITERIA**

**Phase 3 is SUCCESSFUL if:**
- ? All 10 tests pass
- ? No binding errors in Output window
- ? Calculated properties update in real-time
- ? ComboBoxes work for both string and double values
- ? CheckBoxes toggle ViewModel properties
- ? Read-only fields cannot be edited
- ? Default values load correctly

---

## ?? **NEXT STEPS AFTER TESTING**

### **If All Tests Pass:**
1. ? Mark Phase 3 complete
2. ?? Move to Phase 4: Button Commands
3. ?? Prepare for production deployment

### **If Tests Fail:**
1. ?? Debug and fix issues
2. ?? Re-run failed tests
3. ?? Document fixes in PHASE_3_COMPLETE_SUMMARY.md

---

## ??? **DEBUGGING TOOLS**

### **Visual Studio Output Window:**
```
View ? Output ? Show output from: Debug
```
Look for lines like:
```
System.Windows.Data Error: 40 : BindingExpression path error: 
'PropertyName' property not found on 'object' ''MainViewModel'
```

### **Snoop (WPF Inspector):**
```powershell
# Install Snoop
choco install snoop

# Run while UnifiedUI is running
snoop
```

### **Breakpoints:**
Add breakpoints in:
- Property setters: `MainViewModel.cs` (lines ~220-500)
- `CalculateTotals()`: `MainViewModel.cs` (line ~650)
- `UpdateBundleConfiguration()`: `MainViewModel.cs` (line ~600)

---

**Testing Time Estimate:** 15-20 minutes  
**Expected Result:** ? ALL TESTS PASS  
**Status:** ?? **READY TO START TESTING**

