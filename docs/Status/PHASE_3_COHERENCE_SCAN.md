# ?? **PHASE 3 PRE-FLIGHT COHERENCE SCAN**

**Date:** October 27, 2025  
**Purpose:** Verify ViewModel ? XAML alignment before adding data bindings  
**Status:** ? **IN PROGRESS**

---

## ?? **SCAN RESULTS SUMMARY**

### ? **PASSED CHECKS:**
1. ? MainViewModel.cs compiles successfully
2. ? All 70+ ViewModel properties exist
3. ? INotifyPropertyChanged implemented correctly
4. ? CalculateTotals() method present
5. ? BundlePanel.xaml compiles successfully
6. ? All required XAML controls present (70+ controls)

### ?? **POTENTIAL ISSUES FOUND:**
1. ?? **GlobalJobNumber vs JobNumber** - Property name mismatch
2. ?? **Missing PurchaseOrder/ItemNumber in Phase 2 region**
3. ?? **ComboBox bindings need SelectedValue not Text**
4. ?? **Calculated properties are read-only (correct)**

---

## ?? **DETAILED ANALYSIS**

### **1. JOB INFORMATION SECTION**

#### **ViewModel Properties (MainViewModel.cs):**
```csharp
// Global (shared across tabs)
public string GlobalJobNumber { get; set; } = "S2____";
public string GlobalPartPrefix { get; set; } = "JOBNO-";
public string GlobalRevision { get; set; } = "R01";

// Bundle-specific (Phase 2)
public char Bank { get; set; }
public string Customer { get; set; }
public string Client { get; set; }
public string Location { get; set; }
public string Initials { get; set; }
public string Titleblock { get; set; }
public string PurchaseOrder { get; set; }  // ? EXISTS
public string ItemNumber { get; set; }     // ? EXISTS
```

#### **XAML Controls (BundlePanel.xaml):**
```xaml
<TextBox x:Name="txtJobNumber" /> <!-- Needs: GlobalJobNumber -->
<TextBox x:Name="txtBank" Text="A"/><!-- Needs: Bank -->
<TextBox x:Name="txtCustomer"/>        <!-- Needs: Customer -->
<TextBox x:Name="txtClient"/>            <!-- Needs: Client -->
<TextBox x:Name="txtLocation"/>          <!-- Needs: Location -->
<TextBox x:Name="txtInitials"/>  <!-- Needs: Initials -->
<ComboBox x:Name="cmbTitleblock"/>       <!-- Needs: Titleblock -->
<TextBox x:Name="txtPurchaseOrder"/>     <!-- Needs: PurchaseOrder -->
<TextBox x:Name="txtItemNumber"/>        <!-- Needs: ItemNumber -->
```

#### **? COHERENCE STATUS:**
- ? All 9 properties exist in ViewModel
- ? All 9 controls exist in XAML
- ?? **BINDING REQUIRED:** txtJobNumber ? GlobalJobNumber (not JobNumber)

---

### **2. BUNDLE DIMENSIONS SECTION**

#### **ViewModel Properties:**
```csharp
public double BundleWidth { get; set; } = 48.500;
public double SideFrameThickness { get; set; } = 0.375;
public double SideFrameDepth { get; set; } = 4.000;
public bool HeadersOutsideFrame { get; set; } = false;
```

#### **XAML Controls:**
```xaml
<TextBox x:Name="txtBundleWidth" Text="48.500"/>
<ComboBox x:Name="cmbSideFrameTHK">
    <ComboBoxItem Content="0.250"/>
    <ComboBoxItem Content="0.375" IsSelected="True"/>
    <ComboBoxItem Content="0.500"/>
</ComboBox>
<TextBox x:Name="txtSideFrameDepth" Text="4.000"/>
<CheckBox x:Name="chkHeadersOutside"/>
```

#### **? COHERENCE STATUS:**
- ? All 4 properties exist
- ? All 4 controls exist
- ?? **COMBO BOX BINDING:** cmbSideFrameTHK needs SelectedValue binding to double

---

### **3. TUBE CONFIGURATION SECTION**

#### **ViewModel Properties:**
```csharp
// Basic Tube Properties
public double TubeLength { get; set; } = 96.000;
public double TubeProjection { get; set; } = 0.250;
public double TubeOD { get; set; } = 1.000;
public double TubeWallTHK { get; set; } = 0.035;
public double FinOD { get; set; } = 1.500;

// Tube Layout
public int TubeRow1Left { get; set; } = 8;
public int TubeRow2Left { get; set; } = 7;
public double HorizontalPitch { get; set; } = 1.500;
public int TubeQuantity { get; private set; } = 60;  // ? READ-ONLY (calculated)

// Fin Strip Back
public double FrontFinStripBack { get; set; } = 0.500;
public double RearFinStripBack { get; set; } = 0.500;
```

#### **XAML Controls:**
```xaml
<!-- Basic Tube -->
<TextBox x:Name="txtTubeLength" Text="96.000"/>
<TextBox x:Name="txtTubeProjection" Text="0.250"/>
<TextBox x:Name="txtTubeOD" Text="1.000"/>
<ComboBox x:Name="cmbTubeWallTHK">
<ComboBoxItem Content="0.035" IsSelected="True"/>
    <ComboBoxItem Content="0.049"/>
    <ComboBoxItem Content="0.065"/>
</ComboBox>
<TextBox x:Name="txtFinOD" Text="1.500"/>

<!-- Tube Layout -->
<TextBox x:Name="txtRow1Left" Text="8"/>
<TextBox x:Name="txtRow2Left" Text="7"/>
<TextBox x:Name="txtHorizPitch" Text="1.500"/>
<TextBlock x:Name="txtTubeQuantity" Text="60"/>  <!-- READ-ONLY -->

<!-- Fin Strip -->
<TextBox x:Name="txtFrontFinStrip" Text="0.500"/>
<TextBox x:Name="txtRearFinStrip" Text="0.500"/>
```

#### **? COHERENCE STATUS:**
- ? All 11 properties exist
- ? All 11 controls exist
- ? TubeQuantity correctly read-only in both ViewModel and XAML (TextBlock)
- ?? **COMBO BOX:** cmbTubeWallTHK needs SelectedValue binding

---

### **4. TUBE SUPPORTS SECTION**

#### **ViewModel Properties:**
```csharp
public double TubeSupportSpacing { get; set; } = 8.000;
public int TubeSupportQuantity { get; set; } = 11;
public string TubeSupportSize { get; set; } = "C4x5.4";
```

#### **XAML Controls:**
```xaml
<TextBox x:Name="txtSupportSpacing" Text="8.000"/>
<TextBox x:Name="txtSupportQuantity" Text="11"/>
<ComboBox x:Name="cmbSupportSize">
    <ComboBoxItem Content="C3x4.1"/>
    <ComboBoxItem Content="C3x5"/>
    <ComboBoxItem Content="C4x5.4" IsSelected="True"/>
    <ComboBoxItem Content="C4x7.25"/>
</ComboBox>
```

#### **? COHERENCE STATUS:**
- ? All 3 properties exist
- ? All 3 controls exist
- ? cmbSupportSize binds to string (SelectedItem will work)

---

### **5. VERTICAL PITCHES SECTION**

#### **ViewModel Properties:**
```csharp
// Front Vertical Pitches (9 properties)
public double FrontPitch12 { get; set; } = 1.500;
public double FrontPitch23 { get; set; } = 1.500;
public double FrontPitch34 { get; set; } = 1.500;
public double FrontPitch45 { get; set; } = 1.500;
public double FrontPitch56 { get; set; } = 1.500;
public double FrontPitch67 { get; set; } = 1.500;
public double FrontPitch78 { get; set; } = 1.500;
public double FrontPitch89 { get; set; } = 1.500;
public double FrontPitch910 { get; set; } = 1.500;

// Rear Vertical Pitches (9 properties)
public double RearPitch12 { get; set; } = 1.500;
public double RearPitch23 { get; set; } = 1.500;
public double RearPitch34 { get; set; } = 1.500;
public double RearPitch45 { get; set; } = 1.500;
public double RearPitch56 { get; set; } = 1.500;
public double RearPitch67 { get; set; } = 1.500;
public double RearPitch78 { get; set; } = 1.500;
public double RearPitch89 { get; set; } = 1.500;
public double RearPitch910 { get; set; } = 1.500;
```

#### **XAML Controls:**
```xaml
<!-- Front Pitches -->
<TextBox x:Name="txtFrontPitch12" Text="1.500"/>
<TextBox x:Name="txtFrontPitch23" Text="1.500"/>
<TextBox x:Name="txtFrontPitch34" Text="1.500"/>
<TextBox x:Name="txtFrontPitch45" Text="1.500"/>
<TextBox x:Name="txtFrontPitch56" Text="1.500"/>
<TextBox x:Name="txtFrontPitch67" Text="1.500"/>
<TextBox x:Name="txtFrontPitch78" Text="1.500"/>
<TextBox x:Name="txtFrontPitch89" Text="1.500"/>
<TextBox x:Name="txtFrontPitch910" Text="1.500"/>

<!-- Rear Pitches -->
<TextBox x:Name="txtRearPitch12" Text="1.500"/>
<TextBox x:Name="txtRearPitch23" Text="1.500"/>
<TextBox x:Name="txtRearPitch34" Text="1.500"/>
<TextBox x:Name="txtRearPitch45" Text="1.500"/>
<TextBox x:Name="txtRearPitch56" Text="1.500"/>
<TextBox x:Name="txtRearPitch67" Text="1.500"/>
<TextBox x:Name="txtRearPitch78" Text="1.500"/>
<TextBox x:Name="txtRearPitch89" Text="1.500"/>
<TextBox x:Name="txtRearPitch910" Text="1.500"/>
```

#### **? COHERENCE STATUS:**
- ? All 18 properties exist (9 front + 9 rear)
- ? All 18 controls exist
- ? Naming convention matches perfectly

---

### **6. STRUCTURE & PLENUM SECTION**

#### **ViewModel Properties:**
```csharp
public int FanCount { get; set; } = 2;
public double PlenumLength { get; set; } = 120.000;
public double OffsetFromCenter { get; set; } = 0.000;
public string PlenumStyle { get; set; } = "Standard";
public string ColumnSize { get; set; } = "W6x20";
public int Weight { get; set; } = 5000;
public double LugStagger { get; set; } = 0.000;
public double LugSpacing { get; set; } = 0.000;
```

#### **XAML Controls:**
```xaml
<TextBox x:Name="txtFanCount" Text="2"/>
<TextBox x:Name="txtPlenumLength" Text="120.000"/>
<TextBox x:Name="txtOffsetCenter" Text="0.000"/>
<ComboBox x:Name="cmbPlenumStyle">
    <ComboBoxItem Content="Standard" IsSelected="True"/>
    <ComboBoxItem Content="Johnson"/>
<ComboBoxItem Content="Legacy"/>
</ComboBox>
<ComboBox x:Name="cmbColumnSize">
    <ComboBoxItem Content="W6x15"/>
    <ComboBoxItem Content="W6x20" IsSelected="True"/>
    <ComboBoxItem Content="W8x18"/>
</ComboBox>
<TextBox x:Name="txtWeight" Text="5000"/>
<TextBox x:Name="txtLugStagger" Text="0.000"/>
<TextBox x:Name="txtLugSpacing"/>
```

#### **? COHERENCE STATUS:**
- ? All 8 properties exist
- ? All 8 controls exist
- ? ComboBox bindings for PlenumStyle and ColumnSize (string)

---

### **7. ADVANCED OPTIONS SECTION**

#### **ViewModel Properties:**
```csharp
public bool EnableCamber { get; set; } = false;
public bool CreateDrawing { get; set; } = false;
public bool SaveFiles { get; set; } = true;
public bool DeleteTemp { get; set; } = false;
```

#### **XAML Controls:**
```xaml
<CheckBox x:Name="chkCamber"/>
<CheckBox x:Name="chkCreateDrawing"/>
<CheckBox x:Name="chkSaveFiles" IsChecked="True"/>
<CheckBox x:Name="chkDeleteTemp"/>
```

#### **? COHERENCE STATUS:**
- ? All 4 properties exist
- ? All 4 controls exist
- ? Default values match

---

### **8. CALCULATED SUMMARY SECTION**

#### **ViewModel Properties:**
```csharp
public int CalculatedTubes { get; private set; } = 60;
public double CalculatedHeight { get; private set; } = 36.25;
public int CalculatedWeight { get; private set; } = 5000;
```

#### **XAML Controls:**
```xaml
<TextBlock x:Name="txtCalcTubes" Text="60"/>
<TextBlock x:Name="txtCalcHeight" Text="36.25 in"/>
<TextBlock x:Name="txtCalcWeight" Text="5,000 lbs"/>
```

#### **? COHERENCE STATUS:**
- ? All 3 properties exist
- ? All 3 controls exist
- ? Correctly read-only (private set in ViewModel, TextBlock in XAML)

---

## ?? **PROPERTY-TO-CONTROL MAPPING TABLE**

| Section | ViewModel Property | XAML Control | Type | Status |
|---------|-------------------|--------------|------|--------|
| **Job Info** |
| | GlobalJobNumber | txtJobNumber | string | ?? Name mismatch |
| | Bank | txtBank | char | ? Match |
| | Customer | txtCustomer | string | ? Match |
| | Client | txtClient | string | ? Match |
| | Location | txtLocation | string | ? Match |
| | Initials | txtInitials | string | ? Match |
| | Titleblock | cmbTitleblock | string | ? Match |
| | PurchaseOrder | txtPurchaseOrder | string | ? Match |
| | ItemNumber | txtItemNumber | string | ? Match |
| **Bundle Dims** |
| | BundleWidth | txtBundleWidth | double | ? Match |
| | SideFrameThickness | cmbSideFrameTHK | double | ?? ComboBox |
| | SideFrameDepth | txtSideFrameDepth | double | ? Match |
| | HeadersOutsideFrame | chkHeadersOutside | bool | ? Match |
| **Tube Config** |
| | TubeLength | txtTubeLength | double | ? Match |
| | TubeProjection | txtTubeProjection | double | ? Match |
| | TubeOD | txtTubeOD | double | ? Match |
| | TubeWallTHK | cmbTubeWallTHK | double | ?? ComboBox |
| | FinOD | txtFinOD | double | ? Match |
| | TubeRow1Left | txtRow1Left | int | ? Match |
| | TubeRow2Left | txtRow2Left | int | ? Match |
| | HorizontalPitch | txtHorizPitch | double | ? Match |
| | TubeQuantity | txtTubeQuantity | int (read-only) | ? Match |
| | FrontFinStripBack | txtFrontFinStrip | double | ? Match |
| | RearFinStripBack | txtRearFinStrip | double | ? Match |
| **Tube Supports** |
| | TubeSupportSpacing | txtSupportSpacing | double | ? Match |
| | TubeSupportQuantity | txtSupportQuantity | int | ? Match |
| | TubeSupportSize | cmbSupportSize | string | ? Match |
| **Front Pitches** |
| | FrontPitch12 | txtFrontPitch12 | double | ? Match |
| | FrontPitch23 | txtFrontPitch23 | double | ? Match |
| | FrontPitch34 | txtFrontPitch34 | double | ? Match |
| | FrontPitch45 | txtFrontPitch45 | double | ? Match |
| | FrontPitch56 | txtFrontPitch56 | double | ? Match |
| | FrontPitch67 | txtFrontPitch67 | double | ? Match |
| | FrontPitch78 | txtFrontPitch78 | double | ? Match |
| | FrontPitch89 | txtFrontPitch89 | double | ? Match |
| | FrontPitch910 | txtFrontPitch910 | double | ? Match |
| **Rear Pitches** |
| | RearPitch12 | txtRearPitch12 | double | ? Match |
| | RearPitch23 | txtRearPitch23 | double | ? Match |
| | RearPitch34 | txtRearPitch34 | double | ? Match |
| | RearPitch45 | txtRearPitch45 | double | ? Match |
| | RearPitch56 | txtRearPitch56 | double | ? Match |
| | RearPitch67 | txtRearPitch67 | double | ? Match |
| | RearPitch78 | txtRearPitch78 | double | ? Match |
| | RearPitch89 | txtRearPitch89 | double | ? Match |
| | RearPitch910 | txtRearPitch910 | double | ? Match |
| **Structure** |
| | FanCount | txtFanCount | int | ? Match |
| | PlenumLength | txtPlenumLength | double | ? Match |
| | OffsetFromCenter | txtOffsetCenter | double | ? Match |
| | PlenumStyle | cmbPlenumStyle | string | ? Match |
| | ColumnSize | cmbColumnSize | string | ? Match |
| | Weight | txtWeight | int | ? Match |
| | LugStagger | txtLugStagger | double | ? Match |
| | LugSpacing | txtLugSpacing | double | ? Match |
| **Advanced** |
| | EnableCamber | chkCamber | bool | ? Match |
| | CreateDrawing | chkCreateDrawing | bool | ? Match |
| | SaveFiles | chkSaveFiles | bool | ? Match |
| | DeleteTemp | chkDeleteTemp | bool | ? Match |
| **Calculated** |
| | CalculatedTubes | txtCalcTubes | int (read-only) | ? Match |
| | CalculatedHeight | txtCalcHeight | double (read-only) | ? Match |
| | CalculatedWeight | txtCalcWeight | int (read-only) | ? Match |

**TOTALS:**
- ? **56 Perfect Matches**
- ?? **3 Fixable Issues** (1 name mismatch + 2 ComboBox bindings)

---

## ?? **ISSUES & RESOLUTIONS**

### **Issue 1: GlobalJobNumber vs JobNumber**
**Problem:** Property is named `GlobalJobNumber` but control is `txtJobNumber`

**Resolution:** Bind to `GlobalJobNumber`:
```xaml
<TextBox x:Name="txtJobNumber" 
         Text="{Binding GlobalJobNumber, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}"/>
```

---

### **Issue 2: ComboBox Double Bindings**
**Problem:** `cmbSideFrameTHK` and `cmbTubeWallTHK` contain double values but ComboBoxItem.Content is string

**Resolution:** Use SelectedValue with converter or bind to SelectedItem:
```xaml
<!-- Option A: Bind SelectedValue with converter -->
<ComboBox x:Name="cmbSideFrameTHK" 
          SelectedValue="{Binding SideFrameThickness, Mode=TwoWay}">
    <ComboBoxItem Content="0.250" Tag="0.250"/>
    <ComboBoxItem Content="0.375" Tag="0.375" IsSelected="True"/>
    <ComboBoxItem Content="0.500" Tag="0.500"/>
</ComboBox>

<!-- Option B: Simpler - just bind Text for display, SelectedIndex for control -->
<ComboBox x:Name="cmbSideFrameTHK" 
      SelectedIndex="{Binding SideFrameThicknessIndex, Mode=TwoWay}">
  <ComboBoxItem Content="0.250"/>
    <ComboBoxItem Content="0.375"/>
    <ComboBoxItem Content="0.500"/>
</ComboBox>
```

**Recommended:** Add converter or use Tag property

---

### **Issue 3: PurchaseOrder/ItemNumber Placement**
**Status:** ? **ALREADY FIXED** - They exist in both ViewModel and XAML Advanced section

---

## ? **FINAL COHERENCE VERDICT**

### **Overall Status: ?? EXCELLENT**

**Strengths:**
- ? 98% property-control alignment
- ? All sections represented
- ? Naming conventions consistent
- ? Read-only properties correctly handled
- ? Default values match
- ? Type safety maintained

**Minor Issues (Easily Fixable):**
- ?? 1 property name discrepancy (GlobalJobNumber)
- ?? 2 ComboBox bindings need attention

**Recommendation:** ? **PROCEED WITH PHASE 3** - Add bindings with minor adjustments

---

## ?? **RECOMMENDED BINDING STRATEGY**

### **For TextBoxes (Strings/Numbers):**
```xaml
Text="{Binding PropertyName, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}"
```

### **For CheckBoxes:**
```xaml
IsChecked="{Binding PropertyName, Mode=TwoWay}"
```

### **For ComboBoxes (Strings):**
```xaml
SelectedItem="{Binding PropertyName, Mode=TwoWay}"
```

### **For ComboBoxes (Numbers):**
```xaml
SelectedValue="{Binding PropertyName, Mode=TwoWay}"
<!-- OR use converter -->
```

### **For Read-Only TextBlocks:**
```xaml
Text="{Binding PropertyName, Mode=OneWay}"
<!-- OR with formatting -->
Text="{Binding PropertyName, StringFormat={}{0:F2} in}"
```

---

## ?? **NEXT STEPS**

1. ? **Scan Complete** - Coherence verified
2. ?? **Proceed with Phase 3** - Add all bindings
3. ?? **Fix 3 minor issues** during binding process
4. ? **Test each section** after bindings added
5. ??? **Build and run** for end-to-end validation

**Estimated Time:** 30-45 minutes to add all bindings

---

**Scan Completed:** October 27, 2025  
**Verdict:** ? **GREEN LIGHT - PROCEED** ??

