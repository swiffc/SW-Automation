# Complete Field Extraction from Original UIs

**Purpose:** Document every field from original UIs to migrate into UnifiedUI

---

## 🎯 **PLENUM UI Fields** (from PlenumUI.Designer.cs)

### **Main Tab - Plenum:**
- Width (L×L)
- Length (L×L)  
- Plenum Depth
- Fan Count
- Fan Diameter
- Fan Ring Depth
- Mid Columns (checkbox)
- Column Size (combobox: W8×31, etc.)
- Total Column Height
- End Panel Thickness (combobox: 0.375)
- Side Panel Thickness (combobox: 0.375)
- Motor Removal Beam (checkbox)
- Machinery Mount Design (combobox: Motor Shaft Down, etc.)
- Machinery Mount Width
- Machinery Mount Headroom

### **Bracing:**
- Brace Type (combobox: TX, etc.)
- Brace Angle
- Brace Clip Height
- Brace Clip Thickness
- Brace Hole Diameter
- Flange Gage WT

### **Job Info:**
- Bank (s24441, A, etc.)
- Customer
- Client
- Location
- Purchase Order
- Item Number
- Your Initials
- Material (combobox: A572_50)

### **Advanced Options Tab:**
- Ship Beam Height
- Shaft Up/Down checkboxes
- Length/Width/Depth overrides
- Splice/End/Divider overrides
- Extra plenum length
- Floor buttons (Standard/Johnson/Legacy)

---

## 📦 **BUNDLE UI Fields** (from BundleUI.Designer.cs)

### **Main Tab:**
- Bundle Width
- Job Number
- Bank
- Fan Count
- Plenum Length
- Offset From Plenum Center
- Plenum Style (combobox)
- Column Size (combobox)
- Extra Length
- Weight
- Lifting Lug Spacing
- Lug Stagger

### **Tube Configuration:**
- Front Vertical Pitch (rows 1-2, 2-3, 3-4, 4-5, 5-6, 6-7, 7-8, 8-9, 9-10)
- Rear Vertical Pitch (rows 1-2, 2-3, 3-4, 4-5, 5-6, 6-7, 7-8, 8-9, 9-10)
- Tube Support Size (combobox)
- Tube Support Spacing (feet)
- Tube Support Quantity
- Camber (checkbox)
- Tileblock Manufacturer (combobox)

### **Extensive Fields** (100+ more fields in designer):
The Bundle UI has 2621 lines with fields including:
- Row counts (multiple rows)
- Tube projections
- Frame dimensions
- Side frame details
- Header inside/outside options
- Material selections
- Tube specifications
- And much more...

---

## 📋 **HEADER UI Fields** (from HeaderUI.Designer.cs)

### **Box Tab:**
- Stiffener configurations (below rows 1, 2, 3 for headers 61-66)
- Partition configurations (below rows 1, 2, 3)
- Stiffener distances (multiple)
- Partition distances (multiple)
- Foot plate part numbers (61-66)

### **Massive Configuration** (8413 lines!):
The Header UI has the most complex configuration with:
- Multiple header types (61, 62, 63, 64, 65, 66)
- Partition details per header
- Stiffener details per header
- Distance measurements
- Part numbers
- Material selections
- Connection details
- Tubesheet configurations
- And 200+ more fields...

---

## 🏠 **HOOD UI Fields** (from HoodUI.Designer.cs)

### **Job Info Tab:**
- Job Number
- Job Customer
- Job Client
- Job Location
- Job PO
- Job Item No
- Initials
- Bank

### **Hood Config Tab:**
- Bank
- Length
- Width
- Height
- Stacks
- Depth (combobox)
- Fan Diameter
- Wind Load (combobox)

### **Advanced Tab:**
- Shift Stiffeners
- Adjust

---

## 📊 **FIELD COUNT SUMMARY**

| UI Component | Approximate Fields | Designer Lines | Complexity |
|--------------|-------------------|----------------|------------|
| Plenum | 40-50 | 1,313 | Medium |
| Bundle | 150-200+ | 2,621 | Very High |
| Header | 200-300+ | 8,413 | Extremely High |
| Hood | 15-20 | 561 | Low |
| Structure | 60+ | ~1,500 | High |
| Walkway | 30-40 | ~800 | Medium |

**Total Estimated Fields: 500-700+ across all UIs!**

---

## 🎯 **IMPLEMENTATION STRATEGY**

### **Option 1: Complete Migration (Weeks of work)**
- Extract every single field from every UI
- Create corresponding WPF controls
- Wire up all data bindings
- **Time:** 40-80 hours

### **Option 2: Essential Fields (Recommended)**
- Migrate the TOP 20 most-used fields per component
- Add "Advanced" expanders for less common fields
- Gradually add more as needed
- **Time:** 10-15 hours

### **Option 3: Hybrid Approach**
- Complete migration for simple UIs (Hood, Structure)
- Essential fields for complex UIs (Bundle, Header)
- Keep original UIs available for edge cases
- **Time:** 15-20 hours

---

## 💡 **RECOMMENDED NEXT STEPS**

### **Phase 1: Quick Wins (1-2 hours)**
Complete these simple UIs fully:
1. ✅ Hood (15 fields)
2. ✅ Walkway (30 fields)
3. ✅ Machinery Mount (25 fields)

### **Phase 2: Essential Fields (3-4 hours)**
Add top 20 fields for complex UIs:
1. Bundle - Core dimensions, tube config, job info
2. Header - Box dimensions, basic config
3. Plenum - Already done! (see your screenshot)

### **Phase 3: Advanced Features (As Needed)**
Add expandable "Advanced Options" sections with remaining fields

---

## 🔧 **IMPLEMENTATION EXAMPLE: Plenum**

Based on your screenshot, Plenum needs these fields:

```xml
<!-- Main Fields (Left Column) -->
- Width (L×L): 108
- Length (L×L): 240
- Plenum Depth: 36
- Fan Count: 2
- Fan Diameter: 8
- Fan Ring Depth: 24
- Machinery Mount Design: Motor Shaft Down (dropdown)
- Machinery Mount Width: 36
- Machinery Mount Headroom: 54

<!-- Configuration (Right Column) -->
- Mid Columns: ☑
- Column Size: W8×31 (dropdown)
- Total Column Height: 120
- Motor Removal Beam: ☑
- End Panel Thickness: 0.375 (dropdown)
- Side Panel Thickness: 0.375 (dropdown)

<!-- Job Info (Right Side) -->
- Bank: s24441 A
- Customer: (text)
- Client: (text)
- Location: (text)
- Purchase Order: (text)
- Item Number: (text)
- Your Initials: (text)
- Material: A572_50 (dropdown)

<!-- Bottom Buttons -->
- Standard / Johnson / Legacy (floor type buttons)
- Extra Plenum Length checkbox
```

---

## 📝 **QUICK REFERENCE**

**For each UI, extract:**
1. TextBox fields → WPF TextBox
2. ComboBox dropdowns → WPF ComboBox  
3. CheckBox toggles → WPF CheckBox
4. Button actions → WPF Button with Click handlers

**All bound to MainViewModel properties**

---

**RECOMMENDATION: Start with Plenum (you have the reference screenshot). I can create the complete Plenum panel now with all those fields!**

Want me to build the complete Plenum panel matching your screenshot?
