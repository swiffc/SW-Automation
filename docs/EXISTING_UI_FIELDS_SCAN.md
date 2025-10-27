# Existing UI Fields - Complete Scan

## Purpose
This document maps all fields from existing component UIs to migrate into the new UnifiedUI.

---

## ✅ **Existing UIs Found:**

1. **BundleUI.cs** - Bundle configuration
2. **HeaderUI.cs** - Header configuration  
3. **HoodUI.cs** - Hood configuration
4. **PlenumUI.cs** - Plenum configuration
5. **StructureUI.cs** - Structure configuration (SCANNED)
6. **WalkwayUI.cs** - Walkway configuration

---

## 📋 **Structure UI Fields** (From StructureUI.cs)

### **Job Information Tab:**
- Bank (textBox_Bank)
- Job/Project (job_Box)
- Customer (customer_Box)
- Client (client_Box)
- Location (location_Box)
- Purchase Order (purchaseOrder_Box)
- Item Number (itemNumber_Box)
- Initials (initials_Box)

### **Column Configuration:**
- Plenum Width (width_TextBox)
- Plenum Length (length_TextBox)
- Total Column Height (height_TextBox)
- Mid Columns (midColumns_Box) - Checkbox
- Beams Rotated (rotate_Box) - Checkbox
- Beam Size (beamSize_Box) - ComboBox
- Beam Depth (textBox_Depth)
- Beam Web THK (textBox_WebTHK)
- Beam Flange Width (textBox_FlangeWidth)
- Beam Flange THK (textBox_FlangeTHK)
- Beam K (textBox_K)
- Beam K1 (textBox_K1)
- Material Spec (materialCombo) - ComboBox

### **Base Plate:**
- Base Plate Width (bpWidth_Box)
- Base Plate Length (bpLength_Box)
- Width Hole Spacing (wSPA_Box)
- Length Hole Spacing (lSPA_Box)
- Hole Diameter (dia_Box)
- Base Plate THK (textBox_BasePlateTHK)

### **Plenum:**
- Fan Count (fanCount_Box)
- Plenum Depth (depth_Box)

### **Machinery Mount:**
- MM Height (mmHeight_Box)

### **Braces Tab:**
- Brace Type (braceType_Box) - ComboBox
- Brace Hole Diameter (braceHoleDiameter_Box)
- Brace Angle (braceAngle_Box)
- Clip THK (clipTHK_Box)
- Leg 1 (leg1_Box)
- Leg 2 (leg2_Box)
- Gage (gage_Box)
- THK L (thkL_Box)
- K L (kL_Box)
- Depth WT (depthWT_Box)
- Stem THK WT (stemTHKWT_Box)
- Flange Width WT (flangeWidthWT_Box)
- Flange THK WT (flangeTHKWT_Box)
- K WT (kWT_Box)
- K1 WT (k1WT_Box)
- Flange Gage WT (flangeGageWT_Box)
- Clip Height (textBox_ClipHeight)

### **Other:**
- Ship Beam Height (textBoxShipBeamHeight)
- Drive Width (textBox_DriveWidth)

---

## 🎯 **Next Steps:**

For EACH component UI, I need to:

1. **Scan the Designer.cs file** to get all field names
2. **Extract field labels and types** (TextBox, ComboBox, CheckBox, etc.)
3. **Create beautiful WPF panel** with organized sections
4. **Add data binding** to ViewModel properties
5. **Group related fields** logically

---

## 📊 **Estimated Fields Per Component:**

- **Bundle**: ~50-80 fields (tubes, dimensions, frames)
- **Header**: ~60-100 fields (box, plates, tubes, connections)
- **Hood**: ~30-40 fields (dimensions, louvers)
- **Plenum**: ~40-50 fields (dimensions, baffles)
- **Structure**: ~60+ fields (columns, beams, braces) ✅ MAPPED
- **Walkway**: ~30-40 fields (dimensions, handrails)

**Total: ~300+ configuration fields across all components!**

---

## ⚡ **Recommendation:**

Due to the large number of fields, we should:

1. **Priority 1**: Complete Bundle panel (most used)
2. **Priority 2**: Complete Header panel (most complex)
3. **Priority 3**: Structure panel (already mapped above)
4. **Priority 4**: Others as needed

**OR**

Create all panels with the MOST IMPORTANT fields only (10-15 per component), then add details later.

---

*Status: Structure fields mapped ✅*  
*Next: Scan remaining UIs for field lists*
