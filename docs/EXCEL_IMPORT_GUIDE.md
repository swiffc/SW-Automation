# Excel Template Import - User Guide

## 🎯 **Two Ways to Configure Components**

Users can choose the method that works best for them:

### **Option 1: Import from Excel Template** ⚡ (Fastest!)
- Click **Import** button in toolbar
- Select Excel file (e.g., `S25140-Prego1.xlsm`)
- Form auto-fills with values
- Modify if needed
- Click **Generate**

### **Option 2: Manual Entry** ✍️ (Most Flexible!)
- Fill form fields manually
- Full control over every parameter
- Click **Generate**

---

## 📁 **Supported Excel Files**

The importer supports:
- ✅ `.xlsm` - Excel Macro-Enabled (like S25140-Prego1.xlsm)
- ✅ `.xlsx` - Excel Workbook
- ✅ `.xls` - Legacy Excel

---

## 🚀 **How Import Works**

### **Current Implementation (Stub):**
1. User clicks Import
2. File dialog opens (default: Automation folder)
3. Selects Excel template
4. System extracts:
   - Job number from filename
   - Component type detection
   - Placeholder values for now

### **Full Implementation (When Office Interop Added):**
1. Opens Excel file
2. Reads `Inputs_Calcs` sheet
3. Maps specific cells to form fields
4. Extracts all calculated values
5. Populates entire form

---

## 📊 **What Gets Imported**

### **Bundle:**
- Job number
- Bundle dimensions (width, depth, thickness)
- Tube configuration
- Row counts
- All calculated fields

### **Header:**
- Job number
- Header type
- Box dimensions
- Plate specifications
- Connection details

---

## 🔧 **For Developers**

### **ExcelTemplateImporter Service:**
Located: `UnifiedUI\Services\ExcelTemplateImporter.cs`

**Key Methods:**
```csharp
ImportBundleFromExcel(string excelFilePath)
ImportHeaderFromExcel(string excelFilePath)
DetectComponentType(string excelFilePath)
ExtractJobNumberFromFileName(string filePath)
```

### **To Enable Full Excel Reading:**
1. Add Microsoft.Office.Interop.Excel reference
2. Uncomment code in ExcelTemplateImporter
3. Map Excel cells to configuration properties
4. Test with S25140-Prego1.xlsm

---

## 💡 **User Experience**

The UI clearly shows import is **OPTIONAL**:
- Yellow tip box in Bundle panel
- "Import (toolbar) OR fill manually"
- No pressure to use Excel if not needed

---

## 📝 **Future Enhancements**

- [ ] Full Office Interop implementation
- [ ] Recent files list in Import dialog
- [ ] Excel template preview
- [ ] Cell mapping configuration
- [ ] Import validation with warnings
- [ ] Save imported configs as templates

---

*Status: Excel import foundation ready ✅*  
*Next: Add Office Interop for full implementation*
