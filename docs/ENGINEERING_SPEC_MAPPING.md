# Engineering Specification Mapping
## Excel → UnifiedUI → SolidWorks Automation

**Purpose:** Map engineering calculations from S25140-Prego1.xlsm to automated CAD generation

---

## **Engineering Workflow Understanding**

### **Customer Input:**
- Fluid properties (temperature, pressure, flow rate)
- Performance requirements (heat duty, approach temp)
- Physical constraints (space, weight limits)
- Code requirements (ASME, API standards)

### **Engineer's Excel Calculations:**
1. **Thermal Design** (Inputs_Calcs)
   - Heat transfer calculations
   - Tube surface area required
   - Number of tubes needed
   - Row configuration

2. **Mechanical Design** (Calc_Eng, Calc_Met)
   - Pressure calculations (MAWP)
   - Tube wall thickness (pressure + corrosion)
   - Header box stress analysis
   - Plate thickness calculations

3. **Material Selection** (Material sheet)
   - Based on temperature/pressure
   - Corrosion allowance
   - Code compliance

4. **Structural Analysis** (Stress Table, FEA)
   - Bundle weight distribution
   - Support requirements
   - Lifting calculations

---

## **Critical Engineering Parameters to Extract**

### **BUNDLE ASSEMBLY:**

#### **Thermal/Tube Layout:**
- **Tube Count:** Total tubes needed (from heat transfer calcs)
- **Row Configuration:** How many tubes per row
- **Tube Spacing:** Horizontal & vertical pitch (fouling considerations)
- **Tube Length:** Based on heat duty and available space

#### **Tube Specifications:**
- **OD:** Outer diameter (standard sizes: 0.75", 1.0", 1.25")
- **Wall Thickness:** From pressure + corrosion allowance
- **Material:** Carbon steel, stainless, copper-nickel
- **Fin Type:** Plain, low fin, high fin

#### **Frame/Structure:**
- **Bundle Width:** Determined by tube rows + clearances
- **Side Frame Thickness:** From structural calcs
- **Side Frame Depth:** From tube projection + clearances
- **Headers Inside/Outside Frames:** Design decision

---

### **HEADER ASSEMBLY:**

#### **Pressure Vessel Design:**
- **Box Dimensions:** Width × Height × Length
  - Width: Accommodates tube layout
  - Height: Based on nozzle size + clearance
  - Length: Tube projection + tubesheet + allowances

#### **Tubesheet:**
- **Thickness:** From ASME calculations (pressure, tube pullout)
- **Hole Pattern:** Matches tube layout exactly
- **Hole Diameter:** Tube OD + expansion/welding clearance
- **Tube Projection:** Into header (rolling length)

#### **Plates & Reinforcement:**
- **Cover Plate:** Removable for maintenance
- **Baffle Plates:** Flow distribution
- **Pass Dividers:** If multi-pass design
- **Reinforcement Pads:** Around nozzles

#### **Connections:**
- **Nozzle Sizes:** From flow calculations
- **Nozzle Locations:** Optimal flow, maintenance access
- **Flange Ratings:** Pressure class (150#, 300#, etc.)
- **Drain/Vent:** Code required

#### **Pressure Design:**
- **MAWP:** Maximum Allowable Working Pressure
- **Design Pressure:** Actual operating + safety factor
- **Hydro Test Pressure:** 1.5 × Design (typically)

---

## **Excel Sheet → Parameter Mapping Strategy**

### **Primary Data Sources:**

1. **Inputs_Calcs** (111,901 cells, 99,845 formulas!)
   - Master calculation sheet
   - All derived dimensions
   - Material selections
   - This is THE source of truth

2. **RAGU** (2,476 cells, 2,058 formulas)
   - Your custom calculations
   - Special configurations
   - Need to understand what RAGU does!

3. **Material** (36,046 cells)
   - Material properties lookup
   - Allowable stresses
   - Temperature limits

4. **Prego_to_Sw** (1,139 cells, 567 formulas)
   - Already formatted for SolidWorks!
   - Likely has the exact dimensions we need

5. **Weights** (1,664 cells, 728 formulas)
   - Component weights
   - Center of gravity
   - Lifting calculations

---

## **Engineering Validation Requirements**

### **What Must Be Checked:**
1. ✅ MAWP ≥ Design Pressure (safety!)
2. ✅ Tube wall thickness ≥ Calculated + Corrosion Allowance
3. ✅ Material compatible with fluid/temperature
4. ✅ Nozzle sizes don't create excessive pressure drop
5. ✅ Lifting lugs adequate for weight
6. ✅ All clearances maintained (TEMA standards)

### **Design Standards:**
- **ASME Section VIII Div 1** - Pressure vessel code
- **TEMA** - Tubular Exchanger Manufacturers Association
- **API 660/661** - Heat exchanger standards

---

## **CAD Designer's Perspective**

### **What Designer Needs from Engineering:**

#### **Exact Dimensions (no ambiguity!):**
- Bundle: W × H × D (to 0.001" precision)
- Tube: OD, wall, length, projection
- Header: Box dimensions, plate thicknesses
- Nozzles: Size, location, orientation

#### **Material Callouts:**
- Tubesheet: SA-516-70 (example)
- Tubes: SA-179 (example)
- Shell: SA-516-70
- All with heat treatment requirements

#### **Assembly Information:**
- How tubes attach (rolled, welded, both)
- Gasket requirements
- Bolt specifications
- Weld details

#### **Tolerances:**
- Tube hole diameter: +0.005" / -0.000"
- Plate flatness: ±0.010"
- Overall length: ±0.125"

---

## **Automation Strategy**

### **Phase 1: Cell Mapping (1-2 days)**
Manually identify in Excel:
- Where is Bundle Width? (cell address)
- Where is Tube OD? (cell address)
- Create mapping table of 50-100 critical parameters

### **Phase 2: Import Engine (2-3 days)**
Build robust Excel reader:
- Read Inputs_Calcs cells
- Handle formulas vs values
- Validate data makes engineering sense
- Flag any anomalies

### **Phase 3: Engineering Validation (1 day)**
Add checks:
- Pressure checks
- Material compatibility
- Dimension reasonableness
- Code compliance flags

### **Phase 4: Multi-Component Generation (2-3 days)**
From one Excel import:
- Generate Bundle with all tubes
- Generate Header(s) with all details
- Generate support structure
- Create assembly

### **Phase 5: Automated Report (1-2 days)**
Generate engineering report:
- Design calculations summary
- Material list
- BOM with part numbers
- Weld map
- Replace those PDFs!

---

## **Expected Outcome**

```
TIME SAVINGS:
  Current Process: 8-16 hours per job
    - Engineer: 2-4 hours in Excel
    - Designer: 6-12 hours modeling
  
  New Process: 30 minutes per job
    - Engineer: Still 2-4 hours in Excel (no change)
    - Import Excel: 1 minute
    - Auto-generate: 5 minutes
    - Designer review/tweaks: 15-20 minutes
  
  SAVINGS: 85-95% of design time!
```

---

## **Risk Mitigation**

### **Engineering Risks:**
1. **Wrong cell mapping** → Dimensions incorrect → Scrap parts
   - Mitigation: Extensive validation, test imports

2. **Excel formula changes** → Mapping breaks
   - Mitigation: Version control, cell naming

3. **Edge cases not handled** → Automation fails
   - Mitigation: Fallback to manual, comprehensive testing

### **Design Risks:**
1. **SolidWorks API limitations** → Can't create certain features
   - Mitigation: Identify early, manual finishing steps

2. **Complex geometries** → Automation too complex
   - Mitigation: Start with 80% case, handle special manually

---

*This requires both engineering precision AND design craftsmanship!*
