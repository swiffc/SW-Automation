# Excel Cell Mapping Report

## Found Engineering Parameters


### Bundle Depth

**Sheet:** RAGU

- **Label Cell:** J84
- **Label Text:** "rd" - (raised face depth)
- **Value Cells:**
  - RIGHT: K84 = `=Inputs_Calcs!CQ73` (FORMULA)
  - BELOW: J85 = `"nb" - (number of bolts)`


### Bundle Width

**Sheet:** RAGU

- **Label Cell:** G17
- **Label Text:** WIDTH - tubesheet & plugsheet
- **Value Cells:**
  - BELOW: G18 = `WIDTH - top & bottom plate`

**Sheet:** Prego_to_Sw

- **Label Cell:** C7
- **Label Text:** Box-1 Wet Width
- **Value Cells:**
  - BELOW: C8 = `Box-3 Wet Width`

**Sheet:** Input

- **Label Cell:** E12
- **Label Text:** Bundle Width
- **Value Cells:**
  - RIGHT: F12 = `77.125`
  - BELOW: E13 = `Toed-In/Out?`


### Design Pressure

**Sheet:** Inputs_Calcs

- **Label Cell:** AA43
- **Label Text:** DP1 w/o vac
- **Value Cells:**
  - RIGHT: AB43 = `=IF(AB12="FV",14.7,IF(AB12="HV",7.35,IF(OR(AB12<0,` (FORMULA)
  - BELOW: AA44 = `DP2 w/o vac`

**Sheet:** Prego_to_Sw

- **Label Cell:** A13
- **Label Text:** =Inputs_Calcs!ADP13
- **Value Cells:**
  - BELOW: A14 = `=Inputs_Calcs!ADQ13` (FORMULA)

**Sheet:** Input

- **Label Cell:** A9
- **Label Text:** Design Pressure
- **Value Cells:**
  - RIGHT: B9 = `200`
  - BELOW: A10 = `Design Temp`


### Header Length

**Sheet:** RAGU

- **Label Cell:** G6
- **Label Text:** LENGTH - header box length
- **Value Cells:**
  - BELOW: G7 = `THK - tubesheet & plugsheet`

**Sheet:** Prego_to_Sw

- **Label Cell:** C25
- **Label Text:** Box Length
- **Value Cells:**
  - BELOW: C26 = `End-1 Thk`


### Header Width

**Sheet:** Input

- **Label Cell:** AC42
- **Label Text:** Box Width
- **Value Cells:**
  - RIGHT: AD42 = `=Inputs_Calcs!WD20` (FORMULA)
  - BELOW: AC43 = `Noz CL to TS`


### Horizontal Pitch

**Sheet:** Inputs_Calcs

- **Label Cell:** T64
- **Label Text:** name on Calcs Heading, horizontal pitch
- **Value Cells:**
  - BELOW: T65 = `standards per tube OD, whether partition`

**Sheet:** RAGU

- **Label Cell:** G56
- **Label Text:** X pitch
- **Value Cells:**
  - BELOW: G57 = `X offset - odd tube rows`

**Sheet:** Input

- **Label Cell:** AJ4
- **Label Text:** V Pitch
- **Value Cells:**
  - RIGHT: AK4 = `V Pitch`


### Job Number

**Sheet:** Inputs_Calcs

- **Label Cell:** Q44
- **Label Text:** Job no
- **Value Cells:**
  - RIGHT: R44 = `=Input!H2` (FORMULA)
  - BELOW: Q45 = `Matl`

**Sheet:** RAGU

- **Label Cell:** G4
- **Label Text:** project number
- **Value Cells:**
  - BELOW: G5 = `header letter designation`

**Sheet:** Input

- **Label Cell:** G2
- **Label Text:** Job:
- **Value Cells:**
  - RIGHT: H2 = `S25140`
  - BELOW: G3 = `Item:`


### MAWP

**Sheet:** Inputs_Calcs

- **Label Cell:** AA31
- **Label Text:** MAWP
- **Value Cells:**
  - RIGHT: AB31 = `=Calcs!AJ24` (FORMULA)
  - BELOW: AA32 = `MAP`

**Sheet:** Input

- **Label Cell:** H32
- **Label Text:** MAWP/MAP in Calcs
- **Value Cells:**
  - RIGHT: I32 = `=Inputs_Calcs!AB98` (FORMULA)
  - BELOW: H33 = `=Inputs_Calcs!AB59` (FORMULA)


### Material

**Sheet:** Inputs_Calcs

- **Label Cell:** AP3
- **Label Text:** Material (echo C3)
- **Value Cells:**
  - RIGHT: AQ3 = `=C9` (FORMULA)
  - BELOW: AP4 = `Smls tube?`

**Sheet:** RAGU

- **Label Cell:** J99
- **Label Text:** INLET - material
- **Value Cells:**
  - RIGHT: K99 = `=Inputs_Calcs!$CP$31` (FORMULA)
  - BELOW: J100 = `OUTLET - nominal size`

**Sheet:** Input

- **Label Cell:** K6
- **Label Text:** Matl
- **Value Cells:**
  - RIGHT: L6 = `=Inputs_Calcs!AQ9` (FORMULA)
  - BELOW: K7 = `=Inputs_Calcs!CA32` (FORMULA)


### Row Count

**Sheet:** Inputs_Calcs

- **Label Cell:** Q2
- **Label Text:** Rows per Header
- **Value Cells:**
  - BELOW: Q3 = `Front split 1 after pass`

**Sheet:** RAGU

- **Label Cell:** J53
- **Label Text:** Rows/box
- **Value Cells:**
  - RIGHT: K53 = `=Inputs_Calcs!R8` (FORMULA)
  - BELOW: J54 = `Y`

**Sheet:** Input

- **Label Cell:** E9
- **Label Text:** Rows
- **Value Cells:**
  - RIGHT: F9 = `=Inputs_Calcs!J53` (FORMULA)
  - BELOW: E10 = `Boxes`


### Tube Count

**Sheet:** RAGU

- **Label Cell:** G72
- **Label Text:** number of tubes to right of shaft
- **Value Cells:**
  - BELOW: G73 = `number of tubes to left of shaft`

**Sheet:** Input

- **Label Cell:** H13
- **Label Text:** Fan Qty
- **Value Cells:**
  - RIGHT: I13 = `2`
  - BELOW: H14 = `Shaft-Thru`


### Tube Length

**Sheet:** Inputs_Calcs

- **Label Cell:** AA5
- **Label Text:** Metric Length
- **Value Cells:**
  - RIGHT: AB5 = `=IF(AB3="","",INDEX(Pulldowns_Lookups!H45:H52,MATC` (FORMULA)
  - BELOW: AA6 = `Conversion`

**Sheet:** RAGU

- **Label Cell:** G6
- **Label Text:** LENGTH - header box length
- **Value Cells:**
  - BELOW: G7 = `THK - tubesheet & plugsheet`

**Sheet:** Prego_to_Sw

- **Label Cell:** C25
- **Label Text:** Box Length
- **Value Cells:**
  - BELOW: C26 = `End-1 Thk`

**Sheet:** Input

- **Label Cell:** K15
- **Label Text:** Tube Length (ft)
- **Value Cells:**
  - RIGHT: L15 = `18`
  - BELOW: K16 = `Pitch`


### Tube OD

**Sheet:** Inputs_Calcs

- **Label Cell:** J2
- **Label Text:** Is odd?
- **Value Cells:**
  - RIGHT: K2 = `Is even?`
  - BELOW: J3 = `=IF(AND(ISODD(I3),I3>0),1,0)` (FORMULA)

**Sheet:** RAGU

- **Label Cell:** G3
- **Label Text:** product line
- **Value Cells:**
  - BELOW: G4 = `project number`

**Sheet:** Prego_to_Sw

- **Label Cell:** C100
- **Label Text:** Inlet Pipe OD
- **Value Cells:**
  - BELOW: C101 = `Outlet Pipe OD`

**Sheet:** Input

- **Label Cell:** K10
- **Label Text:** Tube OD
- **Value Cells:**
  - RIGHT: L10 = `1`
  - BELOW: K11 = `Fins`


### Tube Wall

**Sheet:** Inputs_Calcs

- **Label Cell:** AP48
- **Label Text:** Min thk 1
- **Value Cells:**
  - RIGHT: AQ48 = `=IF(AB43>0,AB43*AQ45/(AQ29+0.4*AB43),0)` (FORMULA)
  - BELOW: AP49 = `Min thk 2`

**Sheet:** RAGU

- **Label Cell:** G7
- **Label Text:** THK - tubesheet & plugsheet
- **Value Cells:**
  - RIGHT: H7 = `These are the cells which I updated per 3.3.2.  gj`
  - BELOW: G8 = `THK - top & bottom plate`

**Sheet:** Prego_to_Sw

- **Label Cell:** C13
- **Label Text:** Top/Btm-1 Thk
- **Value Cells:**
  - BELOW: C14 = `Top/Btm-3 Thk`

**Sheet:** Input

- **Label Cell:** K14
- **Label Text:** Tubewall Thk
- **Value Cells:**
  - RIGHT: L14 = `=Inputs_Calcs!AY2` (FORMULA)
  - BELOW: K15 = `Tube Length (ft)`


### Tubesheet Thickness

**Sheet:** RAGU

- **Label Cell:** G7
- **Label Text:** THK - tubesheet & plugsheet
- **Value Cells:**
  - RIGHT: H7 = `These are the cells which I updated per 3.3.2.  gj`
  - BELOW: G8 = `THK - top & bottom plate`

**Sheet:** Prego_to_Sw

- **Label Cell:** C38
- **Label Text:** Tubesheet-1 PartNo
- **Value Cells:**
  - BELOW: C39 = `Tubesheet-3 PartNo`

**Sheet:** Input

- **Label Cell:** AC49
- **Label Text:** Tubesheet Thk
- **Value Cells:**
  - RIGHT: AD49 = `=Inputs_Calcs!ADP72` (FORMULA)
  - BELOW: AC50 = `=IF(B15="Plug","Plugsheet Thk","CVRPLT thk")` (FORMULA)


---

## Recommended Mapping

```csharp
// Bundle Parameters
config.BundleDepth = ReadCell("RAGU", "K84");
config.BundleWidth = ReadCell("RAGU", "G18");
config.TubeCount = ReadCell("RAGU", "G73");
config.TubeLength = ReadCell("Inputs_Calcs", "AB5");
config.TubeOD = ReadCell("Inputs_Calcs", "K2");
config.TubeWall = ReadCell("Inputs_Calcs", "AQ48");
config.TubesheetThickness = ReadCell("RAGU", "H7");

// Header Parameters
config.HeaderLength = ReadCell("RAGU", "G7");
config.HeaderWidth = ReadCell("Input", "AD42");
config.TubesheetThickness = ReadCell("RAGU", "H7");
```
