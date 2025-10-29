C          DATA SET T3H31D     AT LEVEL 023 AS OF 09/26/90              00000000
      SUBROUTINE T3H31D(WINDL,MTYPE,CASEB,CASEC,VEPANE,VSPANE,          00000010
     1XACCVE,XBCCVE,XCCCVE,XACCVS,XBCCVS,XCCCVS,IVE,IVS,                00000020
     2EVBOLT,SVBOLT,VEQTY,VSQTY,NVE,NVS)                                00000030
C                                                                       00000040
C         MAIN INPUT                                                    00000050
C     WINDL = WIND LOAD                                                 00000060
C     LUNITS = QTY OF UNITS                                             00000070
C     MTYPE  = MATERIAL TYPE (1 = 10 GA.),(2 = 10 GA.),(3 = 3/16)       00000080
C     ISHIP = UNIT SHIPPING CODE - 1 FOR SPLIT                          00000090
C     NOFAN = NUMBER OF FANS                                            00000100
C     F = FAN DIAM.                                                     00000110
C                                                                       00000120
C         MAIN COMPUTE                                                  00000130
C     NUMBER(11) = CHECK FOR COMMON CENTER PANEL                        00000140
C     CASE(1) = CASEB CHECK FOR SPLIT END/CTR. PANEL                    00000150
C     CASE(2) = CASEC CHECK FOR SPLIT SIDE PANEL                        00000160
C     CODE(25) = END/CENTER PANEL WITH (MP) VSPANE                      00000170
C     CODE(26) = SIDE PANEL WIDTH (MP) VSPANE                           00000180
C                                                                       00000190
C         SUBROUTINE RETURN                                             00000200
C     CODE(30) = FIRST VERTICAL STIFF. OFF END/CENTER PANEL CTR         00000210
C     CODE(31) = SECOND VERTICAL STIFF. OFF END/CENTER PANEL CTR        00000220
C     CODE(32) = THIRD VERTICAL STIFF. OFF END/CENTER PANEL CTR         00000230
C     CODE(33) = FIRST VERTICAL STIFF. OFF SIDE PANEL CTR               00000240
C     CODE(34) = SECOND VERTICAL STIFF. OFF SIDE PANEL CTR              00000250
C     CODE(35) = THIRD VERTICAL STIFF. OFF SIDE PANEL CTR               00000260
C     EVBOLT = TOTAL QTY. END/CENTER PANEL BOLTS                        00000270
C     SVBOLT = TOTAL QTY. OF SIDE PANEL STIFF.                          00000280
C     NCODE(5)=  SPACING END/CENTER PANEL (IVE)                         00000290
C     NCODE(11)= SPACING SIDE PANEL (IVS)                               00000300
C     VEQTY = QTY OF END/CTR. STIFF.                                    00000310
C     VSQTY = QTY OF SIDE STIFF.                                        00000320
C                                                                       00000330
C                                                                       00000340
      COMMON IRUN, IDATEX(3)                                             00000350
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000360
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000370
     *,INTS                                                             00000380
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000390
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000400
      COMMON/INOUT/NRDR,NWTR                                            00000410
      REAL CCVE,CCVS,VEPANE,VSPANE                                      00000420
      REAL WINDL,XACCVE,XBCCVE,XCCCVE,XACCVS,XBCCVS,XCCCVS              00000430
      REAL SVCCVE,SVCCVS,VNE,VNS,TNVS,TNVE                              00000440
C                                                                       00000450
      INTEGER NVE,N,NVS,MTYPE,IVE,IVS,SIDEB,CTENDB                      00000460
      INTEGER VSQTY,VEQTY,SVBOLT,EVBOLT                                 00000470

C  RJD 6/16/93 Added the "PPS" array and new code to support
C              a larger range of wind loads and material types.

C          The array "PPS" represents the Plenum Panel Stiffners
C          table. It consists of 5 tables, each table represents
C          the height limits for a given Wind Load and Center to
C          Center spacing of the stiffner. The 15 rows of each table
C          represent the the different windloads (from 30 to 100 by 5,
C          i.e. row 1 is for wind load of 30, row 2 is for 35, row 3
C          is for 40... row 15 is for 100).
C          Each table represents the entries for 5 different material
C          types. 
C          Table 1 is 10 GA plate with L 2 X 2 X 3/16 stiffners.
C          Table 2 is 10 GA plate with L 2.5 X 2.5 X 3/16 stiffners.
C          Table 3 is 3/16 inch plate with L 2 X 2 X 3/16 stiffners.
C          Table 4 is 3/16 inch plate with L 2.5 X 2.5 X 3/16 stiffners.
C          Table 5 is 1/4 inch plate with 2.5 X 2.5 X 1/4 stiffners.
C
C          So the first index of the array is C/C stiffner spacing
C          the second index is the wind load and the third index is
C          the material type.

      REAL PPS(5,15,5)

C           WLI is a  Wind Load Index. Used to as an index variable
C           for the PPS array. WLI will be 1 if Wind Load = 30, 2 if
C           Wind Load = 35 ... 15 if Wind Load = 100

      INTEGER WLI
      
C           The variable "I" is used as a counter for loops.

      INTEGER I

C           "CCVEbad" and "CCVSbad" are flags to indicate that the
C           inputs for the lookup of CCVE and CCVS fell out of the
C           range of the table. (The user should probably use the
C           next larger material type in this instance).

      LOGICAL*1 CCVEbad, CCVSbad
      
      DATA BA,BB,BC /'''B1''','''B2''','''B3'''/                        00000480
      DATA D /' ''D'''/                                                 00000490
      
      DATA PPS /  94.0, 84.0, 76.0, 71.0, 66.0,
     &            87.0, 77.0, 71.0, 65.0, 61.0,
     &            81.0, 72.0, 66.0, 61.0, 57.0,
     &            76.0, 68.0, 62.0, 58.0, 54.0,
     &            72.0, 65.0, 59.0, 55.0, 51.0, 
     &            69.0, 62.0, 56.0, 52.0, 49.0, 
     &            66.0, 59.0, 54.0, 50.0, 47.0, 
     &            64.0, 57.0, 52.0, 48.0, 45.0, 
     &            61.0, 55.0, 50.0, 46.0, 43.0, 
     &            59.0, 53.0, 48.0, 45.0, 42.0, 
     &            57.0, 51.0, 47.0, 43.0, 41.0, 
     &            56.0, 50.0, 45.0, 42.0, 39.0, 
     &            54.0, 48.0, 44.0, 41.0, 38.0, 
     &            53.0, 47.0, 43.0, 40.0, 37.0, 
     &            51.0, 46.0, 42.0, 39.0, 36.0, 
     &
     &            99.0, 89.0, 81.0, 75.0, 70.0,  
     &            92.0, 82.0, 75.0, 69.0, 65.0,  
     &            86.0, 77.0, 70.0, 65.0, 61.0,  
     &            81.0, 72.0, 66.0, 61.0, 57.0, 
     &            77.0, 69.0, 63.0, 58.0, 54.0, 
     &            73.0, 65.0, 60.0, 55.0, 52.0, 
     &            70.0, 63.0, 57.0, 53.0, 50.0, 
     &            67.0, 60.0, 55.0, 51.0, 48.0, 
     &            65.0, 58.0, 53.0, 49.0, 46.0, 
     &            63.0, 56.0, 51.0, 47.0, 44.0, 
     &            61.0, 54.0, 50.0, 46.0, 43.0, 
     &            59.0, 53.0, 48.0, 45.0, 42.0, 
     &            57.0, 51.0, 47.0, 43.0, 40.0, 
     &            56.0, 50.0, 45.0, 42.0, 39.0, 
     &            54.0, 49.0, 44.0, 41.0, 38.0, 
     &            
     &            98.0, 87.0, 80.0, 74.0, 69.0, 
     &            90.0, 81.0, 74.0, 68.0, 64.0, 
     &            85.0, 76.0, 69.0, 64.0, 60.0, 
     &            80.0, 71.0, 65.0, 60.0, 56.0, 
     &            76.0, 68.0, 62.0, 57.0, 53.0, 
     &            72.0, 64.0, 59.0, 54.0, 51.0, 
     &            69.0, 62.0, 56.0, 52.0, 49.0, 
     &            66.0, 59.0, 54.0, 50.0, 47.0, 
     &            64.0, 57.0, 52.0, 48.0, 45.0, 
     &            62.0, 55.0, 50.0, 47.0, 44.0, 
     &            60.0, 53.0, 49.0, 45.0, 42.0, 
     &            58.0, 52.0, 47.0, 44.0, 41.0, 
     &            56.0, 50.0, 46.0, 43.0, 40.0, 
     &            55.0, 49.0, 45.0, 41.0, 39.0, 
     &            53.0, 48.0, 44.0, 40.0, 38.0, 
     &            
     &           120.0,108.0, 98.0, 91.0, 85.0, 
     &           112.0,100.0, 91.0, 84.0, 79.0, 
     &           104.0, 93.0, 85.0, 79.0, 74.0, 
     &            98.0, 88.0, 80.0, 74.0, 70.0, 
     &            93.0, 83.0, 76.0, 71.0, 66.0, 
     &            89.0, 80.0, 73.0, 67.0, 63.0, 
     &            85.0, 76.0, 70.0, 64.0, 60.0, 
     &            82.0, 73.0, 67.0, 62.0, 58.0, 
     &            79.0, 71.0, 64.0, 60.0, 56.0, 
     &            76.0, 68.0, 62.0, 58.0, 54.0, 
     &            74.0, 66.0, 60.0, 56.0, 52.0, 
     &            72.0, 64.0, 58.0, 54.0, 51.0, 
     &            70.0, 62.0, 57.0, 53.0, 49.0, 
     &            68.0, 61.0, 55.0, 51.0, 48.0, 
     &            66.0, 59.0, 54.0, 50.0, 47.0, 
     &            
     &           127.0,113.0,103.0, 96.0, 90.0, 
     &           117.0,105.0, 96.0, 89.0, 83.0, 
     &           110.0, 98.0, 90.0, 83.0, 78.0, 
     &           103.0, 93.0, 84.0, 78.0, 73.0, 
     &            98.0, 88.0, 80.0, 74.0, 69.0, 
     &            94.0, 84.0, 76.0, 71.0, 66.0, 
     &            90.0, 80.0, 73.0, 68.0, 63.0, 
     &            86.0, 77.0, 70.0, 65.0, 61.0, 
     &            83.0, 74.0, 68.0, 63.0, 59.0, 
     &            80.0, 72.0, 65.0, 61.0, 57.0, 
     &            78.0, 69.0, 63.0, 59.0, 55.0, 
     &            75.0, 67.0, 61.0, 57.0, 53.0, 
     &            73.0, 65.0, 60.0, 55.0, 52.0, 
     &            71.0, 64.0, 58.0, 54.0, 50.0, 
     &            69.0, 62.0, 57.0, 52.0, 49.0/
      
C                                                                       00000500
      SIDEB = 0                                                         00000510
      CTENDB = 0                                                        00000520
C                                                                       00000530
C                                                                       00000540
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000550
C        THIS SUBROUTINE COMPUTES THE VERTICAL STIFFENERS              C00000560
C             FOR END/CENTER INDUCTED DRAFT HOOD PANELS.               C00000570
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000580
C     C                                                                 00000590
C                                                                       00000600
C        A. VERTICAL SPAN LENGTH (VSPANE) FOR END/CENTER PANEL          00000610
C     C     (FROM MAIN PROGRAM)                                         00000620
C                                                                       00000630
C                                                                       00000640
C     C  B. DETERMINE THE MAXIMUM CENTER-TO-CENTER                      00000650
C           DISTANCE BETWEEN STIFFENERS.SITIONS :',I5,                  00000660
C           CCVE = CENTER-TO-CENTER DISTANCE BETWEEN                    00000670
C     C            VERTICAL STIFFENERS.                                 00000680
C                                                                       00000690

C RJD 6/16/93 Code to get CCVE value from the array "PPS"

C           Set CCVE to 24.0 - 6.0 so that we can calculate it
C           by looping through the table.
      CCVE=18.0

C           Calculate the index value for the given wind laod.
      WLI=INT((WINDL - 25)/ 5.0)
      IF(WLI .LT. 1) WLI=1
      IF(WLI .GT. 15) WLI=15

C           Loop through the table with the given wind load and
C           material type. Add 6 inches to each time to get the
C           values (24, 30, 36, 42, 48).
      I=1
      WHILE ( (VEPANE .LE. PPS(I, WLI, MTYPE)) .AND. (I .LT. 6))
         CCVE=CCVE + 6.0
         I=I+1
      END DO

C            If the span length was greater than the maximum value 
C            in the table then set the flag. (issue warning later).
      IF (CCVE .EQ. 18.0) THEN
         CCVEbad=.TRUE.
	 CCVE=24.0
	 WRITE(NWTR,*) "VEPANE OUT OF RANGE!!! Use larger material!!"
      ELSE
         CCVEbad=.FALSE.
      END IF
	 
C                                                                       00001340
C       C. CALCULATE MAX. HORIZ. SPAN DISTANCE (VNE)                    00001350
C     C    FOR STIFFENER PLACEMENTS                                     00001360
C                                                                       00001370
  30  SVCCVE = CCVE                                                     00001380
      VNE = F - 15.00                                                   00001390
C                                                                       00001400
C      D. TOTAL QUANTITY OF VERTICAL STIFFENERS (NVE) PER               00001410
C         END/CENTER PANEL                                              00001420
C                                                                       00001430
      NVE = VNE / CCVE                                                  00001440
      TNVE = VNE / CCVE                                                 00001450
      TNVE = TNVE - NVE                                                 00001460
      IF (TNVE .GT. 0.0) NVE = NVE + 1                                  00001470
C                                                                       00001480
      IF(NVE.LE.0) GO TO 50                                             00001490
C                                                                       00001500
C      E. CALCULATE ACTUAL CENTER-TO-CENTER (HORIZ.)                    00001510
C         DISTANCE (CCVE) BETWEEN THE STIFFENERS FOR                    00001520
C         END/CENTER PANELS.                                            00001530
C                                                                       00001540
      CCVE = VNE/NVE                                                    00001550
      IF(CCVE .GT. SVCCVE) CCVE = SVCCVE                                00001560
C                                                                       00001570
C         ROUND DOWN 1/8" (WHERE N IS INTEGER)                          00001580
C                                                                       00001590
      N = CCVE * 8                                                      00001600
      CCVE = .125 *N                                                    00001610
C                                                                       00001620
       IF(NVE .EQ. 1) GO TO 31                                          00001630
       IF(NVE .EQ. 3) GO TO 31                                          00001640
       IF(NVE .EQ. 5) GO TO 31                                          00001650
       IF(NVE .EQ. 7) GO TO 31                                          00001660
       GO TO 32                                                         00001670
C                                                                       00001680
  31   IF(CASEB .EQ. BA) NVE = NVE - 1                                  00001690
       IF(CASEB .EQ. BB) NVE = NVE - 1                                  00001700
       IF(CASEB .EQ. BC) NVE = NVE - 1                                  00001710
       IF(CASEB .EQ. BA) CTENDB = 1                                     00001720
       IF(CASEB .EQ. BB) CTENDB = 1                                     00001730
       IF(CASEB .EQ. BC) CTENDB = 1                                     00001740
C                                                                       00001750
C     F.  FIND THE CODE(30,31,32,33,34,35) LOCATIONS.                   00001760
C                                                                       00001770
  32  XACCVE = 0.0                                                      00001780
      XBCCVE = 0.0                                                      00001790
      XCCCVE = 0.0                                                      00001800
C                                                                       00001810
C         NUMBER OF VERT. STIFF. (NVE)                                  00001820
C                                                                       00001830
      GO TO (41,42,43,44,45,46,47), NVE                                 00001840
C                                                                       00001850
  41  GO TO 50                                                          00001860
  42  IF(CASEB .EQ. BA) GO TO 43                                        00001870
      IF(CASEB .EQ. BB) GO TO 43                                        00001880
      IF(CASEB .EQ. BC) GO TO 43                                        00001890
      XACCVE = CCVE / 2.0                                               00001900
      GO TO 50                                                          00001910
  43  XACCVE = CCVE                                                     00001920
      GO TO 50                                                          00001930
  44  IF(CASEB .EQ. BA) GO TO 45                                        00001940
      IF(CASEB .EQ. BB) GO TO 45                                        00001950
      IF(CASEB .EQ. BC) GO TO 45                                        00001960
      XACCVE = CCVE / 2.0                                               00001970
      XBCCVE = CCVE                                                     00001980
      GO TO 50                                                          00001990
  45  XACCVE = CCVE                                                     00002000
      XBCCVE = CCVE                                                     00002010
      GO TO 50                                                          00002020
  46  IF(CASEB .EQ. BA) GO TO 47                                        00002030
      IF(CASEB .EQ. BB) GO TO 47                                        00002040
      IF(CASEB .EQ. BC) GO TO 47                                        00002050
      XACCVE = CCVE / 2.0                                               00002060
      XBCCVE = CCVE                                                     00002070
      XCCCVE = CCVE                                                     00002080
      GO TO 50                                                          00002090
  47  XACCVE = CCVE                                                     00002100
      XBCCVE = CCVE                                                     00002110
      XCCCVE = CCVE                                                     00002120
C                                                                       00002130
C                                                                       00002140
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00002150
C     THIS SUBROUTINE COMPUTES THE VERTICAL STIFFENERS                 C00002160
C          FOR SIDE INDUCTED DRAFT HOOD PANELS.                        C00002170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00002180
C                                                                       00002190
C                                                                       00002200
C        A. VERTICAL SPANE LENGTH (VSPANE) FOR SIDE PANEL               00002210
C           (FROM MAIN PROGRAM)                                         00002220
C                                                                       00002230
C        B. DETERMINE THE MAXIMUM CENTER-TO-CENTER                      00002240
C           DISTANCE BETWEEN STIFFENERS.                                00002250
C                                                                       00002260
C         CCVS = CENTER-TO-CENTER DISTANCE BEWTEEN                      00002270
C                VERTICAL STIFFENERS.                                   00002280
C                                                                       00002290
C                                                                       00002300
C RJD 6/16/93 Code to get CCVE value from the array "PPS"

C           Set CCVS to 24.0 - 6.0 so that we can calculate it
C           by looping through the table.
  50  CCVS=18.0

C           Calculate the index value for the given wind laod.
      WLI=INT((WINDL - 25)/ 5.0)
      IF(WLI .LT. 1) WLI=1
      IF(WLI .GT. 15) WLI=15

C           Loop through the table with the given wind load and
C           material type. Add 6 inches to each time to get the
C           values (24, 30, 36, 42, 48).
      I=1
      WHILE ( (VSPANE .LE. PPS(I, WLI, MTYPE)) .AND. (I .LT. 6))
         CCVS=CCVS + 6.0
         I=I+1
      END DO

C            If the span length was greater than the maximum value 
C            in the table then set the flag. (issue warning later).
      IF (CCVS .EQ. 18.0) THEN
         CCVSbad=.TRUE.
	 CCVS=24.0
	 WRITE(NWTR,*) "VSPANE OUT OF RANGE!!! Use larger material!!"
      ELSE
         CCVSbad=.FALSE.
      END IF
	 
C                                                                       00002950
C       D. CALCULATE MAX. HORIZ. SPAN DISTANCE (VNS)                    00002960
C          FOR STIFFENER PLACEMENTS                                     00002970
C                                                                       00002980
  130 SVCCVS = CCVS                                                     00002990
      VNS = F - 15.00                                                   00003000
C                                                                       00003010
C       E. TOTAL QUANTITY OF VERTICAL STIFFENERS (NVS) PER              00003020
C          SIDE PANEL.                                                  00003030
C                                                                       00003040
       NVS = VNS / CCVS                                                 00003050
       TNVS = VNS / CCVS                                                00003060
       TNVS = TNVS - NVS                                                00003070
       IF (TNVS .GT. 0.0) NVS = NVS + 1                                 00003080
C                                                                       00003090
      IF(NVS .LE. 0) GO TO 150                                          00003100
C                                                                       00003110
C        F. CALCULATE ACTUAL CENTER-TO-CENTER (HORIZ.)                  00003120
C           DISTANCE (CCVS) BETWEEN THE STIFFENERS FOR                  00003130
C           SIDE PANELS                                                 00003140
C                                                                       00003150
      CCVS = VNS / NVS                                                  00003160
      IF(CCVS .GT. SVCCVS) CCVS = SVCCVS                                00003170
C                                                                       00003180
C        ROUND DOWN 1/8" (WHERE N IS INTEGER)                           00003190
C                                                                       00003200
      N = CCVS * 8                                                      00003210
      CCVS = 0.125 * N                                                  00003220
C                                                                       00003230
      IF(NVS .EQ. 1) GO TO 131                                          00003240
      IF(NVS .EQ. 3) GO TO 131                                          00003250
      IF(NVS .EQ. 5) GO TO 131                                          00003260
      IF(NVS .EQ. 7) GO TO 131                                          00003270
      GO TO 132                                                         00003280
  131 IF (CASEC .EQ. D)   NVS = NVS - 1                                 00003290
      IF (CASEC .EQ. D)   SIDEB = 1                                     00003300
C                                                                       00003310
C      G. FIND THE CODE(30,31,32) LOCATIONS.                            00003320
C                                                                       00003330
  132 XACCVS = 0.0                                                      00003340
      XBCCVS = 0.0                                                      00003350
      XCCCVS = 0.0                                                      00003360
C                                                                       00003370
C         NUMBER OF VERT. STIFF. (NVS)                                  00003380
C                                                                       00003390
      GO TO (141,142,143,144,145,146,147), NVS                          00003400
C                                                                       00003410
 141  GO TO 150                                                         00003420
 142  IF (CASEC .EQ. D) GO TO 143                                       00003430
      XACCVS = CCVS / 2.0                                               00003440
      GO TO 150                                                         00003450
 143  XACCVS = CCVS                                                     00003460
      GO TO 150                                                         00003470
 144  IF (CASEC .EQ. D) GO TO 145                                       00003480
      XACCVS = CCVS / 2.0                                               00003490
      XBCCVS = CCVS                                                     00003500
      GO TO 150                                                         00003510
 145  XACCVS = CCVS                                                     00003520
      XBCCVS = CCVS                                                     00003530
      GO TO 150                                                         00003540
 146  IF (CASEC .EQ. D) GO TO 147                                       00003550
      XACCVS = CCVS / 2.0                                               00003560
      XBCCVS = CCVS                                                     00003570
      XCCCVS = CCVS                                                     00003580
      GO TO 150                                                         00003590
 147  XACCVS = CCVS                                                     00003600
      XBCCVS = CCVS                                                     00003610
      XCCCVS = CCVS                                                     00003620
C                                                                       00003630
C                                                                       00003640
 150  IF(NUMBER(5) .LE. 0) GO TO 162                                    00003650
      GO TO 171                                                         00003660
C                                                                       00003670
C       H. SUB-TOTAL NUMBER OF BOLTS REQUIRED FOR SHOP OR FIELD         00003680
C          ASSEMBLY, WHICHEVER APPLIES                                  00003690
C                                                                       00003700
  162 IVE = IVS                                                         00003710
      IVS = 0                                                           00003720
      VSQTY = 0                                                         00003730
      VSBOLT = 0                                                        00003740
C                                                                       00003750
C       I. SUB-TOTAL NUMBER OF BOLTS REQUIRED FOR SHOP OR FIELD         00003760
C          ASSEMBLY, WHICHEVER APPLIEX                                  00003770
C                                                                       00003780
       GO TO (301,302,303), NOFAN                                       00003790
  301  VEQTY = (NVE + NVS) * 2.0 * LUNITS                               00003800
       GO TO 304                                                        00003810
  302  VEQTY = (NVE * (2.0 * NOFAN)) + (NVS * (2.0 * NOFAN))            00003820
       IF (NUMBER(11) .GT. 0) VEQTY = VEQTY - NVE                       00003830
       VEQTY = VEQTY * LUNITS                                           00003840
       GO TO 304                                                        00003850
  303  VEQTY = (NVE * (2.0 * NOFAN)) + (NVS * (2.0 * NOFAN))            00003860
       IF (NUMBER(11) .GT. 0) VEQTY = VEQTY - (NVE * 2)                 00003870
       VEQTY = VEQTY * LUNITS                                           00003880
  304 VSQTY = 0                                                         00003890
      SVBOLT = 0                                                        00003900
      EVBOLT = VEQTY * (IVE + 1)                                        00003910
      IF(CTENDB .LE. 0) GO TO 191                                       00003920
      EVBOLT = EVBOLT + ((IVE + 1) * LUNITS * (2.0 * NOFAN))            00003930
  191 IF(SIDEB .LE. 0) GO TO 192                                        00003940
      EVBOLT = EVBOLT + ((IVE + 1) * LUNITS * (2.0 * NOFAN))            00003950
      GO TO 193                                                         00003960
  192 IF (NUMBER(11) .GT. 0) GO TO 591                                  00003970
      EVBOLT = EVBOLT - ((IVE + 1) * LUNITS * (4.0 * NOFAN))            00003980
      GO TO 193                                                         00003990
  591 GO TO (501,502,503), NOFAN                                        00004000
  501 EVBOLT = EVBOLT - ((IVE + 1) * LUNITS * 4.0)                      00004010
      GO TO 193                                                         00004020
  502 EVBOLT=EVBOLT-(((IVE+1)*LUNITS)* (3.0 * NOFAN))-((IVE+1)*LUNITS)  00004030
      GO TO 193                                                         00004040
  503 EVBOLT=EVBOLT-(((IVE+1)*LUNITS)*(3.0 *NOFAN))-(((IVE+1)*LUNITS)*2)00004050
  193 CONTINUE                                                          00004060
      GO TO 9999                                                        00004070
C                                                                       00004080
  171  GO TO (401,402,403), NOFAN                                       00004090
  401  VEQTY = NVE * LUNITS * 2.0                                       00004100
       VSQTY = NVS * LUNITS * 2.0                                       00004110
       GO TO 404                                                        00004120
  402  VEQTY =  NVE * (2.0 * NOFAN)                                     00004130
       VSQTY =  NVS * (2.0 * NOFAN)                                     00004140
       IF (NUMBER(11) .GT. 0) VEQTY = VEQTY - NVE                       00004150
       VEQTY = VEQTY * LUNITS                                           00004160
       VSQTY = VSQTY * LUNITS                                           00004170
       GO TO 404                                                        00004180
  403  VEQTY =  NVE * (2.0 * NOFAN)                                     00004190
       VSQTY =  NVS * (2.0 * NOFAN)                                     00004200
       IF (NUMBER(11) .GT. 0) VEQTY = VEQTY - (NVE * 2)                 00004210
       VEQTY = VEQTY * LUNITS                                           00004220
       VEQTY = VSQTY * LUNITS                                           00004230
  404 SVBOLT = VSQTY * (IVS + 1)                                        00004240
      EVBOLT = VEQTY * (IVE + 1)                                        00004250
      IF(CTENDB .LE. 0) GO TO 291                                       00004260
      EVBOLT = EVBOLT + ((IVE + 1) * LUNITS * (2.0 * NOFAN))            00004270
  291 IF(SIDEB .LE. 0) GO TO 292                                        00004280
      SVBOLT = SVBOLT + ((IVS + 1) * LUNITS * (2.0 * NOFAN))            00004290
C     GO TO 9999                                                        00004300
  292 IF (NUMBER(11) .GT. 0) GO TO 691                                  00004310
      EVBOLT = EVBOLT - ((IVE + 1) * LUNITS * (2.0 * NOFAN))            00004320
      GO TO 693                                                         00004330
  691 GO TO (601,602,603), NOFAN                                        00004340
  601 EVBOLT = EVBOLT - ((IVE + 1) * LUNITS * 2.0)                      00004350
      GO TO 693                                                         00004360
  602 EVBOLT=EVBOLT- (((IVE+1)*LUNITS)* 2.0)-((IVE+1)*LUNITS)           00004370
      GO TO 693                                                         00004380
  603 EVBOLT=EVBOLT-(((IVE+1)*LUNITS)* 2.0)-(((IVE+1)*LUNITS)*2)        00004390
  693 SVBOLT = SVBOLT - ((IVS + 1) * LUNITS * (2.0 * NOFAN))            00004400
C                                                                       00004410
 9999 CONTINUE                                                          00004420
C                                                                       00004430
C                                                                       00004440
C                                                                       00004450
      RETURN                                                            00004460
      END                                                               00004470
