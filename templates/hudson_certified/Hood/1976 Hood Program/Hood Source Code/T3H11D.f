C          DATA SET T3H11D     AT LEVEL 004 AS OF 03/20/81              00000000
C          DATA SET T3H11D     AT LEVEL 003 AS OF 02/20/81              00000010
C          DATA SET T3H11D     AT LEVEL 002 AS OF 02/20/81              00000020
C          DATA SET T3H11D     AT LEVEL 001 AS OF 08/26/76              00000030
      SUBROUTINE T3H11D(I,J,II,IGO,IX,IY,IXY,M1,M2,M3,JRMK)             00000040
      COMMON IRUN, IDATEX(3)                                             00000050
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000060
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000070
     *,INTS                                                             00000080
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000090
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000100
      COMMON/INOUT/NRDR,NWTR                                            00000110     
C     CHARACTER NTEGER*4 ,NFRMT*4 
      DIMENSION NFRMT(22), XYZ(2), NTEGER(10),IRMK(24), MCODE(14)       00000120  
      DATA NTEGER/'1   ','2   ','3   ','4   ','5   ','6   ','7   ',     00000130
     & 'I   ','9   ',' A1,'/                                            00000140
      DATA NFRMT/'( / ',',15X',',I3,',' 4X,','    ','    ','    ',      00000150
     1'    ','    ','X,  ','    ','    ','    ','    ','    ','X,A3',   00000160
     2',3X,','A3,3','X,A3',',18X',',3A4',')   '/                        00000170

C      DATA NTEGER/'   1','   2','   3','   4','   5','   6','   7',    00000130
C     & '   I','   9',' A1,'/                                           00000140
C      DATA NFRMT/'(''0''',',15X',',I3,',' 4X,','    ','    ','    ',   00000150
C     1'  , ','    ','X,  ','    ','    ','    ','  , ','    ','X,A3',  00000160
C     2',3X,','A3,3','X,A3',',18X',',3A4',')   '/                       00000170
     
      DATA XYZ/'X','Y'/                                                 00000180
      DATA MCODE/'   ',                                                 00000190
     2           'M87',                                                 00000200
     3           'M06',                                                 00000210
     4           'M85',                                                 00000220
     5           'M71',                                                 00000230
     6           'M75',                                                 00000240
     7           'M00',                                                 00000250
     8           'M99',                                                 00000260
     9           'M73',                                                 00000270
     A           'M76',                                                 00000280
     B           'M77',                                                 00000290
     C           'T11',                                                 00000300
     D           'T46',                                                 00000310
     E           'T47'/                                                 00000320
      DATA IRMK/ '    ','    ','    ',                                  00000330
     2           'TURN',' 180',' DEG',                                  00000340
     3           'TURN',' 90 ','DEG.',                                  00000350
     4           '1"X2','"  R','ECT.',                                  00000360
     5           '2"  ','SQUA','RE  ',                                  00000370
     6           '11/1','6"  ','0   ',                                  00000380
     7           '    ','2"  ','0   ',                                  00000390
     8           '    ','4"  ','0   '/                                  00000400
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000410
C        THIS SUBROUTINE WILL WRITE EVERY LINE (EXCEPT THE HEADING LINES00000420
C        THE PANEL-MASTER PROGRAM SHEET.   THE BASIC AREAS OF THE LINE A00000430
C             1)  WILL KEEP RUNNING COUNT ON THE "BLOCK" NO.'S AND PAGE 00000440
C             2)  WILL PROVIDE VARIABLE INTEGER FORMAT FOR EACH 'X' AND/00000450
C                 (IGO MUST BE PRE-DETERMINED BEFORE ENTRY)             00000460
C             3)  WILL WRITE ANY OF THE 'M' COMMAND FUNCTIONS, IF REQ'D 00000470
C             4)  WILL WRITE THE 'REMARKS' PERTAINING TO TOOL INFO. OR O00000480
C                 CONTROL FOR COMMUNICATING TO THE MACHINE OPERATOR, IF 00000490
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000500
C   PRE-CODES & RANGE                                      X       Y    00000510
C   --------- - -----                                     ---     ---   00000520
C   IX = -63000 TO 63000                       IGO = 1    NONE    NONE  00000530
C   IY = -48000 TO 48000                       IGO = 2    'X'     NONE  00000540
C   M1,M2,M3 = 1 TO 14                         IGO = 3    NONE    'Y'   00000550
C   JRMK = 1 TO 8                              IGO = 4    'X'     'Y'   00000560
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000570
C     NWDISK=7                                                          00000580
 
C RJD 8/4/92 Disabling this routine because the data is no longer needed. 
      RETURN                                                            


      NWDISK=NWTR                                                       00000580
      LINE = LINE + 1                                                   00000590
      KOUNT = KOUNT + 1                                                 00000600
      IF (LINE .GE. 23) CALL T3H10D                                     00000610
      J = 5                                                             00000620
      GO TO (10,30,10,30) , IGO                                         00000630
   10 DO 20 I = 1,3                                                     00000640
      NFRMT(J+I-1) = IRMK(I)                                            00000650
   20 CONTINUE                                                          00000660
      NFRMT(J+4) = NTEGER(9)                                            00000670
      IF (J .EQ. 11) GO TO 100                                          00000680
      GO TO 90                                                          00000690
   30 IXY = IX                                                          00000700
   40 DO 50 I = 1,6                                                     00000710
      IF ((IABS(IXY)) .LT. (10**I))  GO TO 60                           00000720
   50 CONTINUE                                                          00000730
   60 II = I                                                            00000740
      IF (IXY .LT. 0) II = II + 1                                       00000750
      NFRMT(J) = NTEGER(10)                                             00000760
      NFRMT(J+1) = NTEGER(8)                                            00000770
      NFRMT(J+2) = NTEGER(II)                                           00000780
      NFRMT(J+4) = NTEGER(8-II)                                         00000790
C      WRITE(NWTR,999) 'NFRMT=',NFRMT
C      WRITE(9,999) 'NFRMT=',NFRMT
      IF (J .EQ. 11) GO TO 100                                          00000800
   90 J = 11                                                            00000810
      IF (IGO .LE. 2) GO TO 10                                          00000820
      IXY = IY                                                          00000830
      GO TO 40                                                          00000840
  100 II = JRMK * 3                                                     00000850
      J = II - 2                                                        00000860
      GO TO ( 110, 120, 120, 140), IGO                                  00000870
  110 WRITE(NWTR,NFRMT)KOUNT,MCODE(M1), MCODE(M2), MCODE(M3), (IRMK(I), 00000880
     & I = J,II)                                                        00000890
C      WRITE(NWDISK,160)KOUNT,MCODE(M1), MCODE(M2), MCODE(M3)            00000900
C  160 FORMAT(1X,I2,17X,                 A3,1X,A3,1X,A3,49X)             00000910
      GO TO 150                                                         00000920
  120 WRITE(NWTR,NFRMT)KOUNT,XYZ(IGO-1), IXY, MCODE(M1), MCODE(M2),     00000930
     & MCODE(M3), (IRMK(I), I = J,II)                                   00000940
C      IF (IGO .EQ. 2)  WRITE(NWDISK,180)KOUNT, XYZ(IGO-1), IXY,         00000950
C     & MCODE(M1), MCODE(M2), MCODE(M3)                                  00000960
C  180 FORMAT(1X,I2,1X,A1,I6,9X,         A3,1X,A3,1X,A3,49X)             00000970
C      IF (IGO .EQ. 3)  WRITE(NWDISK,190)KOUNT, XYZ(IGO-1), IXY,         00000980
C     & MCODE(M1), MCODE(M2), MCODE(M3)                                  00000990
C  190 FORMAT(1X,I2,9X,         A1,I6,1X,A3,1X,A3,1X,A3,49X)             00001000
      GO TO 150                                                         00001010
  140 WRITE(NWTR,NFRMT)KOUNT,XYZ(1), IX, XYZ(2), IY, MCODE(M1), MCODE(M200001020
     &), MCODE(M3), (IRMK(I), I = J,II)                                 00001030
C      WRITE(NWDISK,170)KOUNT,XYZ(1), IX, XYZ(2), IY, MCODE(M1), MCODE(M200001040
C     &), MCODE(M3)                                                      00001050
C  170 FORMAT(1X,I2,1X,A1,I6,1X,A1,I6,1X,A3,1X,A3,1X,A3,49X)             00001060
  150 IF (JRMK .GE. 6) WRITE (NWTR,155)                                 00001070
  155 FORMAT('+',81X,'/')                                               00001080
  999 FORMAT(22A4)
      RETURN                                                            00001090
      END                                                               00001100
