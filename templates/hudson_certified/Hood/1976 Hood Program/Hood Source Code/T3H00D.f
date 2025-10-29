C          DATA SET T3H00D     AT LEVEL 005 AS OF 10/08/76              00000000
      SUBROUTINE T3H00D(XLIMIT,XNUMER,ID,XD,ND,STD)                     00000010
      COMMON IRUN, IDATEX(3)                                             00000020
      COMMON/MARKNO/NUMBER(21),XWIDE(21),XLONG(21),LBS(21)              00000030
      COMMON/INPUT/JOBNO,IDRAW,NOFAN,H1,F,ISHIP,LUNITS,WIDTH(4),SPACE(3)00000040
     *,INTS                                                             00000050
      COMMON/HEAD/NPAGE,NPAGES,MARK,MARK2,MARK3,MCLMP1,MCLMP2,LINE,KOUNT00000060
      COMMON/ARRAY/K1,K2,X(100),Y(100),XBLOC(20),YBLOC(20)              00000070
      COMMON/INOUT/NRDR,NWTR                                            00000080
      DO 100 ID = 1,10                                                  00000090
      XD = ID                                                           00000100
      IF ((XNUMER/XD) .LE. XLIMIT)  GO TO 105                           00000110
  100 CONTINUE                                                          00000120
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000130
C  THIS SUBROUTINE DETERMINES THE LEAST NO. OF BOLT SPACES & AMT. OF EAC00000140
C        XLIMIT = THE MAX. ALLOWABLE DISTANCE FOR EACH SPACE            00000150
C        XNUMER = SOME UNKNOWN DISTANCE BETWEEN EXTREMES.  AFTER EXECUTI00000160
C             THE VALUE RETURNED IS THE DISTANCE FOR EACH SPACE.        00000170
C        ID = THE VALUE RETURNED IS THE TOTAL NO. OF EQUAL SPACES REQ'D.00000180
C        STD = THE STANDARD FORM OF MEASUREMENT FOR ROUNDING "XNUMER" . 00000190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000200
  105 XNUMER = XNUMER / XD                                              00000210
      IF (STD .EQ. 0.0)  RETURN                                         00000220
      ND = XNUMER / STD                                                 00000230
      XNUMER = STD * ND                                                 00000240
      RETURN                                                            00000250
      END                                                               00000260
