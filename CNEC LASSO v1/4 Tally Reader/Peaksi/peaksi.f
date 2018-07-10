C                      Main Program PEAKSI.F
C                        Revised: 2/17/09
C                       
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(25,17000),Y(17000),SIGMAY(17000),A(25),AU(25),AL(25)
      DIMENSION XX(25,17000),YY(17000),BG(17000)
      DIMENSION DELTAA(25),YFIT(17000),SIGMAA(25),R(25)
      DIMENSION XINT(10,17000)
      DIMENSION RES(17000)
      COMMON/BLOCKA/NOPEAK,NBG,NOPT,NTYPE
      COMMON/BLOCKB/CON,A1,A2,SIG,DEL
      COMMON/BLOCKC/C1,C3
      CHARACTER*20 FILE1
      ZERO=0.0D+00
      HALF=0.5D+00
      ONE=1.0D+00
      TWO=2.0D+00
      PI=TWO*DACOS(ZERO)
      C1=ONE/DSQRT(TWO)
      C3=TWO/DSQRT(PI)
      CON=DSQRT(TWO*PI)
      WRITE(*,*)'Revised 3/31/00'
      WRITE(*,*)''
      WRITE(*,*)'This program (PEAKSI.F) obtains a fit with'
      WRITE(*,*)'experimental data for either a single resolved'
      WRITE(*,*)'Gaussian peak or two unresolved Gaussian peaks'
      WRITE(*,*)'plus a constant, linear, or quadratic'
      WRITE(*,*)'background.  Either a differential or integrated'
      WRITE(*,*)'Gaussian for each channel can be employed (the'
	WRITE(*,*)'integrated is more accurate). The peak channel(s)'
	WRITE(*,*)'and resolution can either be fixed or searched'
      WRITE(*,*)'on. The data file must have pairs of channel' 
      WRITE(*,*)'number and counts per channel on all lines.'
      WRITE(*,*)'Both the experimental data and a comparison of'
      WRITE(*,*)'the experimental data and the derived fit can'
      WRITE(*,*)'be plotted and looked at in MATLAB.  The m'
      WRITE(*,*)'files can be examined by inputting 1 now.'
      READ(*,*)N23
      IF(N23.EQ.1)THEN
        CALL MFILES
        ENDIF
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N1
 9    CONTINUE
      WRITE(*,*)'What is the name of the data file FILE1?'
      READ(*,101)FILE1
 101  FORMAT(A20)
      NCOLS=2
      CALL FLERDN(FILE1,NOCHAN,NCOLS,XINT)
      DO 10 I=1,NOCHAN
       XX(1,I)=XINT(1,I)
       YY(I)=XINT(2,I)
       WRITE(*,*)'XX(1,I)=',XX(1,I),' YY(I)=',YY(I)
 10    CONTINUE
 11    CONTINUE
      WRITE(*,*)'What are the minimum and maximum channel numbers'
      WRITE(*,*)'of the range to be examined?'
      READ(*,*)IMIN,IMAX
      WRITE(*,*)'To plot input 1 - otherwise 0.'
      READ(*,*)N11
      IF(N11.EQ.1)THEN
        WRITE(*,*)'The data is stored in a file named peaksi1.plt.'
        OPEN(UNIT=1,FILE='peaksi1.plt',STATUS='UNKNOWN')
        DO 169 I=IMIN,IMAX
          WRITE(*,*)'XX(1,I)=',XX(1,I),' YY(I)=',YY(I)
          WRITE(1,*)XX(1,I),YY(I)
 169      CONTINUE
          CLOSE(UNIT=1)
        ENDIF
      WRITE(*,*)'Use peak1.m in MATLAB to see plot.'
      WRITE(*,*)'What are the minimum and maximum channel numbers'
      WRITE(*,*)'to be used in the peak(s) determination?'
      READ(*,*)IMIN,IMAX
      DO 16 I=IMIN,IMAX
        IC=I-IMIN+1
        Y(IC)=YY(I)
        X(1,IC)=XX(1,I)
        WRITE(*,162)IC,X(1,IC),Y(IC)
 162    FORMAT(2X,'IC=',I5,' X(1,IC)=',E14.6,' Y(IC)=',E14.6)
 16     CONTINUE
      WRITE(*,*)'To use one peak input 1 - to use two peaks input 2.'
      READ(*,*)NOPEAK
 17   CONTINUE
      WRITE(*,*)'The options are:'
      WRITE(*,*)'    1. Search on the peak(s) and standard deviation.'
      WRITE(*,*)'    2. Search on the peak(s) and fix the standard'
      WRITE(*,*)'       deviation.'
      WRITE(*,*)'    3. Search on one peak and fix the difference'
      WRITE(*,*)'       between peaks and the standard deviation.'
      WRITE(*,*)'    4. Fix the peak(s) and search on the standard'
      WRITE(*,*)'       deviation.'
      WRITE(*,*)'    5. Fix the peak(s) and standard deviation.'
      WRITE(*,*)' '
      WRITE(*,*)'What is the desired option - 1, 2, 3, or 4? '
      READ(*,*)NOPT
      IF(NOPT.LT.1.AND.NOPT.GT.5)GO TO 17
      IF(NOPT.EQ.1.AND.NOPEAK.EQ.1)THEN
        NSEARC=2
        WRITE(*,*)'What is the estimate of the single peak and the'
        WRITE(*,*)'standard deviation?'
        READ(*,*)A(1),A(2)
        ENDIF
      IF(NOPT.EQ.1.AND.NOPEAK.EQ.2)THEN
        NSEARC=3
        WRITE(*,*)'What is the estimate of the two peaks and the'
        WRITE(*,*)'standard deviation?'
        READ(*,*)A(1),A(2),A(3)
        ENDIF
      IF(NOPT.EQ.2.AND.NOPEAK.EQ.1)THEN
        NSEARC=1
        WRITE(*,*)'What is the estimate of the single peak and the'
        WRITE(*,*)'fixed standard deviation?'
        READ(*,*)A(1),SIG
        ENDIF
      IF(NOPT.EQ.2.AND.NOPEAK.EQ.2)THEN
        NSEARC=2
        WRITE(*,*)'What is the estimate of the two peaks and the'
        WRITE(*,*)'fixed standard deviation?'
        READ(*,*)A(1),A(2),SIG
        ENDIF
      IF(NOPT.EQ.3)THEN
        NSEARC=1
        WRITE(*,*)'What is the estimate of the lowest energy peak'
        WRITE(*,*)'and the fixed difference between peaks and the'
        WRITE(*,*)'fixed standard deviation?'
        READ(*,*)A(1),DEL,SIG
        ENDIF
      IF(NOPT.EQ.4.AND.NOPEAK.EQ.1)THEN
        NSEARC=1
        WRITE(*,*)'What is the fixed value of the peak and the'
        WRITE(*,*)'estimate of the standard deviation?'
        READ(*,*)A1,A(1)
        ENDIF
      IF(NOPT.EQ.4.AND.NOPEAK.EQ.2)THEN
        NSEARC=1
        WRITE(*,*)'What are the fixed values of the two peaks and'
        WRITE(*,*)'the estimate of the standard deviation?'
        READ(*,*)A1,A2,A(1)
        ENDIF
      IF(NOPT.EQ.5.AND.NOPEAK.EQ.1)THEN
        NSEARC=0
        WRITE(*,*)'What is the fixed value of the single peak and'
        WRITE(*,*)'the standard deviation?'
        READ(*,*)A1,SIG
        ENDIF
      IF(NOPT.EQ.5.AND.NOPEAK.EQ.2)THEN
        NSEARC=0
        WRITE(*,*)'What are the fixed values of the two peaks and'
        WRITE(*,*)'the standard deviation?'
        READ(*,*)A1,A2,SIG
        ENDIF
C
      WRITE(*,*)'For a constant, linear, or quadratic background'
      WRITE(*,*)'input 1, 2, or 3, respectively.'
      READ(*,*)NBG
      NLS=NOPEAK+NBG
      WRITE(*,*)'NLS=',NLS
      WRITE(*,*)'For a differential Gaussian input 1, for an'
      WRITE(*,*)'integrated Gaussian input 2. The integrated'
	WRITE(*,*)'Gaussian is the most accurate.'
      READ(*,*)NTYPE
      IC=10
      IPRIN=1
      ILIM=0
      INCMAX=10
      CHICUT=0.01
      FLAMDE=0.001
      IF(NSEARC.GE.1)THEN
        DO 6 I=1,NSEARC
          DELTAA(I)=0.001*A(I)
 6       CONTINUE
         ENDIF
      NPTS=IMAX-IMIN+1
      
C
      WRITE(*,*)'MODE=+1: weighting uses SIGMAY(I)'
      WRITE(*,*)'MODE= 0: weighting is unity'
      WRITE(*,*)'MODE=-1: weighting is Poisson'
      WRITE(*,*)'What is MODE?  (-1 is suggested.)'
      READ(*,*)MODE
C
      CALL CURMOD(IC,CHICUT,X,Y,SIGMAY,NPTS,NSEARC,NLS,MODE,
     *  IPRIN,ILIM,INCMAX,NDERIV,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR,
     *  CHIZRO,RELRAT,SIGMAA,R)
      DO 198 I=1,NPTS
	IF(Y(I).LT.ZERO)SIGMAY(I)=DSQRT(-Y(I))
	IF(Y(I).EQ.ZERO)SIGMAY(I)=ONE
	IF(Y(I).GT.ZERO)SIGMAY(I)=DSQRT(Y(I))
        RES(I)=(Y(I)-YFIT(I))/SIGMAY(I)
        WRITE(*,*)'I=',I,' Y(I)=',Y(I),' RES(I)=',RES(I)
 198    CONTINUE
      NTERMS=NSEARC+NLS
      DO 199 I=1,NTERMS
        SIGMAA(I)=100.E+00*SIGMAA(I)/A(I)
 199    CONTINUE
	OPEN(UNIT=1,FILE='peaksi.out',STATUS='UNKNOWN')
      WRITE(*,*)'This is data from Program PEAKSI.'
	WRITE(1,*)'This is data from Program PEAKSI.'
      WRITE(*,*)'This is data from file:'
      WRITE(1,*)'This is data from file:'
      WRITE(*,101)FILE1
      WRITE(*,*)'CHISQR,CHIZRO,MODE=',CHISQR,CHIZRO,MODE
      WRITE(*,*)'SIGMAA(I) is in %.'
      WRITE(*,*)'I, A(I), SIGMAA(I) in %, and R(I) are:'
      WRITE(1,101)FILE1
      WRITE(1,*)'CHISQR,CHIZRO,MODE=',CHISQR,CHIZRO,MODE
      WRITE(1,*)'SIGMAA(I) is in %.'
      WRITE(1,*)'I, A(I), SIGMAA(I) in %, and R(I) are:'
      DO 21 I=1,NTERMS
        WRITE(*,108)I,A(I),SIGMAA(I),R(I)
        WRITE(1,108)I,A(I),SIGMAA(I),R(I)
 108    FORMAT(1X,I5,2(3X,E12.6),' %',2X,E11.4)
 21     CONTINUE
	CLOSE(UNIT=1)
      WRITE(*,*)'To continue print out input 1.'
      READ(*,*)N1
      WRITE(*,*)'RES(I) is RES(I)/SIGMAY(I).'
      WRITE(*,*)'I,X(1,I),Y(I),YFIT(I),RES(I) are:'
      DO 161 I=1,NPTS
        WRITE(*,110)I,X(1,I),Y(I),YFIT(I),RES(I)
 110    FORMAT(1X,I5,5(2X,E12.6))
 161  CONTINUE
      DO 163 I=IMIN,IMAX
        XI=I
	IF(NSEARC.EQ.2)THEN
          IF(NLS.EQ.2)BG(I)=A(4)
          IF(NLS.EQ.3)BG(I)=A(4)+A(5)*XI
          IF(NLS.EQ.4)BG(I)=A(4)+A(5)*XI+A(6)*XI*XI
	  ENDIF
	IF(NSEARC.EQ.3)THEN
	  IF(NLS.EQ.3)BG(I)=A(6)
	  IF(NLS.EQ.4)BG(I)=A(6)+A(7)*XI
	  IF(NLS.EQ.5)BG(I)=A(6)+A(7)*XI+A(8)*XI*XI
	  ENDIF
    	IF(NSEARC.EQ.0)THEN
	  IF(NOPEAK.EQ.1.AND.NBG.EQ.1)BG(I)=A(2)
	  IF(NOPEAK.EQ.1.AND.NBG.EQ.2)BG(I)=A(2)+A(3)*XI
	  IF(NOPEAK.EQ.1.AND.NBG.EQ.3)BG(I)=A(2)+A(3)*XI+A(4)*XI*XI
	  IF(NOPEAK.EQ.2.AND.NBG.EQ.1)BG(I)=A(3)
	  IF(NOPEAK.EQ.2.AND.NBG.EQ.2)BG(I)=A(3)+A(4)*XI
	  IF(NOPEAK.EQ.2.AND.NBG.EQ.3)BG(I)=A(3)+A(4)*XI+A(5)*XI*XI
	  ENDIF
 163    CONTINUE
     	BGTOT=ZERO
      	DO 165 I=IMIN,IMAX
	BGTOT=BGTOT+BG(I)
 165    CONTINUE
      IF(NSEARC.EQ.2)WRITE(*,*)'The net peak area is:',A(3),
     * ' +/- ',SIGMAA(3),' %'     
      IF(NSEARC.EQ.3)WRITE(*,*)'The net area of peak 1 is:',A(4),
     * '+/-',SIGMAA(4),' %'
      IF(NSEARC.EQ.3)WRITE(*,*)'The net area of peak 2 is:',A(5),
     * '+/-',SIGMAA(5),' %'
      WRITE(*,*)'The total background counts in the channels'
      WRITE(*,*)'chosen are:',BGTOT
      WRITE(*,*)'To see a plot of the model vs. the data input 1.'
      READ(*,*)N72
      IF(N72.EQ.1)THEN
        WRITE(*,*)'The plotting data is stored in peaksi2.plt.'
        OPEN(UNIT=2,FILE='peaksi2.plt',STATUS='UNKNOWN')
        DO 179 I=1,NPTS
          WRITE(*,*)'X(1,I)=',X(1,I),' Y(I)=',Y(I),' YFIT(I)=',YFIT(I)
          WRITE(2,*)X(1,I),Y(I),YFIT(I),BG(I) 
 179      CONTINUE
          CLOSE(UNIT=2)
        ENDIF
      WRITE(*,*)'To see the plot use peak2.m in MATLAB.'
      WRITE(*,*)'For another peak input 1 - otherwise 0.'
      READ(*,*)N1
      IF(N1.EQ.1)GO TO 11
      WRITE(*,*)'For another file input 1 - otherwise 0.'
      READ(*,*)N1
      IF(N1.EQ.1)GO TO 9
	WRITE(*,*)'If you would like to keep a record of the output'
	WRITE(*,*)'files you should rename the files peaksi.out,'
	WRITE(*,*)'peaksi1.plt, and peaksi2.plt.'
      STOP
      END
C
C
      SUBROUTINE MODEL(X,I,A,ZI,YFI)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(25,17000),A(25),ZI(25)
      COMMON/BLOCKA/NOPEAK,NBG,NOPT,NTYPE
      COMMON/BLOCKB/CON,A1,A2,SIG,DEL
      ZERO=0.0D+00
      HALF=0.5D+00
      ONE=1.0D+00
      XX=X(1,I)
      NLS=NOPEAK+NBG
      IF(NOPT.EQ.1)GO TO 100
      IF(NOPT.EQ.2)GO TO 200
      IF(NOPT.EQ.3)GO TO 300
      IF(NOPT.EQ.4)GO TO 400
      IF(NOPT.EQ.5)GO TO 500
 100  CONTINUE
      IF(NOPEAK.EQ.1)GO TO 105
      IF(NOPEAK.EQ.2)GO TO 115
 105  CONTINUE
      NSEARC=2
      AVG=A(1)
      SIG=A(2)
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG,SIG,XINT)
        ZI(1)=XINT
        ENDIF
      ZI(2)=ONE
      ZI(3)=XX
      ZI(4)=XX*XX
      YFI=ZERO
      DO 110 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 110    CONTINUE
      RETURN
 115  CONTINUE
      NSEARC=3
      AVG1=A(1)
      AVG2=A(2)
      SIG=A(3)
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
        ZI(2)=DEXP(-HALF*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
        CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
        ZI(1)=XINT1
        ZI(2)=XINT2
        ENDIF
      ZI(3)=ONE
      ZI(4)=XX
      ZI(5)=XX*XX
      YFI=ZERO
      DO 120 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 120    CONTINUE
      RETURN
 200  CONTINUE
      IF(NOPEAK.EQ.1)GO TO 205
      IF(NOPEAK.EQ.2)GO TO 215
 205  CONTINUE
      NSEARC=1
      AVG=A(1)
      SIG=SIG
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG,SIG,XINT)
        ZI(1)=XINT
        ENDIF
      ZI(2)=ONE
      ZI(3)=XX
      ZI(4)=XX*XX
      YFI=ZERO
      DO 210 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 210    CONTINUE
      RETURN
 215  CONTINUE
      NSEARC=2
      AVG1=A(1)
      AVG2=A(2)
      SIG=SIG
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
        ZI(2)=DEXP(-HALF*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
        CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
        ZI(1)=XINT1
        ZI(2)=XINT2
        ENDIF
      ZI(3)=ONE
      ZI(4)=XX
      ZI(5)=XX*XX
      YFI=ZERO
      DO 220 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 220    CONTINUE
      RETURN
 300  CONTINUE
      NSEARC=1
      AVG1=A(1)
      AVG2=A(1)+DEL
      SIG=SIG
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
        ZI(2)=DEXP(-HALF*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
        CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
        ZI(1)=XINT1
        ZI(2)=XINT2
        ENDIF
      ZI(3)=ONE
      ZI(4)=XX
      ZI(5)=XX*XX
      YFI=ZERO
      DO 320 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 320    CONTINUE
      RETURN
 400  CONTINUE
      NSEARC=1
      IF(NOPEAK.EQ.1)GO TO 405
      IF(NOPEAK.EQ.2)GO TO 415
 405  CONTINUE
      AVG=A1
      SIG=A(1)
      IF(NTYPE.EQ.1)THEN
	ZI(1)=DEXP(-HALF*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
	XL=XX-HALF
	XU=XX+HALF
	CALL GAUSSM(XL,XU,AVG,SIG,XINT)
	ZI(1)=XINT
	ENDIF
      ZI(2)=ONE
      ZI(3)=XX
      ZI(4)=XX*XX
      YFI=ZERO
      DO 410 J=1,NLS
	YFI=YFI+A(NSEARC+J)*ZI(J)
 410  	CONTINUE
      RETURN
 415  CONTINUE
      AVG1=A1
      AVG2=A2
      SIG=A(1)
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
        ZI(2)=DEXP(-HALF*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
        CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
        ZI(1)=XINT1
        ZI(2)=XINT2
        ENDIF
      ZI(3)=ONE
      ZI(4)=XX
      ZI(5)=XX*XX
      YFI=ZERO
      DO 420 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 420    CONTINUE
      RETURN
 500  CONTINUE
      NSEARC=0
      IF(NOPEAK.EQ.1)GO TO 505
      IF(NOPEAK.EQ.2)GO TO 515
 505  CONTINUE
      AVG=A1
      SIG=SIG
      IF(NTYPE.EQ.1)THEN
	ZI(1)=DEXP(-HALF*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
	XL=XX-HALF
	XU=XX+HALF
	CALL GAUSSM(XL,XU,AVG,SIG,XINT)
	ZI(1)=XINT
	ENDIF
      ZI(2)=ONE
      ZI(3)=XX
      ZI(4)=XX*XX
      YFI=ZERO
      DO 510 J=1,NLS
	YFI=YFI+A(NSEARC+J)*ZI(J)
 510  	CONTINUE
      RETURN
 515  CONTINUE
      AVG1=A1
      AVG2=A2
      SIG=SIG
      IF(NTYPE.EQ.1)THEN
        ZI(1)=DEXP(-HALF*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
        ZI(2)=DEXP(-HALF*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
        ENDIF
      IF(NTYPE.EQ.2)THEN
        XL=XX-HALF
        XU=XX+HALF
        CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
        CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
        ZI(1)=XINT1
        ZI(2)=XINT2
        ENDIF
      ZI(3)=ONE
      ZI(4)=XX
      ZI(5)=XX*XX
      YFI=ZERO
      DO 520 J=1,NLS
        YFI=YFI+A(NSEARC+J)*ZI(J) 
 520    CONTINUE
      RETURN
      END
C
C
C             SUBROUTINE GAUSSM.F
C             (REVISED 2/27/94)
C
      SUBROUTINE GAUSSM(XL,XU,AVG,SIG,XINT)
C
C     This subroutine calculates the integral XINT from XL to XU of
C     a Gaussian distribution with mean AVG and standard deviation
C     SIG.
C
      IMPLICIT REAL*8(A-H,O-Z)
      TWO=2.0D+00
      CALL AGAUSS(XU,AVG,SIG,XI1)
      XI1=XI1/TWO
      CALL AGAUSS(XL,AVG,SIG,XI2)
      XI2=XI2/TWO
      IF(XL.LE.AVG.AND.XU.GE.AVG)XINT=XI1+XI2
      IF(XL.LE.AVG.AND.XU.LE.AVG)XINT=XI2-XI1
      IF(XL.GE.AVG.AND.XU.GE.AVG)XINT=XI1-XI2
      RETURN
      END
C
C
      SUBROUTINE AGAUSS(X,AVG,SIG,XINT)
C
C     This subroutine is adapted from the function given as Program
C     3-5 AGAUSS given on page 48 of Bevington.
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/BLOCKC/C1,C3
      ZERO=0.0D+00
      ONE=1.0D+00
      TWO=2.0D+00
 11   Z=DABS(X-AVG)/SIG
      XINT=ZERO
      IF(Z.EQ.ZERO)RETURN
 21   TERM=C1*Z
 22   SUM=TERM
      Y2=Z*Z/TWO
C
C     IF(Y2.GT.AMAX)RETURN should be added at this point when AMAX is known.
C
      C2=DEXP(-Y2)
      DENOM=ONE
 31   DENOM=DENOM+TWO
 32   TERM=TERM*Y2*TWO/DENOM
 33   SUM=SUM+TERM
      CHECK=TERM/SUM-1.0D-10
      IF(CHECK.GT.ZERO)GO TO 31
      XINT=C3*SUM*C2
      RETURN
      END
C
C
      SUBROUTINE FLERDN(FILE1,NLINES,NCOLS,XINT)
C
C     This subroutine reads in a file that contains any specified 
C     number NCOLS of columns (up to 10) and any unspecified number of 
C     lines NLINES that is determined by the program.
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XINT(10,17000)
      CHARACTER*20 FILE1
      OPEN(UNIT=1,FILE=FILE1,STATUS='OLD')
      I=0
C
C     The value of NLINES should correspond to the number of lines and
C     is determined by the program.
C     The value of NCOLS is read in and should correspond to the number
C     of columns in the input file.
C
 10   CONTINUE
      I=I+1
      READ(1,*,END=20)(XINT(J,I),J=1,NCOLS)
      GO TO 10
 20   CONTINUE
      CLOSE(UNIT=1)
      NLINES=I-1
      RETURN
      END
C
C
C
      SUBROUTINE MFILES
      WRITE(*,*)'     Replace all quotation marks in these files'
      WRITE(*,*)'     with apostrophes and omit existing'
	WRITE(*,*)'     apostrophes when using in MATLAB.'
	WRITE(*,*)''
      WRITE(*,*)'     This is the peak1.m file.'
      WRITE(*,*)'     clf'
	WRITE(*,*)'     clear all'
      WRITE(*,*)'     load peaksi1.plt'
      WRITE(*,*)'     x=peaksi1(:,1);'
      WRITE(*,*)'     y=peaksi1(:,2);'
      WRITE(*,*)'     semilogy(x,y,"ro","linewidth",2)'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("COUNTS PER CHANNEL")'
      WRITE(*,*)''
      WRITE(*,*)'     This is the peak2.m file.'
      WRITE(*,*)'     clf'
	WRITE(*,*)'     clear all'
      WRITE(*,*)'     load peaksi2.plt'
      WRITE(*,*)'     x=peaksi2(:,1);'
      WRITE(*,*)'     y1=peaksi2(:,2);'
      WRITE(*,*)'     y2=peaksi2(:,3);'
      WRITE(*,*)'     semilogy(x,y1,"ro",x,y2,"linewidth",2)'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("COUNTS PER CHANNEL")'
      WRITE(*,*)'     legend("Data","Model")'
      RETURN
      END


C
C                                                                               
C        SUBROUTINE CURMOD
C
C               (Revised 12/10/04)
C
C        PURPOSE        
C           To determine the parameters of a model or function that give
C           the minimum reduced chi-square value with the experimental data
C           that is supplied.  The model may contain both linear and 
C           nonlinear parameters.  The nonlinear parameters are obtained by
C           an optimally combined gradient search and linearization procedure
C           (the Marquardt algorithm) while the linear parameters are 
C           determined by the standard multiple linear regression approach 
C           with the previously fixed nonlinear parameters.
C
C        REFERENCE              
C           P.R. Bevington, DATA REDUCTION AND ERROR ANALYSIS FOR THE
C           PHYSICAL SCIENCES, McGraw-Hill Book Company, New York, 1969.
C                                                               
C        USAGE  
C           CALL CURMOD(IC,CHICUT,X,Y,SIGMAY,NPTS,NSEARC,NLS,MODE,IPRIN,
C           ILIM,INCMAX,NDERIV,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR,CHIZRO,
C           RELRAT,SIGMAA,R)   
C 
C        DESCRIPTION OF THE PARAMETERS             
C           IC    -Input integer maximum number of complete minimization
C                  trials to be allowed.  A value of 10 is suggested.
C           CHICUT-Input value to which RELRAT must be reduced before the
C                  minimization is stopped.  A value of 0.01 is suggested.
C           X     -Input matrix of data points for the independent variables.
C                  X(J,I) is the data point for the Jth independent 
C                  variable and the Ith data point.
C           Y     -Input vector of data points for the dependent variable.
C           SIGMAY-Input vector of standard deviations for the Y data points
C                  when available.  MODE must be set equal to plus one to
C                  to use these values.
C           NPTS  -Input integer number of data points.
C           NSEARC-Input integer number of parameters to be searched on by
C                  the nonlinear search.  This can be zero.
C           NLS   -Input integer number of parameters to be determined by
C                  a linear least-squares method.  This can be zero.
C           MODE  -Input integer that determines the least-squares weighting.
C                  +1 Gives WEIGHT(I)=1./SIGMAY(I)**2 (instrumental)
C                   0 Gives WEIGHT(I)=1. (no weighting)
C                  -1 Gives WEIGHT(I)=1./Y(I) (statistical)            
C           IPRIN -Input integer that controls whether intermediate parameter
C                  and object values are printed out.  If IPRIN is one the
C                  values are printed out.
C           ILIM  -Input integer that controls whether constraints are 
C                  imposed on the parameters.  When this is one constraints 
C                  are imposed and upper and lower values of the parameters
C                  must be supplied in the vectors AU and AL.
C           INCMAX-Input integer maximum number of minimization trials for
C                  a given set of initial estimates of the parameters A.
C                  A value of 10 is recommended.
C           NDERIV-Input integer that determines if a numerical or
C                  analytical derivative is to be obtained. When one it is
C                  numerical, when two it is analytical.
C           A     -Input values of the estimates of the nonlinear parameters
C                  to be found and output values provided by the search.
C                  The parameters from 1 to NSEARC are searched on by the
C                  nonlinear method while those from NSEARC+1 to NSEARC+NLS
C                  are obtained by a linear least-squares method.
C           AU    -Input vector of upper values of A when ILIM is one.
C           AL    -Input vector of lower values of A when ILIM is one.  It
C                  is suggested that this parameter be made slightly larger
C                  than the corresponding value of DELTAA and that it not 
C                  be set equal to zero.
C           DELTAA-Input vector of increments for the parameters A.  Values
C                  should be given only for the nonlinear parameters.
C           FLAMDE-Input value of the factor that determines the initial
C                  proportion of the search performed by the gradient method.
C                  A value of 0.001 is suggested.
C           YFIT  -Output vector of calculated values of Y with the final
C                  model or function.                                          
C           CHISQR-Output calculated value of the reduced chi-square value.
C                  This value is forced to be exactly unity when MODE is zero.
C                  The values of SIGMAA are also affected by this.
C           CHIZRO-The original value of CHISQR before it was changed to unity
C                  when MODE is zero.
C           RELRAT-Output value of the relative ratio of the difference in
C                  the initial and final reduced chi-square values for the 
C                  final incremental step for a given set of initial
C                  estimates of the parameters A.
C           SIGMAA-Output vector of the calculated standard deviations of A.
C           R     -Output vector of calculated linear correlation 
C                  coefficients.  The values from 1 to NSEARC are set to zero.
C
C        SUBROUTINES REQUIRED    
C           LS(NPTS,NSEARC,NLS,X,Y,WT,A,ISIG,SIGMAA,R)                
C              This subroutine determines the values of A from NSEARC+1
C              to NSEARC+NLS by a linear least-squares method.  The values
C              of A from 1 to NSEARC must be input to the subroutine.
C              The values of SIGMAA are also calculated when ISIG is one.
C           MATINV(ARRAY,NORDER,DET)                 
C              This subroutine inverts the symmetric two-dimensional matrix 
C              AARAY of degree NORDER and calculates the determinant DET.
C              The resulting inverted matrix is returned as ARRAY.
C           FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR) 
C              This subroutine evaluates the reduced chi-square value for 
C              the fit to the data.  NFREE is the number of data points NPTS 
C              minus the total number of parameters NTERMS.
C           FDERIV(NDERIV,NSEARC,X,I,A,DELTAA,DERIV)  
C              This subroutine evaluates the derivatives of the model or 
C              fitting function either analytically or numerically for 
C              the Ith data point with respect to each nonlinear parameter.
C              When NDERIV is one numerical derivatives are obtained,
C              when two analytical.  The analytical derivative algorithym
C              must be supplied by the user for the specific case.  NSEARC 
C              is the total number of nonlinear parameters and DERIV is the 
C              vector of derivatives.  Whenever possible this subroutine 
C              should be rewritten to evaluate the derivatives analytically 
C              since that is more accurate and computationally efficient.
C           MODEL(X,I,A,ZI,YFI)           
C              This subroutine determines the value of the model YFI and the   
C              vector of terms ZI of the model for data point I in terms of 
C              the independent variable matrix X that gives a linear function 
C              for least-squares fitting by subroutine LS.  The resulting 
C              model is given by:
C
C                    YFI=A(NSEARC+1)*ZI(1)+  
C                        A(NSEARC+2)*ZI(2)+...+
C                        A(NSEARC+NLS)*ZI(NLS) 
C                                                                              
C              This subroutine must be written specifically for each model
C              or function of interest.
C
	SUBROUTINE CURMOD(IC,CHICUT,X,Y,SIGMAY,NPTS,NSEARC,NLS,MODE,
     *  IPRIN,ILIM,INCMAX,NDERIV,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR,
     *  CHIZRO,RELRAT,SIGMAA,R)
C
C       The search is stopped when NSEARC is zero; RELRAT is negative, zero, 
C       or less than or equal to CHICUT; when the number of complete 
C       searches exceeds IC; or in the special case when CHISQR becomes zero.
C
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION X(35,17000),Y(17000),SIGMAY(17000),A(35),AU(35),AL(35)
	DIMENSION DELTAA(35),YFIT(17000),SIGMAA(35),R(35)
	DIMENSION WEIGHT(17000),ALPHA(35,35),BETA(35),DERIV(35)
	DIMENSION ARRAY(35,35),B(35),ALPHU(35,35),ZI(35)
  111   FORMAT(' THERE IS INSUFFICIENT DATA.')        
  112   FORMAT(' THE A(I) AND CHISQ1 ARE')            
  113   FORMAT(' ALERT. THE UPPER LIMIT OF')          
  114   FORMAT(' ALERT. THE LOWER LIMIT OF')          
  115   FORMAT(' B(J) AND CHISQR ARE')                
  116   FORMAT(' ALERT. INCMAX HAS BEEN EXCEEDED.')   
  117   FORMAT(4E15.6)                                
  118   FORMAT(' PARAMETER',I3,' IS BEING USED.')
  119   FORMAT(' CHISQR= ',E15.6)
  120   FORMAT(' THE A(I) ARE: ')
  121   FORMAT(' ALERT.  IC HAS BEEN EXCEEDED.')     
	INC=0                                         
	NTERMS=NSEARC+NLS
C	DO 5 J=1,NTERMS
C    5   R(J)=0.0D+00         
   11   NFREE = NPTS - NTERMS     
	IF(NFREE.LE.0)THEN
		WRITE(6,111)
		NFREE=1
		ENDIF
C
C       The weights are evaluated.
C
   20   DO 30 I=1,NPTS                            
   21   IF (MODE) 22,27,29                        
   22   IF (Y(I)) 25,27,23                        
   23   WEIGHT(I) = 1./Y(I)                       
	GO TO 30                                  
   25   WEIGHT(I) = 1. / (-Y(I))                  
	GO TO 30                                  
   27   WEIGHT(I) = 1.                            
	GO TO 30                                  
   29   WEIGHT(I) = 1./SIGMAY(I)**2               
   30   CONTINUE
	FLAMDA=FLAMDE
	II=1
   10   CONTINUE                  
C
C       The ALPHA and BETA matrices are evaluated.  If NSEARC is zero
C       most of the rest is omitted.
C
	IF(NSEARC.EQ.0)THEN
		ISIG=2
		CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,A,ISIG,SIGMAA,R)
		DO 32 I=1,NPTS
		CALL MODEL(X,I,A,ZI,YFI)
   32           YFIT(I)=YFI
		CALL FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
		GO TO 209
		ENDIF           
   31   DO 34 J=1,NSEARC                           
	BETA(J) = 0.0                               
	DO 34 K=1,J                                
   34   ALPHA(J,K)=0.0
	ISIG=2                              
	CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,A,ISIG,SIGMAA,R)
   41   DO 50 I=1,NPTS 
	CALL FDERIV(NDERIV,NSEARC,X,I,A,DELTAA,DERIV)
	CALL MODEL(X,I,A,ZI,YFI) 
	DO 46 J=1,NSEARC
	BETA(J)=BETA(J)+WEIGHT(I)*(Y(I)-YFI)*DERIV(J) 
	DO 46 K=1,J                                      
   46   ALPHA(J,K)=ALPHA(J,K)+WEIGHT(I)*DERIV(J)*DERIV(K)
   50   CONTINUE               
   51   DO 53 J=1,NSEARC       
	DO 53 K=1,J            
   53   ALPHA(K,J)=ALPHA(J,K)
C  
C       The chi-square value at the starting point is evaluated.
C
   61   DO 62 I=1,NPTS
	CALL MODEL(X,I,A,ZI,YFI)   
   62   YFIT(I) = YFI
   63   CALL FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQ1)
	IF(IPRIN-1)64,65,64        
   65   WRITE(6,112)                             
	WRITE(6,117)(A(I),I=1,NTERMS),CHISQ1     
   64   CONTINUE                                 
C
C       The modified curvature matrix is inverted to find new parameters.
C
   71   DO 74 J=1,NSEARC                         
	DO 73 K=1,NSEARC
	ALPHU(J,K)=DSQRT(ALPHA(J,J)*ALPHA(K,K))                         
   73   ARRAY(J,K)=ALPHA(J,K)/ALPHU(J,K)
   74   ARRAY(J,J) = 1. + FLAMDA
	INC=INC + 1                      
   80   CALL MATINV (ARRAY,NSEARC,DET)
   81   DO 84 J=1,NSEARC                 
	B(J) = A(J)                      
	DO 84 K=1,NSEARC                 
   84   B(J)=B(J)+BETA(K)*ARRAY(J,K)/ALPHU(J,K)
C
C       The searched parameters are limited if ILIM is one.
C
	IF(ILIM-1)86,85,86               
   85   CONTINUE                    
	DO 83 J=1,NSEARC            
	IF(B(J)-AU(J))87,87,88      
   87   IF(B(J)-AL(J))89,83,83      
   88   B(J)=AU(J)                  
	WRITE(6,113)                
	WRITE(6,118)J               
	GO TO 83                    
   89   B(J)=AL(J)                  
	WRITE(6,114)                
	WRITE(6,118)J               
   83   CONTINUE                    
   86   CONTINUE                    
C
C       If CHISQR increases FLAMDA is increased and the calculations 
C       are repeated.
C
	ISIG=2
	CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,B,ISIG,SIGMAA,R)
	DO 92 I=1,NPTS
	CALL MODEL(X,I,B,ZI,YFI)                                   
   92   YFIT(I) = YFI
   93   CALL FCHISQ (NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
	IF(IPRIN-1)96,97,96                                  
   97   WRITE(6,115)                                         
	WRITE(6,117)(B(J),J=1,NTERMS),CHISQR                 
   96   CONTINUE                                             
	IF (CHISQ1 - CHISQR) 95,101,101                      
   95   FLAMDA=10.*FLAMDA                                    
	IF(INC-INCMAX)71,71,108
C                              
C       The nonlinear parameters and their uncertainties are evaluated.
C
  108   WRITE(6,116)                         
	FLAMDA=FLAMDA/(10.**INCMAX)          
  101   DO 103 J=1,NSEARC
  103   A(J) = B(J)
	FLAMDA=FLAMDA/10.
	IF(CHISQR.EQ.0.0)THEN
		RELRAT=0.0
		WRITE(6,*)'CHISQR was zero, it is reset to 0.000001.'
		CHISQR=0.000001
		ENDIF
	IF(CHISQR.NE.0.000001)RELRAT=(CHISQ1-CHISQR)/CHISQR
	IF(IPRIN-1)203,202,203
  202   WRITE(6,119)CHISQR
	WRITE(6,120)
	WRITE(6,117)(A(I),I=1,NTERMS)
  203   CONTINUE
	IF(RELRAT)209,209,204
  204   IF(RELRAT-CHICUT)209,209,205
  205   IF(II-IC)206,206,207
  206   II=II+1
	GO TO 10
  207   WRITE(6,121)
  209   CONTINUE
	CON1=1.
	CON2=1.        
C
C       When unit weighting (MODE=0) is used it is assumed that the
C       observed deviations are also the controlling theoretical deviations
C       and the reduced chi-square value is forced to be unity.  This makes
C       corresponding changes in the SIGMAA and R values.  The original
C       chi-square value is saved as CHIZRO in this case.
C
	IF(MODE.EQ.0)THEN
		CHIZRO=CHISQR
		DO 212 I=1,NPTS
  212           WEIGHT(I)=1./CHISQR
		CON1=1./CHISQR
		CON2=DSQRT(CHISQR)
		ENDIF
	IF(NSEARC.EQ.0)GO TO 107
  105   DO 106 J=1,NSEARC                    
  106   SIGMAA(J)=CON2*DSQRT(ARRAY(J,J)/ALPHA(J,J))
  107   CONTINUE
	CHISQR=CON1*CHISQR
	ISIG=1
C
C       The following use of the vector B instead of A is 
C       necessary when MODE is 0 so that different values
C       of the linear A are not calculated with the different
C       weights that are used.
C
	IF(NSEARC.GE.1)THEN
		DO 220 I=1,NSEARC
  220           B(I)=A(I)
	ENDIF
	CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,B,ISIG,SIGMAA,R)
  110   CONTINUE
	RETURN
	END   
C
C
C                                 
	SUBROUTINE LS(NPTS,NSEARC,NLS,X,Y,WT,A,ISIG,SIGMAA,R)
C
C       This subroutine sets up the equations and solves for all parameters
C       A that can be obtained by a linear least-squares method.  The
C       parameters found are numbered from NSEARC +1 to NSEARC +NLS.
C
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION X(35,17000),Y(17000),WT(17000),A(35),SIGMAA(35),R(35) 
	DIMENSION C(35,35),ZI(35),B(35),D(35),XMEAN(35)
	IF(NLS.EQ.0)GO TO 100
C
C       Evaluate the vector of left-hand-side coefficients B(J) and the
C       matrix of right-hand-side coefficients C(J,K).
C 
	DO 11 J=1,NLS    
	B(J)=0.0                                                               
	DO 11 K=1,NLS
   11   C(J,K)=0.0
	DO 12 I=1,NPTS
	CALL MODEL(X,I,A,ZI,YFI)
	DO 12 J=1,NLS
	B(J)=B(J)+WT(I)*Y(I)*ZI(J)
	DO 12 K=1,NLS
   12   C(J,K)=C(J,K)+WT(I)*ZI(J)*ZI(K)
C
C       Invert the matrix of right-hand-side coefficients.
C
	CALL MATINV(C,NLS,DET)                       
C
C       Obtain the solution vector D and the vector of parameters A.
C
	DO 15 J=1,NLS         
   15   D(J)=0.0              
	DO 16 J=1,NLS         
	DO 16 K=1,NLS         
   16   D(J)=D(J)+B(K)*C(J,K) 
	DO 17 J=1,NLS         
	K=NSEARC+J            
   17   A(K)=D(J) 
   20   CONTINUE
	IF(ISIG.EQ.1)GO TO 21
	GO TO 100
   21   CONTINUE
C
C       This section determines the SIGMAA and the R for the linear
C       parameters.
C
	DO 30 I=1,NLS
	K=I+NSEARC
	IF(C(I,I).LT.0.0)C(I,I)=-C(I,I)
	SIGMAA(K)=DSQRT(C(I,I))
   30   CONTINUE
	XNPTS=NPTS
	YMEAN=0.0
	WTSUM=0.0
	DO 31 J=1,NLS
   31   XMEAN(J)=0.0
	DO 35 I=1,NPTS
	YMEAN=YMEAN+WT(I)*Y(I)
	WTSUM=WTSUM+WT(I)
	CALL MODEL(X,I,A,ZI,YFI)
	DO 32 J=1,NLS
	XMEAN(J)=XMEAN(J)+WT(I)*ZI(J)
   32   CONTINUE        
   35   CONTINUE
	YMEAN=YMEAN/WTSUM
	DO 36 J=1,NLS
	XMEAN(J)=XMEAN(J)/WTSUM
   36   CONTINUE
	XN1=XNPTS-1.
	DO 40 J=1,NLS
	K=J+NSEARC
	SJY=0.0
	SYY=0.0
	SJJ=0.0
	DO 38 I=1,NPTS
	CALL MODEL(X,I,A,ZI,YFI)
	SJY=SJY+WT(I)*(Y(I)-YMEAN)*(ZI(J)-XMEAN(J))/(XN1*WTSUM)
	SYY=SYY+WT(I)*(Y(I)-YMEAN)*(Y(I)-YMEAN)/(XN1*WTSUM)
	SJJ=SJJ+WT(I)*(ZI(J)-XMEAN(J))*(ZI(J)-XMEAN(J))/(XN1*WTSUM)
   38   CONTINUE
	IF(SYY.LE.0.0)THEN
		R(K)=0.0
		GO TO 40
		ENDIF
	SY=DSQRT(SYY)
	IF(SJJ.LE.0.0)THEN
		R(K)=0.0
		GO TO 40
		ENDIF
	SJ=DSQRT(SJJ)
	R(K)=SJY/(SY*SJ)
   40   CONTINUE
  100   CONTINUE
	RETURN 
	END
C
C  
C
	SUBROUTINE MATINV (ARRAY,NORDER,DET)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION ARRAY(35,35),IK(35),JK(35)                 
   10   DET=1.                                               
   11   DO 100 K=1,NORDER                                    
C
C       The largest element of the matix ARRAY(I,J) is found.
C
	AMAX=0.                                    
   21   DO 30 I=K,NORDER                           
	DO 30 J=K,NORDER                           
   23   IF (DABS(AMAX) - DABS(ARRAY(I,J)))24,24,30  
   24   AMAX = ARRAY(I,J)                          
	IK(K) = I                                  
	JK(K) = J                                  
   30   CONTINUE                                   
C
C       Rows and columns are interchanged to put AMAX in ARRAY(K,K).
C
   31   IF (AMAX) 41,32,41        
   32   DET=0.                    
	GO TO 140                 
   41   I=IK(K)                   
	IF (I-K) 21,51,43         
   43   DO 50 J=1,NORDER          
	SAVE=ARRAY(K,J)           
	ARRAY(K,J) = ARRAY(I,J)   
   50   ARRAY(I,J) = -SAVE        
   51   J=JK(K)                   
	IF(J-K) 21,61,53          
   53   DO 60 I=1,NORDER          
	SAVE = ARRAY(I,K)         
	ARRAY(I,K) = ARRAY(I,J)   
   60   ARRAY(I,J) = -SAVE        
C
C       Elements of the inverse matrix are accumulated.
C
   61   DO 70 I=1,NORDER            
	IF(I-K) 63,70,63                              
   63   ARRAY(I,K) = -ARRAY(I,K) / AMAX               
   70   CONTINUE                                      
   71   DO 80 I=1,NORDER                              
	DO 80 J=1,NORDER                              
	IF(I-K) 74,80,74                              
   74   IF(J-K) 75,80,75                              
   75   ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)   
   80   CONTINUE                                      
   81   DO 90 J=1,NORDER                              
	IF (J-K) 83,90,83                             
   83   ARRAY(K,J) = ARRAY(K,J) / AMAX                
   90   CONTINUE                                      
	ARRAY(K,K) = 1. / AMAX                        
  100   DET = DET * AMAX
C                              
C       The ordering of the matrix is restored.                                
C
  101   DO 130 L=1,NORDER                            
	K=NORDER-L+1                                 
	J=IK(K)                                      
	IF (J-K) 111,111,105                         
  105   DO 110 I=1,NORDER                            
	SAVE=ARRAY(I,K)                              
	ARRAY(I,K) = -ARRAY(I,J)                     
  110   ARRAY(I,J) = SAVE                            
  111   I=JK(K)                                      
	IF(I-K) 130,130,113                          
  113   DO 120 J=1,NORDER                            
	SAVE=ARRAY(K,J)                              
	ARRAY(K,J) = -ARRAY(I,J)                     
  120   ARRAY(I,J) = SAVE                            
  130   CONTINUE                                     
  140   RETURN                                       
	END
C
C
C                                          
	SUBROUTINE FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION Y(17000),YFIT(17000),WEIGHT(17000)
	CHISQ = 0.                            
	DO 30 I=1,NPTS                        
   30   CHISQ=CHISQ+WEIGHT(I)*(Y(I)-YFIT(I))**2  
	FREE=NFREE
   32   CHISQR=CHISQ/FREE                     
	RETURN                                
	END                                   
C
C
C
	SUBROUTINE FDERIV(NDERIV,NSEARC,X,I,A,DELTAA,DERIV)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION X(35,17000),A(35),DELTAA(35),DERIV(35),ZI(35)
	IF(NDERIV.EQ.1)GO TO 100
	IF(NDERIV.EQ.2)GO TO 200
 100    CONTINUE
 11     DO 18 J=1,NSEARC
	AJ=A(J)
	DELTA=DELTAA(J)
	A(J)=AJ+DELTA
	CALL MODEL(X,I,A,ZI,YF1)
	A(J)=AJ-DELTA
	CALL MODEL(X,I,A,ZI,YF2)
	DERIV(J)=(YF1-YF2)/(2.*DELTA)
 18     A(J)=AJ
	RETURN
 200    CONTINUE
C       The present analytical derivatives here are for exponential
C       terms.
	DO 210 J=1,NSEARC
	  DERIV(J)=-DEXP(-A(J)*X(1,I))*X(1,I)
 210    CONTINUE
	RETURN
	END     





