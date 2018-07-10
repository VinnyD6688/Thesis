C                      Main Program PEAKSI.F
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
      WRITE(*,*)'Gaussian for each channel can be employed and'
      WRITE(*,*)'the peak channel(s) and resolution can either'
      WRITE(*,*)'be fixed or searched on.'
      WRITE(*,*)'The data file must have pairs of channel number'
      WRITE(*,*)'and counts per channel on all lines.'
      WRITE(*,*)'Both the experimental data and a comparison of'
      WRITE(*,*)'the experimental data and the derived fit can'
      WRITE(*,*)'be plotted and looked at in MATLAB.  The "m"'
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
      WRITE(*,*)'integrated Gaussian input 2.'
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
     *  IPRIN,ILIM,INCMAX,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR,
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
      WRITE(*,*)'This is data from Program PEAKSI.'
      WRITE(*,*)'This is data from file:'
      WRITE(*,101)FILE1
      WRITE(*,*)'CHISQR,CHIZRO,MODE=',CHISQR,CHIZRO,MODE
      WRITE(*,*)'SIGMAA(I) is in %.'
      WRITE(*,*)'I, A(I), SIGMAA(I) in %, and R(I) are:'
      DO 21 I=1,NTERMS
        WRITE(*,108)I,A(I),SIGMAA(I),R(I)
 108    FORMAT(1X,I5,2(3X,E12.6),' %',2X,E11.4)
 21     CONTINUE
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
          WRITE(2,*)X(1,I),Y(I),YFIT(I)
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
      WRITE(*,*)'     with apostrophes.'
      WRITE(*,*)'     This is the peak1.m file.'
      WRITE(*,*)'     clf'
      WRITE(*,*)'     load peaksi1.plt'
      WRITE(*,*)'     x=peaksi1(:,1);'
      WRITE(*,*)'     y=peaksi1(:,2);'
      WRITE(*,*)'     semilogy(x,y,"ro")'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("COUNTS PER CHANNEL")'
      WRITE(*,*)''
      WRITE(*,*)'     This is the peak2.m file.'
      WRITE(*,*)'     clf'
      WRITE(*,*)'     load peaksi2.plt'
      WRITE(*,*)'     x=peaksi2(:,1);'
      WRITE(*,*)'     y1=peaksi2(:,2);'
      WRITE(*,*)'     y2=peaksi2(:,3);'
      WRITE(*,*)'     semilogy(x,y1,"ro",x,y2)'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("COUNTS PER CHANNEL")'
      WRITE(*,*)'     legend("Data","Model")'
      RETURN
      END