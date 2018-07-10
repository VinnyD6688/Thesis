C     This is Program GSHIFT.F, a general gain and zero shifting
C     program.
C	                    Revised 2/17/09
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION OCNTS(17000),FCNTS(17000)
      DIMENSION CI(10,15)
      DIMENSION EOU(17000),EOL(17000),EPOU(17000),EPOL(17000)
      DIMENSION EFU(17000),EFL(17000),EPFU(17000),EPFL(17000)
      DIMENSION XINT(10,17000)
      CHARACTER*20 FILE1
C     Constants are set.
      ZERO=0.0D+00
      HALF=0.5D+00
      EMIN=ZERO
      EMAX=13.D+00            
      IEND=10
      EPS=1.0D-10
      CI(1,1)=1.97796D+00
      CI(1,2)=0.429755D+00
      CI(1,3)=0.572781D+00
      CI(1,4)=0.294498E-02
      CI(1,5)=-0.288986E-03
      WRITE(*,*)'Revised: 6/14/00'
      WRITE(*,*)'This is Program GSHIFT.F.  It gain and zero'
      WRITE(*,*)'shifts from any channel-pulse height energy'
      WRITE(*,*)'relationship to any other - including non-linear'
      WRITE(*,*)'relationships.  A number of relationships are'
      WRITE(*,*)'supplied.  The approach used is a linear'
      WRITE(*,*)'interpolation within individual channels.'
      WRITE(*,*)'It is assumed that both the channel-pulse height'
      WRITE(*,*)'energy relationships of the original and'
      WRITE(*,*)'final shifted spectra are known.  It is also'
      WRITE(*,*)'assumed that the nominal channel refers to the'
      WRITE(*,*)'channel center, namely channel 8 means channel'
      WRITE(*,*)'8.00 and it extends from 7.50 to 8.50.  It is'
      WRITE(*,*)'assumed that one always starts with channel 1.'
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N88
      IF(N88.EQ.1)CONTINUE 
      WRITE(*,*)'All channel - pulse-height relationships are of'
      WRITE(*,*)'the form:'
      WRITE(*,*)''
      WRITE(*,*)'      XNCHAN=A*EP+B      (1)'
      WRITE(*,*)''
      WRITE(*,*)'where XNCHAN is channel number in floating point'
      WRITE(*,*)'format, EP is pulse-height energy in MeV, A is a'
      WRITE(*,*)'normalizing constant, and B is number of'
      WRITE(*,*)'background channels and can be plus or minus.'
      WRITE(*,*)'This relationship is for the MCA and'
      WRITE(*,*)'assumes that it is linear.'
      WRITE(*,*)'The relationship for pulse height energy EP'
      WRITE(*,*)'as a function of true energy E is:'
      WRITE(*,*)''
      WRITE(*,*)'      EP=E*f(E)          (2)'
      WRITE(*,*)''
      WRITE(*,*)'where EP is pulse-height energy, E is true X- or'
      WRITE(*,*)'gamma-ray energy in MeV, and f(E) is the'
      WRITE(*,*)'functional form of the relationship that gives'
      WRITE(*,*)'EP/E.'
      WRITE(*,*)'When changing the gain and zero to match two new'
      WRITE(*,*)'desired full energy peak locations, the A and B'
      WRITE(*,*)'can be changed to accomplish this.  The f(E)'
      WRITE(*,*)'should not be changed as it is a fundamental'
      WRITE(*,*)'relationship and describes nonlinearity if'
      WRITE(*,*)'present.'
      WRITE(*,*)'The two required full energy peak locations can'
      WRITE(*,*)'be obtained by using the PEAKSI.F program.'
      WRITE(*,*)'The data can be plotted in MATLAB with m files'
      WRITE(*,*)'gshi1.m and gshi2.m.  These can be seen now by'
      WRITE(*,*)'inputting 1.'
      READ(*,*)N36
      IF(N36.EQ.1)THEN
        CALL MFILES
        ENDIF
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N87
      IF(N87.EQ.1)CONTINUE 
      WRITE(*,*)''
      WRITE(*,*)'Now the original spectrum is read in.  What is'
      WRITE(*,*)'the file name of this spectrum?'
      READ(*,101)FILE1
 101  FORMAT(A20)
      NCOLS=2
      CALL FLERDN(FILE1,NLINES,NCOLS,XINT)
      DO 4 I=1,NLINES
        OCNTS(I)=XINT(2,I)
 4      CONTINUE
      WRITE(*,*)'NLINES=',NLINES
      WRITE(*,*)'To increase the total number of channels'
      WRITE(*,*)'input 1 - otherwise 0.'
      READ(*,*)N78
      IF(N78.EQ.1)THEN
        WRITE(*,*)'What is the total desired number of channels?'
        READ(*,*)NOCHAN
        WRITE(*,*)'To place zeroes in all channels from NLINES to'
        WRITE(*,*)'NOCHAN, input 1 - otherwise 0.'
        READ(*,*)NZERO
        IF(NZERO.EQ.1)THEN
          NLIN1=NLINES+1
          DO 3 I=NLIN1,NOCHAN
 3          OCNTS(I)=ZERO
          ENDIF
        NLINES=NOCHAN  
        ENDIF
      DO 5 I=1,NLINES,10
        WRITE(*,*)'I=',I,' OCNTS(I)=',OCNTS(I)
 5      CONTINUE
      WRITE(*,*)'What is the model number MODO for this data?'
      WRITE(*,*)'MODO=1 is nonlinear model for NaI'
      WRITE(*,*)'MODO=2 is fifth degree polynomial'
      WRITE(*,*)'MODO=3 is directly proportional model for'
      WRITE(*,*)'       Monte Carlo generated spectra'
	WRITE(*,*)'MODO=4 is linear model'
      READ(*,*)MODO
      IF(MODO.EQ.3)THEN
        WRITE(*,*)'What is the single peak energy E01 in MeV'
        WRITE(*,*)'and peak channel number XN01 for MODO=3?'
        READ(*,*)E01,XN01
        AO=XN01/E01
        BO=ZERO
        WRITE(*,*)'AO=',AO,' BO=',BO
        WRITE(*,*)'Input 1 to continue.'
        READ(*,*)N45
        IF(N45.EQ.1)CONTINUE
        DO 7 I=1,NLINES
          XI=I
          XI=XI+HALF
          EPOU(I)=XI/AO
          EOU(I)=EPOU(I)
 7        CONTINUE
        EPOL(1)=HALF/AO
        EOL(1)=EPOL(1)
        DO 8 I=2,NLINES
          EPOL(I)=EPOU(I-1)
          EOL(I)=EPOL(I)
 8        CONTINUE
        ENDIF
C
      IF(MODO.EQ.1.OR.MODO.EQ.2)THEN
      IF(MODO.EQ.2)THEN
        WRITE(*,*)'What is CI(2,I) for I=1,5 for MODO=2?'
        READ(*,*)CI(2,1),CI(2,2),CI(2,3),CI(2,4),CI(2,5)
        ENDIF
      WRITE(*,*)'Now values for two peak energies in the original'
      WRITE(*,*)'spectrum are read in as EO1,EO2,XNO1,XNO2 where'
      WRITE(*,*)'these parameters are the first peak true energy'
      WRITE(*,*)'in MeV, the second peak true energy in MeV,'
      WRITE(*,*)'the first peak channel number, and the second peak'
      WRITE(*,*)'channel, respectively. (Energies recommended'
      WRITE(*,*)'for PGNAA are 0.511, 2.223, 4.945, and 10.829' 
      WRITE(*,*)'MeV.)'
      WRITE(*,*)'Note that one must be very careful in the non-'
      WRITE(*,*)'linear cases to put the peaks in consistent'
      WRITE(*,*)'locations -- especially when different peaks are'
      WRITE(*,*)'used for the original and shifted spectra.'
      READ(*,*)EO1,EO2,XNO1,XNO2
      WRITE(*,*)'Now the pulse-height peak energies EPO1 and EPO2'
      WRITE(*,*)'are determined from the f(E) (Eq. 2) relationship.'
      CALL MODEL(MODO,CI,EO1,EPO1,DERF1)
      CALL MODEL(MODO,CI,EO2,EPO2,DERF2)
      WRITE(*,*)'EO1=',EO1,' EPO1=',EPO1
      WRITE(*,*)'EO2=',EO2,' EPO2=',EPO2
      WRITE(*,*)'Now the A and B are calculated from simultaneous'
      WRITE(*,*)'solution of the two Eq. 1 for EO1 and EO2 to give'
      WRITE(*,*)'AO and BO.'
      AO=(XNO1-XNO2)/(EPO1-EPO2)
      BO=XNO1-AO*EPO1 
      WRITE(*,*)'AO=',AO,' BO=',BO
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N1
      WRITE(*,*)'Next the pulse-height and true upper and lower'
      WRITE(*,*)'channel energy limits EPOU(I) and EPOL(I) and'
      WRITE(*,*)'EOU(I) and EOL(I) are obtained from the model'
      WRITE(*,*)'relationship of Eq. 2 for the original spectrum.'
      DO 10 I=1,NLINES
        XI=I
        XI=XI+HALF
        EPOU(I)=(XI-BO)/AO
        IF(EPOU(I).LE.EMIN.OR.EPOU(I).GE.EMAX)EOU(I)=ZERO
        IF(EPOU(I).LE.EMIN.OR.EPOU(I).GE.EMAX)GO TO 10
        EST=EPOU(I)
        CALL RTNI(MODO,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
        EOU(I)=EDUM
 10     CONTINUE
      XI=HALF
      EPOL(1)=(XI-BO)/AO
      IF(EPOL(1).LE.EMIN.OR.EPOL(1).GE.EMAX)EOL(1)=ZERO
      IF(EPOL(1).LE.EMIN.OR.EPOL(1).GE.EMAX)GO TO 11
      EST=EPOL(1)
      CALL RTNI(MODO,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
      EOL(1)=EDUM
 11   CONTINUE 
      DO 12 I=2,NLINES
        EPOL(I)=EPOU(I-1)
        EOL(I)=EOU(I-1)
 12     CONTINUE
      ENDIF
 13   CONTINUE
      DO 14 I=1,NLINES,10  
        WRITE(*,*)'I=',I,' EPOL(I)=',EPOL(I),' EOL(I)=',EOL(I)
        WRITE(*,*)'EPOU(I)=',EPOU(I),' EOU(I)=',EOU(I)
 14     CONTINUE
      WRITE(*,*)'What is the model number MODF for the final shifted'
      WRITE(*,*)'spectrum?'
      WRITE(*,*)'MODF=1 is nonlinear model for NaI'
      WRITE(*,*)'MODF=2 is fifth degree polynomial'
      WRITE(*,*)'MODF=3 is directly proportional model for'
      WRITE(*,*)'       Monte Carlo generated spectra'
	WRITE(*,*)'MODF=4 is linear model'
      READ(*,*)MODF
	IF(MODF.EQ.4)THEN
	WRITE(*,*)'What are EF1 and EF2, the final two energies in MeV?'
	READ(*,*)EF1,EF2
	WRITE(*,*)'What are the final two channel numbers N1 and N2?'
	READ(*,*)N1,N2
	  ENDIF
      IF(MODF.EQ.2)THEN
        WRITE(*,*)'What is CI(2,I) for I=1,5 for MODF=2?'
        READ(*,*)CI(2,1),CI(2,2),CI(2,3),CI(2,4),CI(2,5)
        ENDIF
      IF(MODF.EQ.3)THEN
        WRITE(*,*)'What is the single peak energy E01 in MeV'
        WRITE(*,*)'and peak channel number XN01 for MODF=3?'
        READ(*,*)E01,XN01
        AF=XN01/E01
        BF=ZERO
        WRITE(*,*)'AF=',AF,' BF=',BF
        WRITE(*,*)'Input 1 to continue.'
        READ(*,*)N47
        IF(N47.EQ.1)CONTINUE
        DO 17 I=1,NLINES
          XI=I
          XI=XI+0.5
          EPFU(I)=XI/AF
          EFU(I)=EPFU(I)
 17       CONTINUE
        EPFL(1)=0.5/AF 
        EFL(1)=EPFL(1)
        DO 18 I=2,NLINES
          EPFL(I)=EPFU(I-1)
          EFL(I)=EPFL(I)
 18       CONTINUE
        ENDIF
C
      IF(MODF.EQ.1)THEN
      WRITE(*,*)'Now values for the two peak energies in the shifted'
      WRITE(*,*)'spectrum are read in as EF1,EF2,XNF1,XNF2 where'        
      WRITE(*,*)'these parameters are the first peak true energy'
      WRITE(*,*)'in MeV, the second peak true energy in MeV,'
      WRITE(*,*)'the first peak channel number, and the second peak'
      WRITE(*,*)'channel, respectively.'
      WRITE(*,*)'Note that one must be very careful in the non-'
      WRITE(*,*)'linear cases to put the peaks in consistent'
      WRITE(*,*)'locations -- especially when different peaks are'
      WRITE(*,*)'used for the original and shifted spectra.'
      READ(*,*)EF1,EF2,XNF1,XNF2
      WRITE(*,*)'Now the pulse-height peak energies EPF1 and EPF2'
      WRITE(*,*)'are determined from the f(E) (Eq. 2) relationship.'
      CALL MODEL(MODF,CI,EF1,EPF1,DERF1)
      CALL MODEL(MODF,CI,EF2,EPF2,DERF2)
      WRITE(*,*)'EF1=',EF1,' EPF1=',EPF1
      WRITE(*,*)'EF2=',EF2,' EPF2=',EPF2
      WRITE(*,*)'Now the A and B are calculated from simultaneous'
      WRITE(*,*)'solution of the two Eq. 1 for EF1 and EF2 to give'
      WRITE(*,*)'AF and BF.'
      AF=(XNF1-XNF2)/(EPF1-EPF2)
      BF=XNF1-AF*EPF1
      WRITE(*,*)'AF=',AF,' BF=',BF
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N1
      WRITE(*,*)'Next the pulse-height and true upper and lower'
      WRITE(*,*)'channel energy limits EPFU(I) and EPFL(I) and'
      WRITE(*,*)'EFU(I) and EFL(I) are obtained from the model'
      WRITE(*,*)'relationship of Eq. 2 for the shifted spectrum.'
      DO 20 I=1,NLINES 
        XI=I
        XI=XI+HALF
        EPFU(I)=(XI-BF)/AF
        IF(EPFU(I).LE.EMIN.OR.EPFU(I).GE.EMAX)EFU(I)=ZERO
        IF(EPFU(I).LE.EMIN.OR.EPFU(I).GE.EMAX)GO TO 20
        EST=EPFU(I)
        CALL RTNI(MODF,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
        EFU(I)=EDUM
 20     CONTINUE
      XI=HALF
      EPFL(1)=(XI-BF)/AF
      IF(EPFL(1).LE.EMIN.OR.EPFL(1).GE.EMAX)EFL(1)=ZERO
      IF(EPFL(1).LE.EMIN.OR.EPFL(1).GE.EMAX)GO TO 21
      EST=EPFL(1)
      CALL RTNI(MODF,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
      EFL(1)=EDUM
 21   CONTINUE
      DO 22 I=2,NLINES
        EPFL(I)=EPFU(I-1)
        EFL(I)=EFU(I-1)
 22     CONTINUE
      ENDIF
 23   CONTINUE  
      DO 24 I=1,NLINES,10
        WRITE(*,*)'I=',I,' EPFL(I)=',EPFL(I),' EFL(I)=',EFL(I)
        WRITE(*,*)'EPFU(I)=',EPFU(I),' EFU(I)=',EFU(I)
 24     CONTINUE  
C
C
      WRITE(*,*)'To store this data in a file, input 1.'
      WRITE(*,*)'The file name for storing the EPOU(I),'
      WRITE(*,*)'EOU(I), EPFU(I), and EFU(I) is gshift1.plt.'
      READ(*,*)N78
      IF(N78.EQ.1)THEN
        OPEN(UNIT=2,FILE='gshift1.plt',STATUS='UNKNOWN')
        DO 30 I=1,NLINES
          WRITE(*,*)'I=',I,' EPOU(I)=',EPOU(I),' EOU(I)=',EOU(I)
          WRITE(*,*)'      EPFU(I)=',EPFU(I),' EFU(I)=',EFU(I)
          WRITE(2,102)I,EPOU(I),EOU(I),EPFU(I),EFU(I)
 30     CONTINUE
        CLOSE(UNIT=2)
      ENDIF
 102  FORMAT(I5,2X,4(E12.6,2X))
      WRITE(*,*)'To look at a plot of this input 1.'
      READ(*,*)N83
      IF(N83.EQ.1)THEN
        WRITE(*,*)'Use gshi1.m in MATLAB to examine plot.'
        ENDIF
      WRITE(*,*)'Now the counts are shifted.'
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N1
C
      DO 40 I=1,NLINES
        FCNTS(I)=ZERO
 40     CONTINUE
      DO 1000 I=1,NLINES
C
C       I's will be used for the final spectrum channels and energies
C       while J's will be used for the original.
C
        WRITE(*,*)'I=',I
        JMIN=0
        JMAX=0
        DO 100 J=1,NLINES
          EDUM=EFL(I)
          IF(EDUM.GE.EOL(J).AND.EDUM.LE.EOU(J))THEN
            JMIN=J
            GO TO 105
            ENDIF
 100      CONTINUE
 105      CONTINUE
        DO 110 J=1,NLINES
          EDUM=EFU(I)
          IF(EDUM.GE.EOL(J).AND.EDUM.LE.EOU(J))THEN
            JMAX=J
            GO TO 115 
            ENDIF
 110      CONTINUE
 115      CONTINUE
        IF(JMIN.EQ.0.AND.JMAX.EQ.0)GO TO 1000
        IF(JMIN.EQ.0.AND.JMAX.NE.0)JMIN=1
        IF(JMIN.NE.0.AND.JMAX.EQ.0)JMAX=NLINES
        IF(JMIN.EQ.JMAX)THEN
          TOP=EFU(I)-EFL(I)
          IF(TOP.LE.ZERO)TOP=ZERO
          BOT=EOU(JMIN)-EOL(JMIN)
          IF(BOT.LE.ZERO)BOT=ZERO
        IF(BOT.GT.ZERO)FCNTS(I)=FCNTS(I)+OCNTS(JMIN)*TOP/BOT
          ENDIF
        IF(JMIN.NE.JMAX)THEN
          TOPL=EOU(JMIN)-EFL(I)
          IF(TOPL.LE.ZERO)TOPL=ZERO
          BOTL=EOU(JMIN)-EOL(JMIN)
          IF(BOTL.LE.ZERO)BOTL=ZERO
          IF(BOTL.GT.ZERO)FCNTS(I)=FCNTS(I)+OCNTS(JMIN)*TOPL/BOTL
          TOPU=EFU(I)-EOL(JMAX)
          IF(TOPU.LE.ZERO)TOPU=ZERO
          BOTU=EOU(JMAX)-EOL(JMAX)
          IF(BOTU.LE.ZERO)BOTU=ZERO
          IF(BOTU.GT.ZERO)FCNTS(I)=FCNTS(I)+OCNTS(JMAX)*TOPU/BOTU
          JCHK=JMAX-JMIN
          IF(JCHK.GT.1)THEN
            JMIN1=JMIN+1
            JMAX1=JMAX-1
          IF(JMIN1.EQ.JMAX1)FCNTS(I)=FCNTS(I)+OCNTS(JMIN1)
          IF(JMIN1.NE.JMAX1)THEN
              DO 120 JJ=JMIN1,JMAX1
                FCNTS(I)=FCNTS(I)+OCNTS(JJ)
 120        CONTINUE
          ENDIF
            ENDIF
          ENDIF
        WRITE(*,*)'FCNTS(I)=',FCNTS(I)
C        WRITE(*,*)'For another I input 1.' 
C        READ(*,*)N1
 1000 CONTINUE
      SUMO=ZERO
      SUMF=ZERO
      DO 1200 I=1,NLINES
        SUMO=SUMO+OCNTS(I)
        SUMF=SUMF+FCNTS(I)
 1200   CONTINUE
      WRITE(*,*)'SUMO=',SUMO,' SUMF=',SUMF
C
C     An output file can be obtained here.
C
      WRITE(*,*)'To store in a file input 1.'
      WRITE(*,*)'The file name used is gshift2.plt.'
      READ(*,*)N1
      IF(N1.EQ.1)THEN
        OPEN(UNIT=3,FILE='gshift2.plt',STATUS='UNKNOWN')
        DO 1500 I=1,NLINES
          WRITE(3,*)I,OCNTS(I),FCNTS(I)
 1500     CONTINUE
        CLOSE(UNIT=3)
        ENDIF
      WRITE(*,*)'To look at a plot of the gshift2.plt file,'
      WRITE(*,*)'use MATLAB with m file gshi2.m.'
      WRITE(*,*)'Input 1 to continue.'
      READ(*,*)N75
      IF(N75.EQ.1)CONTINUE
      WRITE(*,*)'To create a file named gshift3.plt for only the'
      WRITE(*,*)'shifted spectrum, input 1.  To also change all'
      WRITE(*,*)'negative values to zero, input 2.'
      READ(*,*)N77
      IF(N77.EQ.1)THEN
        OPEN(UNIT=4,FILE='gshift3.plt',STATUS='UNKNOWN')
        DO 1505 I=1,NLINES
          WRITE(*,*)'I=',I,' FCNTS(I)=',FCNTS(I)
          WRITE(4,*)I,FCNTS(I)
 1505     CONTINUE
        CLOSE(UNIT=4)  
        ENDIF
      IF(N77.EQ.2)THEN
        OPEN(UNIT=4,FILE='gshift3.plt',STATUS='UNKNOWN')
        DO 1510 I=1,NLINES
          IF(FCNTS(I).LE.ZERO)FCNTS(I)=ZERO
          WRITE(*,*)'I=',I,' FCNTS(I)=',FCNTS(I)
          WRITE(4,*)I,FCNTS(I)
 1510   CONTINUE
        CLOSE(UNIT=4) 
        ENDIF
      WRITE(*,*)'The program has finished.'  
      STOP
      END
C
C
      SUBROUTINE MODEL(MOD,CI,E,EP,DERF)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION CI(10,15)
      ONE=1.0E+00
      TWO=2.0E+00
      THREE=3.0E+00
      FOUR=4.0E+00
      FIVE=5.0E+00
      IF(MOD.EQ.1)GO TO 100
      IF(MOD.EQ.2)GO TO 200
      IF(MOD.EQ.3)GO TO 300
      IF(MOD.GT.3.OR.MOD.LT.1)GO TO 400
 100  CONTINUE
C
C     This is the full energy nonlinear relationship E*f(E) for NaI 
C     detectors.
C
      AK=CI(1,1)
      C1=CI(1,2)
      C2=CI(1,3)
      C3=CI(1,4)
      C4=CI(1,5)
      EP=C1*E*(1.-EXP(-AK*E))+C2*E+C3*E*E+C4*E*E*E
      DERF=C1-C1*EXP(-AK*E)+C1*E*AK*EXP(-AK*E)
      DERF=DERF+C2+TWO*C3*E+THREE*C4*E*E
      WRITE(*,*)'E=',E,' EP=',EP,' DERF=',DERF
      RETURN
 200  CONTINUE
C
C     This is a fifth degree polynomial E*f(E) with no constant.  It can 
C     be used for any polynomial up to a fifth degree by using the 
C     appropriate number of zeroes for the coefficients.  It can be
C     used for Monte Carlo calculated results with only a value for
C     CI(1).
C
      EP=CI(2,1)*E+CI(2,2)*E*E+CI(2,3)*E**3+CI(2,4)*E**4+CI(2,5)*E**5
      DERF=CI(2,1)+TWO*CI(2,2)*E+THREE*CI(2,3)*E*E+FOUR*CI(2,4)*E**3
      DERF=DERF+FIVE*CI(2,5)*E**4
      WRITE(*,*)'E=',E,' EP=',EP,' DERF=',DERF
      RETURN
 300  CONTINUE
C
C     This is the simple directly proportional model that is used for
C     spectra generated by Monte Carlo simulation.
C
      EP=ONE*E
      DERF=ONE
      RETURN
 400  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE RTNI(MOD,CI,X,F,DERF,XST,EPS,IEND,IER)
C
C     See IBM Scientific Subroutine Package page 220 for a description
C     of this subroutine.  It has been slightly modified -- F is taken
C     as F-XST in both places that F is obtained.  This is done because
C     the F(X) is not 0 as it should be unless this is done. 
C
      IMPLICIT REAL*8(A-H,O-Z) 
      DIMENSION CI(10,15)
      ONE=1.0E+00
      IER=0
      X=XST
      TOL=X
      CALL MODEL(MOD,CI,TOL,F,DERF)
C     The next step is a modification.
      F=F-XST
      TOLF=100.*EPS
C
C     Start iteration loop.
C
      DO 6 I=1,IEND
        WRITE(*,*)'I=',I,' X=',X,' F=',F,' DERF=',DERF
        IF(F)1,7,1
C
C       Equation is not satisfied by x.
C
 1      IF(DERF)2,8,2
C
C       Iteration is possible.
C
 2      DX=F/DERF
        X=X-DX
        TOL=X
        CALL MODEL(MOD,CI,TOL,F,DERF)
C     The next step is a modification.
        F=F-XST
C
C       Test on satisfactory accuracy.
C
        TOL=EPS
        A=ABS(X)
        IF(A-ONE)4,4,3
 3      TOL=TOL*A
 4      IF(ABS(DX)-TOL)5,5,6
 5      IF(ABS(F)-TOLF)7,7,6
 6      CONTINUE 
C
C       End of iteration loop.
C
C       No convergence after IEND iteration steps. Error return.
C
        IER=1
 7      RETURN
C
C       Error return in case of zero divisor.
C
 8      IER=2
        RETURN
        END
 
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
      WRITE(*,*)'     with apostrophes and eliminate existing'
	WRITE(*,*)'     apostrophes.'
      WRITE(*,*)'     This is the gshi1.m file.'
      WRITE(*,*)'     clf'
	WRITE(*,*)'	    clear all'
      WRITE(*,*)'     load gshift1.plt'
      WRITE(*,*)'     x=gshift1(:,1);'
      WRITE(*,*)'     y1=gshift1(:,2);'
      WRITE(*,*)'     y2=gshift1(:,3);'
      WRITE(*,*)'     y3=gshift1(:,4);'
      WRITE(*,*)'     y4=gshift1(:,5);'
      WRITE(*,*)'     semilogy(x,y1,x,y2,x,y3,x,y4)'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("CHANNEL ENERGY")'
      WRITE(*,*)'     legend("EPOU(I)","EOU(I)","EPFU(I)",EFU(I)")'
      WRITE(*,*)'     orient landscape'       
      WRITE(*,*)''
      WRITE(*,*)'     This is the gshi2.m file.'
      WRITE(*,*)'     clf'
	WRITE(*,*)'     clear all'
      WRITE(*,*)'     load gshift2.plt'
      WRITE(*,*)'     x=gshift2(:,1);'
      WRITE(*,*)'     y1=gshift2(:,2);'
      WRITE(*,*)'     y2=gshift2(:,3);'
      WRITE(*,*)'     semilogy(x,y1,x,y2)'
      WRITE(*,*)'     xlabel("CHANNEL")'
      WRITE(*,*)'     ylabel("COUNTS PER CHANNEL")'
      WRITE(*,*)'     legend("Original","Shifted")'
      WRITE(*,*)'     orient landscape'
      RETURN
      END