PROGRAM GSHIFT
!     This is Program GSHIFT.F, a general gain and zero shifting
!     program.


      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION OCNTS(17000),FCNTS(17000)
      DIMENSION CI(10,15)
      DIMENSION EOU(17000),EOL(17000),EPOU(17000),EPOL(17000)
      DIMENSION EFU(17000),EFL(17000),EPFU(17000),EPFL(17000)
      DIMENSION XINT(10,17000)
      CHARACTER*20 FILE1

!     Constants are set.
	  EMIN=0.d0
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
      
	  WRITE(*,*)'Press ENTER to continue.'
		READ(*,*)

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
      IF(N36==1)THEN
        CALL MFILES
        ENDIF
      
	  WRITE(*,*)'Press ENTER to continue.'
		READ(*,*)
	   
      WRITE(*,*)
      WRITE(*,*)'Now the original spectrum is read in.  What is'
      WRITE(*,*)'the file name of this spectrum?'
      READ(*,101)FILE1
 101  FORMAT(A20)
      NCOLS=2
      CALL FLERDN(FILE1,NLINES,NCOLS,XINT)
	
		DO I=1,NLINES
			OCNTS(I)=XINT(2,I)
		END DO
     
	  WRITE(*,*)'NLINES=',NLINES
      WRITE(*,*)'To increase the total number of channels'
      WRITE(*,*)'input 1 - otherwise 0.'
      READ(*,*)N78
      IF(N78==1)THEN
        WRITE(*,*)'What is the total desired number of channels?'
        READ(*,*)NOCHAN
        WRITE(*,*)'To place zeroes in all channels from NLINES to'
        WRITE(*,*)'NOCHAN, input 1 - otherwise 0.'
        READ(*,*)NZERO
		IF(NZERO==1)THEN
			NLIN1=NLINES+1
			DO I=NLIN1,NOCHAN
 				OCNTS(I)=0.d0
			END DO
		ENDIF
        NLINES=NOCHAN  
      ENDIF
      
		DO I=1,NLINES,10
			WRITE(*,*)'I=',I,' OCNTS(I)=',OCNTS(I)
 		END DO
 
      WRITE(*,*)'What is the model number MODO for this data?'
      WRITE(*,*)'MODO=1 is nonlinear model for NaI'
      WRITE(*,*)'MODO=2 is fifth degree polynomial'
      WRITE(*,*)'MODO=3 is directly proportional model for'
      WRITE(*,*)'       Monte Carlo generated spectra'
	WRITE(*,*)'MODO=4 is linear model'
      READ(*,*)MODO
      IF(MODO==3)THEN
        WRITE(*,*)'What is the single peak energy E01 in MeV'
        WRITE(*,*)'and peak channel number XN01 for MODO=3?'
        READ(*,*)E01,XN01
        AO=XN01/E01
        BO=0.d0
        WRITE(*,*)'AO=',AO,' BO=',BO
        WRITE(*,*)'Press ENTER to continue.'
			READ(*,*)
        
		DO I=1,NLINES
			XI=I
			XI=XI+0.5d0
			EPOU(I)=XI/AO
			EOU(I)=EPOU(I)
 		END DO
        EPOL(1)=0.5d0/AO
        EOL(1)=EPOL(1)
        DO I=2,NLINES
			EPOL(I)=EPOU(I-1)
			EOL(I)=EPOL(I)
		END DO
      ENDIF

      IF(MODO==1.OR.MODO==2)THEN
		IF(MODO==2)THEN
			WRITE(*,*)'What is CI(2,I) for I=1,5 for MODO=2?'
			READ(*,*)CI(2,1),CI(2,2),CI(2,3),CI(2,4),CI(2,5)
        END IF

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
      WRITE(*,*)'Press ENTER to continue.'
	      READ(*,*)
      WRITE(*,*)'Next the pulse-height and true upper and lower'
      WRITE(*,*)'channel energy limits EPOU(I) and EPOL(I) and'
      WRITE(*,*)'EOU(I) and EOL(I) are obtained from the model'
      WRITE(*,*)'relationship of Eq. 2 for the original spectrum.'
    
		DO I=1,NLINES
			XI=I
			XI=XI+0.5d0
			EPOU(I)=(XI-BO)/AO
			IF(EPOU(I).LE.EMIN.OR.EPOU(I).GE.EMAX)EOU(I)=0.d0
			IF(EPOU(I).LE.EMIN.OR.EPOU(I).GE.EMAX)GO TO 10
			EST=EPOU(I)
			CALL RTNI(MODO,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
			EOU(I)=EDUM
 		END DO
 10		CONTINUE
      XI=0.5d0
      EPOL(1)=(XI-BO)/AO
      IF(EPOL(1).LE.EMIN.OR.EPOL(1).GE.EMAX)EOL(1)=0.d0
      IF(EPOL(1).LE.EMIN.OR.EPOL(1).GE.EMAX)GO TO 11
      EST=EPOL(1)
      CALL RTNI(MODO,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
      EOL(1)=EDUM
 11   CONTINUE 
 
		DO I=2,NLINES
			EPOL(I)=EPOU(I-1)
			EOL(I)=EOU(I-1)
		END DO
      ENDIF

		DO I=1,NLINES,10  
			WRITE(*,*)'I=',I,' EPOL(I)=',EPOL(I),' EOL(I)=',EOL(I)
			WRITE(*,*)'EPOU(I)=',EPOU(I),' EOU(I)=',EOU(I)
		END DO
 
	  WRITE(*,*)'What is the model number MODF for the final shifted'
      WRITE(*,*)'spectrum?'
      WRITE(*,*)'MODF=1 is nonlinear model for NaI'
      WRITE(*,*)'MODF=2 is fifth degree polynomial'
      WRITE(*,*)'MODF=3 is directly proportional model for'
      WRITE(*,*)'       Monte Carlo generated spectra'
	  WRITE(*,*)'MODF=4 is linear model'
		READ(*,*)MODF
	
	IF(MODF==2)THEN
        WRITE(*,*)'What is CI(2,I) for I=1,5 for MODF=2?'
			READ(*,*)CI(2,1),CI(2,2),CI(2,3),CI(2,4),CI(2,5)
	ELSE IF(MODF==3)THEN
        WRITE(*,*)'What is the single peak energy E01 in MeV'
        WRITE(*,*)'and peak channel number XN01 for MODF=3?'
			READ(*,*)E01,XN01
        AF=XN01/E01
        BF=0.d0
        WRITE(*,*)'AF=',AF,' BF=',BF
        WRITE(*,*)'Press ENTER to continue.'
			READ(*,*)
        DO I=1,NLINES
			XI=I
			XI=XI+0.5
			EPFU(I)=XI/AF
			EFU(I)=EPFU(I)
 		END DO
        EPFL(1)=0.5/AF 
        EFL(1)=EPFL(1)
        DO I=2,NLINES
			EPFL(I)=EPFU(I-1)
			EFL(I)=EPFL(I)
		END DO
	ELSE IF(MODF==4)THEN
		WRITE(*,*)'What are EF1 and EF2, the final two energies in MeV?'
			READ(*,*)EF1,EF2
		WRITE(*,*)'What are the final two channel numbers N1 and N2?'
			READ(*,*)N1,N2
	END IF
      
    IF(MODF==1)THEN
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
		WRITE(*,*)'Press ENTER to continue.'
			READ(*,*)
		WRITE(*,*)'Next the pulse-height and true upper and lower'
		WRITE(*,*)'channel energy limits EPFU(I) and EPFL(I) and'
		WRITE(*,*)'EFU(I) and EFL(I) are obtained from the model'
		WRITE(*,*)'relationship of Eq. 2 for the shifted spectrum.'

		DO I=1,NLINES 
			XI=I
			XI=XI+0.5d0
			EPFU(I)=(XI-BF)/AF
			IF(EPFU(I).LE.EMIN.OR.EPFU(I).GE.EMAX)EFU(I)=0.d0
			IF(EPFU(I).LE.EMIN.OR.EPFU(I).GE.EMAX) EXIT
			EST=EPFU(I)
			CALL RTNI(MODF,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
			EFU(I)=EDUM
		END DO

		XI=0.5d0
		EPFL(1)=(XI-BF)/AF
		IF(EPFL(1).LE.EMIN.OR.EPFL(1).GE.EMAX)EFL(1)=0.d0
		IF(EPFL(1).LE.EMIN.OR.EPFL(1).GE.EMAX) THEN
		
		ELSE
			EST=EPFL(1)
			CALL RTNI(MODF,CI,EDUM,F,DERF,EST,EPS,IEND,IER)
			EFL(1)=EDUM
		END IF
    
		DO I=2,NLINES
			EPFL(I)=EPFU(I-1)
			EFL(I)=EFU(I-1)
		END DO
	END IF

    DO I=1,NLINES,10
		WRITE(*,*)'I=',I,' EPFL(I)=',EPFL(I),' EFL(I)=',EFL(I)
        WRITE(*,*)'EPFU(I)=',EPFU(I),' EFU(I)=',EFU(I)
	END DO

    WRITE(*,*)'To store this data in a file, input 1.'
    WRITE(*,*)'The file name for storing the EPOU(I),'
    WRITE(*,*)'EOU(I), EPFU(I), and EFU(I) is gshift1.plt.'
		READ(*,*)N78
	IF(N78==1)THEN
		OPEN(UNIT=2,FILE='gshift1.plt',STATUS='UNKNOWN')
		DO I=1,NLINES
			WRITE(*,*)'I=',I,' EPOU(I)=',EPOU(I),' EOU(I)=',EOU(I)
			WRITE(*,*)'      EPFU(I)=',EPFU(I),' EFU(I)=',EFU(I)
			WRITE(2,102)I,EPOU(I),EOU(I),EPFU(I),EFU(I)
		END DO
		CLOSE(UNIT=2)
	END IF

	102  FORMAT(I5,2X,4(E12.6,2X))
    WRITE(*,*)'To look at a plot of this input 1.'
		READ(*,*)N83
    IF(N83==1) WRITE(*,*)'Use gshi1.m in MATLAB to examine plot.'
        
    WRITE(*,*)'Now the counts are shifted.'
    
	WRITE(*,*)'Press ENTER to continue.'
		READ(*,*)

    DO I=1,NLINES
        FCNTS(I)=0.d0
	END DO

    DO I=1,NLINES

!       I's will be used for the final spectrum channels and energies
!       while J's will be used for the original.

        WRITE(*,*)'I=',I
        JMIN=0
        JMAX=0
        DO J=1,NLINES
			EDUM=EFL(I)
			IF(EDUM.GE.EOL(J).AND.EDUM.LE.EOU(J))THEN
				JMIN=J
				EXIT
            END IF
		END DO

        DO J=1,NLINES
			EDUM=EFU(I)
			IF(EDUM.GE.EOL(J).AND.EDUM.LE.EOU(J))THEN
				JMAX=J
				EXIT 
            END IF
		END DO

        IF(JMIN==0.AND.JMAX==0)GO TO 1000
        IF(JMIN==0.AND.JMAX.NE.0)JMIN=1
        IF(JMIN.NE.0.AND.JMAX==0)JMAX=NLINES
        IF(JMIN==JMAX)THEN
			TOP=EFU(I)-EFL(I)
			IF(TOP.LE.0.d0)TOP=0.d0
			BOT=EOU(JMIN)-EOL(JMIN)
			IF(BOT.LE.0.d0)BOT=0.d0
			IF(BOT.GT.0.d0)FCNTS(I)=FCNTS(I)+OCNTS(JMIN)*TOP/BOT
        END IF
        IF(JMIN.NE.JMAX)THEN
			TOPL=EOU(JMIN)-EFL(I)
			IF(TOPL.LE.0.d0)TOPL=0.d0
			BOTL=EOU(JMIN)-EOL(JMIN)
			IF(BOTL.LE.0.d0)BOTL=0.d0
			IF(BOTL.GT.0.d0)FCNTS(I)=FCNTS(I)+OCNTS(JMIN)*TOPL/BOTL
			TOPU=EFU(I)-EOL(JMAX)
			IF(TOPU.LE.0.d0)TOPU=0.d0
			BOTU=EOU(JMAX)-EOL(JMAX)
			IF(BOTU.LE.0.d0)BOTU=0.d0
			IF(BOTU.GT.0.d0)FCNTS(I)=FCNTS(I)+OCNTS(JMAX)*TOPU/BOTU
			JCHK=JMAX-JMIN
			IF(JCHK.GT.1)THEN
				JMIN1=JMIN+1
				JMAX1=JMAX-1
			
				IF(JMIN1==JMAX1)FCNTS(I)=FCNTS(I)+OCNTS(JMIN1)
				IF(JMIN1.NE.JMAX1)THEN
					DO JJ=JMIN1,JMAX1
						FCNTS(I)=FCNTS(I)+OCNTS(JJ)
					END DO
				END IF
			END IF
		END IF
    
		WRITE(*,*)'FCNTS(I)=',FCNTS(I)

	END DO
1000 CONTINUE
    
	SUMO=0.d0
    SUMF=0.d0
	DO I=1,NLINES
        SUMO=SUMO+OCNTS(I)
        SUMF=SUMF+FCNTS(I)
	END DO
    WRITE(*,*)'SUMO=',SUMO,' SUMF=',SUMF

!     An output file can be obtained here.

      WRITE(*,*)'To store in a file input 1.'
      WRITE(*,*)'The file name used is gshift2.plt.'
      READ(*,*)N1
      IF(N1==1)THEN
        OPEN(UNIT=3,FILE='gshift2.plt',STATUS='UNKNOWN')
        DO I=1,NLINES
          WRITE(3,*)I,OCNTS(I),FCNTS(I)
		END DO

        CLOSE(UNIT=3)
      END IF

    WRITE(*,*)'To look at a plot of the gshift2.plt file, use MATLAB with m file <gshi2.m>.'	!?????????
	WRITE(*,*)
	WRITE(*,*)'The file <gshift3.plt> will contain only the shifted spectrum.'
	WRITE(*,*)'INPUT 1 to change all negative values to zero, else 0.'
		READ(*,*)N77
	
	OPEN(UNIT=4,FILE='gshift3.plt',STATUS='UNKNOWN')
	DO I=1,NLINES
		IF((N77==1) .and.(FCNTS(I).LE.0.d0)) FCNTS(I)=0.d0
		WRITE(*,*)'I=',I,' FCNTS(I)=',FCNTS(I)
		WRITE(4,*)I,FCNTS(I)
	END DO
	CLOSE(4)
		
	WRITE(*,*)
	WRITE(*,*)'The program has finished. Press ENTER to exit.'  
		READ(*,*)

END PROGRAM GSHIFT


SUBROUTINE MODEL(MOD,CI,E,EP,DERF)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION CI(10,15)
     
	IF(MOD==1) THEN	!This is the full energy nonlinear relationship E*f(E) for NaI detectors.
		AK=CI(1,1)
		C1=CI(1,2)
		C2=CI(1,3)
		C3=CI(1,4)
		C4=CI(1,5)
		EP=C1*E*(1.-EXP(-AK*E))+C2*E+C3*E*E+C4*E*E*E
		DERF=C1-C1*EXP(-AK*E)+C1*E*AK*EXP(-AK*E)
		DERF=DERF+C2+2.d0*C3*E+3.d0*C4*E*E
	ELSE IF(MOD==2) THEN
		!This is a fifth degree polynomial E*f(E) with no constant.  It can 
		!be used for any polynomial up to a fifth degree by using the 
		!appropriate number of zeroes for the coefficients.  It can be
		!used for Monte Carlo calculated results with only a value for
		!CI(1).

		EP=CI(2,1)*E+CI(2,2)*E*E+CI(2,3)*E**3+CI(2,4)*E**4+CI(2,5)*E**5
		DERF=CI(2,1)+2.d0*CI(2,2)*E+3.d0*CI(2,3)*E*E+4.d0*CI(2,4)*E**3
		DERF=DERF+5.d0*CI(2,5)*E**4
	ELSE IF(MOD==3) THEN
		!This is the simple directly proportional model that is used for
		!spectra generated by Monte Carlo simulation.

		EP=1.d0*E
		DERF=1.d0
    ELSE
    END IF
	WRITE(*,*)'E=',E,' EP=',EP,' DERF=',DERF
END SUBROUTINE MODEL


SUBROUTINE RTNI(MOD,CI,X,F,DERF,XST,EPS,IEND,IER)

!     See IBM Scientific Subroutine Package page 220 for a description
!     of this subroutine.  It has been slightly modified -- F is taken
!     as F-XST in both places that F is obtained.  This is done because
!     the F(X) is not 0 as it should be unless this is done. 

      IMPLICIT REAL*8(A-H,O-Z) 
      DIMENSION CI(10,15)
      ONE=1.0E+00
      IER=0
      X=XST
      TOL=X
      CALL MODEL(MOD,CI,TOL,F,DERF)
!     The next step is a modification.
      F=F-XST
      TOLF=100.*EPS

!     Start iteration loop.

      DO 6 I=1,IEND
        WRITE(*,*)'I=',I,' X=',X,' F=',F,' DERF=',DERF
        IF(F)1,7,1

!       Equation is not satisfied by x.

 1      IF(DERF)2,8,2

!       Iteration is possible.

 2      DX=F/DERF
        X=X-DX
        TOL=X
        CALL MODEL(MOD,CI,TOL,F,DERF)
!     The next step is a modification.
        F=F-XST

!       Test on satisfactory accuracy.

        TOL=EPS
        A=ABS(X)
        IF(A-1.d0)4,4,3
 3      TOL=TOL*A
 4      IF(ABS(DX)-TOL)5,5,6
 5      IF(ABS(F)-TOLF)7,7,6
 6      CONTINUE 

!       End of iteration loop.

!       No convergence after IEND iteration steps. Error return.

        IER=1
 7      RETURN

!       Error return in case of zero divisor.

 8      IER=2

END SUBROUTINE
 


SUBROUTINE FLERDN(FILE1,NLINES,NCOLS,XINT)
	!This subroutine reads in a file that contains any specified 
	!number NCOLS of columns (up to 10) and any unspecified number of 
	!lines NLINES that is determined by the program.

	IMPLICIT NONE
	CHARACTER(20),INTENT(IN):: FILE1
	INTEGER,INTENT(IN)::NCOLS
	INTEGER:: I,J
	INTEGER,INTENT(OUT)::NLINES
	REAL(8),INTENT(OUT)::XINT(25,17000)
	
	OPEN(UNIT=1,FILE=FILE1,STATUS='OLD')
	I=0

	!The value of NLINES should correspond to the number of lines and is determined by the program.
	!The value of NCOLS is read in and should correspond to the number of columns in the input file.

	DO
		I=I+1
		READ(1,*,END=20)(XINT(J,I),J=1,NCOLS)
	END DO
	20 CONTINUE

	CLOSE(UNIT=1)
	NLINES=I-1

END SUBROUTINE FLERDN



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
END SUBROUTINE MFILES