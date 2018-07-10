Program PEAKSI
	!Modified by Johanna Peeples
	!September 2009

	IMPLICIT NONE
	REAL(8):: X(25,17000),Y(17000),SIGMAY(17000),A(25),AU(25),AL(25)
	REAL(8):: XX(25,17000),YY(17000),BG(17000),RES(17000)
	REAL(8):: DELTAA(25),YFIT(17000),SIGMAA(25),R(25),XINT(10,17000)
	REAL(8):: A1,A2,SIG,DET3,RELRAT,CHIZRO,CHISQR,BGTOT
	REAL(8):: FLAMDE,CHICUT,DEL
	INTEGER:: NOPEAK,NBG,NTYPE,I,NTERMS,XI,NPTS,INCMAX,ILIM,NLS,W1
	INTEGER:: NSEARC,IC,NCOLS=2,NOCHAN,IMIN,IMAX,N23,N1,N11,N72,MODE,NDERIV=1
	INTEGER:: IPRIN
	CHARACTER(20):: FILE1
	INTEGER::NOPT=0				!Option for 2 peak analysis
	CHARACTER(1)::LOPT='x'		!Option for single peak analysis

	WRITE(*,*)'Revised 09/2009'
	WRITE(*,*)''
	WRITE(*,*)'    This program (PEAKSI.F) obtains a fit with'
	WRITE(*,*)'experimental data for either a single resolved'
	WRITE(*,*)'Gaussian peak or two unresolved Gaussian peaks'
	WRITE(*,*)'plus a constant, linear, or quadratic background.'
	WRITE(*,*)'     Either a differential or integrated'
	WRITE(*,*)'Gaussian for each channel can be employed and'
	WRITE(*,*)'the peak channel(s) and resolution can either'
	WRITE(*,*)'be fixed or searched on.'
	WRITE(*,*)'     The data file MUST have pairs of channel'
	WRITE(*,*)'number and counts per channel on all lines.'
	WRITE(*,*)'     If peak fitting will be performed using user'
	WRITE(*,*)'defined weighting (not poisson, nor no weighting),'
	WRITE(*,*)'weight values MUST occur in the third column of '
	WRITE(*,*)'the data file.'
	WRITE(*,*)'     Both the experimental data and a comparison'
	WRITE(*,*)'of the experimental data and the derived fit'
	WRITE(*,*)'can be plotted and looked at in MATLAB.'  
	!WRITE(*,*)'The "m" files can be examined by inputting 1 now.'
	WRITE(*,*)
	WRITE(*,*)
   
    !READ(*,*)N23
	!IF(N23==1)THEN
	!	CALL MFILES
    !END IF
	WRITE(*,'(A)',ADVANCE="NO")'Input 1 to continue:  '
		READ(*,*)N1

	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,'(A)',ADVANCE="NO")'If peak fitting will be performed using user defined weighting, Input 1:  '
		READ(*,*)W1
		IF (W1==1) THEN
			WRITE(*,*)
			WRITE(*,*)
			WRITE(*,*)'*** The values for the weights SIGMAY(I) '
			WRITE(*,*)'    MUST appear in column 3 of the data file.***'
		END IF
DO
	WRITE(*,*)
	WRITE(*,*)
	WRITE(*,'(A)',ADVANCE="NO")'What is the name of the data file FILE1?  '
	READ(*,*) FILE1
	
	IF (W1==1) THEN		!Sigmay in column 3
		NCOLS=3
		CALL FLERDN(FILE1,NOCHAN,NCOLS,XINT)
		DO I=1,NOCHAN
			XX(1,I)=XINT(1,I)
			YY(I)=XINT(2,I)
			SIGMAY(I)=XINT(3,I)
			WRITE(*,101)'XX(1,I)=',INT(XX(1,I)),'YY(I)=',INT(YY(I)),'SIGMAY(I)=',SIGMAY(I)
			101 FORMAT(5x,A8,x,I6,5x,A6,x,I8,x,A10,E10.2)
		END DO	
	ELSE
		NCOLS=2
		CALL FLERDN(FILE1,NOCHAN,NCOLS,XINT)
		DO I=1,NOCHAN
			XX(1,I)=XINT(1,I)
			YY(I)=XINT(2,I)
			WRITE(*,100)'XX(1,I)=',INT(XX(1,I)),'YY(I)=',INT(YY(I))
			100 FORMAT(5x,A8,x,I6,5x,A6,x,I8)
		END DO
	END IF

	DO
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,'(A)',ADVANCE="NO")'To plot the data in MATLAB input 1:  '
			READ(*,*)N11
			IF(N11==1)THEN
				WRITE(*,*)'Specify the minimum and maximum channel numbers' 
				WRITE(*,'(A)',ADVANCE="NO")' to include in the plot:  '
					READ(*,*)IMIN,IMAX
				WRITE(*,*)
				WRITE(*,*)'The data is stored in a file named peaksi1.plt.'
				OPEN(UNIT=1,FILE='peaksi1.plt',STATUS='UNKNOWN')
				DO  I=IMIN,IMAX
					WRITE(*,*)'XX(1,I)=',XX(1,I),' YY(I)=',YY(I)
					WRITE(1,*)XX(1,I),YY(I)
				END DO	
				CLOSE(UNIT=1)
				WRITE(*,*)'Use peak1.m in MATLAB to see plot.'
			END IF
	
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'What are the minimum and maximum channel numbers'
		WRITE(*,'(A)',ADVANCE="NO")' to be used in the peak(s) determination?  '
			READ(*,*)IMIN,IMAX
		
		DO I=IMIN,IMAX
			IC=I-IMIN+1
			Y(IC)=YY(I)
			X(1,IC)=XX(1,I)
			WRITE(*,162)IC,X(1,IC),Y(IC)
			162 FORMAT(2X,'IC=',I5,' X(1,IC)=',E14.6,' Y(IC)=',E14.6)
		END DO

		DO 
			WRITE(*,*)
			WRITE(*,*)
			WRITE(*,'(A)',ADVANCE="NO")'Input number of peaks to be examined (1 or 2): '
			READ(*,*)NOPEAK
			IF ((NOPEAK==1) .or. (NOPEAK==2)) EXIT
		END DO

		IF (NOPEAK==1) THEN
			WRITE(*,*)
			WRITE(*,*)
			WRITE(*,*)'--------------------------------------------------------------------'
			WRITE(*,*)'The options are:'
			WRITE(*,*)'    (a) Search on both the peak and the standard deviation.'
			WRITE(*,*)'    (b) Search on the peak and specify a fixed standard deviation.'
			WRITE(*,*)'    (c) Specify both the fixed peak and fixed standard deviation.'
			WRITE(*,*)'--------------------------------------------------------------------'
			WRITE(*,*)
			WRITE(*,*)

				DO	
					WRITE(*,'(A)',ADVANCE="NO")'Enter the desired option:  '
					READ(*,*)LOPT
					IF((LOPT=='a') .or. (LOPT=='b') .or. (LOPT=='c')) EXIT
					WRITE(*,*)
				END DO

		ELSE IF (NOPEAK==2) THEN
			WRITE(*,*)
			WRITE(*,*)
			WRITE(*,*)'------------------------------------------------------------------------'
			WRITE(*,*)'The options are:'
			WRITE(*,*)'    (1) Search on both peaks and the standard deviation.'
			WRITE(*,*)'    (2) Search on both peaks and specify a fixed standard deviation.'
			WRITE(*,*)'    (3) Search on one peak and specify both a fixed difference '
			WRITE(*,*)'           between the peaks and the standard deviation.'
			WRITE(*,*)'    (4) Specify both fixed peaks and search on the standard deviation.'
			WRITE(*,*)'    (5) Specify both fixed peaks and specify the standard deviation.'
			WRITE(*,*)'-------------------------------------------------------------------------'
			WRITE(*,*)
			WRITE(*,*)

				DO	
					WRITE(*,'(A)',ADVANCE="NO")'Enter the desired option:  '
					READ(*,*)NOPT
					IF((NOPT>0) .and. (NOPT<6)) EXIT
					WRITE(*,*)
				END DO
		END IF


		WRITE(*,*)
		IF (LOPT=='a') THEN
			NSEARC=2
			WRITE(*,'(A)',ADVANCE="NO")'What is the estimate of the single peak and the standard deviation? '
			READ(*,*)A(1),A(2)
		ELSE IF (LOPT=='b') THEN
			NSEARC=1
			WRITE(*,*)'What is the estimate of the single peak '
			WRITE(*,'(A)',ADVANCE="NO")' and the fixed standard deviation? '
			READ(*,*)A(1),SIG
		ELSE IF (LOPT=='c') THEN
			NSEARC=0
			WRITE(*,*)'What is the fixed value of the single peak '
			WRITE(*,'(A)',ADVANCE="NO")' and the fixed standard deviation? '
			READ(*,*)A1,SIG
		ELSE IF (NOPT==1) THEN
			NSEARC=3
			WRITE(*,'(A)',ADVANCE="NO")'What is the estimate of the two peaks and the standard deviation? '
			READ(*,*)A(1),A(2),A(3)
		ELSE IF (NOPT==2) THEN
			NSEARC=2
			WRITE(*,*)'What is the estimate of the two peaks '
			WRITE(*,'(A)',ADVANCE="NO")' and the fixed standard deviation?'
			READ(*,*)A(1),A(2),SIG
		ELSE IF (NOPT==3) THEN
			NSEARC=1
			WRITE(*,*)'What is the estimate of the lowest energy peak and the fixed difference '
			WRITE(*,'(A)',ADVANCE="NO")' between peaks and the fixed standard deviation? '
			READ(*,*)A(1),DEL,SIG
		ELSE IF (NOPT==4) THEN
			NSEARC=1
			WRITE(*,*)'What are the fixed values of the two peaks and '
			WRITE(*,'(A)',ADVANCE="NO")' the estimate of the standard deviation? '
			READ(*,*)A1,A2,A(1)
		ELSE IF (NOPT==5) THEN
			NSEARC=0
			WRITE(*,*)'What are the fixed values of the two peaks '
			WRITE(*,'(A)',ADVANCE="NO")' and the fixed standard deviation? '
			READ(*,*)A1,A2,SIG
		ELSE
		END IF

		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'------------------------------------------------------'
		WRITE(*,*)'Select the desired model for background:   '
		WRITE(*,*)'     (1) Constant background  '
		WRITE(*,*)'     (2) Linear background   '
		WRITE(*,*)'     (3) Quadratic background   '
		WRITE(*,*)'------------------------------------------------------'
		WRITE(*,*)
		WRITE(*,*)
		
		DO 
			WRITE(*,'(A)',ADVANCE="NO")'Input the desired option: '
			READ(*,*)NBG
			IF ((NBG>0) .and. (NBG<4)) EXIT
		END DO
			NLS=NOPEAK+NBG
			WRITE(*,*)'NLS=',NLS
		
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'For a differential Gaussian input 1, for an'
		WRITE(*,'(A)',ADVANCE="NO")' integrated Gaussian input 2:  '
		READ(*,*)NTYPE

		IC=10
		IPRIN=1
		ILIM=0
		INCMAX=10
		CHICUT=0.01
		FLAMDE=0.001

		IF(NSEARC>=1)THEN
			DO I=1,NSEARC
				DELTAA(I)=0.001*A(I)
			END DO
		END IF

		NPTS=IMAX-IMIN+1

		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'---------------------------------------------------------'
		WRITE(*,*)' MODE=+1: weighting uses SIGMAY(I)   '                  
		WRITE(*,*)' MODE= 0: weighting is unity    '
		WRITE(*,*)' MODE=-1: weighting is Poisson  '
		WRITE(*,*)' MODE=-2: weighting is the greater of Poisson and 1% '
		WRITE(*,*)'----------------------------------------------------------'
		WRITE(*,*)
		WRITE(*,'(A)',ADVANCE="NO")'Select a value of MODE (-2 is suggested) :'
		READ(*,*)MODE

		CALL CURMOD(IC,CHICUT,X,Y,SIGMAY,NPTS,NSEARC,NLS,MODE, &
			IPRIN,ILIM,INCMAX,NDERIV,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR, &
			CHIZRO,RELRAT,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)

		DO I=1,NPTS
			IF(Y(I)==0.d0) THEN
				SIGMAY(I)=1.d0
			ELSE
				SIGMAY(I)=DSQRT(abs(Y(I)))
			END IF
			RES(I)=(Y(I)-YFIT(I))/SIGMAY(I)
			WRITE(*,*)'I=',I,' Y(I)=',Y(I),' RES(I)=',RES(I)
		END DO

		NTERMS=NSEARC+NLS

		DO I=1,NTERMS
			SIGMAA(I)=100.d0*SIGMAA(I)/A(I)
		END DO 

		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'This is data from Program PEAKSI.'
		WRITE(*,*)'This is data from file:',FILE1
		WRITE(*,*)'CHISQR,CHIZRO,MODE=',CHISQR,CHIZRO,MODE
		WRITE(*,*)'SIGMAA(I) is in %.'
		WRITE(*,*)'I, A(I), SIGMAA(I) in %, and R(I) are:'
		DO I=1,NTERMS
			WRITE(*,108)I,A(I),SIGMAA(I),R(I)
			108 FORMAT(1X,I5,2(3X,E12.6),' %',2X,E11.4)
		END DO

		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,'(A)',ADVANCE="NO")'To continue print out input 1:  '
			READ(*,*)N1
		
		WRITE(*,*)
		WRITE(*,*)
		WRITE(*,*)'RES(I) is RES(I)/SIGMAY(I).'
		WRITE(*,*)'I,X(1,I),Y(I),YFIT(I),RES(I) are:'
			DO I=1,NPTS
				WRITE(*,110)I,X(1,I),Y(I),YFIT(I),RES(I)
				110 FORMAT(1X,I5,5(2X,E12.6))
			END DO

		DO I=IMIN,IMAX
			XI=I
			IF(NSEARC==2) THEN
				IF(NLS==2) BG(I)=A(4)
				IF(NLS==3) BG(I)=A(4)+A(5)*XI
				IF(NLS==4) BG(I)=A(4)+A(5)*XI+A(6)*XI*XI
			ELSE IF (NSEARC==3) THEN
				IF(NLS==3)BG(I)=A(6)
				IF(NLS==4)BG(I)=A(6)+A(7)*XI
				IF(NLS==5)BG(I)=A(6)+A(7)*XI+A(8)*XI*XI
			ELSE IF(NSEARC==0)THEN
				IF(NOPEAK==1.AND.NBG==1)BG(I)=A(2)
				IF(NOPEAK==1.AND.NBG==2)BG(I)=A(2)+A(3)*XI
				IF(NOPEAK==1.AND.NBG==3)BG(I)=A(2)+A(3)*XI+A(4)*XI*XI
				IF(NOPEAK==2.AND.NBG==1)BG(I)=A(3)
				IF(NOPEAK==2.AND.NBG==2)BG(I)=A(3)+A(4)*XI
				IF(NOPEAK==2.AND.NBG==3)BG(I)=A(3)+A(4)*XI+A(5)*XI*XI
			END IF
		END DO

		BGTOT=0.d0
		DO I=IMIN,IMAX
			BGTOT=BGTOT+BG(I)
		END DO

		IF(NSEARC==2) WRITE(*,*)'The net peak area is:',A(3),' +/- ',SIGMAA(3),' %'     
		IF(NSEARC==3) WRITE(*,*)'The net area of peak 1 is:',A(4),'+/-',SIGMAA(4),' %'
		IF(NSEARC==3) WRITE(*,*)'The net area of peak 2 is:',A(5),'+/-',SIGMAA(5),' %'

		WRITE(*,*)'The total background counts in the channels'
		WRITE(*,*)'chosen are:',BGTOT
		WRITE(*,*)
		WRITE(*,'(A)',ADVANCE="NO")'To see a plot of the model vs. the data input 1:  '
		READ(*,*)N72
		IF(N72==1)THEN
			WRITE(*,*)'The plotting data is stored in peaksi2.plt.'
			OPEN(UNIT=2,FILE='peaksi2.plt',STATUS='UNKNOWN')
			DO I=1,NPTS
				WRITE(*,*)'X(1,I)=',X(1,I),' Y(I)=',Y(I),' YFIT(I)=',YFIT(I)
				WRITE(2,*)X(1,I),Y(I),YFIT(I)
			END DO
			CLOSE(UNIT=2)
		END IF
		
		WRITE(*,*)
		WRITE(*,*)'To see the plot use peak2.m in MATLAB.'
		WRITE(*,*)'For another peak input 1 - otherwise 0.'
			READ(*,*)N1
		IF(N1/=1) EXIT
	END DO
	
	WRITE(*,*)
	WRITE(*,*)'For another file input 1 - otherwise 0.'
	READ(*,*)N1
	IF(N1/=1) EXIT
END DO
	
END PROGRAM PEAKSI

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------

SUBROUTINE MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
	IMPLICIT NONE
	REAL(8):: X(25,17000),A(25),ZI(25),YFI,XINT,XU,XL,AVG
	INTEGER:: NOPEAK,NOPT,NTYPE,NLS,J,NSEARC,I
	REAL(8):: CON,A1,A2,SIG,DEL,XX,XINT1,XINT2,AVG1,AVG2
	INTEGER,INTENT(IN):: NBG
	CHARACTER(1),INTENT(IN):: LOPT
	REAL(8),PARAMETER:: PI=3.1415926535898d0

	CON=DSQRT(2.d0*PI)
	XX=X(1,I)
	NLS=NOPEAK+NBG

	IF (LOPT=='a') THEN		!1 peak
		NSEARC=2
		AVG=A(1)
		SIG=A(2)

		IF(NTYPE==1) THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
		ELSE IF (NTYPE==2) THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG,SIG,XINT)
			ZI(1)=XINT
		END IF
			
		ZI(2)=1.d0
		ZI(3)=XX
		ZI(4)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO
	
	ELSE IF (LOPT=='b') THEN	!1 peak
		NSEARC=1
		AVG=A(1)
		SIG=SIG

		IF(NTYPE==1) THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
		ELSE IF (NTYPE==2) THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG,SIG,XINT)
			ZI(1)=XINT
		END IF

		ZI(2)=1.d0
		ZI(3)=XX
		ZI(4)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO
	
	ELSE IF(LOPT=='c') THEN		!1 peak
		NSEARC=0
		AVG=A1
		SIG=SIG
		IF(NTYPE==1)THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG)*(XX-AVG)/(SIG*SIG))/(SIG*CON)
		ELSE IF(NTYPE==2)THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG,SIG,XINT)
			ZI(1)=XINT
		END IF

		ZI(2)=1.d0
		ZI(3)=XX
		ZI(4)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J)
		END DO
			
	ELSE IF (NOPT==1) THEN		!2 peaks
		NSEARC=3
		AVG1=A(1)
		AVG2=A(2)
		SIG=A(3)

		IF(NTYPE==1) THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
			ZI(2)=DEXP(-0.5d0*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
		ELSE IF(NTYPE==2) THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
			CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
			ZI(1)=XINT1
			ZI(2)=XINT2
		END IF

		ZI(3)=1.d0
		ZI(4)=XX
		ZI(5)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO

	ELSE IF (NOPT==2) THEN		!2 peaks
		NSEARC=2
		AVG1=A(1)
		AVG2=A(2)
		SIG=SIG

		IF(NTYPE==1)THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
			ZI(2)=DEXP(-0.5d0*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
		ELSE IF (NTYPE==2) THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
			CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
			ZI(1)=XINT1
			ZI(2)=XINT2
		END IF

		ZI(3)=1.d0
		ZI(4)=XX
		ZI(5)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO
	
	ELSE IF (NOPT==3) THEN		!2 peaks
		NSEARC=1
		AVG1=A(1)
		AVG2=A(1)+DEL
		SIG=SIG

		IF(NTYPE==1) THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
			ZI(2)=DEXP(-0.5d0*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
		ELSE IF (NTYPE==2) THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
			CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
			ZI(1)=XINT1
			ZI(2)=XINT2
		END IF

		ZI(3)=1.d0
		ZI(4)=XX
		ZI(5)=XX*XX
		YFI=0.d0

		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO
	
	ELSE IF (NOPT==4) THEN		!2 peaks
		NSEARC=1
		AVG1=A1
		AVG2=A2
		SIG=A(1)

		IF(NTYPE==1)THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
			ZI(2)=DEXP(-0.5d0*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
		ELSE IF(NTYPE==2)THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
			CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
			ZI(1)=XINT1
			ZI(2)=XINT2
		END IF

		ZI(3)=1.d0
		ZI(4)=XX
		ZI(5)=XX*XX
		YFI=0.d0
		DO J=1,NLS
			YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO

	ELSE IF( NOPT==5) THEN	!2 peaks
		NSEARC=0
		AVG1=A1
		AVG2=A2
		SIG=SIG

		IF(NTYPE==1)THEN
			ZI(1)=DEXP(-0.5d0*(XX-AVG1)*(XX-AVG1)/(SIG*SIG))/(SIG*CON)
			ZI(2)=DEXP(-0.5d0*(XX-AVG2)*(XX-AVG2)/(SIG*SIG))/(SIG*CON)
		ELSE IF (NTYPE==2)THEN
			XL=XX-0.5d0
			XU=XX+0.5d0
			CALL GAUSSM(XL,XU,AVG1,SIG,XINT1)
			CALL GAUSSM(XL,XU,AVG2,SIG,XINT2)
			ZI(1)=XINT1
			ZI(2)=XINT2
		END IF

		ZI(3)=1.d0
		ZI(4)=XX
		ZI(5)=XX*XX
		YFI=0.d0
		DO J=1,NLS
		  YFI=YFI+A(NSEARC+J)*ZI(J) 
		END DO
	
	END IF


END SUBROUTINE MODEL


!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------


	! SUBROUTINE GAUSSM.F
	! (REVISED 2/27/94)

SUBROUTINE GAUSSM(XL,XU,AVG,SIG,XINT)

	!This subroutine calculates the integral XINT from XL to XU of
	!a Gaussian distribution with mean AVG and standard deviation SIG.

	IMPLICIT NONE
	REAL(8)::XI1,XI2,XU,XL,AVG,SIG,XINT

	CALL AGAUSS(XU,AVG,SIG,XI1)
		XI1=XI1/2.d0
	CALL AGAUSS(XL,AVG,SIG,XI2)
		XI2=XI2/2.d0
	
	IF ((XL<=AVG) .AND. (XU>=AVG)) XINT=XI1+XI2
	IF ((XL<=AVG) .AND. (XU<=AVG)) XINT=XI2-XI1
	IF ((XL>=AVG) .AND. (XU>=AVG)) XINT=XI1-XI2

END SUBROUTINE GAUSSM

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------


SUBROUTINE AGAUSS(X,AVG,SIG,XINT)

	!This subroutine is adapted from the function given as Program
	!3-5 AGAUSS given on page 48 of Bevington.

	IMPLICIT NONE
	REAL(8):: C1,C2,C3,Z,X,AVG,SIG,XINT,TERM,SUM,Y2,DENOM,CHECK
	REAL(8),PARAMETER:: PI=3.1415926535898d0

	C1=1.d0/DSQRT(2.d0)
	C3=2.d0/DSQRT(PI)

    Z=DABS(X-AVG)/SIG

	XINT=0.d0
	
	IF(Z/=0.d0) THEN
		TERM=C1*Z
		SUM=TERM
		Y2=Z*Z/2.d0

		!IF(Y2>AMAX)RETURN should be added at this point when AMAX is known.

		C2=DEXP(-Y2)
		DENOM=1.d0
		
		DO
			DENOM=DENOM+2.d0
			TERM=TERM*Y2*2.d0/DENOM
			SUM=SUM+TERM
			CHECK=TERM/SUM-1.0d0-10.d0
			IF(CHECK<=0.d0) EXIT
		END DO

		XINT=C3*SUM*C2
	END IF

END SUBROUTINE AGAUSS

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------


SUBROUTINE FLERDN(FILE1,NLINES,NCOLS,XINT)
	!This subroutine reads in a file that contains any specified 
	!number NCOLS of columns (up to 10) and any unspecified number of 
	!lines NLINES that is determined by the program.

	IMPLICIT NONE
	CHARACTER(20),INTENT(IN):: FILE1
	INTEGER,INTENT(IN)::NCOLS
	INTEGER:: I,J
	INTEGER,INTENT(OUT)::NLINES
	REAL(8),INTENT(OUT)::XINT(10,17000)
	
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

!---------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------


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
END SUBROUTINE MFILES


!----------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------


SUBROUTINE CURMOD(IC,CHICUT,X,Y,SIGMAY,NPTS,NSEARC,NLS,MODE,	&
	IPRIN,ILIM,INCMAX,NDERIV,A,AU,AL,DELTAA,FLAMDE,YFIT,CHISQR,   &
	CHIZRO,RELRAT,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)

	!  	The search is stopped when NSEARC is zero; RELRAT is negative, zero, 
	!	or less than or equal to CHICUT; when the number of complete 
	!	searches exceeds IC; or in the special case when CHISQR becomes zero.

	IMPLICIT NONE 

	REAL(8):: X(20,17000),Y(17000),SIGMAY(17000),A(25),AU(25),AL(25)
	REAL(8):: DELTAA(25),YFIT(17000),SIGMAA(25),R(25)
    REAL(8):: WEIGHT(17000),ALPHA(25,25),BETA(25),DERIV(25),A1,A2,CON,DEL
	REAL(8):: ARRAY(25,25),B(25),ALPHU(25,25),ZI(25),RELRAT,CHISQ1
	REAL(8):: CHICUT,FLAMDE,FLAMDA,CHISQR,CHIZRO,CON1,CON2,DET,YFI 
	INTEGER:: I,J,IC,IPRIN,ILIM,INCMAX,INC,NTERMS,NSEARC,NLS,NFREE
	INTEGER:: NPTS,MODE,II,ISIG,K,NDERIV,NBG,NOPEAK,NOPT,NTYPE
	CHARACTER(1),INTENT(IN)::LOPT

	INC=0                                         
    NTERMS=NSEARC+NLS
	
	DO J=1,NTERMS
		R(J)=0.0   
	END DO  
	    
   NFREE = NPTS - NTERMS     
    
	IF(NFREE<=0.)THEN
		WRITE(*,'(A)') ' THERE IS INSUFFICIENT DATA.'
		NFREE=1
	END IF

! The weights are evaluated.

	DO I =1,NPTS
		IF (Y(I)==0.d0) THEN
			WEIGHT(I)=1.d0
		ELSE
			IF (MODE==1) THEN
				WEIGHT(I) = 1./SIGMAY(I)**2
			ELSE IF (MODE==0) THEN
				WEIGHT(I) = 1.d0
			ELSE IF (MODE==-1) THEN
				WEIGHT(I) = 1./abs(Y(I))
			ELSE IF (MODE==-2) THEN
				IF(abs(Y(I))<=10000.) THEN			
					WEIGHT(I)=1.d0/abs(Y(I))
				ELSE
					WEIGHT(I)=1.d0/(0.01d0*Y(I))**2
				END IF
			END IF
		END IF
	END DO

	FLAMDA=FLAMDE
	II=1
   
   10	CONTINUE                  

! The ALPHA and BETA matrices are evaluated.  If NSEARC is zero most of the rest is omitted.

	IF(NSEARC==0)THEN
		ISIG=2
		CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,A,ISIG,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)
		DO I=1,NPTS
			CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
   			YFIT(I)=YFI
		END DO
		CALL FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
	ELSE
    	DO J=1,NSEARC                           
      		BETA(J) = 0.0                               
      		DO K=1,J                                
    			ALPHA(J,K)=0.0
			END DO
		END DO
		ISIG=2                              
      	
		CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,A,ISIG,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)
    	
		DO I=1,NPTS 
      		CALL FDERIV(NDERIV,NSEARC,X,I,A,DELTAA,DERIV,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)
			CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT) 
      		DO J=1,NSEARC
      			BETA(J)=BETA(J)+WEIGHT(I)*(Y(I)-YFI)*DERIV(J) 
      			DO K=1,J                                      
			 		ALPHA(J,K)=ALPHA(J,K)+WEIGHT(I)*DERIV(J)*DERIV(K)
				END DO
			END DO
		END DO

    	DO J=1,NSEARC       
      		DO K=1,J            
	 			ALPHA(K,J)=ALPHA(J,K)
			END DO
		END DO

! The chi-square value at the starting point is evaluated.

		DO I=1,NPTS
			CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)   
    		YFIT(I) = YFI
		END DO
   
		CALL FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQ1)
   
		IF ((IPRIN-1)==0) THEN        
			write(*,'(A)')' THE A(I) AND CHISQ1 ARE'                             
      		117 FORMAT(4E15.6)                                
			write(*,117) (A(I),I=1,NTERMS),CHISQ1     
		ELSE                                 
		END IF
! The modified curvature matrix is inverted to find new parameters.

   71 	DO J=1,NSEARC                         
	   		DO K=1,NSEARC
				ALPHU(J,K)=DSQRT(ALPHA(J,J)*ALPHA(K,K))                         
    			ARRAY(J,K)=ALPHA(J,K)/ALPHU(J,K)
    		END DO
			ARRAY(J,J) = 1. + FLAMDA
		END DO
      	
		INC=INC + 1                      
    	CALL MATINV (ARRAY,NSEARC,DET)
    	DO J=1,NSEARC                 
      		B(J) = A(J)                      
      		DO K=1,NSEARC                 
    			B(J)=B(J)+BETA(K)*ARRAY(J,K)/ALPHU(J,K)
			END DO
		END DO

! The searched parameters are limited if ILIM is one.

		IF ((ILIM-1)==0) THEN            
      		DO J=1,NSEARC            
      			IF ((B(J)-AU(J))<=0.) THEN      
    				IF ((B(J)-AL(J))<0.) THEN
						B(J)=AL(J) 
						WRITE(*,'(A)')' ALERT. THE LOWER LIMIT OF'
						WRITE(*,118)J     
					ELSE
					END IF
				ELSE
					B(J)=AU(J)                  
      				WRITE(*,'(A)')' ALERT. THE UPPER LIMIT OF'                
       				118 FORMAT(' PARAMETER',I3,' IS BEING USED.')
  					WRITE(*,118)J 
    			END IF 
			END DO
		ELSE
		END IF                 

! If CHISQR increases FLAMDA is increased and the calculations are repeated.

		ISIG=2
      	CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,B,ISIG,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)
   
      	DO I=1,NPTS
			CALL MODEL(X,I,B,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)                                   
    		YFIT(I) = YFI
		END DO
    
		CALL FCHISQ (NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
      	
		IF ((IPRIN-1)==0) THEN                                  
    		write(*,'(A)')' B(J) AND CHISQR ARE'                                         
      		write(*,117)(B(J),J=1,NTERMS),CHISQR                                                              
      	ELSE
		END IF
		
		IF ((CHISQ1 - CHISQR)<0) THEN                      
			FLAMDA=10.*FLAMDA                                   				
			IF ((INC-INCMAX)<=0) THEN
				GOTO 71
			ELSE
			! The nonlinear parameters and their uncertainties are evaluated.
				write(*,'(A)')' ALERT. INCMAX HAS BEEN EXCEEDED.'                         
     			FLAMDA=FLAMDA/(10.**INCMAX)          
			END IF 
		ELSE
		END IF

		DO J=1,NSEARC
			A(J) = B(J)
		END DO
		FLAMDA=FLAMDA/10.

		IF(CHISQR==0.)THEN
			RELRAT=0.
			write(*,'(A)')'CHISQR was zero, it is reset to 0.000001.'
			CHISQR=0.000001
		END IF
    
		IF (CHISQR/=0.000001) RELRAT=(CHISQ1-CHISQR)/CHISQR
		
		IF ((IPRIN-1)==0) THEN
  			119 FORMAT(' CHISQR= ',E15.6)
			write(*,119)CHISQR
			write(*,'(A)')' THE A(I) ARE: '
			write(*,117)(A(I),I=1,NTERMS)
		ELSE
		END IF
  
		IF (RELRAT>0.) THEN 
			IF ((RELRAT-CHICUT)>0.) THEN
				IF ((II-IC)<=0) THEN 
					II=II+1
					GO TO 10
				ELSE
					write(*,'(A)')' ALERT.  IC HAS BEEN EXCEEDED.'
				END IF
			ELSE
			END IF
		ELSE
  		END IF		
	END IF
	
	CON1=1.
	CON2=1.        

!	When unit weighting (MODE=0) is used it is assumed that the
!	observed deviations are also the controlling theoretical deviations
!	and the reduced chi-square value is forced to be unity.  This makes
!	corresponding changes in the SIGMAA and R values.  The original
!	chi-square value is saved as CHIZRO in this case.

	IF(MODE==0)THEN
		CHIZRO=CHISQR
		DO I=1,NPTS
			WEIGHT(I)=1./CHISQR
		END DO
		CON1=1./CHISQR
		CON2=DSQRT(CHISQR)
	END IF
	
	IF(NSEARC/=0) THEN
	 	DO J=1,NSEARC                    
	 		SIGMAA(J)=CON2*DSQRT(ARRAY(J,J)/ALPHA(J,J))
		END DO
	ELSE
	END IF
  
  	CHISQR=CON1*CHISQR
	ISIG=1

!	The following use of the vector B instead of A is 
!	necessary when MODE is 0 so that different values
!	of the linear A are not calculated with the different
!	weights that are used.

	IF(NSEARC>=1)THEN
		DO I=1,NSEARC
  			B(I)=A(I)
		END DO
	ENDIF
	CALL LS(NPTS,NSEARC,NLS,X,Y,WEIGHT,B,ISIG,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)

!     The new added by Fusheng
		DO I=1,NTERMS
			A(I)=B(I)
		END DO 
END SUBROUTINE CURMOD

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------

SUBROUTINE LS(NPTS,NSEARC,NLS,X,Y,WT,A,ISIG,SIGMAA,R,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)

! This subroutine sets up the equations and solves for all parameters
! A that can be obtained by a linear least-squares method.  The
! parameters found are numbered from NSEARC +1 to NSEARC +NLS.

	IMPLICIT NONE
	REAL(8):: X(20,17000),Y(17000),WT(17000),A(25),SIGMAA(25),R(25) 
    REAL(8):: C(25,25),ZI(25),B(25),D(25),XMEAN(25),YFI,DET,YMEAN
	REAL(8):: WTSUM,XN1,SJY,SYY,SJJ,SY,SJ,A1,A2,DEL
    INTEGER:: NLS,J,K,I,NPTS,NSEARC,ISIG,XNPTS,NBG,NOPEAK,NOPT,NTYPE
	CHARACTER(1),INTENT(IN)::LOPT

	IF(NLS/=0) THEN

	! Evaluate the vector of left-hand-side coefficients B(J) and the
	! matrix of right-hand-side coefficients C(J,K).
 
		DO J=1,NLS    
  			B(J)=0.0                                                               
 			DO K=1,NLS
				C(J,K)=0.0
			END DO
		END DO

		DO I=1,NPTS
			CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
			DO J=1,NLS
				B(J)=B(J)+WT(I)*Y(I)*ZI(J)
				DO K=1,NLS
					C(J,K)=C(J,K)+WT(I)*ZI(J)*ZI(K)
				END DO
			END DO
		END DO

	! Invert the matrix of right-hand-side coefficients.

		CALL MATINV(C,NLS,DET)                       

	! Obtain the solution vector D and the vector of parameters A.

		DO J=1,NLS         
			D(J)=0.0 
		END DO             
    
			DO J=1,NLS         
      			DO K=1,NLS         
    				D(J)=D(J)+B(K)*C(J,K) 
				END DO
			END DO
      		DO J=1,NLS         
      			K=NSEARC+J            
    			A(K)=D(J) 
			END DO
 
 		IF (ISIG==1) THEN

		!	This section determines the SIGMAA and the R for the linear
		!	parameters.

			DO I=1,NLS
				K=I+NSEARC
				IF(C(I,I)<0.) C(I,I)=-C(I,I)
				SIGMAA(K)=DSQRT(C(I,I))
			END DO
		
			XNPTS=NPTS
			YMEAN=0.0
			WTSUM=0.0
		
			DO J=1,NLS
				XMEAN(J)=0.0
			END DO
		
			DO I=1,NPTS
				YMEAN=YMEAN+WT(I)*Y(I)
				WTSUM=WTSUM+WT(I)
				CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
				DO J=1,NLS
					XMEAN(J)=XMEAN(J)+WT(I)*ZI(J)
				END DO	
			END DO

			YMEAN=YMEAN/WTSUM
			
			DO J=1,NLS
				XMEAN(J)=XMEAN(J)/WTSUM
			END DO
		
			XN1=XNPTS-1.
			
			DO J=1,NLS
				K=J+NSEARC
				SJY=0.0
				SYY=0.0
				SJJ=0.0
				
				DO I=1,NPTS	
					CALL MODEL(X,I,A,ZI,YFI,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
					SJY=SJY+WT(I)*(Y(I)-YMEAN)*(ZI(J)-XMEAN(J))/(XN1*WTSUM)
					SYY=SYY+WT(I)*(Y(I)-YMEAN)*(Y(I)-YMEAN)/(XN1*WTSUM)
					SJJ=SJJ+WT(I)*(ZI(J)-XMEAN(J))*(ZI(J)-XMEAN(J))/(XN1*WTSUM)
				END DO

				IF (SYY<=0.0) THEN
					R(K)=0.0
				ELSE
					SY=DSQRT(SYY)
				
					IF (SJJ<=0.0) THEN
						R(K)=0.0
					ELSE
						SJ=DSQRT(SJJ)
						R(K)=SJY/(SY*SJ)
					END IF
				END IF
			END DO
		ELSE
		END IF
	ELSE
	END IF
	
END SUBROUTINE LS

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
  
SUBROUTINE MATINV (ARRAY,NORDER,DET)
	IMPlICIT NONE
    REAL(8):: ARRAY(25,25),IK(25),JK(25)
	REAL(8):: DET,AMAX,SAVE                 
	INTEGER:: K,NORDER,J,I,L

   	DET=1.                                               
	DO K=1,NORDER                                    

		! The largest element of the matix ARRAY(I,J) is found.
		AMAX=0.                                    
			
	 21 DO I=K,NORDER                           
    		DO J=K,NORDER                           
	    		IF ((DABS(AMAX) - DABS(ARRAY(I,J)))<=0) THEN   
	    			AMAX = ARRAY(I,J)                          
					IK(K) = I                                  
					JK(K) = J                                  
				ELSE
				END IF
			END DO                                   
		END DO
		! Rows and columns are interchanged to put AMAX in ARRAY(K,K).

		IF (AMAX==0) THEN        
			DET=0.                    
   			GO TO 140 
		END IF
			
		I=IK(K)                   
      		
		IF ((I-K)<0) THEN
			GOTO 21
		ELSE IF ((I-K)>0) THEN
			DO J=1,NORDER          
   				SAVE=ARRAY(K,J)           
   				ARRAY(K,J) = ARRAY(I,J)   
		    	ARRAY(I,J) = -SAVE
			END DO
		END IF           
			   
		J=JK(K)                   
   		IF ((J-K)<0) THEN
			GOTO 21
		ELSE IF ((J-K)>0) THEN         
			DO I=1,NORDER          
   				SAVE = ARRAY(I,K)         
   				ARRAY(I,K) = ARRAY(I,J)   
		    	ARRAY(I,J) = -SAVE
			END DO        
		END IF
			
	! Elements of the inverse matrix are accumulated.

		DO I=1,NORDER            
   			IF ((I-K)/=0) 	ARRAY(I,K) = -ARRAY(I,K) / AMAX               
		END DO
			
		DO I=1,NORDER                              
   			DO J=1,NORDER                              
   				IF ((I-K)/=0) THEN                               
					IF ((J-K)/=0) ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)   
				END IF
			END DO
		END DO	
					   
		DO J=1,NORDER                              
   			IF ((J-K)/=0) ARRAY(K,J) = ARRAY(K,J) / AMAX                
		END DO                                    
      	
		ARRAY(K,K) = 1. / AMAX                        
		DET = DET * AMAX
	END DO
                        
! The ordering of the matrix is restored.                                

	DO L=1,NORDER                            
		K=NORDER-L+1                                 
      	J=IK(K)                                      
      	IF ((J-K)>0) THEN                         
		 	DO I=1,NORDER                            
		  		SAVE=ARRAY(I,K)                              
		 		ARRAY(I,K) = -ARRAY(I,J)                     
		 		ARRAY(I,J) = SAVE   
			END DO
		END IF                         
		
		I=JK(K)                                      
	    IF ((I-K)>0) THEN                         
		 	DO J=1,NORDER                            
	      		SAVE=ARRAY(K,J)                              
	      		ARRAY(K,J) = -ARRAY(I,J)                     
		 		ARRAY(I,J) = SAVE    
			END DO                        
		END IF
	END DO

140 CONTINUE
END SUBROUTINE MATINV

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
                                          
SUBROUTINE FCHISQ(NPTS,NFREE,Y,WEIGHT,YFIT,CHISQR)
	IMPLICIT NONE
    REAL(8):: Y(17000),YFIT(17000),WEIGHT(17000)
	REAL(8):: CHISQR,FREE,CHISQ
    INTEGER:: I,NPTS,NFREE

	CHISQ = 0.                            
    
	DO I=1,NPTS                        
		CHISQ=CHISQ+WEIGHT(I)*(Y(I)-YFIT(I))**2  
    END DO
	
	FREE=NFREE
    CHISQR=CHISQ/FREE                     

END SUBROUTINE FCHISQ                                  

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------

SUBROUTINE FDERIV(NDERIV,NSEARC,X,I,A,DELTAA,DERIV,A1,A2,DEL,NBG,NOPEAK,NOPT,NTYPE,LOPT)
	IMPLICIT NONE
    REAL(8):: X(20,17000),A(25),DELTAA(25),DERIV(25),ZI(25)
    REAL(8):: AJ,DELTA,YF1,YF2,A1,A2,DEL
	INTEGER:: NDERIV,J,NSEARC,I,NBG,NOPEAK,NOPT,NTYPE
	CHARACTER(1),INTENT(IN)::LOPT

	IF(NDERIV==1) THEN
    	DO J=1,NSEARC
      		AJ=A(J) 
      		DELTA=DELTAA(J)
      		A(J)=AJ+DELTA
			CALL MODEL(X,I,A,ZI,YF1,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)
       		A(J)=AJ-DELTA
			CALL MODEL(X,I,A,ZI,YF2,NBG,NOPEAK,NOPT,NTYPE,A1,A2,DEL,LOPT)                                               
			DERIV(J)=(YF1-YF2)/(2.*DELTA)
    		A(J) = AJ
		END DO
	ELSE IF (NDERIV==2) THEN
		DERIV(1)=-DEXP(-0.481*A(1)*X(1,I))*X(1,I)*0.481		!Manually CHANGE to provide analytical derivatives
		DERIV(2)=-DEXP(-6.447*A(2)*X(1,I))*X(1,I)*6.447		!Manually CHANGE to provide analytical derivatives
 	ELSE
		write(*,'(A)') 'NDERIV must equal 1 or 2'
	END IF

	! The present analytical derivatives here are for exponential terms.

END SUBROUTINE FDERIV





