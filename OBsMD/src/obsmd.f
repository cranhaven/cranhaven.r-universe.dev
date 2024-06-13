      	    SUBROUTINE OBm(X,Y,N,COLS,abeta,bbeta,BLKS,MXFAC,MXINT,NTOP,
     *omdcnt,optop,onftop,ojtop,oprob,osigtop,ind)
      
      INTEGER N,COLS,BLKS,MXFAC,MXINT,NTOP,ind,contam,ef,t0,ti,
     *abeta,bbeta
      DOUBLE PRECISION Y(N),X(N,(COLS+BLKS))

      INTEGER omdcnt,onftop(ntop),ojtop(ntop,mxfac)
      DOUBLE PRECISION odel,optop(ntop),oprob(cols+1),osigtop(ntop)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	Modification to the original program MBCQPI5 in order to implement 
C	Objective Bayesian approach to Follow-up experiment
C	
C	ind     Indicator variable, 1 subroutine exit properly
C	        otherwise it has the format label number.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C     PROGRAM MBCQPI5
C     **********************************************************
C     MBCQPI
C     WRITTEN BY R DANIEL MEYER, THE LUBRIZOL CORPORATION,
C     29400 LAKELAND BLVD, WICKLIFFE, OHIO 44092
C      JUNE 1996
C      ********************************************************
C       WHILE MUCH TIME AND EFFORT HAS BEEN PUT INTO THE PRODUCTION
C       OF RELIABLE SOURCE CODE, IT IS IMPOSSIBLE TO GUARANTEE THAT
C       THE CODE IS ERROR FREE.  COMMENTS ON ERRORS, IRRITATIONS,
C       SUGGESTIONS FOR IMPROVEMENT ARE WELCOMED.
C       COPYRIGHT R DANIEL MEYER 1992
C       **********************************************************
C       THE FOLLOWING PARAMETER STATEMENT SHOULD BE EDITED
C       IF YOU WISH TO INCREASE THE SIZE OF PROBLEM THAT CAN
C       BE TACKLED, CHANGE INPUT/OUTPUT UNIT NUMBERS, OR
C       CHANGE THE NUMBER OF LINES ON AN OUTPUT PAGE.
C       THE PARAMETERS HAVE THE FOLLOWING DEFINITIONS:
C
C        LS      NUMBER OF LINES ON AN OUTPUT PAGE
C        MAXN    MAXIMUM NUMBER OF OBSERVATIONS
C        MAXCOL  MAXIMUM NUMBER OF EXPERIMENTAL FACTORS
C        MAXGAM  MAXIMUM NUMBER OF GAMMA VALUES ALLOWED
C        MAXNMD  MAXIMUM NUMBER OF INDIVIDUAL MODELS TO ID
C        MXMXIN  MAXIMUM ORDER INTERACTION
C        BIG     A LARGE WEIGHT;  WHEN AN INDIVIDUAL MODEL HAS WEIGHT
C                GREATER THAN BIG, ALL PROBABILITIES ARE SCALED BACK
C                TO AVOID OVERFLOW
C        MAXTERM MAXIMUM ORDER OF MODEL MATRIX
C        MAXDIM  MAXIMUM NUMBER OF DISTINCT PROBABILITIES TO COMPUTE
C
      
      	    PARAMETER (LS=66,
     *           MAXN=100,
     *           MAXCOL=25,
     *           MAXGAM=20,
     *           MAXNMD=300,             
     *           MXMXIN=3,
     *           BIG=1.0E10,
     *           MAXTERM=1+MAXCOL+MAXCOL*(MAXCOL-1)/2,
     *           MAXDIM=MAXGAM*MAXCOL)
      	
C        DESCRIPTION OF VARIABLES
C        ------------------------
C
C        INUNIT  INPUT UNIT NUMBER
C        OUNIT   OUTPUT UNIT NUMBER
C        N       NUMBER OF OBSERVATIONS
C        COLS    NUMBER OF FACTORS
C        BLKS    NUMBER OF BLOCK VARIABLES
C        MXFAC   MAX NUMBER OF FACTORS CONSIDERED IN ANY MODEL
C        MXINT   MAX ORDER INTERACTION CONSIDERED
C        X       MATRIX OF FACTORS (N BY COLS)
C        Y       VECTOR OF OBSERVATIONS
C        PROB    MATRIX OF MARGINAL POSTERIOR PROBABILITIES
C        ST      '*'  (USED FOR FORMATTING OUTPUT)
C        BL      ' '  (USED FOR FORMATTING OUTPUT)
C        MEAN    MEAN OF Y
C        S       RESIDUAL SUM OF SQUARES FOR NULL MODEL
C        ROOTN   SQRT(N)
C        NFAC    DO LOOP VARIABLE, 1 TO MXFAC
C        ALL     LOGICAL VARIABLE
C                  .TRUE. = ALL MODELS OF SIZE NFAC HAVE BEEN TRIED
C                 .FALSE. = NOT .TRUE.
C        JFAC    INTEGER VECTOR ID'ING FACTORS IN CURRENT MODEL
C        MULT    INTEGER VECTOR ID'ING FACTORS IN CURRENT INTERACTION
C        NTERM   NUMBER OF TERMS IN CURRENT MODEL
C        A       X-MATRIX AUGMENTED WITH INTERACTION COLUMNS
C        PART    LOGICAL VARIABLE
C                  .TRUE. = ALL INTERACTION COLUMNS HAVE BEEN ADDED
C                  .FALSE. = NOT .TRUE.
C        B       A'Y;  ALSO SOLUTION TO AA*X=A'Y
C        OCOUNT  COUNT OF OUTPUT LINES (USED FOR FORMATTING)
C        RES     RESIDUALS FROM CURRENT MODEL
C        SR2     SUM OF SQUARED RESIDUALS FROM CURRENT MODEL
C        CC      CARRIAGE CONTROL CHARACTER
C        Z       WORK SPACE VECTOR FOR DPOCO SUBROUTINE
C        INFO    ERROR INDICATOR FROM SUBROUTINE DPOCO
C        NTOP    USER-SPECIFIED NUMBER OF INDIVIDUAL MODELS TO ID
C        PTOP    VECTOR OF PROBABILITIES OF TOP NTOP MODELS
C        JTOP    MATRIX OF FACTOR NUMBERS OF TOP NTOP MODELS
C        NLOW    INDEX OF LOWEST PROB MODEL IN TOP NTOP MODELS
C        NFTOP   NUMBER OF FACTORS IN EACH OF NTOP MODELS
C        PINDEX  INTEGER VECTOR OF SORTED INDEXES OF PTOP
C        MDCNT   TOTAL NUMBER OF MODELS EVALUATED
C
      LOGICAL PART,ALL

      DOUBLE PRECISION S,ZZ,
     *PI0, PMI0(MAXNMD),SUM,
     *MEAN,ROOTN,MXNORM,SUMNORM,DEL,COND,WCRIT,PSCAL,
     *SR2,SIGMA(MAXNMD),
     *RES(MAXN),par1f,par2f,par3f,par4f,
     *A(MAXN,MAXTERM),AA(MAXTERM,MAXTERM),ATEM(MAXTERM,MAXTERM),
     *B(MAXTERM),PROB(MAXCOL),
     *Z(MAXTERM),PTOP(MAXNMD),SIGTOP(MAXNMD),PROB0,
     *BFI0(MAXNMD),QI0,P1,P2,P3,RE,IM,IPER,det(2),
     *coef1, coef2, coef3, coef4, coef5, coef6, coef7

      INTEGER NTERM,NN,NFAC,M,I,J,K,II,BAR,OLOOP,
     *par1,par2,par3,par4,    
     *OCOUNT,ISTART,INUNIT,OUNIT,NLOW,INFO,
     *MULT(MAXCOL),JFAC(MAXCOL),JTOP(MAXNMD,MAXCOL),NFTOP(MAXNMD),
     *PINDEX(MAXNMD),MDCNT

      CHARACTER(1) CC,ST,BL


      	
      INUNIT=5
      OUNIT=1 

      OCOUNT=3
      IF ((N .LT. 1) .OR. (N .GT. MAXN)) THEN
      	    ind = 1501
        GO TO 700
      ENDIF

      IF ((COLS .LT. 1) .OR. (COLS .GT. MAXCOL)) THEN
      	    ind = 1502
        GO TO 700
      ENDIF

      IF ((BLKS .LT. 0) .OR. (BLKS .GT. MAXCOL)) THEN
      	    ind = 1502
        GO TO 700
      ENDIF

      IF ((MXFAC .LT. 1) .OR. (MXFAC .GT. COLS)) THEN
      	    ind = 1503
        GO TO 700
      ENDIF

      IF ((MXINT .LT. 1) .OR. (MXINT .GT. MXMXIN)) THEN
      	    ind = 1504
        GO TO 700
      ENDIF

      IF ((MXINT .EQ. 3) .AND.
     & ((MXFAC*(MXFAC-1)*(MXFAC-2)/6) .GT. MAXTERM)) THEN
      	    ind = 1505
        GO TO 700
      ENDIF


       IF ((NTOP .LT. 0) .OR. (NTOP .GT. MAXNMD)) THEN
         NTOP=MAXNMD
       ENDIF

      	
      do 51 i = 1, MAXCOL
        PROB(i) = 0.0D0
51    continue
      	
      do 52 i = 1,MAXNMD
        PTOP(i) = -1.0D0
        SIGMA(i)=0.0D0
52    continue
      	

      ST='*'
      BL=' '
      PSCAL=1.0
      MEAN=0.0
      S=0.0
      MDCNT=0
      CONTAM=0
      t0=1+blks
      ef=0
                              
      DO 15 I=1,NTOP
        PINDEX(I)=I
15      CONTINUE
      	    DO 100 M=1,N
        MEAN=MEAN+Y(M)                                                 
100   continue    
      MEAN=MEAN/FLOAT(N)
      DO 110 M=1,N                                                      
        S=S+(Y(M)-MEAN)**2                                               
110   continue
      DO 420 NFAC=0,MXFAC
         ALL= .FALSE.                                                   
                                                                       
      CALL INITIA(JFAC,NFAC,MAXCOL)                                  

 200  IF (.NOT. ALL) THEN

C     AUGMENT WITH INTERACTION COLUMNS                                  
           		                                                                  
      MDCNT=MDCNT+1
      DO 210 I=1,N                                                      
       A(I,1)=1.0                                                            
      DO 205 J=1,BLKS                                                   
        A(I,J+1)=X(I,J)
205   continue
      DO 210 J=1,NFAC
        A(I,BLKS+J+1)=X(I,BLKS+JFAC(J))
210   continue
      NTERM=NFAC+1+BLKS                                                 

      DO 250 M=2,MIN(MXINT,NFAC)
         CALL INITIA(MULT,M,MAXCOL)                                     
         PART=.FALSE.                                                   
 220     IF (.NOT. PART) THEN                                           
           NTERM=NTERM+1                                                
           DO 230 I=1,N                                                 
              A(I,NTERM)=A(I,MULT(1)+1+BLKS)*A(I,MULT(2)+1+BLKS)
 230       continue
      		  DO 240 II=3,M
             DO 240 I=1,N
            A(I,NTERM)=A(I,NTERM)*A(I,MULT(II)+1+BLKS)
 240       continue
      		  CALL INCREM(MULT,PART,M,NFAC,MAXCOL)
           GO TO 220
      ENDIF                	   	                                                            
 250  CONTINUE

C      FORM X-PRIME-X MATRIX

      	
      NN=MIN(NTERM,N)
      	    IF (NN.LT.N) then
 255  DO 270 I=1,NN                                                  
      DO 270 J=I,NN                                                 
      		        AA(I,J)=0.0
        DO 260 M=1,N                                                    
         AA(I,J)=AA(I,J)+A(M,I)*A(M,J)
 260    continue  
        ATEM(I,J)=AA(I,J)
        ATEM(J,I)=AA(I,J)
        AA(J,I)=AA(I,J)
270   continue

      DO 280 I=1,NN
      DO 280 J=1,NN
          AA(I,J)=ATEM(I,J)
280   continue 
      DO 320 I=1,NN
        B(I)=0.0                                                        
        DO 320 M=1,N                                                    
          B(I)=B(I)+A(M,I)*Y(M) 
320   	continue
                                         
      CALL DPOCO(AA,MAXTERM,NN,COND,Z,INFO)

      	    IF (info.ne.0) THEN                                    
      	    IF (ef.eq.1) then  
      			GO TO 400
      	    else
      			NN=NN-1
      			go to 255
      	    ENDIF 
      	    ENDIF
      	
      DO 271 I=1,NN
      	    DO 271 J=1,NN
      	    ATEM(I,J)=AA(I,J)                       
271   continue

      	    CALL DPODI(ATEM, MAXTERM,NN,DET,10)
      	    IF (DET(2).LT. -3) THEN                                    
      	    IF (ef.eq.1) then  
      			GO TO 400
      	    else
      			 NN=NN-1
      			go to 255
      	    ENDIF                                                   
      	    ENDIF
      					
      CALL DPOSL(AA,MAXTERM,NN,B)
      		

      	    SR=0.0                                                            
      SR2=0.0
      DO 340 M=1,N                                                      
        RES(M)=Y(M)
       DO 330 I=1,NN                                                 
        RES(M)=RES(M)-A(M,I)*B(I)
 330   continue
         SR2=SR2+RES(M)**2
 340   continue
      	    If (mdcnt.eq.1) then 
      	    s=SR2
      	    endif 
      	    QI0=SR2/S
       ti=nn-t0
          SIGMA(MDCNT)=SR2/(N-NN)                                    
      		  ZZ=(1-QI0**(-1))*(ti+t0)/(N+1)
      	    P1=(ti+1)/2.
      	    P2=(N-t0)/2.
      	    P3=(ti+3)/2.
      		  CALL HYP(ZZ,P1,P2,P3,RE,IM)
      		  IF (IM.EQ.0) THEN
      			 IPER=RE
       ENDIF

      coef1=(N+1.0D0)                                                    
      coef5=(ti+t0)
      coef2=(-(ti)/2.0D0)
      coef3=(-(N-t0)/2.0D0)
      coef4=(ti+1.0D0)
      coef6=(coef1**(coef2))
      coef7=((coef5)**(-coef2))
      	    BFI0(MDCNT)=coef6*coef7*(QI0**(coef3))
     */coef4*IPER
      	    ELSE
      		  do 323 i=1,nterm
      	b(i)=0.0
323   	    continue 
      		  BFI0(MDCNT)=0.0D0
      		  CONTAM=CONTAM+1
      	    ENDIF
      		
       par1=abeta+nfac-1
      	     par2=bbeta+cols-nfac-1
      	     par3=abeta-1
      	     par4=bbeta+cols-1
      
      	    CALL Fact(par1,par1f)
      	    CALL Fact(par2,par2f)
      	    CALL Fact(par3,par3f)
      	    CALL Fact(par4,par4f)
      	    PI0=(par1f*par2f)/(par3f*par4f)

      	    PMI0(MDCNT)=PI0*BFI0(MDCNT)
            WCRIT=PSCAL*BIG

      	  	  CALL IDLOW(PTOP,MAXNMD,NTOP,NLOW,WCRIT)
      		  		
      			IF (PMI0(MDCNT).GT.WCRIT) then
      				PTOP(NLOW)=PMI0(MDCNT)
      				SIGTOP(NLOW)=SIGMA(MDCNT)
      				NFTOP(NLOW)=NFAC
      				DO 355 I=1,NFAC
      					JTOP(NLOW,I)=JFAC(I)
 355              continue
      			ENDIF

      	      DO 360 I=1,NFAC
               PROB(JFAC(I))=PROB(JFAC(I))+PMI0(MDCNT)
 360        continue   
      			IF(NFAC.EQ.0) then
      				PROB0=PMI0(1)
      			endif 
        
 400  	CALL INCREM(JFAC,ALL,NFAC,COLS,MAXCOL)
         GO TO 200
      ENDIF
 420  CONTINUE
      	
      	    SUM=0.0D0
      	    DO 444 i=2,MDCNT
      	    SUM=SUM+PMI0(i)
 444  continue 
      	
      	    DO 446 I=1,MDCNT 
      	    PMI0(I)=PMI0(I)/(1.0D0+SUM) 
 446  	continue 

      OCOUNT=OCOUNT+1
      OCOUNT=MOD(OCOUNT,LS)
      IF (COLS .LE. 15) CALL OSPACE(LS,OCOUNT,N+3,CC)
      IF (COLS .GT. 15) CALL OSPACE(LS,OCOUNT,2*N+3,CC)
      CALL OSPACE(LS,OCOUNT,N+3,CC)
      CALL OSPACE(LS,OCOUNT,6,CC)
         DO 545 I=1,NTOP
          PTOP(I)=PTOP(I)/(1.D0+SUM)
 545     continue 
          CALL SSORT(PTOP,PINDEX,NTOP,-2)
        IF (NTOP .GT. MDCNT) NTOP=MDCNT
        CALL OSPACE(LS,OCOUNT,NTOP+4,CC)
      CALL OSPACE(LS,OCOUNT,COLS+5,CC)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Whatever is printed is an output subroutine parameter
      omdcnt = MDCNT
      do 450 i = 1,NTOP
        optop(i) = PTOP(i)
        osigtop(i) = SIGTOP(PINDEX(i))
       onftop(i) = NFTOP(PINDEX(i))
        do 451 j= 1,MXFAC
           ojtop(i,j)=0
451     continue 
        do 452 j= 1,NFTOP(PINDEX(i))
            ojtop(i,j) = JTOP(PINDEX(I),J)
452     continue 
450   continue
      		  oprob(1)=prob0/(1.D0+sum)
      do 459 i = 1, cols
        oprob(i+1) = prob(i)/(1.D0+sum)
459   continue 


      ind = 1

 700  continue

      

C	    CLOSE(OUNIT)

C 1000 FORMAT(' MBCQPI5: BAYESIAN ANALYSIS OF CONFOUNDED DATA',/,
C     &1X,'WRITTEN BY R. DANIEL MEYER, THE LUBRIZOL CORPORATION',/
C     &1X,'ALL RIGHTS RESERVED;    JUNE 1996')
C 1001 FORMAT(A1,'X-MATRIX',/,' --------')
C 1002 FORMAT(' ',I3,15(1X,F7.3))
C 1003 FORMAT(A1,'Y-VECTOR',/,' --------')
C 1004 FORMAT(' ',I3,1X,F10.4)
C 1005 FORMAT(1X,'WEIGHT= ',E16.6E2,' FACTORS: ',30(1X,I2))
C 1100 FORMAT(A1,'NO. OF',3X,'NO. OF',2X,'NO. OF',12X,'MAX ORDER',5X,
C     &'MAX NO. OF',5X,'TOTAL NO. OF',/,' RUNS',5X,'FACTORS',2X,'BLOCKS',
C     &4X,'PI',4X,'INTERACTION',3X,'ACTIVE FACTORS',5X,'MODELS')
C 1101 FORMAT(' ',100('-'))
C 1102 FORMAT(A1,5X,'GAMMA',12X,'PGAM')
C 1103 FORMAT(1X,F10.3,E16.6E2,2X,'+',20(A1),'+')
C 1104 FORMAT(1X,I3,I10,I8,F10.3,I9,I15,I18)
C 1105 FORMAT(A1,' BEST ',I3,' MODELS',//,1X,'PROBABILITY   SIGMA-SQ ',
C     &' NO OF FACTORS   FACTORS')
C 1106 FORMAT(1X,F10.6,F12.4,I6,10X,30(1X,I2))
C 1107 FORMAT(A1,5X,'POSTERIOR PROBABILITIES',
C     &/,' FACTOR',5X,'POST. PROB.')
C 1108 FORMAT(' NONE',F14.3)
C 1109 FORMAT(' ',I4,F14.3,2X,'+',20(A1),'+')
C 1110 FORMAT(A1,11X,'POSTERIOR PROBABILITIES FOR EACH GAMMA VALUE',/)
C 1111 FORMAT(1X,'FACTOR',12(F10.2))
C 1112 FORMAT(' NONE',2X,12(F10.3))
C 1113 FORMAT(' ',I4,2X,12(F10.3))
C 1114 FORMAT(' NONE',F14.3,2X,'+',20(A1),'+')
C 1115 FORMAT(' ',3X,15(1X,F7.3))
C 1116 FORMAT(A1,'POSTERIOR PROBABILITIES WEIGHT-AVERAGED OVER GAMMA',
C     &/,' FACTOR',5X,'POST. PROB.')
C 1117 FORMAT(/,1X,'GAMMA= ',F8.3,'   GAMMA2= ',F8.3,'  NORM= ',E16.6E2)
C 1118 FORMAT(/,1X,'GAMMA= ',F8.3,' TO ',F8.3,' BY ',F6.4,' INCREMENTS')
C 1500 FORMAT(' ***** ERROR *****')
C 1501 FORMAT(1X,'N=',I8,' OUT OF RANGE',/,
C     &' N MUST BE BETWEEN 1 AND ',I4,/)
C 1502 FORMAT(1X,'NO. OF FACTORS = ',I8,' OUT OF RANGE',/,
C     &' MUST BE BETWEEN 1 AND ',I4,/)
C 1503 FORMAT(1X,'MAX. NO. OF FACTORS = ',I8,' OUT OF RANGE',/,
C     &' MUST BE BETWEEN 1 AND TOTAL NO. OF FACTORS= ',I4,/)
C 1504 FORMAT(1X,'MAX. ORDER INTERACTION = ',I8,' OUT OF RANGE',/,
C     &' MUST BE BETWEEN 1 AND ',I4,/)
C 1505 FORMAT(1X,'MAX. ORDER INTERACTION = ',I8,' RESULTS IN TOO',/,
C     &' MANY COLUMNS FOR NO. OF FACTORS = ',I4,/)
C 1506 FORMAT(1X,'PI = ',F8.4,' OUT OF RANGE',/,
C     &' MUST BE BETWEEN 0 AND 1',/)
C 1507 FORMAT(1X,'GAMMA INDICATOR = ',I6,' OUT OF RANGE',/,
C     &' MUST BE EITHER 0 OR 1',/)
C 1508 FORMAT(1X,'GAMMA  = ',I6,' OUT OF RANGE',/,
C     &' MUST BE POSITIVE',/)
C 1509 FORMAT(1X,'NO. OF GAMMAS FOR SEARCH  = ',I6,' OUT OF RANGE',/,
C     &' MUST BE BETWEEN 2 AND ',I5,/)
C 1510 FORMAT(1X,'GAMMA(FIRST) = ',F12.4,' GAMMA(LAST)= ',F12.4,/,
C     &' GAMMA(FIRST) MUST BE LESS THAN GAMMA(LAST)',/)
C 1511 FORMAT(' **** WARNING: SINGULAR MATRIX ENCOUNTERED ****')
C 1512 FORMAT(' **** WARNING: MAX NUMBER OF FACTORS TOO LARGE ****',
C     &/,' CORRECTIVE ACTION: VALUE REDUCED FROM',I3,' TO',I3,/)
C 1513 FORMAT(' **** WARNING: NUMBER OF INDIVIDUAL MODELS TOO BIG ****',
C     &/,' CORRECTIVE ACTION: VALUE REDUCED FROM',I4,' TO',I4,/)
C 1305 FORMAT(1X,'DET:',E16.6E3,' SR:',E16.6E3,' S:',E16.6E3,
C     &' EXPON:',E16.6E3)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      	    SUBROUTINE FACT(INPUT,OUTPUT)
      	    INTEGER input,i
      	    DOUBLE PRECISION output
      	    output=1
      	    do  i=2,input
      	    output=output*i
      	    end do 
      	    return
      	    end


      SUBROUTINE IDLOW(PTOP,MAXNMD,NTOP,K,P)
      DOUBLE PRECISION PTOP(MAXNMD),P
      INTEGER NTOP,I,K
      DO 10 I=1,NTOP
        IF (PTOP(NTOP-I+1) .LT. P) THEN
          K=NTOP-I+1
          P=PTOP(NTOP-I+1)
        ENDIF
 10   CONTINUE
      RETURN
      END

      SUBROUTINE OSPACE(LS,OCOUNT,K,CC)
      INTEGER OCOUNT,K
      CHARACTER(1) CC
      IF (((LS-OCOUNT) .LT. K) .AND. (K .LT. LS)) THEN
        OCOUNT=K
        CC='1'
      ELSE
        OCOUNT=OCOUNT+K+1
        CC='0'
      ENDIF                                                             
      RETURN                                                            
      END                                                               

                                                                       
      SUBROUTINE INCREM(J,ALL,R,N,MAXCOL)                               
                                                                       
C       INCREMENTS THE INTEGER VECTOR J TO THE NEXT VECTOR IN           
C       LEXICAL ORDER
                                                                       
      INTEGER M,L,R,N,J(MAXCOL)                                         
      LOGICAL OK,ALL                                                    
      L=R                                                               
      ALL=.FALSE.
      OK=.FALSE.                                                        
 50   IF ((.NOT. OK) .AND. (L .GT. 0)) THEN
         IF (J(L) .LT. N-R+L) THEN                                      
            J(L)=J(L)+1
            DO 101 M=L+1,R                                              
              J(M)=J(M-1)+1
 101        continue
            OK=.TRUE.                                                   
         ELSE                                                           
            L=L-1                                                       
         ENDIF                                                          
         GO TO 50                                                       
      ENDIF                                                             
      IF (L .LE. 0) ALL=.TRUE.                                          
      RETURN                                                            
      END                                                               
                                                                       
      SUBROUTINE INITIA(J,R,MAXCOL)                                     
                                                                       
C       INITIATES THE INTEGER VECTOR J TO THE FIRST VALUE               
C       IN LEXICAL ORDER (1,2,3,...,R)                                  
                                                                       
        INTEGER J(MAXCOL),R,I                                           
         DO 401 I=1,R                                                   
            J(I)=I                                                      
 401     continue   
         DO 402 I=R+1,MAXCOL                                            
         J(I)=0
 402     continue
      RETURN                                                            
      END
      	                                                           
                                                                  
      SUBROUTINE DPOCO(A,LDA,N,RCOND,Z,INFO)
      INTEGER LDA,N,INFO                                                
      DOUBLE PRECISION A(LDA,N),Z(N)                                    
      DOUBLE PRECISION RCOND                                            
C                                                                       
C     DPOCO FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE      
C     MATRIX AND ESTIMATES THE CONDITION OF THE MATRIX.                 
C
C     IF  RCOND  IS NOT NEEDED, DPOFA IS SLIGHTLY FASTER.               
C     TO SOLVE  A*X = B , FOLLOW DPOCO BY DPOSL.                        
C     TO COMPUTE  INVERSE(A)*C , FOLLOW DPOCO BY DPOSL.                 
C     TO COMPUTE  DETERMINANT(A) , FOLLOW DPOCO BY DPODI.               
C     TO COMPUTE  INVERSE(A) , FOLLOW DPOCO BY DPODI.                   
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE
C                DIAGONAL AND UPPER TRIANGLE ARE USED.                  
C
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R 
C                WHERE  TRANS(R)  IS THE TRANSPOSE.                     
C                THE STRICT LOWER TRIANGLE IS UNALTERED.                
C                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   
C                                                                       
C        RCOND   DOUBLE PRECISION                                       
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .        
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS       
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE             
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND . 
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION     
C                           1.0 + RCOND .EQ. 1.0                        
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING           
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF         
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.  IF INFO .NE. 0 , RCOND IS UNCHANGED.      
C                                                                       
C        Z       DOUBLE PRECISION(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.  
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT           
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .                    
C                IF  INFO .NE. 0 , Z  IS UNCHANGED.                     
C                                                                       
C        INFO    INTEGER                                                
C                = 0  FOR NORMAL RETURN.                                
C                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR
C                     OF ORDER  K  IS NOT POSITIVE DEFINITE.            
C                                                                       
C     LINPACK.  THIS VERSION DATED 08/14/78 .                           
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     LINPACK DPOFA                                                     
C     BLAS DAXPY,DDOT,DSCAL,DASUM                                       
C     FORTRAN DABS,DMAX1,DREAL,DSIGN
C                                                                       
C     INTERNAL VARIABLES                                                
C                                                                       
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM                           
      INTEGER I,J,JM1,K,KB,KP1                                          
                                                                       
                                                                       
C     FIND NORM OF A USING ONLY UPPER HALF                              
                                                                       
      DO 30 J = 1, N                                                    
         Z(J) = DASUM(J,A(1,J),1)                                       
         JM1 = J - 1                                                    
         IF (JM1 .LT. 1) GO TO 20                                       
         DO 10 I = 1, JM1                                               
            Z(I) = Z(I) + DABS(A(I,J))                                  
   10    CONTINUE                                                       
   20    CONTINUE                                                       
   30 CONTINUE                                                          
      ANORM = 0.0D0                                                     
      DO 40 J = 1, N                                                    
         ANORM = DMAX1(ANORM,Z(J))                                      
   40 CONTINUE                                                          

C     FACTOR                                                            
                                                                       	
      CALL DPOFA(A,LDA,N,INFO)                                          
      IF (INFO .NE. 0) GO TO 180
C                                                                       
C        RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C        ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  A*Y = E .      
C        THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL        
C        GROWTH IN THE ELEMENTS OF W  WHERE  TRANS(R)*W = E .           
C        THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.         
C                                                                       
C        SOLVE TRANS(R)*W = E                                           
C
         EK = 1.0D0                                                     
         DO 50 J = 1, N                                                 
            Z(J) = 0.0D0                                                
   50    CONTINUE                                                       
         DO 110 K = 1, N                                                
            IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))                   
            IF (DABS(EK-Z(K)) .LE. A(K,K)) GO TO 60                     
               S = A(K,K)/DABS(EK-Z(K))
               CALL DSCAL(N,S,Z,1)                                      
               EK = S*EK
   60       CONTINUE                                                    
            WK = EK - Z(K)                                              
            WKM = -EK - Z(K)
            S = DABS(WK)                                                
            SM = DABS(WKM)                                              
            WK = WK/A(K,K)                                              
            WKM = WKM/A(K,K)                                            
            KP1 = K + 1                                                 
            IF (KP1 .GT. N) GO TO 100                                   
               DO 70 J = KP1, N                                         
                  SM = SM + DABS(Z(J)+WKM*A(K,J))                       
                  Z(J) = Z(J) + WK*A(K,J)                               
                  S = S + DABS(Z(J))                                    
   70          CONTINUE                                                 
               IF (S .GE. SM) GO TO 90                                  
                  T = WKM - WK                                          
                  WK = WKM                                              
                  DO 80 J = KP1, N                                      
                     Z(J) = Z(J) + T*A(K,J)                             
   80             CONTINUE                                              
   90          CONTINUE                                                 
  100       CONTINUE                                                    
            Z(K) = WK
  110    CONTINUE                                                       
         S = 1.0D0/DASUM(N,Z,1)                                         
         CALL DSCAL(N,S,Z,1)                                            
                                                                       
C        SOLVE R*Y = W                                                  
                                                                       
         DO 130 KB = 1, N
            K = N + 1 - KB                                              
            IF (DABS(Z(K)) .LE. A(K,K)) GO TO 120                       
               S = A(K,K)/DABS(Z(K))                                    
               CALL DSCAL(N,S,Z,1)                                      
  120       CONTINUE                                                    
            Z(K) = Z(K)/A(K,K)                                          
            T = -Z(K)
            CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)                           
  130    CONTINUE                                                       
         S = 1.0D0/DASUM(N,Z,1)                                         
         CALL DSCAL(N,S,Z,1)                                            
                                                                       
         YNORM = 1.0D0
                                                                       
C        SOLVE TRANS(R)*V = Y                                           
                                                                       
         DO 150 K = 1, N
            Z(K) = Z(K) - DDOT(K-1,A(1,K),1,Z(1),1)                     
            IF (DABS(Z(K)) .LE. A(K,K)) GO TO 140
               S = A(K,K)/DABS(Z(K))                                    
               CALL DSCAL(N,S,Z,1)                                      
               YNORM = S*YNORM                                          
  140       CONTINUE                                                    
            Z(K) = Z(K)/A(K,K)                                          
  150    CONTINUE                                                       
         S = 1.0D0/DASUM(N,Z,1)                                         
         CALL DSCAL(N,S,Z,1)                                            
         YNORM = S*YNORM                                                
                                                                       
C        SOLVE R*Z = V                                                  
                                                                       
         DO 170 KB = 1, N                                               
            K = N + 1 - KB                                              
            IF (DABS(Z(K)) .LE. A(K,K)) GO TO 160                       
               S = A(K,K)/DABS(Z(K))                                    
               CALL DSCAL(N,S,Z,1)                                      
               YNORM = S*YNORM                                          
  160       CONTINUE                                                    
            Z(K) = Z(K)/A(K,K)
            T = -Z(K)                                                   
            CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)                           
  170    CONTINUE                                                       
         S = 1.0D0/DASUM(N,Z,1)                                         
         CALL DSCAL(N,S,Z,1)                                            
         YNORM = S*YNORM                                                

         IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM                      
         IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0                            
  180 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      
      	    SUBROUTINE DPODI(A,LDA,N,DET,JOB)                                 
      INTEGER LDA,N,JOB
      DOUBLE PRECISION A(LDA,N)                                         
      DOUBLE PRECISION DET(2)                                           
C                                                                       
C     DPODI COMPUTES THE DETERMINANT AND INVERSE OF A CERTAIN
C     DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE MATRIX (SEE BELOW)   
C     USING THE FACTORS COMPUTED BY DPOCO, DPOFA OR DQRDC.              
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)
C                THE OUTPUT  A  FROM DPOCO OR DPOFA
C                OR THE OUTPUT  X  FROM DQRDC.                          
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C        JOB     INTEGER                                                
C                = 11   BOTH DETERMINANT AND INVERSE.                   
C                = 01   INVERSE ONLY.                                   
C                = 10   DETERMINANT ONLY.                               
C                                                                       
C     ON RETURN                                                         
C                                                                       
C        A       IF DPOCO OR DPOFA WAS USED TO FACTOR  A  THEN          
C                DPODI PRODUCES THE UPPER HALF OF INVERSE(A) .          
C                IF DQRDC WAS USED TO DECOMPOSE  X  THEN                
C                DPODI PRODUCES THE UPPER HALF OF INVERSE(TRANS(X)*X)   
C                WHERE TRANS(X) IS THE TRANSPOSE.
C                ELEMENTS OF  A  BELOW THE DIAGONAL ARE UNCHANGED.      
C                IF THE UNITS DIGIT OF JOB IS ZERO,  A  IS UNCHANGED.   
C                                                                       
C        DET     DOUBLE PRECISION(2)                                    
C                DETERMINANT OF  A  OR OF  TRANS(X)*X  IF REQUESTED.    
C                OTHERWISE NOT REFERENCED.                              
C                DETERMINANT = DET(1) * 10.0**DET(2)                    
C                WITH  1.0 .LE. DET(1) .LT. 10.0                        
C                OR  DET(1) .EQ. 0.0 .
C                                                                       
C     ERROR CONDITION                                                   
C                                                                       
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS     
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.           
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY      
C        AND IF DPOCO OR DPOFA HAS SET INFO .EQ. 0 .
C                                                                       
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DSCAL                                                  
C     FORTRAN MOD                                                       
C                                                                       
C     INTERNAL VARIABLES
C                                                                       
      DOUBLE PRECISION T                                                
      DOUBLE PRECISION S                                                
      INTEGER I,J,JM1,K,KP1                                             
                                                                       
C     COMPUTE DETERMINANT                                               
                                                                       
      IF (JOB/10 .EQ. 0) GO TO 70                                       
         DET(1) = 1.0D0                                                 
         DET(2) = 0.0D0                                                 
         S = 10.0D0                                                     
         DO 50 I = 1, N                                                 
            DET(1) = A(I,I)**2*DET(1)                                   
C        ...EXIT                                                        
            IF (DET(1) .EQ. 0.0D0) GO TO 60                             
   10       IF (DET(1) .GE. 1.0D0) GO TO 20                             
               DET(1) = S*DET(1)                                        
               DET(2) = DET(2) - 1.0D0                                  
            GO TO 10                                                    
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40                                 
               DET(1) = DET(1)/S                                        
               DET(2) = DET(2) + 1.0D0                                  
            GO TO 30                                                    
   40       CONTINUE                                                    
   50    CONTINUE                                                       
   60    CONTINUE                                                       
   70 CONTINUE                                                          
                                                                       
C     COMPUTE INVERSE(R)
                                                                       
      IF (MOD(JOB,10) .EQ. 0) GO TO 140                                 
         DO 100 K = 1, N                                                
            A(K,K) = 1.0D0/A(K,K)                                       
            T = -A(K,K)                                                 
            CALL DSCAL(K-1,T,A(1,K),1)                                  
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90                                    
            DO 80 J = KP1, N                                            
               T = A(K,J)                                               
               A(K,J) = 0.0D0                                           
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)                        
   80       CONTINUE                                                    
   90       CONTINUE                                                    
  100    CONTINUE                                                       

C        FORM  INVERSE(R) * TRANS(INVERSE(R))
                                                                       
         DO 130 J = 1, N                                                
            JM1 = J - 1                                                 
            IF (JM1 .LT. 1) GO TO 120                                   
            DO 110 K = 1, JM1                                           
               T = A(K,J)                                               
               CALL DAXPY(K,T,A(1,J),1,A(1,K),1)                        
  110       CONTINUE                                                    
  120       CONTINUE                                                    
            T = A(J,J)                                                  
            CALL DSCAL(J,T,A(1,J),1)                                    
  130    CONTINUE                                                       
  140 CONTINUE                                                          
      RETURN                                                            
      END                                                               


      SUBROUTINE DPOSL(A,LDA,N,B)                                       
      INTEGER LDA,N                                                     
      DOUBLE PRECISION A(LDA,N),B(N)                                    
C
C     DPOSL SOLVES THE DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE     
C     SYSTEM A * X = B                                                  
C     USING THE FACTORS COMPUTED BY DPOCO OR DPOFA.                     
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE OUTPUT FROM DPOCO OR DPOFA.                        
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C        B       DOUBLE PRECISION(N)
C                THE RIGHT HAND SIDE VECTOR.                            
C
C     ON RETURN                                                         
C                                                                       
C        B       THE SOLUTION VECTOR  X .                               
C                                                                       
C     ERROR CONDITION                                                   
C                                                                       
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS     
C        A ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES
C        SINGULARITY BUT IT IS USUALLY CAUSED BY IMPROPER SUBROUTINE    
C        ARGUMENTS.  IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED
C        CORRECTLY AND  INFO .EQ. 0 .                                   
C                                                                       
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX                 
C     WITH  P  COLUMNS                                                  
C           CALL DPOCO(A,LDA,N,RCOND,Z,INFO)                            
C           IF (RCOND IS TOO SMALL .OR. INFO .NE. 0) GO TO ...          
C           DO 10 J = 1, P                                              
C              CALL DPOSL(A,LDA,N,C(1,J))                               
C        10 CONTINUE                                                    
C                                                                       
C     LINPACK.  THIS VERSION DATED 08/14/78 .                           
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
C                                                                       
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DAXPY,DDOT                                                   
C                                                                       
C     INTERNAL VARIABLES
C                                                                       
      DOUBLE PRECISION DDOT,T                                           
      INTEGER K,KB                                                      
                                                                       
C     SOLVE TRANS(R)*Y = B                                              
                                                                       
      DO 10 K = 1, N                                                    
         T = DDOT(K-1,A(1,K),1,B(1),1)                                  
         B(K) = (B(K) - T)/A(K,K)                                       
   10 CONTINUE                                                          
                                                                       
C     SOLVE R*X = Y
                                                                       
      DO 20 KB = 1, N                                                   
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)                                             
         T = -B(K)                                                      
         CALL DAXPY(K-1,T,A(1,K),1,B(1),1)                              
   20 CONTINUE
      RETURN                                                            
      END                                                               
                                                                       
      SUBROUTINE DPOFA(A,LDA,N,INFO)                                    
      INTEGER LDA,N,INFO                                                
      DOUBLE PRECISION A(LDA,N)                                         
C
C     DPOFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE      
C     MATRIX.                                                           
C
C     DPOFA IS USUALLY CALLED BY DPOCO, BUT IT CAN BE CALLED            
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          
C     (TIME FOR DPOCO) = (1 + 18/N)*(TIME FOR DPOFA) .                  
C                                                                       
C     ON ENTRY                                                          
C                                                                       
C        A       DOUBLE PRECISION(LDA, N)                               
C                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE         
C                DIAGONAL AND UPPER TRIANGLE ARE USED.                  
C                                                                       
C        LDA     INTEGER                                                
C                THE LEADING DIMENSION OF THE ARRAY  A .                
C                                                                       
C        N       INTEGER                                                
C                THE ORDER OF THE MATRIX  A .                           
C                                                                       
C     ON RETURN
C                                                                       
C        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R 
C                WHERE  TRANS(R)  IS THE TRANSPOSE.                     
C                THE STRICT LOWER TRIANGLE IS UNALTERED.                
C                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   
C                                                                       
C        INFO    INTEGER                                                
C                = 0  FOR NORMAL RETURN.                                
C                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR    
C                     OF ORDER  K  IS NOT POSITIVE DEFINITE.            
C                                                                       
C     LINPACK.  THIS VERSION DATED 08/14/78 .                           
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS                                         
C                                                                       
C     BLAS DDOT                                                         
C     FORTRAN DSQRT                                                     
C                                                                       
C     INTERNAL VARIABLES
C                                                                       
      DOUBLE PRECISION DDOT,T                                           
      DOUBLE PRECISION S                                                
      INTEGER J,JM1,K                                                   
C     BEGIN BLOCK WITH ...EXITS TO 40                                   

                                                                       
         DO 30 J = 1, N                                                 
            INFO = J                                                    
            S = 0.0D0
            JM1 = J - 1                                                 
            IF (JM1 .LT. 1) GO TO 20                                    
            DO 10 K = 1, JM1                                            
               T = A(K,J) - DDOT(K-1,A(1,K),1,A(1,J),1)                 
               T = T/A(K,K)                                             
               A(K,J) = T                                               
               S = S + T*T                                              
   10       CONTINUE                                                    
   20       CONTINUE                                                    
            S = A(J,J) - S                                              
C       ....EXIT                                                        
            IF (S .LE. 0.0D0) GO TO 40                                  
            A(J,J) = DSQRT(S)                                           
   30    CONTINUE                                                       
         INFO = 0                                                       
   40 CONTINUE
      RETURN                                                            
      END                                                               
                                                                       
      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)                        
                                                                       
C     RETURNS SUM OF MAGNITUDES OF DOUBLE PRECISION DX.                 
C     DASUM = SUM FROM 0 TO N-1 OF DABS(DX(1+I*INCX))                   
                                                                       
      DOUBLE PRECISION DX(N)                                            
      DASUM = 0.D0                                                      
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.1)GOTO 20                                              

C        CODE FOR INCREMENTS NOT EQUAL TO 1.
                                                                       
      NS = N*INCX                                                       
          DO 10 I=1,NS,INCX                                             
          DASUM = DASUM + DABS(DX(I))                                   
   10     CONTINUE                                                      
      RETURN                                                            
C
C        CODE FOR INCREMENTS EQUAL TO 1.                                
C                                                                       
C                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.   
C
   20 M = MOD(N,6)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
         DASUM = DASUM + DABS(DX(I))                                    
   30 CONTINUE
      IF( N .LT. 6 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,6                                                 
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))    
     1   + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))                
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                       

      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)                            
                                                                       
C     OVERWRITE DOUBLE PRECISION DY WITH DOUBLE PRECISION DA*DX + DY.   
C     FOR I = 0 TO N-1, REPLACE  DY(LY+I*INCY) WITH DA*DX(LX+I*INCX) +  
C       DY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
                                                                       
      DOUBLE PRECISION DX(N),DY(N),DA                                   
      IF(N.LE.0.OR.DA.EQ.0.D0) RETURN                                   
      
      IF(INCX.EQ.INCY) THEN
      	 IF((INCX-1).LT.0) THEN 
      	    GO TO 5
      	 ELSE IF  ((INCX-1).EQ.0) THEN 
      		GO TO 20
      	 ELSE 
      		GO TO 60
      	 ENDIF
      ELSE                                          
      GO TO 5	                                                                                   
      ENDIF                                   
      	 	                                                                              
    5 CONTINUE                                                          
                                                                       
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.                   
                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N                                                     
        DY(IY) = DY(IY) + DA*DX(IX)                                     
        IX = IX + INCX
        IY = IY + INCY                                                  
   10 CONTINUE                                                          
      RETURN                                                            
C                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1                            
C                                                                       
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.   
C                                                                       
   20 M = MOD(N,4)                                                      
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M                                                     
        DY(I) = DY(I) + DA*DX(I)                                        
   30 CONTINUE                                                          
      IF( N .LT. 4 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)                                        
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)                            
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)                            
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)                            
   50 CONTINUE                                                          
      RETURN                                                            
                                                                       
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.                  
                                                                       
   60 CONTINUE                                                          
      NS = N*INCX                                                       
          DO 70 I=1,NS,INCX                                             
          DY(I) = DA*DX(I) + DY(I)                                      
   70     CONTINUE
      RETURN                                                            
      END                                                               
                                                                       
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)                 
                                                                       
C     RETURNS THE DOT PRODUCT OF DOUBLE PRECISION DX AND DY.            
C     DDOT = SUM FOR I = 0 TO N-1 OF  DX(LX+I*INCX) * DY(LY+I*INCY)     
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS       
C     DEFINED IN A SIMILAR WAY USING INCY.                              
                                                                       
      DOUBLE PRECISION DX(N),DY(N)
      DDOT = 0.D0                                                       
      IF(N.LE.0)RETURN 
      	                                                 
      IF(INCX.EQ.INCY) THEN
      	 IF((INCX-1).LT.0) THEN 
      	    GO TO 5
      	 ELSE IF  ((INCX-1).EQ.0) THEN 
      		GO TO 20
      	 ELSE 
      		GO TO 60
      	 ENDIF
      ELSE
      GO TO 5	                                                                      
      ENDIF

    5 CONTINUE                                                          

C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.                   
                                                                       
      IX = 1                                                            
      IY = 1                                                            
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1                                 
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1                                 
      DO 10 I = 1,N
         DDOT = DDOT + DX(IX)*DY(IY)                                    
        IX = IX + INCX                                                  
        IY = IY + INCY
   10 CONTINUE                                                          
      RETURN                                                            
                                                                       
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.                           
                                                                       
                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
                                                                       
   20 M = MOD(N,5)                                                      
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
         DDOT = DDOT + DX(I)*DY(I)                                      
   30 CONTINUE                                                          
      IF( N .LT. 5 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,5                                                 
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +                  
     1   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE                                                          
      RETURN
                                                                      
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.                     
                                                                       
   60 CONTINUE                                                          
      NS = N*INCX                                                       
          DO 70 I=1,NS,INCX                                             
          DDOT = DDOT + DX(I)*DY(I)                                     
   70     CONTINUE                                                      
      RETURN                                                            
      END
C                                                                       
      SUBROUTINE DSCAL(N,DA,DX,INCX)                                    
                                                                       
C     REPLACE DOUBLE PRECISION DX BY DOUBLE PRECISION DA*DX.            
C     FOR I = 0 TO N-1, REPLACE DX(1+I*INCX) WITH  DA * DX(1+I*INCX)    
                                                                       
      DOUBLE PRECISION DA,DX(N)
      IF(N.LE.0)RETURN                                                  
      IF(INCX.EQ.1)GOTO 20                                              
                                                                       
C        CODE FOR INCREMENTS NOT EQUAL TO 1.                            
                                                                       
      NS = N*INCX                                                       
          DO 10 I = 1,NS,INCX
          DX(I) = DA*DX(I)                                              
   10     CONTINUE
      RETURN                                                            
                                                                       
C        CODE FOR INCREMENTS EQUAL TO 1.                                
                                                                       
                                                                       
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.   
                                                                       
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40                                           
      DO 30 I = 1,M                                                     
        DX(I) = DA*DX(I)                                                
   30 CONTINUE                                                          
      IF( N .LT. 5 ) RETURN                                             
   40 MP1 = M + 1                                                       
      DO 50 I = MP1,N,5                                                 
        DX(I) = DA*DX(I)                                                
        DX(I + 1) = DA*DX(I + 1)                                        
        DX(I + 2) = DA*DX(I + 2)                                        
        DX(I + 3) = DA*DX(I + 3)                                        
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE                                                          
      RETURN                                                            
      END                                                               
                                                                       
      SUBROUTINE SSORT(X,Y,N,KFLAG)                                     
C***BEGIN PROLOGUE   SSORT                                              
C***REVISION  OCTOBER 1,1980                                            
C***CATEGORY NO.  M1                                                    
C***KEYWORD(S) SORTING,SORT,SINGLETON QUICKSORT,QUICKSORT
C***DATE WRITTEN  NOVEMBER,1976                                         
C***AUTHOR  JONES R.E., WISNIEWSKI J.A. (SLA)                           
C***PURPOSE                                                             
C         SSORT SORTS ARRAY X AND OPTIONALLY MAKES THE SAME             
C         INTERCHANGES IN ARRAY Y.  THE ARRAY X MAY BE SORTED IN        
C         INCREASING ORDER OR DECREASING ORDER.  A SLIGHTLY MODIFIED    
C         QUICKSORT ALGORITHM IS USED.                                  
C***DESCRIPTION                                                         
C     SANDIA MATHEMATICAL PROGRAM LIBRARY
C     APPLIED MATHEMATICS DIVISION 2646                                 
C     SANDIA LABORATORIES                                               
C     ALBUQUERQUE, NEW MEXICO  87185                                    
C     CONTROL DATA 6600/7600  VERSION 8.1  AUGUST 1980                  
C                                                                       
C     WRITTEN BY RONDALL E JONES                                        
C     MODIFIED BY JOHN A. WISNIEWSKI TO USE THE SINGLETON QUICKSORT
C     ALGORITHM. DATE 18 NOVEMBER 1976.
C                                                                       
C     ABSTRACT                                                          
C         SSORT SORTS ARRAY X AND OPTIONALLY MAKES THE SAME             
C         INTERCHANGES IN ARRAY Y.  THE ARRAY X MAY BE SORTED IN        
C         INCREASING ORDER OR DECREASING ORDER.  A SLIGHTLY MODIFIED    
C         QUICKSORT ALGORITHM IS USED.                                  
C                                                                       
C     REFERENCE                                                         
C         SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM FOR
C         SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,185-7.          
C                                                                       
C     DESCRIPTION OF PARAMETERS                                         
C         X - ARRAY OF VALUES TO BE SORTED   (USUALLY ABSCISSAS)        
C         Y - ARRAY TO BE (OPTIONALLY) CARRIED ALONG                    
C         N - NUMBER OF VALUES IN ARRAY X TO BE SORTED                  
C         KFLAG - CONTROL PARAMETER                                     
C             =2  MEANS SORT X IN INCREASING ORDER AND CARRY Y ALONG.   
C             =1  MEANS SORT X IN INCREASING ORDER (IGNORING Y)         
C             =-1 MEANS SORT X IN DECREASING ORDER (IGNORING Y)         
C             =-2 MEANS SORT X IN DECREASING ORDER AND CARRY Y ALONG.
C                                                                       
C                                                                       
C***REFERENCE(S)                                                        
C         SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM FOR     
C         SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,185-7.          
C***ROUTINES CALLED  XERROR                                             
C***END PROLOGUE                                                        
C
C     DIMENSION X(N),Y(N),IL(21),IU(21)                                 
      DOUBLE PRECISION X(N)                                             
      INTEGER N,KFLAG,Y(N),IL(21),IU(21)                                
C***FIRST EXECUTABLE STATEMENT    SSORT                                 
      NN = N                                                            
      IF (NN.GE.1) GO TO 10                                             
C     CALL XERROR (58HSSORT- THE NUMBER OF VALUES TO BE SORTED WAS NOT P
C     1OSITIVE.,58,1,1)                                                  
      RETURN                                                            
   10 KK = IABS(KFLAG)                                                  
      IF ((KK.EQ.1).OR.(KK.EQ.2)) GO TO 15
C     CALL XERROR (62HSSORT- THE SORT CONTROL PARAMETER, K, WAS NOT 2, 1
C     1, -1, OR -2.,62,2,1)                                              
      RETURN                                                            
C                                                                       
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED                       
C                                                                       
   15 IF (KFLAG.GE.1) GO TO 30
      DO 20 I=1,NN                                                      
      X(I) = -X(I)
   20 continue   
   30 IF(kk.eq.1) then
         go to 100
      	    else if(kk.eq.2) then
      	    go to 200
      	    endif                                                       
C                                                                       
C SORT X ONLY                                                           
C                                                                       
  100 CONTINUE                                                          
      M=1                                                               
      I=1                                                               
      J=NN
      R=.375                                                            
  110 IF (I .EQ. J) GO TO 155                                           
      IF (R .GT. .5898437) GO TO 120                                    
      R=R+3.90625E-2                                                    
      GO TO 125                                                         
  120 R=R-.21875                                                        
  125 K=I                                                               
C                                  SELECT A CENTRAL ELEMENT OF THE      
C                                  ARRAY AND SAVE IT IN LOCATION T      
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)                                                           
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER 
C                                  THAN T, INTERCHANGE WITH T           
      IF (X(I) .LE. T) GO TO 130                                        
      X(IJ)=X(I)                                                        
      X(I)=T                                                            
      T=X(IJ)
  130 L=J                                                               
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T                
      IF (X(J) .GE. T) GO TO 140                                        
      X(IJ)=X(J)                                                        
      X(J)=T                                                            
      T=X(IJ)                                                           
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER 
C                                  THAN T, INTERCHANGE WITH T           
      IF (X(I) .LE. T) GO TO 140                                        
      X(IJ)=X(I)                                                        
      X(I)=T                                                            
      T=X(IJ)
      GO TO 140                                                         
  135 TT=X(L)                                                           
      X(L)=X(K)                                                         
      X(K)=TT                                                           
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140                                        
C                                  FIND AN ELEMENT IN THE FIRST HALF OF 
C                                  THE ARRAY WHICH IS GREATER THAN T    
  145 K=K+1                                                             
      IF (X(K) .LT. T) GO TO 145                                        
C                                  INTERCHANGE THESE ELEMENTS           
      IF (K .LE. L) GO TO 135                                           
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF   
C                                  THE ARRAY YET TO BE SORTED           
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I                                                           
      IU(M)=L                                                           
      I=K                                                               
      M=M+1                                                             
      GO TO 160                                                         
  150 IL(M)=K                                                           
      IU(M)=J                                                           
      J=L                                                               
      M=M+1
      GO TO 160                                                         
C                                  BEGIN AGAIN ON ANOTHER PORTION OF    
C                                  THE UNSORTED ARRAY                   
  155 M=M-1                                                             
      IF (M .EQ. 0) GO TO 300                                           
      I=IL(M)
      J=IU(M)                                                           
  160 IF (J-I .GE. 1) GO TO 125                                         
      IF (I .EQ. 1) GO TO 110                                           
      I=I-1                                                             
  165 I=I+1                                                             
      IF (I .EQ. J) GO TO 155                                           
      T=X(I+1)                                                          
      IF (X(I) .LE. T) GO TO 165                                        
      K=I                                                               
  170 X(K+1)=X(K)                                                       
      K=K-1                                                             
      IF (T .LT. X(K)) GO TO 170                                        
      X(K+1)=T                                                          
      GO TO 165                                                         
C
C SORT X AND CARRY Y ALONG                                              
C                                                                       
  200 CONTINUE                                                          
      M=1                                                               
      I=1
      J=NN                                                              
      R=.375
  210 IF (I .EQ. J) GO TO 255                                           
      IF (R .GT. .5898437) GO TO 220
      R=R+3.90625E-2                                                    
      GO TO 225                                                         
  220 R=R-.21875                                                        
  225 K=I                                                               
C                                  SELECT A CENTRAL ELEMENT OF THE      
C                                  ARRAY AND SAVE IT IN LOCATION T      
      IJ = I + IFIX (FLOAT (J-I) *R)                                    
      T=X(IJ)
      TY= Y(IJ)                                                         
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER 
C                                  THAN T, INTERCHANGE WITH T           
      IF (X(I) .LE. T) GO TO 230                                        
      X(IJ)=X(I)                                                        
      X(I)=T                                                            
      T=X(IJ)                                                           
       Y(IJ)= Y(I)
       Y(I)=TY                                                          
      TY= Y(IJ)                                                         
  230 L=J                                                               
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 240                                        
      X(IJ)=X(J)                                                        
      X(J)=T                                                            
      T=X(IJ)                                                           
       Y(IJ)= Y(J)                                                      
       Y(J)=TY                                                          
      TY= Y(IJ)                                                         
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER 
C                                  THAN T, INTERCHANGE WITH T           
      IF (X(I) .LE. T) GO TO 240                                        
      X(IJ)=X(I)                                                        
      X(I)=T                                                            
      T=X(IJ)                                                           
       Y(IJ)= Y(I)                                                      
       Y(I)=TY                                                          
      TY= Y(IJ)                                                         
      GO TO 240
  235 TT=X(L)                                                           
      X(L)=X(K)                                                         
      X(K)=TT                                                           
      TTY= Y(L)
       Y(L)= Y(K)                                                       
       Y(K)=TTY                                                         
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T    
  240 L=L-1                                                             
      IF (X(L) .GT. T) GO TO 240                                        
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T    
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
C                                  INTERCHANGE THESE ELEMENTS           
      IF (K .LE. L) GO TO 235                                           
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 250
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 260
  250 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  260 IF (J-I .GE. 1) GO TO 225
      IF (I .EQ. 1) GO TO 210
      I=I-1
  265 I=I+1
      IF (I .EQ. J) GO TO 255
      T=X(I+1)
      TY= Y(I+1)
      IF (X(I) .LE. T) GO TO 265
      K=I
  270 X(K+1)=X(K)
       Y(K+1)= Y(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 270
      X(K+1)=T
       Y(K+1)=TY
      GO TO 265
C
C CLEAN UP
C
  300 IF (KFLAG.GE.1) RETURN
      DO 310 I=1,NN
          X(I) = -X(I)
  310 continue   
      RETURN
      END

c***********************************************************************
c
c   program :  hypergeometric function
c
c   notation :  F(a,b;c;z)
c
c   reference:  see article `Computing the hypergeometric function'
c               by R.C. Forrey, J. Comp. Phys. 137, 79-100 (1997).
c
c   send comments to:
c
c        Robert C. Forrey
c        Institute for Theoretical Atomic and Molecular Physics
c        Harvard-Smithsonian Center for Astrophysics
c        60 Garden Street Cambridge, MA 02138
c        rforrey@cfa.harvard.edu
c
c***********************************************************************
c
c  subroutine name    - hyp
c
c  computation
c  performed          - calculates the hypergeometric function
c
c  usage              - call hyp(z,a,b,c,re,im)
c
c  arguments     
c                  z  - the independent variable of the hypergeometric
c                       function (must be real).
c
c               a,b,c - real parameters of the hypergeometric function.
c
c               re,im - the real and imaginary parts of the
c                       hypergeometric function.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine hyp(z,a,b,c,re,im)

      DOUBLE PRECISION  zero,one,two,half
      parameter (zero=0.d0,one=1.d0,two=2.d0,half=0.5d0)
      integer flag,flag2,neps
      DOUBLE PRECISION   a,b,c,z,w,f,f1,f2,gamm,tol,test,pi,machep,re2,
     #         alpha0,alpha1,rn,binom,eps,re,im,x1,x2,x3,x4,
     #         coeff1,coeff2,coeff3,coeff4,temp1,temp2,term,
     #         a1,b1,c1,a2,b2,c2,alpha2
      logical fix
      common /bcoeff/binom(5151)

c  tabulate the binomial coefficients and set the defaults

      fix=.false.
      call binomc
      tol=.1d0
      im=zero
      nmax=100
      n=5

      call geteps(machep,neps)
      pi=dacos(-1.d0)
c     write(20,'(/a,i3/)') ' machine epsilon is machep = (1/2)**',neps
c     write(20,'(a,d23.15//)') ' to machine accuracy, pi = ',pi

c  handle the special case when z=1

      if (z.eq.one) then
        re=gamm(c)*gamm(c-a-b)/gamm(c-a)/gamm(c-b)
        return
      endif

c  transform to a new variable w which lies between 0 and 1/2

      if(z .lt. -one) then
        a1=a
        b1=c-b
        c1=a-b+1
        a2=b
        b2=c-a
        c2=b-a+1
        w=one/(one-z)
        flag=1
      elseif( (-one.le.z) .and. (z.lt.zero) )then
        a1=a
        b1=c-b
        c1=c
        a2=a1
        b2=b1
        c2=c1
        w=z/(z-one)
        flag=2
      elseif( (zero.le.z) .and. (z.le.half) )then
        a1=a
        b1=b
        c1=c
        a2=a1
        b2=b1
        c2=c1
        w=z
        flag=3
      elseif( (half.lt.z) .and. (z.le.one) )then
        a1=a
        b1=b
        c1=a+b-c+1
        a2=c-a
        b2=c-b
        c2=c-a-b+1
        w=one-z
        flag=4
      elseif( (one.lt.z) .and. (z.le.two) )then
        a1=a
        b1=a-c+1
        c1=a+b-c+1
        a2=c-a
        b2=1-a
        c2=c-a-b+1
        w=one-(one/z)
        flag=5
      elseif(two.lt.z)then
        a1=a
        b1=a-c+1
        c1=a-b+1
        a2=b-c+1
        b2=b
        c2=b-a+1
        w=one/z
        flag=6
      endif

c  compute the hypergeometric function of z via the transformation
c  theory

      if (flag .eq. 1)then
        k=nint(dble(a-b))
        test=a-b-dble(k)
        if (dabs(test).lt.tol) then
          fix=.true.
          flag2=0
          if (a.lt.b) then
            temp1=a
            temp2=b
            b=temp1
            a=temp2
            flag2=1
          endif
          k=nint(dble(a-b))
          eps=a-b-dble(k)
          call fix1(a,b,c,n,k,f1,w,machep,eps)
          do m=n+5,nmax,5
            call fix1(a,b,c,m,k,f2,w,machep,eps)
            test=dabs(f1-f2)
            if(test.le.machep)go to 30
            f1=f2
          end do
C          write(*,*)'fix1 warning: not completely converged'
  30      re=f2    
          if (flag2.eq.1) then
            a=temp1
            b=temp2
          endif
        else
          call hyper(w,a1,b1,c1,f1,machep)
          call hyper(w,a2,b2,c2,f2,machep)

            x1=b
            coeff1=one
  1         if (x1.lt.one) then
              coeff1=coeff1*x1
              x1=x1+one
              go to 1
            endif

            x2=c-a
            coeff2=one
  2         if (x2.lt.one) then
              coeff2=coeff2*x2
              x2=x2+one
              go to 2
            endif

            x3=a
            coeff3=one
  3         if (x3.lt.one) then
              coeff3=coeff3*x3
              x3=x3+one
              go to 3
            endif

            x4=c-b
            coeff4=one
  4         if (x4.lt.one) then
              coeff4=coeff4*x4
              x4=x4+one
              go to 4
            endif

          re=(w**a)*gamm(c)*gamm(b-a)*coeff1*coeff2/gamm(x1)/gamm(x2)*f1
     #     +(w**b)*gamm(c)*gamm(a-b)*coeff3*coeff4/gamm(x3)/gamm(x4)*f2

        endif
      elseif (flag .eq. 2)then
        call hyper(w,a1,b1,c1,f1,machep)
        re=((one-w)**a)*f1
      elseif (flag .eq. 3)then
        call hyper(w,a1,b1,c1,f1,machep)
        re=f1
      elseif (flag .eq. 4)then
        k=nint(dble(c-a-b))
        test=c-a-b-dble(k)
        if (dabs(test).lt.tol) then
          fix=.true.
          if (k.ge.zero) then
            eps=c-a-b-dble(k)
            call fix4a(a,b,c,n,k,f1,w,machep,eps)
            do m=n+5,nmax,5
            call fix4a(a,b,c,m,k,f2,w,machep,eps)
            test=dabs(f1-f2)
            if(test.le.machep)go to 31
            f1=f2
            end do
C           write(*,*)'fix4a warning: not completely converged'
  31        re=f2   
          else
            k=-k
            eps=c-a-b+dble(k)
            call fix4b(a,b,c,n,k,f1,w,machep,eps)
            do m=n+5,nmax,5
            call fix4b(a,b,c,m,k,f2,w,machep,eps)
            test=dabs(f1-f2)
            if(test.le.machep)go to 32
            f1=f2
          end do
C          write(*,*)'fix4b warning: not completely converged'
  32      re=f2   
          endif
        else
          call hyper(w,a1,b1,c1,f1,machep)
          call hyper(w,a2,b2,c2,f2,machep)

            x1=c-a
            coeff1=one
  5         if (x1.lt.one) then
              coeff1=coeff1*x1
              x1=x1+one
              go to 5
            endif

            x2=c-b
            coeff2=one
  6         if (x2.lt.one) then
              coeff2=coeff2*x2
              x2=x2+one
              go to 6
            endif

            x3=a
            coeff3=one
  7         if (x3.lt.one) then
              coeff3=coeff3*x3
              x3=x3+one
              go to 7
            endif

            x4=b
            coeff4=one
  8         if (x4.lt.one) then
              coeff4=coeff4*x4
              x4=x4+one
              go to 8
            endif

          re=gamm(c)*gamm(c-a-b)*coeff1*coeff2/gamm(x1)/gamm(x2)*f1
     #       +w**(c-a-b)*gamm(c)*gamm(a+b-c)*coeff3*coeff4/gamm(x3)
     #       /gamm(x4)*f2

        endif
      elseif (flag .eq. 5)then
        k=nint(dble(c-a-b))
        test=c-a-b-dble(k)
        if (dabs(test).lt.tol) then
          fix=.true.
          if (k.ge.zero) then
            eps=c-a-b-dble(k)
            call fix5a(a,b,c,n,k,f1,im,w,machep,eps,pi)
            do m=n+5,nmax,5
            call fix5a(a,b,c,m,k,f2,im,w,machep,eps,pi)
            test=dabs(f1-f2)
            if(test.le.machep)go to 33
            f1=f2
            end do
C            write(*,*)'fix5a warning: not completely converged'
  33        re=f2   
          else
            k=-k
            eps=c-a-b+dble(k)
            call fix5b(a,b,c,n,k,f1,im,w,machep,eps,pi)
            do m=n+5,nmax,5
            call fix5b(a,b,c,m,k,f2,im,w,machep,eps,pi)
            test=dabs(f1-f2)
            if(test.le.machep)go to 34
            f1=f2
            end do
C            write(*,*)'fix5b warning: not completely converged'
  34        re=f2   
          endif
        else
          call hyper(w,a1,b1,c1,f1,machep)
          call hyper(w,a2,b2,c2,f2,machep)

            x1=c-a
            coeff1=one
  11        if (x1.lt.one) then
              coeff1=coeff1*x1
              x1=x1+one
              go to 11
            endif

            x2=c-b
            coeff2=one
  12        if (x2.lt.one) then
              coeff2=coeff2*x2
              x2=x2+one
              go to 12
            endif

            x3=a
            coeff3=one
  13        if (x3.lt.one) then
              coeff3=coeff3*x3
              x3=x3+one
              go to 13
            endif

            x4=b
            coeff4=one
  14        if (x4.lt.one) then
              coeff4=coeff4*x4
              x4=x4+one
              go to 14
            endif

          re=(one-w)**a*gamm(c)*gamm(c-a-b)*coeff1*coeff2/gamm(x1)
     #       /gamm(x2)*f1+w**(c-a-b)*(one-w)**b*dcos(pi*(c-a-b))
     #       *gamm(c)*gamm(a+b-c)*coeff3*coeff3/gamm(x3)/gamm(x4)*f2

        endif
      elseif (flag .eq. 6)then
        k=nint(dble(a-b))
        test=a-b-dble(k)
        if (dabs(test).lt.tol) then
          fix=.true.
          flag2=0
          if (a.lt.b) then
            temp1=a
            temp2=b
            b=temp1
            a=temp2
            flag2=1
          endif
          k=nint(dble(a-b))
          eps=a-b-dble(k)
          call fix6(a,b,c,n,k,f1,im,w,machep,eps,pi)
          do m=n+5,nmax,5
          call fix6(a,b,c,m,k,f2,im,w,machep,eps,pi)
          test=dabs(f1-f2)
          if(test.le.machep)go to 35
          f1=f2
          end do
C          write(*,*)'fix6 warning: not completely converged'
  35      re=f2   
          if (flag2.eq.1) then
            a=temp1
            b=temp2
          endif
        else
          call hyper(w,a1,b1,c1,f1,machep)
          call hyper(w,a2,b2,c2,f2,machep)

            x1=b
            coeff1=one
  15        if (x1.lt.one) then
              coeff1=coeff1*x1
              x1=x1+one
              go to 15
            endif

            x2=c-a
            coeff2=one
  16        if (x2.lt.one) then
              coeff2=coeff2*x2
              x2=x2+one
              go to 16
            endif

            x3=a
            coeff3=one
  17        if (x3.lt.one) then
              coeff3=coeff3*x3
              x3=x3+one
              go to 17
            endif

            x4=c-b
            coeff4=one
  18        if (x4.lt.one) then
              coeff4=coeff4*x4
              x4=x4+one
              go to 18
            endif

          re=w**a*dcos(pi*a)*gamm(c)*gamm(b-a)*coeff1*coeff2/gamm(x1)
     #       /gamm(x2)*f1+w**b*dcos(pi*b)*gamm(c)*gamm(a-b)*coeff3
     #       *coeff4/gamm(x3)/gamm(x4)*f2

        endif
      endif

c     if(fix)then
c       write(20,'(2(a6,2x,i3,2x))')'case=',flag,'m=',m
c     endif

      return
      end


c***********************************************************************
c
c  subroutine name     - hyper
c
c  computation
c  performed           - calculates the hypergeometric function,
c                        f(a,b;c;w), from its power series for 0<w<.5.
c
c  usage               - call hyper(w,a,b,c,f)
c
c  arguments        
c                   w  - the transformed independent variable.
c
c                a,b,c - the parameters of the hypergeometric function.
c
c                   f  - the computed value of the hypergeometric
c                        function which is returned to the caller.
c
c  precision           - double
c
c  language            - fortran
c
c***********************************************************************

      subroutine hyper(w,a,b,c,f,machep)
      implicit none
      integer i,m,n,nmax,k,k0,k1
      parameter (nmax=100)
      DOUBLE PRECISION  a,b,c,w,f,alpha0,alpha1,rn,gamm,term,machep,
     *binom
      common /bcoeff/binom(5151)

c  compute the number of sums needed to get good convergence

      alpha1=a+b-c
      k1=nint(dble(alpha1))

      do 10 n=1,nmax

        rn=0.d0
        alpha0=(a+n+1)*(b+n+1)/(c+n+1)-(n+1)
        k0=nint(dble(alpha0))
        k=max(k0,k1)
        if (k.le.1) k=1
        if (n+k.ge.100) then
C          write(*,*)'error in hyp:  binomial coefficient routine
C     #                              no longer valid'
          return
        endif

        do m=0,k
          rn=rn+binom((n+k+1)*(n+k+2)/2+m+1)
        end do   

        term=1.d0
        do i=1,n+1
          term=(a+i-1)*(b+i-1)/(c+i-1)/(k+i)*term
        end do

        rn=rn*term*(w**(n+1))/(1-w)
        if (dabs(rn).lt.machep) go to 100

 10   continue

C      write(*,*)'error in hyp:  nmax not large enough'
      return

 100  continue
c     write(20,'(2(a6,i3,5x))')'n=',n

c  evaluate the hypergeometric function of w from its power series

      term=1.d0
      f=1.d0
      do 20 k=1,n
        term=term*(a+k-1)*(b+k-1)/(c+k-1)*w/k
        f=f+term
  20  continue

      return
      end

c***********************************************************************
c
c  function name      - gamm
c
c  computation
c  performed          - calculates the gamma function
c
c  usage              - gamm(x)
c
c  argument         x - any real number (excluding negative integers).
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      function gamm(x)

      DOUBLE PRECISION  zero,one
      parameter(zero=0.d0,one=1.d0)
      DOUBLE PRECISION  x,xx,coeff,gamm,g

c  scale input variable and change it's name
c  so that it does not get destroyed

      xx=x-one
      coeff=one

  100 if ( (zero.le.xx) .and. (xx.le.one) ) go to 200

        if (xx.lt.zero) then
          xx=xx+one
          coeff=coeff/xx
          go to 100
        else
          coeff=xx*coeff
          xx=xx-one
          go to 100
        endif

  200 gamm=coeff*g(xx)

      return
      end

c***********************************************************************
c
c  function name     - g
c
c  computation
c  performed         - calculates gamma(xx+1) for xx in the interval
c                      [0,1] using clenshaw's recurrence formula with
c                      tchebychev polynomials and the tabulated
c                      expansion coefficients.
c
c  usage             - g(xx)
c
c  argument       xx - scaled value for 'x' in 'gamm(x)'.
c
c  precision         - double
c
c  language          - fortran
c
c***********************************************************************

      function g(xx)

      DOUBLE PRECISION  zero,one,two
      parameter (zero=0.d0,one=1.d0,two=2.d0)
      DOUBLE PRECISION  c(0:41),xx,y,y1,y2,g

c  use clenshaw recurrence scheme with tchebychev polynomials
c  and the expansion coefficients tabulated in 'cheb' for 0<xx<1 .

      call cheb(c,41,1)

      y1=zero
      y2=zero

      do 10 k=41,1,-1
        y=two*(two*xx-one)*y1-y2+c(k)
        y2=y1
        y1=y
   10 continue

      g=-y2+(two*xx-one)*y1+c(0)

      return
      end

c**********************************************************************
c
c  subroutine name    - fix1
c
c  computation
c  performed          - calculates the hypergeometric function for
c                       z less than -1 when a-b is near an integer.
c
c  usage              - call fix1(a,b,c,n,k,f,w,machep,eps)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of a-b.
c
c                  f  - computed value of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals a-b-k.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix1(a,b,c,n,k,f,w,machep,eps)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,f,eps,gamm,machep,test,arg,rn,sum,et1,
     #         et2,term1,term2,term3,term4,term5,term6,term7,term8,
     #         temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7,
     #         coeff,coeff1,coeff2,coeff3,coeff4,x,x1,x2,x3,x4,
     #         t1(0:80),t2(0:80),t3(0:80),t4(0:80),c1(0:80),c2(0:80),
     #         c3(0:80),c4(0:80),f1(0:80),f2(0:80),f3(0:80),f4(0:80),
     #         g1(0:nmax),g2(0:nmax),g3(0:nmax),g4(0:nmax),g5(0:nmax),
     #         fff1(0:nmax),ff1(0:nmax),fff2(0:nmax),ff2(0:nmax),
     #         ff3(0:nmax),ff4(0:nmax),poch1(0:nmax),poch2(0:nmax),
     #         e1(0:nmax),e2(0:nmax),e3(0:nmax),e4(0:nmax)

      integer  flag

c  calculate the extra terms

      x=b-one
      sum=zero
      coeff=one
      flag=0
  1   if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 1
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
  2     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 2
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 1
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 3 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 3      continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 4 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 4      continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 5 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 5      continue
      endif

      if (flag.eq.0) then
        x1=b
        coeff1=one
 6      if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 6
        endif
        x2=b+eps
        coeff2=one
 7      if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 7
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 8      if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 8
        endif
        x2=x+one+eps
        coeff2=one
 9      if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 9
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif
      et1=-et1
c     write(10,*)et1,(one/gamm(a-dble(k)-eps)-one/gamm(a-dble(k)))
c    #                                           /eps
      x=c-a+dble(k)-one
      sum=zero
      coeff=one
      flag=0
  10  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 10
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 11     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 11
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 10
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c2,41,1)
        t2(0)=one
        t2(1)=two*(x+eps)-one
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 12 i=2,41
          t2(i)=(four*(x+eps)-two)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-two)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 12     continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c2,55,2)
        t2(0)=one
        t2(1)=two*(x+eps)
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 13 i=2,55
          t2(i)=four*(x+eps)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+four*x*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 13     continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c2,34,3)
        t2(0)=one
        t2(1)=two*(x+eps)-two
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 14 i=2,34
          t2(i)=(four*(x+eps)-four)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-four)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 14     continue
      endif

      if (flag.eq.0) then
        x1=c-a+dble(k)
        coeff1=one
 15     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 15
        endif
        x2=c-a+dble(k)+eps
        coeff2=one
 16     if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 16
        endif
        temp2=sum+coeff*temp2
        et2=-temp2*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 17     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 17
        endif
        x2=x+one+eps
        coeff2=one
 18     if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 18
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp2
      endif

c     write(10,*)et2,(one/gamm(c-a+dble(k)+eps)-one/gamm(c-a+dble(k)))
c    #                                                         /eps

c  calculate the f-functions

      x1=a
      coeff1=one
 20   if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 20
      endif

      x2=a-dble(k)
      coeff2=one
 21   if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 21
      endif

      x3=c-a
      coeff3=one
 22   if (x3.lt.one) then
        coeff3=x3*coeff3
        x3=x3+one
        go to 22
      endif

      x4=c-a+dble(k)
      coeff4=one
 23   if (x4.lt.one) then
        coeff4=x4*coeff4
        x4=x4+one
        go to 23
      endif

      coeff=one
      arg=-eps-dble(k)
 24   if (arg.lt.-eps) then
        coeff=coeff/arg
        arg=arg+one
        go to 24
      endif

      fff1(0)=one
      fff2(0)=one
      ff1(0)=one
      ff2(0)=one
      do 25 i=1,k
        fff1(0)=(b+dble(i-1))*fff1(0)
        ff2(0)=(c-a+dble(i-1))*ff2(0)
 25   continue

      fff1(0)=fff1(0)*coeff1/gamm(x1)
      fff2(0)=coeff3/gamm(x3)
      ff1(0)=coeff2/gamm(x2)
      ff2(0)=ff2(0)*coeff4/gamm(x4)
      ff3(0)=(-1)**(k+1)*gamm(one-eps)*coeff
      ff4(0)=gamm(one+eps)

c     do 26 i=1,n
c       fff1(i)=(b+dble(k+i-1))*fff1(i-1)
c       fff2(i)=(c-b+dble(i-1))*fff2(i-1)
c       ff1(i)=(a+dble(i-1))*ff1(i-1)
c       ff2(i)=(c-a+dble(k+i-1))*ff2(i-1)
c       ff3(i)=ff3(i-1)/(eps+dble(i)+dble(k))
c       ff4(i)=ff4(i-1)/(dble(i)-eps)
c26   continue

c     do 27 i=0,n
c       write(10,*)'fff1=',fff1(i),gamm(b+i+k)/gamm(a)/gamm(b)
c       write(10,*)'ff1=',ff1(i),gamm(a+i)/gamm(a)/gamm(a-k)
c       write(10,*)'fff2=',fff2(i),gamm(c-b+i)/gamm(c-a)/gamm(c-b)
c       write(10,*)'ff2=',ff2(i),gamm(c-a+i+k)/gamm(c-a)/gamm(c-a+k)
c       write(10,*)'ff3=',ff3(i),(-1)**(k+i)*eps*gamm(-k-i-eps)
c       write(10,*)'ff4=',ff4(i),(-1)**i*eps*gamm(eps-i)
c27   continue

c   calculate  g1,g2

      x1=a
      coeff1=one
 100  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 100
      endif

      x2=c-a
      coeff2=one
 101  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 101
      endif

      g1(0)=zero
      g2(0)=zero
      poch1(0)=one
      poch2(0)=one
      do 102 i=1,k
        g1(0)=g1(0)*(a-eps+dble(i-k-1))-poch1(0)
        poch1(0)=poch1(0)*(a+dble(i-k-1))
 102  continue

      g1(0)=g1(0)*coeff1/gamm(x1)
      g2(0)=g2(0)*coeff2/gamm(x2)
      poch1(0)=poch1(0)*coeff1/gamm(x1)
      poch2(0)=poch2(0)*coeff2/gamm(x2)
      do 103 i=1,n
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        poch2(i)=(c-a+dble(k+i-1))*poch2(i-1)
        g1(i)=g1(i-1)*(a-eps+dble(i-1))-poch1(i-1)
        g2(i)=g2(i-1)*(c-a+eps+dble(k+i-1))+poch2(i-1)
 103  continue

c     do 104 i=0,n
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps
c       write(10,*)'g2=',g2(i),(fff2(i)-ff2(i))/eps
c104  continue

c  calculate  g3,g4,g5

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3-eps)
      f3(0)=zero
      f3(1)=-two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4+eps)
      f4(0)=zero
      f4(1)=two
      g4(0)=c4(1)*f4(1)

      do 105 i=2,55
        t3(i)=four*(x3-eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4+eps)*t4(i-1)-t4(i-2)
        f3(i)=-four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
 105  continue

      g3(0)=-g3(0)
      do 106 i=-k,-1
        g3(0)=(g3(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)+eps)
 106  continue

      test=dabs(eps*dlog(w))
      temp=-dlog(w)
      if (eps.le.zero) then
         if (test.ge.eighth) then
           temp=(one-exp(test))/eps
         else
           i=1
 107       rn=(eps**(i)*(dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 108
           temp=temp-rn
           i=i+1
           go to 107
         endif
 108     g5(0)=w**(a-eps)*temp
      else
         if (test.ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
 109       rn=(eps**(i)*(-dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 110
           temp=temp+rn
           i=i+1
           go to 109
         endif
 110     g5(0)=(w**a)*temp
      endif

c     write(10,*)g3(0),(-1)**k*gamm(-k-eps)+one/eps/gamm(dble(k+1))
c     write(10,*)g4(0),gamm(eps)-one/eps
c     write(10,*)g5(0),w**(a-eps)/eps-w**a/eps

      e1(0)=one/gamm(dble(k+1))
      e2(0)=one
      do 120 i=1,n
        e1(i)=e1(i-1)/dble(k+i)
        e2(i)=e2(i-1)/dble(i)
        g3(i)=(g3(i-1)+e1(i))/(dble(k+i)+eps)
        g4(i)=(g4(i-1)+e2(i))/(dble(i)-eps)
        g5(i)=w*g5(i-1)
 120  continue

      e1(0)=one
      e2(0)=one
      e3(0)=one
      e4(0)=one
      do 130 i=1,k
        e2(0)=(c-a+dble(i-1))/dble(i)*e2(0)
        e4(0)=e4(0)/dble(i)
 130  continue

c     do 140 i=1,n
c       e1(i)=(a+dble(i-1))/dble(i)*e1(i-1)
c       e2(i)=(c-a+dble(k+i-1))/dble(k+i)*e2(i-1)
c       e3(i)=e3(i-1)/dble(i)
c       e4(i)=e4(i-1)/dble(k+i)
c140  continue

c   put everything back together again

      term1=-gamm(c)*w**a*e3(0)*fff2(0)*ff3(0)*(-1)**k
      term2=gamm(c)*w**a*e3(0)*fff1(0)*ff3(0)*(-1)**k
      term3=gamm(c)*w**a*e3(0)*fff1(0)*ff2(0)*(-1)**k
      term4=gamm(c)*w**a*e4(0)*fff1(0)*ff2(0)*(-1)**k
      term5=gamm(c)*e4(0)*fff1(0)*ff2(0)*ff4(0)*(-1)**k
      term6=gamm(c)*w**a*e1(0)*fff2(0)*ff3(0)*(-1)**k
      term7=gamm(c)*w**(a-eps)*e2(0)*fff1(0)*ff4(0)*(-1)**k

      temp=g1(0)*term1+g2(0)*term2+g3(0)*term3+g4(0)*term4
     #                                       +g5(0)*term5
      temp1=term6
      temp2=term7
      do 150 i=1,n
        term1=term1*w*(c-b+dble(i-1))/(eps+dble(i+k))/dble(i)
        term2=term2*w*(b+dble(k+i-1))/(eps+dble(i+k))/dble(i)
        term3=term3*w*(b+dble(k+i-1))*(c-a+dble(k+i-1))/dble(i)
        term4=term4*w*(b+dble(k+i-1))*(c-a+dble(k+i-1))/dble(k+i)
        term5=term5*(b+dble(k+i-1))*(c-a+dble(k+i-1))/dble(k+i)
     #                                       /(dble(i)-eps)
        term6=term6*w*(a+dble(i-1))/dble(i)*(c-b+dble(i-1))
     #                                       /(eps+dble(i+k))
        term7=term7*w*(c-a+dble(k+i-1))/dble(k+i)*(b+dble(k+i-1))
     #                                       /(dble(i)-eps)
        temp=temp+g1(i)*term1+g2(i)*term2+g3(i)*term3+g4(i)*term4
     #                                       +g5(i)*term5
        temp1=temp1+term6
        temp2=temp2+term7
 150  continue

c  calculate the finite series term

      poch1(0)=one
      poch2(0)=one
      do 160 i=1,k-1
        poch1(i)=(b+dble(i-1))*poch1(i-1)
        poch2(i)=(c-a+dble(i-1))*poch2(i-1)
 160  continue

      temp6=zero
      do 170 i=0,k-1
        temp6=temp6+poch1(i)*poch2(i)*gamm(dble(k-i)+eps)
     #                  /gamm(dble(i+1))*(-w)**i
 170  continue

      x1=a
      coeff1=one
 180  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 180
      endif

      x2=c-b
      coeff2=one
 190  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 190
      endif

      f=temp+et1*temp1+et2*temp2+coeff1*coeff2/gamm(x1)/gamm(x2)
     #                             *gamm(c)*w**(a-eps-dble(k))*temp6

c  alternative method  (must also turn on the individual functions)

c     temp3=zero
c     temp4=zero
c     temp5=zero
c     do 200 i=0,n
c       term1=-gamm(c)*w**(a+dble(i))*e3(i)*g1(i)*fff2(i)*ff3(i)*(-1)**k
c       term2=gamm(c)*w**(a+dble(i))*e3(i)*fff1(i)*g2(i)*ff3(i)*(-1)**k
c       term3=gamm(c)*w**(a+dble(i))*e3(i)*fff1(i)*ff2(i)*g3(i)*(-1)**k
c       term4=gamm(c)*w**(a+dble(i))*e4(i)*fff1(i)*ff2(i)*g4(i)*(-1)**k
c       term5=gamm(c)*fff1(i)*e4(i)*ff2(i)*ff4(i)*g5(i)*(-1)**k
c       temp3=temp3+term1+term2+term3+term4+term5
c       temp4=temp4+gamm(c)*w**(a+dble(i))*e1(i)*fff2(i)*ff3(i)*(-1)**k
c       temp5=temp5+gamm(c)*w**(a+dble(i)-eps)*e2(i)*fff1(i)*ff4(i)
c    #                                                       *(-1)**k
c200  continue
c     write(10,*)'temp=',temp,temp3
c     write(10,*)'temp1=',temp1,temp4
c     write(10,*)'temp2=',temp2,temp5

c     x=temp3+et1*temp4+et2*temp5+coeff1*coeff2/gamm(x1)/gamm(x2)
c    #                             *gamm(c)*w**(a-eps-dble(k))*temp6
c     write(10,*)'f=',f,x

      return
      end

c**********************************************************************
c
c  subroutine name    - fix4a
c
c  computation
c  performed          - calculates the hypergeometric function for z
c                       in the interval (.5,1) when c-a-b is near a
c                       positive integer.
c
c  usage              - call fix4a(a,b,c,n,k,f,w,machep,eps)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of c-a-b.
c
c                  f  - computed value of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals c-a-b-k.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix4a(a,b,c,n,k,f,w,machep,eps)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,f,gamm,eps,machep,test,arg,rn,sum,et1,
     #         et2,term1,term2,term3,term4,term5,term6,term7,term8,
     #         temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,
     #         coeff,coeff1,coeff2,coeff3,coeff4,x,x1,x2,x3,x4,
     #         t1(0:80),t2(0:80),t3(0:80),t4(0:80),c1(0:80),c2(0:80),
     #         c3(0:80),c4(0:80),f1(0:80),f2(0:80),f3(0:80),f4(0:80),
     #         g1(0:nmax),g2(0:nmax),g3(0:nmax),g4(0:nmax),g5(0:nmax),
     #         fff1(0:nmax),ff1(0:nmax),fff2(0:nmax),ff2(0:nmax),
     #         ff3(0:nmax),ff4(0:nmax),poch1(0:nmax),poch2(0:nmax),
     #         e1(0:nmax),e2(0:nmax),e3(0:nmax),e4(0:nmax)

      integer  flag

c  calculate the extra terms

      x=a+dble(k)-one
      sum=zero
      coeff=one
      flag=0
  1   if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 1
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
  2     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 2
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 1
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 3 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
  3     continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 4 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
  4     continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 5 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
  5     continue
      endif

      if (flag.eq.0) then
        x1=a+dble(k)
        coeff1=one
  6     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 6
        endif
        x2=a+dble(k)+eps
        coeff2=one
  7     if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 7
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
  8     if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 8
        endif
        x2=x+one+eps
        coeff2=one
  9     if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 9
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif

c     write(10,*)et1,(one/gamm(a+k+eps)-one/gamm(a+k))/eps

      x=b+dble(k)-one
      sum=zero
      coeff=one
      flag=0
  10  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 10
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
  11    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 11
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 10
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c2,41,1)
        t2(0)=one
        t2(1)=two*(x+eps)-one
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 12 i=2,41
          t2(i)=(four*(x+eps)-two)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-two)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  12    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c2,55,2)
        t2(0)=one
        t2(1)=two*(x+eps)
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 13 i=2,55
          t2(i)=four*(x+eps)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+four*x*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  13    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c2,34,3)
        t2(0)=one
        t2(1)=two*(x+eps)-two
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 14 i=2,34
          t2(i)=(four*(x+eps)-four)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-four)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  14    continue
      endif

      if (flag.eq.0) then
        x1=b+dble(k)
        coeff1=one
  15    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 15
        endif
        x2=b+dble(k)+eps
        coeff2=one
  16    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 16
        endif
        temp2=sum+coeff*temp2
        et2=-temp2*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
  17    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 17
        endif
        x2=x+one+eps
        coeff2=one
  18    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 18
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp2
      endif

c     write(10,*)et2,(one/gamm(b+k+eps)-one/gamm(b+k))/eps

c  calculate the f-functions

      x1=a
      coeff1=one
  20  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 20
      endif

      x2=b
      coeff2=one
  21  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 21
      endif

      coeff=one
      arg=-eps-dble(k)
  22  if (arg.lt.-eps) then
        coeff=coeff/arg
        arg=arg+one
        go to 22
      endif

      ff1(0)=coeff1/gamm(x1)
      ff2(0)=coeff2/gamm(x2)
      fff1(0)=coeff1/gamm(x1)
      fff2(0)=coeff2/gamm(x2)
      ff3(0)=(-1)**(k+1)*gamm(one-eps)*coeff
      ff4(0)=gamm(one+eps)

c     do 23 i=1,n
c       ff1(i)=(a+dble(k+i-1))*ff1(i-1)
c       ff2(i)=(b+dble(k+i-1))*ff2(i-1)
c       fff1(i)=(c-b+dble(i-1))*fff1(i-1)
c       fff2(i)=(c-a+dble(i-1))*fff2(i-1)
c       ff3(i)=ff3(i-1)/(eps+dble(i)+dble(k))
c       ff4(i)=ff4(i-1)/(dble(i)-eps)
c 23  continue

c     do 24 i=0,n
c       write(10,*)'fff1=',fff1(i),gamm(c-b+i)/gamm(a)/gamm(c-b)
c       write(10,*)'ff1=',ff1(i),gamm(a+k+i)/gamm(a)/gamm(a+k)
c       write(10,*)'fff2=',fff2(i),gamm(c-a+i)/gamm(b)/gamm(c-a)
c       write(10,*)'ff2=',ff2(i),gamm(b+k+i)/gamm(b)/gamm(b+k)
c       write(10,*)'ff3=',ff3(i),(-1)**(k+i)*eps*gamm(-k-i-eps)
c       write(10,*)'ff4=',ff4(i),(-1)**i*eps*gamm(eps-i)
c 24  continue

c   calculate  g1,g2

      g1(0)=zero
      g2(0)=zero
      poch1(0)=coeff1/gamm(x1)
      poch2(0)=coeff2/gamm(x2)
      do 100 i=1,n
        poch1(i)=(a+dble(k+i-1))*poch1(i-1)
        poch2(i)=(b+dble(k+i-1))*poch2(i-1)
        g1(i)=g1(i-1)*(a+eps+dble(k+i-1))+poch1(i-1)
        g2(i)=g2(i-1)*(b+eps+dble(k+i-1))+poch2(i-1)
 100  continue

c     do 101 i=0,n
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps
c       write(10,*)'g2=',g2(i),(fff2(i)-ff2(i))/eps
c101  continue

c  calculate  g3,g4,g5

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3-eps)
      f3(0)=zero
      f3(1)=-two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4+eps)
      f4(0)=zero
      f4(1)=two
      g4(0)=c4(1)*f4(1)

      do 105 i=2,55
        t3(i)=four*(x3-eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4+eps)*t4(i-1)-t4(i-2)
        f3(i)=-four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
 105  continue

      g3(0)=-g3(0)
      do 106 i=-k,-1
        g3(0)=(g3(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)+eps)
 106  continue

      test=eps*dlog(w)
      temp=dlog(w)
         if (dabs(test).ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
 107       rn=(eps**(i)*(dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 108
           temp=temp+rn
           i=i+1
           go to 107
         endif
 108     g5(0)=(w**k)*temp

c     write(10,*)g3(0),(-1)**k*gamm(-k-eps)+one/eps/gamm(dble(k+1))
c     write(10,*)g4(0),gamm(eps)-one/eps
c     write(10,*)g5(0),w**(k+eps)/eps-w**k/eps

      do 120 i=1,n
        g3(i)=(g3(i-1)+one/gamm(dble(k+i+1)))/(dble(k+i)+eps)
        g4(i)=(g4(i-1)+one/gamm(dble(i+1)))/(dble(i)-eps)
        g5(i)=w*g5(i-1)
 120  continue

      e1(0)=one
      e2(0)=one
      e3(0)=-one
      e4(0)=one
      do 130 i=1,k
        e1(0)=(a+dble(i-1))*e1(0)
        e2(0)=(b+dble(i-1))*e2(0)
        e3(0)=e3(0)/dble(i)
 130  continue

c     do 140 i=1,n
c       e1(i)=(a+dble(k+i-1))*e1(i-1)
c       e2(i)=(b+dble(k+i-1))*e2(i-1)
c       e3(i)=e3(i-1)/dble(k+i)
c       e4(i)=e4(i-1)/dble(i)
c140  continue

c  put everything back together again

      term1=gamm(c)*(-1)**k*fff2(0)*ff3(0)*e4(0)*w**(c-a-b)
      term2=gamm(c)*(-1)**k*ff1(0)*ff3(0)*e4(0)*w**(c-a-b)
      term3=gamm(c)*(-1)**k*ff1(0)*ff2(0)*e4(0)*w**(c-a-b)
      term4=-gamm(c)*(-1)**k*ff1(0)*ff2(0)*e3(0)*w**(c-a-b)
      term5=gamm(c)*(-1)**k*ff1(0)*ff2(0)*e3(0)*ff4(0)
      term6=-gamm(c)*(-w)**k*et1*e1(0)*ff2(0)*e3(0)*ff4(0)
      term7=-gamm(c)*(-w)**k*et2*ff1(0)*e2(0)*e3(0)*ff4(0)
      term8=-gamm(c)*(-w)**k*eps*et1*et2*e1(0)*e2(0)*e3(0)*ff4(0)

      temp=g1(0)*term1+g2(0)*term2+g3(0)*term3+g4(0)*term4
     #                                       +g5(0)*term5
      temp1=term6
      temp2=term7
      temp3=term8

      do 150 i=1,n
        term1=term1*w*(b+eps+dble(k+i-1))/(eps+dble(i+k))/dble(i)
        term2=term2*w*(a+dble(k+i-1))/(eps+dble(i+k))/dble(i)
        term3=term3*w*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(i)
        term4=term4*w*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(k+i)
        term5=term5*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(k+i)
     #                                       /(dble(i)-eps)
        term6=term6*w*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(k+i)
     #                                       /(dble(i)-eps)
        term7=term7*w*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(k+i)
     #                                       /(dble(i)-eps)
        term8=term8*w*(a+dble(k+i-1))*(b+dble(k+i-1))/dble(k+i)
     #                                       /(dble(i)-eps)
        temp=temp+g1(i)*term1+g2(i)*term2+g3(i)*term3+g4(i)*term4
     #                                       +g5(i)*term5
        temp1=temp1+term6
        temp2=temp2+term7
        temp3=temp3+term8
 150  continue

c  calculate the finite series term

      poch1(0)=one
      poch2(0)=one
      do 160 i=1,k-1
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        poch2(i)=(b+dble(i-1))*poch2(i-1)
 160  continue

      temp4=zero
      do 170 i=0,k-1
        temp4=temp4+poch1(i)*poch2(i)*gamm(eps+dble(k-i))*(-w)**i
     #                                      /gamm(dble(i+1))
 170  continue

      x1=c-a
      coeff1=one
 180  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 180
      endif

      x2=c-b
      coeff2=one
 190  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 190
      endif

      temp4=temp4*gamm(c)*coeff1*coeff2/gamm(x1)/gamm(x2)

      f=temp+temp1+temp2+temp3+temp4

c  alternative method  (must also turn on the individual functions)

c     temp5=zero
c     temp6=zero
c     temp7=zero
c     temp8=zero
c     do 200 i=0,n
c       term1=gamm(c)*(-1)**k*e4(i)*g1(i)*fff2(i)*ff3(i)
c    #                                     *w**(dble(k+i)+eps)
c       term2=gamm(c)*(-1)**k*e4(i)*g2(i)*ff1(i)*ff3(i)
c    #                                     *w**(dble(k+i)+eps)
c       term3=gamm(c)*(-1)**k*e4(i)*g3(i)*ff1(i)*ff2(i)
c    #                                     *w**(dble(k+i)+eps)
c       term4=-gamm(c)*(-1)**k*e3(i)*g4(i)*ff1(i)*ff2(i)
c    #                                     *w**(dble(k+i)+eps)
c       term5=gamm(c)*(-1)**k*e3(i)*g5(i)*ff1(i)*ff2(i)*ff4(i)

c       temp5=temp5+term1+term2+term3+term4+term5
c       temp6=temp6-gamm(c)*e3(i)*(-1)**k*et1*e1(i)*ff2(i)*ff4(i)
c    #                                            *w**(k+i)
c       temp7=temp7-gamm(c)*e3(i)*(-1)**k*et2*e2(i)*ff1(i)*ff4(i)
c    #                                            *w**(k+i)
c       temp8=temp8-gamm(c)*e3(i)*(-1)**k*eps*et1*et2*e1(i)*e2(i)*ff4(i)
c    #                                            *w**(k+i)
c200  continue
c     write(10,*)'temp=',temp,temp5
c     write(10,*)'temp1=',temp1,temp6
c     write(10,*)'temp2=',temp2,temp7
c     write(10,*)'temp3=',temp3,temp8

c     x=temp5+temp6+temp7+temp8+temp4
c     write(10,*)'f=',f,x

      return
      end

c**********************************************************************
c
c  subroutine name    - fix4b
c
c  computation
c  performed          - calculates the hypergeometric function for z
c                       in the interval (.5,1) when c-a-b is near a
c                       negative integer.
c
c  usage              - call fix4b(a,b,c,n,k,f,w,machep,eps)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of a+b-c.
c
c                  f  - computed value of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals c-a-b+k.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix4b(a,b,c,n,k,f,w,machep,eps)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,f,gamm,eps,machep,test,arg,rn,sum,et1,
     #         et2,term1,term2,term3,term4,term5,term6,term7,term8,
     #         temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,
     #         coeff,coeff1,coeff2,coeff3,coeff4,x,x1,x2,x3,x4,
     #         t1(0:80),t2(0:80),t3(0:80),t4(0:80),c1(0:80),c2(0:80),
     #         c3(0:80),c4(0:80),f1(0:80),f2(0:80),f3(0:80),f4(0:80),
     #         g1(0:nmax),g2(0:nmax),g3(0:nmax),g4(0:nmax),g5(0:nmax),
     #         fff1(0:nmax),ff1(0:nmax),fff2(0:nmax),ff2(0:nmax),
     #         ff3(0:nmax),ff4(0:nmax),poch1(0:nmax),poch2(0:nmax),
     #         e1(0:nmax),e2(0:nmax),e3(0:nmax),e4(0:nmax)

      integer  flag

c  calculate the extra terms

      x=a-dble(k)-one
      sum=zero
      coeff=one
      flag=0
   1  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 1
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
   2    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 2
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 1
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 3 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
   3    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 4 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
   4    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 5 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
   5    continue
      endif

      if (flag.eq.0) then
        x1=a-dble(k)
        coeff1=one
   6    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 6
        endif
        x2=a-dble(k)+eps
        coeff2=one
   7    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 7
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
   8    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 8
        endif
        x2=x+one+eps
        coeff2=one
   9    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 9
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif

c     write(10,*)et1,(one/gamm(a-k+eps)-one/gamm(a-k))/eps

      x=b-dble(k)-one
      sum=zero
      coeff=one
      flag=0
  10  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 10
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
  11    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 11
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 10
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c2,41,1)
        t2(0)=one
        t2(1)=two*(x+eps)-one
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 12 i=2,41
          t2(i)=(four*(x+eps)-two)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-two)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  12    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c2,55,2)
        t2(0)=one
        t2(1)=two*(x+eps)
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 13 i=2,55
          t2(i)=four*(x+eps)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+four*x*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  13    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c2,34,3)
        t2(0)=one
        t2(1)=two*(x+eps)-two
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 14 i=2,34
          t2(i)=(four*(x+eps)-four)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-four)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
  14    continue
      endif

      if (flag.eq.0) then
        x1=b-dble(k)
        coeff1=one
  15    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 15
        endif
        x2=b-dble(k)+eps
        coeff2=one
  16    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 16
        endif
        temp2=sum+coeff*temp2
        et2=-temp2*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
  17    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 17
        endif
        x2=x+one+eps
        coeff2=one
  18    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 18
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp2
      endif

c     write(10,*)et2,(one/gamm(b-k+eps)-one/gamm(b-k))/eps

c  calculate the f-functions

      x1=a
      coeff1=one
  20  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 20
      endif

      x2=b
      coeff2=one
  21  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 21
      endif

      x3=a-dble(k)
      coeff3=one
  22  if (x3.lt.one) then
        coeff3=x3*coeff3
        x3=x3+one
        go to 22
      endif

      x4=b-dble(k)
      coeff4=one
  23  if (x4.lt.one) then
        coeff4=x4*coeff4
        x4=x4+one
        go to 23
      endif

      coeff=one
      arg=eps-dble(k)
  24  if (arg.lt.eps) then
        coeff=coeff/arg
        arg=arg+one
        go to 24
      endif

      fff1(0)=one
      fff2(0)=one
      ff1(0)=one
      ff2(0)=one
      do 25 i=1,k
        fff1(0)=(c-b+dble(i-1))*fff1(0)
        fff2(0)=(c-a+dble(i-1))*fff2(0)
  25  continue

      fff1(0)=fff1(0)*coeff1/gamm(x1)
      fff2(0)=fff2(0)*coeff2/gamm(x2)
      ff1(0)=ff1(0)*coeff3/gamm(x3)
      ff2(0)=ff2(0)*coeff4/gamm(x4)
      ff3(0)=-gamm(one-eps)
      ff4(0)=(-1)**k*gamm(one+eps)*coeff

c     do 26 i=1,n
c       fff1(i)=(c-b+dble(k+i-1))*fff1(i-1)
c       fff2(i)=(c-a+dble(k+i-1))*fff2(i-1)
c       ff1(i)=(a+dble(i-1))*ff1(i-1)
c       ff2(i)=(b+dble(i-1))*ff2(i-1)
c       ff3(i)=ff3(i-1)/(eps+dble(i))
c       ff4(i)=ff4(i-1)/(dble(k+i)-eps)
c 26  continue

c     do 27 i=0,n
c       write(10,*)'fff1=',fff1(i),gamm(a+eps+i)/gamm(a)/gamm(c-b)
c       write(10,*)'fff2=',fff2(i),gamm(b+eps+i)/gamm(b)/gamm(c-a)
c       write(10,*)'ff1=',ff1(i),gamm(a+i)/gamm(a)/gamm(a-k)
c       write(10,*)'ff2=',ff2(i),gamm(b+i)/gamm(b)/gamm(b-k)
c       write(10,*)'ff3=',ff3(i),(-1)**i*eps*gamm(-i-eps)
c       write(10,*)'ff4=',ff4(i),(-1)**(k+i)*eps*gamm(eps-k-i)
c 27  continue

c   calculate  g1,g2

      x1=a
      coeff1=one
 100  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 100
      endif

      x2=b
      coeff2=one
 101  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 101
      endif

      g1(0)=zero
      g2(0)=zero
      poch1(0)=one
      poch2(0)=one
      do 102 i=1,k
        g1(0)=g1(0)*(a+eps+dble(i-k-1))+poch1(0)
        g2(0)=g2(0)*(b+eps+dble(i-k-1))+poch2(0)
        poch1(0)=poch1(0)*(a+dble(i-k-1))
        poch2(0)=poch2(0)*(b+dble(i-k-1))
 102  continue

      g1(0)=g1(0)*coeff1/gamm(x1)
      g2(0)=g2(0)*coeff2/gamm(x2)
      poch1(0)=poch1(0)*coeff1/gamm(x1)
      poch2(0)=poch2(0)*coeff2/gamm(x2)
      do 103 i=1,n
        poch1(i)=(a+i-1)*poch1(i-1)/i
        poch2(i)=(b+i-1)*poch2(i-1)/i
        g1(i)=(g1(i-1)*(a+eps+i-1)+poch1(i-1))/i
        g2(i)=(g2(i-1)*(b+eps+i-1)+poch2(i-1))/i
 103  continue

c     do 104 i=0,n
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps/gamma(i+1.0)
c       write(10,*)'g2=',g2(i),(fff2(i)-ff2(i))/eps/gamma(i+1.0)
c104  continue

c  calculate  g3,g4,g5

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3-eps)
      f3(0)=zero
      f3(1)=-two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4+eps)
      f4(0)=zero
      f4(1)=two
      g4(0)=c4(1)*f4(1)

      do 105 i=2,55
        t3(i)=four*(x3-eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4+eps)*t4(i-1)-t4(i-2)
        f3(i)=-four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
 105  continue

      g3(0)=-g3(0)
      do 106 i=-k,-1
        g4(0)=(g4(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)-eps)
 106  continue

      test=eps*dlog(w)
      temp=dlog(w)
         if (dabs(test).ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
 107       rn=(eps**(i)*(dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 108
           temp=temp+rn
           i=i+1
           go to 107
         endif
 108     g5(0)=temp

c     write(10,*)g3(0),gamm(-eps)+one/eps
c     write(10,*)g4(0),(-1)**k*gamm(eps-dble(k))-one/eps/gamm(dble(k+1))
c     write(10,*)g5(0),w**eps/eps-one/eps

      do 120 i=1,n
        temp=one/gamm(dble(k+1))
        do 121 j=1,i
          temp=temp*dble(j)/dble(k+j)
 121    continue
        g3(i)=(g3(i-1)*dble(i)+one)/(dble(i)+eps)
        g4(i)=(g4(i-1)*dble(i)+temp)/(dble(k+i)-eps)
        g5(i)=w*g5(i-1)
 120  continue

      e1(0)=one
      e2(0)=one
      e3(0)=-one
      e4(0)=one
      do 130 i=1,k
        e4(0)=e4(0)/dble(i)
 130  continue

c     do 140 i=1,n
c       e1(i)=(a+dble(i-1))*e1(i-1)
c       e2(i)=(b+dble(i-1))*e2(i-1)
c       e3(i)=e3(i-1)/dble(i)
c       e4(i)=e4(i-1)/dble(k+i)
c140  continue

c  put everything back together again

      term1=gamm(c)*(-1)**k*fff2(0)*ff3(0)*e4(0)*w**eps
      term2=gamm(c)*(-1)**k*ff1(0)*ff3(0)*e4(0)*w**eps
      term3=gamm(c)*(-1)**k*ff1(0)*ff2(0)*e4(0)*w**eps
      term4=-gamm(c)*(-1)**k*ff1(0)*ff2(0)*e3(0)*w**eps
      term5=gamm(c)*(-1)**k*ff1(0)*ff2(0)*e3(0)*ff4(0)
      term6=-gamm(c)*(-1)**k*et1*e1(0)*ff2(0)*e3(0)*ff4(0)
      term7=-gamm(c)*(-1)**k*et2*ff1(0)*e2(0)*e3(0)*ff4(0)
      term8=-gamm(c)*(-1)**k*eps*et1*et2*e1(0)*e2(0)*e3(0)*ff4(0)

      temp=g1(0)*term1+g2(0)*term2+g3(0)*term3+g4(0)*term4
     #                                       +g5(0)*term5
      temp1=term6
      temp2=term7
      temp3=term8

      do 150 i=1,n
        term1=term1*w*(b+eps+dble(i-1))/(eps+dble(i))*dble(i)/dble(k+i)
        term2=term2*w*(a+dble(i-1))/(eps+dble(i))*dble(i)/dble(k+i)
        term3=term3*w*(a+dble(i-1))/dble(i)*(b+dble(i-1))/dble(k+i)
        term4=term4*w*(a+dble(i-1))/dble(i)*(b+dble(i-1))/dble(i)
        term5=term5*(a+dble(i-1))/dble(i)*(b+dble(i-1))/(dble(k+i)-eps)
        term6=term6*w*(a+dble(i-1))/dble(i)*(b+dble(i-1))
     #                                                 /(dble(k+i)-eps)
        term7=term7*w*(a+dble(i-1))/dble(i)*(b+dble(i-1))
     #                                                 /(dble(k+i)-eps)
        term8=term8*w*(a+dble(i-1))/dble(i)*(b+dble(i-1))
     #                                                 /(dble(k+i)-eps)
        temp=temp+g1(i)*term1+g2(i)*term2
     #        +g3(i)*term3+g4(i)*term4+g5(i)*term5
        temp1=temp1+term6
        temp2=temp2+term7
        temp3=temp3+term8
 150  continue

c  calculate the finite series term

      poch1(0)=one
      poch2(0)=one
      do 160 i=1,k-1
        poch1(i)=(c-a+dble(i-1))*poch1(i-1)
        poch2(i)=(c-b+dble(i-1))*poch2(i-1)
 160  continue

      temp4=zero
      do 170 i=0,k-1
        temp4=temp4+poch1(i)*poch2(i)*gamm(-eps+dble(k-i))*(-1)**i
     #                  *w**(eps+dble(i-k))/gamm(dble(i+1))
 170  continue

      x1=a
      coeff1=one
 180  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 180
      endif

      x2=b
      coeff2=one
 190  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 190
      endif

      temp4=temp4*gamm(c)*coeff1*coeff2/gamm(x1)/gamm(x2)

      f=temp+temp1+temp2+temp3+temp4

c  alternative method (must also turn on the individual functions)

c     temp5=zero
c     temp6=zero
c     temp7=zero
c     temp8=zero
c     do 200 i=0,n
c       term1=w**(dble(i)+eps)/gamm(dble(k+i+1))
c    #                      *g1(i)*fff2(i)*ff3(i)
c       term2=w**(dble(i)+eps)/gamm(dble(k+i+1))
c    #                      *g2(i)*ff1(i)*ff3(i)
c       term3=w**(dble(i)+eps)/gamm(dble(k+i+1))
c    #                      *g3(i)*ff1(i)*ff2(i)
c       term4=w**(dble(i)+eps)/gamm(dble(i+1))
c    #                      *g4(i)*ff1(i)*ff2(i)
c       term5=-ff1(i)/gamm(dble(i+1))*ff2(i)*ff4(i)*g5(i)
c
c       temp5=temp5+term1+term2+term3+term4+term5
c
c       temp6=temp6+ff1(i)*et2/gamm(dble(i+1))*ff4(i)*e2(i)
c    #                                 *w**(dble(i))
c       temp7=temp7+ff2(i)*et1/gamm(dble(i+1))*ff4(i)*e1(i)
c    #                                 *w**(dble(i))
c       temp8=temp8+e1(i)*et1*et2*eps/gamm(dble(i+1))*ff4(i)
c    #                                 *w**(dble(i))*e2(i)
c200  continue
c     write(10,*)'temp=',temp,temp5*gamm(c)*(-1)**k
c     write(10,*)'temp1=',temp1,temp7*gamm(c)*(-1)**k
c     write(10,*)'temp2=',temp2,temp6*gamm(c)*(-1)**k
c     write(10,*)'temp3=',temp3,temp8*gamm(c)*(-1)**k
c
c     x=(-1)**k*gamm(c)*(temp5+temp6+temp7+temp8)+temp4
c
c     write(10,*)'f=',f,x

      return
      end

c**********************************************************************
c
c  subroutine name    - fix5a
c
c  computation
c  performed          - calculates the hypergeometric function for z
c                       in the interval (1,2) when c-a-b is near a
c                       positive integer.
c
c  usage              - call fix5a(a,b,c,n,k,re,im,w,machep,eps,pi)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of c-a-b.
c
c               re,im - computed values for the real and imaginary parts
c                       of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals c-a-b-k.
c
c                 pi  - equals 3.1415... to machine accuracy.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix5a(a,b,c,n,k,re,im,w,machep,eps,pi)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,re,im,gamm,temp,temp2,g1(0:nmax),g2,
     #         g3(0:nmax),g4(0:nmax),g5(0:nmax),x,x1,x2,x3,x4,psi,rn,
     #         t1(0:80),t2(0:80),t3(0:80),t4(0:80),test,machep,pi,
     #         f1(0:80),f2(0:80),f3(0:80),f4(0:80),ff3(0:nmax),eps,
     #         ff4(0:nmax),coeff1,coeff2,c1(0:80),c2(0:80),c3(0:80),
     #         c4(0:80),sum,term1,term2,term3,term4,term5,poch1(0:nmax),
     #         coeff,temp1,et1,et2,e1(0:nmax),e2(0:nmax),e3(0:nmax),
     #         ff1(0:nmax),fff1(0:nmax),coeff3,coeff4,f(0:nmax),error,
     #         poch2(0:nmax)

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3+eps)
      f3(0)=zero
      f3(1)=two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4-eps)
      f4(0)=zero
      f4(1)=-two
      g4(0)=c4(1)*f4(1)

      do 7 i=2,55
        t3(i)=four*(x3+eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4-eps)*t4(i-1)-t4(i-2)
        f3(i)=four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=-four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
  7   continue

      g4(0)=-g4(0)
      do 10 i=-k,-1
        g4(0)=(g4(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)+eps)
  10  continue

      test=eps*dlog(w)
      temp=dlog(w)
         if (dabs(test).ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
  20       rn=(eps**(i)*(dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 30
           temp=temp+rn
           i=i+1
           go to 20
         endif
  30     g5(0)=temp*w**k

c     write(10,*)g3(0),gamm(-eps)+one/eps
c     write(10,*)g4(0),(-1)**k*gamm(eps-dble(k))-one/eps/gamm(dble(k+1))
c     write(10,*)g5(0),w**eps/eps-one/eps

      do 60 i=1,n
        g3(i)=(g3(i-1)+one/gamm(dble(i+1)))/(dble(i)-eps)
        g4(i)=(g4(i-1)+one/gamm(dble(k+i+1)))/(dble(k+i)+eps)
        g5(i)=w*g5(i-1)
  60  continue

      do 65 i=0,n
        ff3(i)=eps*g3(i)+one/gamm(dble(i+1))
        ff4(i)=eps*g4(i)-one/gamm(dble(k+i+1))
  65  continue

c  calculate the extra terms

      x=a-one
      sum=zero
      coeff=one
      flag=0
  61  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 61
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 610    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 610
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 61
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 611 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 611    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 612 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 612    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 613 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 613    continue
      endif

      if (flag.eq.0) then
        x1=a
        coeff1=one
 614    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 614
        endif
        x2=a+eps
        coeff2=one
 615    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 615
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 616    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 616
        endif
        x2=x+one+eps
        coeff2=one
 617    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 617
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif
      et1=-et1
c     write(10,*)et1,(one/gamm(c-b-dble(k)-eps)-one/gamm(c-b-dble(k)))
c    #                                                  /eps

      x=b-one
      sum=zero
      coeff=one
      flag=0
  62  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 62
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 620    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 620
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 62
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c2,41,1)
        t2(0)=one
        t2(1)=two*(x+eps)-one
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 621 i=2,41
          t2(i)=(four*(x+eps)-two)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-two)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 621    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c2,55,2)
        t2(0)=one
        t2(1)=two*(x+eps)
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 622 i=2,55
          t2(i)=four*(x+eps)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+four*x*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 622    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c2,34,3)
        t2(0)=one
        t2(1)=two*(x+eps)-two
        f2(0)=zero
        f2(1)=two
        temp2=c2(1)*f2(1)
        do 623 i=2,34
          t2(i)=(four*(x+eps)-four)*t2(i-1)-t2(i-2)
          f2(i)=four*t2(i-1)+(four*x-four)*f2(i-1)-f2(i-2)
          temp2=temp2+c2(i)*f2(i)
 623    continue
      endif

      if (flag.eq.0) then
        x1=b
        coeff1=one
 624    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 624
        endif
        x2=b+eps
        coeff2=one
 625    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 625
        endif
        temp2=sum+coeff*temp2
        et2=-temp2*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 626    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 626
        endif
        x2=x+one+eps
        coeff2=one
 627    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 627
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp2
      endif

c     write(10,*)et2,(one/gamm(b+eps)-one/gamm(b))/eps

      fff1(0)=one
      do 685 i=1,k
        fff1(0)=(a+dble(i-1))*fff1(0)
 685  continue

      ff1(0)=one
      do 686 i=1,n
        fff1(i)=(a+dble(k+i-1))*fff1(i-1)
        ff1(i)=(c-b+dble(i-1))*ff1(i-1)
 686  continue

      x1=c-b
      coeff1=one
 687  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 687
      endif

      x2=c-b-dble(k)
      coeff2=one
 688  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 688
      endif

      do 691 i=0,n
        x3=b+eps-dble(i)
        coeff3=one
 689    if (x3.lt.one) then
          coeff3=x3*coeff3
          x3=x3+one
          go to 689
        endif

        x4=b-dble(i)
        coeff4=one
 690    if (x4.lt.one) then
          coeff4=x4*coeff4
          x4=x4+one
          go to 690
        endif
        f(i)=ff1(i)*coeff4/gamm(x4)
        fff1(i)=fff1(i)*coeff1*coeff3/gamm(x1)/gamm(x3)
        ff1(i)=ff1(i)*coeff2*coeff4/gamm(x2)/gamm(x4)
c       write(10,*)'fff1=',fff1(i),gamm(c-b-eps+dble(i))
c    #            /gamm(a)/gamm(b+eps-dble(i))/gamm(c-b)
c       write(10,*)'ff1=',ff1(i),gamm(c-b+dble(i))
c    #            /gamm(a+eps)/gamm(b-dble(i))/gamm(c-b)
 691  continue

c   calculate  g1

      e1(0)=zero
      poch1(0)=one
      do 697 i=1,k
        e1(0)=e1(0)*(c-b-eps+dble(i-k-1))-poch1(0)
        poch1(0)=poch1(0)*(c-b+dble(i-k-1))
 697  continue
      do 698 i=1,n
        poch1(i)=(c-b+dble(i-1))*poch1(i-1)
        e1(i)=e1(i-1)*(c-b-eps+dble(i-1))-poch1(i-1)
 698  continue

      do 700 i=0,n
        x1=b-dble(i)
        coeff1=one
 699    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 699
        endif
        e1(i)=e1(i)*coeff1/gamm(x1)
 700  continue

      e2(0)=et2
      do 702 i=1,n
        x1=b-dble(i-1)
        coeff1=one
 701    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 701
        endif
        e2(i)=e2(i-1)*(b+eps-dble(i))+coeff1/gamm(x1)
 702  continue

      e3(0)=one
      do 703 i=1,k
        e3(0)=(a+dble(i-1))*e3(0)
 703  continue

      do 704 i=1,n
        e3(i)=(a+dble(k+i-1))*e3(i-1)
 704  continue

      x1=c-b
      coeff1=one
 705  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 705
      endif

      do 706 i=0,n
        g1(i)=(e2(i)*e3(i)+e1(i))*coeff1/gamm(x1)
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps
 706  continue

c  calculate g2

      g2=zero
      if (dabs(eps).lt..1d0) then
        i=1
 707    rn=(-1)**i*pi**(i+i)*eps**(i+i-1)/gamm(dble(i+i+1))
        if (dabs(rn).lt.machep) go to 708
        g2=g2+rn
        i=i+1
        go to 707
      else
        g2=(cos(pi*eps)-one)/eps
      endif
 708  continue
c     write(10,*)'g2=',g2,(cos(pi*eps)-one)/eps

      temp=zero
      temp1=zero
      do 70 i=0,n
        term1=-g1(i)*cos(pi*eps)/gamm(dble(i+1))
     #                      *ff4(i)*w**(eps+dble(i+k))
        term2=fff1(i)*g2/gamm(dble(i+1))
     #                      *ff4(i)*w**(eps+dble(i+k))
        term3=-fff1(i)*g3(i)*ff4(i)*w**(eps+dble(i+k))
        term4=fff1(i)*ff3(i)*g4(i)*w**(eps+dble(i+k))
        term5=-fff1(i)*ff3(i)/gamm(dble(k+i+1))*g5(i)
        temp=temp+(term1+term2+term3+term4+term5)*(-1)**i
        temp1=temp1+(et1*f(i)*cos(pi*eps)/gamm(dble(i+1))*ff4(i)
     #                              *w**(dble(i+k)+eps))*(-1)**i
  70  continue

      poch1(0)=one
      poch2(0)=one
      do 71 i=1,k-1
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        poch2(i)=(one-c+a+dble(i-1))*poch2(i-1)
  71  continue

      x1=c-a
      coeff1=one
  72  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 72
      endif

      x2=c-b
      coeff2=one
  73  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 73
      endif

      temp2=zero
      do 80 i=0,k-1
        temp2=temp2+poch1(i)*poch2(i)*coeff1*coeff2/gamm(x1)/gamm(x2)
     #        *gamm(dble(k-i)+eps)/gamm(dble(i+1))*(-w)**i
  80  continue

c     term1=zero
c     do 81 i=0,k-1
c       term1=term1+gamm(a+dble(i))/gamm(a)*gamm(a-c+dble(1+i))
c    #        /gamm(a-c+one)*gamm(eps+dble(k-i))*(-w)**i/gamm(dble(i+1))
c    #        /gamm(c-a)/gamm(c-b)
c 81  continue
c     write(10,*)temp2,term1

      re=(one-w)**a*gamm(c)*(temp+temp1+temp2)

c  calculate the imaginary part

      x1=a
      coeff1=one
  90  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 90
      endif

      temp=zero
      do 91 i=0,n
        temp=temp+(-1)**i*f(i)/gamm(dble(i+1))*ff4(i)*w**(eps+dble(i+k))
     #       *coeff1/gamm(x1)
  91  continue

      if (dabs(eps).lt..1d0) then
        temp1=one
        i=1
  92    temp2=temp1+(-1)**i*(pi*eps)**(i+i)/gamm(dble(i+i+2))
        error=(temp2-temp1)/temp2
        if (dabs(error).lt.machep) go to 93
        i=i+1
        temp1=temp2
        go to 92
      else
        temp2=sin(pi*eps)/pi/eps
      endif
  93  continue
c     write(10,*)temp2,sin(pi*eps)/pi/eps

      im=-pi*temp2*temp

      return
      end

c**********************************************************************
c
c  subroutine name    - fix5b
c
c  computation
c  performed          - calculates the hypergeometric function for z
c                       in the interval (1,2) when c-a-b is near a
c                       negative integer.
c
c  usage              - call fix5b(a,b,c,n,k,re,im,w,machep,eps,pi)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of a+b-c.
c
c               re,im - computed values for the real and imaginary parts
c                       of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals c-a-b+k.
c
c                 pi  - equals 3.1415... to machine accuracy.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix5b(a,b,c,n,k,re,im,w,machep,eps,pi)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,re,im,gamm,temp,temp2,g1(0:nmax),
     #       g2(0:nmax),g3(0:nmax),g4(0:nmax),g5(0:nmax),x,x1,x2,x3,x4,
     #       psi,rn,t1(0:80),t2(0:80),t3(0:80),t4(0:80),test,machep,pi,
     #       f1(0:80),f2(0:80),f3(0:80),f4(0:80),ff3(0:nmax),eps,
     #       ff4(0:nmax),coeff1,coeff2,c1(0:80),c2(0:80),c3(0:80),
     #       c4(0:80),sum,term1,term2,term3,term4,term5,term6,
     #       coeff,temp1,et1,et2,e1,e2(0:nmax),coeff3,coeff4,
     #       fff1(0:nmax),fff2(0:nmax),ff1(0:nmax),ff2(0:nmax),
     #       poch1(0:nmax),poch2(0:nmax),ttest,error

      integer  flag

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3-eps)
      f3(0)=zero
      f3(1)=-two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4+eps)
      f4(0)=zero
      f4(1)=two
      g4(0)=c4(1)*f4(1)

      do 7 i=2,55
        t3(i)=four*(x3-eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4+eps)*t4(i-1)-t4(i-2)
        f3(i)=-four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
  7   continue

      g3(0)=-g3(0)
      do 10 i=-k,-1
        g4(0)=(g4(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)-eps)
  10  continue

      test=eps*dlog(w)
      temp=dlog(w)
         if (dabs(test).ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
  20       rn=(eps**(i)*(dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 30
           temp=temp+rn
           i=i+1
           go to 20
         endif
  30     g5(0)=temp

c     write(10,*)g3(0),gamm(-eps)+one/eps
c     write(10,*)g4(0),(-1)**k*gamm(eps-dble(k))-one/eps/gamm(dble(k+1))
c     write(10,*)g5(0),w**eps/eps-one/eps

      do 60 i=1,n
        g3(i)=(g3(i-1)+one/gamm(dble(i+1)))/(dble(i)+eps)
        g4(i)=(g4(i-1)+one/gamm(dble(k+i+1)))/(dble(k+i)-eps)
        g5(i)=w*g5(i-1)
  60  continue

      do 65 i=0,n
        ff3(i)=eps*g3(i)-one/gamm(dble(i+1))
        ff4(i)=eps*g4(i)+one/gamm(dble(k+i+1))
  65  continue

c  calculate the extra terms

      x=a-dble(k)-one
      sum=zero
      coeff=one
      flag=0
  61  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 61
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 610    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 610
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 61
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 611 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 611    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 612 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 612    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 613 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 613    continue
      endif

      if (flag.eq.0) then
        x1=a-dble(k)
        coeff1=one
 614    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 614
        endif
        x2=a-dble(k)+eps
        coeff2=one
 615    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 615
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 616    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 616
        endif
        x2=x+one+eps
        coeff2=one
 617    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 617
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif

c     write(10,*)et1,(one/gamm(a-dble(k)+eps)-one/gamm(a-dble(k)))
c    #                                                  /eps

      x=b-dble(k)-one
      sum=zero
      coeff=one
      flag=0
  62  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 62
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 620    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 620
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 62
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 621 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 621    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 622 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 622    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 623 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 623    continue
      endif

      if (flag.eq.0) then
        x1=b-dble(k)
        coeff1=one
 624    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 624
        endif
        x2=b-dble(k)+eps
        coeff2=one
 625    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 625
        endif
        temp=sum+coeff*temp
        et2=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 626    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 626
        endif
        x2=x+one+eps
        coeff2=one
 627    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 627
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp
      endif

c     write(10,*)et2,(one/gamm(b-dble(k)+eps)-one/gamm(b-dble(k)))
c    #                                                  /eps

      fff1(0)=one
      do 685 i=1,k
        fff1(0)=(c-b+dble(i-1))*fff1(0)
 685  continue

      ff1(0)=one
      e2(0)=one
      do 686 i=1,n
        fff1(i)=(c-b+dble(k+i-1))*fff1(i-1)
        ff1(i)=(a+dble(i-1))*ff1(i-1)
        e2(i)=ff1(i)
 686  continue

      x1=a
      coeff1=one
 687  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 687
      endif

      x2=a-dble(k)
      coeff2=one
 688  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 688
      endif

      do 691 i=0,n
        x3=b+eps-dble(i+k)
        coeff3=one
 689    if (x3.lt.one) then
          coeff3=x3*coeff3
          x3=x3+one
          go to 689
        endif

        x4=b-dble(i+k)
        coeff4=one
 690    if (x4.lt.one) then
          coeff4=x4*coeff4
          x4=x4+one
          go to 690
        endif
        fff1(i)=fff1(i)*coeff1/gamm(x1)
        ff1(i)=ff1(i)*coeff2/gamm(x2)
        fff2(i)=coeff3/gamm(x3)
        ff2(i)=coeff4/gamm(x4)
c       write(10,*)'fff1=',fff1(i),gamm(c-b+dble(i+k))/gamm(a)/gamm(c-b)
c       write(10,*)'ff1=',ff1(i),gamm(a+dble(i))/gamm(a)/gamm(a-dble(k))
c       write(10,*)'fff2=',fff2(i),one/gamm(b+eps-dble(k+i))
c       write(10,*)'ff2=',ff2(i),one/gamm(b-dble(k+i))
 691  continue

c   calculate  g1

      g1(0)=zero
      poch1(0)=one
      do 697 i=1,k
        g1(0)=g1(0)*(a+eps+dble(i-k-1))+poch1(0)
        poch1(0)=poch1(0)*(a+dble(i-k-1))
 697  continue
      do 698 i=1,n
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        g1(i)=g1(i-1)*(a+eps+dble(i-1))+poch1(i-1)
 698  continue

      x1=a
      coeff1=one
 699  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 699
      endif
      do 700 i=0,n
        g1(i)=g1(i)*coeff1/gamm(x1)
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps
 700  continue

c   calculate  g2

      g2(0)=et2
      do 702 i=1,n
        x1=b-dble(k+i-1)
        coeff1=one
 701    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 701
        endif
        g2(i)=g2(i-1)*(b+eps-dble(i+k))+coeff1/gamm(x1)
c       write(10,*)'g2=',g2(i),(fff2(i)-ff2(i))/eps
 702  continue

c  calculate  e1

      e1=zero
      if (dabs(eps).lt..1d0) then
        i=1
 703    rn=(-1)**i*pi**(i+i)*eps**(i+i-1)/gamm(dble(i+i+1))
        if (dabs(rn).lt.machep) go to 704
        e1=e1+rn
        i=i+1
        go to 703
      else
        e1=(cos(pi*eps)-one)/eps
      endif
 704  continue
c     write(10,*)'e1=',e1,(cos(pi*eps)-one)/eps

c  put everything back together again

      ttest=zero
      temp=zero
      temp1=zero
      do 70 i=0,n
        term1=g1(i)/gamm(dble(k+i+1))*ff2(i)*ff3(i)
     #                   *cos(pi*eps)*w**(dble(i)+eps)
        term2=-ff1(i)/gamm(dble(k+i+1))*g2(i)*ff3(i)
     #                   *cos(pi*eps)*w**(dble(i)+eps)
        term3=ff1(i)/gamm(dble(k+i+1))*fff2(i)*g3(i)
     #                   *cos(pi*eps)*w**(dble(i)+eps)
        term4=ff1(i)/gamm(dble(i+1))*fff2(i)*g4(i)
     #                   *cos(pi*eps)*w**(dble(i)+eps)
        term5=-ff1(i)/gamm(dble(i+1))*ff4(i)*fff2(i)
     #                   *cos(pi*eps)*g5(i)
        term6=-ff1(i)/gamm(dble(i+1))*ff4(i)*fff2(i)
     #                   *w**(dble(i))*e1
        temp=temp+(term1+term2+term3+term4+term5+term6)*(-1)**(k+i)
        temp1=temp1+e2(i)/gamm(dble(i+1))*et1*fff2(i)
     #                       *ff4(i)*w**(dble(i))*(-1)**(k+i)
c       ttest=ttest+(-1)**(k+i)*(cos(pi*eps)*fff1(i)*ff2(i)*ff3(i)
c    #    /gamm(dble(k+i+1))*w**(dble(i)+eps)+ff1(i)*fff2(i)
c    #    /gamm(dble(i+1))*ff4(i)*w**(dble(i)))/eps
c       write(10,*)temp,ttest
c       ttest=ttest+(-1)**(k+i)*gamm(a+dble(i))/gamm(a)*fff2(i)
c    #    /gamm(dble(i+1))*ff4(i)*w**(dble(i))*et1
c       write(10,*)temp1,ttest
c       ttest=ttest+gamm(dble(i+k+1)-b)/gamm(one-b)*gamm(c-b+dble(i+k))
c    #    /gamm(c-b)/gamm(a)/gamm(b)*gamm(-eps-dble(i))*(-1)**(i+k)
c    #    *w**(eps+dble(i))/gamm(dble(i+k+1))*cos(pi*(eps-dble(k)))
c    #    +gamm(a+dble(i))/gamm(a)*gamm(a-c+dble(i+1))/gamm(a-c+one)
c    #    /gamm(c-a)/gamm(c-b)*gamm(eps-dble(k+i))*(-w)**i
c    #    /gamm(dble(i+1))
c       write(10,*)temp+temp1,ttest
  70  continue

      poch1(0)=one
      poch2(0)=one
      do 71 i=1,k-1
        poch1(i)=(c-b+dble(i-1))*poch1(i-1)
        poch2(i)=(dble(i)-b)*poch2(i-1)
  71  continue

      x1=a
      coeff1=one
  72  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 72
      endif

      x2=b
      coeff2=one
  73  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 73
      endif

      temp2=zero
      do 80 i=0,k-1
        temp2=temp2+coeff1*coeff2/gamm(x1)/gamm(x2)*poch1(i)*poch2(i)
     #  *gamm(dble(k-i)-eps)/gamm(dble(i+1))*w**(eps+dble(i-k))*(-1)**i
  80  continue

c     term1=zero
c     do 81 i=0,k-1
c       term1=term1+gamm(dble(i+1)-b)/gamm(a)*gamm(c-b+dble(i))/gamm(b)
c    #        /gamm(one-b)/gamm(c-b)*(-1)**i/gamm(dble(i+1))
c    #        *gamm(dble(k-i)-eps)*w**(eps+dble(i-k))
c 81  continue
c     write(10,*)temp2,term1

      re=gamm(c)*(one-w)**a*(temp+temp1+temp2*cos(pi*(eps-dble(k))))
c     write(10,*)re,(ttest+term1*cos(pi*(eps-dble(k))))
c    #             *gamm(c)*(one-w)**a

c  calculate the imaginary part

      im=temp2*sin(pi*(eps-dble(k)))

      poch1(0)=one
      poch2(0)=one
      do 90 i=1,k
        poch1(0)=(c-b+dble(i-1))*poch1(0)
        poch2(0)=(dble(i)-b)*poch2(0)
  90  continue
      do 91 i=1,n
        poch1(i)=(c-b+dble(k+i-1))*poch1(i-1)
        poch2(i)=(dble(k+i)-b)*poch2(i-1)
  91  continue

      temp=zero
      do 92 i=0,n
        temp=temp+poch1(i)/gamm(dble(k+i+1))*ff3(i)
     #       *w**(eps+dble(i))*poch2(i)
  92  continue

      x1=a
      coeff1=one
  93  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 93
      endif

      x2=b
      coeff2=one
  94  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 94
      endif

      if (dabs(eps).lt..1d0) then
        temp1=one
        i=1
  95    temp2=temp1+(-1)**i*(pi*eps)**(i+i)/gamm(dble(i+i+2))
        error=(temp2-temp1)/temp2
        if (dabs(error).lt.machep) go to 96
        i=i+1
        temp1=temp2
        go to 95
      else
        temp2=sin(pi*eps)/pi/eps
      endif
  96  continue
c     write(10,*)temp2,sin(pi*eps)/pi/eps

      im=im+temp*coeff1*coeff2/gamm(x1)/gamm(x2)*pi*temp2
      im=-im

      return
      end

c**********************************************************************
c
c  subroutine name    - fix6
c
c  computation
c  performed          - calculates the hypergeometric function for z
c                       greater than 2 when a-b is near an integer.
c
c  usage              - call fix6(a,b,c,n,k,re,im,w,machep,eps,pi)
c
c  arguments    a,b,c - parameters of the hypergeometric function.
c
c                  n  - the upper limit of the finite series expansion
c                       of the hypergeometric function.
c
c                  k  - equals the nearest integer of a-b.
c
c               re,im - computed values for the real and imaginary parts
c                       of the hypergeometric function.
c
c                  w  - transformed independent variable.
c
c              machep - equals machine epsilon.
c
c                eps  - equals a-b-k.
c
c                 pi  - equals 3.1415... to machine accuracy.
c
c  precision          - double
c
c  language           - fortran
c
c***********************************************************************

      subroutine fix6(a,b,c,n,k,re,im,w,machep,eps,pi)

      DOUBLE PRECISION  zero,one,two,four,eighth,seven,eight,sxteen
      parameter (zero=0.d0,one=1.d0,two=2.d0,four=4.d0,eighth=1.d0/8.d0,
     #           seven=7.d0,eight=8.d0,sxteen=16.d0,nmax=100)
      DOUBLE PRECISION   a,b,c,w,re,im,gamm,temp,temp2,g1(0:nmax),
     #      g2(0:nmax),g3(0:nmax),g4(0:nmax),g5(0:nmax),x,x1,x2,x3,x4,
     #     psi,rn,t1(0:80),t2(0:80),t3(0:80),t4(0:80),test,machep,pi,
     #         f1(0:80),f2(0:80),f3(0:80),f4(0:80),ff3(0:nmax),eps,
     #         ff4(0:nmax),coeff1,coeff2,c1(0:80),c2(0:80),c3(0:80),
     #         c4(0:80),sum,term1,term2,term3,term4,term5,et1,et2,error,
     #         term6,temp1,coeff,coeff3,coeff4,fff1(0:nmax),ff1(0:nmax),
     #         fff2(0:nmax),ff2(0:nmax),poch1(0:nmax),poch2(0:nmax),e1

      integer  flag

      x3=zero
      call cheb(c3,55,2)
      t3(0)=one
      t3(1)=two*(x3+eps)
      f3(0)=zero
      f3(1)=two
      g3(0)=c3(1)*f3(1)

      x4=zero
      call cheb(c4,55,2)
      t4(0)=one
      t4(1)=two*(x4-eps)
      f4(0)=zero
      f4(1)=-two
      g4(0)=c4(1)*f4(1)

      do 7 i=2,55
        t3(i)=four*(x3+eps)*t3(i-1)-t3(i-2)
        t4(i)=four*(x4-eps)*t4(i-1)-t4(i-2)
        f3(i)=four*t3(i-1)+four*x3*f3(i-1)-f3(i-2)
        f4(i)=-four*t4(i-1)+four*x4*f4(i-1)-f4(i-2)
        g3(0)=g3(0)+c3(i)*f3(i)
        g4(0)=g4(0)+c4(i)*f4(i)
  7   continue

      g4(0)=-g4(0)
      do 10 i=-k,-1
        g4(0)=(g4(0)+one/gamm(dble(k+i+2)))/(dble(k+i+1)+eps)
  10  continue

      test=-eps*dlog(w)
      temp=-dlog(w)
         if (dabs(test).ge.eighth) then
           temp=(exp(test)-one)/eps
         else
           i=1
  20       rn=(eps**(i)*(-dlog(w))**(i+1))/gamm(dble(i+2))
           if (dabs(rn).lt.machep) go to 30
           temp=temp+rn
           i=i+1
           go to 20
         endif
  30     g5(0)=temp*w**a

c     write(10,*)g3(0),gamm(eps)-one/eps
c     write(10,*)g4(0),(-1)**k*gamm(-eps-k)+one/eps/gamm(dble(k+1))
c     write(10,*)g5(0),w**(a-eps)/eps-w**a/eps

      do 60 i=1,n
        g3(i)=(g3(i-1)+one/gamm(dble(i+1)))/(dble(i)-eps)
        g4(i)=(g4(i-1)+one/gamm(dble(k+i+1)))/(dble(k+i)+eps)
        g5(i)=w*g5(i-1)
  60  continue

      do 65 i=0,n
        ff3(i)=eps*g3(i)+one/gamm(dble(i+1))
        ff4(i)=eps*g4(i)-one/gamm(dble(k+i+1))
  65  continue

c  calculate the extra terms

      x=b-one
      sum=zero
      coeff=one
      flag=0
  61  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 61
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 610    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 610
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 61
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 611 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 611    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 612 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 612    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 613 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 613    continue
      endif

      if (flag.eq.0) then
        x1=b
        coeff1=one
 614    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 614
        endif
        x2=b+eps
        coeff2=one
 615    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 615
        endif
        temp=sum+coeff*temp
        et1=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 616    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 616
        endif
        x2=x+one+eps
        coeff2=one
 617    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 617
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et1=sum+coeff*temp
      endif
      et1=-et1
c     write(10,*)et1,(one/gamm(a-dble(k)-eps)-one/gamm(a-dble(k)))
c    #                                                  /eps

      x=c-a+k-one
      sum=zero
      coeff=one
      flag=0
  62  if (x.gt.one) then
        sum=sum+coeff*gamm(x+eps)
        coeff=coeff*x
        x=x-one
        go to 62
      elseif (x.lt.zero) then
        x1=x+eps+two
        coeff1=one
 620    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 620
        endif
        sum=sum+coeff*coeff1/gamm(x1)
        coeff=coeff*(x+one)
        x=x+one
        flag=1
        go to 62
      endif

      if ((x .ge. .25d0).and.(x .le. .75d0)) then
        call cheb(c1,41,1)
        t1(0)=one
        t1(1)=two*(x+eps)-one
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 621 i=2,41
          t1(i)=(four*(x+eps)-two)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-two)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 621    continue
      elseif ((x .ge. 0.d0).and.(x .lt. .25d0)) then
        call cheb(c1,55,2)
        t1(0)=one
        t1(1)=two*(x+eps)
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 622 i=2,55
          t1(i)=four*(x+eps)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+four*x*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 622    continue
      elseif ((x .gt. .75d0).and.(x .le. 1.d0)) then
        call cheb(c1,34,3)
        t1(0)=one
        t1(1)=two*(x+eps)-two
        f1(0)=zero
        f1(1)=two
        temp=c1(1)*f1(1)
        do 623 i=2,34
          t1(i)=(four*(x+eps)-four)*t1(i-1)-t1(i-2)
          f1(i)=four*t1(i-1)+(four*x-four)*f1(i-1)-f1(i-2)
          temp=temp+c1(i)*f1(i)
 623    continue
      endif

      if (flag.eq.0) then
        x1=c-a+dble(k)
        coeff1=one
 624    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 624
        endif
        x2=c-a+dble(k)+eps
        coeff2=one
 625    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 625
        endif
        temp=sum+coeff*temp
        et2=-temp*coeff1*coeff2/gamm(x1)/gamm(x2)
      elseif (flag.eq.one) then
        x1=x+one
        coeff1=one
 626    if (x1.lt.one) then
          coeff1=x1*coeff1
          x1=x1+one
          go to 626
        endif
        x2=x+one+eps
        coeff2=one
 627    if (x2.lt.one) then
          coeff2=x2*coeff2
          x2=x2+one
          go to 627
        endif
        coeff=-coeff*coeff1*coeff2/gamm(x1)/gamm(x2)
        et2=sum+coeff*temp
      endif
      et2=-et2
c     write(10,*)et2,(one/gamm(c-b-eps)-one/gamm(c-b))/eps
c
      fff1(0)=one
      fff2(0)=one
      ff2(0)=one
      do 685 i=1,k
        fff1(0)=(b+dble(i-1))*fff1(0)
        fff2(0)=(b-c+eps+dble(i))*fff2(0)
        ff2(0)=(b-c+dble(i))*ff2(0)
 685  continue

      ff1(0)=one
      do 686 i=1,n
        fff1(i)=(b+dble(k+i-1))*fff1(i-1)
        fff2(i)=(b-c+eps+dble(k+i))*fff2(i-1)
        ff1(i)=(a+dble(i-1))*ff1(i-1)
        ff2(i)=(b-c+dble(k+i))*ff2(i-1)
 686  continue

      x1=a
      coeff1=one
 687  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 687
      endif

      x2=a-dble(k)
      coeff2=one
 688  if (x2.lt.one) then
        coeff2=x2*coeff2
        x2=x2+one
        go to 688
      endif

      x3=c-b-eps
      coeff3=one
 689  if (x3.lt.one) then
        coeff3=x3*coeff3
        x3=x3+one
        go to 689
      endif

      x4=c-b
      coeff4=one
 690  if (x4.lt.one) then
        coeff4=x4*coeff4
        x4=x4+one
        go to 690
      endif

      do 691 i=0,n
        fff1(i)=fff1(i)*coeff1/gamm(x1)
        ff1(i)=ff1(i)*coeff2/gamm(x2)
        fff2(i)=fff2(i)*coeff3/gamm(x3)
        ff2(i)=ff2(i)*coeff4/gamm(x4)
c       write(10,*)'fff1=',fff1(i),gamm(b+dble(i+k))/gamm(a)/gamm(b)
c       write(10,*)'ff1=',ff1(i),gamm(a+dble(i))/gamm(a)/gamm(a-dble(k))
c       write(10,*)'fff2=',fff2(i),gamm(a-c+dble(i+1))/gamm(c-b-eps)
c    #                               /gamm(one-c+b+eps)
c       write(10,*)'ff2=',ff2(i),gamm(b-c+dble(i+k+1))/gamm(c-b)
c    #                               /gamm(one-c+b)
 691  continue

c   calculate  g1

      g1(0)=zero
      poch1(0)=one
      do 697 i=1,k
        g1(0)=g1(0)*(a-eps+dble(i-k-1))-poch1(0)
        poch1(0)=poch1(0)*(a+dble(i-k-1))
 697  continue
      do 698 i=1,n
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        g1(i)=g1(i-1)*(a-eps+dble(i-1))-poch1(i-1)
 698  continue

      x1=a
      coeff1=one
 699  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 699
      endif
      do 700 i=0,n
        g1(i)=g1(i)*coeff1/gamm(x1)
c       write(10,*)'g1=',g1(i),(fff1(i)-ff1(i))/eps
 700  continue

c   calculate  g2

      g2(0)=zero
      poch2(0)=one
      do 701 i=1,k
        g2(0)=g2(0)*(b-c+eps+dble(i))+poch2(0)
        poch2(0)=poch2(0)*(b-c+dble(i))
 701  continue
      do 702 i=1,n
        poch2(i)=(b-c+dble(i+k))*poch2(i-1)
        g2(i)=g2(i-1)*(b-c+eps+dble(i+k))+poch2(i-1)
 702  continue

      x1=c-b
      coeff1=one
 703  if (x1.lt.one) then
        coeff1=x1*coeff1
        x1=x1+one
        go to 703
      endif

      poch2(0)=one
      do 704 i=1,k
        poch2(0)=(b-c+eps+dble(i))*poch2(0)
 704  continue
      do 705 i=1,n
        poch2(i)=(b-c+eps+dble(i+k))*poch2(i-1)
 705  continue

      do 706 i=0,n
        g2(i)=et2*poch2(i)+g2(i)*coeff1/gamm(x1)
c       write(10,*)'g2=',g2(i),(fff2(i)-ff2(i))/eps
 706  continue

c  calculate  e1

      e1=zero
      if (dabs(eps).lt..1d0) then
        i=1
 707    rn=(-1)**i*pi**(i+i)*eps**(i+i-1)/gamm(dble(i+i+1))
        if (dabs(rn).lt.machep) go to 708
        e1=e1+rn
        i=i+1
        go to 707
      else
        e1=(cos(pi*eps)-one)/eps
      endif
 708  continue
c     write(10,*)'e1=',e1,(cos(pi*eps)-one)/eps

      poch1(0)=one
      poch2(0)=one
      do 709 i=1,n
        poch1(i)=(a+dble(i-1))*poch1(i-1)
        poch2(i)=(a-c+dble(i))*poch2(i-1)
 709  continue

c  put everything back together again

      temp=zero
      temp1=zero
      temp2=zero
      do 70 i=0,n
        term1=-g1(i)/gamm(dble(i+1))*fff2(i)*ff4(i)
     #                     *w**(a+dble(i))*cos(pi*eps)
        term2=fff1(i)/gamm(dble(i+1))*g2(i)*ff4(i)
     #                     *w**(a+dble(i))*cos(pi*eps)
        term3=-fff1(i)*ff2(i)*g3(i)*ff4(i)
     #                     *w**(a+dble(i))*cos(pi*eps)
        term4=fff1(i)*ff2(i)*ff3(i)*g4(i)
     #                     *w**(a+dble(i))*cos(pi*eps)
        term5=fff1(i)/gamm(dble(k+i+1))*ff2(i)*ff3(i)
     #                     *g5(i)*cos(pi*eps)
        term6=-fff1(i)/gamm(dble(k+i+1))*ff2(i)*ff3(i)
     #                     *w**(a-eps+dble(i))*e1
        temp=temp+term1+term2+term3+term4+term5+term6
        temp1=temp1+poch1(i)/gamm(dble(i+1))*poch2(i)*ff4(i)
     #                                      *w**(a+dble(i))
        temp2=temp2+poch1(i)/gamm(dble(i+1))*fff2(i)*ff4(i)
     #                                      *w**(a+dble(i))
  70  continue

      x1=b
      coeff1=one
  71  if (x1.lt.one) then
        coeff1=coeff1*x1
        x1=x1+one
        go to 71
      endif

      x2=c-a
      coeff2=one
  72  if (x2.lt.one) then
        coeff2=coeff2*x2
        x2=x2+one
        go to 72
      endif

      term1=temp*gamm(c)*cos(pi*b)*(-1)**k
      term2=-temp1*gamm(c)*sin(pi*b)*coeff1/gamm(x1)*coeff2/gamm(x2)
      term3=temp2*gamm(c)*cos(pi*b)*cos(pi*eps)*(-1)**k*et1
      term4=temp*gamm(c)*sin(pi*b)*(-1)**k
      term5=temp1*gamm(c)*cos(pi*b)*coeff1/gamm(x1)*coeff2/gamm(x2)
      term6=term6*gamm(c)*sin(pi*b)*cos(pi*eps)*(-1)**k*et1

      if (dabs(eps).lt..1d0) then
        temp1=one
        i=1
  80    temp2=temp1+(-1)**i*(pi*eps)**(i+i)/gamm(dble(i+i+2))
        error=(temp2-temp1)/temp2
        if (dabs(error).lt.machep) go to 81
        i=i+1
        temp1=temp2
        go to 80
      else
        temp2=sin(pi*eps)/pi/eps
      endif
  81  continue
c     write(10,*)temp2,sin(pi*eps)/(pi*eps)

      term2=term2*pi*temp2
      term5=term5*pi*temp2

      re=term1+term2+term3
      im=term4+term5+term6

c  calculate the finite series contribution

      poch1(0)=one
      poch2(0)=one
      do 82 i=1,n
        poch1(i)=(b+dble(i-1))*poch1(i-1)
        poch2(i)=(b-c+dble(i))*poch2(i-1)
  82  continue

      temp=zero
      do 83 i=0,k-1
        temp=temp+poch1(i)*poch2(i)/gamm(dble(i+1))*gamm(eps+dble(k-i))
     #                              *(-1)**i*w**(b+dble(i))
  83  continue

      x1=a
      coeff1=one
  84  if (x1.lt.one) then
        coeff1=coeff1*x1
        x1=x1+one
        go to 84
      endif

      x2=c-b
      coeff2=one
  85  if (x2.lt.one) then
        coeff2=coeff2*x2
        x2=x2+one
        go to 85
      endif

      temp=temp*gamm(c)*coeff1/gamm(x1)*coeff2/gamm(x2)

      re=re+temp*cos(pi*b)
      im=im+temp*sin(pi*b)

      return
      end

c**********************************************************************
*
*   subroutine name     - geteps
*
*   computation
*   performed           - compute the smallest number machep such that
*                           machep+1 is not equal to 1 in the finite
*                           precision arithmetic used by the computer.
*
*   usage               - call geteps(machep,neps)
*
*   argument     machep - double precision (output).  the smallest
*                           number such that machep+1 is not equal to 1
*                           in the finite precision arithmetic used by
*                           the computer.
*                  neps - integer (output).  machine epsilon is machep =
*                           (1/2)**neps
*
*   precision           - double
*
*   language            - fortran 77
*
************************************************************************
*
      subroutine geteps(machep,neps)
*
      DOUBLE PRECISION      machep,one,two,temp
      integer            neps
      parameter (one = 1.0d0, two = 2.0d0)
      machep = one
      neps = 0
100   continue
      machep = machep/two
      neps = neps+1
      temp = machep+one
      if (temp .ne. one) go to 100
*
      machep = two*machep
      neps = neps-1
*
      return
      end
*
c***********************************************************************

      subroutine binomc

c       a
c     (   ) = binom(a*(a+1)/2+b+1)
c       b

      double precision   binom,one
      common /bcoeff/binom(5151)

      maxnll=100

      if (maxnll .lt. 0) go to 300
      if (maxnll .gt. 100) go to 300
      one = 1.0d0
      binom(1) = one
      if (maxnll .eq. 0) go to 300
      binom(2) = one
      binom(3) = one
      if (maxnll .eq. 1) go to 300
      ij = 4
      imax = maxnll+1
      do 200 i = 3,imax
         ii = ((i-1)*(i-2))/2
         binom(ij) = one
         ij = ij+1
         jmax = i-1
         do 100 j = 2,jmax
            binom(ij) = binom(ii+j-1)+binom(ii+j)
            ij = ij+1
100      continue
         binom(ij) = one
         ij = ij+1
200   continue
c
300   continue
c
      return
      end

c***********************************************************************
c
c  subroutine name    - cheb
c
c  computation
c  performed          - tabulates the tchebychev coefficients which
c                       were computed by the program 'tcheb2'.  the
c                       three sets of coefficients correspond to
c                       the three gamma function expansions shown in
c                       equations (4.35),(4.36), and (4.37). see
c                       'tcheb2' for additional documentation.
c
c  usage              - call cheb(c,n,flag)
c
c  arguments       c  - the array (output) which contains the
c                       tchebychev coefficients.
c
c                  n  - the dimension (input) of the array 'c'.
c
c                flag - the parameter (input) which tells the sub-
c                       routine which tchebychev coefficients to
c                       return to the caller.
c
c  precision          - double (although the coefficients are
c                               accurate to quadruple)
c
c  language           - fortran 77
c
c***********************************************************************

      subroutine cheb(c,n,flag)

      DOUBLE PRECISION  c(0:n)
      integer  flag

      if (flag.eq.1) go to 100
      if (flag.eq.2) go to 200
      if (flag.eq.3) go to 300

c  tchebychev expansion coefficients for the range, 0<x<1

 100  c(0) =  0.94178559779549466571096003120435196d+00
      c(1) =  0.44153813248410067571913157711414607d-02
      c(2) =  0.56850436815993633786326645888162378d-01
      c(3) = -0.42198353964185605010125001866024699d-02
      c(4) =  0.13268081812124602205840067963889683d-02
      c(5) = -0.18930245297988804325239470239464680d-03
      c(6) =  0.36069253274412452565780822094442805d-04
      c(7) = -0.60567619044608642184855483216922771d-05
      c(8) =  0.10558295463022833447318234541645507d-05
      c(9) = -0.18119673655423840482918555144273961d-06
      c(10)=  0.31177249647153222777902517006137963d-07
      c(11)= -0.53542196390196871408740949118221475d-08
      c(12)=  0.91932755198595889468877475468573503d-09
      c(13)= -0.15779412802883397617671187106425584d-09
      c(14)=  0.27079806229349545432695717700017206d-10
      c(15)= -0.46468186538257301439531283506784063d-11
      c(16)=  0.79733501920074196555512936759234830d-12
      c(17)= -0.13680782098309160264738694164685656d-12
      c(18)=  0.23473194865638006534799539031857605d-13
      c(19)= -0.40274326149490669507857892267787757d-14
      c(20)=  0.69100517473721009958174457696435176d-15
      c(21)= -0.11855845002219929396593062972684083d-15
      c(22)=  0.20341485424963760969383490105975402d-16
      c(23)= -0.34900543417173691101844936408331408d-17
      c(24)=  0.59879938564842634972645168624438135d-18
      c(25)= -0.10273780578716378747008169519685451d-18
      c(26)=  0.17627028160574041125936108594612916d-19
      c(27)= -0.30243206536626379817809691872233988d-20
      c(28)=  0.51889146600668142375785699199940389d-21
      c(29)= -0.89027708392150216484577040964212789d-22
      c(30)=  0.15274740724470977041487116294681806d-22
      c(31)= -0.26207312865170684216151526387496724d-23
      c(32)=  0.44964644619824783627762340991300087d-24
      c(33)= -0.77147147879836211531329396406348717d-25
      c(34)=  0.13236365808260955301316348853544449d-25
      c(35)= -0.22709797413377406198008958539204735d-26
      c(36)=  0.38966913277073699893252807432563276d-27
      c(37)= -0.66795989154793901466615113245736539d-28
      c(38)=  0.11456694360946249087722449327564468d-28
      c(39)= -0.20956088513945987438866120550893160d-29
      c(40)=  0.34345153487326051089311279207743562d-30
      c(41)= -0.74448389617685196161619686887550341d-31
      return

c  tchebychev expansion coefficients for the range,  -.5<x<.5

 200  c(0) =  0.11528686913857579339872890819003657d+01
      c(1) = -0.39836641427188668813550502856567435d+00
      c(2) =  0.16381491849746834445969671065563396d+00
      c(3) = -0.41349972584595838242416447164595642d-01
      c(4) =  0.11739888104509743948748485834561229d-01
      c(5) = -0.31509159742825717845846783104528302d-02
      c(6) =  0.85084809366682540330028115184077086d-03
      c(7) = -0.22845443192182297253614554810213881d-03
      c(8) =  0.61296656896858907270916323759970391d-04
      c(9) = -0.16433766723011959082591541534833589d-04
      c(10)=  0.44046701847148520660258125028242579d-05
      c(11)= -0.11803851479587223345492859134791582d-05
      c(12)=  0.31630339312403588488305625683201151d-06
      c(13)= -0.84755796666686117564957022251013564d-07
      c(14)=  0.22710572677209079780536954678987573d-07
      c(15)= -0.60853209609268373214751556259951644d-08
      c(16)=  0.16305620921375867864482570008163625d-08
      c(17)= -0.43690846345047718022878883179027790d-09
      c(18)=  0.11706935476739890379554689241357534d-09
      c(19)= -0.31368649843198552351255033209421610d-10
      c(20)=  0.84052057618382692960217222664957228d-11
      c(21)= -0.22521682699590609081199019088965996d-11
      c(22)=  0.60346669123807723976181127096882828d-12
      c(23)= -0.16169841538137032176079290114309245d-12
      c(24)=  0.43326960175123609635570088625382667d-13
      c(25)= -0.11609424034675431553315176322024985d-13
      c(26)=  0.31107358004300087572452155428660087d-14
      c(27)= -0.83351914632193111475558815401948979d-15
      c(28)=  0.22334078222557889355389486422061460d-15
      c(29)= -0.59843982246058550382747881611851515d-16
      c(30)=  0.16035146716190080240936859943115090d-16
      c(31)= -0.42966046133076898235808019603294715d-17
      c(32)=  0.11512717363557431988678458870224873d-17
      c(33)= -0.30848233202835882015258583966299712d-18
      c(34)=  0.82657591746540727258216017499064442d-19
      c(35)= -0.22148034956862123422799663231945171d-19
      c(36)=  0.59345480806145642339133686333296721d-20
      c(37)= -0.15901573656881585725893714030807897d-20
      c(38)=  0.42608138203898096080539369435375448d-21
      c(39)= -0.11416816226321087557458906349840213d-21
      c(40)=  0.30591266842950015571055286508657438d-22
      c(41)= -0.81969053674548061989664444282339330d-23
      c(42)=  0.21963543471485197662543467891802004d-23
      c(43)= -0.58851140572211577956963471197095354d-24
      c(44)=  0.15769121438531798083082131134888596d-24
      c(45)= -0.42253211944581570323425035302537635d-25
      c(46)=  0.11321706791574145306428072576766804d-25
      c(47)= -0.30335842761477973373797446515125892d-26
      c(48)=  0.81281383350578045680446098123885346d-27
      c(49)= -0.21782407988772728568103833180457024d-27
      c(50)=  0.58395544064782062129754390403734767d-28
      c(51)= -0.15729062977489325257494410942884130d-28
      c(52)=  0.42390612257722955199550993363196147d-29
      c(53)= -0.11242203351086692027388616387423238d-29
      c(54)=  0.27892280419588143241883200553486195d-30
      c(55)= -0.75766427928255356179910217971637866d-31
      return

c  tchebychev expansion coefficients for the range,  .5<x<1.5

 300  c(0) =  0.10532770878177862619534128247576828d+01
      c(1) =  0.21902166104535936497306369004840667d+00
      c(2) =  0.53885821783347712865216341722976574d-01
      c(3) =  0.25387290658986838596948519579519148d-02
      c(4) =  0.61466596479014144199820446583715941d-03
      c(5) = -0.32319247384294465724865638122474435d-05
      c(6) =  0.60054921157267140200751871810266970d-05
      c(7) = -0.41824428090189489334617924547407754d-06
      c(8) =  0.74607235650174366232051332482639985d-07
      c(9) = -0.84349526185192483560074198183789434d-08
      c(10)=  0.11322169721817117406057072389666464d-08
      c(11)= -0.14175349900034682206860980369914924d-09
      c(12)=  0.18156967683771854495445069753509525d-10
      c(13)= -0.23052163748763990586386231147733255d-11
      c(14)=  0.29327030584105892891631030300077869d-12
      c(15)= -0.37268590170679729030689484336505900d-13
      c(16)=  0.47360432581610222494078892575939043d-14
      c(17)= -0.60172423075766780010690060490450222d-15
      c(18)=  0.76443979970650480527157880770622904d-16
      c(19)= -0.97108892590783757664936380167684001d-17
      c(20)=  0.12335488659810502174628042595177563d-17
      c(21)= -0.15668997427423797214874298423999374d-18
      c(22)=  0.19902969432180950952170748993213290d-19
      c(23)= -0.25280701093316992983208535829903356d-20
      c(24)=  0.32111217127088658654008440525466587d-21
      c(25)= -0.40787027055654288157193053732139852d-22
      c(26)=  0.51806681115442807351458062924762066d-23
      c(27)= -0.65803415226414646040514708695329147d-24
      c(28)=  0.83581632724068042390791744946381128d-25
      c(29)= -0.10616267321620223331012310816058461d-25
      c(30)=  0.13484159784261929973156667845986312d-26
      c(31)= -0.17130640476670792317750095910458264d-27
      c(32)=  0.21720215147689502411187819143753676d-28
      c(33)= -0.27633054946463729557612727034555572d-29
      c(34)=  0.26664265210535867308016959008022142d-30
      return
      end

CCCCCC INIZIO SUBROUTINE   OMD   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C>>>>>>>>>>>>>>>>>>>>  variation of subroutine mds.f  <<<<<<<<<<<<<<<<<<<<<<<<<C


      SUBROUTINE omd(NSTART,NRUNS,ITMAX,INITDES,N0,X0,Y0,
     *cBL,cCOLS,N,cX,cNM,cP,cSIGMA2,cNF,MNF,cJFAC,cCUT,
     *MBEST,NTOP,TOPD,TOPDES,flag)

      INTEGER NSTART,NRUNS,ITMAX,INITDES,N0,NN,MBEST(NSTART,NRUNS)
      INTEGER cBL,cCOLS,N,cNM,cNF(cNM),MNF,cJFAC(cNM,MNF),cCUT
      INTEGER NTOP,TOPDES(NTOP,NRUNS),flag,OUT,ef
      DOUBLE PRECISION cP(cNM),cSIGMA2(cNM),EPS
      DOUBLE PRECISION X0(N0,cBL+cCOLS),Y0(N0),cX(N,cBL+cCOLS)
      DOUBLE PRECISION TOPD(NTOP)

      PARAMETER (MAXM=100,MAXCOL=100,MAXN0=40,MAXN1=32,MAXN=256,
     &MXSTRT=52360)

      DOUBLE PRECISION BETA(MAXM,MAXCOL),G(MAXM,MAXCOL,MAXCOL)
      DOUBLE PRECISION P(MAXM),X(MAXN,MAXCOL),SIGMA2(MAXM)
      DOUBLE PRECISION DTOP(MXSTRT),XBEST(MAXN1)
      DOUBLE PRECISION D,RCOND,DET(2),DBEST,DSTART
      DOUBLE PRECISION A(MAXN0,MAXCOL),AA(MAXCOL,MAXCOL),Z(MAXCOL)
      DOUBLE PRECISION B(MAXCOL)
      INTEGER IM,NM,I,J,BL,NF(MAXM),II,TK,TOTO,M,CUT
      INTEGER JFAC(MAXM,20),MULT(40),BEST(MAXN1),ROWS(MAXN1),N1,IJ
      INTEGER COLS,INFO,BESTI
      INTEGER TOPROW(MXSTRT,MAXN1),ITOP(MXSTRT),ND,IZ,JJ
      LOGICAL PART,DESIN

      COMMON BETA,G,P,X,SIGMA2,NF,JFAC,BL,CUT,COLS,NM

      OUT = 1
C     OPEN(OUT,FILE="MDPrint.out")

     
      NM = cNM
      BL = cBL
      COLS = cCOLS
      CUT = cCUT
      	    eps=0.0D-5
      	    ef=0    
      	      
      	    do 8 i = 1, N
        do 8 j = 1, (BL+COLS)                                                                                                              
       X(i,j) = cX(i,j)
8       continue
      do 5 i = 1, NM
        NF(i) = cNF(i)
        SIGMA2(i) = cSIGMA2(i)
        P(i) = cP(i)
        do 6 j = 1, MNF
         JFAC(i,j) = cJFAC(i,j)
6       continue
5     continue

      BESTI=0
      	
      do 17 i = 1, MXSTRT
       DTOP(i) = 0.0D0
17    	  continue

      DO 190 IM=1,NM

C     AUGMENT CANDIDATE MATRIX WITH INTERACTION COLUMNSC
      TOTO=COLS+BL
      DO 920 M=2,CUT
         CALL INITIA2(MULT,M)
         PART=.FALSE.
 925     IF (.NOT. PART) THEN
           TOTO=TOTO+1
           DO 930 I=1,N
      			X(I,TOTO)=X(I,MULT(1)+BL)*X(I,MULT(2)+BL)
 930       continue     
           DO II=3,M
             DO I=1,N
                X(I,TOTO)=X(I,TOTO)*X(I,MULT(II)+BL)
             end do 
           end do
      		  CALL INVREM2(MULT,PART,M,COLS)
           GO TO 925
         ENDIF
 920  CONTINUE


      	      
      	    TK=NF(IM)
      DO 110 I=1,N0
       A(I,1)=1.0                                                       
       DO 115 J=1,BL                                                    
         A(I,1+J)=X0(I,J)
 115   continue
      	 DO 110 J=1,TK                                                    
         A(I,J+1+BL)=X0(I,JFAC(IM,J)+BL)                                
 110   continue 
      TOTO=TK+1+BL
      


C     AUGMENT WITH INTERACTION COLUMNS
                                                                       
      DO 120 M=2,MIN(CUT,TK)
         CALL INITIA2(MULT,M)
         PART=.FALSE.
 125     IF (.NOT. PART) THEN
           TOTO=TOTO+1
           DO 130 I=1,N0
             A(I,TOTO)=A(I,MULT(1)+1+BL)*A(I,MULT(2)+1+BL)
 130       continue 
      	     DO II=3,M
             DO I=1,N0
               A(I,TOTO)=A(I,TOTO)*A(I,MULT(II)+1+BL)
             end do
           end do  
      	     CALL INVREM2(MULT,PART,M,TK)
           GO TO 125
         ENDIF
 120  CONTINUE



C      FORM X-PRIME-X MATRIX

      NN=MIN(N0,TOTO)
143   	DO I=1,NN
      DO J=I,NN
        AA(I,J)=0.0
        DO 145 M=1,N0
          AA(I,J)=AA(I,J)+A(M,I)*A(M,J)
 145    continue
        AA(J,I)=AA(I,J)
      end do 
      end do 

      	    DO I=1,NN
      B(I)=0.0
      		  DO M=1,N0
      			B(I)=B(I)+A(M,I)*Y0(M)
      		  end do
      end do 
      
      	    CALL DPOCO(AA,MAXCOL,NN,RCOND,Z,INFO)

      	    IF (info.ne.0) THEN                                    
      	    IF (ef.eq.1) then  
      			GO TO 190
      	    else
      			NN=NN-1
      			go to 143
      	    ENDIF 
      	    ENDIF
      	                    
      	    CALL DPODI(AA, MAXCOL,NN,DET,11) 
      
      	    IF (DET(2).LT. -3) THEN                                    
      	    IF (ef.eq.1) then  
      			GO TO 190
      	    else
      			NN=NN-1
      			go to 143
      	    ENDIF                                                   
      	    ENDIF
      					
      	
      DO I=1,NN
        BETA(IM,I)=0.0
        DO  J=1,NN
          IF (I .GT. J) AA(I,J)=AA(J,I)
          BETA(IM,I)=BETA(IM,I)+AA(I,J)*B(J)
          G(IM,I,J)=AA(I,J)
      	    end do 
      	    end do 

 190  CONTINUE
      	


      NDTOP=1
      IJ=1
      DO 700 ISTART=1,NSTART

      IF (INITDES .GT. 0) THEN
        CALL RANST(NRUNS,N,BEST,0.0D0)
        FLAG = 100
      ELSE
        do 791 I=1,NRUNS
      	       BEST(I) = MBEST(ISTART,I)
 791    continue  
         FLAG = 200
      ENDIF
      CALL EVAL(NRUNS,BEST,D,NM)



      M=0
      DBEST=D
      IF (DTOP(IJ) .LT. DBEST) THEN
        DESIN=.FALSE.
        DO 207 I=1,NRUNS
      		  XBEST(I)=DBLE(BEST(I))
 207    continue 
        CALL SSORT(XBEST,BEST,NRUNS,2)
        DO 209 J=1, NDTOP-1
          IF ((DABS((DTOP(J)-DBEST)/DTOP(J)) .LT. 0.00001) .AND.
     &    (DESIN .EQV. .FALSE.)) THEN
            IZ=0
            DO 208 I=1,NRUNS
      		       IZ=IZ+IABS(TOPROW(J,I)-BEST(I))
 208           continue
      		  IF (IZ .EQ. 0) DESIN=.TRUE.
          ENDIF
 209    CONTINUE
        IF (DESIN .EQV. .FALSE.) THEN
          DTOP(IJ)=DBEST
            DO 210 I=1,NRUNS
              TOPROW(IJ,I)=BEST(I)
 210        CONTINUE
          NDTOP=NDTOP+1
          CALL FINDMIN(NDTOP,IJ,DTOP,MXSTRT)
        ENDIF
      ENDIF
C      WRITE(*,1201) M,D,(BEST(J), J=1,NRUNS)
      IF (ITMAX .EQ. 0) GO TO 700

C     NOW START EXCHANGE ITERATIONS

 500  CONTINUE
      DSTART=DBEST
      M=M+1
      N1=NRUNS+1
      DO 410 I=1,NRUNS
      	    ROWS(I)=BEST(I)
 410  continue
C     FIRST CYCLE THROUGH THE N POSSIBLE ADDITIONAL POINTS              

      DO 450 I=1,N
        ROWS(NRUNS+1)=I
        CALL EVAL(N1,ROWS,D,NM)
        IF (D .GT. DBEST) THEN                                          
          DBEST=D                                                       
          BEST(NRUNS+1)=I
        ENDIF
 450  CONTINUE
CC      WRITE(OUT,1201) M,DBEST,(BEST(J), J=1,NRUNS+1)

C     THEN CYCLE THROUGH THE (NRUNS+1) POSSIBLE DELETED POINTS          
                                                                       
      N1=NRUNS
      DBEST=DSTART                                                      
      DO 460 I=1,NRUNS+1                                                
        DO 465 J=1,NRUNS
          IF (J .LT. I) ROWS(J)=BEST(J)
          IF (J .GE. I) ROWS(J)=BEST(J+1)                               
 465    CONTINUE                                                        
      CALL EVAL(N1,ROWS,D,NM)
      IF (DTOP(IJ) .LT. D) THEN
        DESIN=.FALSE.
        DO 466 II=1,NRUNS                                                                                         
      	     XBEST(II)=DBLE(ROWS(II))                                    
 466    continue 
      	    CALL SSORT(XBEST,ROWS,NRUNS,2)                                  
        DO 468 JJ=1,NDTOP-1
          IF ((DABS((DTOP(JJ)-D)/DTOP(JJ)) .LT. 0.00001) .AND.
     &    (DESIN .EQV. .FALSE.)) THEN
            IZ=0
            DO 467 II=1,NRUNS
           IZ=IZ+IABS(TOPROW(JJ,II)-ROWS(II))
 467        continue   
      	      IF (IZ .EQ. 0) DESIN=.TRUE.
          ENDIF
 468    CONTINUE
        IF (DESIN .EQV. .FALSE.) THEN
          DTOP(IJ)=D                                                    
            DO 469 II=1,NRUNS
              TOPROW(IJ,II)=ROWS(II)
 469        CONTINUE
          NDTOP=NDTOP+1
          CALL FINDMIN(NDTOP,IJ,DTOP,MXSTRT)
        ENDIF
      ENDIF
      IF (D .GE. DBEST) THEN
          DBEST=D                                                       
          BESTI=I
      ENDIF                                                             
 460  CONTINUE                                                          
      DELTAD=DBEST-DSTART
      DO 475 J=1,NRUNS
        IF (J .GE. BESTI) BEST(J)=BEST(J+1)
 475  CONTINUE                                                          
 
C	WRITE(*,1201) M,DBEST,(BEST(J), J=1,NRUNS)                        
      IF ((DELTAD .GT. EPS).AND. (M .LT. ITMAX)) GO TO 500              

C   ITERATIONS ENDED; CONVERGENCE OR MAX ITERATIONS REACHED             
                                                                       
      IF (DELTAD .LE. EPS) THEN
CC        WRITE(OUT,1202)
C       CALL SVIGN(NRUNS,BEST,BEST)                                     
C       WRITE(OUT,1204)                                                   
C       DO 690 I=1,NRUNS                                                
C         WRITE(OUT,1205) I,BEST(I),(X(BEST(I),J), J=1,COLS+BL)           
C690    CONTINUE
      ENDIF                                                             
CC      IF (M .GE. ITMAX) WRITE(OUT,1203)
C     WRITE(OUT,1101)
 700  CONTINUE
      ND=MIN0(MXSTRT,NDTOP-1)                                                 
      DO 701 I=1,ND
         ITOP(I)=I
 701  	continue                                                        
      CALL SSORT(DTOP,ITOP,ND,-2)                                       
CC      WRITE(OUT,1209)
C      WRITE(*,1206) ND
C      WRITE(*,1209)
C      WRITE(*,1210)
      DO 710 J=1,ND
C        WRITE(*,1201) J,DTOP(J),(TOPROW(ITOP(J),K), K=1,NRUNS)
 710  CONTINUE
ccccccccccccc
      NTOP = MIN0(NTOP,ND)
      DO  J = 1,NTOP
        TOPD(J) = DTOP(J)
        DO  K=1,NRUNS
           TOPDES(J,K) = TOPROW(ITOP(J),K)                                  	
        end do
      end do 

C800  FORMAT(7X,' FORTRAN PROGRAM MD: BAYESIAN DESIGN OF EXPERIMENTS',/,                
C     &3X,'FOLLOWUP DESIGN / WYNN EXCHANGE / RANDOM START',/,            
C     &7X,'WRITTEN BY DAN MEYER',/,7X,'ALL RIGHTS RESERVED',/)
C1000 FORMAT(2X,'          NO OF    NO OF  MAX ORDER',
C     &     /,2X,'  N0     FACTORS   BLOCKS INTERACTION  ',
C     &'  GAMMA(MAIN)  GAMMA(INT)  NMODELS')                             
C1001 FORMAT(1X,I6,I8,I9,I10,F15.3,F12.3,I12,//)                        
C1002 FORMAT(1X,'NO OF       NO OF   MAX          NO OF RANDOM',/,
C     &       1X,'CANDIDATES  RUNS    ITERATIONS   STARTS')
C1003 FORMAT(1X,I5,I9,I9,I12,//)                                        
C1004 FORMAT(2X,'MODEL',8X,'PROB',7X,'SIGSQ',3X,'SIZE',3X,'FACTORS')    
C1005 FORMAT(1X,I6,F12.5,F12.4,I7,3X,12(I4))
C1101 FORMAT(1X,100('-'))
C1102 FORMAT(1X,'CANDIDATE RUNS',/,1X,'--------------')
C1103 FORMAT(1X,I3,2X,12(F5.2,1X))
C1104 FORMAT(1X,I3,2X,I5,F8.5,5X,10(I3,1X))
C1105 FORMAT('1MODEL  SIZE   PROB    FACTORS')
C1006 FORMAT(/,' PROGRAM DONE')
C 1199 FORMAT(/,1X,'RANDOM START NUMBER:',I3,/)
C 1200 FORMAT(//,5X,'ITERATION    D',6X,'DESIGN(ROWS)',/,
C     &5X,9('-'),2X,5('-'),4X,98('-'))
C 1203 FORMAT(//,5X,'*** MAX ITERATIONS REACHED ***')
C 1204 FORMAT(1X,'RUN  CAND  FACTOR LEVELS',/,1X,35('-'))
C 1205 FORMAT(1X,I3,2X,I3,2X,12(F5.2,1X))
C 1201 FORMAT(5X,I6,F13.4,2X,24(I4),/,25X,24(I4))
C1202 FORMAT(//,5X,'*** CONVERGENCE ***',//,5X,'DESIGN',/,5X,6('-'),/)
C 1202 FORMAT(//,5X,'*** CONVERGENCE ***',//)
C 1206 FORMAT(1X,'*  THE ',I4,' BEST DESIGNS  *')
C 1207 FORMAT(1X,//,1X,'DESIGN ',I3,' D= ',F13.4,/,
C     &       1X,'RUN  CAND  FACTOR LEVELS',/,1X,35('-'))
C 1209 FORMAT(1X,26('*'))
C 1210 FORMAT(//,8X,'RANK         D',6X,'DESIGN(ROWS)',/,
C     &8X,4('-'),7X,5('-'),4X,98('-'))
C 1211 FORMAT(1X,'  I  J   P(I)   P(J)  TRACE1  TRACE2 QF(I,J)',
C     &' QF(J,I)  N*  TERM(I,J)')

C     CLOSE(OUT)

C      FLAG = 1
      RETURN
      END

      SUBROUTINE FINDMIN(I,J,V,N)
      DOUBLE PRECISION V(N),D
      INTEGER I,J,N,K
      IF (I .LE. N) THEN
        J=I
        RETURN
      ELSE
        D=1.0D20
        DO 100 K=1,N
           IF (V(K) .LT. D) THEN
             J=K
             D=V(K)
           ENDIF
 100      CONTINUE
          RETURN
      ENDIF
      END


      SUBROUTINE RANST(N1,N,ROWS,R)
      INTEGER I,N1,N,ROWS(N)
      DOUBLE PRECISION X,R

C  THE FUNCTION RAND RETURNS A UNIFORM(0,1) DEVIATE;
C  IF ANOTHER RANDOM NUMBER GENERATOR IS AVAILABLE THAT
C  IS SET UP FOR THE MACHINE THIS IS RUNNING ON, IT CAN BE SUBSTITUTED

         	  RR=R
      DO 1 I=1,N1
        X=RANDO(RR)
        ROWS(I)=IDINT((N-1)*X)+1
 1    continue 
      	    RETURN
      END


      SUBROUTINE EVAL(N1,ROWS,D,NM)

C	modification original EVAL for Objective Bayesian Analysis

      COMMON BETA,G,P,X,SIGMA2,NF,JFAC,BL,CUT,COLS
      DOUBLE PRECISION BETA(100,100),G(100,100,100)
      DOUBLE PRECISION YHAT(100,32),W(32),W1(32)
      DOUBLE PRECISION D,D0,TR,TR1,TR2,DEV,DEV2,DEV1,RCOND,DET(2)
      DOUBLE PRECISION P(100),X(256,100),SIGMA2(100)
      DOUBLE PRECISION A(32,100)
      DOUBLE PRECISION V(32,32),V2(32,32),Z(32)
      DOUBLE PRECISION DV(100,32,32),DV2(100,32,32)
      INTEGER IM,NM,I,J,BL,NF(100),TK,TOTO,M,CUT,I0,I1,I2,CNO
      INTEGER JFAC(100,20),MULT(20),ROWS(32),N1,COLS,INFO
      LOGICAL PART
C
      D=0.0
      DO 210 IM=1,NM
        TK=NF(IM)
C
        DO 215 I=1,N1
          A(I,1)=1.0
          DO 220 J=1,BL
             A(I,1+J)=X(ROWS(I),J)
 220      continue 
          DO 215 J=1,TK                                                                                                           
             A(I,J+1+BL)=X(ROWS(I),JFAC(IM,J)+BL)
 215      continue 
        TOTO=TK+1+BL

C     AUGMENT WITH INTERACTION COLUMNS

        DO 225 M=2,MIN(CUT,TK)
          CALL INITIA2(MULT,M)
          PART=.FALSE.
 230      IF (.NOT. PART) THEN
            TOTO=TOTO+1
            I0=JFAC(IM,MULT(1))
            I1=JFAC(IM,MULT(2))
            IF (M .EQ. 2) THEN
              CNO=(I0-1)*COLS-(I0-1)*I0/2+I1-I0+COLS+BL
            ELSE
              I2=JFAC(IM,MULT(3))
              CNO=(COLS-2)*COLS-(COLS-2)*(COLS-1)/2+1+COLS+
     &  ((I0-1)*COLS*COLS-(I0+1)*(I0-1)*COLS+(I0*I0*I0-I0)/3)/2+
     &  (I1-I0-1)*(COLS-I0)-(I1-I0-1)*(I1-I0)/2+I2-I1+BL
            ENDIF
           DO 235 I=1,N1
      				A(I,TOTO)=X(ROWS(I),CNO)
 235       continue  
           CALL INVREM2(MULT,PART,M,TK)
           GO TO 230
         ENDIF
 225  CONTINUE



      DO I=1,TOTO
      DO J=1,N1
         V(I,J)=0.0
       DO M=1,TOTO
         V(I,J)=V(I,J)+G(IM,I,M)*A(J,M)
       end do
      	     end do 
      	     end do 

      DO I=1,N1
      V2(I,I)=1.0
      DO J=1,N1
         IF (I .NE. J) V2(I,J)=0.0
         DO M=1,TOTO
           V2(I,J)=V2(I,J)+A(I,M)*V(M,J)
         end do 
         end do
      	    end do 

      DO I=1,N1
        DO  J=1,N1
          V(I,J)=V2(I,J)
      end do 
      	    end do 

C     CALL DLINDS(N1,V2,32,V,32)

      CALL DPOCO(V,32,N1,RCOND,Z,INFO)

      	      IF (info.ne.0) THEN                                    
      	    ENDIF
      	
      CALL DPODI(V,32,N1,DET,11)


      DO I=1,N1
        DO  J=1,N1
          IF (I .GT. J) V(I,J)=V(J,I)
          DV(IM,I,J)=V(I,J)
          DV2(IM,I,J)=V2(I,J)
        end do 
      end do 

      DO I=1,N1
        YHAT(IM,I)=0.0
        DO J=1,TOTO
          YHAT(IM,I)=YHAT(IM,I)+A(I,J)*BETA(IM,J)
        end do 
      	    end do

 210  CONTINUE


      DO 300 IM=1,NM-1
C       WRITE(15,*) 'DES- ',ID,' 1ST MODEL PAIR MEMBER- ',IM
        DO 301 JM=IM+1,NM

        TR=0.0
        TR1=0.0
        TR2=0.0
        DO I=1,N1
        DO  J=1,N1
         TR1=TR1+0.5*(DV(IM,I,J)*DV2(JM,J,I))
         TR2=TR2+0.5*(DV2(IM,I,J)*DV(JM,J,I))
         TR=TR+0.5*(DV(IM,I,J)*DV2(JM,J,I)+DV2(IM,I,J)*DV(JM,J,I))
        end do
      	      end do 

      DO I=1,N1
        W(I)=0.0
        W1(I)=0.0
        DO J=1,N1
          W(I)=W(I)+DV(JM,I,J)*(YHAT(IM,J)-YHAT(JM,J))/SIGMA2(IM)
          W1(I)=W1(I)+DV(IM,I,J)*(YHAT(IM,J)-YHAT(JM,J))/SIGMA2(JM)
      end do
      	    end do 

      	    DEV=0.0
      DEV1=0.0
      DEV2=0.0
      DO 330 I=1,N1
        DEV1=DEV1+(YHAT(IM,I)-YHAT(JM,I))*W1(I)
        DEV2=DEV2+(YHAT(IM,I)-YHAT(JM,I))*W(I)
 330  continue 
      	    DEV1=DEV1/2.0
      DEV2=DEV2/2.0
      DEV=(DEV1+DEV2)
  
      	    D0=P(IM)*P(JM)*(TR+DEV-N1)
      D=D+D0
C      WRITE(OUT,500) IM,JM,P(IM),P(JM),TR1,TR2,DEV1,DEV2,N1,D0
 301  CONTINUE
 300  continue  
C 500  FORMAT(1X,I3,I3,F7.4,F7.4,F8.2,F8.2,F8.2,F8.2,I3,F8.2)
      RETURN
      END




      SUBROUTINE INVREM2(J,ALL,R,N)
      INTEGER M,L,R,N,J(20)
      LOGICAL OK,ALL
      L=R
      ALL=.FALSE.
      OK=.FALSE.
 50   IF ((.NOT. OK) .AND. (L .GT. 0)) THEN
         IF (J(L) .LT. N-R+L) THEN
            J(L)=J(L)+1
            DO 101 M=L+1,R
              J(M)=J(M-1)+1
 101        continue 
      	      OK=.TRUE.
         ELSE
            L=L-1
         ENDIF                                                          
         GO TO 50                                                       
      ENDIF                                                             
      IF (L .LE. 0) ALL=.TRUE.                                          
      RETURN                                                            
      END                                                               
C                                                                       
      SUBROUTINE INITIA2(J,R)
        INTEGER J(20),R,I                                               
         DO 401 I=1,R                                                   
           J(I)=I 
 401     continue                                                      
         DO 402 I=R+1,20                                                
           J(I)=0
 402     continue                                                        
      RETURN                                                            
      END                                                               
                                                                       
      FUNCTION RANDO (R)
C APRIL 1977 VERSION.  W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
C
C      THIS PSEUDO-RANDOM NUMBER GENERATOR IS PORTABLE AMOUNG A WIDE
C VARIETY OF COMPUTERS.  RAND(R) UNDOUBTEDLY IS NOT AS GOOD AS MANY
C READILY AVAILABLE INSTALLATION DEPENDENT VERSIONS, AND SO THIS
C ROUTINE IS NOT RECOMMENDED FOR WIDESPREAD USAGE.  ITS REDEEMING
C FEATURE IS THAT THE EXACT SAME RANDOM NUMBERS (TO WITHIN FINAL ROUND-
C OFF ERROR) CAN BE GENERATED FROM MACHINE TO MACHINE.  THUS, PROGRAMS
C THAT MAKE USE OF RANDOM NUMBERS CAN BE EASILY TRANSPORTED TO AND      
C CHECKED IN A NEW ENVIRONMENT.                                         
C      THE RANDOM NUMBERS ARE GENERATED BY THE LINEAR CONGRUENTIAL      
C METHOD DESCRIBED, E.G., BY KNUTH IN SEMINUMERICAL METHODS (P.9),      
C ADDISON-WESLEY, 1969.  GIVEN THE I-TH NUMBER OF A PSEUDO-RANDOM
C SEQUENCE, THE I+1 -ST NUMBER IS GENERATED FROM                        
C             X(I+1) = (A*X(I) + C) MOD M,                              
C WHERE HERE M = 2**22 = 4194304, C = 1731 AND SEVERAL SUITABLE VALUES  
C OF THE MULTIPLIER A ARE DISCUSSED BELOW.  BOTH THE MULTIPLIER A AND   
C RANDOM NUMBER X ARE REPRESENTED IN DOUBLE PRECISION AS TWO 11-BIT     
C WORDS.  THE CONSTANTS ARE CHOSEN SO THAT THE PERIOD IS THE MAXIMUM    
C POSSIBLE, 4194304.                                                    
C      IN ORDER THAT THE SAME NUMBERS BE GENERATED FROM MACHINE TO      
C MACHINE, IT IS NECESSARY THAT 23-BIT INTEGERS BE REDUCIBLE MODULO     
C 2**11 EXACTLY, THAT 23-BIT INTEGERS BE ADDED EXACTLY, AND THAT 11-BIT 
C INTEGERS BE MULTIPLIED EXACTLY.  FURTHERMORE, IF THE RESTART OPTION   
C IS USED (WHERE R IS BETWEEN 0 AND 1), THEN THE PRODUCT R*2**22 =      
C R*4194304 MUST BE CORRECT TO THE NEAREST INTEGER.                     
C      THE FIRST FOUR RANDOM NUMBERS SHOULD BE .0004127026,             
C .6750836372, .1614754200, AND .9086198807.  THE TENTH RANDOM NUMBER
C IS .5527787209, AND THE HUNDREDTH IS .3600893021 .  THE THOUSANDTH    
C NUMBER SHOULD BE .2176990509 .                                        
C      IN ORDER TO GENERATE SEVERAL EFFECTIVELY INDEPENDENT SEQUENCES
C WITH THE SAME GENERATOR, IT IS NECESSARY TO KNOW THE RANDOM NUMBER
C FOR SEVERAL WIDELY SPACED CALLS.  THE I-TH RANDOM NUMBER TIMES 2**22, 
C WHERE I=K*P/8 AND P IS THE PERIOD OF THE SEQUENCE (P = 2**22), IS     
C STILL OF THE FORM L*P/8.  IN PARTICULAR WE FIND THE I-TH RANDOM       
C NUMBER MULTIPLIED BY 2**22 IS GIVEN BY                                
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8      
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0          
C THUS THE 4*P/8 = 2097152 RANDOM NUMBER IS 2097152/2**22.              
C      SEVERAL MULTIPLIERS HAVE BEEN SUBJECTED TO THE SPECTRAL TEST     
C (SEE KNUTH, P. 82).  FOUR SUITABLE MULTIPLIERS ROUGHLY IN ORDER OF    
C GOODNESS ACCORDING TO THE SPECTRAL TEST ARE
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5             
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5                     
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5              
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1                       
C                                                                       
C      IN THE TABLE BELOW LOG10(NU(I)) GIVES ROUGHLY THE NUMBER OF      
C RANDOM DECIMAL DIGITS IN THE RANDOM NUMBERS CONSIDERED I AT A TIME.
C C IS THE PRIMARY MEASURE OF GOODNESS.  IN BOTH CASES BIGGER IS BETTER.
C                                                                       
C                   LOG10 NU(I)              C(I)
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
C
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
C   BEST
C    POSSIBLE   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
C
C             INPUT ARGUMENT --
C R      IF R=0., THE NEXT RANDOM NUMBER OF THE SEQUENCE IS GENERATED.
C        IF R.LT.0., THE LAST GENERATED NUMBER WILL BE RETURNED FOR
C          POSSIBLE USE IN A RESTART PROCEDURE.
C        IF R.GT.0., THE SEQUENCE OF RANDOM NUMBERS WILL START WITH THE
C          SEED R MOD 1.  THIS SEED IS ALSO RETURNED AS THE VALUE OF
C          RAND PROVIDED THE ARITHMETIC IS DONE EXACTLY.
C
C             OUTPUT VALUE --
C RAND   A PSEUDO-RANDOM NUMBER BETWEEN 0. AND 1.
C
C IA1 AND IA0 ARE THE HI AND LO PARTS OF A.  IA1MA0 = IA1 - IA0.
      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/
      DATA IC /1731/
      DATA IX1, IX0 /0, 0/

      IF (R.LT.0.) GO TO 10
      IF (R.GT.0.) GO TO 20

C           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)
C                   + IA0*IX0) + IA0*IX0

      IY0 = IA0*IX0
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0
      IY0 = IY0 + IC
      IX0 = MOD (IY0, 2048)
      IY1 = IY1 + (IY0-IX0)/2048
      IX1 = MOD (IY1, 2048)

 10   RANDO = IX1*2048 + IX0
      RANDO = RANDO / 4194304.
      RETURN

 20   IX1 = AMOD(R,1.)*4194304. + 0.5
      IX0 = MOD (IX1, 2048)
      IX1 = (IX1-IX0)/2048
      GO TO 10

      END

