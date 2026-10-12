      SUBROUTINE DGELYP(N,A,C,Q,WK,TYP,JOB,INFO)
      INTEGER N,INFO, JOB, TYP
      DOUBLE PRECISION A(N,N), C(N,N), Q(N,N), WK(5*N)
c     Solve Lyapunov equation 
c         AX + XA**T = C 
c         A**T X + XA = C
c     If JOB .EQ. 0 DGELYP first obtains the Schur factorization and 
c     check if all 
c     eigenvalues have negative real part.
c     IF JOB .GT. 0 DGELYP assume the matrix A is already in Schur form 
c     and Q contains the orthogonal matrix that transfromed it into the
c     Schur form, moreover A is assumed to be stable and no check is
c     performed. 
c     IF JOB .GE. 2 the solution is not back-tranformed with the
c     orthogonal matrix Q, thus QXQ**T is actually returned.
c     internal variables
c   
c     lapck subroutine used 
c     DGEES
c     DTRSYL
c     DGEMM
      INTEGER K,SDIM, UNO,INDR,INDI,INDW
      LOGICAL BWORK(N)
      CHARACTER TRANA, TRANB
      DOUBLE PRECISION ONE,ZERO,SCA,TMP(N,N)
      PARAMETER(ONE=1.0d+0, ZERO=0.0d+0, UNO=1)
      INFO = 0
      SCA = 1.0
      INDR = 1
      INDI = INDR + N
      INDW = INDI + N
      IF (TYP .EQ. 0) THEN
              TRANA = "N"
              TRANB = "T"
      ELSE
              TRANA = "T"
              TRANB = "N"
      ENDIF
c     Schur factorization if needed
      IF (JOB .EQ. 0) THEN 
         CALL DGEES('V','N',SEL,
     *N,A,N,SDIM,WK(INDR),WK(INDI),Q,N,WK(INDW)
     *,3*N,BWORK, INFO) 
c        check stability of A, if no stable return with INFO = -1
         DO 10 K=1,N
            IF (WK(K) .GE. 0) THEN
               INFO = -1 
               GOTO 900
            ENDIF
  10     CONTINUE
      ENDIF
c     Transform C into Q**TCQ and save into C
c       transform C into  Q**TC and save into TMP
      CALL DGEMM('T','N',N,N,N,ONE,Q,N,C,N,ZERO,TMP,N)
c       transform TMP into CQ and save into C
      CALL DGEMM('N','N',N,N,N,ONE,TMP,N,Q,N,ZERO,C,N)
c      CALL MQFWO(N,N,N,C,Q,WK)
c     solve associated sylvester equation
      CALL DTRSYL(TRANA, TRANB, UNO, N, N, A, N, A, N, C, N, SCA, INFO)
cc     transform C into QCQ**T
      IF (JOB .LT. 2) THEN
         CALL DGEMM('N','N',N,N,N,ONE,Q,N,C,N,ZERO,TMP,N)
         CALL DGEMM('N','T',N,N,N,ONE,TMP,N,Q,N,ZERO,C,N)
      ENDIF
 900  CONTINUE
      RETURN
c     last line of DGELYP
      END
c
c     logical function as parameter of DGEES
      LOGICAL FUNCTION SEL(X,Y)
              DOUBLE PRECISION X,Y
              SEL = .TRUE.
              RETURN
      END
c
c
      SUBROUTINE GRAD(N,B,D,S,Q,WK,IX,DD)
      INTEGER N, IX(N * N) 
      DOUBLE PRECISION B(N,N),S(N,N),Q(N,N),DD(N,N),WK(5*N),D(N,N)
c     Subroutine GRAD
c 
c     GRAD computee the gradient  
c     with respect to the entries of the B and C matrix. 
c     In particular it computes the gradient 
c     df/dB = JS(B) dg/dS   where f(B) = g(S(B)) and S(B)
c     denotes the solution of the Lyapunov equation
c     BS + SB'+ C = 0
c
c local variables
      INTEGER I, J, INFO
      DOUBLE PRECISION ZERO, UNO
      ZERO = 0.0
      UNO = 1.0
      CALL DGELYP(N, B, D, Q, WK, 1, 1, INFO)
      CALL DSYMM("R", "U", N, N, UNO, S, N, D, N, ZERO, DD, N) 
      DO 40 J = 1, N
        DO 30 I = 1, N
        IF (IX(I + (J-1)*N) .EQ. 1) THEN
           DD(I,J) = 2 * DD(I,J) 
        ELSE
           DD(I,J) = 0 
        ENDIF
  30    CONTINUE         
  40  CONTINUE     
      RETURN
c     last line of GRAD
      END
      SUBROUTINE GCLMLL(N, SIGMA, B, C, CZ, LAMBDA, LAMBDAC, EPS, 
     *                  ALPHA, MAXITR, JOB)  
      INTEGER N, MAXITR, JOB
      DOUBLE PRECISION SIGMA(N,N), B(N,N), C(N), CZ(N), LAMBDA,
     *EPS, ALPHA, LAMBDAC
c     internal variables
      INTEGER I,J,K,INFO, IX(N*N), ITR
      DOUBLE PRECISION DB(N,N),TMPB(N,N), TMP(N,N), Q(N,N),
     *F,FNW,WK(7*N), S(N,N), STEP, DS(N), BOLD(N,N), H, HNW,
     * UNO, ZERO, G, GNW, DIFF, COLD(N), DC(N), STEPB, STEPC
c     copy C,B,SIGMA and initialize IX 
      ITR = 0
      UNO = 1.0
      ZERO = 0.0
      STEP = 1
      F = 0
      G = 0 
      H = 0
      FNW = 0
      GNW = 0 
      HNW = 0
      DO 20 J = 1,N
         DO 10 I = 1,N
            IX((J-1)*N + I) = 1
            IF (JOB / 10 .EQ. 1 .AND. B(I,J) .EQ. 0) IX((J-1)*N + I)=0 
            S(I,J) = 0 
            TMPB(I,J) = B(I,J)
  10     CONTINUE          
            S(J,J) = - C(J)
  20  CONTINUE          
c     obtain S, solution of CLE
      CALL DGELYP(N,TMPB,S,Q,WK,0,0,INFO)
      IF (INFO .LT. 0) GOTO 900
c     save diagonal of S
      DO 45 K=1,N
         DS(K) = S(K,K) 
 45   CONTINUE
c     obtain cholesky decomposition of S = SIGMA(B,C)
      CALL DPOTRF("L", N, S, N, INFO)
c     compute initial objective function
      F = 0
      G = 0
      H = 0
      DO 46 K=1,N
         F = F + 2 * LOG(S(K,K)) 
         IF (LAMBDAC .GT. 0) H = H + LAMBDAC * (C(K) - CZ(K))**2 
 46   CONTINUE
c     obtain S^(-1)
      CALL DPOTRI("L", N, S, N, INFO)
      DO 60 J = 1,N - 1
         DO 50 I = J + 1,N
            F = F +  
     *          2*S(I,J)*SIGMA(I,J)   
     *           
            G = G + LAMBDA * (ABS(B(I,J)) + ABS(B(J,I))) 
 50      CONTINUE        
            F = F + S(J,J) * SIGMA(J,J) 
 60   CONTINUE
      F = F + S(N,N) * SIGMA(N,N)
c     main loop here, increase iteration counter
 500  CONTINUE      
      ITR = ITR + 1
c     compute P*SIGMA, where P = S^(-1)
      CALL DSYMM("L", "L", N, N, UNO, S, N, SIGMA, N, ZERO, DB, N)
c     compute P*SIGMA - I
      DO 70 K=1,N
         DB(K,K) = DB(K,K) - 1
 70   CONTINUE
c     compute (P*SIGMA - I)*P = P*SIGMA*P - P
      CALL DSYMM("R", "L", N, N, UNO, S, N, DB, N, ZERO, TMP, N)
c     compute gradient 
      DO 75 K=1,N
         S(K,K) = DS(K)
 75   CONTINUE
      CALL GRAD(N,TMPB,TMP,S,Q,WK,IX,DB)
c     copy old B before starting line search 
      DO 90 J = 1,N
         DO 80 I = 1,N
            BOLD(I,J) = B(I,J)
  80     CONTINUE          
         COLD(J) = C(J) 
         DC(J) = 2*TMP(J,J) + 2 * LAMBDAC * (C(J) - CZ(J)) 
  90  CONTINUE 
      STEP = 1
      STEPB = 1
      STEPC = 1
c     line search loop here
  600 CONTINUE     
c     gradient step
      DO 110 J = 1,N
         DO 100 I = 1,N
            B(I,J) = BOLD(I,J) - STEP * STEPB * DB(I,J) 
  100    CONTINUE
         IF (LAMBDAC .GE. 0) THEN
            C(J) = COLD(J) - STEP * STEPC * DC(J) 
            IF (C(J) .LE. 0) THEN
               STEPC = STEPC * ALPHA
               GOTO 600
            ENDIF
         ENDIF
  110 CONTINUE
c     soft thresholding
      DO 130 J =1,N
         DO 120 I=1,N
            IF (I .NE. J .AND. IX((J-1)*N + I) .EQ. 1) THEN
              B(I,J) = SIGN(UNO,B(I,J))*(ABS(B(I,J))-STEP*STEPB*LAMBDA) 
              IF (ABS(B(I,J)) .LT. STEP*STEPB*LAMBDA) THEN
                 B(I,J) = 0
              ENDIF
            ENDIF
 120     CONTINUE
 130  CONTINUE
c     solve new Lyapunov equation
      DO 150 J = 1,N
         DO 140 I = 1,N
            S(I,J) = 0 
            TMPB(I,J) = B(I,J)
  140    CONTINUE          
            S(J,J) = - C(J)
  150 CONTINUE 
      CALL DGELYP(N,TMPB,S,Q,WK,0,0,INFO)
c     chek if B is stable
      IF (INFO .LT. 0) THEN
         STEPB = STEPB * ALPHA
         GOTO 600
      ENDIF 
c     save diagonal of S
      DO 155 K=1,N
         DS(K) = S(K,K) 
  155 CONTINUE
c     obtain cholesky decomposition of S = SIGMA(B,C)
      CALL DPOTRF("L", N, S, N, INFO)
      FNW = 0
      GNW = 0
      HNW = 0
      DIFF = 0
      DO 160 K=1,N
         FNW = FNW + 2 * LOG(S(K,K)) 
         IF (LAMBDAC .GT. 0) HNW = HNW + LAMBDAC*(C(K) - CZ(K))**2
  160 CONTINUE
c     obtain S^(-1)
      CALL DPOTRI("L", N, S, N, INFO)
c     compute FNW, objective function in new B
      DO 180 J = 1,N - 1
         DO 170 I = J + 1,N
            FNW = FNW + 
     *          2*S(I,J)*SIGMA(I,J)              
            GNW = GNW + LAMBDA * (ABS(B(I,J)) + ABS(B(J,I))) 
             DIFF = DIFF+((B(I,J)-BOLD(I,J))**2)/(2 *STEP * STEPB) +  
     *       (B(I,J) - BOLD(I,J)) * DB(I,J) + 
     *       ((B(J,I) - BOLD(J,I))**2) / (2*STEP* STEPB) + 
     *       (B(J,I) - BOLD(J,I)) * DB(J,I)   
 170     CONTINUE        
         FNW = FNW + S(J,J) * SIGMA(J,J)
         DIFF = DIFF + ((B(J,J) - BOLD(J,J))**2)/(2*STEP*STEPB) +  
     *          (B(J,J) - BOLD(J,J)) * DB(J,J) 
         IF (LAMBDAC .GE. 0) THEN 
            DIFF = DIFF + ((C(J) - COLD(J))**2)/(2*STEP*STEPC) +
     *                    (C(J) - COLD(J)) * DC(J)  
         ENDIF
 180  CONTINUE
      FNW = FNW + S(N,N) * SIGMA(N,N)
      DIFF = DIFF + ((B(N,N) - BOLD(N,N))**2) / (2*STEP* STEPB) + 
     *       (B(N,N) - BOLD(N,N)) * DB(N,N)  
      IF (LAMBDAC .GE. 0) THEN 
         DIFF = DIFF + ((C(N) - COLD(N))**2)/(2*STEP *STEPC) +
     *                 (C(N) - COLD(N)) * DC(N) 
      ENDIF
c     descent condition
      IF ((FNW + HNW) .GT. F + H + DIFF .OR. 
     *    (FNW + GNW + HNW) .GT. (F + G + H)) THEN
         STEP = STEP * ALPHA
         IF (STEP .LE. 0) GOTO 900
         GOTO 600
      ENDIF
c     check stopping criteria
      IF (((F+G+H-FNW-GNW-HNW)  .LE. EPS).OR.
     *    ((F+G+H-FNW-GNW-HNW)/(F+G+H)  .LE. EPS).OR.
     *   (ITR .GE. MAXITR)) THEN
         GOTO 900 
      ENDIF  
      IF (MOD(JOB,10) .EQ. 1) THEN
         DO 240 J=1,N
            DO 230 I=1,N
               IF (B(I,J) .EQ. 0) IX((J-1)*N+I)=0
 230        CONTINUE
 240     CONTINUE
      ENDIF
c     update value of objective function and repeat
      F = FNW
      G = GNW
      H = HNW
      GOTO 500
 900  CONTINUE
c     terminate and save additional outputs
         ALPHA = FNW 
         EPS = (F+H+G - FNW - GNW - HNW)  
         MAXITR = ITR
         DO 220 J=2,N
            DO 210 I=1,J-1
               SIGMA(I,J) = S(I,J)
               SIGMA(J,I) = S(I,J)
 210        CONTINUE   
            SIGMA(J,J) = DS(J)
 220     CONTINUE         
         SIGMA(1,1) = DS(1)
      RETURN
      END
      SUBROUTINE GCLMLS(N, SIGMA, B, C, CZ, LAMBDA, LAMBDAC, EPS, 
     *                  ALPHA, MAXITR, JOB)  
      INTEGER N, MAXITR, JOB
      DOUBLE PRECISION SIGMA(N,N), B(N,N), C(N), CZ(N), LAMBDA,
     *EPS, ALPHA, LAMBDAC
c     internal variables
      INTEGER I,J,K,INFO, IX(N*N), ITR
      DOUBLE PRECISION DB(N,N),TMPB(N,N), TMP(N,N), Q(N,N),
     *F,FNW,WK(7*N), S(N,N), STEP, BOLD(N,N), H, HNW,
     *UNO, ZERO, G, GNW, DIFF, COLD(N), DC(N), STEPB, STEPC
c     copy C,B,SIGMA and initialize IX 
      ITR = 0
      UNO = 1.0
      ZERO = 0.0
      F = 0
      G = 0 
      H = 0
      FNW = 0
      GNW = 0 
      HNW = 0
      DO 20 J = 1,N
         DO 10 I = 1,N
            IX((J-1)*N + I) = 1
            IF (JOB / 10 .EQ. 1 .AND. B(I,J) .EQ. 0) IX((J-1)*N + I)=0 
            S(I,J) = 0 
            TMPB(I,J) = B(I,J)
  10     CONTINUE          
            S(J,J) = - C(J)
  20  CONTINUE          
c     obtain S, solution of CLE
      CALL DGELYP(N,TMPB,S,Q,WK,0,0,INFO)
      IF (INFO .LT. 0) GOTO 900
c     compute initial objective function
      F = 0
      G = 0
      H = 0
      DO 60 J = 1,N
         DO 50 I = 1,N
            TMP(I,J) = SIGMA(I,J) - S(I,J)
            F = F + 0.5 * (TMP(I,J)**2)   
            G = G + LAMBDA * ABS(B(I,J))
 50      CONTINUE        
            G = G - LAMBDA * ABS(B(J,J))
c            F = F + 0.5 * (TMP(J,J)**2)
c            TMP(J,J) = 0.5 * TMP(J,J) 
            IF (LAMBDAC .GT. 0) THEN
                H = H + LAMBDAC * (C(J) - CZ(J))**2 
            ENDIF
 60   CONTINUE
c     main loop here, increase iteration counter
 500  CONTINUE      
      ITR = ITR + 1
c     compute gradients
      CALL GRAD(N,TMPB,TMP,S,Q,WK,IX,DB)
c     copy old B before starting line search 
      DO 90 J = 1,N
         DO 80 I = 1,N
            BOLD(I,J) = B(I,J)
  80     CONTINUE          
            COLD(J) = C(J) 
            DC(J) = 2 * TMP(J,J) + 2 * LAMBDAC * (C(J) - CZ(J)) 
  90  CONTINUE  
      STEP = 1
      STEPB = 1
      STEPC = 1
c     line search loop here
  600 CONTINUE     
c     gradient step
      DO 110 J = 1,N
         DO 100 I = 1,N
            B(I,J) = BOLD(I,J) - STEP * STEPB * DB(I,J) 
  100    CONTINUE
            IF (LAMBDAC .GE. 0) THEN 
               C(J) = COLD(J) - STEP * STEPC * DC(J) 
               IF (C(J) .LE. 0) THEN
                   STEPC = STEPC * ALPHA
                   GOTO 600
               ENDIF
            ENDIF
  110 CONTINUE
c     soft thresholding
      DO 130 J =1,N
         DO 120 I=1,N
            IF (I .NE. J .AND. IX((J-1)*N + I) .EQ. 1) THEN
              B(I,J) = SIGN(UNO,B(I,J))*(ABS(B(I,J))-STEP*STEPB*LAMBDA) 
              IF (ABS(B(I,J)) .LT. STEP*STEPB*LAMBDA) THEN
                 B(I,J) = 0
              ENDIF
            ENDIF
 120     CONTINUE
 130  CONTINUE
c     solve new Lyapunov equation
      DO 150 J = 1,N
         DO 140 I = 1,N
            S(I,J) = 0 
            TMPB(I,J) = B(I,J)
  140    CONTINUE          
            S(J,J) = - C(J)
  150 CONTINUE 
      CALL DGELYP(N,TMPB,S,Q,WK,0,0,INFO)
c     chek if B is stable
      IF (INFO .LT. 0) THEN
         STEPB = STEPB * ALPHA
         GOTO 600
      ENDIF 
      FNW = 0
      GNW = 0
      HNW = 0
      DIFF = 0
c     compute objective function in new B,C
      DO 180 J = 1,N
         DO 170 I = 1,N
            TMP(I,J) = SIGMA(I,J) - S(I,J)
            FNW = FNW + 0.5 * (TMP(I,J)**2)            
            GNW = GNW + LAMBDA * ABS(B(I,J)) 
             DIFF = DIFF +((B(I,J)-BOLD(I,J))**2)/(2*STEP* STEPB)+  
     *       (B(I,J) - BOLD(I,J)) * DB(I,J) 
 170     CONTINUE        
c            FNW = FNW + 0.5 * (TMP(J,J)**2)            
c            TMP(J,J) = 0.5 * TMP(J,J) 
            GNW = GNW - LAMBDA * ABS(B(J,J)) 
            IF (LAMBDAC .GE. 0) THEN 
               HNW = HNW + LAMBDAC*(C(J) - CZ(J))**2
               DIFF = DIFF +   
     *                ((C(J) - COLD(J))**2)/(2*STEP * STEPC)
     *                + (C(J) - COLD(J)) * DC(J)
            ENDIF
 180  CONTINUE
c     descent condition
      IF ((FNW + HNW) .GT. F + H + DIFF .OR. 
     *    (FNW + GNW + HNW) .GT. (F + G + H)) THEN
             STEP = STEP * ALPHA
             IF (STEP .LE. 0) GOTO 900
             GOTO 600
      ENDIF
c     check stopping criteria
      IF (((F+G+H-FNW-GNW-HNW)  .LE. EPS).OR.
     *    ((F+G+H-FNW-GNW-HNW)/(F+G+H)  .LE. EPS).OR.
     *   (ITR .GE. MAXITR)) THEN
         GOTO 900 
      ENDIF  
      IF (MOD(JOB,10) .EQ. 1) THEN
         DO 240 J=1,N
            DO 230 I=1,N
               IF (B(I,J) .EQ. 0) IX((J-1)*N+I)=0
 230        CONTINUE
 240     CONTINUE
      ENDIF
c     update value of objective function and repeat
      F = FNW
      G = GNW
      H = HNW
      GOTO 500
 900  CONTINUE
c     terminate and save additional outputs
         ALPHA = FNW 
         EPS = (F+H+G - FNW - GNW - HNW) 
         MAXITR = ITR
         DO 220 J=1,N
            DO 210 I=1,N
               SIGMA(I,J) = S(I,J)
 210        CONTINUE   
 220     CONTINUE         
      RETURN
      END
