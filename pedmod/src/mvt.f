*
*    $Id: mvt.f 355 2019-06-19 09:59:17Z thothorn $
*
*     A couple of subroutines from
*     "Comparison of Methods for the Computation of Multivariate
*         t-Probabilities", by Alan Genz and Frank Bretz
*         J. Comp. Graph. Stat. 11 (2002), pp. 950-971.
*
*          Alan Genz
*          Department of Mathematics
*          Washington State University
*          Pullman, WA 99164-3113
*          Email : AlanGenz@wsu.edu
*
*	Original source available from
*	http://www.math.wsu.edu/faculty/genz/software/fort77/mvtdstpack.f
*
      SUBROUTINE MVSORT( N, LOWER, UPPER, DELTA, CORREL, INFIN, Y,
     &                  PIVOTIN,
     &                  ND,     A,     B,    DL,    COV,  INFI, INFORM,
     &                  IDX, DOSCALEIN  )
*
*     Subroutine to sort integration limits and determine Cholesky factor.
*
*     Benjamin Christoffersen added the IDX argument. It keeps track of the
*     original indices. The DOSCALE argument is added to determine whether
*     to scale the composition to have ones in the diagonal.
*
      INTEGER N, ND, INFIN(*), INFI(*), INFORM, PIVOTIN, DOSCALEIN
      LOGICAL PIVOT, DOSCALE
      DOUBLE PRECISION     A(*),     B(*),    DL(*),    COV(*),
     &                 LOWER(*), UPPER(*), DELTA(*), CORREL(*), Y(*)
      INTEGER I, J, K, L, M, II, IJ, IL, JL, JMIN, IDX(*)
      DOUBLE PRECISION SUMSQ, AJ, BJ, SUM, EPS, EPSI, D, E
      DOUBLE PRECISION CVDIAG, AMIN, BMIN, DEMIN, MVTDNS
      PARAMETER ( EPS = 1D-10 )
      PIVOT = PIVOTIN .NE. 0
      DOSCALE = DOSCALEIN .NE. 0
      INFORM = 0
      IJ = 0
      II = 0
      ND = N
      DO I = 1, N
         A(I) = 0
         B(I) = 0
         DL(I) = 0
         INFI(I) = INFIN(I)
         IF ( INFI(I) .LT. 0 ) THEN
            ND = ND - 1
         ELSE
            IF ( INFI(I) .NE. 0 ) A(I) = LOWER(I)
            IF ( INFI(I) .NE. 1 ) B(I) = UPPER(I)
            DL(I) = DELTA(I)
         ENDIF
         DO J = 1, I-1
            IJ = IJ + 1
            II = II + 1
            COV(IJ) = CORREL(II)
         END DO
         IJ = IJ + 1
         COV(IJ) = 1
      END DO
*
*     First move any doubly infinite limits to innermost positions.
*
      IF ( ND .GT. 0 ) THEN
         DO I = N, ND + 1, -1
            IF ( INFI(I) .GE. 0 ) THEN
               DO J = 1, I-1
                  IF ( INFI(J) .LT. 0 ) THEN
                     CALL MVSWAP( J, I, A, B, DL, INFI, N, COV, IDX )
                     GO TO 10
                  ENDIF
               END DO
            ENDIF
 10         CONTINUE
         END DO
*
*     Sort remaining limits and determine Cholesky factor.
*
         II = 0
         JL = ND
         DO I = 1, ND
*
*        Determine the integration limits for variable with minimum
*        expected probability and interchange that variable with Ith.
*
            DEMIN = 1
            JMIN = I
            CVDIAG = 0
            IJ = II
            EPSI = EPS*I
            IF ( .NOT. PIVOT ) JL = I
            DO J = I, JL
               IF ( COV(IJ+J) .GT. EPSI ) THEN
                  SUMSQ = SQRT( COV(IJ+J) )
                  SUM = DL(J)
                  DO K = 1, I-1
                     SUM = SUM + COV(IJ+K)*Y(K)
                  END DO
                  AJ = ( A(J) - SUM )/SUMSQ
                  BJ = ( B(J) - SUM )/SUMSQ
                  CALL MVLIMS( AJ, BJ, INFI(J), D, E )
                  IF ( DEMIN .GE. E - D ) THEN
                     JMIN = J
                     AMIN = AJ
                     BMIN = BJ
                     DEMIN = E - D
                     CVDIAG = SUMSQ
                  ENDIF
               ENDIF
               IJ = IJ + J
            END DO
            IF ( JMIN .GT. I ) THEN
               CALL MVSWAP( I, JMIN, A, B, DL, INFI, N, COV, IDX )
            END IF
            IF ( COV(II+I) .LT. -EPSI ) THEN
               INFORM = 3
            END IF
            COV(II+I) = CVDIAG
*
*        Compute Ith column of Cholesky factor.
*        Compute expected value for Ith integration variable and
*         scale Ith covariance matrix row and limits.
*
            IF ( CVDIAG .GT. 0 ) THEN
               IL = II + I
               DO L = I+1, ND
                  COV(IL+I) = COV(IL+I)/CVDIAG
                  IJ = II + I
                  DO J = I+1, L
                     COV(IL+J) = COV(IL+J) - COV(IL+I)*COV(IJ+I)
                     IJ = IJ + J
                  END DO
                  IL = IL + L
               END DO
*
*              Expected Y = -( density(b) - density(a) )/( b - a )
*
               IF ( DEMIN .GT. EPSI ) THEN
                  Y(I) = 0
                  IF ( INFI(I) .NE. 0 ) Y(I) =        MVTDNS( 0, AMIN )
                  IF ( INFI(I) .NE. 1 ) Y(I) = Y(I) - MVTDNS( 0, BMIN )
                  Y(I) = Y(I)/DEMIN
               ELSE
                  IF ( INFI(I) .EQ. 0 ) Y(I) = BMIN
                  IF ( INFI(I) .EQ. 1 ) Y(I) = AMIN
                  IF ( INFI(I) .EQ. 2 ) Y(I) = ( AMIN + BMIN )/2
               END IF
               IF(DOSCALE) THEN
                  DO J = 1, I
                     II = II + 1
                     COV(II) = COV(II)/CVDIAG
                  END DO
                  A(I) =  A(I)/CVDIAG
                  B(I) =  B(I)/CVDIAG
                  DL(I) = DL(I)/CVDIAG
               ELSE
                  II = II + I
               END IF
            ELSE
               IL = II + I
               DO L = I+1, ND
                  COV(IL+I) = 0
                  IL = IL + L
               END DO
*
*        If the covariance matrix diagonal entry is zero,
*         permute limits and rows, if necessary.
*
*
               DO J = I-1, 1, -1
                  IF ( ABS( COV(II+J) ) .GT. EPSI ) THEN
                      A(I) =  A(I)/COV(II+J)
                      B(I) =  B(I)/COV(II+J)
                     DL(I) = DL(I)/COV(II+J)
                     IF ( COV(II+J) .LT. 0 ) THEN
                        CALL MVSSWP( A(I), B(I) )
                        IF ( INFI(I) .NE. 2 ) INFI(I) = 1 - INFI(I)
                     END IF
                     DO L = 1, J
                        COV(II+L) = COV(II+L)/COV(II+J)
                     END DO
                     DO L = J+1, I-1
                        IF( COV((L-1)*L/2+J+1) .GT. 0 ) THEN
                           IJ = II
                           DO K = I-1, L, -1
                              DO M = 1, K
                                 CALL MVSSWP( COV(IJ-K+M), COV(IJ+M) )
                              END DO
                              CALL MVSSWP(  A(K),  A(K+1) )
                              CALL MVSSWP(  B(K),  B(K+1) )
                              CALL MVSSWP( DL(K), DL(K+1) )
                              M = INFI(K)
                              INFI(K) = INFI(K+1)
                              INFI(K+1) = M
                              IJ = IJ - K
                           END DO
                           GO TO 20
                        END IF
                     END DO
                     GO TO 20
                  END IF
                  COV(II+J) = 0
               END DO
 20            II = II + I
               Y(I) = 0
            END IF
         END DO
      ENDIF
      END
*
      DOUBLE PRECISION FUNCTION MVTDNS( NU, X )
      INTEGER NU, I
      DOUBLE PRECISION X, PROD, PI, SQTWPI
      PARAMETER (     PI = 3.141592653589793D0 )
      PARAMETER ( SQTWPI = 2.506628274631001D0 )
      MVTDNS = 0
      IF ( NU .GT. 0 ) THEN
         PROD = 1/SQRT( DBLE(NU) )
         DO I = NU - 2, 1, -2
            PROD = PROD*( I + 1 )/I
         END DO
         IF ( MOD( NU, 2 ) .EQ. 0 ) THEN
            PROD = PROD/2
         ELSE
            PROD = PROD/PI
         END IF
         MVTDNS = PROD/SQRT( 1 + X*X/NU )**( NU + 1 )
      ELSE
        IF ( ABS(X) .LT. 10 ) MVTDNS = EXP( -X*X/2 )/SQTWPI
      END IF
      END
*
      SUBROUTINE MVLIMS( A, B, INFIN, LOWER, UPPER )
      DOUBLE PRECISION A, B, LOWER, UPPER, MVPHI
      INTEGER INFIN
      LOWER = 0
      UPPER = 1
      IF ( INFIN .GE. 0 ) THEN
         IF ( INFIN .NE. 0 ) LOWER = MVPHI(A)
         IF ( INFIN .NE. 1 ) UPPER = MVPHI(B)
      ENDIF
      UPPER = MAX( UPPER, LOWER )
      END
*
      SUBROUTINE MVSSWP( X, Y )
      DOUBLE PRECISION X, Y, T
      T = X
      X = Y
      Y = T
      END
*
      SUBROUTINE MVSWAP( P, Q, A, B, D, INFIN, N, C, IDX )
*
*     Swaps rows and columns P and Q in situ, with P <= Q.
*
*     Benjamin Christoffersen added the IDX argument to keep track
*     of the original indices.
*
      DOUBLE PRECISION A(*), B(*), C(*), D(*)
      INTEGER INFIN(*), IDX(*), P, Q, N, I, J, II, JJ
      CALL MVSSWP( A(P), A(Q) )
      CALL MVSSWP( B(P), B(Q) )
      CALL MVSSWP( D(P), D(Q) )
      J = INFIN(P)
      INFIN(P) = INFIN(Q)
      INFIN(Q) = J
      J = IDX(P)
      IDX(P) = IDX(Q)
      IDX(Q) = J
      JJ = ( P*( P - 1 ) )/2
      II = ( Q*( Q - 1 ) )/2
      CALL MVSSWP( C(JJ+P), C(II+Q) )
      DO J = 1, P-1
         CALL MVSSWP( C(JJ+J), C(II+J) )
      END DO
      JJ = JJ + P
      DO I = P+1, Q-1
         CALL MVSSWP( C(JJ+P), C(II+I) )
         JJ = JJ + I
      END DO
      II = II + Q
      DO I = Q+1, N
         CALL MVSSWP( C(II+P), C(II+Q) )
         II = II + I
      END DO
      END
*
