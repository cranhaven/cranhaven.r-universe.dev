      SUBROUTINE FDS(K, NPTS, N, INFO, MAT, SPV)
C CALCULATES THE SPV.
C k = nr of model terms; npts = nr of points
C N = nr of design points; INFO = (X'X)^-1
C MAT = PTS AT WHICH SPV TO BE EVALUATED
C SPV = VECTOR OF SPV VALUES
      IMPLICIT NONE
      INTEGER K, NPTS, N, I, J, L
      DOUBLE PRECISION INFO, MAT, SPV
      DIMENSION INFO(K, K), MAT(NPTS, K), SPV(NPTS)
      DO 10 I = 1, NPTS
        DO 20 J = 1, K
          DO 30 L = 1, K
            SPV(I) = SPV(I) + N*MAT(I,J)*MAT(I,L)*INFO(J,L)
30        CONTINUE
20      CONTINUE
10    CONTINUE
      END
