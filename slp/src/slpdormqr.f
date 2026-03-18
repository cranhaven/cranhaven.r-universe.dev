************************************************************************
*
*     DORMQR from LAPACK 3.5.0 (not updated since 3.4.0):
*     ---------------------------------------------------
*
*     Authors:
*     ---------------------------------------
*     \author Univ. of Tennessee 
*     \author Univ. of California Berkeley 
*     \author Univ. of Colorado Denver 
*     \author NAG Ltd. 
*
*     \date November 2011
*
*     Original function definition:
*     -----------------------------
*      SUBROUTINE dormqr( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
*     $                   work, lwork, info )
*
*     Header:
*     -------------------
* -- LAPACK computational routine (version 3.4.0) --
* -- LAPACK is a software package provided by Univ. of Tennessee,    
* -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd.
*    November 2011
*
************************************************************************
*
*     Modified version of DORMQR, named SLPDOR:
*     -----------------------------------------
*
*     Author/Contributor:
*     ---------------------------------
*     \author Wesley Burr
*
*     \date July 2014
*
*     Modifications:
*     ------------------
*
*     DORMQR overwrites the general real M-by-N matrix C with Q * C
*     where Q is a real orthogonal matrix defined as the product of k
*     elementary reflectors
*
*         Q = H(1) H(2) . . . H(k)
*
*     as returned by DGEQRF. Q is of order M if SIDE = 'L'
*
*     This modified version adds an additional parameter NLSV, and only
*     computes the first NLSV left singular vectors, rather than the
*     entire M = N set. This provides significant speed improvements.
*
*     NOTE: the svd() call in R has parameters which appear (in the
*     documentation) to do the same thing, but they act _after_ the
*     computation, and truncate the returned vectors. This is obviously
*     not the point of reducing the number of vectors, which is why this
*     routine is necessary.
*
************************************************************************
*
      SUBROUTINE SLPDOR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, NLSV )

      CHARACTER          SIDE, TRANS
      INTEGER            K, LDA, LDC, M, N, NLSV
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )

      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )

      INTEGER            I, I1, I2, I3, IB, IC, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NQ, NW
      DOUBLE PRECISION   T( LDT, NBMAX )

      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV

      EXTERNAL           DLARFB, DLARFT, DORM2R, XERBLA

      INTRINSIC          MAX, MIN

      NQ = M
      NW = N

*
*     Determine the block size.  NB may be at most NBMAX, where NBMAX
*     is used to define the local array T.
*
      NB = MIN( NBMAX, ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N, K,
     $          -1 ) )

      NBMIN = 2
      LDWORK = NW
      IWS = NW*NB

      I1 = ( ( K-1 ) / NB )*NB + 1
      I2 = 1
      I3 = -NB

      JC = 1

      DO 10 I = I1, I2, I3
         IB = MIN( NB, K-I+1 )
*
*        Form the triangular factor of the block reflector
*        H = H(i) H(i+1) . . . H(i+ib-1)
*
         CALL DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                LDA, TAU( I ), T, LDT )

*        H or H**T is applied to C(i:m,1:n)
         MI = M - I + 1
         IC = I

*     Apply H or H**T
*     SIDE = 'L', TRANS = 'N'
      CALL DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NLSV,
     $             IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $             WORK, LDWORK )

   10 CONTINUE

      RETURN
      END
