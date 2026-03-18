************************************************************************
*
*     DGESDD from LAPACK 3.5.0:
*     ----------------------------------------------------------------
*
*     Authors:
*     ---------------------------------------
*     \author Univ. of Tennessee 
*     \author Univ. of California Berkeley 
*     \author Univ. of Colorado Denver 
*     \author NAG Ltd. 
*
*     \date November 2013
*
*     Contributors:
*     -------------
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*     Original function definition:
*     -----------------------------
*      SUBROUTINE dgesdd( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK,
*     $                   lwork, iwork, info )
*
*     Header:
*     ----------
* -- LAPACK computational routine (version 3.5.0) --
* -- LAPACK is a software package provided by Univ. of Tennessee,    --
* -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd.
*    November 2013
*     
************************************************************************
*
*     Modified version of DGESDD, named SLPSVD:
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
*     Removed a lot of features, since we will only ever call this
*     function with M = N, and will only ever require the left
*     singular vectors (not the right). Also calls SLPDOR instead of
*     DORMQR.
*
*     In addition, all frameworks regarding size decisions have been
*     removed, and the "optimal" size for our specific sub-case will 
*     always be provided
*
************************************************************************
*
      SUBROUTINE SLPSVD( M, N, A, LDA, S, VT, LDVT, WORK,
     $                   LWORK, IWORK, NLSV )

      INTEGER            LDA, LDVT, LWORK, M, N, NLSV
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), S( * ), VT( LDVT, * ), WORK( * )

      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )

      INTEGER            IE, IERR, ISCL, ITAUP, ITAUQ, IU, LDWRKU, 
     $                   MINMN, NWORK
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM

      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )

      EXTERNAL           DBDSDC, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA

      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           DLAMCH, DLANGE, ILAENV, LSAME

      INTRINSIC          INT, MAX, MIN, SQRT

*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF

*     Reduce to bidiagonal form without QR decomposition
      IE = 1
      ITAUQ = IE + N
      ITAUP = ITAUQ + N
      NWORK = ITAUP + N

*     Bidiagonalize A
      CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $             WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $             IERR )

      IU = NWORK

      LDWRKU = M
      NWORK = IU + LDWRKU*N
      CALL DLASET( 'F', M, N, ZERO, ZERO, WORK( IU ), LDWRKU )
      NWORK = IU + LDWRKU*N

*     Perform bidiagonal SVD
      CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ),
     $             LDWRKU, VT, LDVT, DUM, IDUM, WORK( NWORK ),
     $             IWORK, INFO )

*     ...  computing left singular vectors of bidiagonal matrix in WORK(IU) 
      CALL SLPDOR( 'L', 'N', M, N, N, A, LDA, WORK ( ITAUQ ),
     $             WORK ( IU ), LDWRKU, WORK ( NWORK ), NLSV )
*    Customize: only applies factors to C[, 1:K], since that's all we
*    need to pass back anyway

*     Copy left singular vectors of A from WORK(IU) to A
*     only bother copying the first K columns
      CALL DLACPY( 'F', M, NLSV, WORK( IU ), LDWRKU, A, LDA )

*     Undo scaling if necessary
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
      END IF

      RETURN
      END
