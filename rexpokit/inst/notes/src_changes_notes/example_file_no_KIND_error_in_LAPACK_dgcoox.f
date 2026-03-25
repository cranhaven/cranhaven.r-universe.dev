* Copyright: See /inst/LAPACK_LICENSE.txt for 
* original FORTRAN code in /src.
*
* The FORTRAN lapack/blas code in rexpokit was 
* originally copied from the EXPOKIT package
* with permission of Roger Sidje (who is
* thus listed as coauthor on rexpokit).
*
* The FORTRAN has since had various minor 
* modifications to satisfy new checks as
* CRAN updates their FORTRAN, OSs, and
* R CMD check function.
* 

* 2023-10-28:
* Fix: 
* Version: 0.26.6.10
* Check: usage of KIND in Fortran files
* Result: WARN
*     Found the following files with non-portable usage of KIND:
*      itscale5.f
*      mataid.f
*      my_expokit.f
* 
* mataid.f
* c      complex(kind=8)
*       complex
*
*
* double precision
* replaced with:
* REAL(kind=selected_real_kind(15)) ::
*
* complex
* replaced with
* complex(kind=selected_real_kind(15)) :: 


      subroutine  zzcopy(n,zx,incx,zy,incy)
c
c     copies a vector, x, to a vector, y.
c     jack dongarra, linpack, 4/11/78.
c
      complex(kind=8) zx(1),zy(1)
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        zy(iy) = zx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
   20 do 30 i = 1,n
        zy(i) = zx(i)
   30 continue
      return
      end


*-------------------------------NOTE-----------------------------------*
*     This is an accessory to Expokit and it is not intended to be     *
*     complete. It is supplied primarily to ensure an unconstrained    *
*     distribution and portability of the package. The matrix-vector   *
*     multiplication routines supplied here fit the non symmetric      *
*     storage and for a symmetric matrix, the entire (not half) matrix *
*     is required.  If the sparsity pattern is known a priori, it is   *
*     recommended to use the most advantageous format and to devise    *
*     the most advantageous matrix-vector multiplication routine.      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 
      subroutine ixsrt1( nx, ix, xx )
c      subroutine ixsrt1( xx )

*---  IDSRT1: indirect sort -- sort ix and carry xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|
c      implicit none
c      integer          nx

      complex(kind=8) xx(1)
      integer nx,ix
c      integer, dimension(nx) :: ix

      do 10 i = 1,nx
        xx(ix) = xx(ix)
        ix = ix + 1
   10 continue


c ERROR:
c     REAL(kind=selected_real_kind(15)), dimension(nx) :: xx
c			complex xx
c			complex, dimension(nx) :: xx
c			double complex :: xx(nx)
c			REAL(KIND=selected_real_kind(p=15)), dimension(nx) :: xx

c Compile failure:
c			use iso_fortran_env, only: xx(nx) => real64
c			REAL xx(nx)
c			complex, parameter :: xx(nx) = selected_real_kind(33, 4931)
c			REAL xx(nx)

c			COMPLEX(KIND=selected_complex_kind(15,15)) :: xx(nx)

c     SAME ERROR
c			COMPLEX*16 :: xx(nx)
      
c      USE ISO_FORTRAN_ENV, xx => real64
c      COMPLEX(KIND=8) xx(nx)

c 300  CONTINUE
      RETURN
      END




*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
*
      subroutine iysrt1( nx, ix, xx )

*---  IDSRT1: indirect sort -- sort ix and carry xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer nx,ix(nx)
      complex(kind=8) xx(nx)
c      integer          nx, ix(nx)
c      REAL, dimension(nx) :: xx

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,L
      REAL TX, TTX, R

      if ( nx.le.1 ) return

c      implicit none
c      complex(kind=8) xx(1)
c      integer nx,ix
c      integer, dimension(nx) :: ix

c      integer        M,I,J,K,IL(21),IU(21),IT,IIT,IJ,L
c      complex        TX, TTX
c      REAL R

*     DOES THIS HELP??
c      do 10 i = 1,nx
c        xx(ix) = xx(ix)
c        ix = ix + 1
c   10 continue



c      if ( nx.le.1 ) return

*---  And now...Just a little black magic...
      M = 1
      I = 1
      J = NX
      R = .375
 210  IF( R.LE.0.5898437 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875
      ENDIF
 225  K = I
*
*---  Select a central element of the array and save it in location 
*---  IT, TX.
*
      IJ = I + IDINT( DBLE(J-I)*R )
      IT = IX(IJ)
      TX = XX(IJ)
*
*---  If first element of array is greater than IT, interchange with IT.
*
      IF( IX(I).GT.IT ) THEN
         IX(IJ) = IX(I)
         IX(I)  = IT
         IT     = IX(IJ)
         XX(IJ)  = XX(I)
         XX(I)   = TX
         TX     = XX(IJ)
      ENDIF
      L=J
*                           
*---  If last element of array is less than IT, swap with IT.
*
      IF( IX(J).LT.IT ) THEN
         IX(IJ) = IX(J)
         IX(J)  = IT
         IT     = IX(IJ)
         XX(IJ)  = XX(J)
         XX(J)   = TX
         TX     = XX(IJ)
*
*---  If first element of array is greater than IT, swap with IT.
*
         IF ( IX(I).GT.IT ) THEN
            IX(IJ) = IX(I)
            IX(I)  = IT
            IT     = IX(IJ)
            XX(IJ)  = XX(I)
            XX(I)   = TX
            TX     = XX(IJ)
         ENDIF
      ENDIF
*
*---  Find an element in the second half of the array which is 
*---  smaller than IT.
*
 240  L=L-1
      IF( IX(L).GT.IT ) GO TO 240
*
*---  Find an element in the first half of the array which is 
*---  greater than IT.
*
 245  K=K+1
      IF( IX(K).LT.IT ) GO TO 245
*
*---  Interchange these elements.
*
      IF( K.LE.L ) THEN
         IIT   = IX(L)
         IX(L) = IX(K)
         IX(K) = IIT
         TTX   = XX(L)
         XX(L)  = XX(K)
         XX(K)  = TTX
         GOTO 240
      ENDIF
*
*---  Save upper and lower subscripts of the array yet to be sorted.
*
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260
*
*---  Begin again on another portion of the unsorted array.
*
 255  M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
 260  IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
 265  I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IX(I+1)
      TX =  XX(I+1)
      IF( IX(I).LE.IT ) GO TO 265
      K=I
 270  IX(K+1) = IX(K)
      XX(K+1)  =  XX(K)
      K = K-1
      IF( IT.LT.IX(K) ) GO TO 270
      IX(K+1) = IT
      XX(K+1)  = TX
      GO TO 265

 300  CONTINUE
      RETURN
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|



*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
*
      subroutine iisrt1( nx, ix, xx )

*---  IDSRT1: indirect sort -- sort ix and carry xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer          nx, ix(nx)
      REAL xx(nx)

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,L
      REAL TX, TTX, R

      if ( nx.le.1 ) return

*---  And now...Just a little black magic...
      M = 1
      I = 1
      J = NX
      R = .375
 210  IF( R.LE.0.5898437 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875
      ENDIF
 225  K = I
*
*---  Select a central element of the array and save it in location 
*---  IT, TX.
*
      IJ = I + IDINT( DBLE(J-I)*R )
      IT = IX(IJ)
      TX = XX(IJ)
*
*---  If first element of array is greater than IT, interchange with IT.
*
      IF( IX(I).GT.IT ) THEN
         IX(IJ) = IX(I)
         IX(I)  = IT
         IT     = IX(IJ)
         XX(IJ)  = XX(I)
         XX(I)   = TX
         TX     = XX(IJ)
      ENDIF
      L=J
*                           
*---  If last element of array is less than IT, swap with IT.
*
      IF( IX(J).LT.IT ) THEN
         IX(IJ) = IX(J)
         IX(J)  = IT
         IT     = IX(IJ)
         XX(IJ)  = XX(J)
         XX(J)   = TX
         TX     = XX(IJ)
*
*---  If first element of array is greater than IT, swap with IT.
*
         IF ( IX(I).GT.IT ) THEN
            IX(IJ) = IX(I)
            IX(I)  = IT
            IT     = IX(IJ)
            XX(IJ)  = XX(I)
            XX(I)   = TX
            TX     = XX(IJ)
         ENDIF
      ENDIF
*
*---  Find an element in the second half of the array which is 
*---  smaller than IT.
*
 240  L=L-1
      IF( IX(L).GT.IT ) GO TO 240
*
*---  Find an element in the first half of the array which is 
*---  greater than IT.
*
 245  K=K+1
      IF( IX(K).LT.IT ) GO TO 245
*
*---  Interchange these elements.
*
      IF( K.LE.L ) THEN
         IIT   = IX(L)
         IX(L) = IX(K)
         IX(K) = IIT
         TTX   = XX(L)
         XX(L)  = XX(K)
         XX(K)  = TTX
         GOTO 240
      ENDIF
*
*---  Save upper and lower subscripts of the array yet to be sorted.
*
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260
*
*---  Begin again on another portion of the unsorted array.
*
 255  M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
 260  IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
 265  I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IX(I+1)
      TX =  XX(I+1)
      IF( IX(I).LE.IT ) GO TO 265
      K=I
 270  IX(K+1) = IX(K)
      XX(K+1)  =  XX(K)
      K = K-1
      IF( IT.LT.IX(K) ) GO TO 270
      IX(K+1) = IT
      XX(K+1)  = TX
      GO TO 265

 300  CONTINUE
      RETURN
      END

