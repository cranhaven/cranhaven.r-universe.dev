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
      subroutine dgcoov ( x, y )
      implicit none
      double precision x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed here to be under the COOrdinates storage format.
*
      integer n, nz, nzmax
      parameter( nzmax = 600000 )
      integer ia(nzmax), ja(nzmax)
      double precision a(nzmax)
      common /RMAT/ a, ia, ja, nz, n
      integer i, j
 
      do j = 1,n
         y(j) = 0.0d0
      enddo
      do i = 1,nz
         y(ia(i)) = y(ia(i)) + a(i)*x(ja(i))
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine dgcrsv ( x, y )
      implicit none
      double precision x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed to be under the Compress Row Storage (CRS) format.
*
      integer n, nz, nzmax
      parameter( nzmax = 600000 )
      integer ia(nzmax), ja(nzmax)
      double precision a(nzmax)
      common /RMAT/ a, ia, ja, nz, n
      integer i, j

      do i = 1,n
         y(i) = 0.0d0
         do j = ia(i),ia(i+1)-1
            y(i) = y(i) + a(j)*x(ja(j))
         enddo
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine dgccsv( x, y )
      implicit none
      double precision x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed to be under the Compress Column Storage (CCS) format.
*
      integer n, nz, nzmax
      parameter( nzmax = 600000 )
      integer ia(nzmax), ja(nzmax)
      double precision a(nzmax)
      common /RMAT/ a, ia, ja, nz, n
      integer i, j

      do i = 1,n
         y(i) = 0.0d0
      enddo
      do j = 1,n
         do i = ja(j),ja(j+1)-1
            y(ia(i)) = y(ia(i)) + a(i)*x(j)
         enddo
      enddo
      end
*----------------------------------------------------------------------|

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
      subroutine zgcoov ( x, y )
      implicit none
      complex*16 x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed here to be under the COOrdinates storage format.
*
      integer n, nz, nzmax
      parameter( nzmax = 50000 )
      integer ia(nzmax), ja(nzmax)
      complex*16 a(nzmax)
      common /CMAT/ a, ia, ja, nz, n

      integer i, j
      complex*16 ZERO
      parameter( ZERO=(0.0d0,0.0d0) )
 
      do j = 1,n
         y(j) = ZERO
      enddo
      do i = 1,nz
         y(ia(i)) = y(ia(i)) + a(i)*x(ja(i))
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine zgcrsv ( x, y )
      implicit none
      complex*16 x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed to be under the Compress Row Storage (CRS) format.
*
      integer n, nz, nzmax
      parameter( nzmax = 50000 )
      integer ia(nzmax), ja(nzmax)
      complex*16 a(nzmax)
      common /CMAT/ a, ia, ja, nz, n

      integer i, j
      complex*16 ZERO
      parameter( ZERO=(0.0d0,0.0d0) )

      do i = 1,n
         y(i) = ZERO
         do j = ia(i),ia(i+1)-1
            y(i) = y(i) + a(j)*x(ja(j))
         enddo
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine zgccsv( x, y )
      implicit none
      complex*16 x(*), y(*)
*
*---  Computes y = A*x. A is passed via a fortran `common statement'.
*---  A is assumed to be under the Compress Column Storage (CCS) format.
*
      integer n, nz, nzmax
      parameter( nzmax = 50000 )
      integer ia(nzmax), ja(nzmax)
      complex*16 a(nzmax)
      common /CMAT/ a, ia, ja, nz, n

      integer i, j
      complex*16 ZERO
      parameter( ZERO=(0.0d0,0.0d0) )

      do i = 1,n
         y(i) = ZERO
      enddo
      do j = 1,n
         do i = ja(j),ja(j+1)-1
            y(ia(i)) = y(ia(i)) + a(i)*x(j)
         enddo
      enddo
      end
*----------------------------------------------------------------------|


*----------------------------------------------------------------------|
*
      subroutine dcmpac( n, nx, ix, ixx, xx, iwsp )
*--   DCMPAC compacts the array ix and sorts ixx and xx
*--   (This is a gateway routine for DGCNVR) ...
*----------------------------------------------------------------------|

      implicit none
      integer          n, nx, ix(nx), ixx(nx), iwsp(n)
      double precision xx(nx)
      integer          k
*
*---  sort ix and carry ixx and xx along ...
*
      call idsrt2( nx, ix, ixx, xx )
*
*---  adjust pointers ...
*
      do k = 1,n
         iwsp(k) = 0
      enddo
      do k = 1,nx
         iwsp(ix(k)) = iwsp(ix(k)) + 1
      enddo
      ix(n+1) = nx + 1
      do k = n,1,-1
         ix(k) = ix(k+1)-iwsp(k)
      enddo
* 
*---  sort ixx in increasing order and carry xx along ...
*
      do k = 1,n
         call idsrt1( iwsp(k), ixx(ix(k)), xx(ix(k)) )
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
*
      subroutine idsrt1( nx, ix, xx )

*---  IDSRT1: indirect sort -- sort ix and carry xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer          nx, ix(nx)
      double precision xx(nx)

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,L
      double precision TX, TTX, R

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
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine idsrt2( nx, ix, ixx, xx )

*---  IDSRT2: indirect sort: sort ix and carry ixx and xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer          nx, ix(nx), ixx(nx)
      double precision xx(nx)

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,JT,JJT,L
      double precision TX, TTX, R

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
*---  IT, JT, TX.
*
      IJ = I + IDINT( DBLE(J-I)*R )
      IT = IX(IJ)
      JT = IXX(IJ)
      TX = XX(IJ)
*
*---  If first element of array is greater than IT, interchange with IT.
*
      IF( IX(I).GT.IT ) THEN
         IX(IJ) = IX(I)
         IX(I)  = IT
         IT     = IX(IJ)
         IXX(IJ)= IXX(I)
         IXX(I) = JT
         JT     = IXX(IJ)
         XX(IJ) = XX(I)
         XX(I)  = TX
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
         IXX(IJ)= IXX(J)
         IXX(J) = JT
         JT     = IXX(IJ)
         XX(IJ) = XX(J)
         XX(J)  = TX
         TX     = XX(IJ)
*
*---  If first element of array is greater than IT, swap with IT.
*
         IF ( IX(I).GT.IT ) THEN
            IX(IJ) = IX(I)
            IX(I)  = IT
            IT     = IX(IJ)
            IXX(IJ)= IXX(I)
            IXX(I) = JT
            JT     = IXX(IJ)
            XX(IJ) = XX(I)
            XX(I)  = TX
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
         JJT   = IXX(L)
         IXX(L)= IXX(K)
         IXX(K)= JJT
         TTX   = XX(L)
         XX(L) = XX(K)
         XX(K) = TTX
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
      JT = IXX(I+1)
      TX =  XX(I+1)
      IF( IX(I).LE.IT ) GO TO 265
      K=I
 270  IX(K+1) = IX(K)
      IXX(K+1) = IXX(K)
      XX(K+1)  =  XX(K)
      K = K-1
      IF( IT.LT.IX(K) ) GO TO 270
      IX(K+1) = IT
      IXX(K+1) = JT
      XX(K+1)  = TX
      GO TO 265

 300  CONTINUE
      RETURN
      END
*----------------------------------------------------------------------|


*----------------------------------------------------------------------|
*
      subroutine zcmpac( n, nx, ix, ixx, xx, iwsp )
*--   ZCMPAC compacts the array ix and sorts ixx and xx
*--   (This is a gateway routine for ZGCNVR) ...
*----------------------------------------------------------------------|

      implicit none
      integer          n, nx, ix(nx), ixx(nx), iwsp(n)
      complex*16       xx(nx)
      integer          k
*
*---  sort ix and carry ixx and xx along ...
*
      call izsrt2( nx, ix, ixx, xx )
*
*---  adjust pointers ...
*
      do k = 1,n
         iwsp(k) = 0
      enddo
      do k = 1,nx
         iwsp(ix(k)) = iwsp(ix(k)) + 1
      enddo
      ix(n+1) = nx + 1
      do k = n,1,-1
         ix(k) = ix(k+1)-iwsp(k)
      enddo
* 
*---  sort ixx in increasing order and carry xx along ...
*
      do k = 1,n
         call izsrt1( iwsp(k), ixx(ix(k)), xx(ix(k)) )
      enddo
      END
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
*
      subroutine izsrt1( nx, ix, xx )

*---  IZSRT1: indirect sort -- sort ix and carry xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer          nx, ix(nx)
      complex*16       xx(nx)

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,L
      complex*16       TX, TTX
      double precision R

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
*----------------------------------------------------------------------|
*----------------------------------------------------------------------|
      subroutine izsrt2( nx, ix, ixx, xx )

*---  IZSRT2: indirect sort: sort ix and carry ixx and xx along
*---  adapted from a SLAP (Sparse Linear Algebra Package) code.
*----------------------------------------------------------------------|

      implicit none
      integer          nx, ix(nx), ixx(nx)
      complex*16       xx(nx)

      integer          M,I,J,K,IL(21),IU(21), IT,IIT,IJ,JT,JJT,L
      complex*16       TX, TTX
      double precision R

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
*---  IT, JT, TX.
*
      IJ = I + IDINT( DBLE(J-I)*R )
      IT = IX(IJ)
      JT = IXX(IJ)
      TX = XX(IJ)
*
*---  If first element of array is greater than IT, interchange with IT.
*
      IF( IX(I).GT.IT ) THEN
         IX(IJ) = IX(I)
         IX(I)  = IT
         IT     = IX(IJ)
         IXX(IJ)= IXX(I)
         IXX(I) = JT
         JT     = IXX(IJ)
         XX(IJ) = XX(I)
         XX(I)  = TX
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
         IXX(IJ)= IXX(J)
         IXX(J) = JT
         JT     = IXX(IJ)
         XX(IJ) = XX(J)
         XX(J)  = TX
         TX     = XX(IJ)
*
*---  If first element of array is greater than IT, swap with IT.
*
         IF ( IX(I).GT.IT ) THEN
            IX(IJ) = IX(I)
            IX(I)  = IT
            IT     = IX(IJ)
            IXX(IJ)= IXX(I)
            IXX(I) = JT
            JT     = IXX(IJ)
            XX(IJ) = XX(I)
            XX(I)  = TX
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
         JJT   = IXX(L)
         IXX(L)= IXX(K)
         IXX(K)= JJT
         TTX   = XX(L)
         XX(L) = XX(K)
         XX(K) = TTX
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
      JT = IXX(I+1)
      TX =  XX(I+1)
      IF( IX(I).LE.IT ) GO TO 265
      K=I
 270  IX(K+1) = IX(K)
      IXX(K+1) = IXX(K)
      XX(K+1)  =  XX(K)
      K = K-1
      IF( IT.LT.IX(K) ) GO TO 270
      IX(K+1) = IT
      IXX(K+1) = JT
      XX(K+1)  = TX
      GO TO 265

 300  CONTINUE
      RETURN
      END
*----------------------------------------------------------------------|
