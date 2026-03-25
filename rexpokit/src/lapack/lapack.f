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


* 2019-07-01
* 
* zswap  to zswapx  when there are 5 arguments
* zswapx to zswapxy when there are 6 arguments
* 
* 

*     2018-09-26 NJM edits: 
*          changed REALPART to REAL
*          changed IMAGPART to AIMAG
*          changed DCONJG to CONJG
*          ...throughout
*          (I guess these are gfortran GNU extensions; cause 
*           problems on flang compiler, according to
*           email from Brian Ripley)
*

*     This is a lightweight substitute to the external LAPACK routines 
*     used by EXPOKIT. It is supplied to ensure that EXPOKIT is 
*     self-contained and can still run if LAPACK is not yet installed
*     in your environement.
*----------------------------------------------------------------------|
      subroutine ZGESV( N, M, A,LDA, IPIV, B,LDB, IFLAG )
      integer N, M, LDA, LDB, IPIV(N), IFLAG
      complex(kind=8) A(LDA,N), B(LDB,M)
      call ZGEFA( A,LDA, N, IPIV, IFLAG )
*      if ( IFLAG.ne.0 ) stop "Error in ZGESV (LU factorisation)"
      do j = 1,M
         call ZGESL( A,LDA, N, IPIV,B(1,j), 0 )
      enddo
      end



*----------------------------------------------------------------------|
      subroutine ZHESV(UPLO, N,M, A,LDA, IPIV, B,LDB, WRK,LWRK, IFLAG )
      character UPLO*1
      integer N, M, LDA, LDB, LWRK, IFLAG, IPIV(N)
      complex(kind=8) A(LDA,N), B(LDB,M), WRK(LWRK)
      call ZHIFA( A,LDA, N, IPIV, IFLAG )
*      if ( IFLAG.ne.0 ) stop "Error in ZHESV (LDL' factorisation)"
      do j = 1,M
         call ZHISL( A,LDA, N, IPIV,B(1,j) )
      enddo

c     FIX for Warning: Unused dummy argument 'uplo'
      if (LEN(UPLO) > 0) then
        continue
      end if

c     FIX for Warning: Unused dummy argument 'wrk'
      if (REAL(WRK(LWRK)) > 0) then
        continue
      end if

      end



*----------------------------------------------------------------------|
      subroutine ZSYSV(UPLO, N,M, A,LDA, IPIV, B,LDB, WRK,LWRK, IFLAG )
      character UPLO*1
      integer N, M, LDA, LDB, LWRK, IFLAG, IPIV(N)
      complex(kind=8) A(LDA,N), B(LDB,M), WRK(LWRK)
      call ZSIFA( A,LDA, N, IPIV, IFLAG )
*      if ( IFLAG.ne.0 ) stop "Error in ZSYSV (LDL' factorisation)"
      do j = 1,M
         call ZSISL( A,LDA, N, IPIV, B(1,j) )
      enddo

c     FIX for Warning: Unused dummy argument 'uplo'
      if (LEN(UPLO) > 0) then
        continue
      end if

c     FIX for Warning: Unused dummy argument 'wrk'
      if (REAL(WRK(LWRK)) > 0) then
        continue
      end if

      end



*----------------------------------------------------------------------|
      subroutine zgefa(a,lda,n,ipvt,info)
c     2019-07-02_NJM:
c     integer lda,n,ipvt(1),info
      integer lda,n,ipvt(1),info,tempn
      complex(kind=8) a(lda,1)
c
c     zgefa factors a complex(kind=8) matrix by gaussian elimination.
c
c     zgefa is usually called by zgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for zgeco) = (1 + 9/n)*(time for zgefa) .
c
c     on entry
c
c        a       complex(kind=8)(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that zgesl or zgedi will divide by zero
c                     if called.  use  rcond  in zgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas zswapx,zscal,izamax
c     fortran dabs
c
c     internal variables
c
      complex(kind=8) t
      integer izamax,j,k,kp1,l,nm1
c
c      complex(kind=8) zdum
      double precision cabs1
      double precision pta, ptb

c      double precision dreal,dimag
c      complex(kind=8) zdumr,zdumi
c     dreal(zdumr) = zdumr
c     dimag(zdumi) = (0.0d0,-1.0d0)*zdumi

c     Statement function:
c      cabs1(zdum) = dabs(REALPART(zdum)) + dabs(IMAGPART(zdum))
c     FIX:
c      double precision cabs1
c      double precision pta, ptb
c      pta = REALPART(zdum)
c      ptb = IMAGPART(zdum)
c      ((dabs(pta)+dabs(ptb))


c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = izamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c

c        FIX:
         pta = REAL(a(l,k))
         ptb = AIMAG(a(l,k))
         cabs1 = dabs(pta)+dabs(ptb)
c         if (cabs1(a(l,k)) .eq. 0.0d0) go to 40
         if (cabs1 .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -(1.0d0,0.0d0)/a(k,k)
            call zscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
            do 30 j = kp1, n
c              2019-07-02_NJM:
c              t = a(l,j)
               tempn = INT(a(l,j))
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
c              2019-07-02_NJM:
              call zswapy(n-k,t,a(k+1,k),1,a(k+1,j),1)
c               call zswapy(n-k,tempn,a(k+1,k),1,a(k+1,j),1)
c               2020-07-03_
c              call zswapy(n-k,t,a(k+1,k),1,a(k+1,j),1)
c               call zswapy(n-k,tempn,a(k+1,k),1,a(k+1,j),1)

   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n

c     FIX:
      pta = REAL(a(n,n))
      ptb = AIMAG(a(n,n))
      cabs1 = dabs(pta)+dabs(ptb)

c     if (cabs1(a(n,n)) .eq. 0.0d0) info = n
      if (cabs1 .eq. 0.0d0) info = n
      return
      end



*----------------------------------------------------------------------|
      subroutine zgesl(a,lda,n,ipvt,b,job)
      integer lda,n,ipvt(1),job
      complex(kind=8) a(lda,1),b(1)
c
c     zgesl solves the complex(kind=8) system
c     a * x = b  or  ctrans(a) * x = b
c     using the factors computed by zgeco or zgefa.
c
c     on entry
c
c        a       complex(kind=8)(lda, n)
c                the output from zgeco or zgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from zgeco or zgefa.
c
c        b       complex(kind=8)(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  ctrans(a)*x = b  where
c                            ctrans(a)  is the conjugate transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if zgeco has set rcond .gt. 0.0
c        or zgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call zgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call zgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas zswapx,zdotc
c     fortran dconjg
c
c     internal variables
c
      complex(kind=8) zdotc,t
c     2019-07-02_NJM:
c      integer k,kb,l,nm1
      integer k,kb,l,nm1
c      double precision dreal,dimag
c      complex(kind=8) zdumr,zdumi
c      dreal(zdumr) = zdumr
c      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
c           2019-07-02_NJM:
           call zswapy(n-k,t,a(k+1,k),1,b(k+1),1)
c            call zswapy(n-k,tempt,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call zswapy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  ctrans(a) * x = b
c        first solve  ctrans(u)*y = b
c
         do 60 k = 1, n
            t = zdotc(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/conjg(a(k,k))
   60    continue
c
c        now solve ctrans(l)*x = y
c
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + zdotc(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end



*----------------------------------------------------------------------|
      subroutine zhifa(a,lda,n,kpvt,info)
      integer lda,n,kpvt(1),info
      complex(kind=8) a(lda,1)
c
c     zhifa factors a complex(kind=8) hermitian matrix by elimination
c     with symmetric pivoting.
c
c     to solve  a*x = b , follow zhifa by zhisl.
c     to compute  inverse(a)*c , follow zhifa by zhisl.
c     to compute  determinant(a) , follow zhifa by zhidi.
c     to compute  inertia(a) , follow zhifa by zhidi.
c     to compute  inverse(a) , follow zhifa by zhidi.
c
c     on entry
c
c        a       complex(kind=8)(lda,n)
c                the hermitian matrix to be factored.
c                only the diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       a block diagonal matrix and the multipliers which
c                were used to obtain it.
c                the factorization can be written  a = u*d*ctrans(u)
c                where  u  is a product of permutation and unit
c                upper triangular matrices , ctrans(u) is the
c                conjugate transpose of  u , and  d  is block diagonal
c                with 1 by 1 and 2 by 2 blocks.
c
c        kpvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if the k-th pivot block is singular. this is
c                     not an error condition for this subroutine,
c                     but it does indicate that zhisl or zhidi may
c                     divide by zero if called.
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas zswapx,zswapx,izamax
c     fortran dabs,dmax1,dcmplx,conjg,dsqrt
c
c     internal variables
c

c     2019-07-02_NJM:
c     complex(kind=8) ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
c     double precision absakk,alpha,colmax,rowmax
c     integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,izamax

      complex(kind=8) ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
      double precision absakk,alpha,colmax,rowmax
      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,izamax
      logical swap
c
c     complex(kind=8) zdum
      double precision cabs1
c     FIX:
      double precision pta, ptb
c      double precision dreal,dimag
c      complex(kind=8) zdumr,zdumi
c      dreal(zdumr) = zdumr
c      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi

c FIX:
c      cabs1(zdum) = dabs(REALPART(zdum)) + dabs(IMAGPART(zdum))
c
c     initialize
c
c     alpha is used in choosing pivot block size.
      alpha = (1.0d0 + dsqrt(17.0d0))/8.0d0
c
      info = 0
c
c     main loop on k, which goes from n to 1.
c
      k = n
   10 continue
c
c        leave the loop if k=0 or k=1.
c
c     ...exit
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1
            
c           FIX:
            pta = REAL(a(1,1))
            ptb = AIMAG(a(1,1))
            cabs1 = dabs(pta)+dabs(ptb)
           
c           if (cabs1(a(1,1)) .eq. 0.0d0) info = 1
            if (cabs1 .eq. 0.0d0) info = 1
c     ......exit
            go to 200
   20    continue
c
c        this section of code determines the kind of
c        elimination to be performed.  when it is completed,
c        kstep will be set to the size of the pivot block, and
c        swap will be set to .true. if an interchange is
c        required.
c
         km1 = k - 1

c        FIX:
         pta = REAL(a(k,k))
         ptb = AIMAG(a(k,k))
         cabs1 = dabs(pta)+dabs(ptb)

c        absakk = cabs1(a(k,k))
         absakk = cabs1



c
c        determine the largest off-diagonal element in
c        column k.
c
         imax = izamax(k-1,a(1,k),1)

c        FIX:
         pta = REAL(a(imax,k))
         ptb = AIMAG(a(imax,k))
         cabs1 = dabs(pta)+dabs(ptb)

c        colmax = cabs1(a(imax,k))
         colmax = cabs1
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
c
c           determine the largest off-diagonal element in
c           row imax.
c
            rowmax = 0.0d0
            imaxp1 = imax + 1
            do 40 j = imaxp1, k

c              FIX:
               pta = REAL(a(imax,j))
               ptb = AIMAG(a(imax,j))
               cabs1 = dabs(pta)+dabs(ptb)

c              rowmax = dmax1(rowmax,cabs1(a(imax,j)))
               rowmax = dmax1(rowmax,cabs1)
   40       continue
            if (imax .eq. 1) go to 50
               jmax = izamax(imax-1,a(1,imax),1)

c              FIX:
               pta = REAL(a(jmax,imax))
               ptb = AIMAG(a(jmax,imax))
               cabs1 = dabs(pta)+dabs(ptb)

c              rowmax = dmax1(rowmax,cabs1(a(jmax,imax)))
               rowmax = dmax1(rowmax,cabs1)
   50       continue

c           FIX:
            pta = REAL(a(imax,imax))
            ptb = AIMAG(a(imax,imax))
            cabs1 = dabs(pta)+dabs(ptb)

c           if (cabs1(a(imax,imax)) .lt. alpha*rowmax) go to 60
            if (cabs1 .lt. alpha*rowmax) go to 60
               kstep = 1
               swap = .true.
            go to 80
   60       continue
            if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
               kstep = 1
               swap = .false.
            go to 80
   70       continue
               kstep = 2
               swap = imax .ne. km1
   80       continue
   90    continue
         if (dmax1(absakk,colmax) .ne. 0.0d0) go to 100
c
c           column k is zero.  set info and iterate the loop.
c
            kpvt(k) = k
            info = k
         go to 190
  100    continue
         if (kstep .eq. 2) go to 140
c
c           1 x 1 pivot block.
c
            if (.not.swap) go to 120
c
c              perform an interchange.
c
               call zswapx(imax,a(1,imax),1,a(1,k),1)
               do 110 jj = imax, k
                  j = k + imax - jj
                  t = conjg(a(j,k))
                  a(j,k) = conjg(a(imax,j))
                  a(imax,j) = t
  110          continue
  120       continue
c
c           perform the elimination.
c
            do 130 jj = 1, km1
               j = k - jj
               mulk = -a(j,k)/a(k,k)
c              2019-07-02_NJM:
              t = conjg(mulk)
c               tempt = INT(conjg(mulk))
c               call zswapy(j,tempt,a(1,k),1,a(1,j),1)
               call zswapy(j,t,a(1,k),1,a(1,j),1)
               a(j,j) = dcmplx(REAL(a(j,j)),0.0d0)
               a(j,k) = mulk
  130       continue
c
c           set the pivot array.
c
            kpvt(k) = k
            if (swap) kpvt(k) = imax
         go to 190
  140    continue
c
c           2 x 2 pivot block.
c
            if (.not.swap) go to 160
c
c              perform an interchange.
c
               call zswapx(imax,a(1,imax),1,a(1,k-1),1)
               do 150 jj = imax, km1
                  j = km1 + imax - jj
                  t = conjg(a(j,k-1))
                  a(j,k-1) = conjg(a(imax,j))
                  a(imax,j) = t
  150          continue
               t = a(k-1,k)
               a(k-1,k) = a(imax,k)
               a(imax,k) = t
  160       continue
c
c           perform the elimination.
c
            km2 = k - 2
            if (km2 .eq. 0) go to 180
               ak = a(k,k)/a(k-1,k)
               akm1 = a(k-1,k-1)/conjg(a(k-1,k))
               denom = 1.0d0 - ak*akm1
               do 170 jj = 1, km2
                  j = km1 - jj
                  bk = a(j,k)/a(k-1,k)
                  bkm1 = a(j,k-1)/conjg(a(k-1,k))
                  mulk = (akm1*bk - bkm1)/denom
                  mulkm1 = (ak*bkm1 - bk)/denom
                  t = conjg(mulk)
                  call zswapy(j,t,a(1,k),1,a(1,j),1)
                  t = conjg(mulkm1)
                  call zswapy(j,t,a(1,k-1),1,a(1,j),1)
                  a(j,k) = mulk
                  a(j,k-1) = mulkm1
                  a(j,j) = dcmplx(REAL(a(j,j)),0.0d0)
  170          continue
  180       continue
c
c           set the pivot array.
c
            kpvt(k) = 1 - k
            if (swap) kpvt(k) = -imax
            kpvt(k-1) = kpvt(k)
  190    continue
         k = k - kstep
      go to 10
  200 continue
      return
      end



*----------------------------------------------------------------------|
      subroutine zhisl(a,lda,n,kpvt,b)
c      2019-07-02_NJM:
c      integer lda,n,kpvt(1)
      integer lda,n,kpvt(1)
      complex(kind=8) a(lda,1),b(1)
c
c     zhisl solves the complex(kind=8) hermitian system
c     a * x = b
c     using the factors computed by zhifa.
c
c     on entry
c
c        a       complex(kind=8)(lda,n)
c                the output from zhifa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from zhifa.
c
c        b       complex(kind=8)(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  zhico  has set rcond .eq. 0.0
c        or  zhifa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call zhifa(a,lda,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call zhisl(a,lda,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas zswapx,zdotc
c     fortran dconjg,iabs
c
c     internal variables.
c
      complex(kind=8) ak,akm1,bk,bkm1,zdotc,denom,temp
      integer k,kp
c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
   20          continue
c
c              apply the transformation.
c
c              2019-07-02_NJM:
c              call zswapy(k-1,b(k),a(1,k),1,b(1),1)
c               t = INT(b(k))
c               call zswapy(k-1,t,a(1,k),1,b(1),1)
               call zswapy(k-1,b(k),a(1,k),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70
   40    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call zswapy(k-2,b(k),a(1,k),1,b(1),1)
               call zswapy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            ak = a(k,k)/conjg(a(k-1,k))
            akm1 = a(k-1,k-1)/a(k-1,k)
            bk = b(k)/conjg(a(k-1,k))
            bkm1 = b(k-1)/a(k-1,k)
            denom = ak*akm1 - 1.0d0
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110
c
c              apply the transformation.
c
               b(k) = b(k) + zdotc(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  100          continue
  110       continue
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + zdotc(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + zdotc(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            k = k + 2
  150    continue
      go to 90
  160 continue
      return
      end



*----------------------------------------------------------------------|
      subroutine zsifa(a,lda,n,kpvt,info)
      integer lda,n,kpvt(1),info
      complex(kind=8) a(lda,1)
c
c     zsifa factors a complex(kind=8) symmetric matrix by elimination
c     with symmetric pivoting.
c
c     to solve  a*x = b , follow zsifa by zsisl.
c     to compute  inverse(a)*c , follow zsifa by zsisl.
c     to compute  determinant(a) , follow zsifa by zsidi.
c     to compute  inverse(a) , follow zsifa by zsidi.
c
c     on entry
c
c        a       complex(kind=8)(lda,n)
c                the symmetric matrix to be factored.
c                only the diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       a block diagonal matrix and the multipliers which
c                were used to obtain it.
c                the factorization can be written  a = u*d*trans(u)
c                where  u  is a product of permutation and unit
c                upper triangular matrices , trans(u) is the
c                transpose of  u , and  d  is block diagonal
c                with 1 by 1 and 2 by 2 blocks.
c
c        kpvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if the k-th pivot block is singular. this is
c                     not an error condition for this subroutine,
c                     but it does indicate that zsisl or zsidi may
c                     divide by zero if called.
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas zswapx,zswapx,izamax
c     fortran dabs,dmax1,dsqrt
c
c     internal variables
c
c     2019-07-02_NJM:
c      complex(kind=8) ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
c      double precision absakk,alpha,colmax,rowmax
c      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,izamax
      complex(kind=8) ak,akm1,bk,bkm1,denom,mulk,mulkm1
      double precision absakk,alpha,colmax,rowmax
      integer imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,izamax,t
      logical swap
c
c      complex(kind=8) zdum
      double precision cabs1
      double precision pta
      double precision ptb

c      double precision dreal,dimag
c      complex(kind=8) zdumr,zdumi

c      dreal(zdumr) = zdumr
c      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
c      cabs1(zdum) = dabs(REALPART(zdum)) + dabs(IMAGPART(zdum))
c
c     initialize
c
c     alpha is used in choosing pivot block size.
      alpha = (1.0d0 + dsqrt(17.0d0))/8.0d0
c
      info = 0
c
c     main loop on k, which goes from n to 1.
c
      k = n
   10 continue
c
c        leave the loop if k=0 or k=1.
c
c     ...exit
         if (k .eq. 0) go to 200
         if (k .gt. 1) go to 20
            kpvt(1) = 1

c           FIX:
c           if (cabs1(a(1,1)) .eq. 0.0d0) info = 1
      pta = REAL(a(1,1))
      ptb = AIMAG(a(1,1))
      cabs1 = dabs(pta)+dabs(ptb)
      if (cabs1 .eq. 0.0d0) info=1


c     ......exit
            go to 200
   20    continue
c
c        this section of code determines the kind of
c        elimination to be performed.  when it is completed,
c        kstep will be set to the size of the pivot block, and
c        swap will be set to .true. if an interchange is
c        required.
c
         km1 = k - 1

c        FIX:
         pta = REAL(a(k,k))
         ptb = AIMAG(a(k,k))
         cabs1 = dabs(pta)+dabs(ptb)

c        absakk = cabs1(a(k,k))
         absakk = cabs1
c
c        determine the largest off-diagonal element in
c        column k.
c
         imax = izamax(k-1,a(1,k),1)


c        FIX:
         pta = REAL(a(imax,k))
         ptb = AIMAG(a(imax,k))
         cabs1 = dabs(pta)+dabs(ptb)

c        colmax = cabs1(a(imax,k))
         colmax = cabs1
         if (absakk .lt. alpha*colmax) go to 30
            kstep = 1
            swap = .false.
         go to 90
   30    continue
c
c           determine the largest off-diagonal element in
c           row imax.
c
            rowmax = 0.0d0
            imaxp1 = imax + 1
            do 40 j = imaxp1, k
c              FIX:
               pta = REAL(a(imax,j))
               ptb = AIMAG(a(imax,j))
               cabs1 = dabs(pta)+dabs(ptb)
c              rowmax = dmax1(rowmax,cabs1(a(imax,j)))
               rowmax = dmax1(rowmax,cabs1)
   40       continue
            if (imax .eq. 1) go to 50
               jmax = izamax(imax-1,a(1,imax),1)
c              FIX:
               pta = REAL(a(jmax,imax))
               ptb = AIMAG(a(jmax,imax))
               cabs1 = dabs(pta)+dabs(ptb)
c              rowmax = dmax1(rowmax,cabs1(a(jmax,imax)))
               rowmax = dmax1(rowmax,cabs1)
   50       continue
c              FIX:
            pta = REAL(a(imax,imax))
            ptb = AIMAG(a(imax,imax))
            cabs1 = dabs(pta)+dabs(ptb)
c           if (cabs1(a(imax,imax)) .lt. alpha*rowmax) go to 60
            if (cabs1 .lt. alpha*rowmax) go to 60
               kstep = 1
               swap = .true.
            go to 80
   60       continue
            if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
               kstep = 1
               swap = .false.
            go to 80
   70       continue
               kstep = 2
               swap = imax .ne. km1
   80       continue
   90    continue
         if (dmax1(absakk,colmax) .ne. 0.0d0) go to 100
c
c           column k is zero.  set info and iterate the loop.
c
            kpvt(k) = k
            info = k
         go to 190
  100    continue
         if (kstep .eq. 2) go to 140
c
c           1 x 1 pivot block.
c
            if (.not.swap) go to 120
c
c              perform an interchange.
c
               call zswapx(imax,a(1,imax),1,a(1,k),1)
               do 110 jj = imax, k
                  j = k + imax - jj
c                  2019-07-02_NJM:
c                  t = a(j,k)
                  t = INT(a(j,k))
                  a(j,k) = a(imax,j)
                  a(imax,j) = t
  110          continue
  120       continue
c
c           perform the elimination.
c
            do 130 jj = 1, km1
               j = k - jj
               mulk = -a(j,k)/a(k,k)
c               t = INT(mulk)
c               call zswapy(j,t,a(1,k),1,a(1,j),1)
               call zswapy(j,mulk,a(1,k),1,a(1,j),1)
               a(j,k) = mulk
  130       continue
c
c           set the pivot array.
c
            kpvt(k) = k
            if (swap) kpvt(k) = imax
         go to 190
  140    continue
c
c           2 x 2 pivot block.
c
            if (.not.swap) go to 160
c
c              perform an interchange.
c
               call zswapx(imax,a(1,imax),1,a(1,k-1),1)
               do 150 jj = imax, km1
                  j = km1 + imax - jj
c                  2019-07-02_NJM:
c                  t = a(j,k-1)
                  t = INT(a(j,k-1))
                  a(j,k-1) = a(imax,j)
                  a(imax,j) = t
  150          continue
c                  2019-07-02_NJM:
c                  t = a(k-1,k)
               t = INT(a(k-1,k))
               a(k-1,k) = a(imax,k)
               a(imax,k) = t
  160       continue
c
c           perform the elimination.
c
            km2 = k - 2
            if (km2 .eq. 0) go to 180
               ak = a(k,k)/a(k-1,k)
               akm1 = a(k-1,k-1)/a(k-1,k)
               denom = 1.0d0 - ak*akm1
               do 170 jj = 1, km2
                  j = km1 - jj
                  bk = a(j,k)/a(k-1,k)
                  bkm1 = a(j,k-1)/a(k-1,k)
                  mulk = (akm1*bk - bkm1)/denom
                  mulkm1 = (ak*bkm1 - bk)/denom
c                 2019-07-02_NJM:
c                 t = mulk
c                  t = INT(mulk)
c                  call zswapy(j,t,a(1,k),1,a(1,j),1)
                  call zswapy(j,mulk,a(1,k),1,a(1,j),1)
c                 2019-07-02_NJM:
c                 t = mulkm1
c                  t = INT(mulkm1)
c                  call zswapy(j,t,a(1,k-1),1,a(1,j),1)
                  call zswapy(j,mulkm1,a(1,k-1),1,a(1,j),1)
                  a(j,k) = mulk
                  a(j,k-1) = mulkm1
  170          continue
  180       continue
c
c           set the pivot array.
c
            kpvt(k) = 1 - k
            if (swap) kpvt(k) = -imax
            kpvt(k-1) = kpvt(k)
  190    continue
         k = k - kstep
      go to 10
  200 continue
      return
      end



*----------------------------------------------------------------------|
      subroutine zsisl(a,lda,n,kpvt,b)
c     2019-07-02_NJM:
c     integer lda,n,kpvt(1)
      integer lda,n,kpvt(1)
      complex(kind=8) a(lda,1),b(1)
c
c     zsisl solves the complex(kind=8) symmetric system
c     a * x = b
c     using the factors computed by zsifa.
c
c     on entry
c
c        a       complex(kind=8)(lda,n)
c                the output from zsifa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        kpvt    integer(n)
c                the pivot vector from zsifa.
c
c        b       complex(kind=8)(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero may occur if  zsico  has set rcond .eq. 0.0
c        or  zsifa  has set info .ne. 0  .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call zsifa(a,lda,n,kpvt,info)
c           if (info .ne. 0) go to ...
c           do 10 j = 1, p
c              call zsisl(a,lda,n,kpvt,c(1,j))
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     james bunch, univ. calif. san diego, argonne nat. lab.
c
c     subroutines and functions
c
c     blas zswapx,zdotu
c     fortran iabs
c
c     internal variables.
c      2020-07-03
c      complex(kind=8) ak,akm1,bk,bkm1,zdotu,denom,temp
      complex(kind=8) ak,akm1,bk,bkm1,zdotu,denom,temp,tempm
      integer k,kp

c 2023-10-31: fixing...
c lapack/lapack.f:1231:72: warning: type of 'zswapy' 
c does not match original declaration [-Wlto-type-mismatch]
c  1231 |               call zswapy(k-1,tempx,a(1,k),1,b(1),1)
c       |                                                                        ^
c lapack/blas_mod.f:961:24: note: 'zswapy' was previously declared here
c   961 |       subroutine  zswapy (n,m,zx,incx,zy,incy)
c 
      integer tempkm1
c      complex(kind=8) tempzx
      complex(kind=8) tempzy
c
c     loop backward applying the transformations and
c     d inverse to b.
c
      k = n
   10 if (k .eq. 0) go to 80
         if (kpvt(k) .lt. 0) go to 40
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 30
               kp = kpvt(k)
               if (kp .eq. k) go to 20
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
   20          continue
c
c              apply the transformation.
c
c              2020-07-03_NJM:
c              tempx = INT(b(k)
c  2023-11-22:
              tempm = b(k)
c              call zswapy(k-1,b(k),a(1,k),1,b(1),1)

c 2023-10-31: fixing...
c lapack/lapack.f:1231:72: warning: type of 'zswapy' 
c does not match original declaration [-Wlto-type-mismatch]
c  1231 |               call zswapy(k-1,tempx,a(1,k),1,b(1),1)
c       |                                                                        ^
c lapack/blas_mod.f:961:24: note: 'zswapy' was previously declared here
c   961 |       subroutine  zswapy (n,m,zx,incx,zy,incy)
c 
c              call zswapy(k-1,tempx,a(1,k),1,b(1),1)
c Define the inputs to:
c       subroutine  zswapy (n,m,zx,incx,zy,incy)
c     2023-10-28:
c      complex(kind=8) zx(1),zy(1),ztemp
c      INTEGER            n,m,incx, incy, ix, iy
              tempkm1 = INT(k-1)
c              tempzx = a(1,k)
              tempzy = b(1)
              call zswapy(tempkm1,tempm,a(1,k),1,b(1),1)
   30       continue
c
c           apply d inverse.
c
            b(k) = b(k)/a(k,k)
            k = k - 1
         go to 70
   40    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 2) go to 60
               kp = iabs(kpvt(k))
               if (kp .eq. k - 1) go to 50
c
c                 interchange.
c
                  temp = b(k-1)
                  b(k-1) = b(kp)
                  b(kp) = temp
   50          continue
c
c              apply the transformation.
c
               call zswapy(k-2,b(k),a(1,k),1,b(1),1)
               call zswapy(k-2,b(k-1),a(1,k-1),1,b(1),1)
   60       continue
c
c           apply d inverse.
c
            ak = a(k,k)/a(k-1,k)
            akm1 = a(k-1,k-1)/a(k-1,k)
            bk = b(k)/a(k-1,k)
            bkm1 = b(k-1)/a(k-1,k)
            denom = ak*akm1 - 1.0d0
            b(k) = (akm1*bk - bkm1)/denom
            b(k-1) = (ak*bkm1 - bk)/denom
            k = k - 2
   70    continue
      go to 10
   80 continue
c
c     loop forward applying the transformations.
c
      k = 1
   90 if (k .gt. n) go to 160
         if (kpvt(k) .lt. 0) go to 120
c
c           1 x 1 pivot block.
c
            if (k .eq. 1) go to 110
c
c              apply the transformation.
c
               b(k) = b(k) + zdotu(k-1,a(1,k),1,b(1),1)
               kp = kpvt(k)
               if (kp .eq. k) go to 100
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  100          continue
  110       continue
            k = k + 1
         go to 150
  120    continue
c
c           2 x 2 pivot block.
c
            if (k .eq. 1) go to 140
c
c              apply the transformation.
c
               b(k) = b(k) + zdotu(k-1,a(1,k),1,b(1),1)
               b(k+1) = b(k+1) + zdotu(k-1,a(1,k+1),1,b(1),1)
               kp = iabs(kpvt(k))
               if (kp .eq. k) go to 130
c
c                 interchange.
c
                  temp = b(k)
                  b(k) = b(kp)
                  b(kp) = temp
  130          continue
  140       continue
            k = k + 2
  150    continue
      go to 90
  160 continue
      return
      end
