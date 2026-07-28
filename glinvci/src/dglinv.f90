module dglinv
  use, intrinsic :: iso_c_binding
  implicit integer(c_int) (i-k), integer(c_int) (m,n), &
       & real(c_double) (a-h), real(c_double) (l), real(c_double) (o-z)
  
  type, bind(C) :: llst
     type(c_ptr)                        nxt
     integer(c_int)                     siz
     !! This is not standard-compliant but conventional enough to work
     !! in most cases - we are fine unless the Fortran compiler decides
     !! to deep copy C's memory when a subroutine is called from C.
     !!
     !! Officially the Fortran standard explicitly allow both pass-by-reference
     !! and pass-by-copying for Fortran-to-Fortran calls. However, I doubt
     !! any reasonable compilers will do the copying in C-to-Fortran calls.
     !!
     !! I have tested on all combinations of gcc, icc, clang, opencc, pgcc
     !! gfortran, ifort, pgfortran. All works.
     !!
     !! Additionally, See this non-official document:
     !!
     !! https://docs.oracle.com/cd/E19957-01/805-4940/6j4m1u7qq/index.html
     !!
     !! Quote: "The standard method for passing data between Fortran routines
     !!         and C procedures is by reference."
     !!
     !! The official Fortran 2003 standard didn't say anything specific about
     !! this.
     real(c_double)                     dat(1)
  end type
  type, bind(C) :: llstptr
     type(c_ptr)                        nxt
     integer(c_int)                     siz
     type(c_ptr)                        dat
  end type

  type dfdqdk
     real(c_double), pointer::          dfdv(:,:,:,:)
     real(c_double), pointer::          dfdphi(:,:,:,:)
     real(c_double), pointer::          dqdv(:,:,:)
     real(c_double), pointer::          dqdphi(:,:,:)
     real(c_double), pointer::          dqdw(:,:)
     real(c_double), pointer::          dkdv(:,:,:,:)
     real(c_double), pointer::          dkdphi(:,:,:,:)
     real(c_double), pointer::          f1n(:,:)
     real(c_double), pointer::          q1n(:)
     integer(c_int), pointer::          kr
     integer(c_int), pointer::          knu
     integer(c_int), pointer::          knv
     integer(c_int), pointer::          kmu
     integer(c_int), pointer::          kmv
  end type

  type, bind(C) :: dfdqdkCMEM
     type(c_ptr)            ::          R
     integer(c_int)         ::          kr
     integer(c_int)         ::          knu
     integer(c_int)         ::          knv
     integer(c_int)         ::          kmu
     integer(c_int)         ::          kmv
  end type
  real(c_double), parameter :: PI = 3.141592653589793238462_c_double
  integer(c_int), parameter :: IVV       = 0_c_int
  integer(c_int), parameter :: IVPHI     = 1_c_int
  integer(c_int), parameter :: IVW       = 2_c_int
  integer(c_int), parameter :: IPHIPHI   = 3_c_int
  integer(c_int), parameter :: IPHIW     = 4_c_int
  integer(c_int), parameter :: IWW       = 5_c_int

contains
  recursive subroutine read_dfqk (dfqk_cptr, dfqk_ftn)
    type(dfdqdk)              :: dfqk_ftn
    type(dfdqdkCMEM), target  :: dfqk_cptr
    integer(c_int)    :: p
    real(c_double), pointer :: L(:)
    dfqk_ftn%kr  => dfqk_cptr%kr
    dfqk_ftn%knu => dfqk_cptr%knu
    dfqk_ftn%knv => dfqk_cptr%knv
    dfqk_ftn%kmu => dfqk_cptr%kmu
    dfqk_ftn%kmv => dfqk_cptr%kmv
    call c_f_pointer(dfqk_cptr%R, L, &
         & [ dfqk_ftn%knu * dfqk_ftn%kr * dfqk_ftn%kmu * dfqk_ftn%kmu  +&
         &   dfqk_ftn%knu * dfqk_ftn%kr * dfqk_ftn%kmu * dfqk_ftn%kmv  +&
         &   dfqk_ftn%knu * dfqk_ftn%kmu * dfqk_ftn%kmu                +&
         &   dfqk_ftn%knu * dfqk_ftn%kmu * dfqk_ftn%kmv                +&
         &   dfqk_ftn%knu * dfqk_ftn%kmu                               +&
         &   dfqk_ftn%knu * dfqk_ftn%knu * dfqk_ftn%kmu * dfqk_ftn%kmu +&
         &   dfqk_ftn%knu * dfqk_ftn%knu * dfqk_ftn%kmu * dfqk_ftn%kmv +&
         &   dfqk_ftn%knu * dfqk_ftn%kr + &
         &   dfqk_ftn%knu ])
    p = 1
    dfqk_ftn%dfdv(1:dfqk_ftn%knu,1:dfqk_ftn%kr,1:dfqk_ftn%kmu,1:dfqk_ftn%kmu)   => L(p:)
    p = p + size(dfqk_ftn%dfdv)
    dfqk_ftn%dfdphi(1:dfqk_ftn%knu,1:dfqk_ftn%kr,1:dfqk_ftn%kmu,1:dfqk_ftn%kmv) => L(p:)
    p = p + size(dfqk_ftn%dfdphi)
    dfqk_ftn%dqdv(1:dfqk_ftn%knu,1:dfqk_ftn%kmu,1:dfqk_ftn%kmu)                 => L(p:)
    p = p + size(dfqk_ftn%dqdv)
    dfqk_ftn%dqdphi(1:dfqk_ftn%knu,1:dfqk_ftn%kmu,1:dfqk_ftn%kmv)               => L(p:)
    p = p + size(dfqk_ftn%dqdphi)
    dfqk_ftn%dqdw(1:dfqk_ftn%knu,1:dfqk_ftn%kmu)                                => L(p:)
    p = p + size(dfqk_ftn%dqdw)
    dfqk_ftn%dkdv(1:dfqk_ftn%knu,1:dfqk_ftn%knu,1:dfqk_ftn%kmu,1:dfqk_ftn%kmu)  => L(p:)
    p = p + size(dfqk_ftn%dkdv)
    dfqk_ftn%dkdphi(1:dfqk_ftn%knu,1:dfqk_ftn%knu,1:dfqk_ftn%kmu,1:dfqk_ftn%kmv)=> L(p:)
    p = p + size(dfqk_ftn%dkdphi)
    dfqk_ftn%f1n(1:dfqk_ftn%knu,1:dfqk_ftn%kr)                                  => L(p:)
    p = p + size(dfqk_ftn%f1n)
    dfqk_ftn%q1n(1:dfqk_ftn%knu)                                                => L(p:)
    p = p + size(dfqk_ftn%q1n)
  end subroutine


  ! recursive subroutine printmat (A, nrow, ncol)        bind(C, name="printmat_")
  !   dimension A(nrow, ncol)
  !   do i=1,nrow
  !      print *, A(i,:)
  !   enddo
  ! end subroutine

  recursive subroutine diagone (A, k)                  bind(C, name="diagone_")
    real(c_double) A
    integer(c_int) k
    dimension A(k,k)
    do j = 1, k
       A(j,j) = 1.0_c_double
    enddo
  end subroutine

  recursive subroutine diagoneclr (A, k)               bind(C, name="diagoneclr_")
    real(c_double) A
    integer(c_int) k
    dimension A(k,k)
    A(:,:) = 0.0_c_double
    do j = 1, k
       A(j,j) = 1.0_c_double
    enddo
  end subroutine

  recursive subroutine gesylcpy (dst, src, k)            bind(C, name="gesylcpy_")
    real(c_double) dst, src
    integer(c_int) k
    dimension src(k,k), dst((k*(k+1))/2)
    n = 1
    do j = 1,k
       do i = j,k
          dst(n) = src(i,j)
          n = n + 1
       enddo
    enddo
  end subroutine
  recursive subroutine syflgefill (src, k)
    dimension src(k,k)
    do j = 1,k
      i = 1
200   if (i >= j) goto 99
      src(i,j) = src(j,i)
      i = i + 1
      goto 200
99    continue
    end do
  end subroutine
  recursive subroutine sylgecpy (dst, src, k)            bind(C, name="sylgecpy_")
    real(c_double) dst, src
    integer(c_int) k
    dimension src((k*(k+1))/2), dst(k,k)
    n = 1
    do j = 1,k;
       do i = j,k
          dst(i,j) = src(n)
          dst(j,i) = src(n)
          n = n + 1
       enddo
    enddo
  end subroutine
  recursive subroutine lsylgecpy (dst, src, k)            bind(C, name="lsylgecpy_")
    real(c_double) dst, src
    integer(c_long) k, i, j
    dimension src((k*(k+1))/2), dst(k,k)
    n = 1
    do j = 1,k
       do i = j,k
          dst(i,j) = src(n)
          dst(j,i) = src(n)
          n = n + 1
       enddo
    enddo
  end subroutine
  
  recursive subroutine vwphisimstep(Phi, w, V, daddy, kv, ku, wsp, info) bind(c, name="vwphisimstep_")
    implicit none
    real(c_double) :: Phi, w, V, daddy, wsp, L !vterm, 
    integer(c_int) :: kv, ku, info
    dimension Phi(ku,kv), w(ku), V((ku*(ku+1))/2), daddy(kv), wsp(ku), L((ku*(ku+1))/2)
    ! On entry, wsp contains standard normal random numbers. On exit, wsp contains the ouput
    external dpptrf, dspmv, dgemm
    L(:)     = V(:)
    call dpptrf('L',ku,L,info)
    if (info /= 0) return
    !call dspmv('L',ku,1.0_c_double,L,vterm,1_c_int,1.0_c_double,wsp,1_c_int)
    call dtpmv('L','N','N',ku,L,wsp,1_c_int)
    wsp = wsp + w
    call dgemv('N',ku,kv,1.0_c_double,Phi,ku,daddy,1_c_int,1.0_c_double,wsp,1_c_int)
    !wsp = wsp + matmul(Phi, daddy) + w
  end subroutine

  pure recursive integer(c_long) function ijtouplolidx (k, i,j) bind(c, name="ijtouplolidx_")
    integer(c_long), intent(in) :: k,i,j
    ! Pre-condition: i >= j
    ijtouplolidx = (j-1_c_long) * k - ((j-1_c_long)*j)/2_c_long + i
  end function
  pure recursive integer(c_int) function iijtouplolidx (k, i,j) bind(c, name="iijtouplolidx_")
    integer(c_int), intent(in) :: k,i,j
    ! Pre-condition: i >= j
    iijtouplolidx = (j-1) * k - ((j-1)*j)/2_c_int + i
  end function

  recursive subroutine mergintern ( &
       & Vro, w, Phi, kv, ku, c, gam, o, d, H, b, V, solV, &
       & cout, gamout, oout, dout, info)
    integer(c_int) kv, ku, info
    real(c_double) Vro, w, Phi, c, gam, o, d, H, b, V, solV, &
       & cout, gamout, oout, dout
!   intent(in)    :: kv, ku, Vro, w, Phi, c, d, gam, o
!   intent(out)   :: H, b, V, solV
    intent(inout) :: cout, dout, gamout, oout
    real(c_double) tmpscl
    dimension Vro(ku, ku), w(ku), Phi(ku, kv), gam(ku), o(ku, ku), H(ku, ku), b(ku), V(ku, ku), &
         & solV(ku, ku), gamout(kv), oout(kv, kv)
    real(c_double), allocatable :: tmpkuku(:,:), tmpkvku(:,:)
    external dpotrf, dpotri
    allocate(tmpkuku(ku,ku), tmpkvku(kv,ku))
    ! V will contain Lambda eventually after the first inverse. So it was a misnomer.
    V = Vro
    b = gam
    call dgemv('N',ku,ku,-1.0_c_double,o,ku,w,1_c_int,1.0_c_double,b,1_c_int)
    !b = gam - matmul(o, w)
    call dpotrf( 'U', ku, V, ku, info )
    if (info /= 0) goto 80
    tmpscl = 0.0_c_double
    do jj = 1,ku
      tmpscl = tmpscl + log(V(jj,jj))
    end do
    tmpscl = tmpscl * 2.0_c_double
    dout = dout + d + tmpscl
    !dout = dout + d + sum((/(log(V(j,j)), j=1, ku)/)*2.0_c_double)
    call dpotri( 'U', ku, V, ku, info )
    if (info /= 0) goto 80
    solV = V                    ! Now V contains V^(-1)
    V = V + o
    call dpotrf('U', ku, V, ku, info)
    if (info /= 0) goto 85
        tmpscl = 0.0_c_double
    do jj = 1,ku
      tmpscl = tmpscl + log(V(jj,jj))
    end do
    tmpscl = tmpscl * 2.0_c_double
    dout = dout + tmpscl        ! Now Delta is done
    !dout = dout + sum((/(log(V(j,j)), j=1, ku)/)*2.0_c_double) ! Now Delta is done    
    call dpotri( 'U', ku, V, ku, info ) ! Now V contains Lambda
    if (info /= 0) goto 85
    do j = 1, ku
       do i = j, ku
          solV(i,j) = solV(j,i)
       enddo
    enddo
    do j = 1, ku
       do i = j, ku
          V(i,j) = V(j,i)
       enddo
    enddo
    call dgemm('N','N',ku,ku,ku,-1.0_c_double,V,ku,o,ku,0.0_c_double,H,ku)
    !H = -matmul(V, o)
    do j = 1, ku
       H(j,j) = H(j,j) + 1.0_c_double ! Now H is computed
    enddo
    cout = cout + c
    tmpkuku(:,1) = gam
    call dgemv('N',ku,ku,1.0_c_double,o,ku,w,1_c_int,-2.0_c_double,tmpkuku(:,1),1_c_int)
    cout = cout + ddot(ku, w, 1_c_int, tmpkuku(:,1), 1_c_int)
    call dgemv('N',ku,ku,1.0_c_double,V,ku,b,1_c_int,0.0_c_double,tmpkuku(:,1),1_c_int)
    cout = cout - ddot(ku, b, 1_c_int, tmpkuku(:,1), 1_c_int)

    call dgemv('T',ku,ku,1.0_c_double,H,ku,b,1_c_int,0.0_c_double,tmpkuku(:,1),1_c_int)
    call dgemv('T',ku,kv,1.0_c_double,Phi,ku,tmpkuku(:,1),1_c_int,1.0_c_double,gamout,1_c_int)

    call dgemm('N','N',ku,ku,ku,1.0_c_double,o,ku,H,ku,0.0_c_double,tmpkuku,ku)
    call dgemm('T','N',kv,ku,ku,1.0_c_double,Phi,ku,tmpkuku,ku,0.0_c_double,tmpkvku,kv)
    call dgemm('N','N',kv,kv,ku,1.0_c_double,tmpkvku,kv,Phi,ku,1.0_c_double,oout,kv)
    !cout = cout + c + dot_product(w, matmul(o, w) - 2.0_c_double * gam) &
    !     - dot_product(b, matmul(V, b))
    !gamout = gamout + matmul(transpose(Phi), matmul(transpose(H), b))
    !oout = oout + matmul(matmul(transpose(Phi), matmul(o, H)), Phi)

79  deallocate(tmpkuku, tmpkvku)
    return
80  info = -1
    goto 79
85  info = -2
    goto 79
! 80  call rexit('mergintern(): V is numerically non-positive-definite!')
! 85  call rexit('mergintern(): V^-1 + sum_k(Omega_k) is numerically non-positive-definite!')
  end subroutine

  recursive subroutine ndmerg(&
       V, w, Phi, kv, ku, c, gam, o, d, &
       cout, gamout, oout, dout, info)                  bind(C, name="ndmerg_")
    real(c_double) V, w, Phi, c, gam, o, d, cout, gamout, oout, dout
    integer(c_int) kv, ku, info
    dimension V(ku, ku), w(ku), Phi(ku, kv), gam(ku), o(ku, ku), gamout(kv), oout(kv, kv)
    real(c_double), allocatable :: H(:,:), b(:), Lamb(:,:), solV(:,:)
    allocate(H(ku, ku), b(ku), Lamb(ku, ku), solV(ku, ku))
    call mergintern ( &
         V, w, Phi, kv, ku, c, gam, o, d, H, b, Lamb, solV, &
         cout, gamout, oout, dout, info)
    deallocate(H,b,Lamb,solV)
  end subroutine

  recursive subroutine dmerg ( &
       & V, w, Phi, kv, ku, c, gam, o, d, &
       & cout, gamout, oout, dout, &
       & a, Hphi, Lamb, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, info)   bind(C, name="dmerg_")
    integer(c_int) kv, ku, info
    real(c_double) V, w, Phi, c, gam, o, d, &
       & cout, gamout, oout, dout, &
       & a, Hphi, Lamb, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev
    dimension V(ku, ku), w(ku), Phi(ku, kv), gam(ku), o(ku, ku), gamout(kv), oout(kv, kv), &
         a(ku), Hphi(ku, kv), Lamb(ku, ku), &
         & dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv), &
         & dcdvev(ku, ku), dcdwev(ku), dddvev(ku, ku)
    real(c_double), allocatable :: dldvev(:,:,:,:), H(:,:), b(:), solV(:,:)
    allocate(dldvev(ku, ku, ku, ku), H(ku, ku), b(ku), solV(ku, ku))
    call mergintern ( &
         V, w, Phi, kv, ku, c, gam, o, d, H, b, Lamb, solV, &
         cout, gamout, oout, dout, info)
    call dgemm('N','N',ku,kv,ku,1.0_c_double,H,ku,Phi,ku,0.0_c_double,HPhi,ku)
    a = w
    call dgemv('N',ku,ku,1.0_c_double,Lamb,ku,b,1_c_int,1.0_c_double,a,1_c_int)
    !HPhi = matmul(H, Phi)
    !a = matmul(Lamb, b) + w
    call dldv(Lamb, ku, solV, dldvev)
    call dcdv(dldvev, b, ku, dcdvev)
    call dcdw(H, b, ku, dcdwev)
    call dgamdv(dldvev, Phi, o, b, kv, ku, dgamdvev)
    call dgamdw(HPhi, o, kv, ku, dgamdwev)
    call dgamdphi(H, b, kv, ku, dgamdphiev)
    call dodv(dldvev, Phi, o, kv, ku, dodvev)
    call dodphi(o, H, Phi, kv, ku, dodphiev)
    call dddv(solV, o, dldvev, ku, dddvev)
    deallocate(dldvev, H, b, solV)
  end subroutine

  recursive subroutine hmerg ( &
       & V, w, Phi, kv, ku, c, gam, o, d, &
       & cout, gamout, oout, dout, &
       & a, b, solV, H, Hphi, Lamb, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, info)   bind(C, name="hmerg_")
    integer(c_int) kv, ku, info
    real(c_double) V, w, Phi, c, gam, o, d, &
       & cout, gamout, oout, dout, &
       & a, b, solV, H, Hphi, Lamb, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev
    dimension V(ku, ku), w(ku), Phi(ku, kv), gam(ku), o(ku, ku), gamout(kv), oout(kv, kv), &
         & a(ku), Hphi(ku, kv), Lamb(ku, ku), &
         & dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv), &
         & dcdvev(ku, ku), dcdwev(ku), dddvev(ku, ku), &
         & H(ku, ku), b(ku), solV(ku, ku)
    real(c_double), allocatable :: dldvev(:,:,:,:)
    allocate(dldvev(ku, ku, ku, ku))
    call mergintern ( &
         & V, w, Phi, kv, ku, c, gam, o, d, H, b, Lamb, solV, &
         & cout, gamout, oout, dout, info)

    call dgemm('N','N',ku,kv,ku,1.0_c_double,H,ku,Phi,ku,0.0_c_double,HPhi,ku)
    a = w
    call dgemv('N',ku,ku,1.0_c_double,Lamb,ku,b,1_c_int,1.0_c_double,a,1_c_int)
    !HPhi = matmul(H, Phi)
    !a = matmul(Lamb, b) + w
    call dldv(Lamb, ku, solV, dldvev)
    call dcdv(dldvev, b, ku, dcdvev)
    call dcdw(H, b, ku, dcdwev)
    call dgamdv(dldvev, Phi, o, b, kv, ku, dgamdvev)
    call dgamdw(HPhi, o, kv, ku, dgamdwev)
    call dgamdphi(H, b, kv, ku, dgamdphiev)
    call dodv(dldvev, Phi, o, kv, ku, dodvev)
    call dodphi(o, H, Phi, kv, ku, dodphiev)
    call dddv(solV, o, dldvev, ku, dddvev)
    deallocate(dldvev)
  end subroutine

  recursive subroutine tcgodintern (V, w, Phi, x, kv, ku, c, gam, o, d, b, solV, info)   bind(C, name="tcgodintern_")
    real(c_double) V, w, Phi, x, c, gam, o, d, b, solV
    integer(c_int) kv, ku, info
    dimension V(ku, ku), w(ku), x(ku), Phi(ku, kv), gam(kv), o(kv, kv), b(ku), solV(ku, ku)
    allocatable :: Lb(:), tmp(:,:)
    external dpotrf, dpotri, dgemv
    allocate(Lb(ku), tmp(ku,kv))
    solV = V
    b = x - w
    call dpotrf( 'U', ku, solV, ku, info )
    if (info /= 0) goto 90
    !!d = d + sum((/(log(solV(j,j)), j=1, ku)/)*2.0_c_double)
    do i = 1,ku
       d = d+2.0_c_double*log(solV(i,i))
    enddo
    !! Now d == logdet(V)
    
    call dpotri( 'U', ku, solV, ku, info )
    if (info /= 0) goto 90
    do j = 1, ku
       do i = j, ku
          solV(i,j) = solV(j,i)
       enddo
    enddo
    
    call dgemv('N',ku,ku,1.0_c_double,solV,ku,b,1_c_int,0.0_c_double,Lb,1_c_int)
    c = c + ddot(ku,b,1_c_int,Lb,1_c_int)
!    Lb = matmul(solV, b)
!    c = c + dot_product(b, Lb)
!    gam = gam + matmul(transpose(Phi), Lb)
!    o = o + matmul(transpose(Phi), matmul(solV, Phi))
    call dgemv('T',ku,kv,1.0_c_double,Phi,ku,Lb,1_c_int,1.0_c_double,gam,1_c_int)
    call dgemm('N','N',ku,kv,ku,1.0_c_double,solV,ku,Phi,ku,0.0_c_double,tmp,ku)
    call dgemm('T','N',kv,kv,ku,1.0_c_double,Phi,ku,tmp,ku,1.0_c_double,o,kv)
    deallocate(Lb, tmp)
    info = 0
    return

!! 90  call rexit('tcgodintern(): V is numerically non-positive-definite!')
90  info = -1
  end subroutine
  
  recursive subroutine ndtcgod (V, w, Phi, x, kv, ku, c, gam, o, d, info)   bind(C, name="ndtcgod_")
    integer(c_int) kv, ku, info
    real(c_double) V, w, Phi, x, c, gam, o, d
    dimension V(ku, ku), w(ku), Phi(ku, kv), x(ku), gam(kv), o(kv, kv)
    real(c_double), allocatable :: b(:), solV(:,:)
    allocate(b(ku), solV(ku, ku))
    call tcgodintern(V, w, Phi, x, kv, ku, c, gam, o, d, b, solV, info)
    deallocate(b, solV)
  end subroutine

  recursive subroutine dtcgod (V, w, Phi, x, kv, ku, c, gam, o, d, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, info)   bind(C, name="dtcgod_")
    integer(c_int) kv, ku, info
    real(c_double) V, w, Phi, x, c, gam, o, d, &
         & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev
    dimension V(ku, ku), w(ku), x(ku), Phi(ku, kv), gam(kv), o(kv, kv), &
         & dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv), &
         & dcdvev(ku, ku), dcdwev(ku), dddvev(ku, ku)
         
    allocatable ::solV(:,:), b(:)
    allocate(solV(ku, ku), b(ku))
    call htcgod (V, w, Phi, x, kv, ku, c, gam, o, d, solV, b, &
         dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, info)
    deallocate(solV, b)
  end subroutine
  recursive subroutine htcgod (V, w, Phi, x, kv, ku, c, gam, o, d, solV, b, &
       dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, info)   bind(C, name="htcgod_")
    real(c_double) V, w, Phi, x, c, gam, o, d, solV, b, dodvev, dodphiev, &
         & dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev
    integer(c_int) kv, ku, info
    dimension V(ku, ku), w(ku), x(ku), Phi(ku, kv), gam(kv), o(kv, kv), &
         & dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv), &
         & dcdvev(ku, ku), dcdwev(ku), dddvev(ku, ku), &
         & solV(ku, ku), b(ku)
    allocatable :: dldvev(:,:,:,:), sco(:,:), tmpkukv(:,:)
    allocate(dldvev(ku, ku, ku, ku), sco(ku, ku), tmpkukv(kv, ku))
    call tcgodintern(V, w, Phi, x, kv, ku, c, gam, o, d, b, solV, info)
    call ndinv(solV, ku, dldvev)
    call dcdv(dldvev, b, ku, dcdvev)
    call dcdw(solV, b, ku, dcdwev)

    do j = 1, ku
       do i = 1, ku
          ! Use sco as temporary variable.
          do jj = 1,ku
             do ii = 1,ku
                sco(ii,jj) = dldvev(i,j,jj,ii)
             end do
          end do
          do jj = 1,ku
             do ii = 1,ku
                dldvev(i,j,ii,jj) = sco(ii,jj)
             end do
          end do       
          !dldvev(i,j,:,:) = transpose(dldvev(i,j,:,:))
       enddo
    enddo
    sco = 0.0_c_double
    do j = 1, ku
       sco(j,j) = 1.0_c_double
    enddo
    call dodv(dldvev, Phi, sco, kv, ku, dodvev)
    call dodphi(sco, solV, Phi, kv, ku, dodphiev)
    call dgamdv(dldvev, Phi, sco,b, kv, ku, dgamdvev)
    call dgemm('N','N',ku,kv,ku,1.0_c_double,solV,ku,Phi,ku,0.0_c_double,tmpkukv,ku)
    call dgamdw(tmpkukv, sco, kv, ku, dgamdwev)
    !call dgamdw(matmul(solV, Phi), sco, kv, ku, dgamdwev)
    call dgamdphi(solV, b, kv, ku, dgamdphiev)
    dddvev = solV
    deallocate(dldvev, sco, tmpkukv)
  end subroutine


  recursive subroutine phygausslik (c, gam, o, d, x0, k0, k, lik) bind(C, name="phygausslik_")
    real(c_double) c, gam, o, d, x0, lik
    integer(c_int) k0, k
!   intent(in) :: c, gam, o, d, x0, k0, k;   intent(out) :: lik
    dimension gam(k0), x0(k0), o(k0,k0)
    real(c_double), allocatable :: tmpk0(:)
    allocate(tmpk0(k0))
    tmpk0 = gam
    call dgemv('N',k0,k0,-0.5_c_double,o,k0,x0,1_c_int,1.0_c_double,tmpk0,1_c_int)
    lik = (c + k*log(2.0_c_double * PI) + d) / (-2.0_c_double) + &
        & ddot(k0, x0, 1_c_int, tmpk0, 1_c_int)  
    !lik = (c + k*log(2.0_c_double * PI) + d) / (-2.0_c_double) &
    !     + dot_product(x0, gam - (matmul(o, x0) / 2.0_c_double))
    deallocate(tmpk0)
  end subroutine
  
  recursive subroutine ndinv (solA, ku, dA)    bind(C, name="ndinv_")
    real(c_double) solA, dA
    integer(c_int) ku
!   intent(in) :: ku, solA;   intent(out) :: dA
    dimension solA(ku,ku), dA(ku,ku,ku,ku)
    do j=1,ku
       do i=1,ku
          do m=1,ku
             do k=1,ku
                dA(k,m,i,j) = solA(k,i) * solA(j,m)
             enddo
          enddo
       enddo
    enddo
    !! Post-condition: dA(:,:,i,j)(k,l) = dA(k,l,i,j) = solA(k,i) * solA(j,l)
  end subroutine
  
  !! TODO: should be dsymm not dgemm but for small matrcies it doesn't matter.
  recursive subroutine dldv (Lamb, ku, solV, out)  bind(C, name="dldv_")
    integer(c_int) ku
    real(c_double) Lamb, solV, out
!   intent(in) :: Lamb, ku, solV;   intent(out) :: out
    dimension Lamb(ku,ku), solV(ku,ku), out(ku,ku,ku,ku)
    real(c_double), allocatable :: tmp(:,:,:,:), tmpkuku(:,:)
    allocate(tmp(ku,ku,ku,ku), tmpkuku(ku,ku))
    call ndinv(solV, ku, tmp)
    do j=1,ku
       do i=1,ku
          call dgemm('N','N',ku,ku,ku,1.0_c_double,tmp(:,:,i,j),ku,Lamb,ku,0.0_c_double,tmpkuku,ku)
          call dgemm('N','N',ku,ku,ku,1.0_c_double,Lamb,ku,tmpkuku,ku,0.0_c_double,out(:,:,i,j),ku)
          !out(:,:,i,j) = matmul(Lamb, matmul(tmp(:,:,i,j), Lamb))
       enddo
    enddo
    !! Post-condition: out(:,:,i,j) = d Lambda / d V_ij
    deallocate(tmp)
  end subroutine

  recursive subroutine dodv (dLdVev, Phi_u, ScOmega, kv, ku, out)  bind(C, name="dodv_")
    integer(c_int) kv, ku
    real(c_double) dLdVev, Phi_u, ScOmega, out
!   intent(in) :: dLdVev, Phi_u, ScOmega, kv, ku;   intent(out) :: out
    dimension ScOmega(ku,ku), dLdVev(ku,ku,ku,ku), Phi_u(ku,kv), out(kv, kv, ku, ku)
    real(c_double), allocatable :: tmpkukv(:,:), A(:,:)
    allocate(tmpkukv(ku,kv), A(ku,kv))
    call dgemm('N','N',ku,kv,ku,1.0_c_double,ScOmega,ku,Phi_u,ku,0.0_c_double,A,ku)
    !A = matmul(ScOmega, Phi_u)
    do j=1,ku
       do i=1,ku
          call dgemm('N','N',ku,kv,ku,1.0_c_double,dLdVev(:,:,i,j),ku,A,ku,0.0_c_double,tmpkukv,ku)
          call dgemm('T','N',kv,kv,ku,-1.0_c_double,A,ku,tmpkukv,ku,0.0_c_double,out(:,:,i,j),kv)
          !out(:,:,i,j) = - matmul(transpose(A), matmul(dLdVev(:,:,i,j) , A))
       enddo
    enddo
    deallocate(tmpkukv, A)
  end subroutine

  recursive subroutine dodphi (ScOmega, H_u, Phi_u, kv, ku, out) bind(C, name="dodphi_")
    integer(c_int) kv, ku
    real(c_double) ScOmega, H_u, Phi_u, out
!   intent(in) :: ScOmega, H_u, Phi_u, kv, ku;   intent(out) :: out
    dimension ScOmega(ku,ku), Phi_u(ku,kv), H_u(ku,ku), out(kv,kv,ku,kv)
    real(c_double), allocatable :: RM(:,:), LM(:,:), B(:,:)
    allocate(RM(kv,ku), LM(kv, ku), B(ku,ku))
    out = 0._c_double
    call dgemm('N','N',ku,ku,ku,1.0_c_double,ScOmega,ku,H_u,ku,0.0_c_double,B,ku)
    call dgemm('T','N',kv,ku,ku,1.0_c_double,Phi_u,ku,B,ku,0.0_c_double,LM,kv)
    call dgemm('T','T',kv,ku,ku,1.0_c_double,Phi_u,ku,B,ku,0.0_c_double,RM,kv)
    !B = matmul(ScOmega, H_u)
    !LM = matmul(transpose(Phi_u), B)
    !RM = transpose(matmul(B, Phi_u))
    do j = 1, kv
       out(:,j,:,j) = out(:,j,:,j) + LM
       out(j,:,:,j) = out(j,:,:,j) + RM
    enddo
    deallocate(RM, LM, B)
  end subroutine

  recursive subroutine dgamdw (HPhi, ScOmega, kv, ku, out) bind(C, name="dgamdw_")
    integer(c_int) kv, ku
    real(c_double) HPhi, ScOmega, out
!   intent(in) :: HPhi, ScOmega, kv, ku;    intent(out) :: out
    dimension ScOmega(ku,ku), HPhi(ku,kv), out(kv,ku)
    call dgemm('T','N',kv,ku,ku,-1.0_c_double,HPhi,ku,ScOmega,ku,0.0_c_double,out,kv)
    !out(:,:) = - matmul(transpose(HPhi), ScOmega)
  end subroutine

  recursive subroutine dgamdphi (H_u, b_u, kv, ku, out)  bind(C, name="dgamdphi_")
    integer(c_int) kv, ku
    real(c_double) H_u, b_u, out
!   intent(in) :: H_u, b_u, kv, ku;  intent(out) :: out
    dimension H_u(ku,ku), b_u(ku), out(kv,ku,kv)
    real(c_double), allocatable :: d(:)
    allocate(d(ku))
    out = 0._c_double
    call dgemv('T',ku,ku,1.0_c_double,H_u,ku,b_u,1_c_int,0.0_c_double,d,1_c_int)
    !d = matmul(transpose(H_u), b_u)
    do j = 1, kv
       out(j,:,j) = d
    enddo
    deallocate(d)
  end subroutine

  recursive subroutine dgamdv (dLdVev, Phi_u, ScOmega, b_u, kv, ku, out) bind(C, name="dgamdv_")
    !! Precondition:
    !!  1. If u is a tip, then dldVev[k,l,i,j] == - d(inv(Vu)[k,l])/d(Vu[j,i]).
    !!     Notice the negative and the transpose at denominator.
    !!  2. If u is non-tip, then dldVev[k,l,i,j] == d(Lambda[k,l])/d(Vu[i,j]).
    integer(c_int) kv, ku
    real(c_double) dLdVev, Phi_u, ScOmega, b_u, out
!   intent(in) :: dLdVev, Phi_u, ScOmega, b_u, kv, ku;  intent(out) :: out
    dimension dLdVev(ku,ku,ku,ku), Phi_u(ku,kv), ScOmega(ku,ku), b_u(ku), out(kv,ku,ku)
    external dgemm,dgemv
    allocatable :: tmp1(:,:), tmp2(:)
    allocate(tmp1(ku,ku),tmp2(ku))
    do j=1,ku
       do i=1,ku
!          out(:,i,j) = - matmul(transpose(Phi_u), matmul(transpose(matmul(dLdVev(:,:,i,j), ScOmega)), b_u))
          call dgemm('N','N',ku,ku,ku,1.0_c_double,dLdVev(:,:,i,j),ku,ScOmega,ku,0.0_c_double,tmp1,ku)
          call dgemv('T',ku,ku,1.0_c_double,tmp1,ku,b_u,1_c_int,0.0_c_double,tmp2,1_c_int)
          call dgemv('T',ku,kv,-1.0_c_double,Phi_u,ku,tmp2,1_c_int,0.0_c_double,out(:,i,j),1_c_int)
       enddo
    enddo
    deallocate(tmp1,tmp2)
  end subroutine

  recursive subroutine dcdw (H_u, b_u, ku, out)  bind(C, name="dcdw_")
    integer(c_int) ku
    real(c_double) H_u, b_u, out
!   intent(in) :: H_u, b_u, ku;   intent(out) :: out
    dimension H_u(ku,ku), b_u(ku), out(ku)
    call dgemv('T',ku,ku,-2.0_c_double,H_u,ku,b_u,1_c_int,0.0_c_double,out,1_c_int)
    !out = - 2._c_double * matmul(transpose(H_u), b_u)
  end subroutine
  
  recursive subroutine dcdv (dLdVev, b_u, ku, out)   bind(C, name="dcdv_")
    !! Precondition:
    !!  1. If u is a tip, then dldVev[k,l,i,j] == - d(inv(Vu)[k,l])/d(Vu[i,j]).
    !!     Notice the negative, but no transpose at denominator.
    !!  2. If u is non-tip, then dldVev[k,l,i,j] == d(Lambda[k,l])/d(Vu[i,j]).
    !!    
    integer(c_int) ku
    real(c_double) dLdVev, b_u, out
!   intent(in) :: dLdVev, b_u, ku;   intent(out) :: out
    dimension dLdVev(ku,ku,ku,ku), b_u(ku), out(ku,ku)
    real(c_double), allocatable :: tmpku(:)
    allocate(tmpku(ku))
    do j=1,ku
       do i=1,ku
          call dgemv('N',ku,ku,1.0_c_double,dLdVev(:,:,i,j),ku,b_u,1_c_int,0.0_c_double,tmpku,1_c_int)
          out(i,j) = -1.0_c_double * ddot(ku, b_u, 1_c_int, tmpku, 1_c_int)
          !out(i,j) = - dot_product(b_u, matmul(dLdVev(:,:,i,j), b_u))
       enddo
    enddo
    deallocate(tmpku)
  end subroutine

  recursive subroutine dddv (solV, o, dldvev, ku, out)    bind(C, name="dddv_")
    !! For non-tips:
    !!
    !! dddv_ij = inverse-of-V_ij + Tr( (V+Sum(O))^-1 %*% (-ndinv)_..ij )
    !!         = inverse-of-V_ij  - Tr( Lambda^T %*% ndinv_..ij )
    !!         = inverse-of-V_ij  - sum( Lambda * ndinv_..ij )
    !!
    !! Or better,
    !! dddv_ij = inverse-of-V_ij - D[lndet(Lambda)]
    !!         = inverse-of-V_ij - Tr(Lambda^-1 %*% dldv_..ij )
    !!         = inverse-of-V_ij - sum((V^-1 + o) * dldv_..ij )
    !!
    !! For tips:
    !! dddv = inverse-of-v
    !!
    !! Chain rule:
    !! dDvdVb_ij = dDudVb_ij - D[lndet(Lambda)]            Well, if Lambda has positive determinant at all
    !!        = dDudVb - Tr[(Vb^-1 + O) %*% dOudVb_..ij]
    !!        = dDudVb - sum((Vb^-1 + O)^T * dOudVb_..ij)
    integer(c_int) ku
    real(c_double) solV, o, dldvev, out
!   intent(in) :: solV, o, dldvev, ku;   intent(out) :: out
    dimension solV(ku,ku), o(ku,ku), dldvev(ku,ku,ku,ku), out(ku,ku)
    real(c_double), allocatable :: tmp1(:,:)
    allocate(tmp1(ku,ku))
    tmp1 = solV + o
    out = solV
    do j=1,ku
       do i=1,ku
          !! The (solV+o) sum is done the 2nd time and should be optimised away later.
          out(i,j) = out(i,j) - ddot(ku*ku,tmp1,1_c_int,dldvev(:,:,i,j),1_c_int)
          !out(i,j) = out(i,j) - sum((solV + o) * dldvev(:,:,i,j))
       enddo
    enddo
    deallocate(tmp1)
  end subroutine
  
  !! Compute the derivative of likelihood of the direct children of the global root.
  recursive subroutine ddcr (kr, ku, x0, dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, &
                     & dcdwev, dcdvev, dddvev, dlikdv, dlikdw, dlikdphi)   bind(C, name="ddcr_")
    integer(c_int) kr, ku
    real(c_double) x0, dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, &
         & dcdwev, dcdvev, dddvev, dlikdv, dlikdw, dlikdphi
    intent(in) :: kr, ku, x0, dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, &
         & dcdwev, dcdvev, dddvev
!   intent(out) :: dlikdv, dlikdw, dlikdphi
    dimension x0(kr), dodvev(kr,kr,ku,ku), dodphiev(kr,kr,ku,kr), &
         & dgamdvev(kr,ku,ku), dgamdwev(kr,ku), dgamdphiev(kr,ku,kr), &
         & dcdvev(ku,ku), dcdwev(ku), dddvev(ku,ku), dlikdv(ku,ku), dlikdw(ku), dlikdphi(ku,kr)
    integer(c_int) :: i, j
    real(c_double), allocatable :: tmpkr1(:)
    allocate(tmpkr1(kr))
    do j=1,ku
       do i=1,ku
          tmpkr1 = dgamdvev(:,i,j)
          call dgemv('N',kr,kr,-0.5_c_double,dodvev(:,:,i,j),kr,x0,1_c_int,1.0_c_double,tmpkr1,1_c_int)
          dlikdv(i,j) = ddot(kr,x0,1_c_int, tmpkr1,1_c_int) - (dcdvev(i,j) + dddvev(i,j)) / 2.0_c_double
          !dlikdv(i,j)= dot_product(x0, dgamdvev(:,i,j) - matmul(dodvev(:,:,i,j), x0)/2.0_c_double) &
          !     - (dcdvev(i,j) + dddvev(i,j)) / 2.0_c_double
       enddo
    enddo
    call symdiff0d(dlikdv, ku, tmpkr1)
    do j=1,kr
       do i=1,ku
          tmpkr1 = dgamdphiev(:,i,j)
          call dgemv('N',kr,kr,-0.5_c_double,dodphiev(:,:,i,j),kr,x0,1_c_int,1.0_c_double,tmpkr1,1_c_int)
          dlikdphi(i,j)= ddot(kr,x0,1_c_int,tmpkr1,1_c_int)
          !dlikdphi(i,j)= dot_product(x0, dgamdphiev(:,i,j) - matmul(dodphiev(:,:,i,j), x0)/2.0_c_double)
       enddo
    enddo
    do i=1,ku
       dlikdw(i)= ddot(kr,x0, 1_c_int,dgamdwev(:,i),1_c_int) - dcdwev(i) / 2.0_c_double
    enddo
    deallocate(tmpkr1)
  end subroutine
  
  recursive subroutine symdiff0d(x, k, wsp)
    dimension x(k,k), wsp(k)
    do j = 1,k
      wsp(j) = x(j,j)
    end do
    do j = 1,k
       do i = 1,k
         x(i,j) = x(i,j) + x(j,i)
       end do
    end do
    !x = x + transpose(x)
    do j = 1, k
       x(j,j) = x(j,j) - wsp(j)
    enddo
  end subroutine

  recursive subroutine fzkdown (Fb, zb, Kb, HPhib, ab, Lambb, x0, ksc, ksb, ksa, ksr, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, &
       & dlikdv, dlikdw, dlikdphi, Fa, za, Ka)   bind(C, name="fzkdown_")
    intent(in) :: Fb, zb, Kb, HPhib, ab, Lambb, x0, ksc, ksb, ksa, ksr, &
         dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev
!   intent(out) :: dlikdv, dlikdw, dlikdphi, Fa, za, Ka
    real(c_double) :: Fb, zb, Kb, HPhib, ab, Lambb, x0, &
         & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dcdwev, dcdvev, dddvev, &
         & dlikdv, dlikdw, dlikdphi, Fa, za, Ka
    integer(c_int) ksc, ksb, ksa, ksr
    dimension Fb(ksc, ksr), zb(ksc), Kb(ksc, ksc), HPhib(ksb, ksc), &
         & ab(ksb), Lambb(ksb, ksb), x0(ksr), &
         & dodvev(ksb, ksb, ksa, ksa), dodphiev(ksb, ksb, ksa, ksb), &
         & dgamdvev(ksb, ksa, ksa), dgamdwev(ksb, ksa), dgamdphiev(ksb, ksa, ksb), &
         & dcdvev(ksa, ksa), dcdwev(ksa), dddvev(ksa, ksa), &
         & dlikdv(ksa, ksa), dlikdw(ksa), dlikdphi(ksa, ksb), &
         & Fa(ksb, ksr), za(ksb), Ka(ksb, ksb)

    real(c_double), allocatable :: tmpkscksb(:,:), Fax0(:), zaFax02(:), tmpksb1(:), tmpksbksb(:,:)
    allocate(tmpkscksb(ksc,ksb), Fax0(ksb), zaFax02(ksb), tmpksb1(ksb), tmpksbksb(ksb,ksb))

    call dgemm('N','N',ksb,ksr,ksc,1.0_c_double,HPhib,ksb,Fb,ksc,0.0_c_double,Fa,ksb)
    za = ab
    call dgemv('N',ksb,ksc,1.0_c_double,HPhib,ksb,zb,1_c_int,1.0_c_double,za,1_c_int)
    call dgemm('N','T',ksc,ksb,ksc,1.0_c_double,Kb,ksc,HPhib,ksb,0.0_c_double,tmpkscksb,ksc)
    Ka = Lambb
    call dgemm('N','N',ksb,ksb,ksc,1.0_c_double,HPhib,ksb,tmpkscksb,ksc,1.0_c_double,Ka,ksb)
!    Fa = matmul(HPhib, Fb)
!    za = matmul(HPhib, zb) + ab
!    Ka = matmul(HPhib, matmul(Kb, transpose(HPhib))) + Lambb
    call dgemv('N',ksb,ksr,1.0_c_double,Fa,ksb,x0,1_c_int,0.0_c_double,Fax0,1_c_int)
    zaFax02 = za + Fax0/2.0_c_double
    do j=1,ksa
       do i=1,ksa
          tmpksb1 = dgamdvev(:,i,j)
          call dgemv('N',ksb,ksb,-1.0_c_double,dodvev(:,:,i,j),ksb,zaFax02,1_c_int,1.0_c_double,tmpksb1,1_c_int)
          dlikdv(i,j) = ddot(ksb, Fax0, 1_c_int, tmpksb1, 1_c_int)
          tmpksb1 = dgamdvev(:,i,j)
          call dgemv('N',ksb,ksb,1.0_c_double,dodvev(:,:,i,j),ksb,za,1_c_int,-2.0_c_double,tmpksb1,1_c_int)
          dlikdv(i,j) = dlikdv(i,j) + (ddot(ksb, za, 1_c_int, tmpksb1, 1_c_int) + dcdvev(i,j) + dddvev(i,j) + &
                & ddot(ksb*ksb, dodvev(1,1,i,j), 1_c_int, Ka(1,1), 1_c_int))/(-2.0_c_double)
!          dlikdv(i,j)=dot_product(Fax0, -matmul(dodvev(:,:,i,j), za + Fax0/(2.0_c_double))&
!               + dgamdvev(:,i,j)) &
!               +( dcdvev(i,j) + dot_product(za, matmul(dodvev(:,:,i,j), za) - 2.0_c_double * dgamdvev(:,i,j)) &
!               + dddvev(i,j) + sum(dodvev(:,:,i,j) * Ka) ) / (-2.0_c_double)
       enddo
    enddo
    call symdiff0d(dlikdv, ksa, dlikdw) ! use dlikdw as workspace
    do j=1,ksb
       do i=1,ksa
          tmpksb1 = dgamdphiev(:,i,j)
          call dgemv('N',ksb,ksb,-1.0_c_double,dodphiev(:,:,i,j),ksb,zaFax02,1_c_int,1.0_c_double,tmpksb1,1_c_int)
          dlikdphi(i,j) = ddot(ksb, Fax0, 1_c_int, tmpksb1, 1_c_int)
          tmpksb1 = dgamdphiev(:,i,j)
          call dgemv('N',ksb,ksb,1.0_c_double,dodphiev(:,:,i,j),ksb,za,1_c_int,-2.0_c_double,tmpksb1,1_c_int)
          dlikdphi(i,j) = dlikdphi(i,j) + (ddot(ksb, za, 1_c_int, tmpksb1, 1_c_int) + &
                & ddot(ksb*ksb, dodphiev(1,1,i,j), 1_c_int, Ka(1,1), 1_c_int))/(-2.0_c_double)
!          dlikdphi(i,j)=dot_product(Fax0, - matmul(dodphiev(:,:,i,j), za + Fax0/(2.0_c_double))&
!               + dgamdphiev(:,i,j)) &
!               + (dot_product(za, matmul(dodphiev(:,:,i,j), za) - 2.0_c_double * dgamdphiev(:,i,j)) &
!               + sum(dodphiev(:,:,i,j) * Ka) ) / (-2.0_c_double)
       enddo
    enddo
    zaFax02 = Fax0 + za
    dlikdw  = dcdwev
    call dgemv('T',ksb,ksa,1.0_c_double,dgamdwev,ksb,zaFax02,1_c_int,-0.5_c_double,dlikdw,1_c_int)
!    dlikdw(:) = matmul(transpose(dgamdwev), Fax0 + za) - dcdwev/(2.0_c_double)
    deallocate(tmpkscksb, Fax0, zaFax02, tmpksb1, tmpksbksb)
  end subroutine

  recursive subroutine hodvdvtip (solVPhi, solV, kv, ku, i, j, p, q, dvdv)    bind(C, name="hodvdvtip_")
    integer(c_int), intent(in)  :: ku, kv, i, j, p, q
    real(c_double), intent(in)  :: solVPhi(ku, kv), solV(ku, ku)
    real(c_double) :: dvdv(kv, kv)
!   real(c_double), intent(out) :: dvdv(kv, kv)
    integer :: k, l
    do l=1,kv
       do k=1,kv
          dvdv(k,l) =  solVPhi(p,k) * solV(q,i) * solVPhi(j,l) &
            &     + solVPhi(i,k) * solV(j,p) * solVPhi(q,l)
       enddo
    enddo
  end subroutine
  recursive subroutine hodvdvgen (solVLsOPhi, VmVLV, kv, ku, i, j, p, q, dvdv)    bind(C, name="hodvdvgen_")
    integer(c_int), intent(in)  :: ku, kv, i, j, p, q
    real(c_double), intent(in)  :: solVLsOPhi(ku, kv), VmVLV(ku, ku)
    real(c_double) :: dvdv(kv, kv)
!   real(c_double), intent(out) :: dvdv(kv, kv)
    integer :: k,l
    do l=1,kv
       do k=1,kv
          dvdv(k,l) =   solVLsOPhi(p,k) * VmVLV(q,i) * solVLsOPhi(j,l) &
               &      + solVLsOPhi(i,k) * VmVLV(j,p) * solVLsOPhi(q,l)
       enddo
    enddo
  end subroutine

  
  recursive subroutine hodvdphitip (solV, solVPhi, kv, ku, i, j, p, q, dvdphi) bind(C, name="hodvdphitip_")
    integer(c_int), intent(in)    :: ku, kv, i, j, p, q
    real(c_double), intent(in)    :: solV(ku, ku), solVPhi(ku, kv)
    real(c_double), intent(inout) :: dvdphi(kv, kv)

    ! Precondition: dvdphi contains all zeros.
    !! dvdphi(:,q) =             - solVPhi(p,:) * solV(j,p)
    dvdphi(:,q) =             - solVPhi(i,:) * solV(j,p)
    dvdphi(q,:) = dvdphi(q,:) - solVPhi(j,:) * solV(p,i)
  end subroutine
  recursive subroutine hodvdphigen (solVLsO, solVLsOPhi, kv, ku, i, j, p, q, dvdphi) bind(C, name="hodvdphigen_")
    integer(c_int), intent(in)    :: ku, kv, i, j, p, q
    real(c_double), intent(in)    :: solVLsO(ku, ku), solVLsOPhi(ku, kv)
    real(c_double), intent(inout) :: dvdphi(kv, kv)

    ! Precondition: dvdphi is zero
    dvdphi(q,:) =             - solVLso(i,p) * solVLsoPhi(j,:)
    dvdphi(:,q) = dvdphi(:,q) - solVLsO(j,p) * solVLsOPhi(i,:)
  end subroutine

  
  recursive subroutine hodphidphitip (solV, kv, ku, i, j, p, q, dphidphi)    bind(C, name="hodphidphitip_")
    integer(c_int), intent(in)    :: ku, kv, i, j, p, q
    real(c_double), intent(in)    :: solV(ku, ku)
    real(c_double), intent(inout) :: dphidphi(kv, kv)
    ! Precondition: dphidphi contains all zeros.
    dphidphi(q,j) = solV(i,p)
    dphidphi(j,q) = dphidphi(j,q) + solV(p,i)
  end subroutine
  recursive subroutine hodphidphigen (Hto, kv, ku, i,j,p,q, dphidphi)    bind(C, name="hodphidphigen_")
    integer(c_int), intent(in)    :: ku, kv, i, j, p, q
    real(c_double), intent(in)    :: Hto(ku, ku)
    real(c_double), intent(inout) :: dphidphi(kv, kv)
    ! Pre-condition: dphidphi = 0
    dphidphi(q,j) = Hto(i,p)                     ! Notice the transpose.
    dphidphi(j,q) = dphidphi(j,q) + Hto(p,i)
  end subroutine

  recursive subroutine hgamdvdvtip (solVPhi, solV, solVxw, kv,ku, i,j,p,q, dvdv) bind(C, name="hgamdvdvtip_")
    integer(c_int), intent(in)  :: ku, kv, i, j, p, q
    real(c_double), intent(in)  :: solVPhi(ku, kv), solV(ku, ku), solVxw(ku)
    real(c_double), intent(out) :: dvdv(kv)
    dvdv(:) =  solVPhi(p,:) * solV(q,i) * solVxw(j) + solVPhi(i,:) * solV(j,p) * solVxw(q)
  end subroutine
  recursive subroutine hgamdvdvgen (solVLsOPhi, VmVLV, solVLb, kv, ku, i,j,p,q, dvdv) bind(C, name="hgamdvdvgen_")
    integer(c_int), intent(in)  :: kv, ku, i,j,p,q
    real(c_double), intent(in)  :: solVLsOPhi(ku, kv), VmVLV(ku, ku), solVLb(ku)
    real(c_double), intent(out) :: dvdv(kv)
    dvdv(:) =  solVLsOPhi(p,:) * VmVLV(q,i) * solVLb(j) + solVLsOPhi(i,:) * VmVLV(j,p) * solVLb(q)
  end subroutine
  
  recursive subroutine hgamdvdphitip (solV, solVxw, kv, ku, i,j,p,q, dvdphi)    bind(C, name="hgamdvdphitip_")
    integer(c_int), intent(in)    :: ku, kv, i, j, p, q
    real(c_double), intent(in)    :: solV(ku, ku), solVxw(ku)
    real(c_double), intent(inout) :: dvdphi(kv)
    ! Pre-condition: dvdphi is zero
    dvdphi(q) = -solV(p,i) * solVxw(j)
  end subroutine
  recursive subroutine hgamdvdphigen (solVLsO, solVLb, kv, ku, i,j,p,q, dvdphi)   bind(C, name="hgamdvdphigen_")
    integer(c_int), intent(in)    :: kv, ku, i,j,p,q
    real(c_double), intent(in)    :: solVLsO(ku, ku), solVLb(ku)
    real(c_double), intent(inout) :: dvdphi(kv)
    ! Pre-condition: dvdphi = 0
    dvdphi(q) = - solVLsO(i,p) * solVLb(j)
  end subroutine


  recursive subroutine hgamdwdvtip (solVPhi, solV, kv, ku, i,p,q, dwdv)    bind(C, name="hgamdwdvtip_")
    integer(c_int), intent(in)  :: kv, ku, i,p,q
    real(c_double), intent(in)  :: solVPhi(ku, kv), solV(ku, ku)
    real(c_double), intent(out) :: dwdv(kv)
    dwdv(:) = solVPhi(p,:) * solV(q,i)
  end subroutine
  recursive subroutine hgamdwdvgen (solVLsOPhi, solVLsO, kv, ku, i,p,q, dwdv)   bind(C, name="hgamdwdvgen_")
    integer(c_int), intent(in)  :: kv, ku, i,p,q
    real(c_double), intent(in)  :: solVLsOPhi(ku, kv), solVLsO(ku, ku)
    real(c_double), intent(out) :: dwdv(kv)
    dwdv(:) = solVLsOPhi(p,:) * solVLsO(q,i)
  end subroutine


  recursive subroutine hgamdwdphitip (solV, kv, ku, i,p,q, dwdphi)   bind(C, name="hgamdwdphitip_")
    integer(c_int), intent(in)    :: ku, kv, i,p,q
    real(c_double), intent(in)    :: solV(ku, ku)
    real(c_double), intent(inout) :: dwdphi(kv)
    ! Pre-condtion: dwdphi is zero
    dwdphi(q) = -solV(p,i)
  end subroutine
  recursive subroutine hgamdwdphigen (Hto, kv, ku, i,p,q, dwdphi)   bind(C, name="hgamdwdphigen_")
    integer(c_int), intent(in)    :: kv, ku, i,p,q
    real(c_double), intent(in)    :: Hto(ku, ku)
    real(c_double), intent(inout) :: dwdphi(kv)
    ! Pre-condition: dphidw = 0
    dwdphi(q) = - Hto(p,i)
  end subroutine


  recursive subroutine hcdwdwtip (solV, ku, i,p, dwdw)  bind(C, name="hcdwdwtip_")
    integer(c_int), intent(in)  :: ku, i,p
    real(c_double), intent(in)  :: solV(ku, ku)
    real(c_double), intent(out) :: dwdw
    dwdw = 2.0_c_double * solV(i,p)
  end subroutine
  recursive subroutine hcdwdwgen (Hto, ku, i,p, dwdw)  bind(C, name="hcdwdwgen_")
    integer(c_int), intent(in)  :: ku, i,p
    real(c_double), intent(in)  :: Hto(ku, ku)
    real(c_double), intent(out) :: dwdw
    dwdw = 2.0_c_double * Hto(i,p)
  end subroutine
  
  recursive subroutine hcdwdvtip (solVxw, solV, ku, i,p,q, dwdv)  bind(C, name="hcdwdvtip_")
    integer(c_int), intent(in)  :: ku, i,p,q
    real(c_double), intent(in)  :: solV(ku, ku), solVxw(ku)
    real(c_double), intent(out) :: dwdv
!    dwdv = 2.0_c_double * solV(i,p) * solVxw(q)
    dwdv = solV(q,i) * solVxw(p) + solV(p,i) * solVxw(q)
!    dwdv = 0_c_double
  end subroutine
  recursive subroutine hcdwdvgen (solVLb, solVLsO, ku, i,p,q, dwdv)  bind(C, name="hcdwdvgen_")
    integer(c_int), intent(in)  :: ku, i,p,q
    real(c_double), intent(in)  :: solVLsO(ku, ku), solVLb(ku)
    real(c_double), intent(out) :: dwdv
!    dwdv = 2.0_c_double * solVLsO(i,p) * solVLb(q)
    dwdv = solVLsO(q,i) * solVLb(p) + solVLsO(p,i) * solVLb(q)
!    dwdv = 0_c_double
  end subroutine

  recursive subroutine hcdvdvtip (solVxw, solV, ku, i,j,p,q, dvdv) bind(C, name="hcdvdvtip_")
    integer(c_int), intent(in)  :: ku, i,j,p,q
    real(c_double), intent(in)  :: solV(ku, ku), solVxw(ku)
    real(c_double), intent(out) :: dvdv
    dvdv = solVxw(p) * solV(q,i) * solVxw(j) + solVxw(i) * solV(j,p) * solVxw(q)
  end subroutine
  recursive subroutine hcdvdvgen (solVLb, VmVLV, ku, i,j,p,q, dvdv) bind(C, name="hcdvdvgen_")
    integer(c_int), intent(in)  :: ku, i,j,p,q
    real(c_double), intent(in)  :: VmVLV(ku, ku), solVLb(ku)
    real(c_double), intent(out) :: dvdv
    dvdv = solVLb(p) * VmVLV(q,i) * solVLb(j) + solVLb(i) * VmVLV(j,p) * solVLb(q)
  end subroutine

  recursive subroutine hddvdvtip (solV, ku, i,j,p,q, dvdv) bind(c, name="hddvdvtip_")
    integer(c_int), intent(in)  :: ku, i,j,p,q
    real(c_double), intent(in)  :: solV(ku, ku)
    real(c_double), intent(out) :: dvdv
    dvdv = -solV(i,p) * solV(q,j)
  end subroutine
  recursive subroutine hddvdvgen (VmVLV, ku, i,j,p,q, dvdv) bind(c, name="hddvdvgen_")
    integer(c_int), intent(in)  :: ku, i,j,p,q
    real(c_double), intent(in)  :: VmVLV(ku, ku)
    real(c_double), intent(out) :: dvdv
    dvdv = -VmVLV(i,p) * VmVLV(q,j)
  end subroutine

  recursive subroutine hlchainrule (x0, ho, hgam, hc, hd, kr, out) bind(c, name="hlchainrule_")
    real(c_double) x0, ho, hgam, hc, hd, out
    integer(c_int) kr
!   intent(in) :: x0, ho, hgam, hc, hd, kr;   intent(out) :: out
    dimension ho(kr,kr), hgam(kr), x0(kr)
    real(c_double), allocatable :: tmp(:)
    allocate(tmp(kr))
    tmp = hgam
    call dgemv('N',kr,kr,-0.5_c_double,ho,kr,x0,1_c_int,1.0_c_double,tmp,1_c_int)
    out = ddot(kr, x0, 1_c_int, tmp, 1_c_int) - (hc + hd) / 2.0_c_double
    deallocate(tmp)
  end subroutine

  recursive subroutine hselfbktip(solV, x, w, Phi, kv, ku, solVPhi, solVxw) bind(C, name="hselfbktip_")
    real(c_double) solV, x, w, Phi, solVPhi, solVxw
    integer(c_int) kv, ku
    intent(in) :: solV, x, w, Phi, kv, ku;   intent(out) :: solVPhi, solVxw
    dimension solV(ku,ku), x(ku), w(ku), Phi(ku,kv), solVPhi(ku,kv), solVxw(ku)
    real(c_double), allocatable :: tmp(:)
    allocate(tmp(ku))
    call dgemm('N','N',ku,kv,ku,1.0_c_double,solV,ku,Phi,ku,0.0_c_double,solVPhi,ku)
    tmp = x - w
    call dgemv('N',ku,ku,1.0_c_double,solV,ku,tmp,1_c_int,0.0_c_double,solVxw,1_c_int)
    !solVPhi = matmul(solV, Phi)
    !solVxw  = matmul(solV, x - w)
    deallocate(tmp)
  end subroutine

  recursive subroutine hselfbkgen(solV, Lamb, sO, Phi, b, H, kv, ku, &
       & solVLsO, solVLsOPhi, VmVLV, solVLb, Hto) bind(C, name="hselfbkgen_")
    integer(c_int) kv, ku
    real(c_double) solV, Lamb, sO, Phi, b, H, &
         & solVLsO, VmVLV, solVLb, Hto
!   intent(in)  :: solV, Lamb, sO, Phi, b, H, kv, ku
!   intent(out) :: solVLsO, solVLsOPhi, VmVLV, solVLb, Hto
    dimension solV(ku,ku), Lamb(ku,ku), sO(ku,ku), Phi(ku,kv), b(ku), H(ku,ku), &
         & solVLsO(ku,ku), solVLsOPhi(ku*kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku)!, &
         !& solVL(ku,ku)
    real(c_double), target :: solVLsOPhi
    real(c_double), pointer :: solVL(:,:)
    solVL(1:ku,1:ku) => solVLsOPhi(1:)
    !! Use solVLsOPhi to store solV
    call dgemm('N','N',ku,ku,ku,1.0_c_double,solV,ku,Lamb,ku,0.0_c_double,solVL,ku)
    VmVLV = solV
    call dgemm('N','N',ku,ku,ku,-1.0_c_double,solVL,ku,solV,ku,1.0_c_double,VmVLV,ku)
    call dgemm('T','N',ku,ku,ku,1.0_c_double,H,ku,sO,ku,0.0_c_double,Hto,ku)
    call dgemv('N',ku,ku,1.0_c_double,solVL,ku,b,1_c_int,0.0_c_double,solVLb,1_c_int)
    call dgemm('N','N',ku,ku,ku,1.0_c_double,solVL,ku,sO,ku,0.0_c_double,solVLsO,ku)
    !! -- Stop messing with solVLsOPhi
    call dgemm('N','N',ku,kv,ku,1.0_c_double,solVLsO,ku,Phi,ku,0.0_c_double,solVLsOPhi,ku)
    
    !solVL      = matmul(solV, Lamb)
    !solVLb     = matmul(solVL, b)
    !solVLsO    = matmul(solVL, sO)
    !solVLsOPhi = matmul(solVLsO, Phi)
    !VmVLV      = solV - matmul(solVL, solV)
    !Hto        = matmul(transpose(H), sO)
  end subroutine

  !
  ! TODO: all the solVLsOPhi etc. are slow. I should make it the transpose so memory access can be faster.
  !
  recursive subroutine dbledifftopgen(ictx, i,j,m,n, kr,kv,ku, solVLsO,solVLsOPhi,VmVLV,solVLb,Hto,x0,d2L) &
       & bind(C, name="dbledifftopgen_")
    integer(c_int) ictx, i, j, m, n, kr, kv, ku
    real(c_double) solVLsO,solVLsOPhi,VmVLV,solVLb,Hto,x0,d2L
    dimension solVLsO(ku,ku), solVLsOPhi(ku,kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku), x0(kr)
    allocatable :: ho(:,:), hgam(:)
    allocate(ho(kv,kv), hgam(kv))
    call ddsfgen(ictx, i,j,m,n, kv,ku, solVLsO, solVLsOPhi, VmVLV, solVLb, Hto,ho,hgam,hc,hd)
    call hlchainrule (x0, ho, hgam, hc, hd, kr, d2L)
    deallocate(ho, hgam)
  end subroutine
  recursive subroutine dbledifftoptip(ictx, i,j,m,n, kr,kv,ku, solV, solVPhi, solVxw, x0, d2L) &
       & bind(C, name="dbledifftoptip_")
    integer(c_int) ictx, i,j,m,n, kr,kv,ku
    real(c_double) solV, solVPhi, solVxw, x0, d2L
    dimension solV(ku,ku), solVPhi(ku,kv), solVxw(ku), x0(kr)
    allocatable :: ho(:,:), hgam(:)
    allocate(ho(kv,kv), hgam(kv))
    call ddsftip(ictx, i,j,m,n, kv,ku, solV, solVPhi, solVxw, ho,hgam,hc,hd)
    call hlchainrule (x0, ho, hgam, hc, hd, kr, d2L)
    deallocate(ho, hgam)
  end subroutine
  
  recursive subroutine ddsfgen(ictx, i,j,m,n, kv,ku, &
       & solVLsO, solVLsOPhi, VmVLV, solVLb, Hto, ho, hgam, hc, hd) bind(C, name="ddsfgen_")
    integer(c_int) ictx, i,j,m,n, kv,ku
    real(c_double) solVLsO, solVLsOPhi, VmVLV, solVLb, Hto, ho, hgam, hc, hd
!   intent(in)  :: ictx, i,j,m,n, kv,ku, solVLsO, solVLsOPhi, VmVLV, solVLb, Hto;  intent(out) :: ho, hgam, hc, hd
    dimension solVLsO(ku,ku), solVLsOPhi(ku,kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku), ho(kv,kv), hgam(kv)
    ho(:,:)= 0_c_double;  hgam(:)= 0_c_double;  hc= 0_c_double;  hd= 0_c_double
    select case (ictx)
    case (IVV)
       call hodvdvgen   (solVLsOPhi, VmVLV, kv, ku, i,j,m,n, ho)
       call hgamdvdvgen (solVLsOPhi, VmVLV, solVLb, kv, ku, i,j,m,n, hgam)
       call hcdvdvgen   (solVLb,     VmVLV, ku, i,j,m,n, hc)
       call hddvdvgen   (VmVLV,      ku, i,j,m,n, hd)
    case (IVPHI)
       call hodvdphigen   (solVLsO, solVLsOPhi, kv, ku, i,j,m,n, ho)
       call hgamdvdphigen (solVLsO, solVLb, kv, ku, i,j,m,n, hgam)
    case (IVW)
       call hgamdwdvgen (solVLsOPhi, solVLsO, kv, ku, m,i,j, hgam)
       call hcdwdvgen   (solVLb, solVLsO, ku, m,i,j, hc)
    case (IPHIPHI)
       call hodphidphigen (Hto, kv, ku, i,j,m,n, ho)
    case (IPHIW)
       call hgamdwdphigen (Hto, kv, ku, m,i,j, hgam)
    case (IWW)
       call hcdwdwgen (Hto, ku, i,m, hc)
    end select
  end subroutine

  recursive subroutine ddsftip(ictx, i,j,m,n, kv,ku, solV, solVPhi, solVxw, ho,hgam,hc,hd) bind(C, name="ddsftip_")
    integer(c_int) ictx, i,j,m,n, kv,ku
    real(c_double) solV, solVPhi, solVxw, ho,hgam,hc,hd
!   intent(in)  :: ictx, i,j,m,n,kv,ku, solV, solVPhi, solVxw;  intent(out) :: ho,hgam,hc,hd
    dimension solV(ku,ku), solVPhi(ku,kv), solVxw(ku), ho(kv,kv), hgam(kv)
    ho(:,:)= 0_c_double;  hgam(:)= 0_c_double;  hc= 0_c_double;  hd= 0_c_double
    select case (ictx)
    case (IVV)
       call hodvdvtip   (solVPhi, solV, kv, ku, i,j,m,n, ho)
       call hgamdvdvtip (solVPhi, solV, solVxw, kv, ku, i,j,m,n, hgam)
       call hcdvdvtip   (solVxw,  solV, ku, i,j,m,n, hc)
       call hddvdvtip   (solV, ku, i,j,m,n, hd)
    case (IVPHI)
       call hodvdphitip   (solV, solVPhi, kv, ku, i,j,m,n, ho)
       call hgamdvdphitip (solV, solVxw,  kv, ku, i,j,m,n, hgam)
    case (IVW)
       call hgamdwdvtip (solVPhi, solV, kv, ku, m,i,j, hgam)
       call hcdwdvtip   (solVxw,  solV, ku, m,i,j, hc)
    case (IPHIPHI)
       call hodphidphitip (solV, kv, ku, i,j,m,n, ho)
    case (IPHIW)
       call hgamdwdphitip (solV, kv, ku, m,i,j, hgam)
    case (IWW)
       call hcdwdwtip (solV, ku, i,m, hc)
    end select
  end subroutine
  
  recursive subroutine symhessvv(i,j,m,n, dlijmn,dljimn,dljinm,dlijnm, dl) bind(C, name="symhessvv_")
    real(c_double) dlijmn, dljimn, dljinm, dlijnm, dl
    integer(c_int) i, j, m, n
    if (i /= j) then
       if (m /= n) then
          dl = dlijmn + dlijnm + dljimn + dljinm
       else
          dl = dlijmn + dljimn
       endif
    else
       if (m /= n) then
          dl = dlijmn + dlijnm
       else
          dl = dlijmn
       endif
    endif
    !! The folloing is a direct translation from the formula on paper...
!     dl = dlijmn + dlijnm + dljimn + dljinm
!     if (m /= n) goto 10
!     dl = dl-dlijmn
!     dl = dl-dljimn
! 10  if (i /= j) goto 20
!     dl = dl-dlijmn
!     dl = dl-dlijnm
! 20  if ((i .eq. j) .and. (m .eq. n)) dl=dl+dlijmn
  end subroutine
  recursive subroutine symhessvany(i,j, dlijany,dljiany, dl) bind(C, name="symhessvany_")
    real(c_double) dlijany, dljiany, dl
    integer(c_int) i, j
    if (i /= j) then
       dl=dlijany+dljiany
    else
       dl=dlijany
    endif
  end subroutine

  recursive subroutine initfalfm_beta(falfm_c, fmg_c, kbu, kmv) bind(C, name="initfalfm_beta_")
    implicit none
    integer(c_int) :: kbu, kmv
    type(c_ptr) :: falfm_c, fmg_c
    type(c_ptr) :: i_c
    type(llst), pointer :: falfm_p, fmg_p
    real(c_double), pointer :: lamb_beta(:,:)
    real(c_double), allocatable :: lamb(:,:)
    call c_f_pointer(falfm_c, falfm_p);    lamb_beta(1:kbu,1:kbu) => falfm_p%dat(1:)
    call c_f_pointer(fmg_c, fmg_p)
    allocate(lamb(kbu,kbu))
    lamb(:,:) = lamb_beta(:,:) ! Copy to new memory so it doesn't screw up dgemm
    i_c = fmg_p%nxt            ! Can be NULL if m is a direct child of beta
    if (c_associated(i_c)) then
       call c_f_pointer(i_c, fmg_p);
       call dgemm('N','T',kbu,kmv,kbu,1.0_c_double,lamb,kbu,fmg_p%dat,kmv,0.0_c_double,falfm_p%dat,kbu)
       !falfm = matmul(lamb, transpose(fmg))
    endif
    deallocate(lamb)
  end subroutine

  ! The fmlfm, etc which is used in dqmmkda comes from here before calling the next iteration.
  ! fmlfm_new needs to be 4 dimensional after the update. 
  recursive subroutine updategbk(kv, ku, fmlfm_c, fmlfm_new, fm_c, fm_new, qm_c, qm_new, &
       & Lamb, HPhi, a, mdim)  bind(C, name="updategbk_")
    implicit none
    integer(c_int) :: kv,ku, mdim
    type(c_ptr) :: fmlfm_c, fm_c, qm_c
    real(c_double) :: Lamb(ku,ku), HPhi(ku,kv), a(ku)
    type(c_ptr) :: fmlfm_new, fm_new, qm_new
!   type(c_ptr), intent(out) :: fmlfm_new, fm_new, qm_new
    type(llst), pointer :: tmp_p
    real(c_double), pointer :: dcur(:,:), dnew(:,:), qcur(:), qnew(:), &
                                         & tmpkvkthis(:,:)
    type(c_ptr) :: i_c
    real(c_double), allocatable :: tmpkvku(:,:)
    real(c_double), allocatable, target :: stkvkmax(:)
    allocate(tmpkvku(kv,ku), stkvkmax(kv*mdim))

    i_c = fmlfm_c
10  call c_f_pointer(i_c, tmp_p);
    dcur(1:kv,1:kv) => tmp_p%dat
    dnew(1:ku,1:ku) => tmp_p%dat
    call dgemm('N','T',kv,ku,kv,1.0_c_double,dcur,kv,HPhi,ku,0.0_c_double,tmpkvku,kv)
    call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,tmpkvku,kv,0.0_c_double,dnew,ku)
    !dnew = matmul(HPhi, matmul(dcur, transpose(HPhi)))
    tmp_p%siz = ku
    if (c_associated(tmp_p%nxt)) then
       i_c = tmp_p%nxt
       goto 10
    endif
    tmp_p%nxt = fmlfm_new
    call c_f_pointer(fmlfm_new, tmp_p)
    dnew(1:ku,1:ku) => tmp_p%dat
    dnew(1:ku,1:ku) = Lamb
    tmp_p%siz = ku

    i_c = fm_c
20  call c_f_pointer(i_c, tmp_p)
    tmpkvkthis(1:kv,1:tmp_p%siz) => stkvkmax(1:(kv*(tmp_p%siz)))
    dcur(1:kv,1:tmp_p%siz) => tmp_p%dat
    dnew(1:ku,1:tmp_p%siz) => tmp_p%dat
    tmpkvkthis = dcur
    call dgemm('N','N',ku,tmp_p%siz,kv,1.0_c_double,HPhi,ku,tmpkvkthis,kv,0.0_c_double,dnew,ku)
    !dnew = matmul(HPhi, tmpkvkthis)
    if (c_associated(tmp_p%nxt)) then
       i_c = tmp_p%nxt
       goto 20
    endif
    tmp_p%nxt = fm_new
    call c_f_pointer(fm_new, tmp_p)
    dnew(1:ku,1:kv) => tmp_p%dat
    dnew(1:ku,1:kv) = HPhi
    tmp_p%siz = kv

    i_c = qm_c
30  call c_f_pointer(i_c, tmp_p)
    qcur(1:kv) => tmp_p%dat
    qnew(1:ku) => tmp_p%dat
    tmpkvku(:,1) = qcur(:)
    qnew = a
    call dgemv('N',ku,kv,1.0_c_double,HPhi,ku,tmpkvku(:,1),1_c_int,1.0_c_double,qnew,1_c_int)
    !qnew = matmul(HPhi, qcur) + a
    tmp_p%siz = ku
    if (c_associated(tmp_p%nxt)) then
       i_c = tmp_p%nxt
       goto 30
    endif
    tmp_p%nxt = qm_new
    call c_f_pointer(qm_new, tmp_p)
    qnew(1:ku) => tmp_p%dat
    qnew(1:ku) = a
    tmp_p%siz = ku
    deallocate(tmpkvku, stkvkmax)
  end subroutine

  recursive subroutine d2Lijmn(ictx, i,j,m,n, kr,kv,ku, q1m, fm, dFm1, dqm1, dkm1, k, dodtn, dgamdtn, &
       & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, d2L)
    integer(c_int) :: ictx, istip, i,j,m,n,kr,kv,ku
    !!real(c_double), pointer :: fm(:,:), q1m(:)
    real(c_double), pointer :: fm(:,:), q1m(:)
    real(c_double) :: ho(kv,kv), hgam(kv), hc, hd
    real(c_double) :: ho1(kr,kr), hgam1(kr), hc1, hd1,   dqm1(kv), dkm1(kv,kv), dFm1(kv,kr), k(kv,kv)
    real(c_double) :: x0(kr), dodtn(kv,kv), dgamdtn(kv)
    real(c_double) :: solVLsO(ku,ku), solVLsOPhi(ku,kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku)
    !! If istip:      solV          , solVPhi,         , IGNORED,      solVxw,     IGNORED
    real(c_double) :: d2L
    real(c_double), allocatable :: tmpkvkr(:,:)
    allocate(tmpkvkr(kv,kr))
    if (istip == 1_c_int) then
       call ddsfgen(ictx, i,j,m,n, kv,ku, solVLsO, solVLsOPhi, VmVLV, solVLB, Hto, ho, hgam, hc, hd)
    else
       call ddsftip(ictx, i,j,m,n, kv,ku, solVLsO, solVLsOPhi, solVLB,             ho, hgam, hc, hd)
    endif
    ! -- Compute ho1
    call dgemm('N','N',kv,kr,kv,1.0_c_double,dodtn,kv,fm,kv,0.0_c_double,tmpkvkr,kv)
    call dgemm('T','N',kr,kr,kv,1.0_c_double,dFm1,kv,tmpkvkr,kv,0.0_c_double,ho1,kr)
    call dgemm('N','N',kv,kr,kv,1.0_c_double,ho,kv,fm,kv,0.0_c_double,tmpkvkr,kv)
    call dgemm('T','N',kr,kr,kv,1.0_c_double,fm,kv,tmpkvkr,kv,1.0_c_double,ho1,kr)
    call dgemm('N','N',kv,kr,kv,1.0_c_double,dodtn,kv,dFm1,kv,0.0_c_double,tmpkvkr,kv)
    call dgemm('T','N',kr,kr,kv,1.0_c_double,fm,kv,tmpkvkr,kv,1.0_c_double,ho1,kr)
    
    ! -- Compute hgam1
    tmpkvkr(:,1) = dgamdtn
    call dgemv('N',kv,kv,-1.0_c_double,dodtn,kv,q1m,1_c_int,1.0_c_double,tmpkvkr(:,1),1_c_int)
    call dgemv('T',kv,kr, 1.0_c_double,dFm1,kv,tmpkvkr(:,1),1_c_int,0.0_c_double,hgam1,1_c_int)
    tmpkvkr(:,1) = hgam
    call dgemv('N',kv,kv,-1.0_c_double,dodtn,kv,dqm1,1_c_int,1.0_c_double,tmpkvkr(:,1),1_c_int)
    ! -- just BTW update this value into hc1. This is hgam - matmul(dodtn,dqm1) in tmpkvkr(:,1)
    hc1 = hc -2.0_c_double * ddot(kv, q1m, 1_c_int, tmpkvkr(:,1), 1_c_int)
    call dgemv('N',kv,kv,-1.0_c_double,ho,kv,q1m,1_c_int,1.0_c_double,tmpkvkr(:,1),1_c_int)
    call dgemv('T',kv,kr, 1.0_c_double,fm,kv,tmpkvkr(:,1),1_c_int,1.0_c_double,hgam1,1_c_int)

    ! -- Compute hc1
    hc1 = hc1 - 2.0_c_double * ddot(kv, dqm1, 1_c_int, dgamdtn, 1_c_int)
    call dgemv('N',kv,kv,1.0_c_double,ho,kv,q1m,1_c_int,0.0_c_double,tmpkvkr(:,1),1_c_int)
    hc1 = hc1 + ddot(kv, q1m, 1_c_int, tmpkvkr(:,1), 1_c_int)

    ! -- Compute hd1
    hd1 = hd
    do jj = 1, kv
      do ii = 1, kv
         hd1 = hd1 + ho(jj,ii)*k(ii,jj) + dodtn(jj,ii) * dkm1(ii,jj)
      end do
    end do
    !hd1 = hd + sum(transpose(dodtn) * dkm1)
    ! -- Compute hc1
    !ho1 =  matmul(transpose(dFm1), matmul(dodtn, fm)) + matmul(transpose(fm), matmul(ho, fm)) + &
    !     & matmul(transpose(fm),   matmul(dodtn, dFm1))
    !hgam1 = matmul(transpose(dFm1), dgamdtn - matmul(dodtn, q1m)) &
    !     & + matmul(transpose(fm), hgam - matmul(ho, q1m) - matmul(dodtn, dqm1))
    !hc1 = hc + dot_product(q1m, 2.0_c_double*(matmul(dodtn,dqm1) - hgam) + matmul(ho,q1m)) &
    !     & - 2.0_c_double * dot_product(dqm1,dgamdtn)
    !hd1 = hd + sum(transpose(ho) * k) + sum(transpose(dodtn) * dkm1)
    call hlchainrule (x0, ho1, hgam1, hc1, hd1, kr, d2L)
    deallocate(tmpkvkr)
  end subroutine



  ! NEED TO MAKE ABSOLUTELY SURE NO REPETITION CAN HAPPEN BECAUSE WE ARE DEALING WITH THE CROSS OF THE
  ! SAME NODE!
  recursive subroutine tmtmdir(kr,kv,ku, fmlfm_c, qm_c, fm_c, a_c, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dfqk1_ch, K, &
       & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, bilinmat, dir, ndir, derivlen, adphi, adV, adw) bind(C, name="tmtmdir_")
    implicit none
    type(c_ptr) :: fmlfm_c, qm_c, fm_c, a_c
    integer(c_int)  :: istip, i,j,m,n,kr,kv,ku, ndir
    integer(c_long) :: adphi,adV,adw,derivlen,   idx1,idx2
    real(c_double) :: bilinmat(ndir,ndir), dir(derivlen,ndir)
    real(c_double) :: dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv)
    real(c_double) :: x0(kr)
    real(c_double) :: dlijmn, dljimn, dljinm, dlijnm, hessv
    real(c_double) :: kvkrzero(kv,kr), kvzero(kv,kv), K(kv,kv)
    type(dfdqdkCMEM) :: dfqk1_ch
    type(dfdqdk) :: dfqk1
    type(llst), pointer :: it_p
    !!real(c_double), pointer :: f1m(:,:), q1m(:)
    real(c_double), pointer :: f1m(:,:), q1m(:)
    real(c_double) :: solVLsO(ku,ku), solVLsOPhi(ku,kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku)
    !! If istip:      solV          , solVPhi,         , IGNORED,      solVxw,     IGNORED
    kvzero = 0.0_c_double;       kvkrzero = 0.0_c_double
    call read_dfqk(dfqk1_ch, dfqk1)
    call dqmmkda(fmlfm_c, qm_c, fm_c, a_c, kr, kv, ku, dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dfqk1, K)
    call c_f_pointer(fm_c, it_p);  f1m(1:kv,1:kr)=>it_p%dat     ! 1st elem. is always F1_m
    call c_f_pointer(qm_c, it_p);  q1m(1:kv)=>it_p%dat          ! 1st elem. is always q1_m
    do n=1,ku                                                   ! V-V
       do m=n,ku
          i=m; j=n
15        idx1 = adV+iijtouplolidx(ku,i,j)
          idx2 = adV+iijtouplolidx(ku,m,n)
          !! TODO: Cut this duplication to potentially gain 20% (?) of execution time!
          call d2Lijmn(IVV, i,j,m,n, kr,kv,ku, q1m, f1m,  &
               & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodvev(:,:,m,n),dgamdvev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
          call d2Lijmn(IVV, j,i,m,n, kr,kv,ku, q1m, f1m,  &
               & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodvev(:,:,m,n),dgamdvev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
          call d2Lijmn(IVV, j,i,n,m, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodvev(:,:,n,m),dgamdvev(:,n,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljinm)
          call d2Lijmn(IVV, i,j,n,m, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodvev(:,:,n,m),dgamdvev(:,n,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijnm)
          call symhessvv(i,j,m,n,dlijmn,dljimn,dljinm,dlijnm, hessv)
          call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
          i = i+1
          if (i <= ku) goto 15
          j = j+1
          if (j <= ku) then
             i = j
             goto 15
          endif
       enddo
    enddo
    do n=1,kv                   ! Phi-V
       do m=1,ku
          do j=1,ku
             do i=j,ku
                idx1 = adV + iijtouplolidx(ku, i, j)
                idx2 = adPhi + (n-1) * ku + m
                call d2Lijmn(IVPHI, i,j,m,n, kr,kv,ku, q1m, f1m, &
                     & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodphiev(:,:,m,n),dgamdphiev(:,m,n),&
                     & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
                call d2Lijmn(IVPHI, j,i,m,n, kr,kv,ku, q1m, f1m, &
                     & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodphiev(:,:,m,n),dgamdphiev(:,m,n),&
                     & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
                call symhessvany(i,j,dlijmn,dljimn, hessv)
                call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
             enddo
          enddo
       enddo
    enddo
    
    do m=1,ku
       do j=1,ku
          do i=j,ku             ! w-V
             idx1 = adV + iijtouplolidx(ku, i, j)
             idx2 = adw + m
             call d2Lijmn(IVW, i,j,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, kvzero,dgamdwev(:,m),&
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
             call d2Lijmn(IVW, j,i,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, kvzero,dgamdwev(:,m),&
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
             call symhessvany(i,j,dlijmn,dljimn, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
          enddo
       enddo
    enddo
    do n=1,kv                   !Phi-Phi
       do m=1,ku;
          i=m; j=n
45        idx1 = adPhi + (j-1) * ku + i
          idx2 = adPhi + (n-1) * ku + m
          call d2Lijmn(IPHIPHI, i,j,m,n, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), K, &
               & dodphiev(:,:,m,n),dgamdphiev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessv)
          call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
          i=i+1
          if (i <= ku) goto 45
          j=j+1
          if (j <= kv) then
             i=1
             goto 45
          endif
       enddo
    enddo
    do m=1,ku                   !Phi-w
       do j=1,kv
          do i=1,ku
             idx1 = adw + m
             idx2 = adPhi + (j-1) * ku + i
             call d2Lijmn(IPHIW, i,j,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), K, &
                  & kvzero, dgamdwev(:,m), &
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
          enddo
       enddo
    enddo
    do m=1,ku                   !w-w
       do i=m,ku
          idx1 = adw + i
          idx2 = adw + m
          call d2Lijmn(IWW, i,i,m,m, kr,kv,ku, q1m, f1m, &
               & kvkrzero, dfqk1%dqdw(:,i), kvzero, K, kvzero,dgamdwev(:,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessv)
          call bilinupdt(hessv, bilinmat, derivlen, idx1, idx2, dir, ndir)
       enddo
    enddo
  end subroutine

  recursive subroutine tmtm2(kr,kv,ku, fmlfm_c, qm_c, fm_c, a_c, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dfqk1_ch, K, &
       & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessflat, derivlen, adphi, adV, adw) bind(C, name="tmtm2_")
    implicit none
    type(c_ptr) :: fmlfm_c, qm_c, fm_c, a_c
    integer(c_int)  :: istip, i,j,m,n,kr,kv,ku
    integer(c_long) :: adphi,adV,adw,hfidx, derivlen
    real(c_double) :: hessflat((derivlen*(derivlen+1))/2_c_int)
    real(c_double) :: dodvev(kv, kv, ku, ku), dodphiev(kv, kv, ku, kv), &
         & dgamdvev(kv, ku, ku), dgamdwev(kv, ku), dgamdphiev(kv, ku, kv)
    real(c_double) :: x0(kr)
    real(c_double) :: dlijmn, dljimn, dljinm, dlijnm
    real(c_double) :: kvkrzero(kv,kr), kvzero(kv,kv), K(kv,kv)
    type(dfdqdkCMEM) :: dfqk1_ch
    type(dfdqdk) :: dfqk1
    type(llst), pointer :: it_p
    !!real(c_double), pointer :: f1m(:,:), q1m(:)
    real(c_double), pointer :: f1m(:,:), q1m(:)
    real(c_double) :: solVLsO(ku,ku), solVLsOPhi(ku,kv), VmVLV(ku,ku), solVLb(ku), Hto(ku,ku)
    !! If istip:      solV          , solVPhi,         , IGNORED,      solVxw,     IGNORED
    kvzero = 0.0_c_double;       kvkrzero = 0.0_c_double
    call read_dfqk(dfqk1_ch, dfqk1)
    call dqmmkda(fmlfm_c, qm_c, fm_c, a_c, kr, kv, ku, dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, dfqk1, K)
    call c_f_pointer(fm_c, it_p);  f1m(1:kv,1:kr)=>it_p%dat     ! 1st elem. is always F1_m
    call c_f_pointer(qm_c, it_p);  q1m(1:kv)=>it_p%dat          ! 1st elem. is always q1_m
    do n=1,ku
       do m=n,ku
          i=m; j=n
15        hfidx = ijtouplolidx(derivlen, adV+iijtouplolidx(ku,i,j), adV+iijtouplolidx(ku,m,n))
          !! TODO: Cut this duplication to potentially gain 20% (?) of execution time!
          call d2Lijmn(IVV, i,j,m,n, kr,kv,ku, q1m, f1m,  &
               & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodvev(:,:,m,n),dgamdvev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
          call d2Lijmn(IVV, j,i,m,n, kr,kv,ku, q1m, f1m,  &
               & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodvev(:,:,m,n),dgamdvev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
          call d2Lijmn(IVV, j,i,n,m, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodvev(:,:,n,m),dgamdvev(:,n,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljinm)
          call d2Lijmn(IVV, i,j,n,m, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodvev(:,:,n,m),dgamdvev(:,n,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijnm)
          call symhessvv(i,j,m,n,dlijmn,dljimn,dljinm,dlijnm, hessflat(hfidx))
          i = i+1
          if (i <= ku) goto 15
          j = j+1
          if (j <= ku) then
             i = j
             goto 15
          endif
       enddo
    enddo
    do n=1,kv
       do m=1,ku
          do j=1,ku
             do i=j,ku
                hfidx = ijtouplolidx(derivlen, adV + iijtouplolidx(ku, i, j), adPhi + (n-1) * ku + m)
                call d2Lijmn(IVPHI, i,j,m,n, kr,kv,ku, q1m, f1m, &
                     & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, dodphiev(:,:,m,n),dgamdphiev(:,m,n),&
                     & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
                call d2Lijmn(IVPHI, j,i,m,n, kr,kv,ku, q1m, f1m, &
                     & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, dodphiev(:,:,m,n),dgamdphiev(:,m,n),&
                     & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
                call symhessvany(i,j,dlijmn,dljimn, hessflat(hfidx))
             enddo
          enddo
       enddo
    enddo
    
    do m=1,ku
       do j=1,ku
          do i=j,ku
             hfidx = ijtouplolidx(derivlen, adV + iijtouplolidx(ku, i, j), adw + m)
             call d2Lijmn(IVW, i,j,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), K, kvzero,dgamdwev(:,m),&
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dlijmn)
             call d2Lijmn(IVW, j,i,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), K, kvzero,dgamdwev(:,m),&
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto,dljimn)
             call symhessvany(i,j,dlijmn,dljimn, hessflat(hfidx))
          enddo
       enddo
    enddo
    do n=1,kv
       do m=1,ku;
          i=m; j=n
45        hfidx = ijtouplolidx(derivlen, adPhi + (j-1) * ku + i, adPhi + (n-1) * ku + m)
          call d2Lijmn(IPHIPHI, i,j,m,n, kr,kv,ku, q1m, f1m, &
               & dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), K, &
               & dodphiev(:,:,m,n),dgamdphiev(:,m,n), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessflat(hfidx))
          i=i+1
          if (i <= ku) goto 45
          j=j+1
          if (j <= kv) then
             i=1
             goto 45
          endif
       enddo
    enddo
    do m=1,ku
       do j=1,kv
          do i=1,ku
             hfidx = ijtouplolidx(derivlen, adw + m, adPhi + (j-1) * ku + i)
             call d2Lijmn(IPHIW, i,j,m,m, kr,kv,ku, q1m, f1m, &
                  & dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), K, &
                  & kvzero, dgamdwev(:,m), &
                  & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessflat(hfidx))
          enddo
       enddo
    enddo
    do m=1,ku
       do i=m,ku
          hfidx = ijtouplolidx(derivlen, adw + i, adw + m)
          call d2Lijmn(IWW, i,i,m,m, kr,kv,ku, q1m, f1m, &
               & kvkrzero, dfqk1%dqdw(:,i), kvzero, K, kvzero,dgamdwev(:,m), &
               & x0, istip,solVLsO,solVLsOPhi,VmVLV,solVLB,Hto, hessflat(hfidx))
       enddo
    enddo
  end subroutine

  ! ONE OF THE BOTTLENECK FUNCTION!!!
  recursive subroutine dqmmkda(fmlfm_c, qm_c, fm_c, a_c, kr,kv,ku,   &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, &
       & dfqk1, k)
    implicit none
    type(dfdqdk), intent(inout) :: dfqk1
    type(c_ptr)  :: a_c, fmlfm_c, qm_c, fm_c
    integer(c_int)  :: ku, kv, kr
    type(llstptr), pointer :: a_p
    type(llst),    pointer :: fm_p,   fmlfm_p,   qm_p
    real(c_double), pointer :: a(:), fm(:,:), qm(:), fmlfm(:,:), tailsum_fmlfm(:,:)
    type(c_ptr) ::  i_c,j_c,k_c,r_c
    real(c_double), target :: k(kv,kv)
!   real(c_double), intent(out), target :: k(kv,kv)
    type(c_ptr), allocatable :: stfm(:), sta(:), stfmlfm(:)
    real(c_double), allocatable :: tmpkvkv(:,:), tmpkvkv2(:,:)
    real(c_double), allocatable, target :: tmpkr(:)
    real(c_double), pointer :: tmpkts(:) ! points to tmpkr with varying sizes
    integer(c_int) :: i,m,n
    real(c_double) :: dodvev(kv,kv,ku,ku), dodphiev(kv,kv,ku,kv), dgamdvev(kv,ku,ku), &
         & dgamdwev(kv,ku), dgamdphiev(kv,ku,kv)
    allocate(stfm(1660000), sta(1660000), stfmlfm(1660000))     ! 50 MBs. 
    allocate(tmpkvkv(kv,kv), tmpkvkv2(kv,kv), tmpkr(kr))
    dfqk1%dfdv = 0.0_c_double;  dfqk1%dfdphi = 0.0_c_double;
    dfqk1%dqdv = 0.0_c_double;  dfqk1%dqdphi = 0.0_c_double;    dfqk1%dqdw = 0.0_c_double;
    dfqk1%dkdv = 0.0_c_double;  dfqk1%dkdphi = 0.0_c_double;

    i = 1_c_int
    i_c=fmlfm_c;  j_c=qm_c;  k_c=a_c;  r_c=fm_c;
    call c_f_pointer(r_c, fm_p);       r_c=fm_p%nxt;    ! +1 step before the loop as only F_{i+1}^m a_i is needed.
    !------ START LOOP 100
    ! This loop computes the "positive" part of dqd* and push stuff onto the stack
100 call c_f_pointer(k_c, a_p);       call c_f_pointer(a_p%dat, a, [a_p%siz]) 
    call c_f_pointer(i_c, fmlfm_p);   fmlfm(1:kv,1:kv)  => fmlfm_p%dat(1:)
    call c_f_pointer(j_c, qm_p);      qm(1:kv)          => qm_p%dat(1:)
    do n=1,ku
       do m=1,ku
          tmpkvkv(:,1) = dgamdvev(:,m,n)
          call dsymv('L',kv,-1.0_c_double,dodvev(:,:,m,n),kv,qm,1_c_int,1.0_c_double,tmpkvkv(:,1),1_c_int)
          call dsymv('L',kv,1.0_c_double,fmlfm,kv,tmpkvkv(:,1),1_c_int,1.0_c_double,dfqk1%dqdv(:,m,n),1_c_int)
          !dfqk1%dqdv(:,m,n)   =dfqk1%dqdv(:,m,n) + matmul(fmlfm, dgamdvev(:,m,n) - matmul(dodvev(:,:,m,n), qm))
       enddo
    enddo
    do n=1,kv
       do m=1,ku
          tmpkvkv(:,1) = dgamdphiev(:,m,n)
          call dsymv('L',kv,-1.0_c_double,dodphiev(:,:,m,n),kv,qm,1_c_int,1.0_c_double,tmpkvkv(:,1),1_c_int)
          call dsymv('L',kv,1.0_c_double,fmlfm,kv,tmpkvkv(:,1),1_c_int,1.0_c_double,dfqk1%dqdphi(:,m,n),1_c_int)
          !dfqk1%dqdphi(:,m,n) =dfqk1%dqdphi(:,m,n) + matmul(fmlfm, dgamdphiev(:,m,n) - matmul(dodphiev(:,:,m,n), qm))
       enddo
    enddo
    call dsymm('L','L',kv,ku,1.0_c_double,fmlfm,kv,dgamdwev,kv,1.0_c_double,dfqk1%dqdw,kv)
    !dfqk1%dqdw = dfqk1%dqdw + matmul(fmlfm, dgamdwev)
    stfm(i)        = r_c            ! May be NULL on top of the stack
    sta(i)         = k_c            ! Never NULL
    stfmlfm(i)     = i_c            ! Never NULL
    i = i + 1
    if (c_associated(fmlfm_p%nxt)) then
       call c_f_pointer(r_c, fm_p); !!  ASSERT: r_c isn't NULL
       i_c=fmlfm_p%nxt;  j_c=qm_p%nxt;  k_c=a_p%nxt;  r_c = fm_p%nxt
       goto 100
    endif
    !------ END LOOP 100
    tailsum_fmlfm => k
    !tailsum_fmlfm(1:kv,1:kv) => k
    tailsum_fmlfm(:,:) = 0.0_c_double
    
    i = i - 1
    call c_f_pointer(stfmlfm(i), fmlfm_p);           fmlfm(1:kv,1:kv)   => fmlfm_p%dat(1:)
    do n=1,ku
       do m=1,ku
          call dsymm('L','L',kv,kv,1.0_c_double,dodvev(:,:,m,n),kv,fmlfm,kv,0.0_c_double,tmpkvkv,kv)
          call dsymm('L','L',kv,kv,-1.0_c_double,fmlfm,kv,tmpkvkv,kv,0.0_c_double,dfqk1%dkdv(:,:,m,n),kv)
          !dfqk1%dkdv(:,:,m,n) = - matmul(fmlfm, matmul(dodvev(:,:,m,n), fmlfm))
       enddo
    enddo
    do n=1,kv
       do m=1,ku
          call dsymm('L','L',kv,kv,1.0_c_double,dodphiev(:,:,m,n),kv,fmlfm,kv,0.0_c_double,tmpkvkv,kv)
          call dsymm('L','L',kv,kv,-1.0_c_double,fmlfm,kv,tmpkvkv,kv,0.0_c_double,dfqk1%dkdphi(:,:,m,n),kv)
          !dfqk1%dkdphi(:,:,m,n) = - matmul(fmlfm, matmul(dodphiev(:,:,m,n), fmlfm))
       enddo
    enddo
    tailsum_fmlfm = tailsum_fmlfm + fmlfm
    !!------ START LOOP 200
    !! This loop pops stuff from stack, reversely compute sum_{l=i+1}^{m-1} F_{l+1}^{m} \Lambda_l F_{l+1}^{m}^T
    !! for all "reasonable" l, complete the "negative" part of dqd* and dkd*.
200 i = i - 1                 
    if (i > 0) then
       call c_f_pointer(stfmlfm(i), fmlfm_p);           fmlfm(1:kv,1:kv)   => fmlfm_p%dat(1:)
       call c_f_pointer(sta(i), a_p);                   call c_f_pointer(a_p%dat, a, [a_p%siz])
       call c_f_pointer(stfm(i), fm_p);                 fm(1:kv,1:a_p%siz) => fm_p%dat(1:)
       tmpkts(1:a_p%siz) => tmpkr(1:a_p%siz)
       do n=1,ku
          do m=1,ku
             call dsymm('R','L',kv,kv,1.0_c_double,dodvev(:,:,m,n),kv,tailsum_fmlfm,kv,0.0_c_double,tmpkvkv,kv)
             call dgemv('N',kv,a_p%siz,1.0_c_double,fm,kv,a,1_c_int,0.0_c_double,tmpkts,1_c_int)
             call dgemv('N',kv,kv,-1.0_c_double,tmpkvkv,kv,tmpkts,1_c_int,1.0_c_double,dfqk1%dqdv(:,m,n),1_c_int)
             call dsyr2k('L','N',kv,kv,-1.0_c_double,tmpkvkv,kv,fmlfm,kv,0.0_c_double,tmpkvkv2,kv)
             call syflgefill(tmpkvkv2, kv)
             dfqk1%dkdv(:,:,m,n) = dfqk1%dkdv(:,:,m,n) +tmpkvkv2
             call dsymm('L','L',kv,kv,1.0_c_double,dodvev(:,:,m,n),kv,fmlfm,kv,0.0_c_double,tmpkvkv,kv)
             call dsymm('L','L',kv,kv,-1.0_c_double,fmlfm,kv,tmpkvkv,kv,1.0_c_double,dfqk1%dkdv(:,:,m,n),kv)
             !tmpkvkv = matmul(tailsum_fmlfm, dodvev(:,:,m,n))
             !dfqk1%dqdv(:,m,n)   = dfqk1%dqdv(:,m,n) - matmul(tmpkvkv, matmul(fm, a))
             !tmpkvkv2 = matmul(tmpkvkv, fmlfm)
             !dfqk1%dkdv(:,:,m,n) = dfqk1%dkdv(:,:,m,n) - tmpkvkv2 - transpose(tmpkvkv2) &
             !                    & -matmul(fmlfm, matmul(dodvev(:,:,m,n), fmlfm))
          enddo
       enddo
       do n=1,kv
          do m=1,ku
             call dsymm('R','L',kv,kv,1.0_c_double,dodphiev(:,:,m,n),kv,tailsum_fmlfm,kv,0.0_c_double,tmpkvkv,kv)
             call dgemv('N',kv,a_p%siz,1.0_c_double,fm,kv,a,1_c_int,0.0_c_double,tmpkts,1_c_int)
             call dgemv('N',kv,kv,-1.0_c_double,tmpkvkv,kv,tmpkts,1_c_int,1.0_c_double,dfqk1%dqdphi(:,m,n),1_c_int)
             call dsyr2k('L','N',kv,kv,-1.0_c_double,tmpkvkv,kv,fmlfm,kv,0.0_c_double,tmpkvkv2,kv)
             call syflgefill(tmpkvkv2, kv)
             dfqk1%dkdphi(:,:,m,n) = dfqk1%dkdphi(:,:,m,n) +tmpkvkv2
             call dsymm('L','L',kv,kv,1.0_c_double,dodphiev(:,:,m,n),kv,fmlfm,kv,0.0_c_double,tmpkvkv,kv)
             call dsymm('L','L',kv,kv,-1.0_c_double,fmlfm,kv,tmpkvkv,kv,1.0_c_double,dfqk1%dkdphi(:,:,m,n),kv)
             !tmpkvkv = matmul(tailsum_fmlfm, dodphiev(:,:,m,n))
             !dfqk1%dqdphi(:,m,n)   = dfqk1%dqdphi(:,m,n) - matmul(tmpkvkv, matmul(fm, a))
             !tmpkvkv2 = matmul(tmpkvkv, fmlfm)
             !dfqk1%dkdphi(:,:,m,n) = dfqk1%dkdphi(:,:,m,n) - tmpkvkv2 - transpose(tmpkvkv2) &
             !                      & -matmul(fmlfm, matmul(dodphiev(:,:,m,n), fmlfm))
          enddo
       enddo
       tailsum_fmlfm = tailsum_fmlfm + fmlfm
       !! For w, tmp1 and dodwev are always zero so no update on q is needed. dk is of course zero.
       goto 200
    endif
    !------ END LOOP 200
    ! Finally, compute dFd*.
    call c_f_pointer(fm_c, fm_p);  fm(1:kv,1:kr)=>fm_p%dat(1:)
    do n=1,ku
       do m=1,ku
          call dsymm('R','L',kv,kv,1.0_c_double,dodvev(:,:,m,n),kv,tailsum_fmlfm,kv,0.0_c_double,tmpkvkv,kv)
          call dgemm('N','N',kv,kr,kv,-1.0_c_double,tmpkvkv,kv,fm,kv,0.0_c_double,dfqk1%dfdv(:,:,m,n),kv)
          !dfqk1%dfdv(:,:,m,n) = - matmul(matmul(tailsum_fmlfm, dodvev(:,:,m,n)), fm)
       enddo
    enddo
    do n=1,kv
       do m=1,ku
          call dsymm('R','L',kv,kv,1.0_c_double,dodphiev(:,:,m,n),kv,tailsum_fmlfm,kv,0.0_c_double,tmpkvkv,kv)
          call dgemm('N','N',kv,kr,kv,-1.0_c_double,tmpkvkv,kv,fm,kv,0.0_c_double,dfqk1%dfdphi(:,:,m,n),kv)
          !dfqk1%dfdphi(:,:,m,n) = - matmul(matmul(tailsum_fmlfm, dodphiev(:,:,m,n)), fm)
       enddo
    enddo
    deallocate(stfm, sta, stfmlfm, tmpkvkv, tmpkvkv2, tmpkr)
  end subroutine


  ! NOTICE This doesn't even use any of fmlfm_c, fm_c and q_c: Those are used largely to move m forward, not n.
  ! BOTTLENECK FUNCTION!
  recursive subroutine tndown (dfqk1_ch, HPhi, a, knv, knu, kmv, kmu, dfqk1new_ch) bind(C, name="tndown_")
    type(dfdqdkCMEM)  :: dfqk1_ch,       dfqk1new_ch
    type(dfdqdk)      :: dfqk1,          dfqk1new
    real(c_double) HPhi, a
    integer(c_int) knv, knu, kmv, kmu
    dimension HPhi(knu,knv), a(knu)
    real(c_double), allocatable :: tmpknuknv(:,:)
    call read_dfqk(dfqk1_ch, dfqk1)
    call read_dfqk(dfqk1new_ch, dfqk1new)
    allocate(tmpknuknv(knu,knv))
    do n=1,kmu
       do m=1,kmu
          !! dfqk1%dfdv has knv by dfqk1_ch%kr dimension
          call dgemm('N','N',knu,dfqk1_ch%kr,knv,1.0_c_double,HPhi,knu,dfqk1%dfdv(:,:,m,n),&
                   & knv,0.0_c_double,dfqk1new%dfdv(:,:,m,n),knu)
          call dsymm('R','L',knu,knv,1.0_c_double,dfqk1%dkdv(:,:,m,n),knv,HPhi,knu,0.0_c_double,tmpknuknv,knu)
          !call dgemm('N','N',knu,knv,knv,1.0_c_double,HPhi,knu,dfqk1%dkdv(:,:,m,n),knv,0.0_c_double,tmpknuknv,knu)
          call dgemm('N','T',knu,knu,knv,1.0_c_double,tmpknuknv,knu,HPhi,knu,0.0_c_double,dfqk1new%dkdv(:,:,m,n),knu)
          !dfqk1new%dfdv(:,:,m,n) = matmul(HPhi, dfqk1%dfdv(:,:,m,n))
          !dfqk1new%dqdv(:,m,n)   = matmul(HPhi, dfqk1%dqdv(:,m,n))
          !dfqk1new%dkdv(:,:,m,n) = matmul(matmul(HPhi, dfqk1%dkdv(:,:,m,n)), transpose(HPhi))
       enddo
       call dgemm('N','N',knu,kmu,knv,1.0_c_double,HPhi,knu,dfqk1%dqdv(:,:,n),knv,0.0_c_double,&
                & dfqk1new%dqdv(:,:,n),knu)       ! Batched from dqdphi vector mult.
    enddo
    do n=1,kmv
       do m=1,kmu
          call dgemm('N','N',knu,dfqk1_ch%kr,knv,1.0_c_double,HPhi,knu,dfqk1%dfdphi(:,:,m,n),&
                   & knv,0.0_c_double,dfqk1new%dfdphi(:,:,m,n),knu)
          call dsymm('R','L',knu,knv,1.0_c_double,dfqk1%dkdphi(:,:,m,n),knv,HPhi,knu,0.0_c_double,tmpknuknv,knu)
          !call dgemm('N','N',knu,knv,knv,1.0_c_double,HPhi,knu,dfqk1%dkdphi(:,:,m,n),knv,0.0_c_double,tmpknuknv,knu)
          call dgemm('N','T',knu,knu,knv,1.0_c_double,tmpknuknv,knu,HPhi,knu,0.0_c_double,dfqk1new%dkdphi(:,:,m,n),knu)
          !dfqk1new%dfdphi(:,:,m,n) = matmul(HPhi, dfqk1%dfdphi(:,:,m,n))
          !dfqk1new%dqdphi(:,m,n)   = matmul(HPhi, dfqk1%dqdphi(:,m,n))
          !dfqk1new%dkdphi(:,:,m,n) = matmul(matmul(HPhi, dfqk1%dkdphi(:,:,m,n)), transpose(HPhi))
       enddo
       call dgemm('N','N',knu,kmu,knv,1.0_c_double,HPhi,knu,dfqk1%dqdphi(:,:,n),knv,0.0_c_double,&
                & dfqk1new%dqdphi(:,:,n),knu)     ! Batched from dqdphi vector mult.
    enddo
    call dgemm('N','N',knu,kmu,knv,1.0_c_double,HPhi,knu,dfqk1%dqdw,knv,0.0_c_double,dfqk1new%dqdw,knu)
    call dgemm('N','N',knu,dfqk1_ch%kr,knv,1.0_c_double,HPhi,knu,dfqk1%f1n,knv,&
             & 0.0_c_double,dfqk1new%f1n,knu)
    dfqk1new%q1n(:) = a(:)
    call dgemv('N',knu,knv,1.0_c_double,HPhi,knu,dfqk1%q1n,1_c_int,1.0_c_double,dfqk1new%q1n,1_c_int)
    !call dgemm('N','N',knu,knu,knv,1.0_c_double,HPhi,knu,dfqk1%q1n,knv,1.0_c_double,dfqk1new%q1n,knu)
    !dfqk1new%dqdw = matmul(HPhi, dfqk1%dqdw)
    !dfqk1new%f1n  = matmul(HPhi, dfqk1%f1n)
    !dfqk1new%q1n  = matmul(HPhi, dfqk1%q1n) + a
    deallocate(tmpknuknv)
  end subroutine

  recursive subroutine dfqk_mmp1 (dfqk1_ch,H,HPhi,w,a,Lamb,solV,solVLsOPhi,ku,kr) bind(C, name="dfqk_mmp1_")
    implicit none
    integer(c_int)    :: ku, kr
    real(c_double)    :: HPhi(ku,kr), H(ku,ku), w(ku), a(ku), Lamb(ku,ku), solV(ku,ku), solVLsOPhi(ku,kr)
    type(dfdqdkCMEM) :: dfqk1_ch
    type(dfdqdk)      :: dfqk1
    integer(c_int)    :: m, n, i
    real(c_double), allocatable :: tmpku1(:), LsolV(:,:), solVaw(:)
    allocate(tmpku1(ku), LsolV(ku,ku), solVaw(ku))
    call read_dfqk(dfqk1_ch, dfqk1)
    dfqk1%dfdv   = 0.0_c_double
    dfqk1%dkdv   = 0.0_c_double
    dfqk1%dfdphi = 0.0_c_double
    dfqk1%dqdphi = 0.0_c_double
    dfqk1%dkdphi = 0.0_c_double
    dfqk1%f1n    = 0.0_c_double
    dfqk1%q1n    = 0.0_c_double

    call dgemm('N','N',ku,ku,ku,1.0_c_double,Lamb,ku,solV,ku,0.0_c_double,LsolV,ku)
    tmpku1 = a - w
    call dgemv('N',ku,ku,1.0_c_double,solV,ku,tmpku1,1_c_int,0.0_c_double,solVaw,1_c_int)
    !LsolV = matmul(Lamb, solV)
    !solVaw = matmul(solV, a - w)
    !! dv
    do n=1,ku
       do m=1,ku
          call dger(ku,kr,-1.0_c_double,LsolV(1,m),1_c_int,solVLsOPhi(n,1),ku,dfqk1%dfdv(1,1,m,n),ku)
          !dfqk1%dfdv(:,:,m,n) = - matmul(LsolV(:,m:m), solVLsOPhi(n:n,:))
       enddo
    enddo
    do n=1,ku
       do m=1,ku
          dfqk1%dqdv(:,m,n) = solVaw(n)*LsolV(:,m)
       enddo
    enddo
    do n=1,ku
       do m=1,ku
          call dger(ku,ku,1.0_c_double,LsolV(1,m),1_c_int,LsolV(1,n),1_c_int,dfqk1%dkdv(1,1,m,n),ku)
          !dfqk1%dkdv(:,:,m,n) = matmul(LsolV(:,m:m), transpose(LsolV(:,n:n)))
       enddo
    enddo
    !! dphi
    do n=1,kr
       do m=1,ku
          do i=1,ku
             dfqk1%dfdphi(i,n,m,n) = H(i,m)
          end do
       enddo
    enddo
    !! dkdw
    dfqk1%dqdw = H
    dfqk1%f1n  = HPhi
    dfqk1%q1n  = a
    deallocate(tmpku1, LsolV, solVaw)
  end subroutine

  recursive subroutine tndown1st (dfqk1_ch,K,H,HPhi,w,a,f1m,q1m,Lamb,solV,solVLsOPhi,kr,kv,ku,dfqk1new_ch) &
       & bind(C, name = "tndown1st_")
    type(dfdqdkCMEM)  :: dfqk1_ch,       dfqk1new_ch
    type(dfdqdk)      :: dfqk1,          dfqk1new
    real(c_double) K,H,HPhi,w,a,f1m,q1m,Lamb,solV,solVLsOPhi
    integer(c_int) kr,kv,ku
    dimension H(ku,ku), K(kv,kv), HPhi(ku,kv), f1m(kv,kr), Lamb(ku,ku), solV(ku,ku), solVLsOPhi(ku,kv), &
         & q1m(kv),w(ku),a(ku)
    real(c_double), allocatable :: dfmmp1(:,:), extrakterm(:,:), solVaw(:), LsolV(:,:), tmpkvku(:,:)
    external dgemm, dger, dgemv
    allocate(dfmmp1(ku,kv), extrakterm(ku,ku), solVaw(ku), LsolV(ku,ku), tmpkvku(kv,ku))
    ! In dfqk1, f1n and q1n is intentionally undefined.
    call read_dfqk(dfqk1new_ch, dfqk1new)
    call read_dfqk(dfqk1_ch, dfqk1)

    call dgemm('N','N',ku,ku,ku,1.0_c_double,Lamb,ku,solV,ku,0.0_c_double,LsolV,ku)
    dfmmp1(:,1) = a-w
    call dgemv('N',ku,ku,1.0_c_double,solV,ku,dfmmp1(:,1),1_c_int,0.0_c_double,solVaw,1_c_int)
!   LsolV = matmul(Lamb, solV)
!   solVaw = matmul(solV, a - w)
    
    ! Optimisation oppotunity: + matmul(LsolV(:,m), transpose(LsolV(:,n))) have symmetry.
    do n=1,ku
       do m=1,ku
          dfmmp1 = 0.0_c_double
          call dger(ku, kv, -1.0_c_double, LsolV(1,m), 1_c_int, solVLsOPhi(n,1), ku, dfmmp1(1,1), ku)
          
          !! K1m = sum_1^{m-1} {F{i+1}m Li F{i+1}m}
          !! K1n = sum_1^m {F{i+1}{m+1} Li F{i+1}{m+1}}
          !! So  K1n = sum_1^{m-1} {F{i+1}{m+1} Li F{i+1}{m+1}} + Lm
          !! So  K1n = HPhi_m K1m HPhi_m + Lm
          !! So  d{K1n} = d{HPhi_m} K1m (HPhi_m)^T + HPhi_m d{K1m} (HPhi_m)^T + HPhi_m K1m d{HPhi_m} + d{Lm}

          call dgemm('N','T',kv,ku,kv,1.0_c_double,dfqk1%dkdv(:,:,m,n),kv,HPhi,ku,0.0_c_double,tmpkvku,kv)
          call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,tmpkvku,kv,0.0_c_double,dfqk1new%dkdv(:,:,m,n),ku)
          call dgemm('N','T',kv,ku,kv,1.0_c_double,K,kv,dfmmp1,ku,0.0_c_double,tmpkvku,kv)
          call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,tmpkvku,kv,0.0_c_double,extrakterm,ku)
          do jj = 1,ku
             do ii = 1,ku
                dfqk1new%dkdv(ii,jj,m,n) = dfqk1new%dkdv(ii,jj,m,n) + extrakterm(ii,jj) + extrakterm(jj,ii)
             end do
          end do
          call dger(ku,ku,1.0_c_double, LsolV(1,m),1_c_int,LsolV(1,n),1_c_int,dfqk1new%dkdv(1,1,m,n),ku)

          call dgemm('N','N',ku,kr,kv,1.0_c_double,HPhi,ku,dfqk1%dfdv(:,:,m,n),kv,0.0_c_double,dfqk1new%dfdv(:,:,m,n),ku)
          call dgemm('N','N',ku,kr,kv,1.0_c_double,dfmmp1,ku,f1m,kv,1.0_c_double,dfqk1new%dfdv(:,:,m,n),ku)

          do jj = 1,ku
            dfqk1new%dqdv(jj,m,n) = solVaw(n)*LsolV(jj,m)
          end do
          call dgemv('N',ku,kv,1.0_c_double,HPhi,ku,dfqk1%dqdv(:,m,n),1_c_int,1.0_c_double,dfqk1new%dqdv(:,m,n),1_c_int)
          call dgemv('N',ku,kv,1.0_c_double,dfmmp1,ku,q1m,1_c_int,1.0_c_double,dfqk1new%dqdv(:,m,n),1_c_int)

!           dfmmp1 = - matmul(LsolV(:,m:m), solVLsOPhi(n:n,:))
!          extrakterm = matmul(HPhi, matmul(K, transpose(dfmmp1)))
!          dfqk1new%dkdv(:,:,m,n) = matmul(matmul(HPhi, dfqk1%dkdv(:,:,m,n)), transpose(HPhi)) &
!               & + extrakterm + transpose(extrakterm) + matmul(LsolV(:,m:m), transpose(LsolV(:,n:n)))
!          dfqk1new%dfdv(:,:,m,n) = matmul(HPhi, dfqk1%dfdv(:,:,m,n)) + matmul(dfmmp1,f1m)
!          dfqk1new%dqdv(:,m,n)   = matmul(HPhi, dfqk1%dqdv(:,m,n))   + matmul(dfmmp1,q1m) + solVaw(n)*LsolV(:,m)
       enddo
    enddo

    do n=1,kv
       do m=1,ku
          call dgemm('N','T',kv,ku,kv,1.0_c_double,dfqk1%dkdphi(:,:,m,n),kv,HPhi,ku,0.0_c_double,tmpkvku,kv)
          call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,tmpkvku,kv,0.0_c_double,dfqk1new%dkdphi(:,:,m,n),ku)
          tmpkvku = 0.0_c_double
          call dger(kv, ku, 1.0_c_double, K(1,n), 1_c_int, H(1,m), 1_c_int, tmpkvku(1,1), kv)
          call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,tmpkvku,kv,0.0_c_double,extrakterm,ku)
          do jj = 1,ku
             do ii = 1,ku
                dfqk1new%dkdphi(ii,jj,m,n) = dfqk1new%dkdphi(ii,jj,m,n) + extrakterm(ii,jj) + extrakterm(jj,ii)
             end do
          end do
          call dgemm('N','N',ku,kr,kv,1.0_c_double,HPhi,ku,dfqk1%dfdphi(:,:,m,n),kv,0.0_c_double,dfqk1new%dfdphi(:,:,m,n),ku)
          call dger(ku,kr,1.0_c_double,H(1,m),1_c_int,f1m(n,1),kv,dfqk1new%dfdphi(1,1,m,n),ku)
          do jj=1,ku
            dfqk1new%dqdphi(jj,m,n) = q1m(n) * H(jj,m)
          end do

          call dgemv('N',ku,kv,1.0_c_double,HPhi,ku,dfqk1%dqdphi(:,m,n),1_c_int,1.0_c_double,dfqk1new%dqdphi(:,m,n),1_c_int)
!         extrakterm = matmul(HPhi, matmul(K(:,n:n), transpose(H(:,m:m))))
!         dfqk1new%dkdphi(:,:,m,n) = matmul(matmul(HPhi, dfqk1%dkdphi(:,:,m,n)), transpose(HPhi)) &
!              & + extrakterm + transpose(extrakterm)
!         dfqk1new%dfdphi(:,:,m,n) = matmul(HPhi, dfqk1%dfdphi(:,:,m,n)) + matmul(H(:,m:m), f1m(n:n,:))
!        dfqk1new%dqdphi(:,m,n)   = matmul(HPhi, dfqk1%dqdphi(:,m,n))   + q1m(n) * H(:,m)
       enddo
    enddo
    dfqk1new%dqdw = H
    call dgemm('N','N',ku,ku,kv,1.0_c_double,HPhi,ku,dfqk1%dqdw,kv,1.0_c_double,dfqk1new%dqdw,ku)
    call dgemm('N','N',ku,kr,kv,1.0_c_double,HPhi,ku,f1m,kv,0.0_c_double,dfqk1new%f1n,ku)
    dfqk1new%q1n = a
    call dgemv('N',ku,kv,1.0_c_double,HPhi,ku,q1m,1_c_int,1.0_c_double,dfqk1new%q1n,1_c_int)
!    dfqk1new%dqdw = matmul(HPhi, dfqk1%dqdw) + H
!    dfqk1new%f1n = matmul(HPhi, f1m)
!    dfqk1new%q1n = matmul(HPhi, q1m) + a
    deallocate(dfmmp1, extrakterm, solVaw, LsolV, tmpkvku)
  end subroutine

  recursive subroutine initfqk4b(dfqk1_ch, fmg_c, qmg_c, falfm_c, f1a, q1a, a_c, nk, kr, kav, kmv, kmu, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev) bind(C, name="initfqk4b_")
    implicit none
    type(c_ptr) :: falfm_c, qmg_c, fmg_c, a_c
    integer(c_int) :: nk, kr, kav, kmv, kmu
    real(c_double) :: dodvev(kmv,kmv,kmu,kmu), dodphiev(kmv,kmv,kmu,kmv), dgamdvev(kmv,kmu,kmu), &
         & dgamdwev(kmv,kmu), dgamdphiev(kmv,kmu,kmv), f1a(kav,kr), q1a(kav)
    type(c_ptr) ::  i_c,j_c,r_c,k_c
    type(dfdqdkCMEM), intent(inout) :: dfqk1_ch
    integer(c_int)    :: i,m,n, nn,mm
    type(dfdqdk)      :: dfqk1
    type(c_ptr), allocatable :: stfmg(:), sta(:), stfalfm(:), stfalfm_nxt(:)
    real(c_double), allocatable :: tmpkmv1(:), tmpkmvkav(:,:), tmpkavkmv(:,:), tmpkavkav(:,:), &
        & tailsum_falfm(:,:)
    type(llst), pointer :: fmg_p, qmg_p, falfm_p
    type(llstptr), pointer :: a_p
    real(c_double), pointer :: a(:), fmg(:,:), qmg(:), falfm(:,:)
    external dgemv, dgemm
    allocate(stfmg(1660000), sta(1660000), stfalfm(1660000), stfalfm_nxt(1660000))     ! 50 MBs. 
    call read_dfqk(dfqk1_ch, dfqk1)
    dfqk1%kr = kr
    dfqk1%knu = kav
    dfqk1%knv = kmu          !NOT USED
    dfqk1%kmu = kmu
    dfqk1%kmv = kmv
    allocate(tmpkmv1(kmv), tmpkmvkav(kmv,kav), tailsum_falfm(kav,kmv), tmpkavkmv(kav,kmv), tmpkavkav(kav,kav))

    dfqk1%dfdv = 0.0_c_double;  dfqk1%dfdphi = 0.0_c_double;
    dfqk1%dqdv = 0.0_c_double;  dfqk1%dqdphi = 0.0_c_double;    dfqk1%dqdw = 0.0_c_double;
    dfqk1%dkdv = 0.0_c_double;  dfqk1%dkdphi = 0.0_c_double;
    
    i = 1
    i_c = falfm_c
    j_c = qmg_c
    k_c = a_c
    r_c = fmg_c

    call c_f_pointer(r_c, fmg_p)
    r_c=fmg_p%nxt;    ! +1 step before the loop as only F_{i+1}^m a_i is needed.
    !------ START LOOP 100
    ! This loop computes the "positive" part of dqd* and push stuff onto the stack
100 call c_f_pointer(k_c, a_p);       call c_f_pointer(a_p%dat, a, [a_p%siz]) 
    call c_f_pointer(i_c, falfm_p);   falfm(1:kav,1:kmv) => falfm_p%dat(1:)
    call c_f_pointer(j_c, qmg_p);     qmg(1:kmv)         => qmg_p%dat(1:)

    do n=1,kmu
       do m=1,kmu
          ! Here you need a kmv x 1 to store the vector. Then a dgemm call.
          tmpkmv1 = dgamdvev(:,m,n)
          call dgemv('N',kmv,kmv,-1.0_c_double,dodvev(:,:,m,n),kmv,qmg,1_c_int,1.0_c_double,tmpkmv1,1_c_int)
          call dgemv('N',kav,kmv,1.0_c_double,falfm,kav,tmpkmv1,1_c_int,1.0_c_double,dfqk1%dqdv(:,m,n),1_c_int)
          !dfqk1%dqdv(:,m,n)    = dfqk1%dqdv(:,m,n) + matmul(falfm, dgamdvev(:,m,n) - matmul(dodvev(:,:,m,n), qmg))
       enddo
    enddo
    do n=1,kmv
       do m=1,kmu
          tmpkmv1 = dgamdphiev(:,m,n)
          call dgemv('N',kmv,kmv,-1.0_c_double,dodphiev(:,:,m,n),kmv,qmg,1_c_int,1.0_c_double,tmpkmv1,1_c_int)
          call dgemv('N',kav,kmv,1.0_c_double,falfm,kav,tmpkmv1,1_c_int,1.0_c_double,dfqk1%dqdphi(:,m,n),1_c_int)
          !dfqk1%dqdphi(:,m,n)  = dfqk1%dqdphi(:,m,n) + matmul(falfm, dgamdphiev(:,m,n) - matmul(dodphiev(:,:,m,n), qmg))
       enddo
    enddo
    stfalfm(i)  = i_c
    sta(i) = k_c
    stfmg(i)  = r_c    ! Can be NULL on the top of the stack
    if (i < nk+1) then ! Till beta. The last i is nk+1
       i = i + 1
       call c_f_pointer(r_c, fmg_p)
       i_c=falfm_p%nxt;   j_c=qmg_p%nxt;  k_c=a_p%nxt;  r_c = fmg_p%nxt
       goto 100
    endif
    !------- END LOOP 100
    
    !! Invariant: Now falfm_p, i, i_c and stfalfm(i) all points to beta's place.
    !!
    !! 0. falfm_p === i_c === stfalfm(i), therefore proving the top of the stack is pointed to beta's position
    !! 1. If beta is one then nk is 0, so the stack has one element and the loop executes only once. In the loop there
    !!    is the first element in the chain, which is beta's parameter.
    !! 2. If beta >= 2, meaning nk>=1 then, for nk==1, the loop was executed twice and hence i ends with i == nk+1 == 2,
    !!    therefore in the top of the stack there is the second element of the chain, which satisfies the invariant.
    !! 
    !! Now i points to beta's location... Note that the top-of-the-stack r_c is never used
    call c_f_pointer(stfalfm(i), falfm_p);      falfm(1:kav,1:kmv) => falfm_p%dat(1:)
    tailsum_falfm = falfm
    do n=1,kmu
       do m=1,kmu
          call dgemm('N','T',kmv,kav,kmv,1.0_c_double,dodvev(:,:,m,n),kmv,falfm,kav,0.0_c_double,tmpkmvkav,kmv)
          call dgemm('N','N',kav,kav,kmv,-1.0_c_double,falfm,kav,tmpkmvkav,kmv,0.0_c_double,dfqk1%dkdv(:,:,m,n),kav)
          !dfqk1%dkdv(:,:,m,n) = - matmul(falfm, matmul(dodvev(:,:,m,n), transpose(falfm)))
       enddo
    enddo
    do n=1,kmv
       do m=1,kmu
          call dgemm('N','T',kmv,kav,kmv,1.0_c_double,dodphiev(:,:,m,n),kmv,falfm,kav,0.0_c_double,tmpkmvkav,kmv)
          call dgemm('N','N',kav,kav,kmv,-1.0_c_double,falfm,kav,tmpkmvkav,kmv,0.0_c_double,dfqk1%dkdphi(:,:,m,n),kav)
          !dfqk1%dkdphi(:,:,m,n) = - matmul(falfm, matmul(dodphiev(:,:,m,n), transpose(falfm)))
       enddo
    enddo

    
    call dgemm('N','N',kav,kmu,kmv,1.0_c_double,falfm,kav,dgamdwev,kmv,1.0_c_double,dfqk1%dqdw,kav)
    !dfqk1%dqdw = dfqk1%dqdw + matmul(falfm, dgamdwev)
200 i = i - 1
    !! If beta - 1 is too small then nothing should be summed.
    if (i > 0) then
       call c_f_pointer(stfalfm(i), falfm_p);   falfm(1:kav,1:kmv) => falfm_p%dat(1:)
       call c_f_pointer(sta(i), a_p);           call c_f_pointer(a_p%dat, a, [a_p%siz])
       call c_f_pointer(stfmg(i), fmg_p);       fmg(1:kmv,1:a_p%siz) => fmg_p%dat(1:)
       call dgemv('N',kmv,a_p%siz,1.0_c_double,fmg,kmv,a,1_c_int,0.0_c_double,tmpkmv1,1_c_int)
       do n=1,kmu
          do m=1,kmu
             call dgemm('N','N',kav,kmv,kmv,1.0_c_double,tailsum_falfm,kav,dodvev(:,:,m,n),kmv,0.0_c_double,tmpkavkmv,kav)
             call dgemv('N',kav,kmv,-1.0_c_double,tmpkavkmv,kav,tmpkmv1,1_c_int,1.0_c_double,dfqk1%dqdv(:,m,n),1_c_int)
             call dgemm('N','T',kav,kav,kmv,1.0_c_double,tmpkavkmv,kav,falfm,kav,0.0_c_double,tmpkavkav,kav)
             do nn = 1,kav
                do mm = 1,kav
                   dfqk1%dkdv(nn,mm,m,n) = dfqk1%dkdv(nn,mm,m,n) - tmpkavkav(nn,mm) - tmpkavkav(mm,nn)
                end do
             end do
             call dgemm('N','T',kmv,kav,kmv,1.0_c_double,dodvev(:,:,m,n),kmv,falfm,kav,0.0_c_double,tmpkmvkav,kmv)
             call dgemm('N','N',kav,kav,kmv,-1.0_c_double,falfm,kav,tmpkmvkav,kmv,1.0_c_double,dfqk1%dkdv(:,:,m,n),kav)
             !tmp1 = matmul(tailsum_falfm, dodvev(:,:,m,n))
             !dfqk1%dqdv(:,m,n)   = dfqk1%dqdv(:,m,n) - matmul(tmp1, matmul(fmg, a))
             !tmp2 = matmul(tmp1, transpose(falfm))
             !dfqk1%dkdv(:,:,m,n) = dfqk1%dkdv(:,:,m,n) - tmp2 &
             !     & - transpose(tmp2) &
             !     & - matmul(falfm, matmul(dodvev(:,:,m,n), transpose(falfm)))
          enddo
       enddo
       do n=1,kmv
          do m=1,kmu
             call dgemm('N','N',kav,kmv,kmv,1.0_c_double,tailsum_falfm,kav,dodphiev(:,:,m,n),kmv,0.0_c_double,tmpkavkmv,kav)
             call dgemv('N',kav,kmv,-1.0_c_double,tmpkavkmv,kav,tmpkmv1,1_c_int,1.0_c_double,dfqk1%dqdphi(:,m,n),1_c_int)
             call dgemm('N','T',kav,kav,kmv,1.0_c_double,tmpkavkmv,kav,falfm,kav,0.0_c_double,tmpkavkav,kav)
             do nn = 1,kav
                do mm = 1,kav
                   dfqk1%dkdphi(nn,mm,m,n) = dfqk1%dkdphi(nn,mm,m,n) - tmpkavkav(mm,nn) - tmpkavkav(nn,mm)
                end do
             end do
             call dgemm('N','T',kmv,kav,kmv,1.0_c_double,dodphiev(:,:,m,n),kmv,falfm,kav,0.0_c_double,tmpkmvkav,kmv)
             call dgemm('N','N',kav,kav,kmv,-1.0_c_double,falfm,kav,tmpkmvkav,kmv,1.0_c_double,dfqk1%dkdphi(:,:,m,n),kav)
             !tmp1 = matmul(tailsum_falfm, dodphiev(:,:,m,n))
             !dfqk1%dqdphi(:,m,n)   = dfqk1%dqdphi(:,m,n) - matmul(tmp1, matmul(fmg, a))
             !tmp2 = matmul(tmp1, transpose(falfm))
             !dfqk1%dkdphi(:,:,m,n) = dfqk1%dkdphi(:,:,m,n) - tmp2 - transpose(tmp2) &
             !     & -matmul(falfm, matmul(dodphiev(:,:,m,n), transpose(falfm)))
          enddo
       enddo
       tailsum_falfm = tailsum_falfm + falfm
       call dgemm('N','N',kav,kmu,kmv,1.0_c_double,falfm,kav,dgamdwev,kmv,1.0_c_double,dfqk1%dqdw,kav)
       !dfqk1%dqdw = dfqk1%dqdw + matmul(falfm, dgamdwev)       
       goto 200
    endif
    !------- END LOOP 200

    ! Invariant: tailsum_falfm == sum from 1 to beta fmlfm
    call c_f_pointer(fmg_c, fmg_p);      fmg(1:kmv,1:kr) => fmg_p%dat(1:)
    do n=1,kmu
       do m=1,kmu
          call dgemm('N','N',kav,kmv,kmv,1.0_c_double,tailsum_falfm,kav,dodvev(:,:,m,n),kmv,0.0_c_double,tmpkavkmv,kav)
          call dgemm('N','N',kav,kr,kmv,-1.0_c_double,tmpkavkmv,kav,fmg,kmv,0.0_c_double,dfqk1%dfdv(:,:,m,n),kav)
          !dfqk1%dfdv(:,:,m,n) = - matmul(matmul(tailsum_falfm, dodvev(:,:,m,n)), fmg)
       enddo
    enddo
    do n=1,kmv
       do m=1,kmu
          call dgemm('N','N',kav,kmv,kmv,1.0_c_double,tailsum_falfm,kav,dodphiev(:,:,m,n),kmv,0.0_c_double,tmpkavkmv,kav)
          call dgemm('N','N',kav,kr,kmv,-1.0_c_double,tmpkavkmv,kav,fmg,kmv,0.0_c_double,dfqk1%dfdphi(:,:,m,n),kav)
          !dfqk1%dfdphi(:,:,m,n) = - matmul(matmul(tailsum_falfm, dodphiev(:,:,m,n)), fmg)
       enddo
    enddo
    dfqk1%f1n = f1a
    dfqk1%q1n = q1a
    deallocate(stfmg, sta, stfalfm, tmpkmv1, tmpkmvkav, tailsum_falfm, tmpkavkmv, tmpkavkav)
  end subroutine

  recursive subroutine betadown(falfm_c, falfm_new, fmg_c, f1a, q1a, HPhi, a, nk, mdim, kr, kbu, kbv, kmv) bind(C, name="betadown_")
    ! HPhi is the HPhi of beta so we can call it the falfm of the (simultaneously) all the alpha which
    ! are direct children of beta
    implicit none
    integer(c_int) :: nk, kr, kbu, kbv, kmv, mdim
    real(c_double) :: HPhi(kbu, kbv), a(kbu)
    real(c_double), target, intent(inout) :: f1a(mdim*mdim), q1a(mdim)
    type(c_ptr), intent(inout) :: falfm_c, falfm_new
    type(c_ptr)  :: fmg_c
    integer(c_int) :: j
    type(c_ptr) :: i_c
    type(llst), pointer :: tmp_p, fmg_p
    real(c_double), pointer :: dcur(:,:), dnew(:,:), fmg(:,:)
    real(c_double), allocatable :: tmpkbukr(:,:), tmpkbukmv(:,:)
    external dgemm, dgemv
    allocate(tmpkbukr(kbu,kr), tmpkbukmv(kbu,kmv))

    ! Everything in falfm needs to be multiplied by HPhi
    i_c = falfm_c
10  call c_f_pointer(i_c, tmp_p)
    dcur(1:kbv,1:kmv) => tmp_p%dat
    dnew(1:kbu,1:kmv) => tmp_p%dat
    call dgemm('N','N',kbu,kmv,kbv,1.0_c_double,HPhi,kbu,dcur,kbv,0.0_c_double,tmpkbukmv,kbu)
    dnew = tmpkbukmv
    !dnew = matmul(HPhi, dcur)
    tmp_p%siz = kbu
    if (c_associated(tmp_p%nxt)) then
       i_c = tmp_p%nxt
       goto 10
    endif

    ! At the end of the falfm chain, append beta's lambda.
    ! In falfm_new there should be beta's lambda in it. This
    ! Lambda needs to be multiplied by the F_1^m.
    tmp_p%nxt = falfm_new
    call c_f_pointer(falfm_new, tmp_p)
    dcur(1:kbu,1:kbu) => tmp_p%dat
    dnew(1:kbu,1:kmv) => tmp_p%dat
    
    i_c = fmg_c
    j = nk+1
20  if (j /= 0) then
       call c_f_pointer(i_c, fmg_p)
       i_c = fmg_p%nxt
       j = j - 1
       goto 20
    endif
    if (c_associated(i_c)) then
       call c_f_pointer(i_c, fmg_p)
       fmg(1:kmv,1:kbu) => fmg_p%dat
       call dgemm('N','T',kbu,kmv,kbu,1.0_c_double,dcur,kbu,fmg,kmv,0.0_c_double,tmpkbukmv,kbu)
       dnew = tmpkbukmv
       !dnew = matmul(dcur, transpose(fmg))
       tmp_p%siz = kbu
    else
       tmp_p%siz = kbu          ! Now kbu = kmv because beta = parent(m).
    endif
    
    dcur(1:kbv,1:kr) => f1a(1:);  dnew(1:kbu,1:kr) => f1a(1:)!;    dnew = matmul(HPhi, dcur)
    call dgemm('N','N',kbu,kr,kbv,1.0_c_double,HPhi,kbu,dcur,kbv,0.0_c_double,tmpkbukr,kbu)
    dnew = tmpkbukr
    dcur(1:kbv,1:1)  => q1a(1:);  dnew(1:kbu,1:1)  => q1a(1:)!;    dnew(:,1) = matmul(HPhi, dcur(:,1)) + a
    tmpkbukr(:,1) = a
    call dgemv('N',kbu,kbv,1.0_c_double,HPhi,kbu,dcur(:,1),1_c_int,1.0_c_double,tmpkbukr(:,1),1_c_int)
    dnew(:,1) = tmpkbukr(:,1)
    deallocate(tmpkbukr, tmpkbukmv)
  end subroutine


  recursive subroutine d2Lijmntmtn(dF1, dq1, dk1, f1n, q1n, dodtn, dgamdtn, kr, knv, x0, d2L)
    ! tip-ness is encoded in d*dtn so no dummy argument for istip here
    integer(c_int)    :: kr, knv
    real(c_double)    :: dF1(knv,kr), dq1(knv), dk1(knv,knv), f1n(knv,kr), q1n(knv), &
                       & dodtn(knv,knv), dgamdtn(knv), x0(kr), d2L, hc1, hd1
    real(c_double), allocatable    :: wsp1(:,:), ho1(:,:), hgam1(:)
    external dgemm, dgemv, ddot
    allocate(wsp1(knv,kr), ho1(kr,kr), hgam1(kr))
    call dgemm('N','N', knv,kr,knv,1.0_c_double,dodtn,knv,f1n,knv,0.0_c_double,wsp1,knv)
    call dgemm('T','N', kr,kr,knv,1.0_c_double,dF1,knv,wsp1,knv,0.0_c_double,ho1,kr)
    call dgemm('N','N', knv,kr,knv,1.0_c_double,dodtn,knv,dF1,knv,0.0_c_double,wsp1,knv)
    call dgemm('T','N', kr,kr,knv,1.0_c_double,f1n,knv,wsp1,knv,1.0_c_double,ho1,kr)
    wsp1(:,1) = dgamdtn
    call dgemv('N',knv,knv,-1.0_c_double,dodtn,knv,q1n,1,1.0_c_double,wsp1(:,1),1)
    hc1 = -2.0_c_double * ddot(knv, dq1,1_c_int, wsp1(:,1),1_c_int)
    call dgemv('T',knv,kr,1.0_c_double,dF1,knv,wsp1(:,1),1,0.0_c_double,hgam1,1)
    call dgemv('N',knv,knv,1.0_c_double,dodtn,knv,dq1,1,0.0_c_double,wsp1(:,1),1)
    call dgemv('T',knv,kr,-1.0_c_double,f1n,knv,wsp1(:,1),1,1.0_c_double,hgam1,1)
    ! ho1   = matmul(transpose(dF1), matmul(dodtn, f1n)) + matmul(transpose(f1n), matmul(dodtn, dF1))
    ! hgam1 = matmul(transpose(dF1), dgamdtn - matmul(dodtn, q1n)) - matmul(transpose(f1n), matmul(dodtn, dq1))
    ! hc1   = 2.0_c_double * dot_product(dq1, matmul(dodtn, q1n) - dgamdtn)
    hd1 = 0.0_c_double
    do jj = 1,knv
       do ii = 1,knv
          hd1 = hd1 + dodtn(jj,ii)*dk1(ii,jj)
       end do
    end do
    !hd1   = sum(transpose(dodtn)*dk1)
    call hlchainrule (x0, ho1, hgam1, hc1, hd1, kr, d2L)
    deallocate(wsp1, ho1, hgam1)
  end subroutine
  
  recursive subroutine tntmdir(kr,knv,knu,kmv,kmu,dfqk1_ch, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, x0, &
       & bilinmat, dir, ndir, derivlen, admphi, admV, admw, adnphi, adnV, adnw) bind(C, name="tntmdir_")
    implicit none
    integer(c_int)    :: kr, kmv, kmu, knv, knu, ndir
    integer(c_long)   :: derivlen, admphi, admV, admw, adnphi, adnV, adnw
    type(dfdqdkCMEM) :: dfqk1_ch
    real(c_double)    :: &
                       & dodvev(knv,knv,knu,knu), dodphiev(knv,knv,knu,knv), &
                       & dgamdvev(knv,knu,knu), dgamdwev(knv,knu), dgamdphiev(knv,knu,knv), &
                       & x0(kr), bilinmat(ndir,ndir), dir(derivlen,ndir), dlijmn,dljimn,dljinm,dlijnm, hessv
    real(c_double), allocatable   :: zeroknvkr(:,:),zeroknvknv(:,:)
    type(dfdqdk)      :: dfqk1
    integer(c_int)    :: i, j, m, n
    integer(c_long), target  :: idx_m, idx_n
    integer(c_long), pointer :: idx_big, idx_small
    allocate(zeroknvkr(knv,kr),zeroknvknv(knv,knv))
    
    call read_dfqk(dfqk1_ch, dfqk1)
    zeroknvkr = 0.0_c_double;  zeroknvknv = 0.0_c_double
    
    if (admV < adnV) then
       idx_big => idx_n;        idx_small => idx_m
    else
       idx_big => idx_m;        idx_small => idx_n
    endif
    
    ! v - v
    do n=1,knu
       do m=n,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnV+iijtouplolidx(knu,m,n)
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dljimn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dljinm )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm )
                call symhessvv(i,j,m,n,dlijmn,dljimn,dljinm,dlijnm, hessv)
                call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             enddo
          enddo
       enddo
    enddo
    ! v - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnPhi+ (n-1) * knu + m;
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dlijmn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dljimn )
                call symhessvany(i,j, dlijmn, dljimn, hessv)
                call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             enddo
          enddo
       enddo
    enddo
    ! v - w
    do m=1,knu
       do j=1,kmu
          do i=j,kmu
             idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnw + m;
             call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dlijmn )
             call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dljimn )
             call symhessvany(i,j, dlijmn, dljimn, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
          enddo
       enddo
    enddo
    ! phi - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmv
             do i=1,kmu
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnPhi + (n-1) * knu + m;
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, hessv)
                call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             enddo
          enddo
       enddo
    enddo
    ! phi - w
    do j=1,kmv
       do i=1,kmu
          do m=1,knu
             idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnw + m
             call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
          enddo
       enddo
    enddo
    ! phi - v
    do n=1,knu
       do m=n,knu
          do j=1,kmv
             do i=1,kmu
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnv + iijtouplolidx(knu,m,n)
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dljimn)
                call symhessvany(m,n, dlijmn, dljimn, hessv)
                call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             enddo
          enddo
       enddo
    enddo
    ! w - phi
    do i=1,kmu
       do n=1,knv
          do m=1,knu
             idx_m = admw + i;   idx_n = adnPhi + (n-1) * knu + m
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
          enddo
       enddo
    enddo
    ! w - w
    do i=1,kmu;
       do m=1,knu
          idx_m = admw + i;   idx_n = adnw + m
          call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
               & zeroknvknv, dgamdwev(:,m), kr, knv, x0, hessv)
          call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
       enddo
    enddo
    ! w - V
    do i=1,kmu
       do n=1,knu
          do m=n,knu
             idx_m = admw + i;   idx_n = adnV + iijtouplolidx(knu,m,n)
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm)
             call symhessvany(m,n, dlijmn, dlijnm, hessv)
             call bilinupdt(hessv, bilinmat, derivlen, idx_big, idx_small, dir, ndir)
          enddo
       enddo
    enddo
    deallocate(zeroknvkr,zeroknvknv)
  end subroutine

  recursive subroutine tntm(kr,knv,knu,kmv,kmu,dfqk1_ch, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, x0, &
       & hessflat, derivlen, admphi, admV, admw, adnphi, adnV, adnw) bind(C, name="tntm_")
    implicit none
    integer(c_int)    :: kr, kmv, kmu, knv, knu
    integer(c_long)   :: derivlen, admphi, admV, admw, adnphi, adnV, adnw
    type(dfdqdkCMEM) :: dfqk1_ch
    real(c_double)    :: &
                       & dodvev(knv,knv,knu,knu), dodphiev(knv,knv,knu,knv), &
                       & dgamdvev(knv,knu,knu), dgamdwev(knv,knu), dgamdphiev(knv,knu,knv), &
                       & x0(kr), hessflat((derivlen*(derivlen+1))/2_c_int), dlijmn,dljimn,dljinm,dlijnm
    real(c_double), allocatable :: zeroknvkr(:,:),zeroknvknv(:,:)
    type(dfdqdk)      :: dfqk1
    integer(c_int)    :: i, j, m, n
    integer(c_long), target  :: idx_m, idx_n
    integer(c_long), pointer :: idx_big, idx_small
    allocate(zeroknvkr(knv,kr),zeroknvknv(knv,knv))
    call read_dfqk(dfqk1_ch, dfqk1)
    zeroknvkr = 0.0_c_double;  zeroknvknv = 0.0_c_double
    
    if (admV < adnV) then
       idx_big => idx_n;        idx_small => idx_m
    else
       idx_big => idx_m;        idx_small => idx_n
    endif
    
    ! v - v
    do n=1,knu
       do m=n,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnV+iijtouplolidx(knu,m,n)
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dljimn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dljinm )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm )
                call symhessvv(i,j,m,n,dlijmn,dljimn,dljinm,dlijnm, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
             enddo
          enddo
       enddo
    enddo
    ! v - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnPhi+ (n-1) * knu + m;
                call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dlijmn )
                call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dljimn )
                call symhessvany(i,j, dlijmn, dljimn, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
             enddo
          enddo
       enddo
    enddo
    ! v - w
    do m=1,knu
       do j=1,kmu
          do i=j,kmu
             idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnw + m;
             call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dlijmn )
             call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dljimn )
             call symhessvany(i,j, dlijmn, dljimn, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
          enddo
       enddo
    enddo
    ! phi - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmv
             do i=1,kmu
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnPhi + (n-1) * knu + m;
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
             enddo
          enddo
       enddo
    enddo
    ! phi - w
    do j=1,kmv
       do i=1,kmu
          do m=1,knu
             idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnw + m
             call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                  & zeroknvknv, dgamdwev(:,m), kr, knv, x0, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
          enddo
       enddo
    enddo
    ! phi - v
    do n=1,knu
       do m=n,knu
          do j=1,kmv
             do i=1,kmu
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnv + iijtouplolidx(knu,m,n)
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dljimn)
                call symhessvany(m,n, dlijmn, dljimn, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
             enddo
          enddo
       enddo
    enddo
    ! w - phi
    do i=1,kmu
       do n=1,knv
          do m=1,knu
             idx_m = admw + i;   idx_n = adnPhi + (n-1) * knu + m
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
          enddo
       enddo
    enddo
    ! w - w
    do i=1,kmu;
       do m=1,knu
          idx_m = admw + i;   idx_n = adnw + m
          call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
               & zeroknvknv, dgamdwev(:,m), kr, knv, x0, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
       enddo
    enddo
    ! w - V
    do i=1,kmu
       do n=1,knu
          do m=n,knu
             idx_m = admw + i;   idx_n = adnV + iijtouplolidx(knu,m,n)
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm)
             call symhessvany(m,n, dlijmn, dlijnm, hessflat(ijtouplolidx(derivlen, idx_big, idx_small)))
          enddo
       enddo
    enddo
    deallocate(zeroknvkr,zeroknvknv)
  end subroutine

  !! ONE OF THE BOTTLENECK FUNCTION
  recursive subroutine tntmthrfast(kr,knv,knu,kmv,kmu,dfqk1_ch, &
       & dodvev, dodphiev, dgamdvev, dgamdwev, dgamdphiev, x0, &
       & wsp, lwsp) bind(C, name="tntmthrfast_")
    implicit none
    integer(c_int)    :: kr, kmv, kmu, knv, knu
    type(dfdqdkCMEM) :: dfqk1_ch
    integer(c_size_t)   :: lwsp
    real(c_double)    :: &
                       & dodvev(knv,knv,knu,knu), dodphiev(knv,knv,knu,knv), &
                       & dgamdvev(knv,knu,knu), dgamdwev(knv,knu), dgamdphiev(knv,knu,knv), &
                       & x0(kr), dlijmn,dljimn,dljinm,dlijnm, &
                       & wsp(lwsp)
    real(c_double), allocatable :: zeroknvkr(:,:), zeroknvknv(:,:)
    type(dfdqdk)      :: dfqk1
    integer(c_int)    :: i, j, m, n
    integer(c_size_t)   :: r
    allocate(zeroknvkr(knv,kr),zeroknvknv(knv,knv))
    call read_dfqk(dfqk1_ch, dfqk1)
    zeroknvkr = 0.0_c_double;  zeroknvknv = 0.0_c_double
    r = 1
    ! v - v
    do n=1,knu
       do m=n,knu
          do j=1,kmu
             do i=j,kmu
                if (i /= j) then
                   if (m /= n) then
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dljimn )
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dljinm )
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm )
                      wsp(r) = dlijmn+dljimn+dljinm+dlijnm
                   else
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dljimn )
                      wsp(r) = dlijmn + dljimn
                   endif
                else
                   if (m /= n) then
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm )
                      wsp(r) = dlijmn + dlijnm
                   else
                      call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                           & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn )
                      wsp(r) = dlijmn
                   endif
                endif
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmu
             do i=j,kmu
                if (i /= j) then
                   call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                        & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dlijmn )
                   call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                        & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dljimn )
                   wsp(r)=dlijmn+dljimn
                else
                   call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                        & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, dlijmn )
                   wsp(r)=dlijmn
                endif
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - w
    do m=1,knu
       do j=1,kmu
          do i=j,kmu
            if (i /= j) then
               call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                    & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dlijmn )
               call d2Lijmntmtn( dfqk1%dfdv(:,:,j,i), dfqk1%dqdv(:,j,i), dfqk1%dkdv(:,:,j,i), dfqk1%f1n, dfqk1%q1n, &
                    & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dljimn )
               wsp(r)=dlijmn+dljimn
            else
               call d2Lijmntmtn( dfqk1%dfdv(:,:,i,j), dfqk1%dqdv(:,i,j), dfqk1%dkdv(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                    & zeroknvknv, dgamdwev(:,m), kr, knv, x0, dlijmn )
               wsp(r)=dlijmn
            endif
            r=r+1
         enddo
      enddo
   enddo
    ! phi - phi
   do n=1,knv
      do m=1,knu;
         do j=1,kmv
            do i=1,kmu;
               call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                    & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, wsp(r))
               r=r+1
            enddo
         enddo
      enddo
   enddo
    ! phi - w
   do j=1,kmv
      do i=1,kmu;
         do m=1,knu
            call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                 & zeroknvknv, dgamdwev(:,m), kr, knv, x0, wsp(r))
            r=r+1
         enddo
      enddo
   enddo
    ! phi - v
   do n=1,knu
      do m=n,knu;
         do j=1,kmv
            do i=1,kmu;
               if (m /= n) then
                  call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                       & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                  call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                       & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm)
                  wsp(r) = dlijmn + dlijnm
               else
                  call d2Lijmntmtn( dfqk1%dfdphi(:,:,i,j), dfqk1%dqdphi(:,i,j), dfqk1%dkdphi(:,:,i,j), dfqk1%f1n, dfqk1%q1n, &
                       & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                  wsp(r) = dlijmn
               endif
               r=r+1
            enddo
         enddo
      enddo
   enddo
    ! w - phi
    do i=1,kmu
       do n=1,knv
          do m=1,knu
             call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                  & dodphiev(:,:,m,n), dgamdphiev(:,m,n), kr, knv, x0, wsp(r))
             r=r+1
          enddo
       enddo
    enddo
    ! w - w
    do i=1,kmu
       do m=1,knu
          call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
               & zeroknvknv, dgamdwev(:,m), kr, knv, x0, wsp(r))
          r=r+1
       enddo
    enddo
    ! w - V
    do i=1,kmu
       do n=1,knu
          do m=n,knu
             if (m /= n) then
                call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,n,m), dgamdvev(:,n,m), kr, knv, x0, dlijnm)
                wsp(r) = dlijmn + dlijnm
             else
                call d2Lijmntmtn( zeroknvkr, dfqk1%dqdw(:,i), zeroknvknv, dfqk1%f1n, dfqk1%q1n, &
                     & dodvev(:,:,m,n), dgamdvev(:,m,n), kr, knv, x0, dlijmn)
                wsp(r) = dlijmn
             endif
             r=r+1
          enddo
       enddo
    enddo
    deallocate(zeroknvkr,zeroknvknv)
!    if (r-1 /= lwsp) print *, "LENGTH IS WRONG!", r-1, lwsp
    !! lwsp = (knu*(knu+1)/2)*(kmu*(kmu+1)/2)+ knv*knu + kmu*kmu + knu*(kmu*(kmu+1))/2 + knv*knu*kmv*kmu
    !!        + kmv*kmu*knu + (knu*(knu+1))/2 * kmv*kmu + knu*kmv*kmu + kmu*knu + kmu*knu*(knu+1)/2
  end subroutine

  recursive subroutine tntmcpydir(knv,knu,kmv,kmu,&
       & wsp, lwsp, bilinmat, dir, ndir, derivlen, admphi, admV, admw, adnphi, adnV, adnw) bind(C, name="tntmcpydir_")
    implicit none
    integer(c_int)    :: knv,knu,kmv,kmu, ndir
    integer(c_size_t) :: lwsp
    integer(c_long)   :: admphi, admV, admw, adnphi, adnV, adnw, derivlen
    real(c_double) :: bilinmat(ndir,ndir), dir(derivlen,ndir)
    real(c_double) :: wsp(lwsp)
    integer(c_long), target  :: idx_m, idx_n
    integer(c_long), pointer :: idx_big, idx_small
    integer(c_int)    :: i,j,m,n
    integer(c_long)   :: r
    if (admV < adnV) then
       idx_big => idx_n;        idx_small => idx_m
    else
       idx_big => idx_m;        idx_small => idx_n
    endif
    r = 1
    ! v - v
    do n=1,knu
       do m=n,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnV+iijtouplolidx(knu,m,n)
                call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnPhi+ (n-1) * knu + m;
                call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - w
    do m=1,knu
       do j=1,kmu
          do i=j,kmu
             idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnw + m;
             call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             r=r+1
          enddo
       enddo
    enddo
    ! phi - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmv
             do i=1,kmu;
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnPhi + (n-1) * knu + m;
                call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! phi - w
    do j=1,kmv
       do i=1,kmu
          do m=1,knu
             idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnw + m
             call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             r=r+1
          enddo
       enddo
    enddo
    ! phi - v
    do n=1,knu
       do m=n,knu
          do j=1,kmv
             do i=1,kmu;
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnv + iijtouplolidx(knu,m,n)
                call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
                r = r+1
             enddo
          enddo
       enddo
    enddo
    ! w - phi
    do i=1,kmu
       do n=1,knv
          do m=1,knu
             idx_m = admw + i;   idx_n = adnPhi + (n-1) * knu + m
             call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             r = r+1
          enddo
       enddo
    enddo
    ! w - w
    do i=1,kmu
       do m=1,knu
          idx_m = admw + i;   idx_n = adnw + m
          call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
          r = r+1
       enddo
    enddo
    ! w - V
    do i=1,kmu
       do n=1,knu
          do m=n,knu
             idx_m = admw + i;   idx_n = adnV + iijtouplolidx(knu,m,n)
             call bilinupdt(wsp(r), bilinmat, derivlen, idx_big, idx_small, dir, ndir)
             r = r+1
          enddo
       enddo
    enddo
!    if (r-1 /= lwsp) print *, "LENGTH IS WRONG!", r-1, lwsp
  end subroutine

  recursive subroutine tntmcpy(knv,knu,kmv,kmu,&
       & wsp, lwsp, hessflat, derivlen, admphi, admV, admw, adnphi, adnV, adnw) bind(C, name="tntmcpy_")
    implicit none
    integer(c_int)    :: knv,knu,kmv,kmu
    integer(c_size_t) :: lwsp
    integer(c_long)   :: admphi, admV, admw, adnphi, adnV, adnw, derivlen
    real(c_double) :: hessflat((derivlen*(derivlen+1))/2_c_int)
    real(c_double) :: wsp(lwsp)
    integer(c_long), target  :: idx_m, idx_n
    integer(c_long), pointer :: idx_big, idx_small
    integer(c_int)    :: i,j,m,n
    integer(c_long)   :: r
    if (admV < adnV) then
       idx_big => idx_n;        idx_small => idx_m
    else
       idx_big => idx_m;        idx_small => idx_n
    endif
    r = 1
    ! v - v
    do n=1,knu
       do m=n,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnV+iijtouplolidx(knu,m,n)
                hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmu
             do i=j,kmu
                idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnPhi+ (n-1) * knu + m;
                hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! v - w
    do m=1,knu
       do j=1,kmu
          do i=j,kmu
             idx_m = admV+iijtouplolidx(kmu,i,j);  idx_n = adnw + m;
             hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
             r=r+1
          enddo
       enddo
    enddo
    ! phi - phi
    do n=1,knv
       do m=1,knu
          do j=1,kmv
             do i=1,kmu;
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnPhi + (n-1) * knu + m;
                hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
                r=r+1
             enddo
          enddo
       enddo
    enddo
    ! phi - w
    do j=1,kmv
       do i=1,kmu
          do m=1,knu
             idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnw + m
             hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
             r=r+1
          enddo
       enddo
    enddo
    ! phi - v
    do n=1,knu
       do m=n,knu
          do j=1,kmv
             do i=1,kmu;
                idx_m = admPhi + (j-1) * kmu + i;   idx_n = adnv + iijtouplolidx(knu,m,n)
                hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
                r = r+1
             enddo
          enddo
       enddo
    enddo
    ! w - phi
    do i=1,kmu
       do n=1,knv
          do m=1,knu
             idx_m = admw + i;   idx_n = adnPhi + (n-1) * knu + m
             hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
             r = r+1
          enddo
       enddo
    enddo
    ! w - w
    do i=1,kmu
       do m=1,knu
          idx_m = admw + i;   idx_n = adnw + m
          hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
          r = r+1
       enddo
    enddo
    ! w - V
    do i=1,kmu
       do n=1,knu
          do m=n,knu
             idx_m = admw + i;   idx_n = adnV + iijtouplolidx(knu,m,n)
             hessflat(ijtouplolidx(derivlen, idx_big, idx_small)) = wsp(r)
             r = r+1
          enddo
       enddo
    enddo
!    if (r-1 /= lwsp) print *, "LENGTH IS WRONG!", r-1, lwsp
  end subroutine

  recursive subroutine bilinupdt(d, bilinmat, npar, idx1, idx2, dir, ndir) bind(C, name="bilinupdt_")
    integer(c_int) ndir
    real(c_double) d, bilinmat, dir
    integer(c_long) :: npar, idx1, idx2
    dimension bilinmat(ndir,ndir), dir(npar,ndir)
    do j = 1,ndir
       do i = j,ndir
          if (idx1 /= idx2) then
             bilinmat(i,j) = bilinmat(i,j) + d * (dir(idx1,i) * dir(idx2,j) + dir(idx2,i) * dir(idx1,j))
          else
             bilinmat(i,j) = bilinmat(i,j) + d * dir(idx1,i) * dir(idx2,j)
          endif
          bilinmat(j,i) = bilinmat(i,j)
       enddo
    enddo
  end subroutine

  recursive subroutine hesscpyskip(Hnew, nnew, Hold, nold, m, istart, ihowmuch) bind(C, name="hesscpyskip_")
    integer(c_int) nnew, nold, m, istart, ihowmuch
    real(c_double) Hnew, Hold
    dimension Hnew(m, nnew, nnew), Hold(m,nold,nold)
    iin = 1_c_int
    do iio=1,nold
       if (.not. (iio > istart .and. iio <= istart+ihowmuch)) then
          ijn = 1_c_int
          do ijo=1,nold
             if (.not. (ijo > istart .and. ijo <= istart+ihowmuch)) then
                do im=1,m
                   Hnew(im,iin,ijn) = Hold(im,iio,ijo)
                enddo
                ijn = ijn+1
             endif
          enddo
          iin = iin+1
       endif
    enddo
  end subroutine

  recursive subroutine hesschopnondiag(Hnew, nnew, Hold, nold, m, istart, k) bind(C, name="hesschopnondiag_")
    real(c_double) Hnew, Hold
    integer(c_int) nnew, nold, m, istart, k
    dimension Hnew(m, nnew, nnew), Hold(m,nold,nold)
    ijdiag = 0_c_int
    ijo = 1_c_int
    ijn = 1_c_int
1   if (ijo > nold) goto 100
    if ((ijo >= istart+1_c_int) .and. (ijo < istart+k*k))   ijo= ijo+ijdiag
    iidiag = 0_c_int
    iio = 1_c_int
    iin = 1_c_int
2   if (iio > nold) goto 90
    if ((iio >= istart+1_c_int) .and. (iio < istart+k*k))   iio= iio+iidiag
    do im=1,m
       Hnew(im,iin,ijn) = Hold(im,iio,ijo)
    enddo
    if (iio >= istart+1_c_int .and. iio < istart+k*k) then
       iio = iio + (k-iidiag)
       iidiag = iidiag+1_c_int
    else
       iio = iio + 1_c_int
    endif
    iin = iin+1_c_int
    goto 2
90  continue
    if (ijo >= istart+1_c_int .and. ijo < istart+k*k) then
       ijo    = ijo+ (k-ijdiag)
       ijdiag = ijdiag + 1_c_int
    else
       ijo    = ijo+ 1_c_int
    endif
    ijn = ijn+1_c_int
    goto 1
100 continue
  end subroutine


  subroutine hessdiag2ltri (Hnew, nnew, Hold, nold, m, k, istart) bind(C, name='hessdiag2ltri_')
    integer(c_int) nnew, nold, m, k, istart
    real(c_double) Hnew, Hold
    dimension Hnew(m,nnew,nnew),  Hold(m,nold,nold)
    ijdiag = 0_c_int
    ijo = 1_c_int
    ijn = 1_c_int
1   if (ijo > nold) goto 100
    iidiag = 0_c_int
    iio = 1_c_int
    iin = 1_c_int
2   if (iio > nold) goto 90
    do im=1,m
       Hnew(im,iin,ijn) = Hold(im,iio,ijo)
    enddo
    if (iio >= istart+1_c_int .and. iio < istart+(k*(k+1_c_int))/2_c_int) then
       iio = iio + (k-iidiag)
       iidiag = iidiag+1_c_int
    else
       iio = iio + 1_c_int
    endif
    iin = iin+1_c_int
    goto 2
90  continue
    if (ijo >= istart+1_c_int .and. ijo < istart+(k*(k+1_c_int))/2_c_int) then
       ijo    = ijo+ (k-ijdiag)
       ijdiag = ijdiag + 1_c_int
    else
       ijo    = ijo+ 1_c_int
    endif
    ijn = ijn+1_c_int
    goto 1
100 continue
  end subroutine
    
end module dglinv
