module chn
  use, intrinsic :: iso_c_binding
  implicit integer(c_int) (i-k), integer(c_int) (m,n), &
       & real(c_double) (a-h), real(c_double) (l), real(c_double) (o-z)
contains
  
  ! wsp needs to be of size 2*npar^2+1, initialised as zero when first calling
  ! this subroutine.
  subroutine curvifyupdate(H, HV, Hw, HPhi, npar, ku, kv, &
       & dlikdv, dlikdw, dlikdphi, wsp) bind(C, name="curvifyupdate_")
    integer(c_int) ku, kv, npar
    real(c_double) H, HV, Hw, HPhi, &
       & dlikdv, dlikdw, dlikdphi, wsp
    dimension H(npar, npar), HV((ku*(ku+1))/2,npar,npar), Hw(ku,npar,npar), HPhi(ku*kv,npar,npar), &
         & dlikdv(ku,ku), dlikdw(ku), dlikdphi(ku,kv), wsp(2*(npar**2)+1)
    target wsp
    real(c_double), pointer :: dkahan(:,:), tmp(:,:), y
    dkahan(1:npar,1:npar) => wsp(1:(npar**2))
    tmp(1:npar,1:npar)    => wsp(((npar**2)+1):(2*(npar**2)))
    y                     => wsp(2*(npar**2)+1)
    tmp = 0.0_c_double
    do j = 1,npar
       do i = 1,npar
          ic = 1
          do n = 1,ku
             do m = n,ku
                y           = dlikdv(m,n) * HV(ic,i,j) - dkahan(i,j)
                tmp(i,j)    = H(i,j) + y
                dkahan(i,j) = (tmp(i,j) - H(i,j)) - y
                H(i,j)      = tmp(i,j)
!                H(i,j) = H(i,j) + dlikdv(m,n) * HV(ic,i,j)
                ic          = ic+1
             enddo
          enddo
       enddo
    enddo
    ! The w part
    do j = 1,npar
       do i = 1,npar
          do m = 1,ku
             y              = dlikdw(m) * Hw(m,i,j) - dkahan(i,j)
             tmp(i,j)       = H(i,j) + y
             dkahan(i,j)    = (tmp(i,j) - H(i,j)) - y
             H(i,j)         = tmp(i,j)
!             H(i,j) = H(i,j) + dlikdw(m) * Hw(m,i,j)
          enddo
       enddo
    enddo
    ! The Phi part
    do j = 1,npar
       do i = 1,npar
          ic = 1
          do n = 1,kv
             do m = 1,ku
                y              = dlikdPhi(m,n) * HPhi(ic,i,j) - dkahan(i,j)
                tmp(i,j)       = H(i,j) + y
                dkahan(i,j)    = (tmp(i,j) - H(i,j)) - y
                H(i,j)         = tmp(i,j)
!                H(i,j) = H(i,j) + dlikdPhi(m,n) * HPhi(ic,i,j)
                ic = ic+1
             enddo
          enddo
       enddo
    enddo
  end subroutine
end module
