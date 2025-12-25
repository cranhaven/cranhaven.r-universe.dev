subroutine f90_mchart_simultaneous_EWMA_both(eeijk,nobs,&
           nind,nmaxobs,ndim,lambda,limit,CCij,SSijk)
use, intrinsic :: ieee_arithmetic, only: IEEE_VALUE, IEEE_QUIET_NAN
use, intrinsic :: iso_fortran_env, only: real32
implicit none
integer,intent(in)::nind,nmaxobs,ndim,nobs(nind)
double precision,intent(in)::lambda,limit
double precision,intent(in)::eeijk(nind,nmaxobs,ndim)
double precision::SSijk(nind,nmaxobs,ndim)
double precision::CCij(nind,nmaxobs)
integer::ii,jj,kk
double precision::nan

CCij=IEEE_VALUE(nan, IEEE_QUIET_NAN)
SSijk=IEEE_VALUE(nan, IEEE_QUIET_NAN)

do ii=1,nind
 do kk=1,ndim
  SSijk(ii,1,kk)=lambda*eeijk(ii,1,kk)
 end do
 CCij(ii,1)=maxval(abs(SSijk(ii,1,:)))
 if(CCij(ii,1)>limit)cycle
 do jj=2,nobs(ii)
  do kk=1,ndim
   SSijk(ii,jj,kk)=(1d0-lambda)*SSijk(ii,jj-1,kk)+lambda*eeijk(ii,jj,kk)
  end do
  CCij(ii,jj)=maxval(abs(SSijk(ii,jj,:)))
  if(CCij(ii,jj)>limit)exit
 end do
end do

end subroutine
