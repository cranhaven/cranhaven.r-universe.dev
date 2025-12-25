subroutine f90_mchart_simultaneous_CUSUM_both(eeijk,nobs,&
           nind,nmaxobs,ndim,allowance,limit,CCij,SSijk_upward,SSijk_downward)
use, intrinsic :: ieee_arithmetic, only: IEEE_VALUE, IEEE_QUIET_NAN
use, intrinsic :: iso_fortran_env, only: real32
implicit none
integer,intent(in)::nind,nmaxobs,ndim
integer,intent(in)::nobs(nind)
double precision,intent(in)::allowance,limit
double precision,intent(in)::eeijk(nind,nmaxobs,ndim)
double precision::SSijk_upward(nind,nmaxobs,ndim),SSijk_downward(nind,nmaxobs,ndim)
double precision::CCij(nind,nmaxobs)
integer::ii,jj,kk
double precision::nan

CCij=IEEE_VALUE(nan, IEEE_QUIET_NAN)
SSijk_upward=IEEE_VALUE(nan, IEEE_QUIET_NAN)
SSijk_downward=IEEE_VALUE(nan, IEEE_QUIET_NAN)

do ii=1,nind
 do kk=1,ndim
  SSijk_upward(ii,1,kk)=max(0d0,eeijk(ii,1,kk)-allowance)
  SSijk_downward(ii,1,kk)=max(0d0,-eeijk(ii,1,kk)-allowance)
 end do
 CCij(ii,1)=max(maxval(SSijk_upward(ii,1,:)),maxval(SSijk_downward(ii,1,:)))
 if(CCij(ii,1)>limit)cycle
 do jj=2,nobs(ii)
  do kk=1,ndim
   SSijk_upward(ii,jj,kk)=max(0d0,SSijk_upward(ii,jj-1,kk)+eeijk(ii,jj,kk)-allowance)
   SSijk_downward(ii,jj,kk)=max(0d0,SSijk_downward(ii,jj-1,kk)-eeijk(ii,jj,kk)-allowance)
  end do
  CCij(ii,jj)=max(maxval(SSijk_upward(ii,jj,:)),maxval(SSijk_downward(ii,jj,:)))
  if(CCij(ii,jj)>limit)exit
 end do
end do

end subroutine

