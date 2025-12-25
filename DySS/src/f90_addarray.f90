subroutine f90_addarray(AA,ndim)
use, intrinsic :: ieee_arithmetic, only: IEEE_VALUE, IEEE_QUIET_NAN
use, intrinsic :: iso_fortran_env, only: real32
implicit none
double precision :: nan
integer::ndim
double precision::AA(ndim,ndim,ndim,ndim)
integer::ii1,ii2,ii3,ii4
do ii1=1,ndim
 do ii2=1,ndim
  do ii3=1,ndim
   do ii4=1,ndim
    AA(ii1,ii2,ii3,ii4)=max(0d0,AA(ii1,ii2,ii3,ii4)+1d0)
    if(ii1.eq.ndim)AA(ii1,ii2,ii3,ii4)=IEEE_VALUE(nan, IEEE_QUIET_NAN)
   end do
  end do
 end do
end do
end subroutine