subroutine f90_local_const_var_est_mult(eps,ttij,nobs,&
           nind,nmaxobs,ndim,ntimepoints,hh,var_est)
implicit none
integer,intent(in)::nind,nmaxobs,ndim,ntimepoints,hh
double precision,intent(in)::eps(nind,nmaxobs,ndim)
double precision::eps2(nind,nmaxobs,ndim)
integer,intent(in)::ttij(nind,nmaxobs),nobs(nind)
integer::tt,ii,jj,jj1,jj2,ll,ll1,ll2,diff,diff1,diff2
double precision::var_est(ntimepoints,ndim,ndim)
double precision::U,V
double precision::kterm1,kterm2,kterm
double precision::allkvalues(-hh:hh)

var_est=0d0

do ii=-hh,hh
 allkvalues(ii)=max(0d0,1d0-(dble(ii)/dble(hh))**2)
end do

do tt=1,ntimepoints
 U=0d0;V=0d0
 do ll1=1,ndim
  do ll2=1,ll1
   if(ll1.eq.ll2)cycle
   U=0d0;V=0d0
   do ii=1,nind
    do jj1=1,nobs(ii)
     diff1=ttij(ii,jj1)-tt
     if(diff1.ge.hh .or. diff1.le.-hh)cycle
     kterm1=allkvalues(diff1)
     do jj2=1,nobs(ii)
      diff2=ttij(ii,jj2)-tt
      if(diff2.ge.hh .or. diff2.le.-hh)cycle
      kterm2=allkvalues(diff2)
      kterm=kterm1*kterm2
      U=U+kterm
      V=V+kterm*eps(ii,jj1,ll1)*eps(ii,jj2,ll2)
     end do
    end do
   end do
   var_est(tt,ll1,ll2)=V/U
   var_est(tt,ll2,ll1)=var_est(tt,ll1,ll2)
  end do
 end do
end do

eps2=eps*eps
do tt=1,ntimepoints
 do ll=1,ndim
  U=0d0;V=0d0
  do ii=1,nind
   do jj=1,nobs(ii)
    diff=ttij(ii,jj)-tt
    if(diff.ge.hh .or. diff.le.-hh)cycle
    kterm=allkvalues(diff)
    U=U+kterm
    V=V+kterm*eps2(ii,jj,ll)
   end do
  end do
  var_est(tt,ll,ll)=V/U
 end do
end do

end subroutine
