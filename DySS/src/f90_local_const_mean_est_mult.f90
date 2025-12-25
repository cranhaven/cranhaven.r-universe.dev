subroutine f90_local_const_mean_est_mult(yyij,ttij,nobs,&
           nind,nmaxobs,ndim,ntimepoints,hh,mu_est)
implicit none
integer,intent(in)::nind,nmaxobs,ndim,ntimepoints,hh
double precision,intent(in)::yyij(nind,nmaxobs,ndim)
integer,intent(in)::ttij(nind,nmaxobs),nobs(nind)
integer::tt,ii,jj,ll,diff
double precision::mu_est(ntimepoints,ndim)
double precision::U,V
double precision::kterm
double precision::allkvalues(-hh:hh)

mu_est=0d0

do ii=-hh,hh
 allkvalues(ii)=max(0d0,1d0-(dble(ii)/dble(hh))**2)
end do

do ll=1,ndim
 do tt=1,ntimepoints
  U=0d0;V=0d0
  do ii=1,nind
   do jj=1,nobs(ii)
    diff=ttij(ii,jj)-tt
    if(diff.ge.hh .or. diff.le.-hh)cycle
    kterm=allkvalues(diff)
    U=U+kterm
    V=V+kterm*yyij(ii,jj,ll)
   end do
  end do
  mu_est(tt,ll)=V/U
 end do
end do

end subroutine
