subroutine f90_local_const_cov_est_mult(eps,ttij,nobs,&
           nind,nmaxobs,ndim,ntimepoints,hh,varcov_est)
implicit none
integer,intent(in)::nind,nmaxobs,ndim,ntimepoints,hh
double precision,intent(in)::eps(nind,nmaxobs,ndim)
double precision::eps2(nind,nmaxobs,ndim)
integer,intent(in)::ttij(nind,nmaxobs),nobs(nind)
integer::tt,tt1,tt2,ii,jj,jj1,jj2,ll,ll1,ll2,diff,diff1,diff2
double precision::varcov_est(ntimepoints,ntimepoints,ndim,ndim)
double precision::U,V
double precision::kterm1,kterm2,kterm
double precision::allkvalues(-hh:hh)
integer::starting_idx(ntimepoints,nind),ending_idx(ntimepoints,nind)

do ii=-hh,hh
 allkvalues(ii)=max(0d0,1d0-(dble(ii)/dble(hh))**2)
end do

do tt=1,ntimepoints
 do ii=1,nind
  
  if(ttij(ii,nobs(ii)).le.tt-hh)then
   starting_idx(tt,ii)=nobs(ii)+1
  else
   do jj=1,nobs(ii)
    if(ttij(ii,jj).gt.tt-hh)then
     starting_idx(tt,ii)=jj
     exit
    end if
   end do
  end if
  
  if(ttij(ii,1).ge.tt+hh)then
   ending_idx(tt,ii)=0
  else 
   do jj=nobs(ii),1,-1
    if(ttij(ii,jj).le.tt+hh)then
     ending_idx(tt,ii)=jj
     exit
    end if
   end do
  end if
  
 end do
end do
    
do tt1=1,ntimepoints
 do tt2=1,ntimepoints
  do ll1=1,ndim
   do ll2=1,ll1
    U=0d0;V=0d0
    do ii=1,nind
     do jj1=starting_idx(tt1,ii),ending_idx(tt1,ii) !1,nobs(ii)
      diff1=ttij(ii,jj1)-tt1
      !if(diff1.ge.hh .or. diff1.le.-hh)cycle
      kterm1=allkvalues(diff1)
      do jj2=starting_idx(tt2,ii),ending_idx(tt2,ii) !1,nobs(ii)
       diff2=ttij(ii,jj2)-tt2
       !if(diff2.ge.hh .or. diff2.le.-hh)cycle
       if(ll1.eq.ll2 .and. jj1.eq.jj2)cycle
       kterm2=allkvalues(diff2)
       kterm=kterm1*kterm2
       U=U+kterm
       V=V+kterm*eps(ii,jj1,ll1)*eps(ii,jj2,ll2)
      end do
     end do
    end do
    varcov_est(tt1,tt2,ll1,ll2)=V/U
    varcov_est(tt2,tt1,ll2,ll1)=varcov_est(tt1,tt2,ll1,ll2)
   end do
  end do
 end do
end do

eps2=eps*eps
do tt=1,ntimepoints
 do ll=1,ndim
  U=0d0;V=0d0
  do ii=1,nind
   do jj=starting_idx(tt,ii),ending_idx(tt,ii) !1,nobs(ii)
    diff=ttij(ii,jj)-tt
    !if(diff.ge.hh .or. diff.le.-hh)cycle
    kterm=allkvalues(diff)
    U=U+kterm
    V=V+kterm*eps2(ii,jj,ll)
   end do
  end do
  varcov_est(tt,tt,ll,ll)=V/U
 end do
end do

end subroutine
