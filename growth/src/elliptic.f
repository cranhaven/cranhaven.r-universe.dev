c
c  growth : A Library of Normal Distribution Growth Curve Models
c  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
c
c  This program is free software; you can redistribute it and/or modify
c  it under the terms of the GNU General Public License as published by
c  the Free Software Foundation; either version 2 of the License, or
c  (at your option) any later version.
c
c  This program is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c  GNU General Public License for more details.
c
c  You should have received a copy of the GNU General Public License
c  along with this program; if not, write to the Free Software
c  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
c
c  SYNOPSIS
c
c     subroutine plra(theta,like,iist,rxl,x,y,tvcov,ccov,dose,nobs,
c    +     nbs,nest,lnest,dev,nind,nld,nxrl,np,npell,npv,npvl,
c    +     nccov,npvar,cvar,ccvar,twins,npre,npar,link,torder,inter,
c    +     model,ar,tvc,beta,betacov,v,sigsq,ey,tb,mu,var,covar)
c
c  DESCRIPTION
c
c    Function to compute the likelihood function for the multivariate
c elliptical distribution with various autocorrelation functions,
c one or two levels of random effects, and nonlinear regression.
c
c
      subroutine plra(theta,like,dist,rxl,x,y,tvcov,ccov,dose,nobs,
     +     nbs,nest,lnest,dev,nind,nld,nxrl,np,npell,npv,npvl,
     +     nccov,npvar,cvar,ccvar,twins,npre,npar,link,torder,inter,
     +     model,ar,tvc,beta,betacov,v,sigsq,ey,tb,mu,var,covar)
c
c routine for computing -log(l) under the various models,
c including both autoregression and (nested) random effects
c
      implicit none
      integer np,nind,nxrl,link,i,j,k,nccov,tvc,ar,nbs,npell,dist,
     +     lnest,nm,torder,model,npv,npvar,npre,npar,nld,npvl,cvar,
     +     ccvar,twins
      integer nobs(nind),inter(nccov),nest(*)
      double precision x(*),y(*),beta(npvl),betacov(npvl,npvl),
     +     tvcov(*),rxl(nind),dev(*),ccov(nind,nccov),ey(nld),
     +     like,theta(np),tb(npvl),v(nld,nld),tausq(2),sigsq(nld),
     +     rho,ldet,qf,qf2,qf3,dose(nind),mu(*),var(*),pow,tmp1,tmp2,
     +     covar(*)
c
c     compute parameters of the covariance matrix
c
      if(npvar.eq.1.and.cvar.eq.0)then
         if(theta(npv+1).gt.24)theta(npv+1)=24.
         tmp1=dexp(theta(npv+1))
         do 1 i=1,nld
            sigsq(i)=tmp1
 1       continue
      endif
      if(npre.gt.0.and.ccvar.eq.0)then
         do 3 i=1,npre
            if(theta(npv+npvar+i).gt.24)theta(npv+npvar+i)=24.
            tausq(i)=dexp(theta(npv+npvar+i))
 3       continue
         if(npre.eq.1)tausq(2)=0.
      else
         tausq(1)=0.
         tausq(2)=0.
      endif
      if(npar.gt.0)then
         if(dabs(theta(npv+npvar+npre+1)).lt.24)then
            rho=dexp(theta(npv+npvar+npre+1))
            if(ar.ne.5)then
               rho=rho/(1+rho)
               if(rho.eq.1.)rho=0.9999
            endif
         else
            rho=0.9999
         endif
      else
         rho=0.
      endif
c
c estimate parameters in the linear model
c
      if(model.eq.1)then
         if(cvar.eq.0.and.ccvar.eq.0.and.link.eq.1.and.dist.ne.4)then
            call plml(x,y,beta,betacov,tb,ccov,tvcov,np,npv,npvl,
     +           nld,npvar,nind,nobs,nccov,torder,inter,tvc,v,
     +           sigsq,tausq,rho,nest,lnest,npar,npre,ar,nbs,twins)
         else
            npvl=npv
            do 2 i=1,npv
               beta(i)=theta(i)
 2          continue
         endif
      endif
c
c     compute the likelihood components
c
      like=0.d0
      nm=0
      do 6 i=1,nind
c
c     if necessary, compute variance function
c
         if(npvar.gt.1.or.cvar.gt.0)then
            call plmv(dev,theta,i,nm,nind,rxl,x,y,ccov,dose(i),nobs(i),
     +        link,nxrl,torder,model,tvcov,tvc,nccov,npv,npvar,np,ey,
     +        nld,beta,npvl,inter,sigsq,cvar,var,tb,nbs,mu)
         endif
c
c     if necessary, store value of covariance function
c
         if(ccvar.eq.1)tausq(1)=covar(nm+1)
c
c     compute the inverse of variance matrix and its log determinant
c
         call cmpvar(v,ldet,sigsq,tausq,rho,nind,i,nm,x,nobs(i),
     +        nest,lnest,nld,npre,npar,ar,twins)
c
c     compute the mean function
c
         call plmn(dev,theta,i,nm,nind,rxl,x,y,ccov,dose(i),nobs(i),
     +        link,nxrl,torder,model,tvcov,tvc,nccov,npv,npvar,np,
     +        ey,nld,beta,npvl,inter,tb,nbs,mu)
c
c     compute the quadratic form
c
         if(npell.gt.0.or.dist.ne.4)then
            qf=0.
            do 4 k=1,nobs(i)
               do 5 j=1,nobs(i)
                  qf=qf+dev(nm+k)*v(k,j)*dev(nm+j)
 5             continue
 4          continue
         endif
c
c     calculate likelihood for multivariate distribution
c
         if(npell.gt.0)then
            if(link.eq.1.and.cvar.eq.0)then
               if(dist.eq.4)then
                  pow=theta(np-npvl)
               else
                  pow=dexp(theta(np-npvl))
               endif
            else
               if(dist.eq.4)then
                  pow=theta(np)
               else
                  pow=dexp(theta(np))
               endif
            endif
            if(dist.ne.4.and.pow.gt.40)pow=40.
            if(dist.eq.2)then
c     multivariate power exponential
               call flgamma(dble(nobs(i))/2.,tmp1)
               call flgamma(1.+nobs(i)/(2.*pow),tmp2)
               like=like+(ldet+qf**pow)/2.+tmp2+(1.+nobs(i)/(2.*pow))*
     +              dlog(dble(2))-dlog(dble(nobs(i)))-tmp1
            else if(dist.eq.3)then
c     multivariate Student t
               call flgamma((pow+nobs(i))/2.,tmp1)
               call flgamma(pow/2,tmp2)
               like=like+(ldet+nobs(i)*dlog(pow)+(pow+nobs(i))
     +              *dlog(1+qf/pow))/2.-tmp1+tmp2
            else if(dist.eq.4)then
c     multivariate Laplace
               qf2=0.
               qf3=0.
               do 9 k=1,nobs(i)
                  do 10 j=1,nobs(i)
                     qf2=qf2+v(j,k)
                     qf3=qf3+dev(nm+j)*v(j,k)
 10               continue
 9             continue
               qf2=qf2*pow*pow
               qf3=qf3*pow
               tmp2=(2.-nobs(i))/2.
               call fbesselk(dsqrt((2+qf2)*qf),tmp2,tmp1)
               like=like-qf3+ldet/2.0-(tmp2/2.)*dlog(qf/(2+qf2))
     +              -dlog(tmp1)
            endif
         else if(dist.eq.4)then
c     multivariate Laplace
            qf=0.
            qf2=0.
            qf3=0.
            do 7 k=1,nobs(i)
               tmp2=y(nm+k)-dev(nm+k)
               do 8 j=1,nobs(i)
                  qf=qf+y(nm+j)*v(j,k)*y(nm+k)
                  qf2=qf2+(y(nm+j)-dev(nm+j))*v(j,k)*tmp2
                  qf3=qf3+y(nm+j)*v(j,k)*tmp2
 8             continue
 7          continue
            tmp2=(2.-nobs(i))/2.
            call fbesselk(dsqrt((2+qf2)*qf),tmp2,tmp1)
            like=like-qf3+ldet/2.0-(tmp2/2.)*dlog(qf/(2+qf2))
     +           -dlog(tmp1)
         else   
            like=like+ldet+qf
         endif
         nm=nm+nobs(i)
 6    continue
c
c     calculate likelihood for multivariate normal distribution
c
      if(dist.eq.1)like=like/2.+nbs*dlog(2.d0)/2.
      if(dist.eq.4)like=like+(nbs/2.-nind)*dlog(2.0d0)
      return
      end
c
c
      subroutine plml(x,y,beta,betacov,tb,ccov,tvcov,np,npv,
     +     npvl,nld,npvar,nind,nobs,nccov,torder,inter,tvc,
     +     v,sigsq,tausq,rho,nest,lnest,npar,npre,ar,nbs,twins)
c
c estimate parameters in linear part of model
c
      implicit none
      integer npvl,npvar,nind,nccov,torder,lnest,np,npv,nld,info,
     +     nm,i,j,k,l,m,j1,j2,k1,k2,npar,npre,tvc,ar,nbs,twins
      integer nobs(nind),inter(nccov),nest(*)
      double precision beta(npvl),betacov(npvl,npvl),tb(npvl),
     +     ccov(nind,nccov),v(nld,nld),tvcov(*),t1,t2,det(2),
     +     x(*),y(*),sigsq(nld),tausq(2),rho,ldet
c
      do 17 j=1,npvl
         tb(j)=0.
         do 16 k=1,npvl
            betacov(j,k)=0.
 16      continue
 17   continue
      nm=0
      do 18 i=1,nind
c
c     compute the inverse of variance matrix for individual i
c
         call cmpvar(v,ldet,sigsq,tausq,rho,nind,i,nm,x,nobs(i),
     +        nest,lnest,nld,npre,npar,ar,twins)
c
c     setup generalized least squares
c
         do 12 m=1,nobs(i)
            t1=1
            j1=1
            j2=1
            do 14 j=1,npvl
               if(j.gt.1)then
                  if(j.le.torder)then
                     t1=t1*x(nm+m)
                  else if(j.gt.npvl-tvc)then
                     t1=tvcov(nm+m+nbs*(j-npvl+tvc-1))
                  else
                     if(j2.gt.inter(j1))then
                        j1=j1+1
                        j2=1
                     endif
                     if(j2.eq.1)then
                        t1=ccov(i,j1)
                        j2=j2+1
                     else
                        t1=t1*x(nm+m)
                        j2=j2+1
                     endif
                  endif
               endif
               do 15 l=1,nobs(i)
                  tb(j)=tb(j)+t1*v(m,l)*y(nm+l)
                  t2=1
                  k1=1
                  k2=1
                  do 13 k=1,npvl
                     if(k.gt.1)then
                        if(k.le.torder)then
                           t2=t2*x(nm+l)
                        else if(k.gt.npvl-tvc)then
                           t2=tvcov(nm+l+nbs*(k-npvl+tvc-1))
                        else
                           if(k2.gt.inter(k1))then
                              k1=k1+1
                              k2=1
                           endif
                           if(k2.eq.1)then
                              t2=ccov(i,k1)
                              k2=k2+1
                           else
                              t2=t2*x(nm+l)
                              k2=k2+1
                           endif
                        endif
                     endif
                     betacov(j,k)=betacov(j,k)+t1*v(m,l)*t2
 13               continue
 15            continue
 14         continue
 12      continue
         nm=nm+nobs(i)
 18   continue
c
c     solve least squares equations
c
      call dpofa(betacov,npvl,npvl,info)
      call dpodi(betacov,npvl,npvl,det,01)
      do 7 k=2,npvl
         do 8 j=1,k-1
            betacov(k,j)=betacov(j,k)
 8       continue
 7    continue
      do 11 j=1,npvl
         beta(j)=0.
         do 6 k=1,npvl
            beta(j)=beta(j)+betacov(j,k)*tb(k)
 6       continue
 11   continue
      return
      end
c
c
      subroutine plmn(dev,theta,i,nm,nind,rxl,x,y,ccov,dose,nobs,
     +     link,nxrl,torder,model,tvcov,tvc,nccov,npv,
     +     npvar,np,ey,nld,beta,npvl,inter,tb,nbs,mu)
c
c compute the mean function for nonlinear models and return residuals
c
      implicit none
      integer nobs,nm,i,j,k,link,model,nxrl,nld,torder,npv,npvar,
     +     nind,np,npvl,nccov,k1,k2,tvc,nbs
      integer inter(nccov)
      double precision theta(np),thetap(4),ey(nld),tvcov(*),
     +     x(*),y(*),rxl(nind),ccov(nind,nccov),beta(npvl),
     +     dose,dev(*),beta1,delta,tmp,d,tb(npvl),mu(*)
c
c     linear/polynomial model
c
      if(model.eq.1)then
         do 49 j=1,nobs
            ey(j)=beta(1)
            tmp=1
            k1=1
            k2=1
            do 4 k=2,npvl
               if(k.le.torder)then
                  tmp=tmp*x(nm+j)
               else if(k.gt.npvl-tvc)then
                  tmp=tvcov(nm+j+nbs*(k-npvl+tvc-1))
               else
                  if(k2.gt.inter(k1))then
                     k1=k1+1
                     k2=1
                  endif
                  if(k2.eq.1)then
                     tmp=ccov(i,k1)
                     k2=k2+1
                  else
                     tmp=tmp*x(nm+j)
                     k2=k2+1
                  endif
               endif
               ey(j)=ey(j)+tmp*beta(k)
 4          continue
 49      continue
c
c     model is an R function for the mean
c
      else if(model.eq.2)then
         do 58 j=1,nobs
            ey(j)=mu(nm+j)
 58      continue
c
c     generalized logistic model
c
      else if(model.eq.3)then
         if(tvc.eq.1)then
            j=rxl(i)
            beta1=theta(j+3)
            thetap(1)=theta(1)
            thetap(2)=theta(1)
            thetap(3)=theta(2)
            thetap(4)=theta(3)
            call genlog(x(nm+1),thetap,ey(1))
            do 39 j=2,nobs
               thetap(1)=dlog(ey(j-1))
               thetap(2)=dlog(2./(1+dexp(tvcov(nm+j-1)*beta1)))+theta(1)
               delta=x(nm+j)-x(nm+j-1)
               call genlog(delta,thetap,ey(j))
 39         continue
         else
            do 36 j=1,4
               thetap(j)=theta(j)
 36         continue
            if(nxrl.gt.1.and.rxl(i).gt.1)then
               do 37 j=1,4
                  k=j+(rxl(i)-1)*4
                  thetap(j)=thetap(j)+theta(k)
 37            continue
            endif
            do 2 j=1,nobs
               delta=x(nm+j)-x(nm+1)
               call genlog(delta,thetap,ey(j))
 2          continue
         endif
c
c     one compartment pk model
c
      else if(model.eq.4)then
         do 6 j=1,3
            thetap(j)=theta(j)
 6       continue
         if(nxrl.gt.1.and.rxl(i).gt.1)then
            j=2+rxl(i)
            thetap(3)=thetap(3)+theta(j)
         endif
         if(tvc.ne.1)d=dose
         if(dabs(thetap(1)-thetap(2)).gt.0.0001)then
            do 59 j=1,nobs
               if(tvc.eq.1)d=tvcov(nm+j)
               ey(j)=dexp(thetap(1)-thetap(3))*d/(dexp(thetap(1))
     +              -dexp(thetap(2)))*(dexp(-dexp(thetap(2))*x(nm+j))-
     +              dexp(-dexp(thetap(1))*x(nm+j)))
 59         continue
         else
            do 60 j=1,nobs
               if(tvc.eq.1)d=tvcov(nm+j)
               ey(j)=dexp(thetap(1)-thetap(3))*d*x(nm+j)*
     +              dexp(-dexp(thetap(1))*x(nm+j))
 60         continue
         endif
      endif
      if(link.eq.2)then
         do 69 j=1,nobs
            dev(nm+j)=y(nm+j)-dlog(ey(j))
 69      continue
         else if(link.eq.3)then
         do 68 j=1,nobs
            dev(nm+j)=y(nm+j)-sqrt(ey(j))
 68      continue
         else if(link.eq.4)then
         do 67 j=1,nobs
            dev(nm+j)=y(nm+j)-ey(j)**2
 67      continue
         else if(link.eq.5)then
         do 66 j=1,nobs
            dev(nm+j)=y(nm+j)-dexp(ey(j))
 66      continue
         else
         do 65 j=1,nobs
            dev(nm+j)=y(nm+j)-ey(j)
 65      continue
         endif
      return
      end
c
c
      subroutine plmv(dev,theta,i,nm,nind,rxl,x,y,ccov,dose,nobs,
     +     link,nxrl,torder,model,tvcov,tvc,nccov,npv,npvar,np,
     +     ey,nld,beta,npvl,inter,sigsq,cvar,var,tb,nbs,mu)
c
c calculate variance function, when required
c
      implicit none
      integer nobs,nm,i,j,k,model,nld,npv,nind,np,nccov,link,nxrl,
     +     torder,npvar,npvl,cvar,tvc,nbs
      integer inter(nccov)
      double precision theta(np),tvcov(*),ccov(nind,nccov),
     +     sigsq(nld),tmp,d,dose,dev(*),rxl(nind),x(*),y(*),
     +     ey(nld),beta(npvl),var(*),tb(npvl),mu(*)
c
c     an R function for the variance
c 
      if(cvar.eq.1)then
         do 57 j=1,nobs
            sigsq(j)=var(nm+j)
 57      continue
c
c     variance as a function of the mean
c
      else if(cvar.gt.1)then
         call plmn(dev,theta,i,nm,nind,rxl,x,y,ccov,dose,nobs,
     +        link,nxrl,torder,model,tvcov,tvc,nccov,npv,npvar,
     +        np,ey,nld,beta,npvl,inter,tb,nbs,mu)
         do 1 j=1,nobs
            tmp=y(nm+j)-dev(nm+j)
            if(cvar.eq.3)tmp=tmp*tmp
            sigsq(j)=tmp*dexp(theta(npv+npvar))
            if(npvar.eq.2)sigsq(j)=sigsq(j)+dexp(theta(npv+1))
 1       continue
      else
c
c     nonlinear variance function for pk model
c
         if(model.eq.4.and.npvar.eq.4)then
            if(tvc.ne.1)d=dose
            if(dabs(theta(npv+1)-theta(npv+2)).gt.0.001)then
               do 69 j=1,nobs
                  if(tvc.eq.1)d=tvcov(nm+j)
                  tmp=(dexp(-dexp(theta(npv+2))*x(nm+j))
     +                 -dexp(-dexp(theta(npv+1))*x(nm+j)))/
     +                 (dexp(theta(npv+1))-dexp(theta(npv+2)))
                  if(dabs(tmp).le.1.e-34)tmp=1.e-34
                  sigsq(j)=theta(npv+1)-theta(npv+3)+theta(npv+4)*
     +                 dlog(d*tmp)
 69            continue
            else
               do 68 j=1,nobs
                  if(tvc.eq.1)d=tvcov(nm+j)
                  sigsq(j)=theta(npv+1)-theta(npv+3)+theta(npv+4)*
     +                 (dlog(d*x(nm+j))-dexp(theta(npv+1))*x(nm+j))
 68            continue
            endif
         else
c
c     variance as a polynomial function of time
c
            do 2 j=1,nobs
               tmp=1
               sigsq(j)=theta(npv+1)
               do 7 k=2,npvar
                  tmp=tmp*x(nm+j)
                  sigsq(j)=sigsq(j)+theta(npv+k)*tmp
 7             continue
 2          continue
         endif
      endif
      if(cvar.le.1)then
         do 3 j=1,nobs
            if(sigsq(j).gt.24.)sigsq(j)=24.
            sigsq(j)=dexp(sigsq(j))
 3       continue
      endif
      return
      end
c
c
      subroutine cmpvar(v,ldet,sigsq,tausq,rho,nind,i,nm,x,nobs,
     +     nest,lnest,nld,npre,npar,ar,twins)
c
c compute the variance matrix for the unit
c
      implicit none
      integer nobs,i,j,k,nm,nest(*),lnest,nn1,nn2,k1,j1,
     +     nind,nld,info,npre,npar,ar,twins
      double precision x(*),v(nld,nld),det(2),sigsq(nld),tausq(2),
     +     rho,ldet,tmp
c
      k1=0
      nn1=nest(nm+1)
      do 9 k=1,nobs
         if(lnest.gt.0)then
            if(nest(nm+k).ne.nn1)then
               k1=k1+1
               nn1=nest(nm+k)
            endif
         endif
         j1=0
         nn2=nest(nm+1)
         do 19 j=1,k
            if(lnest.gt.0)then
               if(nest(nm+j).ne.nn2)then
                  j1=j1+1
                  nn2=nest(nm+j)
               endif
            endif
            if(twins.eq.0)then
               v(k,j)=tausq(1)
            else
               if(j.ne.k)then
                  v(k,j)=tausq(1)
               else
                  v(k,j)=0.0
               endif
            endif
            if(k1.eq.j1)then
               v(k,j)=v(k,j)+tausq(2)
               if(k.eq.j)then
                  if(ar.ne.5)then
                     v(k,k)=v(k,k)+sigsq(k)
                  else
                     v(k,k)=v(k,k)+sigsq(k)*(rho*x(nm+j)+
     +                    exp(-rho*x(nm+j))-1)/rho**3
                  endif
               else if(rho.gt.0.0)then
                  if(sigsq(k).eq.sigsq(j))then
                     tmp=sigsq(k)
                  else
                     tmp=sqrt(sigsq(k)*sigsq(j))
                  endif
                  if(ar.eq.1)then
                     tmp=tmp*rho**dabs(x(nm+k)-x(nm+j))
                  else if(ar.eq.2)then
                     tmp=tmp*rho**((x(nm+k)-x(nm+j))**2)
                  else if(ar.eq.3)then
                     tmp=tmp/(1+rho*(x(nm+k)-x(nm+j))**2)
                  else if(ar.eq.4)then
                     if(dabs(x(nm+k)-x(nm+j)).le.1/rho)then
                        tmp=tmp*((dabs(x(nm+k)-x(nm+j))*rho)**3-3*rho*
     +                    dabs(x(nm+k)-x(nm+j))+2)/2
                     else
                        tmp=0.
                     endif
                  else
                     tmp=tmp*(2*rho*x(nm+j)+dexp(-rho*x(nm+j))+
     +                    dexp(-rho*x(nm+k))-1-exp(-rho*
     +                    dabs(x(nm+k)-x(nm+j))))/(2*rho**3)
                  endif
                  v(k,j)=v(k,j)+tmp
               endif
            endif
 19      continue
 9    continue
      do 1 k=2,nobs
         do 3 j=1,k-1
            v(j,k)=v(k,j)
 3       continue
 1    continue
c
c     factor v and compute the inverse and determinant
c
      if(npre+npar.eq.0)then
         ldet=0.0
         do 2 j=1,nobs
            ldet=ldet+dlog(v(j,j))
            v(j,j)=1/v(j,j)
 2       continue
      else
         call dpofa(v,nld,nobs,info)
         call dpodi(v,nld,nobs,det,11)
         ldet=dlog(det(1))+det(2)*dlog(10.0d0)
         do 8 k=2,nobs
            do 18 j=1,k-1
               v(k,j)=v(j,k)
 18         continue
 8       continue
      endif
      return
      end
c
c
      subroutine genlog(x,thetap,w)
c
c compute the log generalized logistic function.
c                      Daniel F. Heitjan, 26 June 1990
c
      implicit none
      double precision thetap(4),x,w,y0,yinf
      y0=dexp(thetap(1))
      yinf=dexp(thetap(2))
      if(dabs(thetap(4)).ge.1.0d-8)then
         w=yinf*(1+((yinf/y0)**thetap(4)-1)*dexp(-yinf**thetap(4)
     +        *dexp(thetap(3))*x))**(-1/thetap(4))
      else
         w=yinf*dexp(dlog(y0/yinf)*dexp(-dexp(thetap(3))*x))
      endif
      if(w.lt.1.0d-8)w=1.0d-8
      return
      end
