module my_subs
IMPLICIT NONE
contains
RECURSIVE FUNCTION sigfun(k,upm,sigma,dimen,ldim,maxd,pbarold) result(sigcol)
	IMPLICIT NONE
	INTEGER, INTENT(in)::k,upm,pbarold,maxd
	INTEGER::i,j1,j2,nvars,pbar
	INTEGER::ldim
	INTEGER::dimen(ldim)
        DOUBLE PRECISION::sigma(ldim,maxd**2)
        DOUBLE PRECISION::tmp(1,pbarold)
	DOUBLE PRECISION::sigcol(pbarold)
	nvars=pbarold
	pbar=nvars/dimen(upm)
	j2=ceiling(dble(k)/pbar)
	j1=mod(k,pbar)
	IF (j1==0) j1=pbar	
	IF (upm>2) THEN
		tmp=reshape(spread(sigma(upm,maxd*(j2-1)+1:maxd*(j2-1)+dimen(upm)),dim=1,ncopies=pbar) &
			*spread(sigfun(j1,upm-1,sigma,dimen,ldim,maxd,pbar),dim=2,ncopies=dimen(upm)),(/1,nvars/))
		sigcol=tmp(1,:)
	ELSE
		tmp=reshape(spread(sigma(upm,maxd*(j2-1)+1:maxd*(j2-1)+dimen(upm)),dim=1,ncopies=pbar) &
			*spread(sigma(1,maxd*(j1-1)+1:maxd*(j1-1)+dimen(1)),dim=2,ncopies=dimen(upm)),(/1,nvars/))
		!see if pbar needs modification
		sigcol=tmp(1,:)
	ENDIF
END FUNCTION sigfun
end module my_subs
! --------------------------------------------------
SUBROUTINE clustertensor(obj,nk,nvars,ldim,dimen,maxd,sigma,delta,pf,dfmax,pmax,nlam,flmin,ulam,&
        eps,maxit,sml,verbose,nalam,theta,m,ntheta,alam,npass,jerr)
! --------------------------------------------------
    use my_subs
    IMPLICIT NONE
    ! - - - arg types - - -
    DOUBLE PRECISION, PARAMETER :: big=9.9E30
    DOUBLE PRECISION, PARAMETER :: mfl = 1.0E-6
    INTEGER, PARAMETER :: mnlam = 6
    INTEGER::mnl
    INTEGER::nk
    INTEGER::nvars
    INTEGER::p
    INTEGER::q
    INTEGER::dfmax
    INTEGER::pmax
    INTEGER::nlam
    INTEGER::nalam
    INTEGER::npass
    INTEGER::jerr
    INTEGER::verbose
    INTEGER::maxit
    INTEGER::ldim
    INTEGER::m(pmax)
    INTEGER::dimen(ldim)
    INTEGER::ntheta(nlam)
    INTEGER::maxd
    DOUBLE PRECISION::flmin
    DOUBLE PRECISION::eps
    DOUBLE PRECISION::sml
    DOUBLE PRECISION::sigma(ldim,maxd**2)
    DOUBLE PRECISION::delta(nk,nvars)
    DOUBLE PRECISION::pf(nvars)
    DOUBLE PRECISION::ulam(nlam)
    DOUBLE PRECISION::theta(nk,pmax,nlam)
    DOUBLE PRECISION::alam(nlam)
    DOUBLE PRECISION::obj(nlam)
    ! - - - local declarations - - -
    INTEGER::mm(nvars)
    INTEGER::k
    INTEGER::j
    INTEGER::jj
    INTEGER::l
    INTEGER::vrg
    INTEGER::ni
    INTEGER::me
    INTEGER::i1
    INTEGER::i2
    INTEGER::ii
    INTEGER::j1
    INTEGER::j2
    INTEGER::k1
    INTEGER::k2
    INTEGER::kk
    INTEGER::pbar
    INTEGER::prod
    INTEGER::i
    INTEGER::row
    INTEGER::col
    INTEGER::dindex
    DOUBLE PRECISION::dif
    DOUBLE PRECISION::v
    DOUBLE PRECISION::al
    DOUBLE PRECISION::alf
    DOUBLE PRECISION::unorm
    DOUBLE PRECISION::ksigma
    DOUBLE PRECISION::t(nk,nvars)
    DOUBLE PRECISION::tmp(1,nvars)
    DOUBLE PRECISION::ttmp(nvars)
    DOUBLE PRECISION::thetanew(nk,nvars)
    DOUBLE PRECISION::thetaold(nk,nvars)
    DOUBLE PRECISION::r(nk,nvars)
    DOUBLE PRECISION::ab(nk,nvars)
    DOUBLE PRECISION::d(nk)
    DOUBLE PRECISION::theta_sum(nk)
    DOUBLE PRECISION::thetatmp(nk)
    DOUBLE PRECISION::u(nk)
    DOUBLE PRECISION::loss_diff
    DOUBLE PRECISION::penalty_diff
    DOUBLE PRECISION::dev
    DOUBLE PRECISION::dev_tmp
    DOUBLE PRECISION::tmp1_new
    DOUBLE PRECISION::tmp2_new
    DOUBLE PRECISION::dev_new
    DOUBLE PRECISION::dev1_new
    DOUBLE PRECISION::dev2_new
    DOUBLE PRECISION::dev3_new
    DOUBLE PRECISION::tmp1_old
    DOUBLE PRECISION::tmp2_old
    DOUBLE PRECISION::dev_old
    DOUBLE PRECISION::dev1_old
    DOUBLE PRECISION::dev2_old
    DOUBLE PRECISION::dev3_old
!    DOUBLE PRECISION::sigfun(nvars/dimen(1))
! - - - begin - - -
    IF(maxval(pf)  <= 0.0D0) THEN
        jerr=10000
        RETURN
    ENDIF
    pf=max(0.0D0,pf)
! - - - some initial setup - - -
    mnl = Min (mnlam, nlam)
    r = delta
    thetanew=0.0D0
    thetaold=0.0D0
    dev=0.0D0
    m=0
    mm=0
    npass=0
    ni=npass
! --------- lambda loop ----------------------------
    IF(flmin < 1.0D0) THEN
        flmin = Max (mfl, flmin)
        alf=flmin**(1.0D0/(nlam-1.0D0))
    ENDIF

    DO l=1,nlam
        IF(flmin>=1.0D0) THEN
            al=ulam(l)
        ELSE
            IF(l > 2) THEN
                al=al*alf
            ELSE IF(l==1) THEN
                al=big
            ELSE IF(l==2) THEN
                al=0.0D0
                DO j = 1,nvars
                    IF(pf(j)>0.0D0) THEN
                            u = delta(:,j)
                            v = sqrt(dot_product(u,u))
			    al=max(al,v/pf(j))
                    ENDIF
                END DO
                al=al*alf
            ENDIF
        ENDIF
! --------- outer loop ----------------------------
        DO
	    IF(ni>0) thetaold(:,m(1:ni))=thetanew(:,m(1:ni))
! --middle loop-------------------------------------
            DO
		npass=npass+1
                dif=0.0D0
                dev_tmp=dev
		DO k=1,nvars
                    thetatmp=thetanew(:,k)

		    !Calculate the (k,k) element of ksigma
		    prod=nvars
		    row=k
		    col=k
		    ksigma=1
		    DO dindex=ldim,1,-1
			pbar=dble(prod)/dimen(dindex)
			prod=pbar
			k1=ceiling(dble(col)/pbar)
			k2=ceiling(dble(row)/pbar)
			i1=mod(row,pbar)
			i2=mod(col,pbar)
			IF (i1==0) i1=pbar
			IF (i2==0) i2=pbar
			ksigma=ksigma*sigma(dindex,(k1-1)*maxd+k2)
			row=i1
			col=i2
		    ENDDO
		    u=r(:,k)/ksigma+thetatmp
		    unorm = sqrt(dot_product(u,u))         
 		    v = unorm-al*pf(k)/ksigma
		    IF(v > 0.0D0) THEN
                        thetanew(:,k) = v*u/unorm
                    ELSE
                        thetanew(:,k)=0.0D0
                    ENDIF
                    d=thetanew(:,k)-thetatmp
                    theta_sum=thetanew(:,k)+thetatmp
		    IF(any(d/=0.0D0)) THEN
			dif=max(dif,maxval(abs(d)))
                        loss_diff = sum(d*(0.5*theta_sum-r(:,k)-ksigma*thetatmp))
			penalty_diff = al*pf(k)*(sqrt(dot_product(thetanew(:,k),thetanew(:,k))) &
                        - sqrt(dot_product(thetatmp,thetatmp)))
		        dev = dev + loss_diff + penalty_diff

 			pbar=nvars/dimen(ldim)
			j2=ceiling(dble(k)/pbar)
			j1=mod(k,pbar)
                        IF (j1==0) j1=pbar
			
			
			IF (ldim>2) THEN	
				tmp=reshape(spread(sigma(ldim,maxd*(j2-1)+1:maxd*(j2-1)+dimen(ldim)),dim=1,ncopies=pbar) & 
					*spread(sigfun(j1,ldim-1,sigma,dimen,ldim,maxd,pbar),dim=2,ncopies=dimen(ldim)),(/1,nvars/))
				
			ELSE
				tmp=reshape(spread(sigma(2,maxd*(j2-1)+1:maxd*(j2-1)+dimen(2)),dim=1,ncopies=pbar)&
					*spread(sigma(1,maxd*(j1-1)+1:maxd*(j1-1)+dimen(1)),dim=2,ncopies=dimen(2)),(/1,nvars/))
			ENDIF
			ttmp=tmp(1,:)
			
			t=spread(ttmp,dim=1,ncopies=nk)*spread(d,dim=2,ncopies=nvars)
			r=r-t
			IF(mm(k)==0) THEN
                            ni=ni+1
                            IF(ni>pmax) EXIT
                            mm(k)=ni
                            m(ni)=k
                        ENDIF
                    ENDIF
                ENDDO
		IF(abs((dev-dev_tmp)/dev)<sml)	EXIT 
		IF(ni>pmax) 	EXIT
                IF(dif<eps) 	EXIT
		IF(npass > maxit) THEN
                    jerr=-l
                    RETURN
                ENDIF
! --inner loop----------------------
                DO
		    npass=npass+1
		    dif=0.0D0
                    dev_tmp = dev
		    DO j=1,ni
                        k=m(j)
                        thetatmp=thetanew(:,k)
			
			!Calculate the (k,k) element of ksigma
		    	prod=nvars
		       	row=k
		    	col=k
		    	ksigma=1
		    	DO dindex=ldim,1,-1
				pbar=prod/dimen(dindex)
				prod=pbar
				k1=ceiling(dble(col)/pbar)
				k2=ceiling(dble(row)/pbar)
				i1=mod(row,pbar)
				i2=mod(col,pbar)
				IF (i1==0) i1=pbar
				IF (i2==0) i2=pbar
				ksigma=ksigma*sigma(dindex,(k1-1)*maxd+k2)
				row=i1
				col=i2
	                  ENDDO

                        u=r(:,k)/ksigma+thetatmp
	                unorm = sqrt(dot_product(u,u))    	       
 			v = unorm-al*pf(k)/ksigma		
			IF(v > 0.0D0) THEN
                            thetanew(:,k) = v*u/unorm
                        ELSE
                            thetanew(:,k)=0.0D0
                        ENDIF
                        d=thetanew(:,k)-thetatmp
                        theta_sum=thetanew(:,k)+thetatmp
                      	IF(any(d/=0.0D0)) THEN	
			  dif=max(dif,maxval(abs(d)))
			  loss_diff = sum(d*(0.5*theta_sum-r(:,k)-ksigma*thetatmp))
			  penalty_diff = al*pf(k)*(sqrt(dot_product(thetanew(:,k),thetanew(:,k))) &
                          - sqrt(dot_product(thetatmp,thetatmp)))
			  dev = dev + loss_diff + penalty_diff
			  pbar=nvars/dimen(ldim)
			  j2=ceiling(dble(k)/pbar)
			  j1=mod(k,pbar)
                          IF (j1==0) j1=pbar

			
			  IF (ldim>2) THEN	
				tmp=reshape(spread(sigma(ldim,maxd*(j2-1)+1:maxd*(j2-1)+dimen(ldim)),dim=1,ncopies=pbar) & 
					*spread(sigfun(j1,ldim-1,sigma,dimen,ldim,maxd,pbar),dim=2,ncopies=dimen(ldim)),(/1,nvars/))
				
			  ELSE
				tmp=reshape(spread(sigma(2,maxd*(j2-1)+1:maxd*(j2-1)+dimen(2)),dim=1,ncopies=pbar)&
					*spread(sigma(1,maxd*(j1-1)+1:maxd*(j1-1)+dimen(1)),dim=2,ncopies=dimen(2)),(/1,nvars/))
			  ENDIF
			  ttmp=tmp(1,:)
			

			  t=spread(ttmp,dim=1,ncopies=nk)*spread(d,dim=2,ncopies=nvars)
			  r=r-t	
			ENDIF 
		    ENDDO
		    IF(abs((dev-dev_tmp)/dev)<sml) 	EXIT
		    IF(dif<eps) 	EXIT
		    IF(npass > maxit) THEN
                       jerr=-l
                       RETURN
                    ENDIF
                ENDDO
            ENDDO
            IF(ni>pmax) EXIT
!--- this is the final check ------------------------
            vrg=1
	    DO j=1,ni
                IF(maxval(abs(thetanew(:,m(j))-thetaold(:,m(j))))>=eps) THEN
                    vrg=0
                    EXIT
                ENDIF
            ENDDO
	    IF(vrg==1) EXIT
            ! test deviance loop
            dev1_new = 0.0
            dev2_new = 0.0
            dev1_old = 0.0
            dev2_old = 0.0
            DO jj = 1,nk
		tmp1_new=0.0
		DO ii=1,ni
			DO kk=1,ni
				 !Calculate the (m(kk),m(ii)) element of ksigma
		    		prod=1
		    		DO k=1,ldim
					prod=prod*dimen(k)
		    		ENDDO
	     	    		row=m(kk)
		    		col=m(ii)
		    		ksigma=1
		    		DO dindex=ldim,1,-1
					pbar=prod/dimen(dindex)
					prod=pbar
					k1=ceiling(dble(col)/pbar)
					k2=ceiling(dble(row)/pbar)
					i1=mod(row,pbar)
					i2=mod(col,pbar)
					IF (i1==0) i1=pbar
					IF (i2==0) i2=pbar
					ksigma=ksigma*sigma(dindex,(k1-1)*maxd+k2)
					row=i1
					col=i2
		    		ENDDO

				tmp1_new=tmp1_new+thetanew(jj,m(kk))*ksigma*thetanew(jj,m(ii))
			ENDDO
		ENDDO

                tmp2_new = dot_product(thetanew(jj,m(1:ni)),delta(jj,m(1:ni)))
                dev1_new = dev1_new + tmp1_new
                dev2_new = dev2_new + tmp2_new
                tmp1_old=0.0
		DO ii=1,ni
			DO kk=1,ni
		        	 !Calculate the (m(kk),m(ii)) element of ksigma
		    		prod=1
		    		DO k=1,ldim
					prod=prod*dimen(k)
		    		ENDDO
	     	    		row=m(kk)
		    		col=m(ii)
		    		ksigma=1
		    		DO dindex=ldim,1,-1
					pbar=prod/dimen(dindex)
					prod=pbar
					k1=ceiling(dble(col)/pbar)
					k2=ceiling(dble(row)/pbar)
					i1=mod(row,pbar)
					i2=mod(col,pbar)
					IF (i1==0) i1=pbar
					IF (i2==0) i2=pbar
					ksigma=ksigma*sigma(dindex,(k1-1)*maxd+k2)
					row=i1
					col=i2
		    		ENDDO
				tmp1_old=tmp1_old+thetaold(jj,m(kk))*ksigma*thetaold(jj,m(ii))
			ENDDO
		ENDDO
                tmp2_old = dot_product(thetaold(jj,m(1:ni)),delta(jj,m(1:ni)))
                dev1_old = dev1_old + tmp1_old
                dev2_old = dev2_old + tmp2_old
            ENDDO
	    dev3_new = al * sum(pf(m(1:ni)) * sqrt(sum(thetanew(:,m(1:ni)) * thetanew(:,m(1:ni)), DIM = 1)))
            dev3_old = al * sum(pf(m(1:ni)) * sqrt(sum(thetaold(:,m(1:ni)) * thetaold(:,m(1:ni)), DIM = 1)))
            dev_new = 0.5 * dev1_new - dev2_new + dev3_new
            dev_old = 0.5 * dev1_old - dev2_old + dev3_old
            IF(verbose==1) THEN
                CALL intpr('Current Lambda',-1,l,1)
                CALL dblepr('Obj-func Jump',-1,abs((dev_new-dev_old)/dev_new),1)
            ENDIF
            IF(abs((dev_new-dev_old)/dev_new)<sml) EXIT
            ! test deviance loop end
        ENDDO
!--- final update variable save results------------
        IF(ni>pmax) THEN
	   jerr=-10000-l
           EXIT
        ENDIF
        IF(ni>0) theta(:,1:ni,l)=thetanew(:,m(1:ni))
	me = count(maxval(abs(theta(:,1:ni,l)),dim=1)/=0.0D0)
        IF(me>dfmax) THEN
			jerr=-20000-l
			EXIT
		ENDIF
        obj(l) = dev_new
        ntheta(l)=ni
        alam(l)=al
        nalam=l
        IF (l < mnl) CYCLE
        IF (flmin >= 1.0D0) CYCLE
    ENDDO
    RETURN
END SUBROUTINE clustertensor


			






