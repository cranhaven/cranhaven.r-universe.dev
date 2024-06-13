	subroutine pic2wt(x,zz,iz,idn,pidn,w,nobs,nobsmax,nvarmax)
c	subroutine to compute ratios of variances and convert pic to bitas (weights)
c	using KNN
      Implicit double precision (A-H,O-Z) 
	real(8) pidn(nvarmax),z(nobsmax,nvarmax),zz(nobsmax,nvarmax),
	1       ak(nobsmax),sdz(nvarmax),cor(nvarmax),w(nvarmax)
	real(8) x(nobsmax),y(nobsmax),sum,sdx,av
	integer iz,knear,iseed,nobsmax,nvarmax,idn(nvarmax)

  	knear=int(3.0*sqrt(float(nobs))+0.5)
c	find out weight probability for k neighbours
      sum=0.0
	do i=1,knear
	 sum=sum+1.0/float(i)
      enddo
      do i=1,knear
       ak(i)=1.0/(float(i)*sum)
      enddo
c	form a vector of significant predictors
	do j=1,iz
	do i=1,nobs
	z(i,j)=zz(i,idn(j))
	enddo
	enddo


	call basic(x,av,sdx,nobs)
	do j=1,iz
	do i=1,nobs
	y(i)=z(i,j)
	enddo
	call basic(y,av,sdz(j),nobs)
	enddo

	do j=1,iz
	sdd=sdz(j)
	ih=j
	call partial_cor(x,z,ih,iz,ak,knear,nobs,sd,cc,
	1                 iseed,sdx,sdd,nobsmax,nvarmax)
	cor(j)=cc
	w(j)=pidn(j)*sd
 	enddo
	return
	end
c**********************************************
	subroutine partial_cor(x,z,jt,iz,ak,knear,n,sd,cor,
	1                       iseed,sdxz,sdz,nobsmax,nvarmax)
      Implicit double precision (A-H,O-Z) 
	real(8) x(nobsmax),z(nobsmax,nvarmax),zz(nobsmax,nvarmax)
	real(8) wt(nvarmax),res1(nobsmax),res2(nobsmax),
	1       y(nobsmax),ak(nobsmax),gen1(nobsmax),
	2       gen2(nobsmax)
	real(8) sd1,sd2,sdz
c	for x and predictor(-p1) series
	do i=1,n
	ii=0
c	zz(i,ii)=1.0
	do j=1,iz
	if(j.ne.jt)then
	ii=ii+1
	zz(i,ii)=z(i,j)
	endif
	enddo
	enddo
	nv=ii
	do j=1,nv
	wt(j)=1.0
	enddo

	call likeli (x,zz,zz,wt,knear,ak,nv,n,n,gen1,iseed,
	1             nobsmax,nvarmax)
c	calculate residuals
	do i=1,n
	res1(i)=x(i)-gen1(i)
	enddo
	call basic(res1,av,sd1,n)

c	for p1 and other predictor series
	do i=1,n
	y(i)=z(i,jt)
	ii=0
c	zz(i,ii)=1.0
	do j=1,iz
	if(j.ne.jt)then
	ii=ii+1
	zz(i,ii)=z(i,j)
	endif
	enddo
	enddo
	nv=ii
	do j=1,nv
	wt(j)=1.0
	enddo

	call likeli (y,zz,zz,wt,knear,ak,nv,n,n,gen2,iseed,
	1             nobsmax,nvarmax)
c	calculate residuals
	do i=1,n
	res2(i)=y(i)-gen2(i)
	enddo
	call basic(res2,av,sd2,n)

c	calculate average sd of res1 and res2 series
	sd1=sd1/sdxz
	sd2=sd2/sdz
	sd=(sd1+sd2)/2.0

c	calculate corrl of res1 and res2 series
	do i=1,n
	res2(i)=y(i)-gen2(i)
	enddo
	do i=1,n
	res1(i)=x(i)-gen1(i)
	enddo
	n1=n
	n2=n
c	calculate correl and sd of res1 and res2 series
	CALL CORR(res1,res2,N1,N2,COR,NOBSMAX)
	
	return
	end
C	***********************************************************
	SUBROUTINE CORR(X,Y,N1,N2,ACOR,NOBSMAX)
      Implicit double precision (A-H,O-Z) 
	real(8) X(nobsmax),ACOR,Y(nobsmax)

	CALL BASIC(X,AV1,SD1,N1)
	CALL BASIC(Y,AV2,SD2,N2)

	AN=N1
	SUM=0.0
	DO 20 J=1,N1
20	SUM=SUM+(X(J)-AV1)*(Y(J)-AV2)
	ACOR=SUM/(AN*SD1*SD2)
	RETURN
	RETURN
	END
c***********************************************
      SUBROUTINE likeli (x,zz,zp,w,knear,ak,nvar,ng,nobs,ze,
	1           iseed,nobsmax,nvarmax)

      Implicit double precision (A-H,O-Z) 
C     THIS SUBROUTINE calculates the response series

      real(8) w(nvarmax),y(nvarmax),ze(nobsmax),xx(nobsmax)
	real(8) avz(nvarmax),sdz(nvarmax),ak(nobsmax),x(nobsmax),
	1       zz(nobsmax,nvarmax),zp(nobsmax,nvarmax),sdp(nvarmax),
	2       avp(nvarmax)
	integer i,j,it
 	do i=1,ng
	ze(i)=0.0
	enddo
c	calculate mean and sd of zz series
	do j=1,nvar
	do i=1,nobs
	xx(i)=zz(i,j)
	enddo
	call basic(xx,avz(j),sdz(j),nobs)
	enddo
c	calculate mean and sd of zp series
	do j=1,nvar
	do i=1,ng
	xx(i)=zp(i,j)
	enddo
	call basic(xx,avp(j),sdp(j),ng)
	enddo
c	form y vector for current time step values of zp
	do i=1,ng
	do j=1,nvar
	y(j)=(zp(i,j)-avp(j))/sdp(j)
 	enddo
c	call subroutine of knn method to estimate ze based on k nearest neighbours
	it=i
	call boot(x,zz,avz,sdz,ak,nobs,nvar,knear,it,w,y,gen,iseed,
	1          nvarmax,nobsmax)
	ze(i)=gen
	enddo
      RETURN
      END
c**************************** BOOT ******************************************
c       subroutine to compute k nearest bootstraps
      subroutine boot(x,zz,av,sd,ak,nobs,nvar,knear,ki,w,di,gen,
	1           iseed,nvarmax,nobsmax)
      Implicit double precision (A-H,O-Z) 
        real(8) di(nvarmax),dn(nvarmax),w(nvarmax),av(nvarmax),
	1         scale(nobsmax,2),sd(nvarmax),ak(nobsmax),
	1         zz(nobsmax,nvarmax),x(nobsmax)

      do j=1,nobs
         scale(j,1)=1000.0
	enddo

c	scan the complete given series, excluding the time step for which
c 	we are predicting
	nn=0
	do 10 i=1,nobs
c	check for day for which we are predicting
	if(ki.eq.i)goto 10

c	initialize and create dn vector
	do j=1,nvar
	dn(j)=(zz(i,j)-av(j))/sd(j)
	enddo

	rmse=0.0
	do j=1,nvar
	diff=(di(j)-dn(j))
	rmse=rmse+w(j)*(diff)**2
	enddo

 	rmse=sqrt(rmse)

	nn=nn+1
	scale(nn,1)=rmse
	scale(nn,2)=x(i)
 10	continue

c	findout k nearest neighbours
	call nearest(scale,nn,knear,nobsmax)
c	findout value of z for the ith time step
	call average (scale,knear,ak,gen,nobsmax)
	return
        end
c ************************************************************
	subroutine average (scale,k,ak,sum,nobsmax)
c	findout expected value of generated series
      Implicit double precision (A-H,O-Z) 
	real(8) scale(nobsmax,2),ak(nobsmax)
	integer k,j
	sum=0.0
      do j=1,k
	 sum=sum+scale(j,2)*ak(j)
	enddo
	return
	end
c************************************************************
	subroutine nearest(scale,n,kn,nobsmax)
      Implicit double precision (A-H,O-Z) 
	real(8) scale(nobsmax,2)
c	rank the scale series(just to save time can do it for kn terms only)
	do i=1,kn
	do j=i+1,n
	if(scale(j,1).lt.scale(i,1))then
	a=scale(i,1)
	scale(i,1)=scale(j,1)
	scale(j,1)=a
	a=scale(i,2)
	scale(i,2)=scale(j,2)
	scale(j,2)=a
	endif
	enddo
	enddo

	return
	end
c************************************************

        SUBROUTINE BASIC (XX,AVER,SD,N)
	real(8) xx(*),aver,sd,sx,sssx,rn,a
	integer n,i

C       THIS SUBROUTINE CALCULATES THE MEAN AND STANDARD
C       DEVIATION OF AN INPUT SEARIES OF LENGTH N
        SX=0.0
        SSSX=0.0
        RN=FLOAT(N)
C        IF(RN.EQ.0.0)write(*,41)
       DO 21 I=1,N
        A=XX(I)
        SX=SX+A
        SSSX=SSSX+A*A
 21    CONTINUE
        AVER=SX/RN
        SD=SSSX-SX*AVER
C        IF((RN-1.0).EQ.0.0)write(*,41)
        SD=sqrt(SD/(RN-1.0))
C 41     FORMAT(10X,'DATA LENGTH MUST BE GREATER THAN  2')
        RETURN
        END
c************************************************
