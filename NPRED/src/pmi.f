c	subroutine to compute partial mutual information
c  Input required								       row x col 
c	x -time series of response with n observations (n x 1)
c	y -time series of predictor under consideraion (n x 1)
c	z -matrix including time series of 'iz' pre-identified predictors (n x iz)
c	for iz=0 it would be a null matrix
c	nnmax- maximum number of observations for n
c	n varmax - maximum number of variables for iz

c  Output
c	pms - partial mutual information
c	pic - partial information correlation	

	subroutine pmi(x,y,z,iz,n,pms,pic,nnmax,nvarmax)
      Implicit double precision (A-H,O-Z) 
	real(8) x(nnmax),y(nnmax),z(nnmax,nvarmax),xx(nnmax,nvarmax)
	real(8) pic,pms
	integer iz,n,nnmax,nvarmax

	pic=0.0
	pms=0.0
	iboot=0
	nxmax1=nnmax
	nvmax1=nvarmax
	maxboot=nnmax


	xpzmi=0.0
	xzmi=0.0
	pzmi=0.0
	zmi=0.0
	ami=0.0



c	if(ip.eq.1)write(*,*)iy,(izv(j),j=1,iz)
		
c	for MI of xpZ
	xpzmi=0.0
	do i=1,n
	ii=1
	xx(i,ii)=x(i)
	ii=ii+1
	xx(i,ii)=y(i)
	if(iz.gt.0)then
	do j=1,iz
	ii=ii+1
	xx(i,ii)=z(i,j)
	enddo
	endif
	enddo

	nvv=2
	if(iz.gt.0)nvv=nvv+iz
	call mutinf(xx,n,nvv,nnmax,nvarmax,xpzmi)
c	for MI of pZ
	pzmi=0.0
	if(iz.eq.0)goto 313
	do i=1,n
	ii=1
	xx(i,ii)=y(i)
	if(iz.gt.0)then
	do j=1,iz
	ii=ii+1
	xx(i,ii)=z(i,j)
	enddo
	endif
	enddo

	nvv=ii
	call mutinf(xx,n,nvv,nnmax,nvarmax,pzmi)

c	for MI of xZ
313	if(iz.eq.0)goto 312
c	if(ixzmi.gt.0)goto 312
	do i=1,n
	ii=1
	xx(i,ii)=x(i)
	if(iz.gt.0)then
	do j=1,iz
	ii=ii+1
	xx(i,ii)=z(i,j)
	enddo
	endif
	enddo

	nvv=ii
	call mutinf(xx,n,nvv,nnmax,nvarmax,xzmi)
	ixzmi=1 

c	for MI of Z
 312	if(iz.le.1)goto 311
c	if(izmi.gt.0)goto 311
	do i=1,n
	ii=0
	if(iz.gt.0)then
	do j=1,iz
	ii=ii+1
	xx(i,ii)=z(i,j)
	enddo
	endif
	enddo

	nvv=ii
	call mutinf(xx,n,nvv,nnmax,nvarmax,zmi)

	izmi=1

311	pms=xpzmi-pzmi-xzmi+zmi
	aa=dabs(xpzmi)-dabs(pzmi)-dabs(xzmi)+dabs(zmi)
c	if(pms.lt.0.0)pms=0.0
c	if(iz.gt.3)then
c	if(ip.eq.1)write(*,*)pms,n,nn,iz
c	pause
c	endif
c	if(iz.eq.2)stop
	if(pms.gt.0.0)pic=dsqrt(1.0-dexp(-2.0*pms))
c	if(xpzmi.gt.0.0)ami=sqrt(1.0-exp(-2.0*xpzmi))
c	write(4,315)iy,itr,xpzmi,pzmi,xzmi,zmi,pms,aa,pic
c 315	format(2i5,10f8.2)
c	pause

	return
	end
c****************************************************************
c
c subroutine mimain.for
c written by : Ashish Sharma, UNSW, 22/3/98
c last updated : 22/3/98
c modified by : Kin Luk, UNSW, 26/3/98
c modified by : Ashish Sharma, UNSW, 14/03/99
c last updated : 4/4/98 - with subroutine mi and comments added.
c	 	 1/7/98 - (1) with arguments changed to conform with
c			  the conventions used in the Numerical Recipes.
c			  (2) the following options added:
c				a. grid or point probability density
c				   estimate
c				b. Gaussian reference bandwidth (href) 
c				   or least square cross validation
c				   (LSCV)
c version: 1.02
c
c purpose: This subroutine calculates the mutual information criteron
c          score (mi score). The mi score is a multiple integral in the
c          form of:
c          mi = integation(f(x,y)*log2(f(x,y)/fm(x)/fm(y))*dx*dy)
c               where f(x,y) is the joint probability density function
c                     fm(x), fm(y) are the marginal probability of x
c                     and y respectively.
c          The above is an expression of mi for two dimensions. The
c          subroutine written herein is designed for multi-dimensions.
c
c design: This program is composed of the following 4 major subroutines:
c         mutinf - the main subroutine, calling all subroutines.
c         grid - calculates multi-dimensional data grids.
c         kde - estimates kernel density function from input data.
c         mi - performs the multiple integration to estimate the
c              mi score.
c
c major variables:
c       x(nx,nv) - observed data series, with nx rows and nv columns.
c       nx - number of observations.
c       nv - dimensions of the data series.
c
c files used:
c       Input - passed from driver program.
c       Output - passed to driver program. 
c
c external references:
c       Input - driver program.
c	Output - driver program.
c               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        subroutine mutinf(x,nx,nv,nxmax,nvmax,amiscore) 
      Implicit double precision (A-H,O-Z) 
        real(8) x(nxmax,nvmax)
c        parameter (nxmax1=nxmax,nvmax1=nvmax)
	real(8) funiv(nxmax,nvmax),fmult(nxmax),ftmp(nxmax),
     &  xtmp1(nxmax),xcopy1(nxmax,nvmax)
	real(8) tmp1(nxmax),score,product
        real(8) sum, total, amiscore
        
        
ccccccccccc sample estimate of the MI score ccccccccccccccccccccccccccc

	
        
c 1 - to estimate univariate prob densities

        do 200 i=1,nv
	  sum = 0.0
	  total = 0.0
        
          do 210 j=1,nx
	    xtmp1(j)=x(j,i)
210	  continue

          call kde(xtmp1,nx,1,nxmax,nvmax,ftmp)
 
          do 220 j=1,nx
       funiv(j,i)=ftmp(j)
220	continue
200     continue


c
c 2 - to estimate multivariate probability density
c       
	
	do 240 j=1,nx
	  do 250 i=1,nv
	    xcopy1(j,i)=x(j,i)
250	  continue
240	continue

        call kde(xcopy1,nx,nv,nxmax,nvmax,fmult)
c
c 	to calculate the value log2(fmult/fm(x)/fm(y)) for each sample
c 	point and then sum them over.
c
	score = 0.0d0
	
	do 270 j=1,nx
          tmp1(j) = fmult(j)
	  do 290 i=1,nv
	if(funiv(j,i).lt.1.d0/(10**5))funiv(j,i)=1.d0/(10**5)
	    tmp1(j) = tmp1(j)/funiv(j,i)
290	  continue	
	if(tmp1(j).gt.0.0d0)then
	product=dlog(tmp1(j))
	else
	product=0.0d0
	endif
	  score = score + product
270	continue

	amiscore = score / dble(nx)
c	if(nv.le.2.and.amiscore.lt.0.0)amiscore=0.0
c	following is commented as MI of multi-dimensional variables(gt than 2) can be negative as well
c	however, check for such instances
c	if(amiscore.lt.0.0)write(4,*)nv,amiscore


c        if(ip.eq.1)write(*,*) 'amiscore = ', amiscore
c	pause   	

c	close(3)
c	close(8)
	
        return
	end

c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine kde(x,nx,nv,nxmax,nvmax,f)
      Implicit double precision (A-H,O-Z) 
        dimension x(nxmax,nvmax),f(nxmax)
c   	parameter (nvmax1=nvmax)
       	real(8) stin(nvmax,nvmax),det 
c       open(unit=3,file='bandwidth.log',status='unknown')
c        write(*,*)'kde check1'
 	 call calc_href(nx,nv,h)
       call stinv(x,nv,nx,stin,det,nxmax,nvmax) 
        call fest(x,nx,nv,nxmax,nvmax,h,f,stin,det)
c		close(3)
        return
        end
c
c***********************************************************************
        subroutine fest(x,nx,nv,nxmax,nvmax,h,fh,stin,det)
      Implicit double precision (A-H,O-Z) 
        parameter (pi = 3.1415926535897932385)
        dimension x(nxmax,nvmax),
     &  fh(nxmax), hgamma(nxmax,nvmax), sump(nvmax)
	  dimension a(nvmax),b(nvmax,nvmax)
	dimension stin(nvmax,nvmax)
 
 
        piden = (pi*2.0) ** (dble(nv)/2.0) * dble(nx-1)
  
	  al=dble(nv)
c        h=1.0/h
c	find gamma bandwidths
c	do i=1,nx
c	do j=1,nv
c	hgamma(i,j)=h
c	enddo
c	enddo
	call hfracx(x,h,hgamma,nx,nv,nxmax,nvmax) 

        do 100 i=1,nx
          fh(i)=0.d0
          do 200 j=1,nx
		  if(j.eq.i)goto 200

	do j1=1,nv
	do j2=1,nv
	b(j1,j2)=stin(j1,j2)/(hgamma(j,j1)*hgamma(j,j2))
	enddo
	a(j1)=x(j,j1) - x(i,j1)
	enddo

	DO 10 J1=1,NV
	SUMP(J1)=0.d0
	DO 20 J2=1,NV
	SUMP(J1)=SUMP(J1)+B(J1,J2)*A(J2)
 20     CONTINUE
 10     CONTINUE

	dij2=0.d0
	do j1=1,nv
	dij2=dij2+sump(j1)*a(j1)
	enddo

      if(dij2.gt.200.d0)go to 200

	bw=1.d0
	do j1=1,nv
	bw=bw*hgamma(j,j1)
	enddo
	bw=(bw)**(1.d0/al)

	fk=exp(-0.5d0*dij2)
	fp=(bw**al)*piden*sqrt(det)
	fh(i)=fh(i)+fk/fp
	
200       continue

100     continue

	do i=1,nx
c	write(*,110)i,fh(i)
	enddo
c 110	format(i5,f10.7)
c	pause
        return
        end
c

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine calc_href(nx,nv,h)
      Implicit double precision (A-H,O-Z) 
	fac = (4.d0/(dble(nv)+2.d0))**(1.d0/(dble(nv)+4.d0))
 	h = fac * dble(nx)**(-1.d0/(dble(nv)+4.d0))
c 	write(3,*) 'h=', h
        return
        end

c*******************************************************
	subroutine stinv(dt,nvar,n,stin,det,nobsmax,nvarmax)
      Implicit double precision (A-H,O-Z) 
	real(8) dt(nobsmax,nvarmax),stin(nvarmax,nvarmax)
	real(8) av(nvarmax),sd(nvarmax),sum,an,a,b
	real(8) x(nobsmax)
	integer n,nvar,iv,iv1,i

	an=float(n)

	if(n.lt.7)then
	do iv=1,nvar
	do iv1=1,nvar
	stin(iv,iv1)=1.0d0
	enddo
	av(iv)=0.0d0
	enddo
	det=1.0d0
c	write(*,*)'n is less than 7'
	return
	endif

	do iv=1,nvar
	do i=1,n
	x(i)=dt(i,iv)
	enddo
	call basic8(x,av(iv),sd(iv),n)
	enddo
c	write(*,20)(sd(iv)**2,iv=1,nvar)
c	write(*,*)'Covar matrix details'
	do iv=1,nvar
	do iv1=1,nvar
	sum=0.0000000001d0
	do i=1,n
	a=dt(i,iv)
	b=dt(i,iv1)
	sum=sum+(a-av(iv))*(b-av(iv1))
	enddo
	stin(iv,iv1)=sum/an
	enddo
c	write(*,20)(stin(iv,iv1),iv1=1,nvar)
	enddo
	call solve(stin,nvar,nvarmax,det)
c	write(*,*)'inverse covar details'
c	do iv=1,nvar
c	write(*,20)(stin(iv,iv1),iv1=1,nvar)
c	enddo
c	pause
c 20	format(8f9.3)
	return
	end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       subroutine solve(ss,nv,nvrmax,det)
      Implicit double precision (A-H,O-Z) 
        real(8) ss(nvrmax,nvrmax)        
        real(8) A(nvrmax,nvrmax),W(nvrmax),V(nvrmax,nvrmax),
     &        temp(nvrmax,nvrmax),det

	if(nv.eq.1)then
	det=ss(1,1)
	if(ss(1,1).ne.0.0d0)ss(1,1)=1.0d0/ss(1,1)
	return
	endif		 
        do 100 i=1,nv
          do 110 j=1,nv
            A(i,j)=ss(i,j)
110       continue
100     continue
        call SVDCMP(a,nv,nv,nvrmax,nvrmax,w,v)

c this bit is calculation of 1/wj*U^T - see eqn 2.9.5 in NR
        do 200 i=1,nv
          do 210 j=1,nv
            temp(i,j)=A(j,i)/w(i)
210       continue
200     continue

c this bit is calculation of 2.9.5 in NR
        call matmul(v,temp,ss,nv,nvrmax)

c	calculate determinant
	det=1.0d0
        do 220 i=1,nv
            det=det*w(i)
220       continue
        return
        end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine matmul(A,B,C,n,nmax)
      Implicit double precision (A-H,O-Z) 
        real(8) A(nmax,nmax), B(nmax,nmax), C(nmax,nmax)

        do 100 i=1,n
          do 110 j=1,n
110       C(i,j)=0.d0
100     continue
        do 200 i=1,n
          do 210 j=1,n
            do 220 k=1,n
              C(i,j)=C(i,j)+A(i,k)*B(k,j)
220         continue
210       continue
200     continue

        return
        end
c
c****************************************************************
      SUBROUTINE BASIC8 (data,AVE,SD,N)
      Implicit double precision (A-H,O-Z) 
      INTEGER n
      real(8) ave,var,sd,data(n)
      INTEGER j
      real(8) s,ep
      ave=0.d0
      do 11 j=1,n
        ave=ave+data(j)
11    continue
      ave=ave/n
      var=0.0
      ep=0.0
      do 12 j=1,n
        s=data(j)-ave
        ep=ep+s
        var=var+s*s
12    continue
      sd=dsqrt((var-ep**2/n)/(n-1))
      return
      END

c************************************************
      SUBROUTINE svdcmp(a,m,n,mp,np,w,v)
      Implicit double precision (A-H,O-Z) 
      INTEGER m,mp,n,np,NMAX
      dimension a(mp,np),v(np,np),w(np)
      PARAMETER (NMAX=100)
        dimension rv1(NMAX)
CU    DOES NOT USE pythag - modification by AS
      INTEGER i,its,j,jj,k,l,nm
      real(8) anorm,c,f,g,h,s,scale,x,y,z
      g=0.d0
      scale=0.d0
      anorm=0.d0
      do 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.d0
        s=0.d0
        scale=0.d0
        if(i.le.m)then
          do 11 k=i,m
            scale=scale+dabs(a(k,i))
11        continue
          if(scale.ne.0.d0)then
            do 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          continue
            f=a(i,i)
            g=-sign(dsqrt(s),f)
            h=f*g-s
            a(i,i)=f-g
            do 15 j=l,n
              s=0.0
              do 13 k=i,m
                s=s+a(k,i)*a(k,j)
13            continue
              f=s/h
              do 14 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
14            continue
15          continue
            do 16 k=i,m
              a(k,i)=scale*a(k,i)
16          continue
          endif
        endif
        w(i)=scale *g
        g=0.d0
        s=0.d0
        scale=0.d0
        if((i.le.m).and.(i.ne.n))then
          do 17 k=l,n
            scale=scale+dabs(a(i,k))
17        continue
          if(scale.ne.0.0)then
            do 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          continue
            f=a(i,l)
            g=-sign(dsqrt(s),f)
            h=f*g-s
            a(i,l)=f-g
            do 19 k=l,n
              rv1(k)=a(i,k)/h
19          continue
            do 23 j=l,m
              s=0.0
              do 21 k=l,n
                s=s+a(j,k)*a(i,k)
21            continue
              do 22 k=l,n
                a(j,k)=a(j,k)+s*rv1(k)
22            continue
23          continue
            do 24 k=l,n
              a(i,k)=scale*a(i,k)
24          continue
          endif
        endif
        anorm1=dabs(w(i))+dabs(rv1(i))
        if(anorm1.gt.anorm)anorm=anorm1
25    continue
      do 32 i=n,1,-1
        if(i.lt.n)then
          if(g.ne.0.0)then
            do 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          continue
            do 29 j=l,n
              s=0.0
              do 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            continue
              do 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            continue
29          continue
          endif
          do 31 j=l,n
            v(i,j)=0.0
            v(j,i)=0.0
31        continue
        endif
        v(i,i)=1.d0
        g=rv1(i)
        l=i
32    continue
      do 39 i=min(m,n),1,-1
        l=i+1
        g=w(i)
        do 33 j=l,n
          a(i,j)=0.d0
33      continue
        if(g.ne.0.0)then
          g=1.0/g
          do 36 j=l,n
            s=0.0
            do 34 k=l,m
              s=s+a(k,i)*a(k,j)
34          continue
            f=(s/a(i,i))*g
            do 35 k=i,m
              a(k,j)=a(k,j)+f*a(k,i)
35          continue
36        continue
          do 37 j=i,m
            a(j,i)=a(j,i)*g
37        continue
        else
          do 38 j= i,m
            a(j,i)=0.d0
38        continue
        endif
        a(i,i)=a(i,i)+1.d0
39    continue
      do 49 k=n,1,-1
        do 48 its=1,30
          do 41 l=k,1,-1
            nm=l-1
            if((dabs(rv1(l))+anorm).eq.anorm)  goto 2
            if((dabs(w(nm))+anorm).eq.anorm)  goto 1
41        continue
1         c=0.0
          s=1.0
          do 43 i=l,k
            f=s*rv1(i)
            rv1(i)=c*rv1(i)
            if((dabs(f)+anorm).eq.anorm) goto 2
            g=w(i)
            h=dsqrt(f*f+g*g)
            w(i)=h
            h=1.0/h
            c= (g*h)
            s=-(f*h)
            do 42 j=1,m
              y=a(j,nm)
              z=a(j,i)
              a(j,nm)=(y*c)+(z*s)
              a(j,i)=-(y*s)+(z*c)
42          continue
43        continue
2         z=w(k)
          if(l.eq.k)then
            if(z.lt.0.0)then
              w(k)=-z
              do 44 j=1,n
                v(j,k)=-v(j,k)
44            continue
            endif
            goto 3
          endif
c         if(its.eq.30) pause 'no convergence in svdcmp'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=dsqrt(f*f+1.0)
          f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
          c=1.0
          s=1.0
          do 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=dsqrt(f*f+h*h)
            rv1(j)=z
            c=f/z
            s=h/z
            f= (x*c)+(g*s)
            g=-(x*s)+(g*c)
            h=y*s
            y=y*c
            do 45 jj=1,n
              x=v(jj,j)
              z=v(jj,i)
              v(jj,j)= (x*c)+(z*s)
              v(jj,i)=-(x*s)+(z*c)
45          continue
            z=dsqrt(f*f+h*h)
            w(j)=z
            if(z.ne.0.0)then
              z=1.0/z
              c=f*z
              s=h*z
            endif
            f= (c*g)+(s*y)
            x=-(s*g)+(c*y)
            do 46 jj=1,m
              y=a(jj,j)
              z=a(jj,i)
              a(jj,j)= (y*c)+(z*s)
              a(jj,i)=-(y*s)+(z*c)
46          continue
47        continue
          rv1(l)=0.0
          rv1(k)=f
          w(k)=x
48      continue
3       continue
49    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 3#(11,1&#)6UK'VIka5..
cc*************************************************
	SUBROUTINE hfracx(x,href,hgamma,n,nv,nmax,nvmax)
      Implicit double precision (A-H,O-Z)
	dimension x(nmax,nvmax),hgamma(nmax,nvmax)
	real(8) hupper, hlower
	real(8) xx(nmax),rn
	hupper=3.5
	hlower=1.0/hupper

        fac = (4.0/(float(nv)+2.0))**(1.0/(float(nv)+4.0))
         href = fac * float(n)**(-1.0/(float(nv)+4.0))
c	href=href*0.80

	do j=1,nv
	amx=-1000000.0
	amn=1000000.0
	do i=1,n
	xx(i)=x(i,j)
	hgamma(i,j)=href
	if(xx(i).gt.amx)amx=xx(i)
	if(xx(i).lt.amn)amn=xx(i)
	enddo
c	enddo
c	return



	call rank_h(xx,n,rn,nmax)
	do i=1,n
	ii=0
	aup=xx(i)+0.5*rn
	alr=xx(i)-0.5*rn
	if(aup.gt.amx)then
	alr=alr-(aup-amx)
	aup=amx
	endif
	if(alr.lt.amn)then
	aup=aup+(amn-alr)
	alr=amn
	endif

	do jj=1,n
	if(xx(jj).gt.alr.and.xx(jj).lt.aup)ii=ii+1
	enddo
	hgamma(i,j)=(1.2-float(ii)/float(n))*href
	if(hgamma(i,j).gt.hupper*href)hgamma(i,j)=hupper*href
	if(hgamma(i,j).lt.hlower*href)hgamma(i,j)=hlower*href
	enddo
	enddo
	return
	end
c****************************************************************
	subroutine rank_h(y,n,rn,nmax)
c	rank the given series from highest to lowest
        Implicit double precision (A-H,O-Z)
	real(8) x(nmax),y(nmax)
	do i=1,n
	x(i)=y(i)
	enddo
	do i=1,n-1
	do j=i+1,n
	if(x(j).gt.x(i))then
	a=x(i)
	x(i)=x(j)
	x(j)=a
	endif
	enddo
	enddo
	i1=int(float(n)*0.25+0.05)
	i2=int(float(n)*0.75+0.05)
	rn=abs(x(i1)-x(i2))
	return
	end
c****************************************************************
