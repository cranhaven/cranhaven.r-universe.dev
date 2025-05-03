
! Program to do synoptic clasification from reanalysis: geostrophic wind
! components, 500Mb wind components and 1000Mb and 500Mb geopotential

SUBROUTINE clasif(ic,nd,nlon,nlat,slat,slon,rlat,rlon,um,vm,u500,v500,&
                  z1000,z500,nger,gu92,gv92,gu52,gv52)

USE MOD_CSTS
USE MOD_FUNCS

      Implicit none

      INTEGER, INTENT(IN) :: ic
      INTEGER, INTENT(IN) :: nd

      INTEGER, INTENT(IN) :: nlat
      INTEGER, INTENT(IN) :: nlon
      DOUBLE PRECISION, INTENT(IN) :: slat
      DOUBLE PRECISION, INTENT(IN) :: slon
      DOUBLE PRECISION, INTENT(IN) :: rlat
      DOUBLE PRECISION, INTENT(IN) :: rlon
      DOUBLE PRECISION, INTENT(IN) :: um(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: vm(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: u500(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: v500(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: z1000(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: z500(nd,ic)
      INTEGER, INTENT(OUT) :: nger
      DOUBLE PRECISION, INTENT(OUT) :: gu92(nd,ic)
      DOUBLE PRECISION, INTENT(OUT) :: gv92(nd,ic)
      DOUBLE PRECISION, INTENT(OUT) :: gu52(nd,ic)
      DOUBLE PRECISION, INTENT(OUT) :: gv52(nd,ic)

      integer n,nm
      integer i,j,k,i1,i2,iel,iger,igmin,ipos,iter
      real dis,disu5,disv5,disu9,disv9,dmin 
      real u9(nd,ic),v9(nd,ic),u5(nd,ic),v5(nd,ic)
      real gu91(nd,ic),gv91(nd,ic),gu51(nd,ic),gv51(nd,ic)
      real ger9(nd,ic),ger5(nd,ic),geo9(nd,ic),geo5(nd,ic)
      real ger9_newUnd(nd,ic)
      real ser(nd),md,sg,mu9(ic),su9(ic),mv9(ic),sv9(ic)
      real mu5(ic),su5(ic),mv5(ic),sv5(ic)

      integer cl(nd,umb_ger+1),ger(nd+1)
      real p9(ic),p5(ic),rlt(ic),rln(ic)

      n=nd
      nm=umb_ger

!      print*,'program 3: sinoptic clasification'

! Calculation to assign the weights to each grid point.

      do j=1,ic
       rlt(j)=slat+(((j-1)/nlon)*rlat)
       rln(j)=slon+((mod(j-1,nlon)+1-1)*rlon)
      enddo

       p9=0.
       p5=1.

       do i1=1,ic
        if((rlt(i1).le.fnor2).and.(rlt(i1).ge.fsur2)) then
         if((rln(i1).ge.foes2).and.(rln(i1).le.fest2)) then
          p9(i1)=1.
          p5(i1)=4.
         endif
        endif
       enddo
       do i1=1,ic
        if((rlt(i1).le.fnor1).and.(rlt(i1).ge.fsur1)) then
         if((rln(i1).ge.foes1).and.(rln(i1).le.fest1)) then
          p9(i1)=2.
          p5(i1)=8.
         endif
        endif
       enddo
!
! REANALYSIS VARIABLES 

       u5(:,:)=u500(:,:)
       v5(:,:)=v500(:,:)
       geo9(:,:)=z1000(:,:)
       geo5(:,:)=z500(:,:)

! Mean and standard deviation of reference synoptic fields

      do j=1,ic
       do i=1,n
        ser(i)=um(i,j)
       enddo
       call estadis(ser,md,sg,n)
       mu9(j)=md
       su9(j)=sg
       do i=1,n
        ser(i)=vm(i,j)
       enddo
       call estadis(ser,md,sg,n)
       mv9(j)=md
       sv9(j)=sg
       do i=1,n
        ser(i)=u5(i,j)
       enddo
       call estadis(ser,md,sg,n)
       mu5(j)=md
       su5(j)=sg
       do i=1,n
        ser(i)=v5(i,j)
       enddo
       call estadis(ser,md,sg,n)
       mv5(j)=md
       sv5(j)=sg
      enddo


! Geostrophic wind components standatization

      do i=1,n
       do j=1,ic
        u9(i,j)=(um(i,j)-mu9(j))/su9(j)
        v9(i,j)=(vm(i,j)-mv9(j))/sv9(j)
        u5(i,j)=(u5(i,j)-mu5(j))/su5(j)
        v5(i,j)=(v5(i,j)-mv5(j))/sv5(j)
       enddo
      enddo

! Finding the cluster centers 

      ger(n+1)=1
      ger(1)=1
      do 200 i=2,n
       do 210 j=1,ger(n+1)
        iger=ger(j)
        call distan9(u9,n,ic,i,iger,p9,disu9)
        call distan9(v9,n,ic,i,iger,p9,disv9)
        call distan5(u5,n,ic,i,iger,p5,disu5)
        call distan5(v5,n,ic,i,iger,p5,disv5)
        dis=(disu9+disv9+disu5+disv5)/4.
        if(dis.lt.umb) go to 200
 210   continue
       ger(n+1)=ger(n+1)+1
       ipos=ger(n+1)
       ger(ipos)=i
 200  continue

      do k=1,ger(n+1)
      enddo

! K-means method: weather types

      nger=ger(n+1)

!      print*,'  number of synoptic types = ', nger

      do k=1,nger
       iger=ger(k)
       do j=1,ic
        gu92(k,j)=u9(iger,j)
        gv92(k,j)=v9(iger,j)
        gu52(k,j)=u5(iger,j)
        gv52(k,j)=v5(iger,j)
        gu91(k,j)=u9(iger,j)
        gv91(k,j)=v9(iger,j)
        gu51(k,j)=u5(iger,j)
        gv51(k,j)=v5(iger,j)
       enddo
      enddo

       iter=0
 251   continue
       cl=0
       iter=iter+1
       do 300 i1=1,n
        dmin=1000.
        igmin=0
        do 310 i2=1,nger
         call distancia9(u9,n,gu92,n,i1,i2,p9,disu9,ic)
         call distancia9(v9,n,gv92,n,i1,i2,p9,disv9,ic)
         call distancia5(u5,n,gu52,n,i1,i2,p5,disu5,ic)
         call distancia5(v5,n,gv52,n,i1,i2,p5,disv5,ic)
         dis=(disu9+disv9+disu5+disv5)/4.
         if(dis.lt.dmin) then
          dmin=dis
          igmin=i2
         endif
 310    continue
        cl(igmin,nm+1)=cl(igmin,nm+1)+1
        ipos=cl(igmin,nm+1)
        cl(igmin,ipos)=i1
 300   continue

      ger9=0.
      ger5=0.
      gu92=0.
      gv92=0.
      gu52=0.
      gv52=0.

      do i=1,nger
       do j=1,cl(i,nm+1)
        iel=cl(i,j)
        do k=1,ic
         ger9(i,k)=ger9(i,k)+geo9(iel,k)
         ger5(i,k)=ger5(i,k)+geo5(iel,k)
         gu92(i,k)=gu92(i,k)+u9(iel,k)
         gv92(i,k)=gv92(i,k)+v9(iel,k)
         gu52(i,k)=gu52(i,k)+u5(iel,k)
         gv52(i,k)=gv52(i,k)+v5(iel,k)
        enddo
       enddo
       do k=1,ic
        gu92(i,k)=gu92(i,k)/real(cl(i,nm+1))
        gv92(i,k)=gv92(i,k)/real(cl(i,nm+1))
        gu52(i,k)=gu52(i,k)/real(cl(i,nm+1))
        gv52(i,k)=gv52(i,k)/real(cl(i,nm+1))
       enddo

      enddo

      do i=1,nger
       call distancia9(gu91,n,gu92,n,i,i,p9,disu9,ic)
       call distancia9(gv91,n,gv92,n,i,i,p9,disv9,ic)
       call distancia5(gu51,n,gu52,n,i,i,p5,disu5,ic)
       call distancia5(gv51,n,gv52,n,i,i,p5,disv5,ic)
       dis=(disu9+disv9+disu5+disv5)/4.
       if(dis.ge.0.10) go to 250
      enddo

      go to 252

 250  continue

      do i=1,nger
       do j=1,ic
        gu91(i,j)=gu92(i,j)
        gv91(i,j)=gv92(i,j)
        gu51(i,j)=gu52(i,j)
        gv51(i,j)=gv52(i,j)
       enddo
      enddo
      go to 251

 252  continue

!cccccccccccccccccccccccccccccccccccccccccc

      do i=1,nger
       do j=1,ic
        gu92(i,j)=(gu92(i,j)*su9(j))+mu9(j)
        gv92(i,j)=(gv92(i,j)*sv9(j))+mv9(j)
        gu52(i,j)=(gu52(i,j)*su5(j))+mu5(j)
        gv52(i,j)=(gv52(i,j)*sv5(j))+mv5(j)
       enddo
      enddo

! These variables are not going to be used but they should not be delated
      do 401 i=1,nger
       do k=1,ic
        ger9(i,k)=ger9(i,k)/real(cl(i,nm+1))
        ger9_newUnd(i,k)=1000.+(ger9(i,k)/8.)
        ger5(i,k)=ger5(i,k)/real(cl(i,nm+1))
       enddo

 401  continue

END SUBROUTINE clasif



        



