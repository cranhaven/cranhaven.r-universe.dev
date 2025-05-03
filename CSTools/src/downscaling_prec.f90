! Program to downscale precipitation based on analogs method 
! for Iberian Peninsula and Balearic Islands (Autor: Petisco de Lara)  
! ******************************************************

SUBROUTINE down_prec(ic,id,nd,nm,nlat,nlon,nlatt,nlont,slat,slon,rlat,rlon,&
                     slatt,slont,ngridd,u500,v500,t500,t850,msl_si,q700,&
                     prec_hr,nanx,um,vm,nger,gu92,gv92,gu52,gv52,&
                     neni,vdmin,vref4,new_ccm,new_kvars,new_corrpar,u500e,&
                     v500e,t500e,t850e,msle,q700e,pp) 


USE MOD_CSTS
USE MOD_FUNCS
       
      IMPLICIT NONE

!cccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccc

      INTEGER, INTENT(IN) :: ic
      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: nd
      INTEGER, INTENT(IN) :: nm

      INTEGER, INTENT(IN) :: nlat
      INTEGER, INTENT(IN) :: nlon
      INTEGER, INTENT(IN) :: nlatt
      INTEGER, INTENT(IN) :: nlont
      DOUBLE PRECISION, INTENT(IN) :: slat
      DOUBLE PRECISION, INTENT(IN) :: slon
      DOUBLE PRECISION, INTENT(IN) :: rlat
      DOUBLE PRECISION, INTENT(IN) :: rlon
      DOUBLE PRECISION, INTENT(IN) :: slatt
      DOUBLE PRECISION, INTENT(IN) :: slont
      INTEGER, INTENT(IN) :: ngridd

      DOUBLE PRECISION, INTENT(IN) :: u500(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: v500(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: msl_si(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: q700(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: t500(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: t850(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: prec_hr(nd,nptos)

      INTEGER, INTENT(IN) :: nanx

      DOUBLE PRECISION, INTENT(IN) :: um(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: vm(nd,ic)
      INTEGER, INTENT(IN) :: nger
      DOUBLE PRECISION, INTENT(IN) :: gu92(nger,ic)
      DOUBLE PRECISION, INTENT(IN) :: gv92(nger,ic)
      DOUBLE PRECISION, INTENT(IN) :: gu52(nger,ic)
      DOUBLE PRECISION, INTENT(IN) :: gv52(nger,ic)

      INTEGER, INTENT(IN) :: neni
      DOUBLE PRECISION, INTENT(IN) :: vdmin(nptos,4)
      INTEGER, INTENT(IN) :: vref4(nptos,4)

      DOUBLE PRECISION, INTENT(IN) :: new_ccm(nger,nptos)
      INTEGER, INTENT(IN) :: new_kvars(nger,nptos,npx)
      DOUBLE PRECISION, INTENT(IN) :: new_corrpar(nger,nptos,npx)

      DOUBLE PRECISION, INTENT(IN) :: u500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: v500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: t500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: t850e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: msle(nm,id)
      DOUBLE PRECISION, INTENT(IN) :: q700e(nm,ic)

      DOUBLE PRECISION, INTENT(OUT) :: pp(nm,nptos)

!cccccccccccccccccccccccccccccccccc
      integer m,n
      integer i,j
      integer nvar
      integer ii
      integer jt,jp
      real rlx,rly
      real sp,dd
!cccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccc
      integer i1,i2,i3,i4,i7,iana,ice,ien,ipos,ips,ipu,ir,ire,iti,jk,k
      integer mesa,nan,nan2,nanf,nanv,nen,nmm,np
      real disu5,disu9,disv5,disv9,dmin,dt,du5,du9,dv5,dv9,supo
      real vorm,vorz

!*****************************************************************

      integer anai(nanx),ana(nanx),puh(ic)
      real u9(nd,ic),v9(nd,ic),u5(nd,ic),v5(nd,ic),he7(nd,ic),he7m(ic)
      real psl(nd,ic),ut9(nger,ic),vt9(nger,ic),ut5(nger,ic),vt5(nger,ic)
      real t5(nd,ic),t8(nd,ic),tm5(ic),tm8(ic)
      real pslm(ic),um9(ic),vm9(ic),um5(ic),vm5(ic),pslma(ic)
      real bdlon,bilat
      real pres(id),bar(id)
      real pred1(npx,nd,neni),pred1m(npx,neni)
      character sc*8,pt*9,nomeb*90

      integer nor(nanx),prs(nger,nptos,npx+7)
      integer annoa
      integer ior(nd),eqc(nptos)
      integer ref(nptos),puce(neni),puen(neni,5001) 
      
      integer est(nptos)
      real prec(nd,nptos)
      real p9(ic),p5(ic)
      real dista(nd),prees(nm,nptos)
      real rlt(ic),rln(ic),rltt(id),rlnt(id)
      real dist(nanx),dist1(npx,nanx),serin(nanx)
      real aaa(nanx)
      real ser(nd),media(npx,neni),sigma(npx,neni)
      real md,sg
      real mu9(ic),su9(ic),mv9(ic),sv9(ic),copar(nger,nptos,npx)
      real mu5(ic),su5(ic),mv5(ic),sv5(ic),ccm(nger,nptos)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c Variables nuevas por la nueva interpolacion de los predictores

      integer Vref(nptos,4)
      real Vdis(nptos,4)
      integer iii
      real distancion1, distancion2, distancion3, distancion4
      real peso1, peso2, peso3, peso4
      real calculin_modelo, calculin_calibracion
      integer ien1, ien2, ien3, ien4
      integer ik

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      m=nger
      n=nd

!********************************
! 1. Synoptic latitude and longitude calculation and assignment of
! weights windows
!
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
! Latitude and longitude calculation in the extended domain (called low
! resolution)

      do j=1,id
       rltt(j)=slatt+(((j-1)/nlont)*rlat)
       rlnt(j)=slont+((mod(j-1,nlont)+1-1)*rlon)
      enddo
!
!****************************************************************
! TRAINING REANALYSIS VARIABLES

      u5(:,:)=u500(:,:)
      v5(:,:)=v500(:,:)
      psl(:,:)=msl_si(:,:)
      he7(:,:)=q700(:,:)
      t5(:,:)=t500(:,:)
      t8(:,:)=t850(:,:)

! HIGH RESOLUTION (5KM) OBSERVATIONS
! It is neccesary to convert to tenths of mm (multiplying by 10).

      prec(:,:)=prec_hr(:,:)*10.

! Mean and standard deviation of reference synoptic fields (wind components).

      do j=1,ic
       do i=1,n
        ser(i)=um(i,j)
       enddo
       md=0.
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

! A reference centers (matching points between synoptic and high 
! resolution grids) are define to know where the predictor must be 
! calculated. 

      Vref(:,:)=vref4(:,:)
      Vdis(:,:)=vdmin(:,:)

      nen=1
      puce(1)=Vref(1,1)

      do iii=1,4 
         do  j=1,nptos 
             do  k=1,nen 
                if (Vref(j,iii).eq.puce(k)) go to 101
             enddo 
                nen=nen+1
                ipos=nen
                puce(ipos)=Vref(j,iii)
 101      continue
         enddo 
      enddo 

! Each reference point have associated to a group of high resolution grids.
      puen=0

        do k=1,nen
         do j=1,nptos
          do iii=1,4
          if(Vref(j,iii).eq.puce(k)) then
           puen(k,5001)=puen(k,5001)+1
           ipos=puen(k,5001)
           puen(k,ipos)=j
          endif
         enddo
        enddo
      enddo
   
! The predictors are obtained and normalized
! OBTAINING THE SEA LEVEL PRESSURE (PREDICTOR 1) IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(1,i,j)=psl(i,ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(1,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(1,j)=md
       sigma(1,j)=sg
       do i=1,n
        pred1(1,i,j)=(pred1(1,i,j)-media(1,j))/sigma(1,j)
       enddo
      enddo

! OBTAINING THE TREND (PREDICTOR 11) IN THE REFERENCE CENTERS

      do j=1,nen
       pred1(11,1,j)=0.
      enddo
    
      do i=2,n
       do j=1,nen
        ice=puce(j)
        pred1(11,i,j)=psl(i,ice)-psl((i-1),ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(11,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(11,j)=md
       sigma(11,j)=sg
       do i=1,n
        pred1(11,i,j)=(pred1(11,i,j)-media(11,j))/sigma(11,j)
       enddo
      enddo

! OBTAINING THE VERTICAL THERMAL GRADIENT(PREDICTOR 3)
! IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(3,i,j)=t8(i,ice)-t5(i,ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(3,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(3,j)=md
       sigma(3,j)=sg
       do i=1,n
        pred1(3,i,j)=(pred1(3,i,j)-media(3,j))/sigma(3,j)
       enddo
      enddo

! OBTAINING THE 500 hPa TEMPERATURE (PREDICTOR 2)
! IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(2,i,j)=t5(i,ice)
       enddo
      enddo

      do j=1,nen
       do i=1,n
        ser(i)=pred1(2,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(2,j)=md
       sigma(2,j)=sg
       do i=1,n
        pred1(2,i,j)=(pred1(2,i,j)-media(2,j))/sigma(2,j)
       enddo
      enddo

! OBTAINING THE VORTICITY (PREDICTOR 4) IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        rlx=rt*cos(rlt(ice)*pi/180.)*pi*rlon/180.
        rly=rt*abs(rlat)*pi/180.
        vorm=um(i,ice-nlon)-um(i,ice+nlon)
        vorm=vorm/(2.*rly)
        vorz=vm(i,ice+1)-vm(i,ice-1)
        vorz=vorz/(2.*rlx)
        pred1(4,i,j)=vorz-vorm
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(4,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(4,j)=md
       sigma(4,j)=sg
       do i=1,n
        pred1(4,i,j)=(pred1(4,i,j)-media(4,j))/sigma(4,j)
       enddo
      enddo

! OBTAINING THE GEOSTROPHIC U/V COMPONENTS (PREDICTORS 5 AND 6) IN THE REFERENCE
! CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(5,i,j)=um(i,ice)
        pred1(6,i,j)=vm(i,ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(5,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(5,j)=md
       sigma(5,j)=sg
       do i=1,n
        pred1(5,i,j)=(pred1(5,i,j)-media(5,j))/sigma(5,j)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(6,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(6,j)=md
       sigma(6,j)=sg
       do i=1,n
        pred1(6,i,j)=(pred1(6,i,j)-media(6,j))/sigma(6,j)
       enddo
      enddo

! OBTAINING THE VORTICITY IN 500 hPa (PREDICTOR 7) IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        rlx=rt*cos(rlt(ice)*pi/180.)*pi*rlon/180.
        rly=rt*abs(rlat)*pi/180.
        vorm=u5(i,ice-nlon)-u5(i,ice+nlon)
        vorm=vorm/(2.*rly)
        vorz=v5(i,ice+1)-v5(i,ice-1)
        vorz=vorz/(2.*rlx)
        pred1(7,i,j)=vorz-vorm
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(7,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(7,j)=md
       sigma(7,j)=sg
       do i=1,n
        pred1(7,i,j)=(pred1(7,i,j)-media(7,j))/sigma(7,j)
       enddo
      enddo

! OBTAINING THE GEOSTROPHIC U/V COMPONENTS IN 500 hPa (PREDICTORS 8 AND 9) 
! IN THE RERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(8,i,j)=u5(i,ice)
        pred1(9,i,j)=v5(i,ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(8,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(8,j)=md
       sigma(8,j)=sg
       do i=1,n
        pred1(8,i,j)=(pred1(8,i,j)-media(8,j))/sigma(8,j)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(9,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(9,j)=md
       sigma(9,j)=sg
       do i=1,n
        pred1(9,i,j)=(pred1(9,i,j)-media(9,j))/sigma(9,j)
       enddo
      enddo

! OBTAINING THE ESPECIFIC HUMIDITY IN 700 hPa (PREDICTOR 10) IN THE REFERENCE
! CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(10,i,j)=he7(i,ice)
       enddo
      enddo
      do j=1,nen
       do i=1,n
        ser(i)=pred1(10,i,j)
       enddo
       call estadis(ser,md,sg,n)
       media(10,j)=md
       sigma(10,j)=sg
       do i=1,n
        pred1(10,i,j)=(pred1(10,i,j)-media(10,j))/sigma(10,j)
       enddo
      enddo

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  ESTANDARIZATION OF REFERENCE WINDS (SYNOPTIC WINDS ALSO)

      do i=1,n
       do j=1,ic
        u9(i,j)=(um(i,j)-mu9(j))/su9(j)
        v9(i,j)=(vm(i,j)-mv9(j))/sv9(j)
        u5(i,j)=(u5(i,j)-mu5(j))/su5(j)
        v5(i,j)=(v5(i,j)-mv5(j))/sv5(j)
       enddo
      enddo

      do i=1,m
       do j=1,ic
        ut9(i,j)=(gu92(i,j)-mu9(j))/su9(j)
        vt9(i,j)=(gv92(i,j)-mv9(j))/sv9(j)
        ut5(i,j)=(gu52(i,j)-mu5(j))/su5(j)
        vt5(i,j)=(gv52(i,j)-mv5(j))/sv5(j)
       enddo
      enddo

! SIGNIFICANT PREDICTORS FOR EACH SYNOPTIC TYPE IN EACH HIGH
! RESOLUTION GRID POINT.
      
      ccm(:,:)=new_ccm(:,:)
      prs(:,:,:)=new_kvars(:,:,:)
      copar(:,:,:)=new_corrpar(:,:,:)   

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!**************************************************************
!**************************************************************
!
! DOWNSCALING BEGINS (ESTIMATING THE PROBLEM DAYS PRECIPITATION 
! IN EACH HIGH RESOLUTION GRID POINT)
!
!**************************************************************
!**************************************************************
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!      print*,'Downscaling begins...'
!      print*,'estimated day... '

      nmm=0
      do 1000 i=1,nm ! Estimated days loop

! ESTIMATED REANALYSIS VARIABLES

       um5(:)=u500e(i,:)
       vm5(:)=v500e(i,:)
       pres(:)=msle(i,:)
       he7m(:)=q700e(i,:)
       tm5(:)=t500e(i,:)
       tm8(:)=t850e(i,:)

!       print*,'    ',i
       nmm=nmm+1

!     Pressure synoptic grid calculation from low resolution one

      bilat=slat+(nlat-1)*rlat
      bdlon=slon+(nlon-1)*rlon
 
       ire=0
       do 111 j=1,id
        if((rltt(j).gt.slat).or.(rltt(j).lt.bilat)) go to 111
        if((rlnt(j).lt.slon).or.(rlnt(j).gt.bdlon)) go to 111
        ire=ire+1
        pslm(ire)=pres(j)
  111  continue

!      Geostrophic wind components at sea level to the estimated
!      day (pressure in Pa).
!      "/g" to invalidate gravity acceleration.

       bar=pres*100./g

       call geostrofico(bar,um9,vm9,id,ic,slatt,slont,slat,slon,&
                   rlat,rlon,rlat,rlon,nlatt,nlont,nlat,nlon,ngridd)

!      It is divided by density at sea level (standard atmosphere) to obtain 
!      the geostrophic wind components. 

       do j=1,ic
        um9(j)=um9(j)/1.225
        vm9(j)=vm9(j)/1.225
       enddo

! The estimated predictors are obtained and normalized

! OBTAINING THE 500 hPa TEMPERATURE (PREDICTOR 2)
! IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(2,j)=tm5(ice)
        pred1m(2,j)=(pred1m(2,j)-media(2,j))/sigma(2,j)
       enddo

! OBTAINING THE SEA LEVEL PRESSURE (PREDICTOR 1) IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(1,j)=pslm(ice)
        pred1m(1,j)=(pred1m(1,j)-media(1,j))/sigma(1,j)
       enddo

! OBTAINING THE TREND (PREDICTOR 11) IN THE REFERENCE CENTERS

       if(i.eq.1) then
       do j=1,nen
        pred1m(11,j)=0.
       enddo
       else
       do j=1,nen
        ice=puce(j)
        pred1m(11,j)=pslm(ice)-pslma(ice)
        pred1m(11,j)=(pred1m(11,j)-media(11,j))/sigma(11,j)
       enddo
       endif

       pslma=pslm

! OBTAINING THE VERTICAL THERMAL GRADIENT(PREDICTOR 3)
! IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(3,j)=tm8(ice)-tm5(ice)
        pred1m(3,j)=(pred1m(3,j)-media(3,j))/sigma(3,j)
       enddo

! OBTAINING THE VORTICITY (PREDICTOR 4) IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        rlx=rt*cos(rlt(ice)*pi/180.)*pi*rlon/180.
        rly=rt*abs(rlat)*pi/180.
        vorm=um9(ice-nlon)-um9(ice+nlon)
        vorm=vorm/(2.*rly)
        vorz=vm9(ice+1)-vm9(ice-1)
        vorz=vorz/(2.*rlx)
        pred1m(4,j)=vorz-vorm
        pred1m(4,j)=(pred1m(4,j)-media(4,j))/sigma(4,j)
       enddo

! OBTAINING THE GEOSTROPHIC U/V COMPONENTS (PREDICTORS 5 AND 6) IN THE REFERENCE
! CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(5,j)=um9(ice)
        pred1m(5,j)=(pred1m(5,j)-media(5,j))/sigma(5,j)
        pred1m(6,j)=vm9(ice)
        pred1m(6,j)=(pred1m(6,j)-media(6,j))/sigma(6,j)
       enddo

! OBTAINING THE VORTICITY IN 500 hPa (PREDICTOR 7) IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        rlx=rt*cos(rlt(ice)*pi/180.)*pi*rlon/180.
        rly=rt*abs(rlat)*pi/180.
        vorm=um5(ice-nlon)-um5(ice+nlon)
        vorm=vorm/(2.*rly)
        vorz=vm5(ice+1)-vm5(ice-1)
        vorz=vorz/(2.*rlx)
        pred1m(7,j)=vorz-vorm
        pred1m(7,j)=(pred1m(7,j)-media(7,j))/sigma(7,j)
       enddo

! OBTAINING THE GEOSTROPHIC U/V COMPONENTS IN 500 hPa (PREDICTORS 8 AND 9) 
! IN THE RERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(8,j)=um5(ice)
        pred1m(8,j)=(pred1m(8,j)-media(8,j))/sigma(8,j)
        pred1m(9,j)=vm5(ice)
        pred1m(9,j)=(pred1m(9,j)-media(9,j))/sigma(9,j)
       enddo

! OBTAINING THE ESPECIFIC HUMIDITY IN 700 hPa (PREDICTOR 10) IN THE REFERENCE
! CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(10,j)=he7m(ice)
        pred1m(10,j)=(pred1m(10,j)-media(10,j))/sigma(10,j)
       enddo

!  ESTANDARIZATION OF REFERENCE WINDS 

       do j=1,ic
        um9(j)=(um9(j)-mu9(j))/su9(j)
        vm9(j)=(vm9(j)-mv9(j))/sv9(j)
        um5(j)=(um5(j)-mu5(j))/su5(j)
        vm5(j)=(vm5(j)-mv5(j))/sv5(j)
       enddo

! Synoptic type determination to which the estimated day belongs.

       dmin=99999.
       iti=0
       do j=1,m
        call distan9_2(um9,ut9,m,j,p9,du9,ic)
        call distan9_2(vm9,vt9,m,j,p9,dv9,ic)
        call distan5_2(um5,ut5,m,j,p5,du5,ic)
        call distan5_2(vm5,vt5,m,j,p5,dv5,ic)
        dd=(du9+dv9+du5+dv5)/4.
        if(dd.lt.dmin) then
         dmin=dd
         iti=j
        endif
       enddo

! Determine the "nanx" reference alements more similar to each synoptic type
! and the corresponding distances

       do i1=1,n
        ior(i1)=i1
        dista(i1)=9999.
       enddo
       do 113 i1=1,n
        call distan9_2(um9,u9,n,i1,p9,disu9,ic)
        call distan9_2(vm9,v9,n,i1,p9,disv9,ic)
        call distan5_2(um5,u5,n,i1,p5,disu5,ic)
        call distan5_2(vm5,v5,n,i1,p5,disv5,ic)
        dista(i1)=(disu9+disv9+disu5+disv5)/4.
 113   continue
       call burbuja1(dista,ior,n,nanx)
       do i1=1,nanx
        anai(i1)=ior(i1)
       enddo

!*******************************************************************
!*******************************************************************
      
      do 1100 ien=1,nen 
       do 1200 i2=1,puen(ien,5001)
        ipu=puen(ien,i2)

!****************
! An analog (nanf) have synoptic similarity regarding estimated day
! when it has value in a point and presents lower distance than a 
! given threshold.

         nan=0
         nanf=0

         do i3=1,nanx
         iana=anai(i3)
          if(prec(iana,ipu).ne.-999.) then
           nan=nan+1
           ana(nan)=iana
           dist(nan)=dista(i3)
           if(dist(nan).eq.0.0) dist(nan)=0.1
           if(dist(nan).le.umb) nanf=nanf+1
          endif
         enddo

         if(nan.lt.nmin) then
!          print*,i,ipu,' there are not enough analogs '
          goto 1200
         endif

         if(nanf.le.nmin) nanf=nmin

! Significant predictors for the synoptic type of the estimated day
! in each HR grid point.

         np=prs(iti,ipu,npx+7)
         if(ccm(iti,ipu).lt.ccmi) np=0

! If no significant predictors, just a synoptic similarity is taken account.

         if(np.eq.0) then
           if(nanf.gt.nmax) then
            nan2=nmax
           else
            nan2=nanf
           endif

!!!!!!!!!!!!!!!!!!!!!!!

           prees(i,ipu)=0.
           sp=0.
           do i3=1,nan2
            iana=ana(i3)
            dt=dist(i3)
            sp=sp+1./dt
            prees(i,ipu)=prees(i,ipu)+prec(iana,ipu)*(1./dt)
           enddo
           prees(i,ipu)=prees(i,ipu)/sp
           go to 1200
         endif

! If there are significant predictors:

         do ik=1,nen
           ice=puce(ik)

           if (Vref(ipu,1).eq.ice) then
             ien1=ik   
             distancion1=Vdis(ipu,1)
             peso1=1/distancion1
             go to 251
           endif
         enddo
 251     continue


         do ik=1,nen
           ice=puce(ik)
           if (Vref(ipu,2).eq.ice) then
             ien2=ik
             distancion2=Vdis(ipu,2)
             peso2=1/distancion2
             go to 252
           endif
         enddo
 252     continue

          do ik=1,nen
           ice=puce(ik)
           if (Vref(ipu,3).eq.ice) then
             ien3=ik
             distancion3=Vdis(ipu,3)
             peso3=1/distancion3
             go to 253
           endif
         enddo
 253     continue


          do ik=1,nen
           ice=puce(ik)
           if (Vref(ipu,4).eq.ice) then
             ien4=ik
             distancion4=Vdis(ipu,4)
             peso4=1/distancion4
             go to 254
           endif
         enddo
 254     continue


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!               

         do 1250 i4=1,np
          ips=prs(iti,ipu,i4)
          do i1=1,nanf
           iana=ana(i1)
           calculin_modelo = pred1m(ips,ien1)*peso1+pred1m(ips,ien2)* &
      peso2+pred1m(ips,ien3)*peso3+pred1m(ips,ien4)*peso4/(peso1+peso2+ &
      peso3+peso4)
           calculin_calibracion = pred1(ips,iana,ien1)*peso1+ &
      pred1(ips,iana,ien2)*peso2+pred1(ips,iana,ien3)*peso3+ &
      pred1(ips,iana,ien4)*peso4/(peso1+peso2+peso3+peso4)

      dist1(ips,i1)=(calculin_modelo - calculin_calibracion)**2

          enddo
 1250    continue

! The "nanf" analogs are sorted from higher to lower similarity (taken account
! both synoptic similarity and significant predictors)

          do ii=1,nanf
           aaa(ii)=0.
           nor(ii)=ii
          enddo
          nanv=0
          do ii=1,nanf
           supo=0.
           do i7=1,np
            ips=prs(iti,ipu,i7)
            aaa(ii)= aaa(ii)+dist1(ips,nor(ii))*copar(iti,ipu,i7)
            supo=supo+copar(iti,ipu,i7)
           enddo
           aaa(ii)=aaa(ii)/supo
           if(aaa(ii).eq.0.) aaa(ii)=0.1
           if(aaa(ii).le.umbl) nanv=nanv+1
           serin(ii)=(aaa(ii)+dist(nor(ii)))/2.
          enddo

          call burbuja(serin,nor,nanf,nanf,nanf)

!!!!!!!!!!!!!!!!

         if(nanv.lt.nmin) go to 1998
         prees(i,ipu)=0.
         sp=0.
         nan2=0
         do 8888 ii=1,nanf
          if(aaa(nor(ii)).gt.umbl) go to 8888
          iana=ana(nor(ii))
          dt=serin(ii)
          sp=sp+(1./dt)
          prees(i,ipu)=prees(i,ipu)+prec(iana,ipu)*(1./dt)
          nan2=nan2+1
          if(nan2.eq.nmax) go to 1995
 8888    continue
 1995    continue
         prees(i,ipu)=prees(i,ipu)/sp

         go to 1200

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 1998    continue
         prees(i,ipu)=0.
         sp=0.
         nan2=0
!      1) If there is some local similarity analog, it is taken account
         if(nanv.gt.0) then
          do 8889 ii=1,nanf
           if(aaa(nor(ii)).gt.umbl) go to 8889
           nan2=nan2+1
           iana=ana(nor(ii))
           dt=serin(ii)
           sp=sp+(1./dt)
           prees(i,ipu)=prees(i,ipu)+prec(iana,ipu)*(1./dt)
 8889     continue
!         and it is completed with the rest of analogs in order of 
!         total similarity until the minimum number of analogs are completed. 
          do 8890 ii=1,nanf
           if(aaa(nor(ii)).le.umbl) go to 8890
           nan2=nan2+1
           iana=ana(nor(ii))
           dt=serin(ii)
           sp=sp+(1./dt)
           prees(i,ipu)=prees(i,ipu)+prec(iana,ipu)*(1./dt)
           if(nan2.eq.nmin) go to 1997
 8890     continue
 1997     continue
          prees(i,ipu)=prees(i,ipu)/sp
          go to 1200
!        2)If no analogs with local similarity, analogs with total similarity
!        are taken (synoptic+local predictors) until the minimum number of
!        analogs are completed.    
         elseif(nanv.eq.0) then
          do ii=1,nmin
           nan2=nan2+1
           iana=ana(nor(ii))
           dt=serin(ii)
           sp=sp+(1./dt)
           prees(i,ipu)=prees(i,ipu)+prec(iana,ipu)*(1./dt)
          enddo
          prees(i,ipu)=prees(i,ipu)/sp
          go to 1200
         endif


 1200   continue
 1100   continue
       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 1000 continue  !End of estimated days loop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       pp(:,:)=prees(:,:)

!++++++++++++++++++++++++++++++++++++++++++++++++++

END SUBROUTINE down_prec

