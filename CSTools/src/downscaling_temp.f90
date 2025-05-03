! Program to downscale maximum and minimum temperature based on analogs method 
! for Iberian Peninsula and Balearic Islands (Autor: Petisco de Lara)  
! ******************************************************

SUBROUTINE down_temp(ic,id,nd,nm,nlat,nlon,nlatt,nlont,slat,slon,rlat,rlon,&
                     slatt,slont,ngridd,u500,v500,t500,t850,msl_si,q700,&
                     t700,tm2m,tmx_hr,tmn_hr,nanx,nvar,dia,mes,um,vm,&
                     insol,neni,vdmin,vref4,u500e,v500e,t500e,t850e,&
                     msle,q700e,t700e,tm2me,tmax,tmin)

       
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
      DOUBLE PRECISION, INTENT(IN) :: t700(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: tm2m(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: tmx_hr(nd,nptos)
      DOUBLE PRECISION, INTENT(IN) :: tmn_hr(nd,nptos)

      INTEGER, INTENT(IN) :: nanx
      INTEGER, INTENT(IN) :: nvar
      INTEGER, INTENT(IN) :: dia(nm)
      INTEGER, INTENT(IN) :: mes(nm)

      DOUBLE PRECISION, INTENT(IN) :: um(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: vm(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: insol(nd)

      INTEGER, INTENT(IN) :: neni
      DOUBLE PRECISION, INTENT(IN) :: vdmin(nptos,4)
      INTEGER, INTENT(IN) :: vref4(nptos,4)

      DOUBLE PRECISION, INTENT(IN) :: u500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: v500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: t500e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: t850e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: msle(nm,id)
      DOUBLE PRECISION, INTENT(IN) :: q700e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: t700e(nm,ic)
      DOUBLE PRECISION, INTENT(IN) :: tm2me(nm,ic)

      DOUBLE PRECISION, INTENT(OUT) :: tmax(nm,nptos)
      DOUBLE PRECISION, INTENT(OUT) :: tmin(nm,nptos)

!cccccccccccccccccccccccccccccccccc
      integer m,n
      integer i,j
      integer ii,jp
      real sp,dd
      real aaa,bdlon,bilat,dim,dift,ccm
      integer jv,kk,kki,ik,it,kp,imes,ida,ida2,idia,mi,mm
!
      integer i1,i2,i3,i4,i7,iana,ice,ien,ipos,ips,ipu,ir,ire,iti,jk,k
      integer mesa,nan,nan2,nanf,nanv,nen,nmm,np
      real disu5,disu9,disv5,disv9,dmin,dt,du5,du9,dv5,dv9,supo
      real vorm,vorz
!
      character sc*8,pt*9
      real u9(nd,ic),v9(nd,ic),u5(nd,ic),v5(nd,ic)
      real mu9(ic),su9(ic),mv9(ic),sv9(ic)
      real mu5(ic),su5(ic),mv5(ic),sv5(ic)
      real p9(ic),p5(ic),rlt(ic),rln(ic),rltt(id),rlnt(id)
      real inso(nd),t8(nd,ic),t7(nd,ic),t5(nd,ic),he7(nd,ic)
      real efan(nd,ic),psl(nd,ic),pslm(ic),t2(nd,ic),t2m(ic)
      real pres(id),bar(id),ser(nd),md,sg
      integer ior(nd),ref(nptos),puce(neni)
      
      real um9(ic),vm9(ic),um5(ic),vm5(ic),he7m(ic)
      real t8m(ic),t7m(ic),t5m(ic)
      real insom,pred1(nps,nd,neni),pred1m(nps,neni),pred(nvar)
      integer ana(nanx),anai(nanx)
      real tmxr(nd,nptos),tmir(nd,nptos),dis(nd)
      real dato1(nvar,nanx),tempx(nanx),tempi(nanx)
      real ccmux(nptos),ccmui(nptos)
      integer kvars(nvar)
      real corrpar(nvar)
      real tmxes(nm,nptos),tmies(nm,nptos)
      real coe(nvar),con
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer Vref(nptos,4)
      real Vdis(nptos,4)
      integer iii
      real distancion1, distancion2, distancion3, distancion4
      real peso1, peso2, peso3, peso4, calculin
      integer ien1, ien2, ien3, ien4

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      m=nm
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

!
!****************************************************************
! TRAINING REANALYSIS VARIABLES

      u9(:,:)=um(:,:) 
      v9(:,:)=vm(:,:) 
      u5(:,:)=u500(:,:)
      v5(:,:)=v500(:,:)
      psl(:,:)=msl_si(:,:)
      he7(:,:)=q700(:,:)
      t5(:,:)=t500(:,:)
      t8(:,:)=t850(:,:)
      t7(:,:)=t700(:,:)
      t2(:,:)=tm2m(:,:)

! INSOLATION PARAMETER

      inso(:)=insol(:)

! The predictors are obtained 
! OBTAINING THE GEOSTROPHIC U/V COMPONENTS IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(2,i,j)=u9(i,ice)
        pred1(3,i,j)=v9(i,ice)
       enddo
      enddo

! OBTAINING THE SEA LEVEL PRESSURE IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(4,i,j)=psl(i,ice)
       enddo
      enddo

! OBTAINING THE 850 hPa TEMPERATURE IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(1,i,j)=t8(i,ice)
       enddo
      enddo

! OBTAINING THE ESPECIFIC HUMIDITY IN 700 hPa IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(5,i,j)=he7(i,ice)
       enddo
      enddo

! OBTAINING THE 2 METERS TEMPERATURE IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(8,i,j)=t2(i,ice)
       enddo
      enddo

! OBTAINING THE 700 hPa TEMPERATURE IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(6,i,j)=t7(i,ice)
       enddo
      enddo

! OBTAINING THE 500 hPa TEMPERATURE IN THE REFERENCE CENTERS

      do i=1,n
       do j=1,nen
        ice=puce(j)
        pred1(7,i,j)=t5(i,ice)
       enddo
      enddo

! MEAN AND DEVIATION OF REFERENCE FIELDS (WINDS)

      do j=1,ic
       do i=1,n
        ser(i)=u9(i,j)
       enddo
       call estadis(ser,md,sg,n)
       mu9(j)=md
       su9(j)=sg
       do i=1,n
        ser(i)=v9(i,j)
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

! REFERENCE WINDS ARE NORMALIZED  

      do i=1,n
       do j=1,ic
        u9(i,j)=(u9(i,j)-mu9(j))/su9(j)
        v9(i,j)=(v9(i,j)-mv9(j))/sv9(j)
        u5(i,j)=(u5(i,j)-mu5(j))/su5(j)
        v5(i,j)=(v5(i,j)-mv5(j))/sv5(j)
       enddo
      enddo

! HIGH RESOLUTION (5KM) MAXIMUM AND MINIMUM OBSERVED TEMPERATURE
  
      tmxr(:,:)=tmx_hr(:,:)
      tmir(:,:)=tmn_hr(:,:)

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!**************************************************************
!**************************************************************
!
! DOWNSCALING BEGINS (ESTIMATING THE PROBLEM DAYS MAXIMUM AND
! MINIMUM TEMPERATURES IN EACH HIGH RESOLUTION GRID POINT)
!
!**************************************************************
!**************************************************************
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!      print*,'Downscaling begins...'
!      print*,'estimated day... '

      mm=0
      do 1000 i=1,m

! ESTIMATED REANALYSIS VARIABLES

       um5(:)=u500e(i,:)
       vm5(:)=v500e(i,:)
       t8m(:)=t850e(i,:)
       t7m(:)=t700e(i,:)
       t5m(:)=t500e(i,:)
       he7m(:)=q700e(i,:)
       t2m(:)=tm2me(i,:)
       pres(:)=msle(i,:)

!       print*,'    ',i
       mm=mm+1

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

       bar=pres*100./9.81

       call geostrofico(bar,um9,vm9,id,ic,slatt,slont,slat,slon,&
                   rlat,rlon,rlat,rlon,nlatt,nlont,nlat,nlon,ngridd)    

!      It is divided by density at sea level (standard atmosphere) to obtain 
!      the geostrophic wind components. 

       do j=1,ic
        um9(j)=um9(j)/1.225
        vm9(j)=vm9(j)/1.225
       enddo
       
! The estimated predictors are obtained
! OBTAINING THE GEOSTROPHIC U/V COMPONENTS IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(2,j)=um9(ice)
        pred1m(3,j)=vm9(ice)
       enddo

! OBTAINING THE SEA LEVEL PRESSURE IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(4,j)=pslm(ice)
       enddo

! OBTAINING THE 850 hPa TEMPERATURE IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(1,j)=t8m(ice)
       enddo

! OBTAINING THE ESPECIFIC HUMIDITY IN 700 hPa IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(5,j)=he7m(ice)
       enddo

! OBTAINING THE 2 METERS TEMPERATURE IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(8,j)=t2m(ice)
       enddo

! OBTAINING THE 700 hPa TEMPERATURE IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(6,j)=t7m(ice)
       enddo

! OBTAINING THE 500 hPa TEMPERATURE IN THE REFERENCE CENTERS

       do j=1,nen
        ice=puce(j)
        pred1m(7,j)=t5m(ice)
       enddo

! REFERENCE WINDS ARE NORMALIZED

       do j=1,ic
        um9(j)=(um9(j)-mu9(j))/su9(j)
        vm9(j)=(vm9(j)-mv9(j))/sv9(j)
        um5(j)=(um5(j)-mu5(j))/su5(j)
        vm5(j)=(vm5(j)-mv5(j))/sv5(j)
       enddo

! INSOLATION PARAMETER ARE CALCULATED

        idia=dia(i)
        imes=mes(i)
        call fechanno(idia,imes,ida)
        ida2=ida-80
        if(ida2.le.0) ida2=ida2+365
        aaa=2.*pi*float(ida2)/365.
        insom=sin(aaa)


! Synoptic type determination: the "nanx" reference alements 
! more similar to each synoptic type and the corresponding 
! distances.

       do k=1,n
        ior(k)=k
        dis(k)=9999.
       enddo

       do 110 j=1,n
        call distan9_2(um9,u9,n,j,p9,disu9,ic)
        call distan9_2(vm9,v9,n,j,p9,disv9,ic)
        call distan5_2(um5,u5,n,j,p5,disu5,ic)
        call distan5_2(vm5,v5,n,j,p5,disv5,ic)
        dim=(disu9+disv9+disu5+disv5)/4.
        dis(j)=dim
 110   continue
       call burbuja1(dis,ior,n,nanx)
       do j=1,nanx
        anai(j)=ior(j)
       enddo

        do 1200 ipu=1,nptos

! Reference environment

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


! Predictors for the estimated day
! INSOLATION PREDICTOR

         pred(1)=insom

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do jp=1,nps
         kp=jp+1
      calculin = pred1m(jp,ien1)*peso1+pred1m(jp,ien2)*peso2+ &
      pred1m(jp,ien3)*peso3+pred1m(jp,ien4)*peso4
      pred(kp)= calculin/(peso1+peso2+peso3+peso4)
        enddo

!!!!!!!!!!!!!!!!!!!!!!!!!
! FOR MAXIMUM TEMPERATURE         
!!!!!!!!!!!!!!!!!!!!!!!!!

         nan=0
         do 1201 i2=1,nanx
          iana=anai(i2)
          if(tmxr(iana,ipu).ne.-999.) then
           nan=nan+1
           ana(nan)=iana

           dato1(1,nan)=inso(iana)
           do jp=1,nps
            kp=jp+1
             dato1(kp,nan) = (pred1(jp,iana,ien1)*peso1+ &
             pred1(jp,iana,ien2)*peso2 + pred1(jp,iana,ien3)*peso3+ &
             pred1(jp,iana,ien4)*peso4 ) / (peso1+peso2+peso3+peso4)
           enddo

           tempx(nan)=tmxr(iana,ipu)
          else
           go to 1201
          endif
 1201    continue

         if(nan.gt.150) nan=150

! Calculation of significant predictors, their coeficients and their
! multiple and partial correlation coeficients to estimate the 
! maximum temperature
!
!        mi: number of selected predictors
!        ccm: multiple correlation coeficient
!        kvars: selected predictors labels (vector)
!        corrpar: partial correlation of selected predictors (vector)
!        coe: regression coeficients associated to each predictor (vector).
!             (value = 0 when there is no selected predictor). 
!        con: Y-intercept (independent equation term)
!        tol: tolerance to select predictors
           
         call stepregrs &
          (tempx,dato1,nanx,nvar,nan,mi,ccm,kvars,corrpar,coe,con,tol)

! Maximum temperature estimation. When there are no significant predictors,
! estimated temperature is the temperature of the analog that has the 2
! meters temperature more similar to the estimated day.

        if(mi.eq.0) then
         dift=999999.
         do kk=1,nan
          if(abs(pred(9)-dato1(9,kk)).lt.dift) then
            kki=kk
            dift=abs(pred(9)-dato1(9,kk))
          endif
         enddo
         tmxes(i,ipu)=tmxr(ana(kki),ipu)
        else
         tmxes(i,ipu)=con
         do jv=1,nvar
          tmxes(i,ipu)=coe(jv)*pred(jv)+tmxes(i,ipu)
         enddo
        endif

 1203   CONTINUE

!!!!!!!!!!!!!!!!!!!!!!!!!
! FOR MINIMUM TEMPERATURE
!!!!!!!!!!!!!!!!!!!!!!!!!

         nan=0
         do 1202 i2=1,nanx
          iana=anai(i2)
          if(tmir(iana,ipu).ne.-999.) then
           nan=nan+1
           ana(nan)=iana
!!!!
! With NAN observed tmin data, next lines should be included
!c           dato1(1,nan)=inso(iana)
!c           do jp=1,nps
!c            kp=jp+1
!c            dato1(kp,nan)=pred1(jp,iana,ien)
!c            enddo
!!!!
           tempi(nan)=tmir(iana,ipu)
          else
           go to 1202
          endif
 1202    continue

         if(nan.gt.150) nan=150

! Calculation of significant predictors, their coeficients and their
! multiple and partial correlation coeficients to estimate the 
! minimum temperature
!
!        mi: number of selected predictors
!        ccm: multiple correlation coeficient
!        kvars: selected predictors labels (vector)
!        corrpar: partial correlation of selected predictors (vector)
!        coe: regression coeficients associated to each predictor (vector).
!             (value = 0 when there is no selected predictor). 
!        con: Y-intercept (independent equation term)
!        tol: tolerance to select predictors

         call stepregrs &
          (tempi,dato1,nanx,nvar,nan,mi,ccm,kvars,corrpar,coe,con,tol)

! Minimum temperature estimation. When there are no significant predictors,
! estimated temperature is the temperature of the analog that has the 2
! meters temperature more similar to the estimated day.

        if(mi.eq.0) then
         dift=999999.
         do kk=1,nan
          if(abs(pred(9)-dato1(9,kk)).lt.dift) kki=kk
         enddo
         tmies(i,ipu)=tmir(ana(kki),ipu)
        else
         tmies(i,ipu)=con
         do jv=1,nvar
          tmies(i,ipu)=coe(jv)*pred(jv)+tmies(i,ipu)
         enddo
        endif

 1200   continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 1000 continue  !End of estimated days loop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      tmax(:,:)=nint(10.*tmxes(:,:)) 
      tmin(:,:)=nint(10.*tmies(:,:)) 
 
!++++++++++++++++++++++++++++++++++++++++++++++++++

END SUBROUTINE down_temp


