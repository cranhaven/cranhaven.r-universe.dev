
! sig_predic program selects significance predictor from 
! the finded collection 
SUBROUTINE sig_predic(nlat,nlon,nlatt,nlont,slat,slon,rlat,rlon,slatt,&
                      slont,n,ic,id,prec_hr,nger,um,vm,gu92,gv92,gu52,&
                      gv52,iri,u500,v500,msl_si,q700,t500,t850,&
                      nanx,neni,new_mi,new_ccm,new_kvars,new_corrpar)

USE MOD_CSTS
USE MOD_FUNCS

      IMPLICIT NONE 

!       0.1   Declarations of arguments
!              -------------------------

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
      INTEGER, INTENT(IN) :: n
      INTEGER, INTENT(IN) :: ic
      INTEGER, INTENT(IN) :: id
      DOUBLE PRECISION, INTENT(IN) :: prec_hr(n,nptos)
      INTEGER, INTENT(IN) :: nger
      DOUBLE PRECISION, INTENT(IN) :: um(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: vm(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: gu92(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: gv92(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: gu52(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: gv52(n,ic)
      INTEGER, INTENT(IN) :: iri(nptos)
      DOUBLE PRECISION, INTENT(IN) :: u500(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: v500(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: msl_si(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: q700(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: t500(n,ic)
      DOUBLE PRECISION, INTENT(IN) :: t850(n,ic)
      INTEGER, INTENT(IN) :: nanx
      INTEGER, INTENT(IN) :: neni

      INTEGER, INTENT(OUT) :: new_mi(nger,nptos)
      DOUBLE PRECISION, INTENT(OUT) :: new_ccm(nger,nptos)
      INTEGER, INTENT(OUT) :: new_kvars(nger,nptos,npx)
      DOUBLE PRECISION, INTENT(OUT) :: new_corrpar(nger,nptos,npx)

      integer nvar,m
 
      integer nulon,nulat,nulev,nudays,ideb,ifin,ip
      integer i,j,tt,vv
      integer is
      
!*****************************************************************
      integer mi
      real ccm
      character mdl*20,sc*8,pt*9,nomeb*90,nomef*90,ta*3,nta*1
      real he7(n,ic),he7m(ic)
      double precision u9(n,ic),v9(n,ic),u5(n,ic),v5(n,ic)
      real psl(n,ic),ut9(nger,ic),vt9(nger,ic),ut5(nger,ic),vt5(nger,ic),pseal(id)
      real presor(n,id)
      real xlat(ic),xlon(ic)
      real t5(n,ic),t8(n,ic),tm5(ic),tm8(ic)
      real pslm(ic),um9(ic),vm9(ic),um5(ic),vm5(ic),pslma(ic)
      real ue5(id),ve5(id),he7ms(ic),he7mr(id)
!
      character (len=6) :: he7ca(id)
      character (len=6) :: t8ca(id)
      real te8(id),te5(id)
      real he7me(24)
      real pres(id),bar(id),den(ic)
      real pred1(npx,n,neni),pred1m(npx,neni),predh(n,neni),predhm(neni)
!
      integer anai(nanx),ana(nanx)
      integer kvars(npx)
      integer nor(nanx)
      integer indi1(ic),indi2(ic)
      integer annor(n),mesr(n),diar(n)
      integer ior(n),anno,mes,dia,eqc(nptos)
      integer ref(nptos),puce(neni),puen(neni,5001)

      integer i1,i2,i3,i4,iana,ice,ien,ipos,ipu,ir,iv,jk,k,nan,ndcp
      integer nen,rlx,rly,vorm,vorz
      real prec(n,nptos),dis(n)
      real p9(ic),p5(ic)
      real dato1(npx,nanx),pr(nanx)
      real corrpar(npx)
      real coe(npx),con
      real rlt(ic),rln(ic),rltt(id),rlnt(id)
      real dist(nanx),dist1(npx,nanx),serin(nanx)
      real aaa(nanx)
      real ser(n),media(npx,neni),sigma(npx,neni)
      real md,sg,medh(neni),sigh(neni)
      real mu9(ic),su9(ic),mv9(ic),sv9(ic)
      real mu5(ic),su5(ic),mv5(ic),sv5(ic)
      real disu5,disu9,disv5,disv9
!*******************************************************
    
!      print*,"program 7: significant predictors"

      nvar=npx
      m=nger
     
!*********************************
! 1. Sinoptic latitude and longitude calculation and assignment of
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

!***********************************
! REANALYSIS VARIABLES

      u5(:,:)=u500(:,:)
      v5(:,:)=v500(:,:)
      psl(:,:)=msl_si(:,:)
      he7(:,:)=q700(:,:)
      t5(:,:)=t500(:,:)
      t8(:,:)=t850(:,:)

! HIGH RESOLUTION (5KM) OBSERVATIONS
! It is neccesary to convert to tenths of mm (multiplying by 10).

      prec(:,:)=prec_hr(:,:)*10.

! Mean and standard deviation of reference synoptic fields.

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

! A reference centers (matching points between sinoptic and high 
! resolution grids) are define to know where the predictor must be 
! calculated. 

      nen=1
      ref=iri

      puce(1)=ref(1)
      do 101 j=1,nptos
       do k=1,nen
        if(ref(j).eq.puce(k)) go to 101
       enddo
       nen=nen+1
       ipos=nen
       puce(ipos)=ref(j)
 101  continue

! Each reference point have associated a group of high resolution grids.
      puen=0
      do k=1,nen
       do j=1,nptos
        if(ref(j).eq.puce(k)) then
         puen(k,5001)=puen(k,5001)+1
         ipos=puen(k,5001)
         puen(k,ipos)=j
        endif
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

! OBTAINING THE GEOSTROPHIC U/V COMPONENTS (PREDICTORS 5 AND 6) IN THE REFERENCE CENTERS

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

!  ESTANDARIZATION OF REFERENCE WINDS (SINOPTIC WINDS ALSO)

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

! OBTAINING SIGNIFICANT PREDICTORS FOR EACH SINOPTIC TYPE IN EACH HIGH
! RESOLUTION GRID POINT.

      do 1000 i=1,m

!       print*,i


! Determine the "nanx" reference alements more similar to each sinoptic type
! and the corresponding distances

       do i1=1,n
        ior(i1)=i1
        dis(i1)=9999.
       enddo
       do 113 i1=1,n
        call distancia9(ut9,m,u9,n,i,i1,p9,disu9,ic)
        call distancia9(vt9,m,v9,n,i,i1,p9,disv9,ic)
        call distancia5(ut5,m,u5,n,i,i1,p5,disu5,ic)
        call distancia5(vt5,m,v5,n,i,i1,p5,disv5,ic)
        dis(i1)=(disu9+disv9+disu5+disv5)/4.

 113   continue

       call burbuja1(dis,ior,n,nanx)

       do i1=1,nanx
        anai(i1)=ior(i1)
       enddo

! Consider all high resolution grid points associated with the low resolution
! ones        

       do 1100 ien=1,nen 
        do 1200 i2=1,puen(ien,5001)
         ipu=puen(ien,i2)

! Consider predictand values (precipitation) and predictors from the analogs

         nan=0
         ndcp=0
         do i3=1,nanx
          iana=anai(i3)

          if(prec(iana,ipu).ne.-999.) then
           nan=nan+1
           ana(nan)=iana
           do i4=1,nvar
            dato1(i4,nan)=pred1(i4,iana,ien)
           enddo
           pr(nan)=prec(iana,ipu)
           if(pr(nan).eq.-3.) pr(nan)=1.
           if(pr(nan).ge.1.) ndcp=ndcp+1
          endif
         enddo
         if(nan.le.30) then
          mi=0
          ccm=-7.77
          go to 1199
         endif

         if(ndcp.le.30) then
          mi=0
          ccm=-9.99
          go to 1199
         endif

         if(nan.gt.150) nan=150

! Calculation of significant predictors, their coeficients and their
! multiple and partial correlation coeficients to estimate the 
! precipitation
!
!        mi: number of selected predictors
!        ccm: multiple correlation coeficient
!        kvars: selected predictors labels (vector)
!        corrpar: partial correlation of selected predictors (vector)
!        coe: regression coeficients associated to each predictor (vector).
!             (value = 0 when there is no selected predictor). 
!        con: Y-intercept (independent equation term)
!        tol: tolerance to select predictors

         call stepregrs&
         (pr,dato1,nanx,nvar,nan,mi,ccm,kvars,corrpar,coe,con,tol)

 1199    continue
         
          new_mi(i,ipu)=0
          new_ccm(i,ipu)=0.
          new_kvars(i,ipu,:)=0
          new_corrpar(i,ipu,:)=0. 

          new_mi(i,ipu)=mi
          new_ccm(i,ipu)=ccm

          if (mi.ne.0) then
             vv=mi
                do tt=1,mi
                   new_kvars(i,ipu,tt)=kvars(tt)
                   new_corrpar(i,ipu,tt)=corrpar(kvars(tt))
                end do   
          else
              vv=1
              tt=vv  
                new_kvars(i,ipu,tt)=0
                new_corrpar(i,ipu,tt)=0.
          end if            


 1200   continue

 1100  continue

 1000 continue

END SUBROUTINE sig_predic

