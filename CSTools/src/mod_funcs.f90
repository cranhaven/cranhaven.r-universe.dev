
MODULE MOD_FUNCS

CONTAINS

      SUBROUTINE ESTADIS(SER,MEDIA,SIGMA,N)
! CALCULA LA MEDIA Y LA DESVIACION TIPO DE UNA SERIE DE DATOS
      REAL SER(N),MEDIA,SIGMA
      MEDIA=0.
      DO I=1,N
       MEDIA=MEDIA+SER(I)
      ENDDO
      MEDIA=MEDIA/REAL(N)
      SIGMA=0.
      DO I=1,N
       SIGMA=SIGMA+(SER(I)-MEDIA)**2
      ENDDO
      SIGMA=SIGMA/REAL(N)
      SIGMA=SQRT(SIGMA)
      RETURN
      END SUBROUTINE

      SUBROUTINE BURBUJA(A,NOR,NAN,NAN1,IAN)
      REAL A(NAN)
      INTEGER NOR(NAN)
      DO 100 I=1,IAN
       DO 110 J=I+1,NAN1
         IF(A(I).GT.A(J)) THEN
           TEM=A(J)
           ITEM=NOR(J)
           A(J)=A(I)
           NOR(J)=NOR(I)
           A(I)=TEM
           NOR(I)=ITEM
         ENDIF
  110  CONTINUE
  100 CONTINUE
      RETURN
      END SUBROUTINE

      subroutine burbuja1(a,nor,n,nan)
      real a(n)
      integer nor(n)
      do 100 i=1,nan
       do 110 j=i+1,n
         if(a(i).gt.a(j)) then
          tem=a(j)
          item=nor(j)
          a(j)=a(i)
          nor(j)=nor(i)
          a(i)=tem
          nor(i)=item
         endif
 110   continue
 100  continue
      return
      end subroutine

      subroutine distan9(ca,n,ic,i,nr,p,dis)
      real ca(n,ic),p(ic)
      dis=0.
      sp=0.
      do 10 k=1,ic
       if(p(k).eq.0.) go to 10
       dis=dis+p(k)*(ca(i,k)-ca(nr,k))**2
       sp=sp+p(k)
 10   continue
      dis=dis/sp
      return
      end subroutine

      subroutine distan5(ca,n,ic,i,nr,p,dis)
      real ca(n,ic),p(ic)
      dis=0.
      sp=0.
      do k=1,ic
       dis=dis+p(k)*(ca(i,k)-ca(nr,k))**2
       sp=sp+p(k)
      enddo
      dis=dis/sp
      return
      end subroutine

!
      subroutine distancia9(ca,n,cg,m,i,nr,p,dis,ic)
!      implicit none
!      real ca(n,ic),cg(m,ic),p(ic)
      real ca(n,ic),p(ic)
!      real cg(m,ic)
      double precision cg(m,ic)
      dis=0.
      sp=0.
      do 100 k=1,ic
       if(p(k).eq.0.) go to 100
       dis=dis+p(k)*(ca(i,k)-cg(nr,k))**2
       sp=sp+p(k)
 100  continue
      dis=dis/sp
      return
      end subroutine

      subroutine distancia5(ca,n,cg,m,i,nr,p,dis,ic)
!      implicit none
!      real ca(n,ic),cg(m,ic),p(ic)
      real ca(n,ic),p(ic)
      double precision cg(m,ic)
      dis=0.
      sp=0.
      do k=1,ic
       dis=dis+p(k)*(ca(i,k)-cg(nr,k))**2
       sp=sp+p(k)
      enddo
      dis=dis/sp
      return
      end subroutine

      subroutine distan9_2(cb,ca,n,nr,p,dis,ic)
      real cb(ic),ca(n,ic),p(ic)
      dis=0.
      sp=0.
      do 100 k=1,ic
       if(p(k).eq.0.) go to 100
       dis=dis+p(k)*(cb(k)-ca(nr,k))**2
       sp=sp+p(k)
 100  continue
      dis=dis/sp
      return
      end subroutine

      subroutine distan5_2(cb,ca,n,nr,p,dis,ic)
      real cb(ic),ca(n,ic),p(ic)
      dis=0.
      sp=0.
      do k=1,ic
       dis=dis+p(k)*(cb(k)-ca(nr,k))**2
       sp=sp+p(k)
      enddo
      dis=dis/sp
      return
      end subroutine

      subroutine distancia9_2(ca,n,cg,m,i,nr,p,dis,ic)
      real ca(n,ic),cg(m,ic),p(ic)
      dis=0.
      sp=0.
      do 100 k=1,ic
       if(p(k).eq.0.) go to 100
       dis=dis+p(k)*(ca(i,k)-cg(nr,k))**2
       sp=sp+p(k)
 100  continue
      dis=dis/sp
      return
      end subroutine

      subroutine distancia5_2(ca,n,cg,m,i,nr,p,dis,ic)
      real ca(n,ic),cg(m,ic),p(ic)
      dis=0.
      sp=0.
      do k=1,ic
       dis=dis+p(k)*(ca(i,k)-cg(nr,k))**2
       sp=sp+p(k)
      enddo
      dis=dis/sp
      return
      end subroutine

      SUBROUTINE STEPREGRS &
       (YI,XI,NX,NVARX,N,MI,CCM,IVAR,COPA,COE,CON,TOL)

!EFECTUA UNA REGRESION LINEAL MULTIPLE POR ETAPAS
!MEDIANTE LA TECNICA 'PASO A PASO' INTRODUCIENDO
!EN CADA PASO COMO NUEVA VARIABLE LA DE MEJOR  
!CORRELACION PARCIAL CON LA VARIABLE DEPENDIENTE
!Y ELIMINANDO AQUELLAS QUE DESPUES DE CADA
!REGRESION NO SEAN SIGNIFICATIVAS

!NX=numero maximo de datos posibles
!N=numero de datos actuales a usar
!NVAR= numero total de variables posibles
!     de regresion

!LA VARIABLE YI(NX) CONTIENE LOS VALORES DEL PREDICTANDO
!LA VARIABLE XI(NVARX,NX) CONTIENE LOS VALORES DE LOS PREDICTORES

!LA VARIABLE YY(N) CONTIENE LOS VALORES DEL PREDICTANDO
!LA VARIABLE XX(NVARX,N) CONTIENE LOS VALORES DE LOS PREDICTORES

!   MI es el numero de variables o predictores seleccionados

!   CCM es el coeficiente de correlacion multiple

!   IVAR(NVARX) contiene los numeros de etiqueta de los predictores
!      seleccionados en la regresion

!COPA(NVARX) contiene las correlaciones parciales de 
!      los predictores seleccionados.

!   COE(NVARX) contiene los coeficientes de regresion beta(i) de las 
!      variables seleccionadas  

!   CON contiene la constante de la regresion (beta0)
   
!   DATO1(NVARX,N) contiene los datos de los predictores 
!      que se meten en cada paso de regresion

!   COEF(0:NVARX) contiene la constante y los coeficientes
!      de regresion de las variables introducidas en cada paso

!   YYES(N) contiene valores del predictando estimados por la regresion

!   SST es la variabilidad total,
!   SSE es la variabilidad residual no explicada por la regresion 

!   CDET es el coeficiente de determinacion en el paso actual 
!   CDATA es el coeficiente de determinacion en el paso anterior

!   CDETP(NVARX) contiene los coeficientes de determinacion
!      cuando se considera cada variable como introducida en el paso
!      actual. Se utiliza como base para eliminar variables que 
!      se consideran no significativas

!   TOL representa el minimo incremento de variabilidad explicada
!      por la introduccion de una variable para que esta se 
!      considere significativa


      real yi(nx),xi(nvarx,nx)
      real yy(n),xx(nvarx,n),res1(n),res(n),ser1(n),ser2(n),aa(n)
      real yyr(n),cdet1,cp,ay(n),cormax,cor,ccm
      real copa(nvarx),dato1(nvarx,n)
      real coef(0:nvarx),coe(nvarx),con
      real yyes(n),sst,sse,cdet,cdeta,cdetp(nvarx),myy,incr
      
      character var(nvarx)*5
      integer ivar(nvarx),ivar1(nvarx)

!TRASPASAMOS LOS DATOS INICIALES ENVIADOS POR EL PROGRAMA PRINCIPAL
!DESDE LAS VARIABLES DE DIMENSION MAXIMA A LAS VARIABLES CON LA  
!DIMENSION AJUSTADA AL NUMERO ACTUAL DE DATOS UTILIZADOS

      do i=1,n
       yy(i)=yi(i)
       do k=1,nvarx
        xx(k,i)=xi(k,i)
       enddo
      enddo


!CALCULO DE LA MEDIA Y DE LA VARIABILIDAD TOTAL DEL PREDICTANDO

      myy=0.
      do i=1,n
       myy=myy+yy(i)
      enddo
      myy=myy/real(n)

      sst=0.
      do i=1,n
       sst=sst+(yy(i)-myy)**2
      enddo


      


!********************************************************
!INICIALIZACION DEL CONTROL
!DE LAS VARIABLES INTRODUCIDAS EN EL MODELO
!EN CADA PASO
      
      do j=1,nvarx
       var(j)='nosel'
      enddo

!****************************************************

!BUSQUEDA DE LA PRIMERA VARIABLE DEL MODELO 
!   (UNICA VARIABLE EN LA PRIMERA ETAPA)

      cdeta=0.
      cormax=-2.0
      nvx=0
      do j=1,nvarx
       do i=1,n
        ser2(i)=xx(j,i)
       enddo
       call corr1(yy,ser2,n,cor)
       if(abs(cor).gt.cormax) then
        cormax=abs(cor)
        nvx=j
       endif
      enddo
      var(nvx)='sisel'
      nvult=nvx
      

!****************************************************
!PREPARACION DE LA MATRIZ DE DATOS PARA EL
!CALCULO DE LA REGRESION DE LAS VARIABLES
!INDEPENDIENTES SELECCIONADAS CON LA VARIABLE
!DEPENDIENTE
      
 222  nuvar=0
      dato1=0.
      ivar=0
      do 100 j=1,nvarx
       if(var(j).ne.'sisel') go to 100
       nuvar=nuvar+1
       ivar(nuvar)=j
       do i=1,n
        dato1(nuvar,i)=xx(j,i)
       enddo
 100  continue
      
!SE CALCULA LA REGRESION CON LAS VARIABLES SELECCIONADAS

      yyr=yy
      call regr(yyr,dato1,nvarx,nuvar,n,coef)



!SE CALCULA EL COEFICIENTE DE DETERMINACION (esta subrutina
!  devuelve los residuos de la regresion y el coeficiente de
!  determinacion)

      call coedet(yy,xx,n,nvarx,ivar,nuvar,coef,res1,sst,cdet)
   

!COMPROBAMOS SI EL COEFICIENTE DE DETERMINACION SE HA INCREMENTADO 
!SUFICIENTEMENTE COMO PARA CONSIDERAR SIGNIFICATIVA LA ULTIMA VARIABLE
!INTRODUCIDA. SI NO LO ES SE ACABA EL PROCESO PASO A PASO Y OBTENEMOS
!LA REGRESION DEFINITIVA

      incr=cdet-cdeta
      if(incr.lt.tol) then
       if(nuvar.eq.1) then
        mi=0
        ccm=-8.88       
        go to 555
       else
        var(nvult)='elimi'
        go to 444
       endif
      endif

      cdeta=cdet

!SE COMPRUEBA SI ALGUNA DE LAS VARIABLES SELECCIONADAS RESULTA NO
!SIGNIFICATIVA. PARA ELLO SE COMPARAN LOS COEFICIENTES DE DETERMINACION
!OBTENIDOS QUITANDO CADA VARIABLE, CON EL OBTENIDO SIN QUITAR NINGUNA
!DE LAS QUE YA TENEMOS, SI PARA ALGUNA VARIABLE EL INCREMENTO NO 
!SUPERA EL MINIMO LA VARIABLE SE ELIMINA DEFINITIVAMENTE

      if(nuvar.eq.1) go to 333

!    Quitamos una variable cada vez

      do 200 k=1,nuvar
       dato1=0.
       nivar=0
       ivar1=0
       do 210 k1=1,nuvar
        if(k1.eq.k) go to 210
        nivar=nivar+1
        ivar1(nivar)=ivar(k1)
        do i=1,n
         dato1(nivar,i)=xx(ivar1(nivar),i)
        enddo
 210   continue

!    Se calcula la regresion con la variable quitada
   
       yyr=yy
       call regr(yyr,dato1,nvarx,nivar,n,coef)

!    Se calcula el coeficiente de determinacion
     
       call coedet(yy,xx,n,nvarx,ivar1,nivar,coef,res,sst,cdet1)

!    Si la diferencia entre el coeficiente de determinacion
!    con todas las variables y el mismo con la variable
!    quitada es menor que el umbral, la variable se considera
!    no significativa y se elimina definitivamente.

       if((cdet-cdet1).lt.tol) then
        var(ivar(k))='elimi'
       endif
 200  continue

      do k=1,nuvar
       if(var(ivar(k)).eq.'elimi') go to 332
      enddo
      go to 333

      

!ELIMINADAS LAS VARIABLES NO SIGNIFICATIVAS SE CALCULA DE NUEVO 
!LA REGRESION CON LAS VARIABLES QUE HAN QUEDADO

 332  continue
      nuvar=0
      dato1=0.
      ivar=0
      do 220 j=1,nvarx
       if(var(j).ne.'sisel') go to 220
       nuvar=nuvar+1
       ivar(nuvar)=j
       do i=1,n
        dato1(nuvar,i)=xx(j,i)
       enddo
 220  continue


      yyr=yy
      call regr(yyr,dato1,nvarx,nuvar,n,coef)
      call coedet(yy,xx,n,nvarx,ivar,nuvar,coef,res1,sst,cdet)

      cdeta=cdet

 333  continue

!SE COMPRUEBA SI HAY AUN VARIABLES QUE PUEDAN SER SELECCIONADAS
!SI HAY SE TRATA DE BUSCAR UNA NUEVA, SI NO HAY SE TERMINA

      do j=1,nvarx
       if(var(j).eq.'nosel') go to 334
      enddo
      go to 444

 334  continue

!SE BUSCA UNA NUEVA VARIABLE TOMANDO LA QUE TENGA MAYOR CORRELACION
!PARCIAL CON EL PREDICTANDO

!    Se construye matriz de datos con variables ya seleccionadas

      dato1=0.
      nivar=0
      do j=1,nvarx
       if(var(j).eq.'sisel') then
        nivar=nivar+1
        do i=1,n
         dato1(nivar,i)=xx(j,i)
        enddo
       endif
      enddo

!    Se busca nueva variable 

      cormax=-2.0
      nvx=0
      do 230 j=1,nvarx
       if(var(j).ne.'nosel') go to 230
       do i=1,n
        aa(i)=xx(j,i)
       enddo
       call corpar(res1,n,dato1,nvarx,nivar,aa,cp)
       if(abs(cp).gt.cormax) then
        cormax=abs(cp)
        nvx=j
       endif
 230  continue
        if (nvx.gt.0) then
         var(nvx)='sisel'
         nvult=nvx
        endif
        go to 222

 444  continue

! REGRESION DEFINITIVA 
     
!    PREPARACION DE MATRIZ DE DATOS CON VARIABLES DEFINITIVAS

      nuvar=0
      dato1=0.
      ivar=0
      do 250 j=1,nvarx
       if(var(j).ne.'sisel') go to 250
       nuvar=nuvar+1
       ivar(nuvar)=j
       do i=1,n
        dato1(nuvar,i)=xx(j,i)
       enddo
 250  continue

! CALCULO DE LA REGRESION
  
  
      yyr=yy
      call regr(yyr,dato1,nvarx,nuvar,n,coef)     

! CALCULO DEL COEFICIENTE DE DETERMINACION Y DE LOS RESIDUOS
      
      call coedet(yy,xx,n,nvarx,ivar,nuvar,coef,res1,sst,cdet)



! RESULTADOS FINALES

! COEFICIENTES Y DEMAS DATOS DE LA REGRESION

      mi=nuvar
      ccm=sqrt(cdet)

      con=coef(0)
      coe=0.
      do k=1,nuvar
       coe(ivar(k))=coef(k)
      enddo


! COEFICIENTES DE CORRELACION PARCIAL DE LAS VARIABLES
! SELECCIONADAS CON LA VARIABLE DEPENDIENTE

      copa=-1.

      do 300 j=1,nuvar
       do i=1,n
        aa(i)=xx(ivar(j),i)
        ay(i)=yy(i)
       enddo
       nivar=0
       dato1=0.
       do k=1,nuvar
        if(k.ne.j) then
         nivar=nivar+1
         do i=1,n
          dato1(nivar,i)=xx(ivar(k),i)
         enddo
        endif
       enddo
       call corpar1(ay,n,dato1,nvarx,nivar,aa,cp)
       copa(ivar(j))=abs(cp)
 300  continue

 555     continue

      return
      end subroutine


      SUBROUTINE CORR1(CENT,COMP,IC,CORRE1)
      REAL SUM1,SUM2,MED1,MED2
      REAL CENT(IC),COMP(IC),SUMC1,SUMC2,SUMCR
      REAL COV,VAR1,VAR2,CORRE1
      SUM1=0.0
      SUM2=0.0
      DO 100 I=1,IC
         SUM1=SUM1+CENT(I)
         SUM2=SUM2+COMP(I)
100   CONTINUE
      C=REAL(IC)
      MED1=SUM1/C
      MED2=SUM2/C
      SUMC1=0.0
      SUMC2=0.0
      SUMCR=0.0
      DO 200 J=1,IC
         SUMCR=SUMCR+((CENT(J)-MED1)*(COMP(J)-MED2))
         SUMC1=SUMC1+(CENT(J)-MED1)**2
         SUMC2=SUMC2+(COMP(J)-MED2)**2
200   CONTINUE
      COV=SUMCR/C
      VAR1=SUMC1/C
      VAR2=SUMC2/C
      CORRE1=COV/SQRT(VAR1*VAR2)
      RETURN
      END SUBROUTINE


      SUBROUTINE REGR(aa,bb,nvarx,nvar,ndat,creg)


!CALCULA LA ECUACION DE REGRESION A PARTIR DE UNA MUESTRA DE DATOS

!TRABAJA CON LAS DESVIACIONES RESPECTO A LA MEDIA PARA MINIMIZAR
!LOS ERRORES DE REDONDEO POR LO QUE AL FINAL HAY QUE CALCULAR
!APARTE EL TERMINO INDEPENDIENTE DE LA ECUACION DE REGRESION
!(ver libro de D. Penna capitulo regresion multiple)


! ndat:          numero de datos de la muestra
! nvar:          numero de variables independientes
! yy(ndat):      contiene las desviaciones del predictando
! xx(nvar,ndat): contiene las desviaciones de los predictores
! myy,mxx(nvar): contiene las medias de predictandos y predictores

!         Elementos de las ecuaciones normales

!nn(nvar,nvar):  Matriz de los coeficientes de las incognitas
!                       (coeficientes de regresion salvo termino
!                        independiente beta0)
!b(nvar)          :  En entrada contiene los terminos independientes
!                       de las ecuaciones normales que se pasan a
!                       a las subrutinas que resuelven el sistema
!                    En salida contiene los coeficientes de regresion
!                       (no el termino independiente beta0)
!creg(0:nvarx)    :  Contiene la salida al programa principal de los 
!                       coeficientes de regresion y del termino
!                       independiente
!sxx(nvar):          Suma de los valores de los predictores de todos los
!                       datos de la muestra
!syy:                Suma de los valores de los predictandos 
!syyxx(nvar):        Suma de productos predictando-predictores
!sxxxx(nvar):        Suma de productos predictores-predictores


      real yy(ndat)
      real xx(nvar,ndat)
      real aa(ndat),bb(nvarx,ndat)
      real myy,mxx(nvar)
      real b(nvar),nn(nvar,nvar),creg(0:nvarx)
      real sxx(nvar),syy,syyxx(nvar),sxxxx(nvar,nvar),d
      integer indx(nvar)



! SE CALCULAN LAS MEDIAS DE LOS VALORES DE PREDICTANDOS Y PREDICTORES

      myy=0.
      do i=1,ndat
       myy=myy+aa(i)
      enddo
      myy=myy/real(ndat)
 
      mxx=0.
      do j=1,nvar
       do i=1,ndat
        mxx(j)=mxx(j)+bb(j,i)
       enddo
       mxx(j)=mxx(j)/real(ndat)
      enddo

! SE SUSTITUYEN LOS DATOS ORIGINALES POR SUS DESVIACIONES RESPECTO 
! A LA MEDIA

      do i=1,ndat
       yy(i)=aa(i)-myy   
      enddo

      do j=1,nvar
       do i=1,ndat
        xx(j,i)=bb(j,i)-mxx(j)
       enddo
      enddo

! CALCULO DE LA SUMA DE VALORES DE PREDICTANDO Y PREDICTORES
! DE TODOS LOS DATOS DE LA MUESTRA ASI COMO LA DE PRODUCTOS
! CRUZADOS (utiliza ya como variables las desviaciones respecto
! a las medias)
! (En realidad utilizando el modelo de regresion en desviaciones
!   no utilizamos las sumas de las variables aunque las calculamos)

      syy=0.
      do i=1,ndat
       syy=syy+yy(i)
      enddo

   
      sxx=0.
      syyxx=0.
      do j=1,nvar
       do i=1,ndat
        sxx(j)=sxx(j)+xx(j,i)
        syyxx(j)=syyxx(j)+yy(i)*xx(j,i)
       enddo
      enddo
      
      sxxxx=0.
      do j=1,nvar
       do k=j,nvar
        do i=1,ndat
         sxxxx(j,k)=sxxxx(j,k)+xx(j,i)*xx(k,i)
        enddo
        if(j.ne.k) sxxxx(k,j)=sxxxx(j,k)
       enddo
      enddo


! CONSTRUYE LA MATRIZ DE LOS COEFICIENTES DE LAS ECUACIONES NORMALES

      do j=1,nvar
       do k=1,nvar
        nn(j,k)=sxxxx(j,k)
       enddo
      enddo


! CONSTRUYE EL VECTOR DE TERMINOS INDEPENDIENTES DE LAS ECUACIONES
! NORMALES. EN LA SALIDA CONTENDRA LOS VALORES DE LOS COEFICIENTES
! DE REGRESION


      do j=1,nvar
       b(j)=syyxx(j)
      enddo

  
! SE RESUELVE EL SISTEMA DE ECUACIONES NORMALES Y SE OBTIENEN
! LOS COEFICIENTES DE REGRESION DE CADA VARIABLE EN LA ECUACION
! DE REGRESION
     
      call ludcmp(nn,nvar,nvar,indx,d)
      call lubksb(nn,nvar,nvar,indx,b)

      do j=1,nvar
       creg(j)=b(j)
      enddo

! SE CALCULA EL TERMINO INDEPENDIENTE DE LA ECUACION DE REGRESION

      creg(0)=myy
      do j=1,nvar
       creg(0)=creg(0)-b(j)*mxx(j)
      enddo


      return
      end subroutine

      SUBROUTINE lubksb(a,n,np,indx,b)
      INTEGER n,np,indx(nP)
      REAL a(np,np),b(np)
      INTEGER i,ii,j,ll
      REAL sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      END SUBROUTINE
! (C) Copr. 1986-92 Numerical Recipes Software !)#.
!**********************************************************
!
      SUBROUTINE ludcmp(a,n,np,indx,d)
      INTEGER n,np,indx(nP),NMAX
      REAL d,a(np,np),TINY
      PARAMETER (NMAX=500,TINY=1.0e-20)
      INTEGER i,imax,j,k
      REAL aamax,dum,sum,vv(NMAX)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax .eq. 0.) then 
!        write (*,*) 'singular matrix in ludcmp'
        else
        vv(i)=1./aamax
        endif
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      END SUBROUTINE

      SUBROUTINE COEDET(yy,xx,n,nvarx,ivar1,nivar,coef,res,sst,cdet1)
      real yy(n),yyes(n),xx(nvarx,n),res(n),sst,cdet1
      real sse
      real coef(0:nvarx)
      integer ivar1(nvarx)

!    ESTA SUBRUTINA DEVUELVE LOS RESIDUOS DE LA REGRESION 
!    Y EL COEFICIENTE DE DETERMINACION

!SE CALCULAN LOS VALORES ESTIMADOS DEL PREDICTANDO ESTIMADOS
!CON LA REGRESION

       
      do i=1,n
       yyes(i)=coef(0)
       do k=1,nivar
        yyes(i)=yyes(i)+coef(k)*xx(ivar1(k),i)
       enddo
      enddo

!SE CALCULAN LOS RESIDUOS DE LA REGRESION Y LA VARIABILIDAD
!NO EXPLICADA

      sse=0.
      do i=1,n
       res(i)=yy(i)-yyes(i)
       sse=sse+res(i)**2
      enddo
     

!SE CALCULA EL COEFICIENTE DE DETERMINACION

      cdet1=sse/sst
      cdet1=1.-cdet1

      return
      end subroutine


      SUBROUTINE CORPAR(res1,n,dato1,nvarx,nivar,aa,cp)
      real res1(n),res2(n),dato1(nvarx,n),aa(n)
      real coef(0:nvarx)
      real aaes(n),aar(n),cp

! SE OBTIENE LA REGRESION DE LA VARIABLE CUYA CORRELACION
! PARCIAL SE CALCULA, CON LAS OTRAS VARIABLES PRESENTES

      aar=aa
      call regr(aar,dato1,nvarx,nivar,n,coef)
      
! Y SE OBTIENEN LOS VALORES ESTIMADOS POR ESTA REGRESION

      do i=1,n
       aaes(i)=coef(0)
       do k=1,nivar
        aaes(i)=aaes(i)+coef(k)*dato1(k,i)
       enddo
      enddo

! SE OBTIENEN LOS RESIDUOS CORRESPONDIENTES

      do i=1,n
       res2(i)=aa(i)-aaes(i)
      enddo

! SE CALCULA LA CORRELACION PARCIAL

      call corr1(res1,res2,n,cp)


      return
      end subroutine

      SUBROUTINE CORPAR1(ay,n,dato1,nvarx,nivar,aa,cp)
      real ay(n),res1(n),res2(n),dato1(nvarx,n),aa(n)
      real ayes(n),aaes(n),ayr(n),aar(n),cp
      real coef(0:nvarx),coefy(0:nvarx)


! SE OBTIENE LA REGRESION DE LA VARIABLE DEPENDIENTE CON LAS
! OTRAS VARIABLES PRESENTES DISTINTAS DE AQUELLAS CUYA CORRELACION
! PARCIAL SE QUIERE CALCULAR

      ayr=ay
      call regr(ayr,dato1,nvarx,nivar,n,coefy)

! Y SE OBTIENEN LOS VALORES ESTIMADOS POR ESTA REGRESION

      do i=1,n
       ayes(i)=coefy(0)
       do k=1,nivar
        ayes(i)=ayes(i)+coefy(k)*dato1(k,i)
       enddo
      enddo

! SE OBTIENEN LOS RESIDUOS CORRESPONDIENTES

      do i=1,n
       res1(i)=ay(i)-ayes(i)
      enddo
      
      

! SE OBTIENE LA REGRESION DE LA VARIABLE CUYA CORRELACION
! PARCIAL SE CALCULA, CON LAS OTRAS VARIABLES PRESENTES

      aar=aa
      call regr(aar,dato1,nvarx,nivar,n,coef)

! Y SE OBTIENEN LOS VALORES ESTIMADOS POR ESTA REGRESION

      do i=1,n
       aaes(i)=coef(0)
       do k=1,nivar
        aaes(i)=aaes(i)+coef(k)*dato1(k,i)
       enddo
      enddo

! SE OBTIENEN LOS RESIDUOS CORRESPONDIENTES

      do i=1,n
       res2(i)=aa(i)-aaes(i)
      enddo

! SE CALCULA LA CORRELACION PARCIAL

      call corr1(res1,res2,n,cp)
      return
      end subroutine

      SUBROUTINE GEOSTROFICO(&
      Z,U,V,NGRID,NGRIDS,SLAT,SLON,SLATS,SLONS,RLAT,RLON,RLATS,RLONS,&
      NLAT,NLON,NLATS,NLONS,NGRIDD)

USE MOD_CSTS

! CALCULA VIENTO GEOSTROFICO A PARTIR DE  CAMPOS DE DOBLE RESOLUCION,
! CON CALCULO CENTRADO, Y DEVUELVE VALORES EN LOS PUNTOS DE GRID DE
! RESOLUCION NORMAL

      IMPLICIT INTEGER(K)

! --------- Parametros de la REJILLA DE BAJA RESOLUCION -----
! NGRID es el npuntos de la rejilla de baja resolucion
! NGRIDD se deja igual
! SLAT es la latitud de la rejilla de baja resolucion -latitud superior
! izda, SLON es la longitud ...........de la rej baja resol-longitud
! superior oeste NLAT es el numero de latitudes 

!      PARAMETER (NGRID=${id},NGRIDD=${ngridd},ROMEGA=${romega})
!      PARAMETER (SLAT=${slatt},SLON=${slont},RLAT=${rlat},
!     $           RLON=${rlon},NLAT=${nlatt},NLON=${nlont})
! ----------------------------------------------------------------------
! ------------- Parametros de la REJILLA SINOPTICA ---------
!      PARAMETER (NGRIDS=${ic},NLATS=${nlat},NLONS=${nlon})   !TERMINADO EN S,
!      GRID DE SALIDA
!      PARAMETER (SLATS=${slat},SLONS=${slon},RLATS=${rlat},
!     $           RLONS=${rlon})
!      PARAMETER (GR=${g},RT=${rt},R=${r},PI=${pi})
! ----------------------------------------------------------------------
      INTEGER, INTENT(IN) :: ngrid
      INTEGER, INTENT(IN) :: ngrids

      INTEGER, INTENT(IN) :: nlat
      INTEGER, INTENT(IN) :: nlon
      INTEGER, INTENT(IN) :: nlats
      INTEGER, INTENT(IN) :: nlons
      DOUBLE PRECISION, INTENT(IN) :: slat
      DOUBLE PRECISION, INTENT(IN) :: slon
      DOUBLE PRECISION, INTENT(IN) :: slats
      DOUBLE PRECISION, INTENT(IN) :: slons
      DOUBLE PRECISION, INTENT(IN) :: rlat
      DOUBLE PRECISION, INTENT(IN) :: rlon
      DOUBLE PRECISION, INTENT(IN) :: rlats
      DOUBLE PRECISION, INTENT(IN) :: rlons
      INTEGER, INTENT(IN) :: ngridd

      REAL GR

! RLX ES LA LONGITUD DEL PASO DE REJILLA SOBRE EL PARALELO CORRESPONDIENTE 
! RLY ES LA LONGITUD DEL PASO DE REJILLA SOBRE EL MERIDIANO CORRESPONDIENTE

! OJO, RLY Y RLX CORRESPONDEN A RESOL NORMAL, Y SON 2Ay Y 2Ax DE LA RES DOBLE
      REAL, INTENT(IN) ::      Z(NGRID)
      REAL, INTENT(OUT) ::      U(NGRIDS),V(NGRIDS)

      REAL      RLX(NGRID),RLY
      REAL      F(NGRIDD)
      REAL      GG(NGRID),GD(NGRIDD)
      REAL      RLT(NGRID),RLN(NGRID)
      REAL      RLTS(NGRIDS),RLNS(NGRIDS)

      GR=g
!      print*,'En GEOSTROFICO, g,ngrid,ngrids= ',g,ngrid,ngrids
!      print*,'En GEOSTROFICO, ngrid= ',ngrid
!      print*,'En GEOSTROFICO, ngrids= ',ngrids
!      print*,'En GEOSTROFICO, ngridd= ',ngridd
!      print*,'En GEOSTROFICO, nlat= ',nlat
!      print*,'En GEOSTROFICO, nlon= ',nlon
!      print*,'En GEOSTROFICO, nlats= ',nlats
!      print*,'En GEOSTROFICO, nlons= ',nlons
!      print*,'En GEOSTROFICO, slat= ',slat
!      print*,'En GEOSTROFICO, slon= ',slon
!      print*,'En GEOSTROFICO, slats= ',slats
!      print*,'En GEOSTROFICO, slons= ',slons
!      print*,'En GEOSTROFICO, rlat= ',rlat
!      print*,'En GEOSTROFICO, rlon= ',rlon
!      print*,'En GEOSTROFICO, rlats= ',rlats
!      print*,'En GEOSTROFICO, rlons= ',rlons

!
! CALCULA LATITUD Y LONGITUD DE CADA PUNTO J
      DO J=1,NGRID
       RLT(J)=SLAT+(((J-1)/NLON)*RLAT)
       RLN(J)=SLON+((MOD(J-1,NLON)+1-1)*RLON)

!       IF(J.GE.1.AND.J.LE.50) THEN
!       print*,"J, RLT = ",J,RLT(J)
!       print*,"   RLN = ",RLN(J)
!       ENDIF
      ENDDO
!       print*,"fuera del bucle de calculo"
!       print*,"RLT=",RLT(1:50) 
!       print*,"RLN",RLN(1:50) 
      NLOND=(NLON*2)-1
!
! CALCULA LOS VALORES DE RLX Y F,LEE P, Y ACTUALIZA KCOD
      DO J=1,NGRID
       RLX(J)=(2.*PI*RT*COS(RLT(J)*PI/180.))/(360./RLON)
       F(J)=2.*ROMEGA*SIN(RLT(J)*PI/180.)
      ENDDO
      RLY=2.*PI*RT*ABS(RLAT)/360.
      K0=0
!
!       print*,"antes de dobla"
!       print*,"RLT=",RLT(1:50) 
!       print*,"RLN",RLN(1:50) 
!
! TRANFORMA ALTURA GEOPOTENCIAL EN GEOPOTENCIAL PHI=Z*g Y CALCULA LOS VALORES
!   EN DOBLE RESOLUCION

         DO IG=1,NGRID
          GG(IG)=Z(IG)*GR
         ENDDO
         
!         print*,"antes de DOBLA"
         CALL DOBLA(SLAT,SLON,RLAT,RLON,NLAT,NLON,GG,GD)
!         print*,"despues de DOBLA"
!
!       print*,"despues de dobla"
!       print*,"RLT=",RLT(1:50) 
!       print*,"RLN",RLN(1:50) 
!
! CALCULO DEL VIENTO GEOSTROFICO 
        JS=0
        DO 17 J=1,NGRID
         
!  SI NO PERTENECE A LA VENTANA DE SALIDA, SALTA
         IF(RLT(J).GT.SLATS.OR.RLT(J).LT.SLATS+((NLATS-1)*RLATS))cycle
!         print*,"entro en el primer IF",j,RLT(J),SLATS,SLATS+((NLATS-1)&
!      *RLATS) 
!         print*,"entro en el primer IF, j, RLT =",j,RLT(1:50)
!         stop
          
!         cycle
!         endif 
         IF(RLN(J).LT.SLONS.OR.RLN(J).GT.SLONS+((NLONS-1)*RLONS))cycle 
!         print*,"entro en el segundo IF",j 
!         cycle
!         endif 

         JS=JS+1
!         print*,"JS=",JS

         JD=((MOD(J-1,NLON)+1)*2)-1+(((((J-1)/NLON)*2)+1-1)*NLOND)
!   POS EN DOBLE= PTO EN ESA LAT +     NUM LATD PASADAS* NLOND
         U(JS)=-(GD(JD-NLOND)-GD(JD+NLOND))/(RLY*F(J))
         V(JS)=(GD(JD+1)-GD(JD-1))/(RLX(J)*F(J))
         RLTS(JS)=RLT(J)
         RLNS(JS)=RLN(J)
 17     ENDDO
!        
!
!        
      RETURN
      END SUBROUTINE

            SUBROUTINE DOBLA(SLAT,SLON,RLAT,RLON,NLAT,NLON,GG,GD)
!        
!   SLAT, LATITUD DEL LIMITE MERIDIONAL DEL CAMPO DE ENTRADA
!   SLATS, LATITUD DEL LIMITE MERIDIONAL DEL CAMPO DE SALIDA
!   SLON, LONGITUD DEL LIMITE OCCIDENTAL DEL CAMPO DE ENTRADA
!   SLONS, LONGITUD DEL LIMITE OCCIDENTAL DEL CAMPO DE SALIDA
      IMPLICIT INTEGER (K)
      INTEGER,INTENT(IN) ::  NLON,NLAT
!      REAL,INTENT(IN) ::  SLAT,SLON,RLAT,RLON
      DOUBLE PRECISION,INTENT(IN) ::  SLAT,SLON,RLAT,RLON
      REAL,INTENT(IN) ::  GG(NLON*NLAT)
      REAL,INTENT(OUT) :: GD(((NLON*2)-1)*((NLAT*2)-1))
      INTEGER IGA(((NLON*2)-1)*((NLAT*2)-1))
      REAL    A(NLON,NLAT),S(((NLON-2)*2)-1,((NLAT-2)*2)-1)
!     NLON2 y NLAT2 son nlont y nlatt que son NLON y NLAT de 
!     GEOSTROFICO que llama a DOBLA   
      NLON2=NLON
      NLAT2=NLAT
!
!      IF(NLAT.NE.NLAT2 .OR. NLON.NE.NLON2)THEN
!       PRINT*,' CAMBIAR NLAT2 Y NLON2 EN SUBRUTINA DOBLA',&
!         NLAT,NLAT2,NLON,NLON2
!       STOP
!      ENDIF
!        
      XLATMIH=SLAT+(RLAT*(NLAT-1))
      XLONMIH=SLON
      XLATMIR=XLATMIH+ABS(RLAT)
      XLONMIR=XLONMIH+RLON
      DLATH=ABS(RLAT)
      DLONH=ABS(RLON)
      DLATR=ABS(RLAT/2.)
      DLONR=ABS(RLON/2.)
      NLONH=NLON
      NLATH=NLAT
      NLONR=((NLON-2)*2)-1
      NLATR=((NLAT-2)*2)-1
      ICA=0
      DO J=NLAT,1,-1
       DO I=1,NLON
        ICA=ICA+1
        A(I,J)=GG(ICA)
       ENDDO
      ENDDO
      CALL BESSEL(XLATMIH,XLONMIH,XLATMIR,XLONMIR,DLATH,DLONH,&
      DLATR,DLONR,NLONH,NLATH,NLONR,NLATR,A,S)
!        
      NLONS=(NLON*2)-1
      NLATS=(NLAT*2)-1
      IGA=0
      ICA=0
      DO 10 IG=1,NLONS*NLATS
       IF (MOD(((IG-1)/NLONS),2).EQ.1) cycle      !LATITUDES PARES
       IF (MOD(MOD((IG-1),NLONS)+1,2).EQ.0) cycle  !LONGITUDES PARES
       ICA=ICA+1
       IGA(IG)=ICA
  10  ENDDO
! ESCRIBE LOS PUNTOS DE LA REJILLA ORIGINAL
      DO IG=1,NLONS*NLATS
       IF(IGA(IG).NE.0) GD(IG)=GG(IGA(IG))
      ENDDO
!
      DO IG=1,NLONS*NLATS
! SOBREESCRIBE INTERPOLACIONES HECHAS POR BESSEL
       IF(IG.GT.NLONS*2 .AND. IG.LT.NLONS*(NLATS-2) .AND.&
         MOD((IG-1),NLONS)+1.GT.2 .AND. MOD((IG-1),NLONS)+1.LT.NLONS-2)&
         THEN   !INT POR BESSEL, TODAS MENOS LAS DOS PRIMERAS Y ULTIMAS
! LATITUDES Y LONGITUDES
        ILAT=NLATR-((((IG-1)/NLONS)-1)-1)
        ILON=(MOD(IG-1,NLONS)+1)-2
        GD(IG)=S(ILON,ILAT)
! INTERPOLA PARA LA FRONTERA DE LA REJILLA
       ELSE
        IF(IGA(IG).EQ.0)THEN
         IF(MOD(((IG-1)/NLONS),2).EQ.1 .AND.&
           MOD(MOD((IG-1),NLONS)+1,2).EQ.0)THEN     !FILA PAR, COLUMNA PAR, INT
!4 PTOS
         GD(IG)=(GD(IG-NLONS-1)+GD(IG-NLONS+1)+GD(IG+NLONS-1)+&
            GD(IG+NLONS+1))/4.
         ELSEIF(MOD(((IG-1)/NLONS),2).EQ.1)THEN     !FILA PAR, COLUMNA IMPAR,
!INT 2 PTOS
          GD(IG)=(GD(IG-NLONS)+GD(IG+NLONS))/2.
         ELSEIF(MOD(MOD((IG-1),NLONS)+1,2).EQ.0)THEN     !FILA IMPAR, COLUMNA
!PAR, INT 2 PTOS
          GD(IG)=(GD(IG-1)+GD(IG+1))/2.
         ENDIF
        ENDIF
       ENDIF
!
      ENDDO
!
      RETURN
      END SUBROUTINE
!
      SUBROUTINE BESSEL(XLATMIH,XLONMIH,XLATMIR,XLONMIR,DLATH,DLONH,&
      DLATR,DLONR,NLONH,NLATH,NLONR,NLATR,A,E)
!     
      INTEGER,INTENT(IN) :: NLONH,NLATH,NLONR,NLATR
      REAL,INTENT(IN) :: DLONH,DLONR,DLATH,DLATR,XLATMIH,XLATMIR,XLONMIH,XLONMIR
      REAL,INTENT(IN) ::  A(NLONH,NLATH)
      REAL,INTENT(OUT) :: E(NLONR,NLATR)
!
! COMPRUEBA QUE LOS LIMITES SON CORRECTOS
      XLATMAH=XLATMIH+((NLATH-1)*DLATH)
      XLONMAH=XLONMIH+((NLONH-1)*DLONH)
      XLATMAR=XLATMIR+((NLATR-1)*DLATR)
      XLONMAR=XLONMIR+((NLONR-1)*DLONR)
!      IF(XLATMIR.LT.XLATMIH+DLATH .OR. XLONMIR.LT.XLONMIH+DLONH&
!       .OR. XLATMAR.GT.XLATMAH-DLATH .OR. XLONMAR.GT.XLONMAH-DLONH)THEN
!       PRINT*,' ERROR EN LIMITES DE REJILLA ESTIMADA:SLATE,ELATE,SLATS,E&
!       LATS, Y LON RESPECTIVOS:',XLATMIH,XLATMAH,XLATMIR,XLATMAR,&
!        XLONMIH,XLONMAH,XLONMIR,XLONMAR
!       STOP
!      ENDIF
!
! HAZ LA INTERPOLACION PARA CADA PUNTO DE LA REJILLA DE SALIDA
3     DO  J=1,NLATR
       DO  I = 1,NLONR
! DETERMINA LA POSICION DEL PUNTO DE LA REJILLA DE SALIDA EN LAS COORDENADAS DE
! LA REJILLA DE ENTRADA
        XX=  (((XLONMIR+DLONR*(I-1)) - XLONMIH) / DLONH ) +1.
        YY=  (((XLATMIR+DLATR*(J-1)) - XLATMIH) / DLATH ) +1.
        M = XX
        N = YY
        DX = XX - M
        DY = YY - N
!  APLICA EL ESQUEMA DE INTERPOLACI\324N DE 16 PT DE BESSEL
        DXX = .25 *(DX - 1.)
        DYY = .25 *(DY - 1.)
        AA = A(M,N-1) + DX *(A(M+1,N-1) - A(M,N-1) + DXX *&
         (A(MIN(M+2,NLONH),N-1) - A(M+1,N-1) + A(M-1,N-1) - A(M,N-1)))
        AB = A(M,N) + DX*(A(M+1,N) - A(M,N) + DXX *(A(MIN(M+2,NLONH),N)&
         - A(M+1,N) + A(M-1,N) - A(M,N)))
        AC = A(M,N+1) + DX *(A(M+1,N+1) - A(M,N+1) + DXX *&
         (A(MIN(M+2,NLONH),N+1) - A(M+1,N+1) + A(M-1,N+1) - A(M,N+1)))
        AD = A(M,MIN(N+2,NLATH)) + DX *(A(M+1,MIN(N+2,NLATH)) -&
         A(M,MIN(N+2,NLATH)) + DXX *(A(MIN(M+2,NLONH),MIN(N+2,NLATH))&
         - A(M+1,MIN(N+2,NLATH)) + A(M-1,MIN(N+2,NLATH)) -&
         A(M,MIN(N+2,NLATH))))
        E(I,J) = AB + DY *(AC - AB + DYY *(AD - AC + AA - AB))
       ENDDO
      ENDDO
      RETURN
      END SUBROUTINE

      subroutine radian(t1,t2,t3,sol)
      implicit real(a-h,o-z)
!      implicit none 
!      double precision (a-h,o-z)
      pi=3.14159265358979d0
      sol=t1+t2/60.d0+t3/3600.d0
      return
      end subroutine

      SUBROUTINE GEOUTM (FLON, FLAT, HUSO, X, Y)
!      IMPLICIT REAL (A-Z)
      IMPLICIT DOUBLE PRECISION (A-Z)
!      IMPLICIT NONE 
!      DOUBLE PRECISION (A-Z)
      PI = 3.14159265
      RG = 180. / PI
      E2 = 0.6722670E-02
      EP2 = 0.6768170E-02
      A0 = 0.998317208055891
      A2 = 5.050503255106305E-03
      A4 = 5.323041134969273E-06
      A6 = 6.981680670962105E-09
      A8 = 9.931708438892222E-12
      A10 = 1.44222427482031E-14
      RA = 6378388.0
      XM = (6. * HUSO - 183.) / RG
      LOI = FLON / RG - XM
      LAT = FLAT / RG
      B = RA*(A0 * LAT - 0.5 * (A2 * SIN(2. * LAT) - A4 * SIN(4. * LAT)&
       + A6*SIN(6. * LAT) - A8 * SIN(8. * LAT) + A10 * SIN(10. * LAT)))
      PSI = LOI * COS(LAT)
      W = SQRT(1. - E2 * (SIN(LAT) ** 2))
      HN = RA / W
      V2 = 1. + EP2 * (COS(LAT) ** 2)
      TF2 = TAN(LAT) ** 2
      C2 = (V2 - TF2) / 6
      C3 = V2 / 24. + V2 ** 2 / 6. - TF2 / 24.
      C4 = (V2 * (14. - 58. * TF2) + 40. * TF2 + TF2 ** 2 - 9.) / 120.
      C5 = (61.- 58.*TF2 + TF2**2 + (V2 - 1.)*(270. - 330.*TF2)) / 720.
      X = 500000. + HN * PSI * (1. + C2 * PSI**2 + C4 * PSI**4)*0.9996
      Y = (B + HN * TAN(LAT) * (0.5 * PSI ** 2 + C3 * PSI ** 4 +&
         C5 * PSI ** 6)) * 0.9996
      RETURN
      END SUBROUTINE

      SUBROUTINE FECHANNO(DIA,MES,IDA)
      INTEGER,INTENT(IN) :: DIA,MES
      INTEGER,INTENT(OUT) :: IDA
      INTEGER NORMAL(12)
      DATA NORMAL/0,31,59,90,120,151,181,212,243,273,304,334/
      IDA=NORMAL(MES)+DIA
      IF(MES.EQ.2 .AND. DIA.GT.28)IDA=60
      RETURN
      END SUBROUTINE
 

END MODULE MOD_FUNCS

