
! The ptos_ref_4 program links Reanalysis grid and 
! observed grid using 4 nearest points interpolation
! (bilineal interpolation approach)


SUBROUTINE ptos_ref_4(ic,x,y,xcand,ycand,Vdmin,Vref,ipos)

USE MOD_CSTS

      Implicit none
      
      INTEGER, INTENT(IN) :: ic
      REAL, INTENT(IN) :: x(ic)
      REAL, INTENT(IN) :: y(ic)
      REAL, INTENT(IN) :: xcand(nptos)
      REAL, INTENT(IN) :: ycand(nptos)
      DOUBLE PRECISION, INTENT(OUT) :: Vdmin(nptos,4)
      INTEGER, INTENT(OUT) :: Vref(nptos,4)
      INTEGER, INTENT(OUT) :: ipos

      integer iri
      real dmin
      integer np,i,j,k 
      real xe(nptos),ye(nptos),xr(ic),yr(ic),dis
      real copiaXr(nptos),copiaYr(nptos) 
      integer valores_unicos(nptos+1)
      integer cont

!      print*,"program 6: 4 nearest points of reference"

      np=nptos

      xr=x/1000.
      yr=y/1000.

      xe=xcand/1000.
      ye=ycand/1000.


      valores_unicos=0

      do i=1,np

        copiaXr=xr
        copiaYr=yr

      do cont=1,4 !4 nearest pts loop

       dmin=1600000000.

       do 110 j=1,ic
        dis=(xe(i)-copiaXr(j))**2+(ye(i)-copiaYr(j))**2
        if(dis.lt.dmin) then
         dmin=dis
         iri=j
        endif
 110   continue

       Vdmin(i,cont)=sqrt(dmin)
        if (Vdmin(i,cont) .lt. 0.1) then
            Vdmin(i,cont)=0.1
        endif

       Vref(i,cont)=iri
         copiaXr(iri)=99999999.
         copiaYr(iri)=99999999.


       do k=1,valores_unicos(np+1)
        if(valores_unicos(k).eq.iri) go to 100 
       enddo

       valores_unicos(np+1)=valores_unicos(np+1)+1
       ipos=valores_unicos(np+1)
       valores_unicos(ipos)=iri

 100  continue
      enddo !4 rearest pts loop

      enddo
 
END SUBROUTINE ptos_ref_4

