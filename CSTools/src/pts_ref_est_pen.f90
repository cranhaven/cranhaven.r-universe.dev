
! The ptos_ref program links Reanalysis grid and 
! observed grid using the nearest neighbor

SUBROUTINE ptos_ref(ic,x,y,xcand,ycand,iri,ipos)

USE MOD_CSTS

      Implicit none
      
      INTEGER, INTENT(IN) :: ic
      REAL, INTENT(IN) :: x(ic)
      REAL, INTENT(IN) :: y(ic)
      REAL, INTENT(IN) :: xcand(nptos)
      REAL, INTENT(IN) :: ycand(nptos)
      INTEGER, INTENT(OUT) :: iri(nptos)
      INTEGER, INTENT(OUT) :: ipos

      integer np,i,j,k 
      real xe(nptos),ye(nptos),xr(ic),yr(ic),dis,dmin(nptos)
      integer valores_unicos(nptos+1)

      np=nptos

!      print*,"program 6: reference points"

      xr=x/1000.
      yr=y/1000.

      xe=xcand/1000.
      ye=ycand/1000.

      valores_unicos=0

      do 100 i=1,np

       dmin(i)=1600000000.

       do 110 j=1,ic
        dis=(xe(i)-xr(j))**2+(ye(i)-yr(j))**2
        if(dis.lt.dmin(i)) then
         dmin(i)=dis
         iri(i)=j
        endif
 110   continue

       dmin(i)=sqrt(dmin(i))

       do k=1,valores_unicos(np+1)
        if(valores_unicos(k).eq.iri(i)) go to 100 
       enddo
       valores_unicos(np+1)=valores_unicos(np+1)+1
       ipos=valores_unicos(np+1)
       valores_unicos(ipos)=iri(i)

 100  continue

END SUBROUTINE ptos_ref

