 
! The utm_obs program calculates the UTM coordinates of high resolution
! (5km x 5km) observational database created by AEMET (Peral et al., 2017).
!
SUBROUTINE utm_obs(lon_hr,lat_hr,xcand,ycand)

USE MOD_CSTS
USE MOD_FUNCS, ONLY : geoutm

      Implicit none

      DOUBLE PRECISION, INTENT(IN) :: lon_hr(nptos)
      DOUBLE PRECISION, INTENT(IN) :: lat_hr(nptos)
      REAL, INTENT(OUT) :: xcand(nptos)
      REAL, INTENT(OUT) :: ycand(nptos)

      integer n
      integer i
!      real*8 rad,rad1,xint,yint
      double precision rad,rad1,xint,yint
      
!      print*,"program 5: UTM coordinates high resolution observational database"

      n=nptos

        do i=1,n
           rad1=lon_hr(i)
           rad=lat_hr(i)
!    Calculation of UTM coordinates
       call geoutm(rad1,rad,huso,xint,yint)

       xcand(i)=xint
       ycand(i)=yint
 
         enddo

END SUBROUTINE utm_obs

!Peral, C., Navascués, B. and Ramos, P.: Serie de precipitación diaria en
!rejilla con fines climáticos. Nota Técnica nº 24, AEMET,
!http://hdl.handle.net/20.500.11765/7573, 2017.
