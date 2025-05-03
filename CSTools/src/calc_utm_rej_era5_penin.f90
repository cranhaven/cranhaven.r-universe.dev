
! The utm_ERA program calculates the Reanalysis UTM coordinates
! with a time zone of 30. 

SUBROUTINE utm_ERA(ic,nlat,nlon,slat,slon,rlat,rlon,x,y)

USE MOD_CSTS
USE MOD_FUNCS

      Implicit none

      INTEGER, INTENT(IN) :: ic

      INTEGER, INTENT(IN) :: nlat
      INTEGER, INTENT(IN) :: nlon
      DOUBLE PRECISION, INTENT(IN) :: slat
      DOUBLE PRECISION, INTENT(IN) :: slon
      DOUBLE PRECISION, INTENT(IN) :: rlat
      DOUBLE PRECISION, INTENT(IN) :: rlon
      REAL, INTENT(OUT) :: x(ic) 
      REAL, INTENT(OUT) :: y(ic)

      integer j
      integer igrad,imin,rseg,igrad1,imin1
      real rseg1
      real rlt(ic),rln(ic)
!      real*8 r1,r2,r3,r4,r5,r6,rad,rad1,xint,yint
      double precision r1,r2,r3,r4,r5,r6,rad,rad1,xint,yint
      
!      print*,"program 4: reanalysis UTM coordinates"

!    Calculation of geostrophic coordinates in each synoptic grid points

      do j=1,ic
       rlt(j)=slat+(((j-1)/nlon)*rlat)
       rln(j)=slon+((mod(j-1,nlon)+1-1)*rlon)
      enddo

!    calculation of UTM coordinates

      do j=1,ic
       rad1=rln(j)
       rad=rlt(j)

       call geoutm(rad1,rad,huso,xint,yint)
       
       x(j)=xint
       y(j)=yint

      enddo

END SUBROUTINE utm_ERA
