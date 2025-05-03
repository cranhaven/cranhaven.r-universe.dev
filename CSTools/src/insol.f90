! The insol program calculates insolation of 'nd' period

SUBROUTINE insolation(nd,day,month,insol)

USE MOD_CSTS
USE MOD_FUNCS, ONLY : fechanno

      IMPLICIT NONE
  
      INTEGER, INTENT(IN) :: nd
      INTEGER, INTENT(IN) :: day(nd)
      INTEGER, INTENT(IN) :: month(nd)

      DOUBLE PRECISION, INTENT(OUT) :: insol(nd)
!      REAL, INTENT(OUT) :: insol(nd)

      integer i,ida,ida2
      integer dd,mm
      real aaa 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!      print*,'program 3: insolation'

       do 1000 i=1,nd
        dd=day(i)
        mm=month(i)
        call fechanno(dd,mm,ida)

        ida2=ida-80
        if(ida2.le.0) ida2=ida2+365
        aaa=2.*pi*float(ida2)/365.
        insol(i)=sin(aaa)
 1000  continue

END SUBROUTINE insolation


