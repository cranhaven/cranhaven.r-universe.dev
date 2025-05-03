
! Program to calculate sea level temperature and density from 1000Mb
! temperature and mean sea level pressure (msl)

SUBROUTINE calc_tempes_densi_sealev(ic,nd,msl_si,t1000,den)

USE MOD_CSTS

     IMPLICIT NONE

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER, INTENT(IN) :: nd
      INTEGER, INTENT(IN) :: ic
      DOUBLE PRECISION, INTENT(IN) :: msl_si(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: t1000(nd,ic)
      REAL, INTENT(OUT) :: den(nd,ic)

      real c,yy
      real psl(ic),tmil(ic),tsl(ic)
      integer i,j

      c=r*a/g

      do i=1,nd
       psl(:)=msl_si(i,:) 
       tmil(:)=t1000(i,:)
       do j=1,ic
        yy=log(tmil(j))-c*log(1000./psl(j))
        tsl(j)=exp(yy)
! Air density equation
        den(i,j)=(psl(j)*100.)/(r*tsl(j))
       enddo
      enddo

END SUBROUTINE calc_tempes_densi_sealev

