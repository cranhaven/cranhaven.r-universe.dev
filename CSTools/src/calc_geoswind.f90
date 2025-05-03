
! Program to calculate the geostrophic wind based on mean
! sea level pressure (msl) and sea level density

SUBROUTINE geos(ic,nd,id,slat,slon,slats,slons,rlat,&
                rlon,rlats,rlons,nlat,nlon,nlats,nlons,den,msl_lr,&
                ngridd,um,vm)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

USE MOD_CSTS
USE MOD_FUNCS, ONLY : geostrofico,dobla,bessel      
   
      Implicit none 

      INTEGER, INTENT(IN) :: nd
      INTEGER, INTENT(IN) :: ic
      INTEGER, INTENT(IN) :: id

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
      REAL, INTENT(IN) :: den(nd,ic)
      DOUBLE PRECISION, INTENT(IN) :: msl_lr(nd,id)
      INTEGER, INTENT(IN) :: ngridd
      DOUBLE PRECISION, INTENT(OUT) :: um(nd,ic)
      DOUBLE PRECISION, INTENT(OUT) :: vm(nd,ic)

      integer i
      real psl(id),umint(ic),vmint(ic)
      character for10*50

!      print*,"program 2: geostrophic wind"

      do 1000 i=1,nd

!      Pressure data
       psl(:)=msl_lr(i,:)*100./g

!      Calculate the geostrophic wind components (umint,vmint)
       call geostrofico(psl,umint,vmint,id,ic,slat,slon,slats,slons,&
                   rlat,rlon,rlats,rlons,nlat,nlon,nlats,nlons,ngridd)

!       do j=1,ic
!        um(i,j)=umint(j)/den(i,j)
!        vm(i,j)=vmint(j)/den(i,j)
!       enddo

        um(i,:)=umint(:)/den(i,:)
        vm(i,:)=vmint(:)/den(i,:)

 1000 continue

END SUBROUTINE geos

