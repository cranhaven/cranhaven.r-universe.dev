!     !!!!!!!!!!!!!!!
      subroutine training_temp(t1000,msl_si,msl_lr,lon_hr,lat_hr,&
                         ngridd,nlat,nlon,ic,nlatt,nlont,id,slat,&
                         slon,rlat,rlon,slatt,slont,nd,day,month,&
                         um,vm,insol,Vdmin,Vref,ipos)      

!     !!!!!!!!!!!!!!!
!*       0.   DECLARATIONS
!             ------------
! MODULES with constants and functions
use mod_csts
use mod_funcs

implicit none

!!!!!!!!!!!!!!!!!!!!!!!!
! INPUT ARGUMENTS
!!!!!!!!!!!!!!!!!!!!!!!!
integer, intent(in) :: ngridd
!***********************************************
! DOMAIN variables
!************************************************
! sinoptic grid
integer, intent(in) :: nlat
integer, intent(in) :: nlon
integer, intent(in) :: ic
!-----------------------------------------------
! low resolution grid
integer, intent(in) :: nlatt
integer, intent(in) :: nlont
integer, intent(in) :: id
!------------------------------------------------
double precision, intent(in) :: slat
double precision, intent(in) :: slon
double precision, intent(in) :: rlat
double precision, intent(in) :: rlon
!------------------------------------------------
double precision, intent(in) :: slatt
double precision, intent(in) :: slont
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!
! TIME variables
integer, intent(in) :: nd
integer, intent(in) :: day(nd)
integer, intent(in) :: month(nd)
!------------------------------------------------
! Reanlysis fields 
double precision, intent(in) :: t1000(nd,ic)
double precision, intent(in) :: msl_si(nd,ic)
double precision, intent(in) :: msl_lr(nd,id)
!------------------------------------------------
! AEMET high resolution observational dat
double precision, intent(in) :: lon_hr(nptos)
double precision, intent(in) :: lat_hr(nptos)
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!
! OUTPUT ARGUMENTS
!!!!!!!!!!!!!!!!!!!!!!!!
double precision, intent(out) :: um(nd,ic)
double precision, intent(out) :: vm(nd,ic)
double precision, intent(out) :: insol(nd)
!!!!!!!!!!!!!!!!!!!!!!!!
double precision, intent(out) :: Vdmin(nptos,4)
integer, intent(out) :: Vref(nptos,4)
!!!!!!!!!!!!!!!!!!!!!!!!
integer, intent(out) :: ipos

!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INNER FORTRAN VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!------------------------------------------------
real :: den(nd,ic)
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real :: x(ic)
real :: y(ic)
!------------------------------------------------
real :: xcand(nptos)
real :: ycand(nptos)
!------------------------------------------------


!print*,""
!print*,"*** TRAINING PROCESS ***"
!print*,""

call calc_tempes_densi_sealev(ic,nd,msl_si,t1000,den)

call geos(ic,nd,id,slatt,slont,slat,slon,rlat,&
          rlon,rlat,rlon,nlatt,nlont,nlat,nlon,den,msl_lr,ngridd,&
          um,vm)

call insolation(nd,day,month,insol)

call utm_ERA(ic,nlat,nlon,slat,slon,rlat,rlon,x,y)

call utm_obs(lon_hr,lat_hr,xcand,ycand)

call ptos_ref_4(ic,x,y,xcand,ycand,Vdmin,Vref,ipos)

end subroutine

