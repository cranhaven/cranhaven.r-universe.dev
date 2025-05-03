!     !!!!!!!!!!!!!!!
      subroutine training_part1(u500,v500,t1000,z500,z1000,&
                         msl_si,msl_lr,ngridd,nlat,nlon,ic,nlatt,nlont,&
                         id,slat,slon,rlat,rlon,slatt,slont,nd,&
                         um,vm,insol,gu92,gv92,gu52,gv52,nger)      

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
!------------------------------------------------
! Reanlysis fields 
double precision, intent(in) :: u500(nd,ic)
double precision, intent(in) :: v500(nd,ic)
double precision, intent(in) :: t1000(nd,ic)
double precision, intent(in) :: z500(nd,ic)
double precision, intent(in) :: z1000(nd,ic)
double precision, intent(in) :: msl_si(nd,ic)
double precision, intent(in) :: msl_lr(nd,id)
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!
! OUTPUT ARGUMENTS
!!!!!!!!!!!!!!!!!!!!!!!!
double precision, intent(out) :: um(nd,ic)
double precision, intent(out) :: vm(nd,ic)
double precision, intent(out) :: insol(nd)
!------------------------------------------------
double precision, intent(out) :: gu92(nd,ic)
double precision, intent(out) :: gv92(nd,ic)
double precision, intent(out) :: gu52(nd,ic)
double precision, intent(out) :: gv52(nd,ic)
!------------------------------------------------
integer, intent(out) :: nger
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INNER FORTRAN VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!------------------------------------------------
real :: den(nd,ic)
!------------------------------------------------

!print*,""
!print*,"*** TRAINING PROCESS ***"
!print*,""

call calc_tempes_densi_sealev(ic,nd,msl_si,t1000,den)

call geos(ic,nd,id,slatt,slont,slat,slon,rlat,&
          rlon,rlat,rlon,nlatt,nlont,nlat,nlon,den,msl_lr,ngridd,&
          um,vm)

call clasif(ic,nd,nlon,nlat,slat,slon,rlat,rlon,um,vm,u500,v500,z1000,&
            z500,nger,gu92,gv92,gu52,gv52)

end subroutine

