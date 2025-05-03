!     !!!!!!!!!!!!!!!
      subroutine training_part2(u500,v500,t500,t850,msl_si,q700,&
                         lon_hr,lat_hr,prec_hr,&
                         nanx,nlat,nlon,ic,nlatt,nlont,id,slat,&
                         slon,rlat,rlon,slatt,slont,nd,um,vm,gu92,gv92,&
                         gu52,gv52,nger,Vdmin,Vref,ipos2,new_mi,new_ccm,&
                         new_kvars,new_corrpar)      

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
integer, intent(in) :: nanx
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
double precision, intent(in) :: t500(nd,ic)
double precision, intent(in) :: t850(nd,ic)
double precision, intent(in) :: msl_si(nd,ic)
double precision, intent(in) :: q700(nd,ic)
!------------------------------------------------
! AEMET high resolution observational dat
double precision, intent(in) :: lon_hr(nptos)
double precision, intent(in) :: lat_hr(nptos)
double precision, intent(in) :: prec_hr(nd,nptos)
!------------------------------------------------
double precision, intent(in) :: um(nd,ic)
double precision, intent(in) :: vm(nd,ic)
!------------------------------------------------
integer, intent(in) :: nger
!------------------------------------------------
double precision, intent(in) :: gu92(nger,ic)
double precision, intent(in) :: gv92(nger,ic)
double precision, intent(in) :: gu52(nger,ic)
double precision, intent(in) :: gv52(nger,ic)
!------------------------------------------------
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!
! OUTPUT ARGUMENTS
!!!!!!!!!!!!!!!!!!!!!!!!
integer, intent(out) :: ipos2
!!!!!!!!!!!!!!!!!!!!!!!!
double precision, intent(out) :: Vdmin(nptos,4)
integer, intent(out) :: Vref(nptos,4)
!------------------------------------------------
integer, intent(out) :: new_mi(nger,nptos)
double precision, intent(out) :: new_ccm(nger,nptos)
integer, intent(out) :: new_kvars(nger,nptos,npx)
double precision, intent(out) :: new_corrpar(nger,nptos,npx)
!------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INNER FORTRAN VARIABLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer :: ipos
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real :: x(ic)
real :: y(ic)
!------------------------------------------------
real :: xcand(nptos)
real :: ycand(nptos)
!------------------------------------------------
integer :: iri(nptos)
!------------------------------------------------

call utm_ERA(ic,nlat,nlon,slat,slon,rlat,rlon,x,y)

call utm_obs(lon_hr,lat_hr,xcand,ycand)

call ptos_ref_4(ic,x,y,xcand,ycand,Vdmin,Vref,ipos2)

call ptos_ref(ic,x,y,xcand,ycand,iri,ipos)

call sig_predic(nlat,nlon,nlatt,nlont,slat,slon,rlat,rlon,slatt,&
                slont,nd,ic,id,prec_hr,nger,um,vm,gu92,gv92,gu52,&
                gv52,iri,u500,v500,msl_si,q700,t500,t850,nanx,&
                ipos,new_mi,new_ccm,new_kvars,new_corrpar)

end subroutine

