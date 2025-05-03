!     !!!!!!!!!!!!!!!
      MODULE MOD_CSTS
!     !!!!!!!!!!!!!!!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE

!CHARACTER(LEN=100),SAVE :: pathSI_estim="/home/sclim/cle/regionalizacion/asun/eraint/datosAscii_SI_estim/"
!CHARACTER(LEN=100),SAVE :: pathLR_estim="/home/sclim/cle/regionalizacion/asun/eraint/datosAscii_LR_estim/"
!***********************************************
! CONSTANTS   
!***********************************************
! 
CHARACTER(LEN=5),SAVE :: mt="aemet"
INTEGER,SAVE :: nvar_p=11 ! total number of predictors for precipitation
INTEGER,SAVE :: nvar_t=9 ! total number of predictors for temperature
INTEGER,SAVE :: nps=8 ! number of predictors from the reference grid point of the model
INTEGER, SAVE :: npx=11
REAL,SAVE :: umb=0.75
REAL,SAVE :: umbl=0.75
INTEGER,SAVE :: nmin=10
INTEGER,SAVE :: nmax=30
REAL,SAVE :: ccmi=0.30
!-----------------------------------------------
REAL,SAVE :: r=287.05
REAL,SAVE :: a=0.0065
REAL,SAVE :: g=9.81
REAL,SAVE :: rt=6.37e6
REAL,SAVE :: pi=3.14159265
REAL,SAVE :: romega=7.292E-5
!-----------------------------------------------
INTEGER,SAVE :: umb_ger=10000
!-----------------------------------------------
!REAL,SAVE :: huso=30.
DOUBLE PRECISION,SAVE :: huso=30.
!-----------------------------------------------
REAL,SAVE :: tol=0.020
!
!***********************************************
! DOMAIN 
!************************************************
!------------------------------------------------
! Limits (lat-lon) of strips to assign weights 
!
REAL,SAVE :: fnor1=45.0
REAL,SAVE :: fnor2=47.5
REAL,SAVE :: fsur1=35.0
REAL,SAVE :: fsur2=32.5
REAL,SAVE :: foes1=-12.5
REAL,SAVE :: foes2=-17.5
REAL,SAVE :: fest1=5.0
REAL,SAVE :: fest2=7.5
!------------------------------------------------
! High resolution (5km x 5km) observed AEMET grid
!
! For precipitation: the variable is multiplied to 10, to
! obtain it in decimes of mm, as the program requaire.
INTEGER, PARAMETER :: nptos=20945 ! number of point where make the estimations
!------------------------------------------------
INTEGER,SAVE :: icl=24
INTEGER,SAVE :: ipui=99
INTEGER,SAVE :: ila=4
INTEGER,SAVE :: ilo=6

END MODULE MOD_CSTS

