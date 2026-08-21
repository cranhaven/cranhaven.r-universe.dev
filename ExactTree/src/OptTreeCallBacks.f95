MODULE OptTreeCallBacks

!Supplementary routines for OptimalTrees modules
!This module contains all routines that need Matlab callbacks,
!including output routines

INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)

!USE Mex
!USE MexLib

!INTEGER prhsGEH(1),prhsGEHptr

!INTEGER, PRIVATE :: &
!   prhsGEH(1), &
!   prhsGEHptr, &
!   IsInterruptedPtr = 0

CONTAINS

SUBROUTINE SetupCallBacks(N)
! Arguments
INTEGER N, IsInterruptedPtr
!INTEGER IsInterruptedPtr
! Locals
! Begin routine

!prhsGEH(1) = mxCreateNumericMatrix(1, N, mxClassIDFromClassName('int32'), 0)
!prhsGEHptr = mxGetPr(prhsGEH(1))
!IsInterruptedPtr = mexGetVariablePtr('global', 'IsInterrupted')
N=N
IsInterruptedPtr = 0

END SUBROUTINE SetupCallBacks



SUBROUTINE CloseCallBacks(N)
! Arguments
INTEGER N
! Locals
! Begin routine

!CALL mxSetN(prhsGEH(1),N)
!CALL mxSetPr(prhsGEH(1),prhsGEHptr)
!CALL mxDestroyArray(prhsGEH(1))

N=N

END SUBROUTINE CloseCallBacks



REAL(dp) FUNCTION GetExternHeterogeneity(N,ObsIndex)
!USE Mex
! Arguments
INTEGER  N
INTEGER  ObsIndex(N)
! Locals
!INTEGER plhs(1), Status
! Begin routine

!CALL mxSetN(prhsGEH(1),N)
!CALL mxCopyInteger4ToPtr(ObsIndex, prhsGEHptr, N)

!Status = mexCallMATLAB(1, plhs, 1, prhsGEH, 'HeterogeneityCallBack')
!GetExternHeterogeneity = mxGetScalar(plhs(1))

! CLEAN UP
!CALL mxDestroyArray(plhs(1))

N=N
ObsIndex=ObsIndex
GetExternHeterogeneity = 0.0

END FUNCTION GetExternHeterogeneity



LOGICAL FUNCTION GetExternVeryHomogeneous(h,N)
! Arguments
REAL(dp)   h
INTEGER  N
!locals
!INTEGER plhs(1), prhs(2), Status
! Begin routine

!prhs(1) = mxCreateNumericMatrix(1, 1, mxClassIDFromClassName('double'), 0)
!CALL mxCopyReal8ToPtr(h, mxGetPr(prhs(1)), 1)
!prhs(2) = mxCreateNumericMatrix(1, 1, mxClassIDFromClassName('int32'), 0)
!CALL mxCopyInteger4ToPtr(N, mxGetPr(prhs(2)), 1)

!Status = mexCallMATLAB(1, plhs, 2, prhs, 'VeryHomogeneousCallBack')

!IF (mxGetScalar(plhs(1)) > 0) THEN
!   GetExternVeryHomogeneous = .TRUE.
!ELSE
   GetExternVeryHomogeneous = .FALSE.
!END IF

! CLEAN UP
!CALL mxDestroyArray(plhs(1))
!CALL mxDestroyArray(prhs(1))
!CALL mxDestroyArray(prhs(2))

h=h
N=N

END FUNCTION GetExternVeryHomogeneous



SUBROUTINE Warning(Msg)
!USE MexLib
CHARACTER(LEN=*) Msg

!CALL mexWarning(Msg)

Msg=Msg

END SUBROUTINE Warning



SUBROUTINE WriteHeader(OutUnit)
!USE MexLib
!Arguments
INTEGER OutUnit
!Locals
!INTEGER i,s,ss
! Begin routine

!CALL mexPrintFF (OutUnit,'***                O P T I M A L  T R E E                ***'//CHAR(10))
!CALL mexPrintFF (OutUnit,'***GLOBALLY OPTIMIZED CLASSIFICATION AND REGRESSION TREES***'//CHAR(10))
!CALL mexPrintFF (OutUnit,'***               USING DYNAMIC PROGRAMMING              ***'//CHAR(10))
!CALL mexPrintFF (OutUnit,'***               AUTHOR: B.J. Van Os @2005              ***'//CHAR(10))
!CALL mexPrintFF (OutUnit,'************************************************************'//CHAR(10))
OutUnit=OutUnit
END SUBROUTINE WriteHeader



SUBROUTINE WriteInterruptWarning(OutUnit)
!USE MexLib
!Arguments
INTEGER OutUnit
!Locals
! Begin routine

!IF (OutUnit/=0) THEN
!   CALL mexWarning('Computational Process Interrupted and Aborted'//CHAR(10))
!END IF

!CALL mexPrintFF (OutUnit,CHAR(10)//'WARNING: COMPUTATIONAL PROCESS INTERRUPTED AND ABORTED'//CHAR(10))
!CALL mexPrintFF (OutUnit,'SUBSEQUENT OUTPUT CAN NOT BE TRUSTED.'//CHAR(10))
!CALL mexPrintFF (OutUnit,'FOUND TREES HAVE NO GLOBAL OPTIMALITY GUARANTEE:'//CHAR(10))
!CALL mexPrintFF (OutUnit,'TREES REPORTED ARE THE BEST TREES FOUND IN AN INCOMPLETE SEARCH.'//CHAR(10))
!CALL mexPrintFF (OutUnit,''//CHAR(10))
!CALL mexPrintFF (OutUnit,''//CHAR(10))

OutUnit=OutUnit
END SUBROUTINE WriteInterruptWarning



SUBROUTINE EchoInput(NColsY,Measure,NCat)
!USE MexLib
IMPLICIT NONE
!Arguments
!INTEGER OutUnit,NRows,NColsY,NColsX
!REAL(dp) Y(NRows,NColsY), X(NRows,NColsX)
!INTEGER Measure,MaxSize,MaxDepth,MinNodeSize,AlgoType,LookAheadDepth
!REAL(dp) MinH, BoundH, Prior(*), LossM(*)
!Locals
!INTEGER
INTEGER Measure, NColsY
REAL(dp) NCat(NColsY)
INTEGER i,s,ss
!CHARACTER(40) GetAlgoTypeStr
! Begin routine

IF (Measure ==3) THEN
        s=1
        ss=1
        DO i=1,NColsY
          s=s+INT(NCat(i))
          ss=ss+INT(NCat(i)**2)
        END DO
END IF


END SUBROUTINE EchoInput



SUBROUTINE WriteHeap2Allocation(OutUnit,Status,MemCount)
!USE MexLib
!Arguments
INTEGER OutUnit,Status,MemCount
!Locals
! Begin routine

!IF (Status>0) THEN
!   IF (OutUnit/=0) CALL mexWarning('Level 2 Heap allocation failed!'//CHAR(10))
!   CALL mexPrintFF (OutUnit,CHAR(10)//'WARNING: LEVEL 2 HEAP ALLOCATION FAILED'//CHAR(10))
!   CALL mexPrintFF (OutUnit,'LIKELY CAUSE: NOT ENOUGH (CONTIGUOUS) MEMORY'//CHAR(10))
!   CALL mexPrintFF (OutUnit,'LEVEL 2 HEAP NEEDS '//LongI2Str(CEILING(MemCount/(1024**2/4.0)))//' MB OF MEMORY'//CHAR(10))
!   CALL mexPrintFF (OutUnit,'=>PROCESS WILL RUN WITHOUT HEAP'//CHAR(10))
!ELSE
!   CALL mexPrintFF (OutUnit,CHAR(10)//'LEVEL 2 HEAP NEEDED '//LongI2Str(CEILING(MemCount/(1024**2/4.0)))//' MB OF MEMORY'//CHAR(10))
!END IF

OutUnit=OutUnit
Status=Status
MemCount=MemCount

END SUBROUTINE WriteHeap2Allocation



CHARACTER(30) FUNCTION GetMeasureStr(Measure)
! Returns string with algorithm type description
!        0  = regression tree
!        1  = simple misclassification rate
!        2  = Risk function
! Parameter block.
INTEGER Measure
! Local variables.
!CHARACTER(30) Str
! BEGIN FUNCTION

SELECT CASE (iand(Measure,127))
CASE (0)
   GetMeasureStr = ' Regression: SS'
CASE (1)
   GetMeasureStr = ' Classification:misclass.rate'
CASE (2)
   GetMeasureStr = ' Classification:risk function'
END SELECT

END FUNCTION GetMeasureStr





CHARACTER(20) FUNCTION GetMeasureCalculationStr(Measure)
! Returns string with algorithm type description
!        0  = regression tree
!        1  = simple misclassification rate
!        2  = Risk function
! Parameter block.
INTEGER Measure
! BEGIN FUNCTION

IF (iand(Measure,128)>0) THEN
   GetMeasureCalculationStr = ' MATLAB CALLBACK'
ELSE
   GetMeasureCalculationStr = ' DIRECT '
END IF

END FUNCTION GetMeasureCalculationStr



CHARACTER(40) FUNCTION GetAlgoTypeStr(AlgoType)
! Returns string with algorithm type description
!        0  = h only;
!        1  = h+tree table
!        2  = branch and bound, MaxSize only
!        +4 = results for all trees <= size restrictions
!        +8 = sorted predictor splitting (faster for continous predictors?)
!        default = 1
! Parameter block.
INTEGER AlgoType
! Local variables.
CHARACTER(40) Str
! BEGIN FUNCTION

Str = ''
IF (iand(AlgoType,16)>0) THEN
   Str = 'lookahead heuristic'
END IF
IF (iand(AlgoType,8)>0) THEN
   Str = 'sorted'
   !//Str
END IF
IF (iand(AlgoType,4)>0) THEN
   Str = 'all trees, '
   !//Str
END IF
SELECT CASE (iand(AlgoType,3))
CASE (0)
   Str = 'h only, '
   !//Str
CASE (1)
   Str = 'h + tree, '
   !//Str
CASE (2)
   Str = 'h (branch and bound), '
   !//Str
END SELECT

GetAlgoTypeStr = Str

END FUNCTION GetAlgoTypeStr






SUBROUTINE OutputAlgorithmDetails ()
!USE MexLib
IMPLICIT NONE
!Arguments
!INTEGER OutUnit,NTrees,Depth,nS,nN,nST
!INTEGER nNL(0:Depth-1)
!REAL(dp)  hAll(NTrees)
!Locals
!INTEGER i
! Begin routine

   !CALL mexPrintFF(OutUnit,CHAR(10)//CHAR(10)//'ALGORITHM DETAILS'//CHAR(10))
   !CALL mexWriteSpecifiedElapsedTime(OutUnit,'OptTreeF')
   !CALL mexPrintFF(OutUnit,'Remaining heterogeneity check: '//CHAR(10))
   !DO i=1,NTrees
  !    CALL mexPrintFF(OutUnit,'Tree '//I2Str(i)//' = '//R2Str(hAll(i)))
   !END DO

   !CALL mexPrintFF(OutUnit,CHAR(10))
   !CALL mexPrintFF(OutUnit,'Actual Depth:                  '//LongI2Str(Depth)//CHAR(10))
   !CALL mexPrintFF(OutUnit,'Number of splits:              '//LongI2Str(nS)//CHAR(10))
   !CALL mexPrintFF(OutUnit,'Number of nodes:               '//LongI2Str(nN)//CHAR(10))
   !DO i=1,Depth
  !    CALL mexPrintFF(OutUnit,'   Level '//I2Str(i-1)//'                 '//LongI2Str(nNL(i-1))//CHAR(10))
   !END DO
   !CALL mexPrintFF(OutUnit,'Number of subTree searches:    '//LongI2Str(nST)//CHAR(10))


END SUBROUTINE OutputAlgorithmDetails



SUBROUTINE OutputAlgorithmDetailsX (OutUnit,hAll, NTrees, Depth, nS, nN, nNL, nST, nR)
!USE MexLib
IMPLICIT NONE
!Arguments
INTEGER OutUnit,NTrees,Depth,nS,nN,nST,nR
INTEGER nNL(0:Depth-1)
REAL(dp)  hAll(NTrees)
!Locals
!INTEGER i
! Begin routine

   !CALL mexPrintFF(OutUnit,CHAR(10)//CHAR(10)//'ALGORITHM DETAILS'//CHAR(10))
   !CALL mexWriteSpecifiedElapsedTime(OutUnit,'OptTreeF')
   !CALL mexPrintFF(OutUnit,'Remaining heterogeneity check: '//CHAR(10))
   !DO i=1,NTrees
    !  CALL mexPrintFF(OutUnit,'Tree '//I2Str(i)//' = '//R2Str(hAll(i)))
   !END DO

   !CALL mexPrintFF(OutUnit,CHAR(10))
   !CALL mexPrintFF(OutUnit,'Actual Depth:                  '//LongI2Str(Depth)//CHAR(10))
   !CALL mexPrintFF(OutUnit,'Number of splits:              '//LongI2Str(nS)//CHAR(10))
   !CALL mexPrintFF(OutUnit,'Number of nodes:               '//LongI2Str(nN)//CHAR(10))
   !DO i=1,Depth
    !  CALL mexPrintFF(OutUnit,'   Level '//I2Str(i-1)//'                 '//LongI2Str(nNL(i-1))//CHAR(10))
   !END DO
   !CALL mexPrintFF(OutUnit,'Number of subTree searches:    '//LongI2Str(nST)//CHAR(10))
   !CALL mexPrintFF(OutUnit,'Number of redundant level2:    '//LongI2Str(nR)//CHAR(10))

   OutUnit=OutUnit
   hAll=hAll
   NTrees=NTrees
   Depth=Depth
   nS=nS
   nN= nN
   nNL=nNL
   nST=nST
   nR=nR

END SUBROUTINE OutputAlgorithmDetailsX




SUBROUTINE OutputProgressP (OutUnit, Predictor)
!USE MexLib
!USE IFPORT
!Arguments
INTEGER OutUnit, Predictor
! Begin routine

!CALL mexPrintFF (OutUnit, CHAR(10)//'Doing p: '//I2Str(Predictor)//', s:')
IF (OutUnit/=0) CALL FLUSH(OutUnit)
Predictor=Predictor

END SUBROUTINE OutputProgressP



SUBROUTINE OutputProgressS (OutUnit, Split)
!USE MexLib
!USE IFPORT
!Arguments
INTEGER OutUnit, Split
! Begin routine

!CALL mexPrintFF (OutUnit, I2Str(Split))
IF (OutUnit/=0) CALL FLUSH(OutUnit)
Split=Split

END SUBROUTINE OutputProgressS



SUBROUTINE OutputProgress (Predictor,Split)
!USE MexLib
! Arguments
INTEGER  Predictor,Split
!locals
!INTEGER plhs(0), prhs(2), Status
! Begin routine

!prhs(1) = mxCreateNumericMatrix(1, 1, mxClassIDFromClassName('int32'), 0)
!CALL mxCopyInteger4ToPtr(Predictor, mxGetPr(prhs(1)), 1)
!prhs(2) = mxCreateNumericMatrix(1, 1, mxClassIDFromClassName('int32'), 0)
!CALL mxCopyInteger4ToPtr(Split, mxGetPr(prhs(2)), 1)

!Status = mexCallMATLAB(0, plhs, 2, prhs, 'ProgressCallBack')


Predictor=Predictor
Split=Split


! CLEAN UP
!CALL mxDestroyArray(prhs(1))
!CALL mxDestroyArray(prhs(2))
END SUBROUTINE OutputProgress



LOGICAL FUNCTION IsInterrupted()
! Returns whether or not in MATLAB an process interruption flag has been set.
!USE MexLib
! Arguments
!locals
! Begin routine

!IF (-1 > 0) THEN
!   IsInterrupted = .TRUE.
!ELSE
  IsInterrupted = .FALSE.
!END IF

END FUNCTION IsInterrupted



END MODULE OptTreeCallBacks
