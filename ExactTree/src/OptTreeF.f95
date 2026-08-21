SUBROUTINE mainFunction(Y,X,Measure,MaxSize,MaxDepth,MinNodeSize,MinH, &
                     AlgoType,BoundH,LookAheadDepth, Prior, LossM, MY, NY, MX, NX, &
                     TAll,hAll,h,Tv, TreeTableSize, NTrees, warn, NMaxNodes, XType)

USE OptTreeCallBacks, ONLY: dp
IMPLICIT NONE
! Arguments
INTEGER measure,MaxSize,MaxDepth,MinNodeSize, &
                     AlgoType,LookAheadDepth,warn, &
                     MY, NY, MX, NX, NTrees, TreeTableSize, NMaxNodes
INTEGER XType(NX)   ! 0=ordinal/continuous, 1=categorical
! Locals
INTEGER,PARAMETER:: LogFileNameLen=255
CHARACTER(LEN=LogFileNameLen) LogFileName
!INTEGER TAllPtr
REAL(dp) TAll(NMaxNodes,5,NTrees)
REAL(dp) MinH, BoundH, Prior(*), LossM(*), h, hAll(NTrees), Tv(NMaxNodes,5), X(MX,NX), Y(MY,NY)
!INTEGER i

LogFileName='default.log'


CALL OptTree (TRIM(LogFileName),MY, NY, Y, NX, X, XType, &
              Measure, MaxSize, MaxDepth, MinNodeSize, MinH, &
              AlgoType, BoundH, LookAheadDepth, Prior, LossM, &
              h, hAll, NTrees, TAll, TreeTableSize, NMaxNodes, warn)


Tv = TAll(1:NMaxNodes,1:5,NTrees) ! T = copy of the (largest) tree


END SUBROUTINE mainFunction


SUBROUTINE TransposeCopyMaxSizeTree(Size,TreeTable,Tree)
!If moved to OptimalTrees module, %VAL statement fails
USE OptTreeCallBacks, ONLY: dp
IMPLICIT NONE
! Arguments
INTEGER Size
REAL(dp) TreeTable(Size,5),Tree(5,Size)
! Begin routine

TreeTable=TRANSPOSE(Tree)

END SUBROUTINE TransposeCopyMaxSizeTree



SUBROUTINE OptTree (LogFileName,NRows,NColsY,Y,NColsX,X,XType,Measure,MaxSize,MaxDepth,MinNodeSize,MinH, &
                    AlgoType, BoundH, LookAheadDepth, Prior, LossM,  &
                    Heterogeneity, hAll, NTrees, TAll, TreeTableSize, NMaxNodes, warn)
USE OptTreeCallBacks
USE OptimalTrees
!USE IFPORT
IMPLICIT NONE
!! Arguments
CHARACTER(LEN=*) LogFileName
INTEGER NRows,NColsY,NColsX,NTrees,TreeTableSize, warn
REAL(dp) Y(NRows,NColsY), X(NRows,NColsX), NCat(NColsY)
INTEGER XType(NColsX)   ! 0=ordinal/continuous, 1=categorical
INTEGER Measure,MaxSize,MaxDepth,MinNodeSize,AlgoType,LookAheadDepth
REAL(dp) MinH, BoundH,Prior(*),LossM(*)
REAL(dp) Heterogeneity, h, hAll(NTrees)
!!Locals
INTEGER OutUnit,i, NMaxNodes
REAL(dp)  Tree(NTrees,TreeTableSize)
REAL(dp) TAll(NMaxNodes,5,NTrees)
!! Begin routine

NCat = MAXVAL(Y,DIM=1)

!Setup output (MATLAB display or file)
IF (LogFileName/='') THEN
   OutUnit = 2
   !OPEN(OutUnit,FILE=LogFileName,IOSTAT=Status,SHARED)
   !OPEN(OutUnit,FILE=LogFileName,ACCESS='APPEND',IOSTAT=Status)
   !IF (Status /= 0) THEN
   !  warn = 10 !CALL mexWarning('Unable to open output file '//LogFileName//CHAR(10))
   !   OutUnit = 0 !MATLAB display
   !END IF
ELSE
   OutUnit = 0 !MATLAB display
END IF



!! start output and show input parameters
!!CALL WriteHeader(OutUnit)
CALL EchoInput(NColsY,Measure,NCat)


IF (OutUnit/=0) CALL FLUSH(OutUnit)


! execute optimal tree algorithm
CALL ComputeMaxTree(OutUnit,NRows,NColsY,Y,NColsX,X,XType,Measure,MaxSize,MaxDepth,MinNodeSize,MinH, &
                    AlgoType, BoundH, LookAheadDepth, Prior,LossM, &
                    hAll, NTrees, Tree, TreeTableSize, warn)

!IF (IsInterrupted()) CALL WriteInterruptWarning(OutUnit)

! process returned data
Heterogeneity = hAll(1)
h = Heterogeneity

!      !Matlab expects TreeTable in transposed form
DO i=1,NTrees
   CALL TransposeCopyMaxSizeTree(TreeTableSize/5,TAll(1:NMaxNodes,1:5,i),Tree(i,1:TreeTableSize))
END DO

IF (OutUnit/=0) warn=45!CLOSE(OutUnit) !close external file

END SUBROUTINE OptTree

