MODULE OptimalTrees

!USE Timer
USE OptTreeCallBacks

IMPLICIT NONE

! Definition of TreeTable:
! each row a node, columns:
!     (predictor, split, left node index, right node index, heterogeneity in node)
! nodes are indexed with their row positions
! for MaxDepth trees, nodes have an absolute index
! for MaxSize  trees, nodes have a relative index: the number of rows from the current row


INTEGER,PARAMETER :: &
   cESS      = 0, &
   cClassErr = 1, &
   cMinRisk  = 3

REAL(dp),PARAMETER :: &
   SomethingLarge = 2.0E20

REAL(dp),ALLOCATABLE,PRIVATE :: &
   YR(:,:), &
   Prior(:), &
   LossM(:)

INTEGER,ALLOCATABLE,PRIVATE :: &
   MSpace(:,:), &
   YI(:,:), &
   MaxYI(:), &
   ClassFreqY(:), &
   MaxS(:), &
   nNL(:), &
   Path(:,:), &
   PredType(:), &   ! 0=ordinal/continuous, 1=categorical
   NCatX(:)         ! number of categories per predictor (categorical only)

! Heap structure def for level=2
TYPE TPPHeap
    REAL(dp), POINTER :: Seg(:,:,:)
END TYPE TPPHeap
TYPE (TPPHeap), PRIVATE, ALLOCATABLE :: PP(:,:)


INTEGER,PRIVATE :: &
   GOutUnit     = 0, &
   NDep         = 0, &
   NPred        = 0, &
   NObs         = 0, &
   Measure      = 0, &
   MinNodeSize  = 0, &
   MaxLevel     = 0, &
   MaxILevel    = 0, &
   LookAheadDepth =0,&
   Depth        = 0, &
   MaxMaxYI     = 0, &
   NTotLCells   = 0, &
   NTotClass    = 0
!, &
!   CurP         = 0

LOGICAL,PRIVATE :: &
   CallBack = .FALSE., &
   Level2Heap = .FALSE.

INTEGER, PRIVATE :: &
   nS           = 0, &
   nN           = 0, &
   nST          = 0, &
   nR           = 0

REAL(dp),PRIVATE :: &
   MinH         = 0.d0, &
   BoundH       = 0.d0, &
   normMSS      = 0.d0, &
   normRisk     = 0.d0

CONTAINS



SUBROUTINE SetupLevel2Heap(OutUnit,X,NRows,NColsX,MaxSize,AlgoType)
! Allocate Heap structure for Level=2
! Level 3 HeapSize: n=# predictoren, c cats, MaxSize= 8 => MaxSize 4 left
! ((2*c)^3*(nchoosek(11,3))*4)/1024^2 +  ! 3 pred combi
! (n*c^2*4)/1024^2                    ! 3 x same pred
! (2*c*c^2*n*(n-1)*4)/1024^2          ! 2 x same * pred combi
! =>((2*c)^3*(nchoosek(11,3))*4)/1024^2 + (n*c^2*4)/1024^2 + (2*c*c^2*n*(n-1)*4)/1024^2
! x 4 bytes!
! Arguments
INTEGER OutUnit,NRows,NColsX,MaxSize,AlgoType
REAL(dp) X(NRows,NColsX)
! Locals
INTEGER i, j, CumStatus, Status, MemCount
! Begin routine


   Level2Heap = .FALSE.
   IF (MaxSize/=0 .AND. MaxLevel>=2 .AND. (iand(AlgoType,4)>0) .AND. (iand(AlgoType, 3)==0)) THEN
      IF (.NOT. ALLOCATED(MaxS)) ALLOCATE(MaxS(NColsX), STAT=CumStatus)
      MaxS=INT(MAXVAL(X, DIM=1))-1 !CHANGED

      !Heap structure for Level=2
      Status=1
      IF (.NOT. ALLOCATED(PP)) ALLOCATE(PP(NColsX,NColsX), STAT=Status)
      CumStatus=CumStatus+Status
      IF (CumStatus==0) THEN
         MemCount = (NColsX+1)*NColsX
         DO i=1,NColsX
            DO j=i+1,NColsX
               ALLOCATE(PP(i,j)%Seg(2*MaxS(i),2*MaxS(j),MaxSize-3), STAT=Status) !2:MaxSize-1
               MemCount = MemCount + 2*MaxS(i)*2*MaxS(j)*(MaxSize-3)
               CumStatus=CumStatus+Status
               IF (Status==0) THEN
                  PP(i,j)%Seg=-1.0
                  PP(j,i)%Seg=>PP(i,j)%Seg
               END IF
            END DO
            ALLOCATE(PP(i,i)%Seg(MaxS(i)+1,MaxS(i)+1,MaxSize-3), STAT=Status) !2:MaxSize-1
            MemCount = MemCount + (MaxS(i)+1)*(MaxS(i)+1)*(MaxSize-3)
            CumStatus=CumStatus+Status
            IF (Status==0) PP(i,i)%Seg=-1.0
         END DO
         IF (CumStatus==0) Level2Heap = .TRUE.
         CALL WriteHeap2Allocation(OutUnit,CumStatus,MemCount)
      END IF
   END IF

END SUBROUTINE SetupLevel2Heap



SUBROUTINE DeleteLevel2Heap
! Locals
INTEGER i, j
! Begin routine

   IF (ALLOCATED(PP)) THEN
      DO i=1,NPred
         !CALL mexPrintFF(OutUnit,CHAR(10))
         !CALL mexWriteR4M(OutUnit,PP(i,i)%Seg,MaxS(i)+1,MaxS(i)+1)
         IF (ASSOCIATED(PP(i,i)%Seg)) DEALLOCATE(PP(i,i)%Seg)
         DO j=i+1,NPred
            !CALL mexPrintFF(OutUnit,CHAR(10))
            !CALL mexWriteR4M(OutUnit,PP(i,j)%Seg,2*MaxS(i),2*MaxS(j))
            NULLIFY(PP(j,i)%Seg)
            IF (ASSOCIATED(PP(i,j)%Seg)) DEALLOCATE(PP(i,j)%Seg)
         END DO
      END DO
      DEALLOCATE (PP)
   END IF
   IF (ALLOCATED(MaxS)) DEALLOCATE (MaxS)

END SUBROUTINE DeleteLevel2Heap





SUBROUTINE ComputeMaxTree (OutUnit,NRows, NColsY, Y, NColsX, X, XType, AMeasure, MaxSize, AMaxDepth, &
                           AMinNodeSize, AMinH, AlgoType, ABoundH, ALookAheadDepth, APrior, ALossM, &
                           Heterogeneity, NTrees, Tree, TreeTableSize, warn)
! Main routine that calls different variants of the recursive tree routines
! Setsup all private, module global variables to avoid argument passing overhead
! Arguments
INTEGER OutUnit,NRows,NColsY,NColsX,NTrees,TreeTableSize, warn
REAL(dp) Y(NRows,NColsY), X(NRows,NColsX)
INTEGER XType(NColsX)   ! 0=ordinal/continuous, 1=categorical
INTEGER AMeasure,MaxSize,AMaxDepth,AMinNodeSize,AlgoType,ALookAheadDepth
REAL(dp) AMinH,ABoundH,APrior(*),ALossM(*)
REAL(dp) Heterogeneity(NTrees), Tree(NTrees,TreeTableSize)
! Locals
INTEGER ObsIndex(NRows), i, TreeSize(NTrees)
!j,Pred(NColsY)
!REAL(dp)  Time
LOGICAL, PARAMETER :: All = .TRUE.
! Begin routine


! Set Global Module Variables

   GOutUnit     = OutUnit
   NDep         = NColsY
   NPred        = NColsX
   NObs         = NRows
   Measure      = iand(AMeasure,127)
   CallBack     = iand(AMeasure,128)>0
   MinNodeSize  = AMinNodeSize
   MaxLevel     = AMaxDepth-1
   MaxILevel    = MAX(0,MaxLevel-3)
   LookAheadDepth = ALookAheadDepth
   MinH         = AMinH
   BoundH       = ABoundH
   Depth        = 0
   nS           = 0
   nN           = 0
   IF (.NOT. ALLOCATED(nNL)) ALLOCATE(nNL(0:MaxLevel))
   nNL          = 0
   nST          = 0
   nR           = 0
   IF (.NOT. ALLOCATED(Path)) ALLOCATE(Path(MaxLevel+1,2))
   Path = 0

   CALL SetupLevel2Heap(OutUnit,X,NRows,NColsX,MaxSize,AlgoType)

   CALL SetupCallBacks(NRows)

   ! Set Locals
   ! initialize output in case they are not defined
   Tree         = -1
   TreeSize     = 2**(1+MaxLevel)-1
   Heterogeneity= 0

   ObsIndex     = (/(i,i=1,NRows)/)

   !If the dll was aborted, variables may still be allocated
   IF (.NOT. ALLOCATED(MSpace)) ALLOCATE (MSpace(NRows,NColsX))
   MSpace = INT(X) !CHANGED

   ! Setup predictor type globals
   IF (.NOT. ALLOCATED(PredType)) ALLOCATE(PredType(NColsX))
   IF (.NOT. ALLOCATED(NCatX))    ALLOCATE(NCatX(NColsX))
   PredType = XType
   NCatX    = 0
   DO i=1,NColsX
      IF (XType(i) == 1) NCatX(i) = INT(MAXVAL(X(:,i)))
   END DO

   ! Setup Measure type globals
   IF (Measure == cESS) THEN
      IF (.NOT. ALLOCATED(YR)) ALLOCATE(YR(NRows,NColsY))
      YR = Y
      normMSS = MinH * GetHeterogeneity(NRows,ObsIndex)/NRows
   ELSE IF (Measure == cClassErr) THEN
      IF (.NOT. ALLOCATED(YI)) ALLOCATE(YI(NRows,NColsY))
      IF (.NOT. ALLOCATED(MaxYI)) ALLOCATE(MaxYI(NColsY))
      YI = INT(Y)
      MaxYI = MAXVAL(YI)
      MaxMaxYI = MAXVAL(MaxYI)
   ELSE IF (Measure == cMinRisk) THEN
      IF (.NOT. ALLOCATED(YI)) ALLOCATE(YI(NRows,NColsY))
      IF (.NOT. ALLOCATED(MaxYI)) ALLOCATE(MaxYI(NColsY))
      YI = INT(Y)
      MaxYI = MAXVAL(YI)
      MaxMaxYI = MAXVAL(MaxYI)
      NTotClass = SUM(MaxYI)
      NTotLCells = SUM(MaxYI**2)
      IF (.NOT. ALLOCATED(Prior)) ALLOCATE(Prior(NTotClass))
      IF (.NOT. ALLOCATED(LossM)) ALLOCATE(LossM(NTotLCells))
      IF (.NOT. ALLOCATED(ClassFreqY)) ALLOCATE(ClassFreqY(NTotClass))
      Prior(1:NTotClass) = APrior(1:NTotClass)
      LossM(1:NTotLCells) = ALossM(1:NTotLCells)
      CALL GetClassFreq(NRows,ObsIndex,ClassFreqY)
      normRisk = minH * GetHeterogeneity(NRows,ObsIndex)
   ELSE
      normMSS = MinH * GetHeterogeneity(NRows,ObsIndex)/NRows
   END IF



   !CALL SetTimer(Time) ! Dummy argument here
   ! Choose and execute the correct algorithm variant
   IF (MaxSize == 0) THEN
      SELECT CASE (iand(AlgoType,3))
      CASE (0)
         IF (iand(AlgoType,8)>0) THEN
            CALL OptMaxDepthTreeSortP (0, NRows, ObsIndex, Heterogeneity(1))
         ELSE IF (iand(AlgoType,4)>0) THEN
            CALL OptMaxDepthTreeAll (0, NRows, ObsIndex, Heterogeneity)
         ELSE
            CALL OptMaxDepthTree (0, NRows, ObsIndex, Heterogeneity(1))
         END IF
      CASE (1)
         IF (iand(AlgoType,16)>0) THEN
            CALL OptMaxDepthTreeT (0, NRows, ObsIndex, Heterogeneity(1), Tree, 0)
            !CALL OptMaxDepthTreeTLAH (0, NRows, ObsIndex, Heterogeneity(1), Tree, 0)
         ELSE IF (iand(AlgoType,8)>0) THEN
            CALL OptMaxDepthTreeTSortP (0, NRows, ObsIndex, Heterogeneity(1), Tree, 0)
         ELSE IF (iand(AlgoType,4)>0) THEN
            CALL OptMaxDepthTreeTAll (0, NRows, ObsIndex, Heterogeneity, Tree, 0)
         ELSE
            CALL OptMaxDepthTreeT (0, NRows, ObsIndex, Heterogeneity(1), Tree, 0)
         END IF
      CASE (2)
         warn = 1 !CALL Warning('Branch and bound not implemented for MaxDepth tree, default used.')
         CALL OptMaxDepthTree (0, NRows, ObsIndex, Heterogeneity(1))
      END SELECT
   ELSE
      IF (AMaxDepth>MaxSize) MaxLevel = MaxSize-1
      SELECT CASE (iand(AlgoType,3))
      CASE (0)
         IF (iand(AlgoType,8)>0) THEN
            warn=2 !CALL Warning('predictor sorting not implemented for MaxSize tree.')
         ELSE IF (iand(AlgoType,4)>0) THEN
            IF (Level2Heap) THEN
               CALL OptMaxSizeTreeAllTest (0, NRows, ObsIndex, MaxSize, Heterogeneity)
            ELSE
               CALL OptMaxSizeTreeAll (0, NRows, ObsIndex, MaxSize, Heterogeneity)
            END IF
         ELSE
            CALL OptMaxSizeTree (0, NRows, ObsIndex, MaxSize, Heterogeneity(1))
         END IF
      CASE (1)
         IF (iand(AlgoType,8)>0) THEN
            warn = 3!CALL Warning('predictor sorting not implemented for MaxSize tree.')
         ELSE IF (iand(AlgoType, 4)>0) THEN
            CALL OptMaxSizeTreeTAll (0, NRows, ObsIndex, MaxSize, &
                                     Heterogeneity, Tree, TreeSize)
         ELSE
            CALL OptMaxSizeTreeT (0, NRows, ObsIndex, MaxSize, &
                                  Heterogeneity(1), Tree, TreeSize(1))
         END IF
      CASE (2)
         CALL OptMaxSizeTreeBB (0, NRows, ObsIndex, MaxSize, Heterogeneity(1))
      END SELECT
   END IF

   !CALL GetTimer(Time) ! Dummy argument here
   !CALL OutputAlgorithmDetails (OutUnit,Heterogeneity, NTrees, Depth, nS, nN, nNL, nST)
   CALL OutputAlgorithmDetailsX (OutUnit,Heterogeneity, NTrees, Depth, nS, nN, nNL, nST, nR)

   DEALLOCATE (MSpace)
   DEALLOCATE (PredType, NCatX)
   CALL DeleteLevel2Heap
   DEALLOCATE (Path)
   DEALLOCATE (nNL)

   IF (Measure == cESS) THEN
      DEALLOCATE(YR)
   ELSE IF (Measure == cClassErr) THEN
      DEALLOCATE(YI,MaxYI)
   ELSE IF (Measure == cMinRisk) THEN
      DEALLOCATE(YI,MaxYI)
      DEALLOCATE(Prior,LossM,ClassFreqY)
   END IF

   CALL CloseCallBacks(NRows)

END SUBROUTINE ComputeMaxTree





SUBROUTINE TransposeCopyMaxSizeTreeX(Size,TreeTable,Tree)
! Arguments
INTEGER Size
REAL(dp) TreeTable(Size,5),Tree(5,Size)
! Begin routine

TreeTable=TRANSPOSE(Tree)

END SUBROUTINE TransposeCopyMaxSizeTreeX





RECURSIVE SUBROUTINE OptMaxDepthTree (Level, N, ObsIndex, h)
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
REAL(dp), INTENT(OUT) :: h
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
REAL(dp)  h1, h2, hs
! Begin routine

nN = nN +1
CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            IF (N2>N1) THEN !Reverse N2>N1= more nodes, less work
               CALL OptMaxDepthTree (Level+1, N1, ObsIndex1, h1)
               !IF (h1<h) THEN
                  CALL OptMaxDepthTree (Level+1, N2, ObsIndex2, h2)
                  IF (h1+h2<h) THEN ! Better split found
                     h = h1 + h2
                  END IF
               !END IF
            ELSE
               CALL OptMaxDepthTree (Level+1, N2, ObsIndex2, h2)
               !IF (h2<h) THEN
                  CALL OptMaxDepthTree (Level+1, N1, ObsIndex1, h1)
                  IF (h1+h2<h) THEN ! Better split found
                     h = h1 + h2
                  END IF
               !END IF
            END IF
            IF (Level == 0) THEN
               hs=h1+h2
               if (hs>BoundH) hs=BoundH
            END IF
         END IF
      END DO
   END DO
END IF

END SUBROUTINE OptMaxDepthTree




RECURSIVE SUBROUTINE OptMaxDepthTreeSortP (Level, N, ObsIndex, h)
! This version uses sorted predictors to find splits.
! So far, this a little bit slower, due to the sorting neccesary.
! Presumably, for continuous predictors, this is faster,
! because sorting is needed only once, and many splits occur for these predictors.
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
REAL(dp), INTENT(OUT) :: h
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
REAL(dp)  h1, h2
!integer i
! Begin routine

nN = nN +1
CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      CALL SetupValidNextSplit(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
      DO WHILE (GetValidNextSplit(s, p, N, N1, N2, ObsIndex1))
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         nS = nS + 1
         IF (N2>N1) THEN !Reverse N2>N1= more nodes, less work
            CALL OptMaxDepthTreeSortP (Level+1, N1, ObsIndex1, h1)
            IF (h1<h) THEN
               CALL OptMaxDepthTreeSortP (Level+1, N2, ObsIndex2, h2)
               IF (h1+h2<h) THEN ! Better split found
                  h = h1 + h2
               END IF
            END IF
         ELSE
            CALL OptMaxDepthTreeSortP (Level+1, N2, ObsIndex2, h2)
            IF (h2<h) THEN
               CALL OptMaxDepthTreeSortP (Level+1, N1, ObsIndex1, h1)
               IF (h1+h2<h) THEN ! Better split found
                  h = h1 + h2
               END IF
            END IF
         END IF
      END DO
   END DO

END IF

END SUBROUTINE OptMaxDepthTreeSortP




RECURSIVE SUBROUTINE OptMaxDepthTreeT (Level, N, ObsIndex, h, Tree, LastNode)
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return both optimal objective function value AND optimal tree structure.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: LastNode
REAL(dp), INTENT(OUT) :: h, Tree(5,2**(1+MaxLevel-Level)-1)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N), ts, ts12
REAL(dp)  h1, h2
REAL(dp) Tree1(5,2**(MaxLevel-Level)-1),Tree2(5,2**(MaxLevel-Level)-1)
! Begin routine

nN = nN +1
ts=2**(1+MaxLevel-Level)-1
ts12=2**(MaxLevel-Level)-1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)
!in case none of the possible splits turn out to be successfull
!define endnode
Tree(1:5,1) = (/0.d0, 0.d0, 0.d0, 0.d0, h /)
Tree(1,2:ts) = -1 ! Signal unused nodes

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) THEN
      h = SomethingLarge
      Tree(5,1) = h
   END IF
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            IF (N2>N1) THEN !Reverse N2>N1= turns out to be less work
               CALL OptMaxDepthTreeT (Level+1, N1, ObsIndex1, h1, Tree1,LastNode+1)
            ELSE
               CALL OptMaxDepthTreeT (Level+1, N2, ObsIndex2, h1, Tree2,LastNode+1+ts12)
            END IF
            IF (h1<h) THEN
               IF (N2>N1) THEN
                  CALL OptMaxDepthTreeT (Level+1, N2, ObsIndex2, h2, Tree2, LastNode+1+ts12)
               ELSE
                  CALL OptMaxDepthTreeT (Level+1, N1, ObsIndex1, h2, Tree1, LastNode+1)
               END IF
               IF (h1+h2<h) THEN ! Better split found
                  h = h1 + h2
                  Tree(1:4,1) = (/p, s, 1, 1+ts12/)
                  Tree(5,1) =  h
                  Tree(1:5,2:1+ts12) = Tree1(1:5,1:ts12)
                  Tree(1:5,2+ts12:ts) = Tree2(1:5,1:ts12)
               END IF
            END IF
         END IF
      END DO
   END DO
END IF

END SUBROUTINE OptMaxDepthTreeT




RECURSIVE SUBROUTINE OptMaxDepthTreeTSortP (Level, N, ObsIndex, h, Tree, LastNode)
! This version uses sorted predictors to find splits.
! So far, this is a little bit slower, due to the sorting neccesary.
! Presumably, for continuous predictors, this is faster,
! because sorting is needed only once, and many splits occur for these predictors.
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return both optimal objective function value AND optimal tree structure.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: LastNode
REAL(dp), INTENT(OUT) :: h, Tree(5,2**(1+MaxLevel-Level)-1)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N), ts, ts12
REAL(dp)  h1, h2
REAL(dp) Tree1(5,2**(MaxLevel-Level)-1),Tree2(5,2**(MaxLevel-Level)-1)
! Begin routine

nN = nN +1
ts=2**(1+MaxLevel-Level)-1
ts12=2**(MaxLevel-Level)-1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)
!in case none of the possible splits turn out to be successfull
!define endnode
Tree(1:5,1) = (/0.d0, 0.d0, 0.d0, 0.d0, h /)
Tree(1,2:ts) = -1 ! Signal unused nodes

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize)  THEN
      h = SomethingLarge
      Tree(5,1) = h
   END IF
ELSE

   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      CALL SetupValidNextSplit(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
      DO WHILE (GetValidNextSplit(s, p, N, N1, N2, ObsIndex1))
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         nS = nS + 1
         IF (N2>N1) THEN !Reverse N2>N1= turns out to be less work
            CALL OptMaxDepthTreeTSortP (Level+1, N1, ObsIndex1, h1, Tree1,LastNode+1)
         ELSE
            CALL OptMaxDepthTreeTSortP (Level+1, N2, ObsIndex2, h1, Tree2,LastNode+1+ts12)
         END IF
         IF (h1<h) THEN
            IF (N2>N1) THEN
               CALL OptMaxDepthTreeTSortP (Level+1, N2, ObsIndex2, h2, Tree2, LastNode+1+ts12)
            ELSE
               CALL OptMaxDepthTreeTSortP (Level+1, N1, ObsIndex1, h2, Tree1, LastNode+1)
            END IF
            IF (h1+h2<h) THEN ! Better split found
               h = h1 + h2
               Tree(1:4,1) = (/p, s, 1, 1+ts12/)
               Tree(5,1) =  h
               Tree(1:5,2:1+ts12) = Tree1(1:5,1:ts12)
               Tree(1:5,2+ts12:ts) = Tree2(1:5,1:ts12)
            END IF
         END IF
      END DO
   END DO
END IF

END SUBROUTINE OptMaxDepthTreeTSortP



RECURSIVE SUBROUTINE OptMaxDepthTreeAll (Level, N, ObsIndex, h)
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
REAL(dp), INTENT(OUT) :: h(1+MaxLevel-Level)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, Size, N1, N2, ObsIndex1(N), ObsIndex2(N)
REAL(dp)  h1(MaxLevel-Level), h2(MaxLevel-Level)
! Begin routine

nN = nN +1
CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex) !<=all

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h(1),N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            CALL OptMaxDepthTreeAll (Level+1, N1, ObsIndex1, h1)
            CALL OptMaxDepthTreeAll (Level+1, N2, ObsIndex2, h2)
            DO Size=2,1+MaxLevel-Level
               IF (h1(Size-1)+h2(Size-1)<h(Size)) THEN ! Better split found
                  h(Size) = h1(Size-1) + h2(Size-1)
               END IF
            END DO
         END IF
      END DO
   END DO

   ! Propagate best values for smaller trees
   DO Size=2,1+MaxLevel-Level
      h(Size) = MINVAL(h(1:Size))
   END DO

END IF

END SUBROUTINE OptMaxDepthTreeAll




RECURSIVE SUBROUTINE OptMaxDepthTreeTAll (Level, N, ObsIndex, h, Tree, LastNode)
! Find optimal tree with maximal depth restrictions only (no size restriction).
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N, LastNode
INTEGER ObsIndex(N)
REAL(dp), INTENT(OUT) :: h(1+MaxLevel-Level)
REAL(dp), INTENT(OUT) :: Tree(1+MaxLevel-Level,5,2**(1+MaxLevel-Level)-1)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, Size, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER ts, ts12, NTrees
REAL(dp)  h1(MaxLevel-Level), h2(MaxLevel-Level)
REAL(dp) Tree1(MaxLevel-Level,5,2**(MaxLevel-Level)-1), &
       Tree2(MaxLevel-Level,5,2**(MaxLevel-Level)-1)
! Begin routine

nN = nN +1
NTrees = 1+MaxLevel-Level
ts=2**(1+MaxLevel-Level)-1
ts12=2**(MaxLevel-Level)-1


CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex) !<=all
!in case none of the possible splits turn out to be successfull
!define endnode
Tree(1:NTrees,5,1) = h
Tree(1:NTrees,1:4,1) = 0.d0
Tree(1:NTrees,1,2:ts) = -1 ! Signal unused nodes

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h(1),N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
   Tree(1:NTrees,5,1) = h
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            CALL OptMaxDepthTreeTAll (Level+1, N1, ObsIndex1, h1, Tree1, LastNode+1)
            CALL OptMaxDepthTreeTAll (Level+1, N2, ObsIndex2, h2, Tree2, LastNode+1+ts12)
            DO Size=2,1+MaxLevel-Level
               IF (h1(Size-1)+h2(Size-1)<h(Size)) THEN ! Better split found
                  ts=2**Size-1
                  ts12=2**(Size-1)-1
                  h(Size) = h1(Size-1) + h2(Size-1)
                  Tree(Size,1:4,1) = (/p, s, 1, 1+ts12/)
                  Tree(Size,5,1) =  h(Size)
                  Tree(Size,1:5,2:1+ts12) = Tree1(Size-1,1:5,1:ts12)
                  Tree(Size,1:5,2+ts12:ts) = Tree2(Size-1,1:5,1:ts12)
               END IF
            END DO
         END IF
      END DO
   END DO

   ! Propagate best values for smaller trees
   DO Size=2,1+MaxLevel-Level
      IF (h(Size-1) <= h(Size)) THEN
         h(Size) = h(Size-1)
         Tree(Size,1:5,1:ts) = Tree(Size-1,1:5,1:ts)
      END IF
   END DO

END IF

END SUBROUTINE OptMaxDepthTreeTAll



RECURSIVE SUBROUTINE OptMaxSizeTree (Level, N, ObsIndex, MaxSize, h)
! Find optimal tree with maximal depth AND maximum size restrictions.
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: MaxSize
REAL(dp), INTENT(OUT) :: h
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER SubMaxSize, MaxSubMaxSize, MaxMaxSize
REAL(dp)  h1, h2
LOGICAL SplitFound
! Begin routine

nN = nN +1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)

IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   SplitFound = .FALSE.
   IF (Level == 0) h = BoundH
   MaxSubMaxSize = MIN(MaxSize-1,MaxNLeaves(MaxLevel-Level))
   MaxMaxSize = MIN(MaxSize,MaxNLeaves(1+MaxLevel-Level))

   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level == 0) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            DO SubMaxSize=MaxSubMaxSize,MaxMaxSize-MaxSubMaxSize,-1
               IF (N1>N2) THEN
                  CALL OptMaxSizeTree (Level+1, N1, ObsIndex1, SubMaxSize, &
                                         h1)
               ELSE
                  CALL OptMaxSizeTree (Level+1, N2, ObsIndex2, SubMaxSize, &
                                         h1)
               END IF
               IF (h1<h) THEN
                  nST = nST +1
                  IF (N1>N2) THEN
                     CALL OptMaxSizeTree (Level+1, N2, ObsIndex2, MaxSize-SubMaxSize, &
                                           h2)
                  ELSE
                     CALL OptMaxSizeTree (Level+1, N1, ObsIndex1, MaxSize-SubMaxSize, &
                                           h2)
                  END IF
                  IF (h1+h2<h) THEN ! Better split found
                     SplitFound = .TRUE.
                     h = h1 + h2
                  END IF
               ELSE
                  EXIT
               END IF
            END DO
         END IF
      END DO
   END DO
   IF ((level==0) .AND. .NOT. SplitFound) h = GetHeterogeneity (N,ObsIndex)
END IF

END SUBROUTINE OptMaxSizeTree




RECURSIVE SUBROUTINE OptMaxSizeTreeT (Level, N, ObsIndex, MaxSize, h, Tree, TreeSize)
! Find optimal tree with maximal depth AND maximum size restrictions.
! Return BOTH optimal objective function value AND optimal tree structure(s)
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: MaxSize
REAL(dp), INTENT(OUT) :: h, Tree(5,1+2*(MaxSize-1))
INTEGER, INTENT(OUT) :: TreeSize
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER SubMaxSize, MaxSubMaxSize, MaxMaxSize, Tree1Size, Tree2Size
REAL(dp)  h1, h2
REAL(dp) Tree1(5,1+2*(MaxSize-2)),Tree2(5,1+2*(MaxSize-2))
LOGICAL SplitFound
!,IsI
! Begin routine

nN = nN +1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)
! Node needs to be defined allways in case none of the splits improves
Tree(1:5,1) = (/0.d0, 0.d0, 0.d0, 0.d0, h /)
Tree(1,2:1+2*(MaxSize-1)) = -1 ! Signal unused nodes
TreeSize = 1

IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) THEN
      h = SomethingLarge
      Tree(5,1) = h
   END IF
ELSE
   SplitFound = .FALSE.
   IF (Level == 0) h = BoundH
   MaxSubMaxSize = MIN(MaxSize-1,MaxNLeaves(MaxLevel-Level))
   MaxMaxSize = MIN(MaxSize,MaxNLeaves(1+MaxLevel-Level))

   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            DO SubMaxSize=MaxSubMaxSize,MaxMaxSize-MaxSubMaxSize,-1
            !DO SubMaxSize=MaxSize-1,1,-1
               IF (N1>N2) THEN
                  CALL OptMaxSizeTreeT (Level+1, N1, ObsIndex1, SubMaxSize, &
                                         h1, Tree1,Tree1Size)
               ELSE
                  CALL OptMaxSizeTreeT (Level+1, N2, ObsIndex2, SubMaxSize, &
                                         h1, Tree2,Tree2Size)
               END IF
               IF (h1<h) THEN
                  nST = nST +1
                  IF (N1>N2) THEN
                     CALL OptMaxSizeTreeT (Level+1, N2, ObsIndex2, MaxSize-SubMaxSize, &
                                           h2, Tree2, Tree2Size)
                  ELSE
                     CALL OptMaxSizeTreeT (Level+1, N1, ObsIndex1, MaxSize-SubMaxSize, &
                                           h2, Tree1, Tree1Size)
                  END IF
                  IF (h1+h2<h) THEN ! Better split found
                     SplitFound = .TRUE.
                     h = h1 + h2
                     TreeSize=1+Tree1Size+Tree2Size
                     Tree(1:4,1) = (/p, s, 1, 1+Tree1Size/)
                     Tree(5,1) =  h
                     Tree(1:5,2:1+Tree1Size) = Tree1(1:5,1:Tree1Size)
                     Tree(1:5,2+Tree1Size:TreeSize) = Tree2(1:5,1:Tree2Size)
                  END IF
               ELSE
                  EXIT
               END IF
            END DO
         END IF
      END DO
   END DO
   Tree(1,TreeSize+1:1+2*(MaxSize-1)) = -1 ! Signal unused nodes of worse but larger previously trees
   IF ((level==0) .AND. .NOT. SplitFound) h = GetHeterogeneity (N,ObsIndex)
END IF

END SUBROUTINE OptMaxSizeTreeT




RECURSIVE SUBROUTINE GetOptMaxDepthTreeBound (Level, N, ObsIndex, h)
! Find optimal (sub)tree with maximal depth restrictions only (no size restriction).
! Return the optimal objective function value,
! TO BE USED AS A BOUND for the OptMaxSizeTreeBB
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
REAL(dp), INTENT(OUT) :: h
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
REAL(dp)  h1, h2
! Begin routine

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)

IF ((Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            IF (N2>N1) THEN !Reverse N2>N1= more nodes, less work
               CALL GetOptMaxDepthTreeBound (Level+1, N1, ObsIndex1, h1)
               IF (h1<h) THEN
                  CALL GetOptMaxDepthTreeBound (Level+1, N2, ObsIndex2, h2)
                  IF (h1+h2<h) THEN ! Better split found
                     h = h1 + h2
                  END IF
               END IF
            ELSE
               CALL GetOptMaxDepthTreeBound (Level+1, N2, ObsIndex2, h2)
               IF (h2<h) THEN
                  CALL GetOptMaxDepthTreeBound (Level+1, N1, ObsIndex1, h1)
                  IF (h1+h2<h) THEN ! Better split found
                     h = h1 + h2
                  END IF
               END IF
            END IF
         END IF
      END DO
   END DO

END IF

END SUBROUTINE GetOptMaxDepthTreeBound



RECURSIVE SUBROUTINE OptMaxSizeTreeBB (Level, N, ObsIndex, MaxSize, h)
! Find optimal tree with maximal depth AND maximum size restrictions
! Include a Branch and Bound strategy:
!     the tree search is bounded by a search strategy using the maximum depth only.
! Return ONLY optimal objective function value.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: MaxSize
REAL(dp), INTENT(OUT) :: h
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER SubMaxSize, MaxSubMaxSize,MaxMaxSize
REAL(dp)  h1, h2
LOGICAL SplitFound
! Begin routine

nN = nN +1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex)


IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h,N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   SplitFound = .FALSE.
   IF (Level == 0) THEN
      h = BoundH
!   ELSE
!      CALL GetOptMaxDepthTreeBound (Level+1, N, ObsIndex, h)
   END IF
   MaxSubMaxSize = MIN(MaxSize-1,MaxNLeaves(MaxLevel-Level))
   MaxMaxSize = MIN(MaxSize,MaxNLeaves(1+MaxLevel-Level))

   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
!            IF (Level > MaxLevel-3) THEN
!            IF (Level == 0) THEN
            IF (Level < MaxLevel-1) THEN
               CALL GetOptMaxDepthTreeBound (Level+1, N1, ObsIndex1, h1)
            ELSE
               h1 = 0
            END IF
            IF (h1<h) THEN
!             IF (Level > MaxLevel-3) THEN
!             IF (Level == 0) THEN
            IF (Level < MaxLevel-1) THEN
                CALL GetOptMaxDepthTreeBound (Level+1, N2, ObsIndex2, h2)
             ELSE
                h2 = 0
             END IF
             IF (h1+h2<h) THEN
               DO SubMaxSize=MaxSubMaxSize,MaxMaxSize-MaxSubMaxSize,-1
                  IF (N1>N2) THEN
                     CALL OptMaxSizeTreeBB (Level+1, N1, ObsIndex1, SubMaxSize, &
                                            h1)
                  ELSE
                     CALL OptMaxSizeTreeBB (Level+1, N2, ObsIndex2, SubMaxSize, &
                                            h1)
                  END IF
                  IF (h1<h) THEN
                     nST = nST +1
                     IF (N1>N2) THEN
                        CALL OptMaxSizeTreeBB (Level+1, N2, ObsIndex2, MaxSize-SubMaxSize, &
                                              h2)
                     ELSE
                        CALL OptMaxSizeTreeBB (Level+1, N1, ObsIndex1, MaxSize-SubMaxSize, &
                                              h2)
                     END IF
                     IF (h1+h2<h) THEN ! Better split found
                        SplitFound = .TRUE.
                        h = h1 + h2
                     END IF
                  ELSE
                     EXIT
                  END IF
               END DO
             END IF
            END IF
         END IF
      END DO
   END DO
   IF ((level==0) .AND. .NOT. SplitFound) h = GetHeterogeneity (N,ObsIndex)
END IF

END SUBROUTINE OptMaxSizeTreeBB




RECURSIVE SUBROUTINE OptMaxSizeTreeAll (Level, N, ObsIndex, MaxSize, h)
! Find optimal tree with maximal depth AND maximum size restrictions
! Include a Branch and Bound strategy:
!     the tree search is bounded by a search strategy using the maximum depth only.
! Return optimal objective function for all sizes<MaxSize.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: MaxSize
REAL(dp), INTENT(OUT) :: h(MaxSize)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER Size, SubMaxSize
REAL(dp)  h1(MaxSize-1), h2(MaxSize-1)
! Begin routine

nN = nN +1
nNL(Level) = nNL(Level)+1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex) !<= all h

IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h(1),N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   DO p=1,NPred ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            CALL OptMaxSizeTreeAll (Level+1, N1, ObsIndex1, MaxSize-1, h1)
            CALL OptMaxSizeTreeAll (Level+1, N2, ObsIndex2, MaxSize-1, h2)
            DO Size=2,MaxSize
               DO SubMaxSize=Size-1,1,-1
                  IF (h1(SubMaxSize)+h2(Size-SubMaxSize)<h(Size)) THEN ! Better split found
                     h(Size) = h1(SubMaxSize) + h2(Size-SubMaxSize)
                  END IF
               END DO
            END DO
         END IF
      END DO
   END DO

   ! Propagate best values for smaller trees
   DO Size=2,MaxSize
      h(Size) = MINVAL(h(1:Size))
   END DO

END IF

END SUBROUTINE OptMaxSizeTreeAll




RECURSIVE SUBROUTINE OptMaxSizeTreeAllTest (Level, N, ObsIndex, MaxSize, h)
!USE MexLib
!
!Deze versie gebruikt 2 level2 heap. Dit is nog experimenteel, eerste resultaten waren niet bemoedigend.
!Dit is waarschijnlijk de enige variant die de level two heap gebruikt.
!
! Find optimal tree with maximal depth AND maximum size restrictions
! Include a Branch and Bound strategy:
!     the tree search is bounded by a search strategy using the maximum depth only.
! Return optimal objective function for all sizes<MaxSize.
! Arguments
INTEGER Level, N
INTEGER ObsIndex(N)
INTEGER, INTENT(IN) :: MaxSize
REAL(dp), INTENT(OUT) :: h(MaxSize)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
INTEGER Size, SubMaxSize
REAL(dp)  h1(MaxSize-1), h2(MaxSize-1)
INTEGER s1, s2
!KeepS,
TYPE (TPPHeap) :: PPHeap
LOGICAL NoSkip
! Begin routine

nN = nN +1
nNL(Level) = nNL(Level)+1
IF (Level==0) Path=0

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex) !<= all h

IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h(1),N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) h = SomethingLarge
ELSE
   NoSkip=.TRUE.
   IF (Level==2) THEN
      PPHeap%Seg=>PP(Path(1,1),Path(2,1))%Seg
      IF (Path(1,1)/=Path(2,1)) THEN
         IF (Path(1,1)<Path(2,1)) THEN
            s1=Path(1,2)
            s2=Path(2,2)
            IF (s1<MaxS(Path(1,1))) s1=MIN(s1,MaxMSpace(Path(1,1)))
            IF (s2<MaxS(Path(2,1))) s2=MIN(s2,MaxMSpace(Path(2,1)))
         ELSE
            s2=Path(1,2)
            s1=Path(2,2)
            IF (s2<MaxS(Path(1,1))) s2=MIN(s2,MaxMSpace(Path(1,1)))
            IF (s1<MaxS(Path(2,1))) s1=MIN(s1,MaxMSpace(Path(2,1)))
         END IF
      ELSE
         PPHeap%Seg=>PP(Path(1,1),Path(1,1))%Seg
         s1=MinMSpace(Path(1,1))
         s2=MaxMSpace(Path(1,1))
      END IF
      IF (PPHeap%Seg(s1,s2,1)>-1.0) THEN
         h(2:MaxSize)=PPHeap%Seg(s1,s2,1:(MaxSize-1))
         nR=nR+1
         NoSkip=.FALSE.
      END IF
   END IF

   IF (NoSkip) THEN
      DO p=1,NPred ! over all predictors
         Path(Level+1,1)=p
         !IF (Level == 0) CALL OutputProgressP (p)
         DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
            IF (Level == 0) CALL OutputProgress (p,s)
            IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

            ! Excute Split
            IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
               nS = nS + 1
               Path(Level+1,2)=s
               CALL OptMaxSizeTreeAllTest (Level+1, N1, ObsIndex1, MaxSize-1, h1)
               Path(Level+1,2)=MaxS(p)+s
               CALL OptMaxSizeTreeAllTest (Level+1, N2, ObsIndex2, MaxSize-1, h2)
               DO Size=2,MaxSize
                  DO SubMaxSize=Size-1,1,-1
                     IF (h1(SubMaxSize)+h2(Size-SubMaxSize)<h(Size)) THEN ! Better split found
                        h(Size) = h1(SubMaxSize) + h2(Size-SubMaxSize)
                     END IF
                  END DO
               END DO
            END IF
         END DO
      END DO

      ! Propagate best values for smaller trees
      DO Size=2,MaxSize
         h(Size) = MINVAL(h(1:Size))
      END DO

      !IF (Level==2) PPHeap%Seg(s1,s2,1:MaxSize-1)=h(2:MaxSize)
      !IF (Level==2) THEN
      !   IF (PPHeap%Seg(s1,s2,1)>-1.0) THEN
      !      IF (ABS(PPHeap%Seg(s1,s2,1)-h(1))>0.1) THEN
      !         DO Size=1,1 !MaxSize
      !            WRITE (GOutUnit, '(5I3,2F15.4," VIOLATED")') Size,Path(1,1),Path(2,1),s1,s2,h1(Size),PPHeap%Seg(s1,s2,Size)
      !         END DO
      !      END IF
      !   END IF
      !   PPHeap%Seg(s1,s2,1:MaxSize)=h(1:MaxSize)
      !END IF

   END IF
END IF

END SUBROUTINE OptMaxSizeTreeAllTest




RECURSIVE SUBROUTINE OptMaxSizeTreeTAll (Level, N, ObsIndex, MaxSize, h, Tree, TreeSize)
! Find optimal tree with maximal depth AND maximum size restrictions
! Include a Branch and Bound strategy:
!     the tree search is bounded by a search strategy using the maximum depth only.
! Return optimal objective function AND tree structures for all sizes<MaxSize.
! Arguments

INTEGER, INTENT(IN)  :: Level, N
INTEGER, INTENT(IN)  :: ObsIndex(N)
INTEGER, INTENT(IN)  :: MaxSize
REAL(dp), INTENT(OUT)  :: h(MaxSize), Tree(MaxSize,5,MaxNNodes(MaxSize))
INTEGER, INTENT(OUT) :: TreeSize(MaxSize)
! Locals
INTEGER MinMSpace(NPred), MaxMSpace(NPred), Dif, MaxNodes
!, MaxSubNodes
INTEGER p, s, N1, N2, ObsIndex1(N), ObsIndex2(N)
!,nnp
INTEGER Size, SubMaxSize, Tree1Size(MaxSize-1), Tree2Size(MaxSize-1)
REAL(dp)  h1(MaxSize-1), h2(MaxSize-1)
REAL(dp)  Tree1(MaxSize-1,5,MaxNNodes(MaxSize-1)),Tree2(MaxSize-1,5,MaxNNodes(MaxSize-1))
!REAL(dp)  ht(MaxSize)
! Begin routine

nN = nN +1

CALL GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
Dif = SUM(MaxMSpace - MinMSpace)
h = GetHeterogeneity (N,ObsIndex) !<= all h
! Node needs to be defined allways in case none of the splits improves
TreeSize = 1
Tree(1:MaxSize,5,1) = h
Tree(1:MaxSize,1:4,1) = 0.d0
MaxNodes = MaxNNodes(MaxSize)

IF ((MaxSize <= 1) .OR. &
    (Dif == 0) .OR. (level == MaxLevel) .OR. (N <= MinNodeSize) .OR. &
    VeryHomogeneous(h(1),N)) THEN
   Depth = MAX(Depth,level+1)
   IF (N < MinNodeSize) THEN
      h = SomethingLarge
      Tree(1:MaxSize,5,1) = h
   END IF
ELSE
   DO p=1,NPred ! over all predictors
   !IF (Level == 0) THEN
   !     nnp=1
   !ELSE
   !     nnp=NPred
   !END IF
   !DO p=1,nnp ! over all predictors
      !IF (Level == 0) CALL OutputProgressP (p)
      DO s=MinMSpace(p),MaxMSpace(p)-1 ! and all possible splits
         IF (Level == 0) CALL OutputProgress (p,s)
         IF ((Level <= MaxILevel) .AND. (IsInterrupted())) RETURN

         ! Excute Split
         IF (GetValidSplitAny(s, p, N, ObsIndex, N1, ObsIndex1, N2, ObsIndex2)) THEN
            nS = nS + 1
            CALL OptMaxSizeTreeTAll (Level+1, N1, ObsIndex1, MaxSize-1, h1, Tree1, Tree1Size)
            CALL OptMaxSizeTreeTAll (Level+1, N2, ObsIndex2, MaxSize-1, h2, Tree2, Tree2Size)
            DO Size=2,MaxSize
    !           ht(Size)= SomethingLarge
               DO SubMaxSize=Size-1,1,-1
    !              IF ((Level == 0) .AND. (h1(SubMaxSize)+h2(Size-SubMaxSize)<ht(Size))) THEN
    !                 ht(Size) = h1(SubMaxSize) + h2(Size-SubMaxSize)
    !              END IF
                  IF (h1(SubMaxSize)+h2(Size-SubMaxSize)<h(Size)) THEN ! Better split found
                     h(Size) = h1(SubMaxSize) + h2(Size-SubMaxSize)
                     TreeSize(Size)=1+Tree1Size(SubMaxSize)+Tree2Size(Size-SubMaxSize)
                     Tree(Size,1:4,1) = (/p, s, 1, 1+Tree1Size(SubMaxSize)/)
                     Tree(Size,5,1) =  h(Size)
                     Tree(Size,1:5,2:1+Tree1Size(SubMaxSize)) = &
                        Tree1(SubMaxSize,1:5,1:Tree1Size(SubMaxSize))
                     Tree(Size,1:5,2+Tree1Size(SubMaxSize):TreeSize(Size)) = &
                        Tree2(Size-SubMaxSize,1:5,1:Tree2Size(Size-SubMaxSize))
                  END IF
               END DO
            END DO
     !       IF (Level == 0) CALL mexWriteRV(0,ht,MaxSize)
         END IF
      END DO
     ! IF (Level == 0) CALL mexPrintFF(0,CHAR(10))
   END DO

   ! Propagate best values for smaller trees
   DO Size=2,MaxSize
      IF (h(Size-1)<= h(Size)) THEN
         h(Size) = h(Size-1)
         TreeSize(Size) = TreeSize(Size-1)
         Tree(Size,1:5,1:MaxNodes) = Tree(Size-1,1:5,1:MaxNodes)
      END IF
      IF (Level == 0) Tree(Size,1:5,TreeSize(Size)+1:MaxNodes) = -1 ! Signal unused nodes
   END DO
END IF

END SUBROUTINE OptMaxSizeTreeTAll






PURE INTEGER FUNCTION MaxNLeaves(NLevels)
! Returns the maximum number of leaves (endnodes) in a (sub)tree
! with depth of NLevels
! Arguments
INTEGER, INTENT(IN) :: NLevels
! Begin routine

   IF (NLevels>0) THEN
      MaxNLeaves = 2**(NLevels-1)
   ELSE
      MaxNLeaves = 0
   END IF

END FUNCTION MaxNLeaves



PURE INTEGER FUNCTION MaxNNodes(MaxSize)
! Returns the maximum number of leaves (endnodes) in a (sub)tree
! with depth of NLevels
! Arguments
INTEGER, INTENT(IN) :: MaxSize
! Begin routine

MaxNNodes = 1+2*(MaxSize-1)

END FUNCTION MaxNNodes



LOGICAL FUNCTION GetValidSplitAny(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
! Dispatches to GetValidSplit or GetValidSplitCat based on PredType(p)
INTEGER  s,p,N,N1,N2
INTEGER  ObsIndex(N),ObsIndex1(N),ObsIndex2(N)
! Begin routine
IF (PredType(p) == 1) THEN
   GetValidSplitAny = GetValidSplitCat(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
ELSE
   GetValidSplitAny = GetValidSplit(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
END IF
END FUNCTION GetValidSplitAny



LOGICAL FUNCTION GetValidSplit(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
! returns the splitted measurement space in ObsIndex1 and 2
! and whether the split s is valid:
!     a. no empty nodes on either side
!     b. the split is not congruent i.e. not redundant w.r.t. other splits
!        (two splits s1,s2 are congruent if {group1}<s1<s2<{group2}
! Arguments
INTEGER  s,p,N,N1,N2
INTEGER  ObsIndex(N),ObsIndex1(N),ObsIndex2(N)
! Locals
INTEGER i
! Begin routine

GetValidSplit = .FALSE.
N1 = 0
N2 = 0
DO i=1,N
   IF (MSpace(ObsIndex(i),p) <= s) THEN
      !The next condition skips redundant splits (factually equal to other splits)
      IF (MSpace(ObsIndex(i),p) == s) GetValidSplit = .TRUE.
      N1 = N1+1
      ObsIndex1(N1)=ObsIndex(i)
   ELSE
      N2 = N2+1
      ObsIndex2(N2)=ObsIndex(i)
   END IF
END DO
IF ((N1 == 0) .OR. (N2 == 0)) GetValidSplit = .FALSE.

END FUNCTION GetValidSplit



LOGICAL FUNCTION GetValidSplitCat(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
! Categorical variant of GetValidSplit.
! s is a bitmask: bit k-1 set means category k goes left.
! Enumerates 1..2^(NCatX(p)-1)-1 bitmasks; complement symmetry handled by caller.
! Valid if both sides non-empty.
! Arguments
INTEGER  s,p,N,N1,N2
INTEGER  ObsIndex(N),ObsIndex1(N),ObsIndex2(N)
! Locals
INTEGER i, cat
! Begin routine

GetValidSplitCat = .FALSE.
N1 = 0
N2 = 0
DO i=1,N
   cat = MSpace(ObsIndex(i),p)              ! category value (1..NCatX(p))
   IF (IAND(s, ISHFT(1,cat-1)) /= 0) THEN  ! bit cat-1 set => goes left
      N1 = N1+1
      ObsIndex1(N1) = ObsIndex(i)
   ELSE
      N2 = N2+1
      ObsIndex2(N2) = ObsIndex(i)
   END IF
END DO
IF ((N1 /= 0) .AND. (N2 /= 0)) GetValidSplitCat = .TRUE.

END FUNCTION GetValidSplitCat



RECURSIVE SUBROUTINE QSortObjIndex(O,l,r,N,p)
! Function    : Quick sort object indices according to predictor p
! Version     : 1.0, November 1996
! Author      : Bart Jan van Os
! Used by     :
! Called by   :
! Parameter block.
INTEGER     l,r,N,p
INTEGER     O(N)
! Local variables.
INTEGER     i,j
INTEGER     x,y
! Program start

  i = l
  j = r
  x = O((l+r)/ 2);
  DO WHILE (i<=j)
    DO WHILE (MSpace(O(i),p)<MSpace(x,p))
       i = i+1
    END DO
    DO WHILE (MSpace(x,p)<MSpace(O(j),p))
       j = j-1
    END DO
    IF (i<=j) THEN !swap i and j
      y = O(i)
      O(i) = O(j)
      O(j) = y
      i = i+1
      j = j-1
    END IF
  END DO
  IF (l<j) CALL QSortObjIndex(O,l,j,N,p)
  IF (i<r) CALL QSortObjIndex(O,i,r,N,p)

END SUBROUTINE QSortObjIndex



SUBROUTINE SetupValidNextSplit(s,p,N,ObsIndex,N1,ObsIndex1,N2,ObsIndex2)
! sets up the function GetValidNextSplit by
! - sorting the ObsIndex according to the predictor
! - set ObsIndex1 to this order
! - set ObsIndex2 to reverse of this order
! - set N1=0
! Arguments
INTEGER  s,p,N,N1,N2
INTEGER  ObsIndex(N),ObsIndex1(N),ObsIndex2(N)
! Begin routine

N2=N2

ObsIndex1=ObsIndex ! crucial, because otherwise recursive routine fails one level back
!CurP=p
CALL QSortObjIndex(ObsIndex1,1,N,N,p)
ObsIndex2=ObsIndex1(N:1:-1)
N1=0
s=0


END SUBROUTINE SetupValidNextSplit



LOGICAL FUNCTION GetValidNextSplit(s,p,N,N1,N2,ObsIndex1)
! returns the splitted measurement space in ObsIndex1 and 2
! this version assumes that observation index is sorted with respect to the predictor
! and whether the split s is valid:
!     a. no empty nodes on either side
!     b. the split is not congruent i.e. not redundant w.r.t. other splits
!        (two splits s1,s2 are congruent if {group1}<s1<s2<{group2}
! Arguments
INTEGER  s,p,N,N1,N2
INTEGER  ObsIndex1(N)
! Locals
!INTEGER i
! Begin routine

GetValidNextSplit = .FALSE.
N1=N1+1
s=MSpace(ObsIndex1(N1),p)
DO WHILE ((N1<N) .AND. (MSpace(ObsIndex1(N1+1),p) == s))
   N1=N1+1
END DO
IF (N1<N) THEN
   GetValidNextSplit = .TRUE.
   N2=N-N1
END IF

END FUNCTION GetValidNextSplit




SUBROUTINE GetMinMaxMSpace(N, ObsIndex, MinMSpace, MaxMSpace)
! Arguments
INTEGER N
INTEGER ObsIndex(N), MinMSpace(NPred), MaxMSpace(NPred)
! Locals
INTEGER p
! Begin routine

MinMSpace = MINVAL(MSpace(ObsIndex,1:NPred),1)
MaxMSpace = MAXVAL(MSpace(ObsIndex,1:NPred),1)

! Override range for categorical predictors: enumerate bitmasks 1..2^(k-1)
! so that the standard DO s=MinMSpace(p),MaxMSpace(p)-1 loop covers 1..2^(k-1)-1
DO p=1,NPred
   IF (PredType(p) == 1) THEN
      MinMSpace(p) = 1
      MaxMSpace(p) = ISHFT(1, NCatX(p)-1)  ! 2^(k-1), loop goes to MaxMSpace-1
   END IF
END DO

END SUBROUTINE GetMinMaxMSpace



SUBROUTINE GetMinMaxMSpaceOri(N, ObsIndex, MinMSpace, MaxMSpace)
! Arguments
INTEGER N
INTEGER ObsIndex(N), MinMSpace(NPred), MaxMSpace(NPred)
! Locals
INTEGER i,p
! Begin routine

MinMSpace = MSpace(ObsIndex(1),1:NPred)
MaxMSpace = MSpace(ObsIndex(1),1:NPred)

DO p=1,NPred
   DO i=2,N
      IF (MSpace(ObsIndex(i),p)>MaxMSpace(p)) THEN
         MaxMSpace(p) = MSpace(ObsIndex(i),p)
      ELSE IF (MSpace(ObsIndex(i),p)<MinMSpace(p)) THEN
         MinMSpace(p) = MSpace(ObsIndex(i),p)
      END IF
   END DO
END DO

END SUBROUTINE GetMinMaxMSpaceOri




LOGICAL FUNCTION VeryHomogeneous(h,N)
! Arguments
REAL(dp)   h
INTEGER  N
! Begin routine

!avoid this external call if possible, extremely slow
!IF (CallBack) THEN
!   VeryHomogeneous = GetExternVeryHomogeneous(h,N)
!ELSE
   SELECT CASE (Measure)
   CASE (cESS)
      VeryHomogeneous = (h/N)<normMSS;
   CASE (cClassErr)
      VeryHomogeneous = (h/N)<minH;
   CASE (cMinRisk)
      VeryHomogeneous = h<normRisk;
   CASE DEFAULT
      VeryHomogeneous = (h/N)<normMSS;
   END SELECT
!END IF

END FUNCTION VeryHomogeneous



REAL(dp) FUNCTION GetHeterogeneity(N,ObsIndex)
! Arguments
INTEGER  N
INTEGER  ObsIndex(N)
! Begin routine

IF (CallBack) THEN
   GetHeterogeneity = GetExternHeterogeneity(N,ObsIndex)
ELSE
   SELECT CASE (Measure)
   CASE (cESS)
      GetHeterogeneity = GetSSE(N,ObsIndex)
   CASE (cClassErr)
      GetHeterogeneity = GetMisclassification(N,ObsIndex)
   CASE (cMinRisk)
      GetHeterogeneity = GetExpectedRisk(N,ObsIndex)
   CASE DEFAULT
      GetHeterogeneity = 0.0
   END SELECT
END IF

END FUNCTION GetHeterogeneity




REAL(dp) FUNCTION GetSSE(N,ObsIndex)
! Arguments
INTEGER  N
INTEGER  ObsIndex(N)
! Locals
INTEGER d,i
REAL(dp)  S,SS,SSError
! Begin routine

SSError=0.0
DO d=1,NDep
   SS = 0.d0
   S=0.d0
   DO i=1,N
      S=S+YR(ObsIndex(i),d)
      SS=SS+YR(ObsIndex(i),d)**2
   END DO
   SSError=SSError+SS-(S**2)/N
END DO

GetSSE = SSError

END FUNCTION GetSSE



INTEGER FUNCTION GetMisclassification(N,ObsIndex)
! Arguments
INTEGER  N
INTEGER  ObsIndex(N)
! Locals
INTEGER d,i, Freq(MaxMaxYI), Error

! Begin routine

Error = 0
DO d=1,NDep
   Freq(1:MaxYI(d)) = 0
   DO i=1,N
      Freq(YI(ObsIndex(i),d)) = Freq(YI(ObsIndex(i),d)) +1
   END DO
   Error = Error + SUM(Freq(1:MaxYI(d))) - MAXVAL(Freq(1:MaxYI(d)))
END DO

GetMisclassification = Error

END FUNCTION GetMisclassification



SUBROUTINE GetClassFreq(N,ObsIndex,AClassFreq)
! Arguments
INTEGER  N
!,MaxClass
INTEGER  ObsIndex(N),AClassFreq(NTotClass)
! Locals
INTEGER d,i,s

! Begin routine
s=0
DO d=1,NDep
   AClassFreq = 0
   DO i=1,N
      AClassFreq(s+YI(ObsIndex(i),d)) = AClassFreq(s+YI(ObsIndex(i),d)) +1
   END DO
   s=s+MaxYI(d)
END DO

END SUBROUTINE GetClassFreq



REAL(dp) FUNCTION GetExpectedRisk(N,ObsIndex)
! Arguments
INTEGER  N
INTEGER  ObsIndex(N)
! Locals
INTEGER d,s,ss
INTEGER  F(NTotClass)
REAL(dp)  ExpectedRisk
! Begin routine

ExpectedRisk=0.0
CALL GetClassFreq(N,ObsIndex,F)

s=1
ss=1
DO d=1,NDep
   ExpectedRisk = ExpectedRisk + &
                 CalculateRiskA(MaxYI(d),F(s),Prior(s),LossM(ss))
END DO

GetExpectedRisk = ExpectedRisk

END FUNCTION GetExpectedRisk



REAL(dp) FUNCTION CalculateRiskA(NCat,F,Prior,LossM)
! Arguments
INTEGER  NCat
INTEGER  F(NCat)
REAL(dp)   Prior(NCat),LossM(NCat,NCat)
! Locals
!INTEGER d,i
REAL(dp)  E(NCat),piA(NCat),pa,ra
! Begin routine

   E=Prior*F/ClassFreqY
   pa=SUM(E);
   piA=E/pa;
   E=MATMUL(piA,LossM)
   ra=MINVAL(E);
   CalculateRiskA=pa*ra;

END FUNCTION CalculateRiskA



END MODULE OptimalTrees


