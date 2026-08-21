OptTreeGate<-function(defY,X,XType,defMeasure,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,AlgoType=NULL,CVVec=NULL,BoundH=NULL,LookAheadDepth, verbose = TRUE){ #,Description
  # % OLD INTERFACE, RETAINED FOR BCAKWARDS COMPATIBILITY
  #  OptTreeGate(Y,X,Measure,MaxSize,MaxDepth,MinNodeSize,MinH,AlgoType,CVVec)
  # defY<-Y
  # defMeasure<-Measure
  # defMaxSize<-MaxSize
  # defMaxDepth<-MaxDepth
  # defMinNodeSize<-MinNodeSize
  # defMinH<-MinH
  # % USE OptimalTree INSTEAD
  # %
  # %
  # % Find the optimal classification or regression tree
  # % minimizing an additive global objective function
  # % This version gateways to a Fortran dll
  # %
  # % X is expected to have ordered, consecutive categories
  # %   Measure:
  # %     0 = minimize residual Sum of Squared Error (continuous Y)
  # %     1 = minimize Misclassification Rate        (discrete Y)
  # %     3 = minimize the Risk Sum P(A)*R(A)        (discrete Y)
  # %         Measure = scalar or Measure={3,[Prior,[LossM]]}
  # %     if zero: only MaxDepth rstrictions are applied
  # %     if defined: the maximum number of terminal nodes for the best tree
  # %   MaxDepth:
  # %		The maximum number of layers in a tree, MaxDepth=1 means a tree with just
  # %     one node, MaxDepth=2 means one split and two terminal nodes, etc.
  # %     MaxDepth-1 is the maximum number of consecutive splits in the tree.
  # %     This is related to the maximum order of interactions in the tree.
  # %   MinNodeSize:
  # %     defines the minimum number of observations in a terminal node
  # %     if the number of observations in a node is equal to this minimum
  # %     the node becomes a possible terminal node; if the number of observations in a node
  # %     is smaller, the node is set illigal and disregarded.
  # % 	 MinH:
  # %		defines the situation when a node A is considered Very Homogeneous
  # %     and is not allowed to be split further, i.e. the node A becomes a possible terminal node
  # %     (|A| = number of objects in node)
  # %		Measure = 0
  # %			(h/|A|)<minH*(TotalH/n);
  # %			=> minH defines a proportion of the Mean SSE of the unpredicted Y (TotalH/n).
  # %			If the actual MSSE in a node is smaller than this proportion
  # %        of the original MSSE, the node is considered Very Homogeneous
  # %		Measure = 1
  # %			(h/|A|)<minH;
  # %			=> minH is the minimum proportion of misclassification.
  # %        If the actual proportion is smaller, the node is considered Very Homogeneous
  # %		Measure = 3
  # %			h<minH*TotalH;
  # %			=> minH is the minimum proportion of Risk of the unpredicted Y Risk.
  # %        If the actual proportion is smaller, the node is considered Very Homogeneous
  # %   AlgoType: OPTIONAL
  # %        0  = h only;
  # %        1  = h+tree table
  # %        2  = branch and bound, MaxSize only, h only, no other options
  # %        +4 = results for all trees <= size restrictions
  # %        +8 = sorted predictor splitting (faster for continous predictors?)
  # %        +16= heuristic search
  # %        default = 1
  # %        NOTE: currently, not all option combinations are implemented:
  # %           4 can not be combined with 8
  # %           8 can only be used with a MaxDepthTree
  # %     CVVec: OPTIONAL
  # %        specify a vector that assigns all observations to N classes for N-fold cross-validation
  # %        if NCV = 0, no N-fold cross-validation
  # %        if NCV<0, abs(N)-fold cross-validation ONLY (no tree estimation)
  # %        default=0
  # %     BoundH: OPTIONAL
  # %        optional bound for best tree value, when using AlgoType == 2
  # % example:
  # %     [h,T]=OptTreeGate(Y,X,0,0,3,8,0.05);
  # %     gives a SSE tree for continuous dependent variables Y
  # %     with no size constraint, MaxDepth=3, MinNodeSize=8, MinH=0.05
  # %
  # % preprocessing:
  # %	 examples of reading data: ReadDataFile?.m
  # %   then use
  # %     [Y,X,Desc]=SelectVar(Data,Names,[11],[4 6 8 9 10]);
  # %   to create the neccesary discretized Predictors and dependent variable(s)
  # %   where Data and Names are the arrays created with `ReadDataFile?.m'
  # %   and the first (index) vector selects the dependent variables (=columns of Data),
  # %   the second (index) vector selects the predictor variables (=columns of Data).
  # %
  # % postprocessing:
  # %   use
  # %     genTexTree(T,Desc)
  # %   to produce a TeX tree (if neccesary, do twice)



  #define globals
  #global LogFileName Description LookAheadDepth m n measure maxLevel maxSize minNodeSize minH somethingLarge nS nN depth Prior LossM ClassSizes SWHandle IsInterrupted CurrentCV;


  # LookAheadDepth<-globalenv()$LookAheadDepth
  # LogFileName<-globalenv()$LogFileName
  # Description<-globalenv()$Description

  if(!exists('XType') || is.null(XType)){
    XType<-rep(0, NCOL(X))

  }



  if(!exists('LookAheadDepth') || is.null(LookAheadDepth)){
    LookAheadDepth <- 0
  }


  # SWHandle<<-c()
  # IsInterrupted <<- FALSE
  CurrentCV<-0
  somethingLarge<-2E10
  Depth_var<-0
  nS<-0
  nN<-0
  m<-dim(X)[2]
  n<-dim(X)[1]

  if((defMaxDepth==0) && (defMaxSize>0)){
    defMaxDepth <- defMaxSize
  }

  if(defMaxSize>(2^(defMaxDepth-1))){
    defMaxSize<-2^(defMaxDepth-1) # Prevent redundant heap
    warning('The MaxSize has been reduced because it exceeds the actual Depth constraint')

    # print('Warning: The MaxSize has been reduced because it exceeds the actual Depth constraint')
  }

  maxSize<-defMaxSize
  maxLevel<-defMaxDepth-1
  minNodeSize<-defMinNodeSize
  minH<-defMinH

  #% Start LogFile

  #%check whether defMeasure includes a Prior and Loss (Measure=3)
  SMeasOut<-SetupMeasure(defMeasure)
  measure<-SMeasOut$measure
  measureCB<-SMeasOut$measureCB
  Prior<-SMeasOut$Prior
  LossM<-SMeasOut$LossM

  #init Y: autorecode for discrete data (measure>1)
  Y <- SetupY(defY,measure)

  #if undefined setup default Prior and LossM (Measure=3)
  SRMeasOut<-SetupRiskMeasure(Y,measure,Prior,LossM)
  Prior<-SRMeasOut$Prior
  LossM<-SRMeasOut$LossM
  ClassSizes<-SRMeasOut$ClassSizes

  UP<-UnravelCells(Prior) #UP = Unraveled Prior
  UL<-UnravelCells(LossM) #UP = Unraveled Loss matrix

  #%setup globals for `Heterogeneity' and `VeryHomogeneous' callbacks
  #SetupCallBacks(Y,X,measureCB,measure, Prior, LossM, ClassSizes)

  # get optional arguments
  if(is.null(BoundH)){
    BoundH<-GetHeterogeneity(Y=Y, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
  }

  if(is.null(AlgoType)){
        AlgoType <- 1
  }


  #Define NCV, the number of cross-validations
  if(is.null(CVVec)){
    NCV<-0
  }else{
    NCV<-max(CVVec)
  }


  tic<-Sys.time()

  #SWHandle <<- OptTreeStatus_('Init',0,varargin=list(max(X)-1,LogFileName,Description,NCV))#MISSING!!! DIFFICULT FUNCTION

  #%call Fortran function
  OptTreeFout<-OptTreeF(Y,X,XType,measureCB,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,
                        AlgoType,BoundH,Prior=UP, LossM=UL,LookAheadDepth)

  h<-OptTreeFout$h
  Tree<-OptTreeFout$Tv
  hAll<-matrix(OptTreeFout$hAll,nrow = 1)
  TAll<-OptTreeFout$TAll


  #output table with h for all trees
  NTrees <- NCOL(hAll)#dim(hAll)[2]
  if(verbose){
    fprintf(fmt='\n%s\n','APPARENT ERROR')
    fprintf(fmt='%s\n','Size      Error     Rel.Error')
  }
  maxH<-GetHeterogeneity(Y=Y, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
  for(t in 1:NTrees){
    Size <- MaxEndNodes(t,NTrees,defMaxSize,defMaxDepth)
    if(verbose){
      fprintf(fmt='%i %13.4f %13.4f\n',Size,hAll[t],hAll[t]/maxH)
    }
  }

  #return
  #T(1,1)

  Tree<-as.matrix(Tree)
  TAllList<-list()
  IndicesList<-list()

  if(Tree[1,1] > 0){
    #tables are available, recalculate everything in MATLAB and
    #show the results for each tree

    for(t in 1:NTrees){
      #do postprocessing stuff in MATLAB
      Tv <- TAll[,,t]
      ProcessT <- ProcessTree(Y,X,Tv,1,1,measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes) #5th argument: boolean check that signals check of fortran output
      Tv <- ProcessT$Tv
      IndicesList[[t]]<-ProcessT$Indices
      TAllList[[t]] <- Tv
      # if(verbose){
      #   fprintf(fmt='\n%s','POST-PROCESSED RESULTS FOR TREE OF ')
      #   if(maxSize==0){
      #     fprintf(fmt='MAX DEPTH %2i\n',(maxLevel)-(NTrees-t)) #Old: fprintf(fmt='MAX DEPTH %2i\n',(maxLevel+1)-(NTrees-t))
      #   }else{
      #     fprintf(fmt='MAX SIZE %2i MAX DEPTH %2i\n',maxSize-(NTrees-t),maxLevel) #Old: fprintf(fmt='MAX SIZE %2i MAX DEPTH %2i\n',maxSize-(NTrees-t),maxLevel+1)
      #   }
      # }
      #DisplayTreeResults(fid,Tv,Y,X,['DisplayNodes,DisplayMSpace']);
      #DTR<-DisplayTreeResults(defT=Tv,Y=Y,X=X,DisplayOptions='DisplayNodes', m=m, measure=measure, Prior= Prior, LossM= LossM, ClassSizes=ClassSizes)
    }
    Tree<-as.matrix(TAllList[[NTrees]])
  }



  IsInterrupted<-FALSE

  CVoutput<-c()

  if((!is.null(CVVec))){ #&&(!IsInterrupted)){
    if(measure!=0){
      CVoutput<-NULL
      warning('No CV results are returned. CROSSVALIDATED ERROR IS NOT YET CALCULATED FOR CLASSIFICATION TREES (MEASURE=1 or 3)')
    }else{
      CVoutput<-CVOptTreeF(CVVec,Y,X,XType,measureCB,defMaxSize,defMaxDepth,defMinNodeSize,defMinH,AlgoType,BoundH,UP,UL, IsInterrupted, LookAheadDepth, measure, Prior, LossM, ClassSizes)
    }
  }




  time<-Sys.time()-tic

  if(verbose){
    cat(paste0("Time: ",time))
  }

  return(list(h=h,Tree=Tree,hAll=hAll,TAll=TAll,TAll_list=TAllList, CVOutput=CVoutput, Indices=IndicesList))

}






