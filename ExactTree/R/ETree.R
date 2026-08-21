#' Main function of the package. It performs the Exact Tree method.
#'
#' @param formula a description of the model to be fit. The format is \code{Y ~  X1 + \dots + Xn},
#'   where the variable before ~ represents the dependent variable and the variables after
#'   the ~ are the independent variables.
#' @param data Dataset to be analyzed. Note: If data contains ordinal variables, transform them to numerical
#' before using this function. Otherwise, they will be treated as nominal variables.
#' The method for nominal variables is still under development.
#' @param map [ori vars] sets for each variable whether the tree will depict
#'                       thresholds based upon original values (var number), or recoded values (0).
#' @param original [TRUE/FALSE] sets for each variable whether the tree will depict
#'                       thresholds based upon original values (true), or recoded values (false).
#' @param round [factors] autorecoding based upon rounded categories; the categories will
#'                       be round after multiplying with the factor, and devided by the
#'                       factor after rounding
#' @param discretize [ncat] optimal discretization while minimizing SS within
#'                       a group, indicate with a number how many categories;set ncat=0 for each variable that is to
#'                       be left unaltered
#' @param selV list containing the output obtained using \code{\link{SelectVar}}.
#' Use this list instead of the previous inputs if you want to perform SelectVars outside ETree.
#' @param control a list with control parameters as returned by \code{\link{ETree.control}}.
#' @param verbose logical, if TRUE, prints information about the progress of the algorithm.
#'
#'
#'
#' @details The function results in a global tree (for given maxsize and maxdepth) and transforms the output to obtain summary information and plot the tree.
#'
#'
#' @return Returns the following 6 elements:
#'  \item{h}{contains the objective function value (fit) of the best tree.}
#'  \item{Tree}{contains the largest Tree table.}
#'  \item{hAll}{(OPTIONAL) contains the objective function values for all trees, if more than one is requested}
#'  \item{TAll}{(OPTIONAL) contains Tree tables for all trees, if more than one is requested.}
#'  \item{Transf_Trees}{contains an object of class ExactTree that can be used in \code{\link{plot.ETree}} and \code{\link{summary.ETree}}}
#'  \item{CVOutput}{Cross validation results for all the requested trees.}
#'
#'
#' @seealso \code{\link{summary.ETree}}, \code{\link{SelectVar}},
#'   \code{\link{plot.ETree}},\code{\link{ETree.control}},
#'   \code{\link{predict.ETree}},\code{\link{prune.ETree}}
#'
#' @examples
#' \donttest{
#'   data(iris)
#'   # Fit an Exact Tree model
#'   controlEtree <- ETree.control(measure=0, maxsize = 4, maxdepth = 3,
#'   minbucket = 5, ncv=5, alltreesizes = FALSE)
#'   ETree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'   control= controlEtree, data = iris)
#' }
#'
#'
#' @keywords tree
#' @keywords exact
#'
#' @importFrom pracma histc repmat randperm Reshape strcmp isempty fprintf
#' @importFrom DescTools DecToBin BinToDec
#'
#' @export
#' @useDynLib ExactTree, .registration=TRUE
ETree<-function(formula=NULL,data=NULL, map=NULL, original=NULL, round=NULL, discretize=NULL, selV=NULL, control=NULL, verbose=TRUE){

  # library(matlab)
  # library(pracma)


  #We need either formula and data or Y,X and Desc
  if(is.null(selV)){#!is.null(formula)){
    if(is.null(data)){
      stop("Data is missing.")
    }
    if(is.null(formula)){
      stop("formula is missing.")
    }
    outSelVar<-SelectVar(formula=formula,data=data,map=map, original=original, round=round, discretize=discretize, verbose=verbose)
    Y<-outSelVar$Y
    X<-outSelVar$X
    Desc<-outSelVar$Desc
    Data<-outSelVar$DataOrig
    XType<-outSelVar$XType
    Yvarnames<-colnames(Y)
    Xvarnames<-colnames(X)

  }else{

    # if(!is.null(data)){
    #   stop("formula is missing.")
    # }

    if(is.null(selV$Y) | is.null(selV$X) | is.null(selV$Desc) | is.null(selV$DataOrig)){
      stop("If formula is missing, the function requires Y, X, DataOrig and Desc. These variables are obtained using SelectVar()")
    }

    Y<-selV$Y
    X<-selV$X
    Desc<-selV$Desc
    Data<-selV$DataOrig
    XType<-selV$XType

    Yvarnames<-colnames(Y)
    Xvarnames<-colnames(X)

  }


  AlgoType<-1
  CVVec<-c()

  if(is.null(control)){
    stop("Control is missing.")
  }else{
    AlgoType<-control$AlgoType
    Measure<-control$Measure
    MaxSize<-control$MaxSize
    MaxDepth<-control$MaxDepth
    MinBucket<-control$MinBucket
    MinH<-control$MinH
    ncv<-control$ncv
    cvVector<-control$cvVector
    #heterogeneityonly<-control$heterogeneityonly
    BoundH<-control$BoundH
    #branchandbound<-control$branchandbound
    #alltreesizes<-control$alltreesizes
    #sortedpsplits<-control$sortedpsplits
    #lookaheadheuristic<-control$lookaheadheuristic
    LookAheadDepth<-control$LookAheadDepth
  }



  if(!is.null(ncv) && ncv>=1){
    NCV<-ncv
    n<-dim(X)[1]
    CVVec<-matrix(0, nrow = n, ncol = 1)
    P<-randperm(n)
    bs<-trunc(n/NCV) #fix(n/NCV);
    for(c in 1:NCV){
      if(c<NCV){
        Pt<-P[(1+(c-1)*bs):(c*bs)]
      }else{
        Pt<-P[(1+(c-1)*bs):n] #pick up remainder
      }
      CVVec[Pt]<-c
    }
  }else if(!is.null(cvVector)){
    CVVec <- cvVector
    NCV <- max(CVVec)
    f <- Freq(CVVec)#Freq(CVVec,1);
    if(verbose){
      print(paste0("Sizes of cross-validation classes: ", t(f)))#disp ( ['Sizes of cross-validation classes: ',num2str(t(f))] )
    }
    if(min(Freq(CVVec))==0){
      warning("Misspecification of CV vector: empty class(es) detected")
      #print("Warning: misspecification of CV vector: empty class(es) detected")
    }
  }


  argout	<- list()


  if(BoundH==0){
    argout<-OptTreeGate(defY=Y,X=X,XType=XType,defMeasure=Measure,defMaxSize=MaxSize,defMaxDepth=MaxDepth,defMinNodeSize=MinBucket,defMinH=MinH,AlgoType=AlgoType,CVVec=CVVec,
                        LookAheadDepth=LookAheadDepth, verbose = verbose)
  }else{
    argout<-OptTreeGate(defY=Y,X=X,XType=XType,defMeasure=Measure,defMaxSize=MaxSize,defMaxDepth=MaxDepth,defMinNodeSize=MinBucket,defMinH=MinH,AlgoType=AlgoType,CVVec=CVVec,
                        BoundH=BoundH,LookAheadDepth=LookAheadDepth, verbose = verbose)
  }



  Ntrees<-ifelse(is.na(dim(argout$TAll)[3]),1,dim(argout$TAll)[3])



  #argout$Indices
  FinalIndex<-list()
  FinalSizes<-list()
  FinalProb<-list()
  ProbLeaves<-list()


  if(Ntrees>1){

    for (i in 2:Ntrees) {


      IndexList<-getIndex(prevNode=1, side="left", node=1, Tree=argout$TAll[,,i], Index=argout$Indices[[i]], keepnode = NULL)

      FinalIndex[[i]]<-IndexList$Index[IndexList$keep]


      #Need to fix it for multiple trees
      if(Measure!=0){
        SizesProb<-computeProb(Y,IndexList$Index)#computeProb(Y,argout$Indices[[i]])
        FinalSizes[[i]]<-SizesProb$Sizes
        FinalProb[[i]]<-SizesProb$Prob
        ProbLeaves[[i]]<-FinalProb[[i]][IndexList$keep+1]
      }


    }
    FinalIndex[[1]]<-1:NROW(Y)

    if(Measure!=0){
      FinalSizes[[1]]<-table(Y)
      names(FinalSizes[[1]])<-NULL
      FinalProb[[1]]<-as.numeric(as.vector(FinalSizes[[1]]))/NROW(Y)
      ProbLeaves[[1]]<-FinalProb[[1]]
    }


  }else{

    IndexList<-getIndex(prevNode=1, side="left", node=1, Tree=argout$TAll[,,1], Index=argout$Indices[[1]], keepnode = NULL)
    FinalIndex[[1]]<-IndexList$Index[IndexList$keep]

    if(Measure!=0){
      SizesProb<-computeProb(Y,IndexList$Index)#computeProb(Y,argout$Indices[[1]])
      FinalSizes[[1]]<-SizesProb$Sizes
      FinalProb[[1]]<-SizesProb$Prob
      ProbLeaves[[1]]<-FinalProb[[1]][IndexList$keep+1]
    }

  }



  #evaluation function

  Evaluation<-evalFunc(Y=Y,Indices=FinalIndex,TAll=argout$TAll_list, measure=Measure, NTrees=Ntrees)




  Transf_Tree<-list()
  if(Ntrees==1){

    Tree<-matrix(argout$TAll, nrow = NROW(argout$TAll), ncol = NCOL(argout$TAll))

    NTN<-sum(Tree[,1]==0)

    if(Measure>0 || is.null(CVVec)){
      CVdata<-NULL
    }else{
      CVout<-argout$CVOutput
      CVdata<-matrix(c(CVout$Size,CVout$Error,CVout$Rel_error,CVout$SE,CVout$SE2,CVout$CHECK),nrow = 1)
      colnames(CVdata)<-c("Size","Error","Rel. Error", "SE", "SE2", "CHECK")
    }


    Transf_Tree[[1]]<- transformResults(TAll=argout$Tree,NtermNodes=NTN,X=X,Y=Y, Desc=Desc, CVdata=CVdata, origData = data[,colnames(X)])



  }else{

    for (i in 1:Ntrees) {

      if(Measure>0 || is.null(CVVec)){
        CVdata<-NULL
      }else{
        CVout<-argout$CVOutput
        CVdata<-matrix(c(CVout$Size[i],CVout$Error[i],CVout$Rel_error[i],CVout$SE[i],CVout$SE2[i],CVout$CHECK[i]),nrow = 1)
        colnames(CVdata)<-c("Size","Error","Rel. Error", "SE", "SE2", "CHECK")
      }
      Transf_Tree[[i]]<-transformResults(TAll=argout$TAll_list,NtermNodes=i,X=X,Y=Y, Desc=Desc, CVdata=CVdata, origData = data[,colnames(X)])
    }

  }




  #class(Transf_Tree)<-"ETree"

  Output<-list(h=argout$h,Tree=argout$Tree,hAll=argout$hAll,TAll=argout$TAll_list, Transf_Trees=Transf_Tree, CVOutput=argout$CVOutput, Index=FinalIndex, Sizes=FinalSizes, Prob=FinalProb, ProbLeaves=ProbLeaves, Evaluation=Evaluation, Desc=Desc, Data=Data, Yvarnames=Yvarnames, Xvarnames=Xvarnames)

  class(Output)<-"ETree"

  return(Output)
}
