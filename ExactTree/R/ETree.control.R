#' Control Parameters for ETree Algorithm
#'
#' Various parameters that control aspects of the \dQuote{ETree} algorithm.
#'
#' @param measure
#'        0 = minimize residual Sum of Squares (continuous Y)
#'        1 = minimize Misclassification Rate        (discrete Y)
#'        3 = minimize the Risk Sum P(A)*R(A)        (discrete Y)
#'            Measure = scalar or Measure=c(3,c(Prior,LossM))
#' @param maxsize
#'        if null: only maxdepth restrictions are applied
#'        if defined: the maximum number of terminal nodes for the best tree
#' @param maxdepth
#'        The maximum number of layers in a tree, NOT INCLUDING THE LAYER 0.
#'        maxdepth=0 means a tree with just one node (the root node),
#'        maxdepth=1 means one split and two terminal nodes, etc.
#' @param minbucket defines the minimum number of observations in a terminal node
#'        if the number of observations in a node is equal to this minimum
#'        the node becomes a possible terminal node; if the number of observations in a node
#'        is smaller, the node is set illegal and disregarded.
#' @param minheterogeneity defines the situation when a node A is considered Very Homogeneous
#'        and is not allowed to be split further, i.e. the node A becomes a possible terminal node
#'        (|A| = number of objects in node)
#' @param ncv specify N for N-fold cross-validation
#'           if NCV = 0, no N-fold cross-validation
#'           if NCV<0, abs(N)-fold cross-validation ONLY (no tree estimation)
#'           if NULL (default), NCV=0
#' @param cvVector <vector> process N-fold cross-validation according to the classes defined by <vector>
#'           vector is assumed to assign each observational unit to one of
#'           N-classes, number 1,...,N
#' @param heterogeneityonly (TRUE,FALSE)
#'        Algorithm will only give Heterogeneity/Impurity not a tree table
#'        => setting to false will give treetable as well
#' @param BoundH optional bound for best tree value, for BranchandBound
#' @param branchandbound (TRUE,FALSE)
#'        Algorithm will use branch and bound rules to speed up
#'        => for maxsize tree only, h only
#'        => make sure you specify boundH
#' @param alltreesizes (FALSE,TRUE)
#'        algorithm will compute multiple trees of size<=size restriction
#' @param sortedpsplits (TRUE,FALSE)
#'        Algorithm will try to speed up use a sorting logic.
#'        => suitable for continuous predictors with many categories
#' @param lookaheadheuristic Specify treedepth=the number of levels the heuristic will lookahead.
#'        When growing a tree with lookahead search (heurist approach),
#'        this specifies the maximum depth with respect to which
#'        any local split is optimized. When setting this depth = 1, this
#'        ammounts to convential tree growing.
#'
#' @return A list containing the options for the function ETree.
#'
#' @seealso \code{\link{ETree}}
#' @examples
#' ETree.control(measure=0, maxsize = 6, maxdepth = 4, minbucket = 5, ncv=10, alltreesizes = TRUE)
#'
#' @export
ETree.control<-function(measure=NULL, maxsize=4, maxdepth=2, minbucket=10, minheterogeneity=0.05,
                        ncv=10, cvVector=NULL, heterogeneityonly=NULL, BoundH=NULL, branchandbound=NULL, alltreesizes=FALSE, sortedpsplits=NULL, lookaheadheuristic=NULL) {





  if(is.null(ncv)+is.null(cvVector)<1){
    stop("Use only one argument between ncv and cvVector.")
  }



  AlgoType<-1
  CVVec<-c()



  if(!is.null(measure)){
    Measure<-measure
  }else{
    Measure<-0
  }


  if(!is.null(maxsize)){
    MaxSize<-maxsize
  }else{
    MaxSize<-0
  }


  if(!is.null(maxdepth)){
    MaxDepth<-maxdepth+1
  }else{
    MaxDepth<-0
  }



  if(!is.null(minbucket)){
    MinBucket<-minbucket
  }else{
    MinBucket<-10
  }




  if(!is.null(minheterogeneity)){
    MinH<-minheterogeneity
  }else{
    MinH<-0.05
  }



  if(!is.null(heterogeneityonly)){
    if(heterogeneityonly){
      AlgoType <- bitset(AlgoType,1,0)
    }else{
      AlgoType <- bitset(AlgoType,1,1)
    }
  }



  if(!is.null(BoundH)){
    BoundH<-BoundH
  }else{
    BoundH<-0
  }



  if(!is.null(branchandbound)){
    if(branchandbound){
      AlgoType <- bitset(AlgoType,2,1)
    }else{
      AlgoType <- bitset(AlgoType,2,0)
    }
  }



  if(!is.null(alltreesizes)){
    if(alltreesizes){
      AlgoType <- bitset(AlgoType,3,1)
    }else{
      AlgoType <- bitset(AlgoType,3,0)
    }
  }




  if(!is.null(sortedpsplits)){
    if(sortedpsplits){
      AlgoType <- bitset(AlgoType,4,1)
    }else{
      AlgoType <- bitset(AlgoType,4,0)
    }
  }



  if(!is.null(lookaheadheuristic)){
    AlgoType <- bitset(AlgoType,5,1)
    LookAheadDepth <- lookaheadheuristic
  }else{
    LookAheadDepth <- 0
  }





  if((MaxSize==0) && (MaxDepth==0)){
    stop('You have to specify at least the maximum size or the maximum depth.')
  }




  return(list(AlgoType=AlgoType, Measure=Measure, MaxSize=MaxSize, MaxDepth=MaxDepth, MinBucket=MinBucket, MinH=MinH,
       ncv=ncv, cvVector=cvVector, BoundH=BoundH, LookAheadDepth=LookAheadDepth))
}
