#'Euclidean Distance Matrix Completion
#'
#'\code{edmc} 
#'
#'@details
#'
#'Depending on the method called, a number of input values are possible. 
#'
#'@param D An nxn partial-distance matrix to be completed, with unkown entries set to NA.
#'@param method The algorithm to be used to complete the distance matrix D. One of sdp, npf, dpf, snl, or grs
#'@param ... The remaining input values required for the completion method specified in \code{method}. See details.
#'
#'@return
#'
#'The return from \code{edmc} depends on the method used. The help pages for each individual method 
#'can be consulted for specific output.
#'
#'@examples
#'set.seed(1337)
#'D <- matrix(c(0,3,4,3,4,3,
#'              3,0,1,NA,5,NA,
#'              4,1,0,5,NA,5,
#'              3,NA,5,0,1,NA,
#'              4,5,NA,1,0,5,
#'              3,NA,5,NA,5,0),byrow=TRUE, nrow=6)
#'              
#'edmc(D,method = "dpf", d=3, toler=1e-8)
#'
#'@seealso \code{\link{sdp}} \code{\link{npf}} \code{\link{dpf}} \code{\link{snl}} \code{\link{grs}}
#'
#' @importFrom methods hasArg
#' @importFrom stats cov.wt
#' @importFrom utils read.csv read.table
#'
#'@export
edmc <- function(D, method = "dpf",...){

  #Store Optional Input as a List
  tmp <- list(...)
  
  #Checks
  
  #Convert D to a matrix object
  
  D <- as.matrix(D)
  
  #General Checks for D
  
  #General Checks for D
  if(nrow(D) != ncol(D)){
    stop("D must be a symmetric matrix")
  }else if(!is.numeric(D)){
    stop("D must be a numeric matrix")
  }else if(!is.matrix(D)){
    stop("D must be a numeric matrix")
  }else if(any(diag(D) != 0)){
    stop("D must have a zero diagonal")
  }else if(!isSymmetric(unname(D),tol=1e-8)){
    stop("D must be a symmetric matrix")
  }else if(!any(is.na(D))){
    stop("D must be a partial distance matrix. Some distances must be unknown.")
  }
  
  #Check that D is connected
  Test <- D
  Test[which(is.na(D))] <- 0
  TestSums <- rowSums(Test)
  
  if(any(TestSums == 0)){
    stop("D must be a connected matrix")
  }
  
  #Check that method is an appropriately named object
  
  if(!method %in% c("sdp","npf","dpf","snl","grs")){
    match.arg(method,c("sdp","npf","dpf","snl","grs"))
    warning("method did not exactly match a known input, using best guess.")
  }
  
  #Computation
  
  if(method == "sdp"){
    if(!hasArg("A")){ #If no argument for A is provided
      A <- matrix(rep(0,nrow(D)^2),nrow=nrow(D))
      A[which(!is.na(D))] <- 1
    }else{
      A <- tmp$A
    }
    
    if(!hasArg("toler")){ #if no arugment for toler is provided
      toler <- 1e-8
    }else{
      toler <- tmp$toler
    }
    
    if(any(c(hasArg("d"),
             hasArg("dmax"),
             hasArg("decreaseDim"),
             hasArg("stretch"),
             hasArg("dimMethod"),
             hasArg("lower"),
             hasArg("upper"),
             hasArg("keepMST"),
             hasArg("anchors")))){
      warning("Only input variables D, A, and toler valid for method sdp. All others ignored.")
    }
    
    out <- sdp(D,A,toler)
    
  }else if(method == "npf"){
    
    if(!hasArg("A")){
      A <- matrix(rep(0,nrow(D)^2),nrow=nrow(D))
      A[which(!is.na(D))] <- 1
    }else{
      A <- tmp$A
    }
    
    if(!hasArg("d")){
      stop("The npf algorithm requires the specification of a dimension d")
    }else{
      d <- tmp$d
    }
    
    if(!hasArg("dmax")){
      dmax <- nrow(D) - 1
    }else{
      dmax <- tmp$dmax
    }
    
    if(!hasArg("decreaseDim")){
      decreaseDim <- 1
    }else{
      decreaseDim <- tmp$decreaseDim
    }
    
    if(!hasArg("stretch")){
      stretch <- 1
    }else{
      stretch <- tmp$stretch
    }
    
    if(!hasArg("dimMethod")){
      dimMethod <- "NLP"
    }else{
      dimMethod <- tmp$dimMethod
    }
    
    if(!hasArg("toler")){
      toler <- 1e-8
    }else{
      toler <- tmp$toler
    }
    
    if(any(c(hasArg("lower"),
             hasArg("upper"),
             hasArg("keepMST"),
             hasArg("anchors")))){
      warning("Only input variables D, A, d, dmax, decreaseDim, stretch, dimMethod, and toler valid for method npf. All others ignored.")
    }
    
    out <- npf(D,A,d,dmax,decreaseDim,stretch,dimMethod,toler)
    
  }else if(method == "dpf"){
    
    if(!hasArg("d")){
      stop("The dpf algorithm requires the specification of a dimension d")
    }else{
      d <- tmp$d
    }
    
    if(!hasArg("toler")){
      toler <- 1e-8
    }else{
      toler <- tmp$toler
    }
    
    if(!hasArg("lower")){
      lower <- NULL
    }else{
      lower <- tmp$lower
    }
    
    if(!hasArg("upper")){
      upper <- NULL
    }else{
      upper <- tmp$upper
    }
    
    if(!hasArg("keepMST")){
      keepMST <- FALSE
    }else{
      keepMST <- out$keepMST
    }
    
    if(any(c(hasArg("A"),
             hasArg("dmax"),
             hasArg("decreaseDim"),
             hasArg("stretch"),
             hasArg("dimMethod"),
             hasArg("anchors")))){
      warning("Only input variables D, d, lower, upper, keepMST, and toler valid for method dpf. All others ignored.")
    }
    
    out <- dpf(D,d,toler,lower,upper,keepMST)
    
  }else if(method == "snl"){
    
    if(!hasArg("d")){
      stop("The snl algorithm requires the specification of a dimension d")
    }else{
      d <- tmp$d
    }
    
    if(!hasArg("anchors")){
      anchors <- NULL
    }else{
      anchors <- tmp$anchors
    }
    
    if(any(c(hasArg("A"),
             hasArg("dmax"),
             hasArg("decreaseDim"),
             hasArg("stretch"),
             hasArg("dimMethod"),
             hasArg("lower"),
             hasArg("upper"),
             hasArg("keepMST")))){
      warning("Only input variables D, d, and anchors valid for method snl. All others ignored.")
    }
    
    out <- snl(D,d,anchors)
    
  }else if(method == "grs"){
    
    if(!hasArg("d")){
      stop("The grs algorithm requires the specification of a dimension d")
    }else{
      d <- tmp$d
    }
    
    if(any(c(hasArg("A"),
             hasArg("dmax"),
             hasArg("decreaseDim"),
             hasArg("stretch"),
             hasArg("dimMethod"),
             hasArg("lower"),
             hasArg("upper"),
             hasArg("keepMST"),
             hasArg("anchors")))){
      warning("Only input variables D and d valid for method grs. All others ignored.")
    }
    
    out <- grs(D,d)
    
  }
  
  return(out)
  
}