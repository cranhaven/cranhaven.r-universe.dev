#' Exact inference of an SRMP model given the lexicographic order of the
#' profiles - no inconsistencies
#' 
#' Exact inference approach from pairwise comparisons of alternatives for the
#' SRMP ranking model. This method only outputs a result when an SRMP model
#' consistent with the provided pairwise comparisons exists. The number of
#' reference profiles and their lexicographic order is fixed. If such a model
#' exists, this method is significantly faster than the one which handles
#' inconsistencies.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table. Each row corresponds to an alternative, and each column to a
#' criterion. Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param lexicographicOrder A vector containing the indexes of the reference
#' profiles in a given order. The number of reference profiles to be used is
#' derrived implicitly from the size of this vector. The elements of this
#' vector need to be a permutation of the indices from 1 to its size.
#' @param preferencePairs A two column matrix containing on each row a pair of
#' alternative names where the first alternative is considered to be strictly
#' preferred to the second.
#' @param indifferencePairs A two column matrix containing on each row a pair
#' of alternative names the two alternatives are considered to indifferent with
#' respect to each other.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @param timeLimit Allows to fix a time limit of the execution, in seconds. By
#' default NULL (which corresponds to no time limit).
#' @return The function returns a list containing: \item{criteriaWeights}{The
#' inferred criteria weights.} \item{referenceProfiles}{The inferred reference
#' profiles.} \item{solverStatus}{The solver status as given by glpk.}
#' \item{humanReadableStatus}{A description of the solver status.}
#' @references A-L. OLTEANU, V. MOUSSEAU, W. OUERDANE, A. ROLLAND, Y. ZHENG,
#' Preference Elicitation for a Ranking Method based on Multiple Reference
#' Profiles, forthcoming 2018.
#' @keywords methods
#' @examples
#' 
#' \donttest{
#' # the performance table
#' 
#' performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
#'                           c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
#'                           c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
#'                           c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))
#' 
#' lexicographicOrder <- c(2,1,3)
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
#'                                 "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22",
#'                                 "a23","a24")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' preferencePairs <- matrix(c("a16","a13","a3","a14","a17","a1","a18","a15","a2","a11","a5",
#'                             "a10","a4","a12","a13","a3","a14","a17","a1","a18","a15","a2",
#'                             "a11","a5","a10","a4","a12","a6"),14,2)
#' indifferencePairs <- matrix(c("a3","a1","a2","a11","a11","a20","a10","a10","a19","a12","a12",
#'                               "a21","a9","a7","a8","a20","a22","a22","a19","a24","a24","a21",
#'                               "a23","a23"),12,2)
#' 
#' result<-SRMPInferenceNoInconsistFixedLexicographicOrder(performanceTable, criteriaMinMax,
#'                                                         lexicographicOrder, preferencePairs,
#'                                                         indifferencePairs, alternativesIDs =
#'                                                         c("a1","a2","a3","a4","a5","a6","a7",
#'                                                         "a8","a10","a11","a12","a14","a16",
#'                                                         "a17","a18","a19","a20","a21","a23",
#'                                                         "a24"))
#' }
#' @export SRMPInferenceNoInconsistFixedLexicographicOrder
SRMPInferenceNoInconsistFixedLexicographicOrder <- function(performanceTable, criteriaMinMax, lexicographicOrder, preferencePairs, indifferencePairs = NULL, alternativesIDs = NULL, criteriaIDs = NULL, timeLimit = NULL){
  
  ## check the input data
  if (!(is.matrix(performanceTable) || is.data.frame(performanceTable))) 
    stop("performanceTable should be a matrix or a data frame")
  
  if (!is.matrix(preferencePairs) || is.data.frame(preferencePairs)) 
    stop("preferencePairs should be a matrix or a data frame")
  
  if (!(is.null(indifferencePairs) || is.matrix(indifferencePairs) || is.data.frame(indifferencePairs))) 
    stop("indifferencePairs should be a matrix or a data frame")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(lexicographicOrder)))
    stop("lexicographicOrder should be a vector")
  
  if (!(is.null(timeLimit)))
  {
    if(!is.numeric(timeLimit))
      stop("timeLimit should be numeric")
    if(timeLimit <= 1)
      stop("timeLimit should be strictly positive (and ideally above one second)")
  }
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  if(dim(preferencePairs)[2] != 2)
    stop("preferencePairs should have two columns")
  
  if(!is.null(indifferencePairs))
    if(dim(indifferencePairs)[2] != 2)
      stop("indifferencePairs should have two columns")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    preferencePairs <- preferencePairs[(preferencePairs[,1] %in% alternativesIDs) & (preferencePairs[,2] %in% alternativesIDs),]
    if(dim(preferencePairs)[1] == 0)
      preferencePairs <- NULL
    if(!is.null(indifferencePairs))
    {
      indifferencePairs <- indifferencePairs[(indifferencePairs[,1] %in% alternativesIDs) & (indifferencePairs[,2] %in% alternativesIDs),]
      if(dim(indifferencePairs)[1] == 0)
        indifferencePairs <- NULL
    }
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  if (is.null(dim(preferencePairs))) 
    stop("preferencePairs is empty or the provided alternativesIDs have filtered out everything from within")
  
  if (!all(sort(lexicographicOrder) == 1:length(lexicographicOrder)))
    stop("lexicographicOrder should be a permutation of profiles indices")
  
  # -------------------------------------------------------
  
  numAlt <- dim(performanceTable)[1]
  numCrit <- dim(performanceTable)[2]
  tempPath <- tempdir()
  
  # get model file depending on function options
  
  modelfilename <- "SRMPNoInconsist.gmpl"
  if(length(indifferencePairs) > 0)
    modelfilename <- "SRMPNoInconsistIndif.gmpl"
  
  modelFile <- system.file("extdata", modelfilename, package="MCDA")
  
  dataFile <- tempfile()
  
  file.copy(modelFile, dataFile)
  
  sink(dataFile, append=TRUE)
  
  cat("data;\n")
  
  cat("param n := ")
  cat(numAlt)
  cat(";\n\n")
  
  cat("param m := ")
  cat(numCrit)
  cat(";\n\n")
  
  cat("param dir := \n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    if (criteriaMinMax[i]=="min")
      cat("-1")
    else
      cat("1")
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n")
  }
  
  cat("param min :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, min)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param max :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, max)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param p := ")
  cat(length(lexicographicOrder))
  cat(";\n\n")
  
  if(length(indifferencePairs) == 0)
  {
    cat("param q := ")
    cat(dim(preferencePairs)[1])
    cat(";\n\n")
  }
  else
  {
    cat("param qp := ")
    cat(dim(preferencePairs)[1])
    cat(";\n\n")
    
    cat("param qi := ")
    cat(dim(indifferencePairs)[1])
    cat(";\n\n")
  }
  
  cat("param LO :=\n")
  for (i in 1:length(lexicographicOrder)){
    cat(i)
    cat("\t")
    cat(lexicographicOrder[i])
    if (i!=length(lexicographicOrder))
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param A : ")
  cat(1:numCrit)
  cat(" := \n")
  for (i in 1:numAlt){
    cat(i)
    cat("\t")
    cat(performanceTable[i,])
    if (i!=numAlt)
      cat("\n")
    else
      cat(";\n\n")
  }
  
  cat("param BC : ")
  cat(1:2)
  cat(" := \n")
  for (i in 1:dim(preferencePairs)[1]){
    cat(i)
    cat("\t")
    cat(which(rownames(performanceTable) == preferencePairs[i,1]))
    cat("\t")
    cat(which(rownames(performanceTable) == preferencePairs[i,2]))
    if (i!=dim(preferencePairs)[1])
      cat("\n")
    else
    {
      if(length(indifferencePairs) == 0)
        cat(";\n\n")
      else
        cat("\n")
    }
  }
  if(!is.null(indifferencePairs))
    for (i in 1:dim(indifferencePairs)[1]){
      cat(dim(preferencePairs)[1] + i)
      cat("\t")
      cat(which(rownames(performanceTable) == indifferencePairs[i,1]))
      cat("\t")
      cat(which(rownames(performanceTable) == indifferencePairs[i,2]))
      if (i!=dim(indifferencePairs)[1])
        cat("\n")
      else
        cat(";\n\n")
    }
  
  cat("param gamma:=0.001;\n")
  
  cat("end;\n")
  sink()
  
  lp<-initProbGLPK()
  
  tran<-mplAllocWkspGLPK()
  
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  
  termOutGLPK(GLP_OFF)
  
  out<-mplReadModelGLPK(tran, dataFile, skip=0)
  
  if (is.null(out))
    out <- mplGenerateGLPK(tran)
  else 
    stop(return_codeGLPK(out))
  
  if (is.null(out))
    mplBuildProbGLPK(tran,lp)
  else 
    stop(return_codeGLPK(out))
  
  
  
  if(!is.null(timeLimit))
    setMIPParmGLPK(TM_LIM, timeLimit * 1000)
  
  solveMIPGLPK(lp)
  
  solverStatus <- mipStatusGLPK(lp)
  
  error <- TRUE
  
  if(mipStatusGLPK(lp)==5){
    
    mplPostsolveGLPK(tran, lp, sol = GLP_MIP)
    
    solution <- mipColsValGLPK(lp)
    
    varnames <- c()
    
    for (i in 1:length(solution))
      varnames <- c(varnames,getColNameGLPK(lp,i))
    
    paro <- "["
    parc <- "]"
    
    error <- FALSE
  }
  
  
  if (!error){  
    
    weightsnames <- c()
    
    for (i in 1:numCrit)
    {
      weightsnames <- c(weightsnames,paste("w",paro,i,parc,sep=""))
    }
    
    weights <- c()
    
    for (i in 1:numCrit)
      weights <- c(weights,solution[varnames==weightsnames[i]])
    
    names(weights) <- colnames(performanceTable)
    
    profilenames <- matrix(nrow=length(lexicographicOrder),ncol=numCrit)
    
    for (i in 1:length(lexicographicOrder)){
      for (j in 1:numCrit)
      {
        profilenames[i,j] <- paste("P",paro,i,",",j,parc,sep="")
      }
    }
    
    referenceProfiles <- matrix(nrow=length(lexicographicOrder),ncol=numCrit)
    
    for (i in 1:length(lexicographicOrder)){
      for (j in 1:numCrit)
        referenceProfiles[i,j] <- solution[varnames==profilenames[i,j]]
    }
    
    colnames(referenceProfiles) <- colnames(performanceTable)
    
    
    return(list(criteriaWeights = weights, referenceProfiles = referenceProfiles, solverStatus = solverStatus, humanReadableStatus = "Solution is optimal."))
    
  }
  else
  {
    humanReadableStatus <- "Unknown"
    if(solverStatus %in% c(5,101,102))
      humanReadableStatus <- "Solution is optimal"
    else if(solverStatus %in% c(3,4,103,102))
      humanReadableStatus <- "Solution is infeasible"
    else if(solverStatus %in% c(111,112))
      humanReadableStatus <- "Memory limit"
    else if(solverStatus %in% c(107,108))
      humanReadableStatus <- "Time limit"
    else if(solverStatus %in% c(6,118))
      humanReadableStatus <- "No unbounded solution"
    return(list(solverStatus = solverStatus, humanReadableStatus = humanReadableStatus))
  }
}
