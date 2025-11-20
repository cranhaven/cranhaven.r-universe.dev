#' Identification of profiles, weights and majority threshold for the MRSort
#' sorting method using an exact approach.
#' 
#' The MRSort method, a simplification of the Electre TRI method, uses the
#' pessimistic assignment rule, without indifference or preference thresholds
#' attached to criteria.  Only a binary discordance condition is considered,
#' i.e. a veto forbids an outranking in any possible concordance situation, or
#' not.  The identification of the profiles, weights and majority threshold are
#' done by taking into account assignment examples.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table.  Each row corresponds to an alternative, and each column to a
#' criterion.  Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param assignments Vector containing the assignments (IDs of the categories)
#' of the alternatives to the categories. The elements are named according to
#' the alternatives.
#' @param categoriesRanks Vector containing the ranks of the categories.  The
#' elements are named according to the IDs of the categories.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param veto Boolean parameter indicating whether veto profiles are being
#' used or not.
#' @param readableWeights Boolean parameter indicating whether the weights are
#' to be spaced more evenly or not.
#' @param readableProfiles Boolean parameter indicating whether the profiles
#' are to be spaced more evenly or not.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the data should be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns a list structured as follows :
#' \item{lambda}{The majority threshold.} \item{weights}{A vector containing
#' the weights of the criteria.  The elements are named according to the
#' criteria IDs.} \item{profilesPerformances}{A matrix containing the lower
#' profiles of the categories.  The columns are named according to the
#' criteria, whereas the rows are named according to the categories. The lower
#' profile of the lower category can be considered as a dummy profile.}
#' \item{vetoPerformances}{A matrix containing the veto profiles of the
#' categories.  The columns are named according to the criteria, whereas the
#' rows are named according to the categories. The veto profile of the lower
#' category can be considered as a dummy profile.} \item{solverStatus}{The
#' solver status as given by glpk.}
#' @references Bouyssou, D. and Marchant, T. An axiomatic approach to
#' noncompen- satory sorting methods in MCDM, II: more than two categories.
#' European Journal of Operational Research, 178(1): 246--276, 2007.
#' @keywords methods
#' @examples
#' 
#' performanceTable <- rbind(c(10,10,9), c(10,9,10), c(9,10,10), c(9,9,10), 
#'                           c(9,10,9), c(10,9,9), c(10,10,7), c(10,7,10), 
#'                           c(7,10,10), c(9,9,17), c(9,17,9), c(17,9,9), 
#'                           c(7,10,17), c(10,17,7), c(17,7,10), c(7,17,10), 
#'                           c(17,10,7), c(10,7,17), c(7,9,17), c(9,17,7), 
#'                           c(17,7,9), c(7,17,9), c(17,9,7), c(9,7,17))
#' 
#' rownames(performanceTable) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", 
#'                                 "a8", "a9", "a10", "a11", "a12", "a13", 
#'                                 "a14", "a15", "a16", "a17", "a18", "a19", 
#'                                 "a20", "a21", "a22", "a23", "a24")
#' 
#' colnames(performanceTable) <- c("c1","c2","c3")
#' 
#' assignments <-c("P", "P", "P", "F", "F", "F", "F", "F", "F", "F", "F", "F", 
#'                 "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F")
#' 
#' names(assignments) <- rownames(performanceTable)
#' 
#' categoriesRanks <-c(1,2)
#' 
#' names(categoriesRanks) <- c("P","F")
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' x<-MRSortInferenceExact(performanceTable, assignments, categoriesRanks, 
#'                          criteriaMinMax, veto = TRUE, readableWeights = TRUE, 
#'                          readableProfiles = TRUE, 
#'                          alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))
#' 
#' ElectreAssignments<-MRSort(performanceTable, x$profilesPerformances, 
#'                            categoriesRanks,
#'                            x$weights, criteriaMinMax, x$lambda, 
#'                            criteriaVetos=x$vetoPerformances,
#'                            alternativesIDs = c("a1","a2","a3","a4","a5","a6","a7"))
#' 
#' @export MRSortInferenceExact
MRSortInferenceExact <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, veto = FALSE, readableWeights = FALSE, readableProfiles = FALSE, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if(is.null(names(categoriesRanks)))
    stop("categoriesRanks should be named")
  
  if(!all(sort(categoriesRanks) == 1:length(categoriesRanks)))
    stop("categoriesRanks should contain a permutation of the category indices (from 1 to the number of categories)")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.logical(veto))
    stop("veto should be a boolean")
  
  if (!is.logical(readableWeights))
    stop("readableWeights should be a boolean")
  
  if (!is.logical(readableProfiles))
    stop("readableProfiles should be a boolean")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    assignments <- assignments[alternativesIDs]
  } 
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  tempPath <- tempdir()
  
  # get model file depending on function options
  
  modelFile <- system.file("extdata","MRSortInferenceModel.gmpl", package="MCDA")
  if(veto)
    modelFile <- system.file("extdata","MRSortVInferenceModel.gmpl", package="MCDA")
  
  if(readableWeights && readableProfiles)
  {
    modelFile <- system.file("extdata","MRSortInferenceModelSpreadWeightsProfiles.gmpl", package="MCDA")
    if(veto)
      modelFile <- system.file("extdata","MRSortVInferenceModelSpreadWeightsProfiles.gmpl", package="MCDA")
  }
  else
  {
    if(readableWeights)
    {
      modelFile <- system.file("extdata","MRSortInferenceModelSpreadWeights.gmpl", package="MCDA")
      if(veto)
        modelFile <- system.file("extdata","MRSortVInferenceModelSpreadWeights.gmpl", package="MCDA")
    }
    if(readableProfiles)
    {
      modelFile <- system.file("extdata","MRSortInferenceModelSpreadProfiles.gmpl", package="MCDA")
      if(veto)
        modelFile <- system.file("extdata","MRSortVInferenceModelSpreadProfiles.gmpl", package="MCDA")
    }
  }
  
  dataFile <- tempfile()
  
  file.copy(modelFile, dataFile)
  
  sink(dataFile, append=TRUE)
  
  cat("data;\n")
  
  cat("param X := ")
  cat(numAlt)
  cat(";\n\n")
  
  cat("param F := ")
  cat(numCrit)
  cat(";\n\n")
  
  cat("param Fdir := \n")
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
  
  cat("param Fmin :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, min)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param Fmax :=\n")
  for (i in 1:numCrit){
    cat(i)
    cat("\t")
    cat(apply(performanceTable, 2, max)[i])
    if (i!=numCrit)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param K := ")
  cat(numCat)
  cat(";\n\n")
  
  cat("param A:=\n")
  for (i in 1:numAlt){
    cat(i)
    cat("\t")
    cat(categoriesRanks[assignments[i]])
    if (i!=numAlt)
      cat("\n")
    else
      cat(";\n\n") 
  }
  
  cat("param PTx : ")
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
  
  
  
  solveMIPGLPK(lp)
  
  solverStatus <- paste("Failed (",return_codeGLPK(mipStatusGLPK(lp)),")")
  
  error <- TRUE
  
  if(mipStatusGLPK(lp)==5){
    solverStatus <- "Solution found"
    
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
    lambda <- solution[varnames=="lambda"]
    
    weightsnames <- c()
    
    for (i in 1:numCrit)
    {
      weightsnames <- c(weightsnames,paste("w",paro,i,parc,sep=""))
    }
    
    weights <- c()
    
    for (i in 1:numCrit)
      weights <- c(weights,solution[varnames==weightsnames[i]])
    
    names(weights) <- colnames(performanceTable)
    
    ptknames <- matrix(nrow=numCat,ncol=numCrit)
    
    for (i in 2:(numCat+1)){
      for (j in 1:numCrit)
      {
        ptknames[i-1,j] <- paste("PTk",paro,i,",",j,parc,sep="")
      }
    }
    
    profilesPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
    
    # the last profile (bottom one) doesn't do anything so we keep it NA
    for (i in 1:(numCat-1)){
      for (j in 1:numCrit)
        profilesPerformances[i,j] <- solution[varnames==ptknames[i,j]]
    }
    
    rownames(profilesPerformances) <- names(sort(categoriesRanks))
    colnames(profilesPerformances) <- colnames(performanceTable)
    
    vetoPerformances <- NULL
    
    if(veto)
    {
      ptvnames <- matrix(nrow=numCat,ncol=numCrit)
      
      for (i in 2:(numCat+1)){
        for (j in 1:numCrit)
        {
          ptvnames[i-1,j] <- paste("PTv",paro,i,",",j,parc,sep="")
        }
      }
      
      vetoPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
      
      # bottom profile doesn't do anything, keep it as NA
      for (i in 1:(numCat-1)){
        for (j in 1:numCrit)
          vetoPerformances[i,j] <- solution[varnames==ptvnames[i,j]]
      }
      
      rownames(vetoPerformances) <- names(sort(categoriesRanks))
      colnames(vetoPerformances) <- colnames(performanceTable)
      
      # determine which vetoes are actually used and remove those that are simply an artefact of the linear program
      
      used_vetoes <- MRSortIdentifyUsedVetoProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, lambda, weights, profilesPerformances, vetoPerformances, alternativesIDs, criteriaIDs)
      
      for (k in (numCat-1):1)
      {
        cat <- names(categoriesRanks)[categoriesRanks == k]
        for (j in 1:numCrit)
        {
          if (!used_vetoes[cat,j])
            vetoPerformances[cat,j] <- NA
        }
      }
    }
    
    return(list(lambda = lambda, weights = weights, profilesPerformances = profilesPerformances, vetoPerformances = vetoPerformances, solverStatus = solverStatus))  
  } else
  {
    return(list(solverStatus = solverStatus))
  }
}
