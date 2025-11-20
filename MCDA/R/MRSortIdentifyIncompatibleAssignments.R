#' Identifies all sets of assignment examples which are incompatible with the
#' MRSort method.
#' 
#' This MRSort method, which is a simplification of the Electre TRI method,
#' uses the pessimistic assignment rule, without indifference or preference
#' thresholds attached to criteria.  Only a binary discordance condition is
#' considered, i.e. a veto forbids an outranking in any possible concordance
#' situation, or not.  This function outputs for all (or a fixed number of)
#' sets of incompatible assignment examples ranging in size from the minimal
#' size and up to a given threshold.  The retrieved sets are also not contained
#' in each other.
#' 
#' 
#' @param performanceTable Matrix or data frame containing the performance
#' table.  Each row corresponds to an alternative, and each column to a
#' criterion.  Rows (resp. columns) must be named according to the IDs of the
#' alternatives (resp. criteria).
#' @param assignments Vector containing the assignments (IDs of the categories)
#' of the alternatives to the categories.  The elements are named according to
#' the alternatives.
#' @param categoriesRanks Vector containing the ranks of the categories.  The
#' elements are named according to the IDs of the categories.
#' @param criteriaMinMax Vector containing the preference direction on each of
#' the criteria.  "min" (resp. "max") indicates that the criterion has to be
#' minimized (maximized).  The elements are named according to the IDs of the
#' criteria.
#' @param veto Boolean parameter indicating whether veto profiles are being
#' used by the model or not.
#' @param incompatibleSetsLimit Pozitive integer denoting the upper limit of
#' the number of sets to be retrieved.
#' @param largerIncompatibleSetsMargin Pozitive integer denoting whether sets
#' larger than the minimal size should be retrieved, and by what margin. For
#' example, if this is 0 then only sets of the minimal size will be retrieved,
#' if this is 1 then sets also larger by 1 element will be retrieved.
#' @param alternativesIDs Vector containing IDs of alternatives, according to
#' which the datashould be filtered.
#' @param criteriaIDs Vector containing IDs of criteria, according to which the
#' data should be filtered.
#' @return The function returns NULL if there is a problem, or a list
#' containing a list of incompatible sets of alternatives as vectors and the
#' status of the execution.
#' @references Bouyssou, D. and Marchant, T. An axiomatic approach to
#' noncompen- satory sorting methods in MCDM, II: more than two categories.
#' European Journal of Operational Research, 178(1): 246--276, 2007.
#' @keywords methods
#' @examples
#' 
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
#' assignments <-c("P", "P", "P", "F", "F", "F", "F", "F", "F", "P", "F", 
#'                 "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", 
#'                 "F", "F")
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
#' incompatibleAssignmentsSets<-MRSortIdentifyIncompatibleAssignments(
#'                                performanceTable, assignments, 
#'                                categoriesRanks, criteriaMinMax, 
#'                                veto = TRUE, 
#'                                alternativesIDs = c("a1","a2","a3","a4",
#'                                "a5","a6","a7","a8","a9","a10"))
#' 
#' print(incompatibleAssignmentsSets)
#' 
#' filteredAlternativesIDs <- setdiff(c("a1","a2","a3","a4","a5","a6","a7","a8","a9"),
#'                                    incompatibleAssignmentsSets[[1]][1])
#' 
#' print(filteredAlternativesIDs)
#' 
#' x<-MRSortInferenceExact(performanceTable, assignments, categoriesRanks, 
#'                         criteriaMinMax, veto = TRUE, 
#'                         readableWeights = TRUE, readableProfiles = TRUE,
#'                         alternativesIDs = filteredAlternativesIDs)
#' 
#' ElectreAssignments<-MRSort(performanceTable, x$profilesPerformances,
#'                            categoriesRanks, x$weights,
#'                            criteriaMinMax, x$lambda, 
#'                            criteriaVetos=x$vetoPerformances,
#'                            alternativesIDs = filteredAlternativesIDs)
#' 
#' @export MRSortIdentifyIncompatibleAssignments
MRSortIdentifyIncompatibleAssignments <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, veto = FALSE, incompatibleSetsLimit = 100, largerIncompatibleSetsMargin = 0, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.logical(veto))
    stop("veto should be a boolean")
  
  if (!is.numeric(incompatibleSetsLimit))
    stop("incompatibleSetsLimit should be numeric")
  else if (incompatibleSetsLimit%%1!=0)
    stop("incompatibleSetsLimit should be an integer")
  else if (incompatibleSetsLimit<=0)
    stop("incompatibleSetsLimit should be strictly pozitive")
  
  if (!is.numeric(largerIncompatibleSetsMargin))
    stop("largerIncompatibleSetsMargin should be numeric")
  else if (largerIncompatibleSetsMargin%%1!=0)
    stop("largerIncompatibleSetsMargin should be an integer")
  else if (largerIncompatibleSetsMargin<0)
    stop("largerIncompatibleSetsMargin should be pozitive")
  
  if (!(is.null(alternativesIDs) || is.vector(alternativesIDs)))
    stop("alternativesIDs should be a vector")
  
  if (!(is.null(criteriaIDs) || is.vector(criteriaIDs)))
    stop("criteriaIDs should be a vector")
  
  ## filter the data according to the given alternatives and criteria
  
  if (!is.null(alternativesIDs)){
    performanceTable <- performanceTable[alternativesIDs,]
    assignments <- assignments[alternativesIDs]
  }
  else
    alternativesIDs = rownames(performanceTable)
  
  if (!is.null(criteriaIDs)){
    performanceTable <- performanceTable[,criteriaIDs]
    criteriaMinMax <- criteriaMinMax[criteriaIDs]
  }
  else
    criteriaIDs = colnames(performanceTable)
  
  # data is filtered, check for some data consistency
  
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  
  if (is.null(dim(performanceTable))) 
    stop("less than 2 criteria or 2 alternatives")
  
  # -------------------------------------------------------
  
  numCrit <- dim(performanceTable)[2]
  
  numAlt <- dim(performanceTable)[1]
  
  numCat <- length(categoriesRanks)
  
  tempPath <- tempdir()
  
  # get data content that remains the same for all following linear program executions
  
  datacontent <- paste("data;\nparam X := ", numAlt, ";\n\nparam F := ", numCrit, ";\n\nparam Fdir := \n", sep = "")
  
  for (i in 1:numCrit){
    datacontent <- paste(datacontent, i, "\t", sep = "")
    if (criteriaMinMax[i]=="min")
      datacontent <- paste(datacontent, "-1", sep = "")
    else
      datacontent <- paste(datacontent, "1", sep = "")
    if (i!=numCrit)
      datacontent <- paste(datacontent, "\n", sep = "")
    else
      datacontent <- paste(datacontent, ";\n\n", sep = "")
  }
  
  datacontent <- paste(datacontent, "param Fmin :=\n", sep = "")
  
  for (i in 1:numCrit){
    datacontent <- paste(datacontent, i, "\t", apply(performanceTable, 2, min)[i], sep = "")
    if (i!=numCrit)
      datacontent <- paste(datacontent, "\n", sep = "")
    else
      datacontent <- paste(datacontent, ";\n\n", sep = "")
  }
  
  datacontent <- paste(datacontent, "param Fmax :=\n", sep = "")
  
  for (i in 1:numCrit){
    datacontent <- paste(datacontent, i, "\t", apply(performanceTable, 2, max)[i], sep = "")
    if (i!=numCrit)
      datacontent <- paste(datacontent, "\n", sep = "")
    else
      datacontent <- paste(datacontent, ";\n\n", sep = "")
  }
  
  datacontent <- paste(datacontent, "param K :=", numCat, ";\n\n", sep = "")
  
  datacontent <- paste(datacontent, "param A:=\n", sep = "")
  
  for (i in 1:numAlt){
    datacontent <- paste(datacontent, i, "\t", categoriesRanks[assignments[i]], sep = "")
    if (i!=numAlt)
      datacontent <- paste(datacontent, "\n", sep = "")
    else
      datacontent <- paste(datacontent, ";\n\n", sep = "")
  }
  
  datacontent <- paste(datacontent, "param PTx : ", sep = "")
  for(i in 1:numCrit)
    datacontent <- paste(datacontent, i, sep = " ")
  datacontent <- paste(datacontent, ":= \n", sep = "")
  
  
  for (i in 1:numAlt){
    datacontent <- paste(datacontent, i, "\t", sep = "")
    for (j in 1:numCrit){
      datacontent <- paste(datacontent, performanceTable[i,j], sep = "")
      if (j!=numCrit)
        datacontent <- paste(datacontent, " ", sep = "")
    }
    if (i!=numAlt)
      datacontent <- paste(datacontent, "\n", sep = "")
    else
      datacontent <- paste(datacontent, ";\n\n", sep = "")
  }
  
  datacontent <- paste(datacontent, "param gamma:=0.0001;\n", sep = "")
  
  # get first model file
  
  modelFile <- system.file("extdata","MRSortIdentifyMinimalInvalidAssignmentsSet.gmpl", package="MCDA")
  if(veto)
    modelFile <- system.file("extdata","MRSortVIdentifyMinimalInvalidAssignmentsSet.gmpl", package="MCDA")
  
  # write data file
  
  dataFile <- tempfile()
  
  file.copy(modelFile, dataFile)
  
  sink(dataFile, append=TRUE)
  
  cat(datacontent)
  
  cat("end;\n")
  
  sink()
  
  # init and run linear program
  
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
    
    # get size of minimal incompatible assignments set and one such set
    
    minIncompatibleSetsSize <- 0
    
    incompatibleSet <- c()
    
    for (i in 1:numAlt)
    {
      if(solution[varnames==paste("OnOff",paro,i,parc,sep="")] == 1)
      {
        incompatibleSet <- c(incompatibleSet,alternativesIDs[i])
        minIncompatibleSetsSize <- minIncompatibleSetsSize + 1
      }
    }
    
    incompatibleSets <- list(incompatibleSet)
    
    # if there are no incompatible sets return the empty set
    
    if(minIncompatibleSetsSize == 0)
      return(incompatibleSets)
    
    # get second model file
    
    modelFile <- system.file("extdata","MRSortIdentifyInvalidAssignmentsSet.gmpl", package="MCDA")
    if(veto)
      modelFile <- system.file("extdata","MRSortVIdentifyInvalidAssignmentsSet.gmpl", package="MCDA")
    
    # create new data content
    
    datacontent2a <- "param PrevOnOff : "
    for(i in 1:numAlt)
      datacontent2a <- paste(datacontent2a, i, sep = " ")
    datacontent2a <- paste(datacontent2a, ":= \n1\t", sep = "")
    for(i in 1:numAlt)
      datacontent2a <- paste(datacontent2a, solution[varnames==paste("OnOff",paro,i,parc,sep="")], sep = " ")
    
    datacontent2b <- paste("param PrevOnOffLimit := \n1\t ", minIncompatibleSetsSize, sep ="")
    
    # iterate through acceptes sizes for incompatible assignment sets
    
    incompatibleSetSize <- minIncompatibleSetsSize
    
    while(incompatibleSetSize <= minIncompatibleSetsSize + largerIncompatibleSetsMargin)
    {
      # break if we've retrieved the desired number of incompatible sets
      
      if(length(incompatibleSets) >= incompatibleSetsLimit)
        break
      
      repeat{
        # write data file
        
        dataFile <- tempfile()
        
        file.copy(modelFile, dataFile)
        
        sink(dataFile, append=TRUE)
        
        cat(datacontent)
        
        cat("param invalid:=")
        cat(incompatibleSetSize)
        cat(";\n")
        
        cat("param Y:=")
        cat(length(incompatibleSets))
        cat(";\n")
        
        cat(datacontent2a)
        cat(";\n\n")
        
        cat(datacontent2b)
        cat(";\n\n")
        
        cat("end;\n")
        
        sink()
        
        # init and run linear program
        
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
          
          # get incompatible assignments set
          
          incompatibleSet <- c()
          
          for (i in 1:numAlt)
            if(solution[varnames==paste("OnOff",paro,i,parc,sep="")] == 1)
              incompatibleSet <- c(incompatibleSet,alternativesIDs[i])
          
          # add set
          
          incompatibleSets <- c(incompatibleSets, list(incompatibleSet))
          
          # update data content
          
          datacontent2a <- paste(datacontent2a, "\n", length(incompatibleSets), "\t", sep = "")
          for(i in 1:numAlt)
            datacontent2a <- paste(datacontent2a, solution[varnames==paste("OnOff",paro,i,parc,sep="")], sep = " ")
          
          datacontent2b <- paste(datacontent2b, "\n", length(incompatibleSets), "\t", incompatibleSetSize, sep ="")
          
        }
        else
          break
      }
      
      # increase size of incompatible sets
      
      incompatibleSetSize <- incompatibleSetSize + 1
    }
    
    return(list(incompatibleSets = incompatibleSets, solverStatus = 'Success'))
    
  }
  else
    return(list(solverStatus = 'Failed'))
}
