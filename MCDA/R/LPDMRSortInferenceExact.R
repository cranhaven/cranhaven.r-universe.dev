#' Identification of profiles, weights, majority threshold and veto and
#' dictator thresholds for the MRSort sorting approach extended to handle large
#' performance differences.
#' 
#' MRSort is a simplified ElectreTRI method that uses the pessimistic
#' assignment rule, without indifference or preference thresholds attached to
#' criteria.  LPDMRSort considers both a binary discordance and a binary
#' concordance conditions including several interactions between them.  The
#' identification of the profiles, weights, majority threshold and veto and
#' dictator thresholds are done by taking into account assignment examples.
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
#' @param majorityRule String denoting how the vetoes and dictators are
#' combined in order to form the assignment rule.  The values to choose from
#' are "M", "V", "D", "v", "d", "dV", "Dv", "dv".  "M" corresponds to using
#' only the majority rule without vetoes or dictators, "V" considers only the
#' vetoes, "D" only the dictators, "v" is like "V" only that a dictator may
#' invalidate a veto, "d" is like "D" only that a veto may invalidate a
#' dictator, "dV" is like "V" only that if there is no veto we may then
#' consider the dictator, "Dv" is like "D" only that when there is no dictator
#' we may consider the vetoes, while finally "dv" is identical to using both
#' dictator and vetoes only that when both are active they invalidate each
#' other, so the majority rule is considered in that case.
#' @param readableWeights Boolean parameter indicating whether the weights are
#' to be spaced more evenly or not.
#' @param readableProfiles Boolean parameter indicating whether the profiles
#' are to be spaced more evenly or not.
#' @param minmaxLPD Boolean parameter indicating whether the veto thresholds
#' are to be minimized (or maximized if lower criteria values are preferred)
#' while the dictator thresholds are to be maximized (or minimized if lower
#' criteria values are preferred).
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
#' 
#' Meyer, P. and Olteanu, A-L. Integrating large positive and negative
#' performance differences in majority-rule sorting models.  European Journal
#' of Operational Research, submitted, 2015.
#' @keywords methods
#' @examples
#' 
#' # the performance table
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
#' categoriesRanks <-c(1,2)
#' 
#' names(categoriesRanks) <- c("P","F")
#' 
#' criteriaMinMax <- c("max","max","max")
#' 
#' names(criteriaMinMax) <- colnames(performanceTable)
#' 
#' assignments <-rbind(c("P","P","P","F","F","F","F","F","F","F","F","F",
#'                     "F","F","F","F","F","F","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","P","P","P","P","P","P",
#'                     "P","P","P","P","P","P","P","P","P","P","P","P"), 
#'                     c("P","P","P","F","F","F","F","F","F","F","F","F",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","P","P","P","P","P","P",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "F","F","F","F","F","F","F","F","F","F","F","F"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "P","P","P","P","P","P","P","P","P","P","P","P"), 
#'                     c("P","P","P","F","F","F","F","F","F","P","P","P",
#'                     "P","P","P","P","P","P","F","F","F","F","F","F"))
#' 
#' colnames(assignments) <- rownames(performanceTable)
#' 
#' majorityRules <- c("V","D","v","d","dV","Dv","dv")
#' 
#' for(i in 1:1)# change to 7 in order to perform all tests
#' {
#'   x<-LPDMRSortInferenceExact(performanceTable, assignments[i,],
#'                              categoriesRanks, criteriaMinMax, 
#'                              majorityRule = majorityRules[i], 
#'                              readableWeights = TRUE,
#'                              readableProfiles = TRUE,
#'                              minmaxLPD = TRUE)
#'   
#'   ElectreAssignments<-LPDMRSort(performanceTable, x$profilesPerformances, 
#'                                 categoriesRanks,
#'                                 x$weights, criteriaMinMax, x$lambda, 
#'                                 criteriaVetos=x$vetoPerformances, 
#'                                 criteriaDictators=x$dictatorPerformances, 
#'                                 majorityRule = majorityRules[i])
#'   
#'   print(x)
#'   
#'   print(all(ElectreAssignments == assignments[i,]))
#' }
#' 
#' @export LPDMRSortInferenceExact
#' @import glpkAPI
LPDMRSortInferenceExact <- function(performanceTable, assignments, categoriesRanks, criteriaMinMax, majorityRule = "M", readableWeights = FALSE, readableProfiles = FALSE, minmaxLPD = FALSE, alternativesIDs = NULL, criteriaIDs = NULL){
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable))))) 
    stop("wrong performanceTable, should be a matrix or a data frame")
  
  if (!(is.vector(assignments)))
    stop("assignments should be a vector")
  
  if (!(is.vector(categoriesRanks)))
    stop("categoriesRanks should be a vector")
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!is.character(majorityRule))
    stop("majorityRule should be a character or a string of characters")
  else if (!(majorityRule %in% c("M","V","D","v","d","dV","Dv","dv")))
    stop("majorityRule needs to take values in {'M','V','D','v','d','dV','Dv','dv'}")
  
  if (!is.logical(readableWeights))
    stop("readableWeights should be a boolean")
  
  if (!is.logical(readableProfiles))
    stop("readableProfiles should be a boolean")
  
  if (!is.logical(minmaxLPD))
    stop("minmaxLPD should be a boolean")
  
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

  modelfilename <- paste("MRSort", c("","V","D","DV1","DV2","DV3","DV4","DV5")[match(majorityRule,c("M","V","D","v","d","dV","Dv","dv"))], "InferenceModel", sep = "")
  
  if(readableWeights || readableProfiles)
  {
    modelfilename <- paste(modelfilename, "Spread", sep = "")
    if(readableWeights)
      modelfilename <- paste(modelfilename, "Weights", sep = "")
    if(readableProfiles)
      modelfilename <- paste(modelfilename, "Profiles", sep = "")
  }
  if(minmaxLPD & majorityRule != "")
    modelfilename <- paste(modelfilename, "LPD", sep = "")
  
  modelfilename <- paste(modelfilename, ".gmpl", sep = "")
  
  modelFile <- system.file("extdata", modelfilename, package="MCDA")
  
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
  
  lp<-glpkAPI::initProbGLPK()
  
  tran<-glpkAPI::mplAllocWkspGLPK()
  
  glpkAPI::setMIPParmGLPK(PRESOLVE, GLP_ON)
  
  glpkAPI::termOutGLPK(GLP_OFF)
  
  out<-glpkAPI::mplReadModelGLPK(tran, dataFile, skip=0)
  
  if (is.null(out))
    out <- glpkAPI::mplGenerateGLPK(tran)
  else 
    stop(glpkAPI::return_codeGLPK(out))
  
  if (is.null(out))
    glpkAPI::mplBuildProbGLPK(tran,lp)
  else 
    stop(glpkAPI::return_codeGLPK(out))
  

    
  glpkAPI::solveMIPGLPK(lp)
    
    solverStatus <- paste("Failed (",glpkAPI::return_codeGLPK(glpkAPI::mipStatusGLPK(lp)),")")
    
    error <- TRUE
    
    if(glpkAPI::mipStatusGLPK(lp)==5){
      solverStatus <- 'Solution found'
      
      glpkAPI::mplPostsolveGLPK(tran, lp, sol = GLP_MIP)
      
      solution <- glpkAPI::mipColsValGLPK(lp)
      
      varnames <- c()
      
      for (i in 1:length(solution))
        varnames <- c(varnames,glpkAPI::getColNameGLPK(lp,i))
      
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
    
    rownames(profilesPerformances) <- names(categoriesRanks)
    colnames(profilesPerformances) <- colnames(performanceTable)
    
    vetoPerformances <- NULL
    
    if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
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
      
      rownames(vetoPerformances) <- names(categoriesRanks)
      colnames(vetoPerformances) <- colnames(performanceTable)
    }
    
    dictatorPerformances <- NULL
    
    if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
    {
      ptdnames <- matrix(nrow=numCat,ncol=numCrit)
      
      for (i in 2:(numCat+1)){
        for (j in 1:numCrit)
        {
          ptdnames[i-1,j] <- paste("PTd",paro,i,",",j,parc,sep="")
        }
      }
      
      dictatorPerformances <- matrix(rep(NA,numCat*numCrit),nrow=numCat,ncol=numCrit)
      
      # bottom profile doesn't do anything, keep it as NA
      for (i in 1:(numCat-1)){
        for (j in 1:numCrit)
          dictatorPerformances[i,j] <- solution[varnames==ptdnames[i,j]]
      }
      
      rownames(dictatorPerformances) <- names(categoriesRanks)
      colnames(dictatorPerformances) <- colnames(performanceTable)
    }
    
    if(majorityRule %in% c("V","v","d","dV","Dv","dv"))
    {
      # determine which vetoes are actually used and remove those that are simply an artefact of the linear program
      
      used <- LPDMRSortIdentifyUsedVetoProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, lambda, weights, profilesPerformances, vetoPerformances, dictatorPerformances, majorityRule, alternativesIDs, criteriaIDs)
      
      for (k in (numCat-1):1)
      {
        cat <- names(categoriesRanks)[categoriesRanks == k]
        for (j in 1:numCrit)
        {
          if (!used[cat,j])
            vetoPerformances[cat,j] <- NA
        }
      }
    }
    
    if(majorityRule %in% c("D","v","d","dV","Dv","dv"))
    {
      # determine which dictators are actually used and remove those that are simply an artefact of the linear program
      
      used <- LPDMRSortIdentifyUsedDictatorProfiles(performanceTable, assignments, sort(categoriesRanks), criteriaMinMax, lambda, weights, profilesPerformances, dictatorPerformances, vetoPerformances, majorityRule, alternativesIDs, criteriaIDs)
      
      for (k in (numCat-1):1)
      {
        cat <- names(categoriesRanks)[categoriesRanks == k]
        for (j in 1:numCrit)
        {
          if (!used[cat,j])
            dictatorPerformances[cat,j] <- NA
        }
      }
    }
    
    return(list(lambda = lambda, weights = weights, profilesPerformances = profilesPerformances, vetoPerformances = vetoPerformances, dictatorPerformances = dictatorPerformances, solverStatus = solverStatus))
    
  }
  else
  {
    return(list(solverStatus = solverStatus))
  }
}
