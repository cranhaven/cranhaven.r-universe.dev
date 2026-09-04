#' PREPARE DATA FOR FITTING 'GIMMEgVAR'
#'
#' Reads in results data from fitting 'graphicalVAR'
#' and creates the logical matrices for kappa and
#' beta needed to fit 'GIMMEgVAR'.  The logical matrices determine
#' which paths will not be regularized in the fitting of 'GIMMEgVAR' to determine
#' the group level network model.
#'
#' The following results are returned in the gvarFiles directory:
#' (1) Returns two data frames that hold the logical matrices for beta and kappa.  These
#' are named logicalBeta and logicalKappa
#'
#' (2) Returns two data frames that indicate the proportion of paths across individuals.
#' These are data frames containing the proportion of paths present across
#' individuals after fitting 'graphicalVAR'.  These proportions are used to determine
#' potential group paths which will not be regularized for individuals when fitting the
#' final 'GIMMEgVAR'. The output files are named proportionKappa.RData and proportionBeta.RData
#'
#' @param gimmeGVARThreshold The cutoff value for group-level paths. Defaults to .50, indicating
#'                       that a path must be non-zero across >= .50% of individuals to be
#'                       included as a group-level path.
#' @param RES_matrixData List containing results of fitting individual 'graphicalVAR' to each individual.
#' @param outputPath The user specified path to the directory where results files should be stored.

prepareData <- function(gimmeGVARThreshold=.50,
                        RES_matrixData,
                        outputPath){

  #STEP1: read in graphicalVAR results and do accounting of whose results are returned

  #STEP1 updated: reduce data set used in this step only to those who had results returned

  # identify who didn't have results returned
  success <- matrix(, length(RES_matrixData))
  for (subject in 1:length(success))
    success[subject] <- any(RES_matrixData[[subject]]$beta > 0)

  datause <- RES_matrixData[success]

  #STEP 2:count number of non-zero elements across people in the repetition for kappa and beta matrices

  #KAPPA COUNTS - flag non-zero values with 1's
  matrixKappa <- list() #initialize list for holding matrices that flag all non-zero elements for individuals with a 1

  matrixKappa <- vector("list") #initialize empty list for tracking current person in the rep
  for (subject in 1:length(datause)){
    matrixKappa[[subject]] <- ifelse(datause[[subject]][["kappa"]]!=0, 1,datause[[subject]][["kappa"]]) #change nonzero elements in matrix to 1's
  }

  #count flags to determine number of non-zero values
  pathCountKappa <- list()
  pathProportionKappa <-list()

  pathCountKappa <- Reduce('+', matrixKappa) #get count of number of times each path appears across all matrices
  pathProportionKappa <- t(t(pathCountKappa)/length(matrixKappa)) #get proportion of times each path appears across all matrices


  #BETA COUNTS - flag non-zero values with 1's
  matrixBeta <- vector("list") #initialize empty list for tracking current person in the rep
  for (subject in 1:length(datause)){
    matrixBeta[[subject]] <- ifelse(datause[[subject]][["beta"]]!=0, 1,datause[[subject]][["beta"]]) #change nonzero elements in matrix to 1's
  }

  #count flags to determine number of non-zero values
  pathCountBeta <- list()
  pathProportionBeta <-list()

  pathCountBeta <- Reduce('+', matrixBeta) #get count of number of times each path appears across all matrices
  pathProportionBeta <- t(t(pathCountBeta)/length(matrixBeta)) #get percent of times each path appears across all matrices

  #save path percents
  outputPathGVAR = here::here(outputPath,"gvarFiles")
  proportionKappaLoc = here(outputPathGVAR, "proportionKappa.RData") #create proportion kappa matrix file in gvar results directory
  proportionBetaLoc = here(outputPathGVAR, "proportionBeta.RData") #create proportion beta matrix file in gvar results directory

  saveRDS(pathProportionKappa, file = proportionKappaLoc)#save proportion kappa file
  saveRDS(pathProportionBeta, file = proportionBetaLoc) #save proportion beta file

  #STEP 3: retain paths greater than cutoff

  groupKappa <- NULL
  groupBeta <- NULL

  groupKappa <- ifelse(pathProportionKappa >= gimmeGVARThreshold, pathProportionKappa, 0)#change nonzero elements in matrix to 1's
  groupBeta <- ifelse(pathProportionBeta >= gimmeGVARThreshold, pathProportionBeta, 0)#change nonzero elements in matrix to 1's

  #STEP 4: determine paths to be forced into individual models for re-run of individual gvar models
  #note: for these matrices paths greater than the cutoff will not be regularized

  #create logical matrices to indicate which paths should be regularized
  #NOTE: zero paths should be regularized, non-zero paths should not

  logicalKappa <- NULL
  logicalBeta <- NULL

  logicalKappa <- ifelse(groupKappa == 0, TRUE, FALSE)
  logicalBeta <- ifelse(groupBeta == 0, TRUE, FALSE)

  #save logical matrices
  logicalKappaLoc = here(outputPathGVAR, "logicalKappa.RData") #create logical kappa matrix file in gvar results directory
  logicalBetaLoc = here(outputPathGVAR, "logicalBeta.RData") #create logical beta matrix file in gvar results directory

  saveRDS(logicalKappa, file = logicalKappaLoc)#save logical kappa
  saveRDS(logicalBeta, file = logicalBetaLoc) #save logical beta

  res <- list(logicalKappa = logicalKappa,
              logicalBeta = logicalBeta,
              datause     = datause)
  return(res)
}
