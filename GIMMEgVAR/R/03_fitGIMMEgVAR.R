#' FIT 'GIMMEgVAR'
#'
#' This function fits 'GIMMEgVAR' using the original input data supplied by the user
#' and the logical matrices that result across people after fitting individual
#' 'graphicalVAR'.  The logical matrices are determined via the gimmeThreshold specified
#' by the user
#'
#' The user can pass in additional 'graphicalVAR' options as specified in the R
#' package 'graphicalVAR' created by Sacha Epskamp.  See 'graphicalVAR' documentation
#' for details.
#'
#' The following results are returned in the 'GIMMEgVAR' Files directory:
#' (1) Data frames containing individual paths.  These are the person-specific results
#' for each individual obtained by not regularizing the group paths indicated
#' by the logical matrices for both beta and kappa.  They are prefaced
#' RESULTS_GIMMEgVAR_SUBJECT_.
#'
#' (2) Data frames that indicate the proportion of paths across individuals:
#' These are dataframes containing the proportion of paths present across
#' individuals after fitting 'GIMMEgVAR'.  These proportions are used to determine
#' the final group model.  They are named proportionKappa.RData and proportionBeta.RData.
#'
#' (3) Group path data frames for beta and kappa:
#' These are dataframes with 1 indicating the presence of a group path and 0 indicating
#' the absence of a group path.
#' These data frames are derived from #2 above for both beta and kappa.  They are the
#' dataframes used to graph the final group models and are prefaced
#' GIMMEgVAR_RESULTS_GRP_BETA_THRESHOLD_ and GIMMEgVAR_RESULTS_GRP_KAPPA_THRESHOLD_.
#' The number following threshold indicates the gimmeThreshold used for the analysis.
#'
#' (4) Final network graphs are added to the gimmegvarFiles folder in .png format.
#' These contain overlay graphs of the group and individual paths for the beta and
#' kappa network respectively.
#'
#'
#' @param inData A matrix or data frame containing repeated measures (rows) on a set of variables (columns).
#'               Must not contain missing data.
#' @param variableNames The vector containing name of variables to be analyzed
#' @param gimmeGVARThreshold The cutoff value for group-level paths. Defaults to .50, indicating
#'                       that a path must be non-zero across >= .50% of individuals to be
#'                       included as a group-level path.
#' @param nLambda The number of both lambda parameters to test. Defaults to 50,
#'                which results in 2500 models to evaluate.
#' @param verbose Logical, should a progress bar be printed to the console?
#' @param gamma The EBIC hyper-parameter. Set to 0 to use regular BIC.
#' @param scale Logical, should responses be standardized before estimation?
#' @param lambda_beta An optional vector of lambda_beta values to test.
#'                    Set lambda_beta = 0 argument and lambda_kappa = 0 for unregularized estimation.
#' @param lambda_kappa An optional vector of lambda_kappa values to test.
#'                     Set lambda_beta = 0 argument and lambda_kappa = 0 for unregularized estimation.
#' @param maxit.in Maximum number of iterations in the inner loop (computing beta)
#' @param maxit.out Maximum number of iterations in the outer loop
#' @param deleteMissings Logical, should missing responses be deleted?
#' @param penalize.diagonal Logical, should the diagonal of beta be penalized (i.e., penalize auto-regressions)?
#' @param lambda_min_kappa Multiplier of maximal tuning parameter for kappa
#' @param lambda_min_beta Multiplier of maximal tuning parameter for beta
#' @param mimic Allows one to mimic earlier versions of 'graphicalVAR'
#' @param beepvar String indicating assessment beep per day (if missing, is added).
#'                Adding this argument will cause non-consecutive beeps to be treated as missing!
#' @param dayvar String indicating assessment day. Adding this argument makes sure that the first measurement of a day is not
#'               regressed on the last measurement of the previous day.
#'               IMPORTANT: only add this if the data has multiple observations per day.
#' @param idvar String indicating the subject ID
#' @param lags Vector of lags to include
#' @param centerWithin logical, should subject data be within-person centered before estimating fixed effects?
#' @param likelihood Should likelihood be computed based on penalized contemporaneous matrix or
#' @param logicals Logical matrices that determine which elements of the beta and kappa matrices will be regularized
#'                 when fititng 'GIMMEgVAR'.  The matrices are determined in the algorithm by obtaining counts across all fitted individual
#'                 network results for beta and kappa, respectively.  Elements with proportions >= gimmeGVARThreshold value
#'                 will not be regularized when fitting 'GIMMEgVAR'.
#' @param RES_matrixData List containing results of fitting individual 'graphicalVAR' to each individual.
#' @param outputPath The user specified path to the directory where results files should be stored.
#' @param labelNames Vector of names used to label nodes in Network graph. Defaults to variable names if no
#'                   vector is supplied.
#' @importFrom here here
#' @importFrom graphicalVAR graphicalVAR

fitGIMMEgVAR <- function(inData,
                         variableNames,
                         gimmeGVARThreshold=.50,
                         nLambda = 50,
                         verbose = TRUE,
                         gamma =.50,
                         scale = TRUE,
                         lambda_beta,
                         lambda_kappa,
                         maxit.in = 100,
                         maxit.out = 100,
                         deleteMissings = TRUE,
                         penalize.diagonal = TRUE ,
                         lambda_min_kappa = 0.05,
                         lambda_min_beta = lambda_min_kappa,
                         mimic = c("current","0.1.2","0.1.4","0.1.5","0.2"),
                         beepvar,
                         dayvar,
                         idvar,
                         lags = 1,
                         centerWithin = TRUE,
                         likelihood = c("unpenalized","penalized"),
                         logicals,
                         RES_matrixData,
                         outputPath,
                         labelNames = variableNames
                         ){

  #===========================================================================
  #CREATE INDIVIDUAL PERSON-SPECIFIC MODELS BY RE-FITTING GRAPHICAL VAR
  #NOT ALLOWING PATHS IN LOGICAL MATRICES TO BE REGULARIZED
  #===========================================================================

  #capture Ids of individual models successfully fitted in graphicalVAR
  gimmeGVARSubject <- names(RES_matrixData)

  #apply IDs to input Data
  names(inData) <- gimmeGVARSubject
  resultsGIMMEgVAR <- list()

  #get user's original options
  originalOptions <- options()
  on.exit(options(originalOptions))

  #create and open log file
  for (subject in gimmeGVARSubject){
    logFileName <- paste("logFile_GIMMEgVAR_subject", #generate logFiles for each individual
                         subject,
                         ".txt", sep="")
    logFileDir <- here(outputPath,"logFiles")
    logFileLoc = here(logFileDir, logFileName) #assign path for each individual's .txt file
    logFile <- file(logFileLoc, open='wt') #open individual's logfile location

    #issue command to capture warnings immediately
    options(warn=1)

    #issue command to save messages to log
    sink(logFile, type='message')

    #capture both output window and messages produced
    sink(logFile, type="output", append=TRUE,split = TRUE)

    #read in data
    gimmegvarData <- NULL
    gimmegvarData <- inData[[subject]]

    if(verbose){print(paste0("running GVAR for subject ", subject))}

    #load needed logical matrices
    logicalBeta <-  as.matrix(logicals$logicalBeta)

    logicalKappa <- as.matrix(logicals$logicalKappa)


    #Call graphicalVAR not allowing the paths in the logical matrices to be regularized
    resultsGIMMEgVAR[[subject]] <- graphicalVAR::graphicalVAR(gimmegvarData,
                                     nLambda = nLambda,
                                     verbose = verbose,
                                     gamma = gamma,
                                     scale = scale,
                                     lambda_beta = lambda_beta,
                                     lambda_kappa = lambda_kappa,
                                     regularize_mat_beta = logicalBeta,
                                     regularize_mat_kappa = logicalKappa,
                                     maxit.in = maxit.in,
                                     maxit.out = maxit.out,
                                     deleteMissings = deleteMissings,
                                     penalize.diagonal = penalize.diagonal,
                                     lambda_min_kappa = lambda_min_kappa,
                                     lambda_min_beta = lambda_min_kappa,
                                     mimic = mimic,
                                     vars = variableNames,
                                     beepvar = beepvar,
                                     dayvar = dayvar,
                                     idvar  = idvar,
                                     lags = lags,
                                     centerWithin = centerWithin,
                                     likelihood = likelihood)

    #create results output for GIMMEgVAR
    resultsNameGIMMEgVAR = paste("RESULTS_GIMMEgVAR_SUBJECT_", subject,".RData", sep="")
    outputPathGIMMEgVAR = here::here(outputPath,"gimmegvarFiles")
    outputFileGIMMEgVAR = here::here(outputPathGIMMEgVAR,resultsNameGIMMEgVAR)

    #save GIMMEgVAR results
    saveRDS(resultsGIMMEgVAR[[subject]], file = outputFileGIMMEgVAR)
    warnings()
    sink()
    sink()
    #closeAllConnections()

    }

    #===========================================================================
    #CREATE GIMMEgVAR GROUP MODEL USING GIMMEgVAR
    #INDIVIDUAL RESULTS BY COUNTING ACROSS INDIVIDUALS'
    #PATHS IDENTIFIED BY NEW ALGORITHM
    #===========================================================================

    #STEP1: for each individual, code paths identified by new algorithm as 1

    #For each individual person's results flag non-zero path values with 1's
    RES_PathsBeta <- list() #initialize list for holding matrices that flag all non-zero elements for individuals with a 1
    RES_PathsKappa <- list()
    for (subject in gimmeGVARSubject){
      RES_PathsBeta[[subject]] <- ifelse(RES_matrixData[[subject]][["beta"]]!=0, 1, RES_matrixData[[subject]][["beta"]])#change nonzero elements in BETA matrix to 1's, else keep 0's
      RES_PathsKappa[[subject]] <- ifelse(RES_matrixData[[subject]][["kappa"]]!=0, 1, RES_matrixData[[subject]][["kappa"]])#change nonzero elements in KAPA matrix to 1's, else keep 0's
      }

    #STEP 2:count flags across individuals to determine number of non-zero values for beta and kappa
    #get beta counts
    pathCountBeta <- list()
    pathProportionBeta <-list()

    pathCountBeta <- Reduce('+', RES_PathsBeta)#get count of number of times each path appears across all matrices
    pathProportionBeta <- t(t(pathCountBeta)/length(RES_matrixData)) #get percent of times each path appears across all matrices

    #get kappa counts
    pathCountKappa <- list()
    pathProportionKappa <-list()

    pathCountKappa <- Reduce('+', RES_PathsKappa) #get count of number of times each path appears across all matrices
    pathProportionKappa <- t(t(pathCountKappa)/length(RES_matrixData)) #get percent of times each path appears across all matrices

    #STEP 3:Determine group beta and kappa paths by retaining paths >= gimmeThreshold value and flag them with 1, else 0
    RES_groupBeta <- list()
    RES_groupKappa <- list()

    RES_groupBeta <- ifelse(pathProportionBeta >= gimmeGVARThreshold, 1, 0)#change elements >=gimmeThreshold to 1's, else 0's
    RES_groupKappa <- ifelse(pathProportionKappa >= gimmeGVARThreshold, 1, 0)#change elements >=gimmeThreshold to 1's, else 0's

    #create results output paths for group beta and kappa
    resultsNameGroupBeta = paste("GIMMEgVAR_RESULTS_GRP_BETA_THRESHOLD_",gimmeGVARThreshold,".RData", sep="") #BETA results
    resultsNameGroupKappa = paste("GIMMEgVAR_RESULTS_GRP_KAPPA_THRESHOLD_",gimmeGVARThreshold,".RData", sep="")#KAPPA results
    outputPathGIMMEgVAR = here::here(outputPath,"gimmegvarFiles")

    #save GIMMEgVAR results for group beta and kappa
    outputFileGIMMEgVAR = here::here(outputPathGIMMEgVAR,resultsNameGroupBeta)
    saveRDS(RES_groupBeta, file = outputFileGIMMEgVAR)
    outputFileGIMMEgVAR = here::here(outputPathGIMMEgVAR,resultsNameGroupKappa)
    saveRDS(RES_groupKappa, file = outputFileGIMMEgVAR)

    #save group path proportions
    proportionKappaLoc = here::here(outputPathGIMMEgVAR, "proportionKappa.RData") #create path proportion kappa file in GIMMEgVAR results directory
    saveRDS(pathProportionKappa, file = proportionKappaLoc)#save path proportion kappa
    proportionBetaLoc = here::here(outputPathGIMMEgVAR, "proportionBeta.RData") #create create path proportion beta file in GIMMEgVAR results results directory
    saveRDS(pathProportionBeta, file = proportionBetaLoc) #save path proportion beta

    warnings()

    res <- list(pathCountBeta = pathCountBeta,
                pathCountKappa = pathCountKappa,
                pathProportionBeta = pathProportionBeta,
                pathProportionKappa = pathProportionKappa,
                groupBeta = RES_groupBeta,
                groupKappa = RES_groupKappa,
                resultsGIMMEgVAR = resultsGIMMEgVAR)

    #graph GIMMEgVAR network graphs

    #plot graphs

    graphKappa(outputPathGIMMEgVAR = outputPathGIMMEgVAR,
               gimmeGVARThreshold = gimmeGVARThreshold,
               pathProportionKappa = pathProportionKappa,
               lableNames = labelNames,
               groupKappa = RES_groupKappa)

    graphBeta(outputPathGIMMEgVAR = outputPathGIMMEgVAR,
              gimmeGVARThreshold = gimmeGVARThreshold,
              pathProportionBeta = pathProportionBeta,
              lableNames = labelNames,
              groupBeta = RES_groupBeta)

    return(invisible(res))

}


