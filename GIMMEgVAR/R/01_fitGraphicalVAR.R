#' fitgraphicalVAR
#'
#' Fits 'graphicalVAR' with defaults used in 'graphicalVAR' package developed by Sacha Epskamp.
#' See 'graphicalVAR' documentation by Sacha Epskamp for details.
#'
#' The following results are returned in the gvarFiles directory:
#' (1) Separate data frames containing the usual individual 'graphicalVAR' results.
#' These dataframes contain the person-specific results for each individual obtained by fitting 'graphicalVAR'.
#' They are prefixed RESULTS_GVAR_SUBJECT
#' The number of data frames returned equals the number of individuals whose models were successfully
#' fitted from the input data file.
#'
#' @param data A matrix or data frame containing repeated measures (rows) on a set of variables (columns).
#'               Must not contain missing data.
#' @param variableNames The vector containing name of variables to be analyzed
#' @param gamma The EBIC hyper-parameter. Set to 0 to use regular BIC.
#' @param nLambda The number of both lambda parameters to test. Defaults to 50,
#'                which results in 2500 models to evaluate.
#' @param verbose Logical, should a progress bar be printed to the console?
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
#' @param outputPath The user specified path to the directory where results files should be stored.
#' @param regularize_mat_beta A logical matrix indicating which elements of the beta matrix should be regularized (experimental).
#' @param regularize_mat_kappa A logical matrix indicating which elements of the kappa matrix should be regularized (experimental).

fitGraphicalVAR <- function(data,
                            nLambda = 50,
                            verbose = TRUE,
                            gamma,
                            scale = TRUE,
                            lambda_beta,
                            lambda_kappa,
                            regularize_mat_beta,
                            regularize_mat_kappa,
                            maxit.in = 100,
                            maxit.out = 100,
                            deleteMissings = TRUE,
                            penalize.diagonal = TRUE ,
                            lambda_min_kappa = 0.05,
                            lambda_min_beta = lambda_min_kappa,
                            mimic = c("current"),
                            variableNames,
                            beepvar,
                            dayvar,
                            idvar,
                            lags = 1,
                            centerWithin = TRUE,
                            likelihood = c("unpenalized","penalized"),
                            outputPath
                            ){

  #create IDs for input data list
  gvarSubjects <- formatC(c(1:length(data)), width = nchar(length(data)), format = "d", flag = "0") #create naming convention for GVAR output and logfiles

  #apply IDs to input Data
  names(data) <- gvarSubjects

  RES_matrixData <- vector("list")

  #get user's original options
  originalOptions <- options()
  on.exit(options(originalOptions))

  #create and open log file
  for (subject in gvarSubjects){

    logFileName <- paste("logFile_GVAR_subject", #generate logFiles for each individual
                         subject,
                         ".txt", sep="")
    logFileDir <- here::here(outputPath,"logFiles")
    logFileLoc = here::here(logFileDir, logFileName) #assign path for each individual's .txt file
    logFile <- file(logFileLoc, open='wt') #open individual's logfile location

    #issue command to capture warnings immediately
    options(warn=1)

    #issue command to save messages to log
    sink(logFile, type='message')

    #capture both output window and messages produced
    sink(logFile, type = "output", append=TRUE,split = TRUE)

    #read in data
    gvarData <- NULL

    gvarData <- data[[subject]]

    #open log file
    if(verbose){print(paste0("running GVAR for subject ", subject))}


    #Call graphicalVAR
    RES_matrixData[[subject]] <- graphicalVAR(gvarData,
                             nLambda = nLambda,
                             verbose = verbose,
                             gamma = gamma,
                             scale = scale,
                             lambda_beta = lambda_beta,
                             lambda_kappa = lambda_kappa,
                             regularize_mat_beta = regularize_mat_beta,
                             regularize_mat_kappa = regularize_mat_kappa,
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

    #create results output file
    resultsNameGVAR = paste("RESULTS_GVAR","_SUBJECT_", subject, "_GAMMA_", gamma,".RData", sep="")
    outputPathGVAR = here::here(outputPath,"gvarFiles")
    outputFileGVAR = here::here(outputPathGVAR,resultsNameGVAR)

    #save results
    saveRDS(RES_matrixData[[subject]], file = outputFileGVAR)

    #close open logFile that is being written to
    sink(NULL, type = "message")
    sink(NULL, type = "output")
    #closeAllConnections()

  }
  return(invisible(RES_matrixData))
}


