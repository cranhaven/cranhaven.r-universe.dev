#' GIMMEgVAR
#''
#' This function calls all the functions needed to fit 'GIMMEgVAR'.
#'
#' @param outputPath The user specified path to the directory where results files should be stored.
#' @param data A matrix or data frame containing repeated measures (rows) on a set of variables (columns).
#'             Must not contain missing data.
#' @param nLambda The number of both lambda parameters to test. Defaults to 50,
#'                which results in 2500 models to evaluate.
#' @param verbose Logical, should a progress bar be printed to the console?
#' @param scale Logical, should responses be standardized before estimation?
#' @param lambda_beta An optional vector of lambda_beta values to test.
#'                    Set lambda_beta = 0 argument and lambda_kappa = 0 for unregularized estimation.
#' @param lambda_kappa An optional vector of lambda_kappa values to test.
#'                     Set lambda_beta = 0 argument and lambda_kappa = 0 for unregularized estimation.
#' @param regularize_mat_beta A logical matrix indicating which elements of the beta matrix should be regularized (experimental).
#'                            Note: In 'GIMMEgVAR' this matrix is determined and set for the user based on their gimmeThreshold
#'                            and the results of fitting individual 'graphicalVAR' across all individuals. The logical matrix used
#'                            to fit determine which elements of the beta matrix are regularized is returned in the gimmegvarFiles folder
#'                            and is named logicalKappa.
#' @param regularize_mat_kappa A logical matrix indicating which elements of the kappa matrix should be regularized (experimental).
#'                             Note: In 'GIMMEgVAR' this matrix is determined and set for the user based on their gimmeThreshold
#'                             and the results of fitting individual 'graphicalVAR' across all individuals. The logical matrix used
#'                             to fit determine which elements of the beta matrix are regularized is returned in the gvarFiles folder
#'                             and is named logicalKappa.
#' @param maxit.in Maximum number of iterations in the inner loop (computing beta)
#' @param maxit.out Maximum number of iterations in the outer loop
#' @param deleteMissings Logical, should missing responses be deleted?
#' @param penalize.diagonal Logical, should the diagonal of beta be penalized (i.e., penalize auto-regressions)?
#' @param lambda_min_kappa Multiplier of maximal tuning parameter for kappa
#' @param lambda_min_beta Multiplier of maximal tuning parameter for beta
#' @param mimic Allows one to mimic earlier versions of graphicalVAR
#' @param variableNames The vector containing name of variables to be analyzed in the network model
#' @param gamma The EBIC hyper-parameter. Set to 0 to use regular BIC.
#' @param gimmeGVARThreshold The cutoff value for group-level paths. Defaults to .50, indicating
#'                       that a path must be non-zero across >= .50% of individuals to be
#'                       included as a group-level path.
#' @param beepvar String indicating assessment beep per day (if missing, is added).
#'                Adding this argument will cause non-consecutive beeps to be treated as missing!
#' @param dayvar String indicating assessment day. Adding this argument makes sure that the first measurement of a day is not
#'               regressed on the last measurement of the previous day.
#'               IMPORTANT: only add this if the data has multiple observations per day.
#' @param idvar String indicating the subject ID
#' @param lags Vector of lags to include
#' @param centerWithin logical, should subject data be within-person centered before estimating fixed effects?
#' @param likelihood Should likelihood be computed based on penalized contemporaneous matrix or
#'                   unpenalized contemporaneous matrix. Set to "penalized" to mimic version 2.5 and later of sparseTSCGM.
#' @param labelNames Vector of names used to label nodes in Network graph. Defaults to variable names if no
#'                   vector is supplied.
#' @returns Returns a list containing 7 elements.  The first 6 elements are the group level results indicating the count, proportion
#'          and presence (coded 1 for present, 0 for absent) of the edges estimated by the algorithm in the kappa and beta matrices.
#'          These results are stored in matrix objects named pathCountBeta, pathProportionBeta, groupBeta and pathCountKappa,
#'          pathProportionKappa, and groupKappa respectively and are used to construct the final group level network. The remaining
#'          element is a list which contains the person-specific network results for every subject in the data. This is estimated using
#'          information from the group-level network model. Along with the the data used to estimate the networks, it contains the following for each subject:
#'          \item{PCC}{The partial contemporaneous correlation network}
#'          \item{PDC}{The  partial directed correlation network}
#'          \item{beta}{The estimated beta matrix}
#'          \item{kappa}{The estimated kappa matrix}
#'          \item{EBIC}{The optimal EBIC}
#'          \item{path}{Results of all tested tuning parameters}
#'          \item{labels}{A vector containing the node labels}
#'          \item{data}{A "tsData" object detailing the features of the data used for estimating the network}
#' @export

#callGiMMEgVAR
GIMMEgVAR <- function(outputPath,
                      data,
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
                      penalize.diagonal = TRUE,
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
                      gimmeGVARThreshold = .5,
                      labelNames = variableNames){

#set up enviornment
environmentSetup(outputPath)

#fit graphicalVAR
RES_matrixData <-fitGraphicalVAR(data,
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
                 variableNames = variableNames,
                 beepvar = beepvar,
                 dayvar = dayvar,
                 idvar  = idvar,
                 lags = lags,
                 centerWithin = centerWithin,
                 likelihood = likelihood,
                 outputPath = outputPath)
#prepare data
logicals <- prepareData(gimmeGVARThreshold,
                        RES_matrixData = RES_matrixData,
                        outputPath = outputPath)

#fit GIMMEgVAR
    fitGIMMEgVAR(data,
               variableNames = variableNames,
               nLambda = nLambda,
               verbose = verbose,
               gamma = gamma,
               scale = scale,
               lambda_beta = lambda_beta,
               lambda_kappa = lambda_kappa,
               maxit.in = maxit.in,
               maxit.out = maxit.out,
               deleteMissings = deleteMissings,
               penalize.diagonal = penalize.diagonal,
               lambda_min_kappa = lambda_min_kappa,
               lambda_min_beta = lambda_min_kappa,
               mimic = mimic,
               #vars = variableNames,
               beepvar = beepvar,
               dayvar = dayvar,
               idvar  = idvar,
               lags = lags,
               centerWithin = centerWithin,
               likelihood = likelihood,
               logicals = logicals,
               RES_matrixData = RES_matrixData,
               outputPath = outputPath,
               labelNames = labelNames)

}
