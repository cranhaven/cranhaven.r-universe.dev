#' BOSO and associates functions
#'
#' Compute the BOSO for use one block. This function calls cplexAPI to solve
#' the optimization problem
#' 
#'
#' @param x Input matrix, of dimension 'n' x 'p'. This is the data from the 
#' training partition. Its recommended to be class "matrix".
#' 
#' @param y Response variable for the training dataset. A matrix of one column 
#' or a vector, with 'n' elements.
#' 
#' @param xval Input matrix, of dimension 'n' x 'p'. This is the data from the 
#' validation partition. Its recommended to be class "matrix".
#' 
#' @param yval Response variable for the validation dataset. A matrix of one 
#' column or a vector, with 'n' elements.
#' 
#' @param intercept Boolean variable to indicate if intercept should be added 
#' or not. Default is false.
#' 
#' @param standardize Boolean variable to indicate if data should be scaled 
#' according to mean(x) mean(y) and sd(x) or not. Default is false.
#' 
#' @param dfmax Maximum number of variables to be included in the problem. The 
#' intercept is not included in this number. Default is min(p,n).
#' 
#' @param maxVarsBlock maximum number of variables in the block strategy.
#' 
#' @param IC information criterion to be used. Default is 'eBIC'.
#' 
#' @param IC.blocks information criterion to be used in the block strategy.
#' Default is the same as IC, but eBIC uses BIC for the block strategy.
#'  
#' @param nlambda The number of lambda values. Default is 100.
#' 
#' @param nlambda.blocks The number of lambda values in the block strategy part. 
#' Default is 10.
#' 
#' @param lambda.min.ratio Smallest value for lambda, as a fraction of 
#' lambda.max, the (data derived) entry value.
#' 
#' @param lambda A user supplied lambda sequence. Typical usage is to have the 
#' program compute its own lambda sequence based on nlambda and 
#' lambda.min.ratio. Supplying a value of lambda overrides this. 
#' WARNING: use with care.
#'  
#' @param costErrorVal Cost of error of the validation set in the objective 
#' function. Default is 1. WARNING: use with care, changing this value changes 
#' the formulation presented in the main article.
#' 
#' @param costErrorTrain Cost of error of the training set in the objective 
#' function. Default is 0. WARNING: use with care, changing this value changes 
#' the formulation presented in the main article.
#' 
#' @param costVars Cost of new variables in the objective function. Default is 0. 
#' WARNING: use with care, changing this value changes the formulation 
#' presented in the main article.
#' 
#' @param Threads CPLEX parameter, number of cores that CPLEX is allowed to use.
#' Default is 0 (automatic).
#' 
#' @param timeLimit CPLEX parameter, time limit per problem provided to CPLEX.
#' Default is 1e75 (infinite time).
#' 
#' @param verbose print progress, different levels: 1) print simple progress. 
#' 2) print result of blocks. 3) print each k in blocks Default is FALSE.
#' 
#' @param seed set seed for random number generator for the block strategy. 
#' Default is system default.
#' 
#' @param warmstart warmstart for CPLEX or use a different problem for each k. 
#' Default is False.
#' 
#' @param TH_IC is the ratio over one that the information criterion must 
#' increase to be STOP. Default is 1e-3.
#' 
#' @param indexSelected array of pre-selected variables. WARNING: debug feature.
#' 
#' @description Fit a ridge linear regression by a feature selection model 
#' conducted by BOSO MILP. The package 'cplexAPI' is necessary to run it.
#' 
#' @return A `BOSO` object which contains the following information: 
#' \item{betas}{estimated betas} 
#' \item{x}{trianing x set used in BOSO (input parameter)}
#' \item{y}{trianing x set used in BOSO (input parameter)}
#' \item{xval}{validation x set used in BOSO (input parameter)}
#' \item{yval}{validation x set used in BOSO (input parameter)}
#' \item{nlambda}{nlambda used by `BOSO` (input parameter)}
#' \item{intercept}{if `BOSO` has used intercept (input parameter)}
#' \item{standardize}{if `BOSO` has used standardization (input parameter)}
#' \item{mx}{Mean value of each variable. 0 if data has not been standarized}
#' \item{sx}{Standard deviation value of each variable. 0 if data has not been standarized}
#' \item{my}{Mean value of output variable. 0 if data has not been standarized}
#' \item{dfmax}{Maximum number of variables set to be used by `BOSO` (input parameter)}
#' \item{result.final}{list with the results of the final problem for each K}
#' \item{errorTrain}{error in training set in the final problem}
#' \item{errorVal}{error in Validation set in the final problem of used by}
#' \item{lambda.selected}{lambda selected in the final problem of}
#' \item{p}{number of initial variables}
#' \item{n}{number of events in the training set}
#' \item{nval}{number of events in the validation set}
#' \item{blockStrategy}{index of variables which were stored in each iteration by `BOSO` in the block strategy}
#'  
#' 
#' @examples 
#' 
#'   #This first example is a basic 
#'   #example of how to execute BOSO
#'   
#'   \donttest{
#'   data("sim.xy", package = "BOSO")
#'   obj <- BOSO(x = sim.xy[['low']]$x,
#'               y = sim.xy[['low']]$y,
#'               xval = sim.xy[['low']]$xval,
#'               yval = sim.xy[['low']]$yval,
#'               IC = 'eBIC',
#'               nlambda=50,
#'               intercept= 0, standardize = 0,
#'               Threads=1, verbose = 3, seed = 2021)
#'   coef(obj)  # extract coefficients at a single value of lambda
#'   predict(obj, newx = sim.xy[['low']]$x[1:20, ])  # make predictions
#'   }
#'
#' @author Luis V. Valcarcel
#' @export BOSO

BOSO = function(x, y, xval, yval, 
                IC = 'eBIC', IC.blocks = NULL,
                nlambda=100, nlambda.blocks = 10,
                lambda.min.ratio=ifelse(nrow(x)<ncol(x),0.01,0.0001),
                lambda=NULL, intercept=TRUE, standardize=TRUE,
                dfmax = NULL,
                maxVarsBlock = 10,
                costErrorVal = 1, costErrorTrain = 0, costVars = 0,
                Threads=0, timeLimit = 1e75, verbose = F,
                seed = NULL,
                warmstart = F,
                TH_IC = 1e-3,
                indexSelected = NULL)  {
  
  # Check for cplexAPI package
  if (!requireNamespace('cplexAPI', quietly = TRUE)) {
    stop("Package cplexAPI not installed (required here)!", call. = FALSE)
  }
  
  # Check the inputs
  if(!is(x,"matrix") & !is(x,"Matrix")){stop("input x must be a matrix or a Matrix class")}
  if(!is(y,"numeric") & !is(y,"matrix") & !is(y,"array")){stop("input y must be numeric")}
  if(!is(xval,"matrix") & !is(xval,"Matrix")){stop("input xval must be a matrix or a Matrix class")}
  if(!is(yval,"numeric") & !is(yval,"matrix") & !is(yval,"array")){stop("input yval must be numeric")}
  if(!is(IC,"character")){stop("information criterion metric must be character")}
  if(!is(nlambda,"numeric")){stop("nlambda must be numeric")}
  if(!is(nlambda.blocks,"numeric")){stop("nlambda.blocks must be numeric")}
  if(!is(lambda.min.ratio,"numeric")){stop("lambda.min.ratio must be numeric")}
  if(!is(nlambda,"numeric")){stop("nlambda must be numeric")}
  if(!is(maxVarsBlock,"numeric")){stop("maxVarsBlock must be numeric")}
  if(!is(TH_IC,"numeric")){stop("TH_IC must be numeric")}
  
  
  # Set up data ####
  x = as.matrix(x)
  y = as.numeric(y)
  xval = as.matrix(xval)
  yval = as.numeric(yval)
  n = nrow(x)
  nval = nrow(xval)
  p = ncol(x)
  
  # standarze?
  if (standardize) {
    # standardize using xtrain and xval
    obj = standardize(x, y, intercept=T, normalize = T)
    x = obj$x
    y = obj$y
    mx = obj$mx
    my = obj$my
    sx = obj$sx
    obj = standardize(xval, yval, mx=mx, my=my, sx=sx)
    xval = obj$x
    yval = obj$y
    # add for later on
    intercept = F  # once data is scaled, no need for beta0
    mx = c(0,mx) 
    sx = c(1,sx)
  } else {
    mx = rep(0,p + 1)
    my = 0
    sx = rep(1,p + 1)
  }
  
  # Set dfmax manually if NULL
  if (is.null(dfmax)){dfmax = p} 
  
  # Generate the lambda array if necessary
  if (is.null(lambda)){
    # lambda_max <- norm(t(x)%*%y, "I")/nrow(x)
    lambda_max <- max( abs(t(y - mean(y)*(1-mean(y))) %*% x ) )/ n #lasso
    lambda_min <- lambda_max * lambda.min.ratio
    lambda <- exp(seq(log(lambda_max*1e3), log(lambda_min), length.out = nlambda)) # lambda_max from ridge, lambda_min from lasso
    lambda.blocks <- exp(seq(log(lambda_max*1e3), log(lambda_min), length.out = nlambda.blocks))
  } else {
    # Reset nlambda if a specific lambda sequence is passed
    nlambda <- length(lambda)
    nlambda.blocks <- length(lambda.blocks)
    lambda.blocks <- lambda.blocks
  }
  
  # if eBIC, set BIC for blocks
  if (is.null(IC.blocks)) {
    IC.blocks <- ifelse(IC=="eBIC", "BIC", IC)
  }
  
  ## Separate data in packages of with a maximum size and perform block strategy ####
  data.raw <- list(x = x, y = y, xval = xval, yval = yval)
  
  if (is.null(indexSelected)){
    indexSelected <- 1:p # index of variables
  }
  
  # set seed for random number generator
  if (!is.null(seed)){ set.seed(seed) }
  
  # auxiliary variables
  FinishProblem <- F
  ContinueBlocks <- T
  numIter <- 0
  indexSelectedIter <- list()
  
  
  while (ContinueBlocks & length(indexSelected)>maxVarsBlock*1.5){
    numIter <- numIter+1
    
    #separate into blocks
    idx <- sample(indexSelected)
    k <- ceiling(length(idx)/maxVarsBlock)*maxVarsBlock 
    if(k!=length(idx)) {idx[(length(idx)+1):k] <- NA}
    idx <- matrix(idx, nrow = maxVarsBlock, byrow = T)
    idx <- lapply(lapply(apply(idx,2,as.list),unlist),function(x){x[!is.na(x)]})  # transform each column in a list
    
    indexSelectedIter[[numIter]] <- list()
    time_block <- rep(NA, length(idx))
    
    if (verbose) { cat(paste0('Iteration ',numIter,'\t number of variables: ',length(indexSelected),' \t block size: ',maxVarsBlock,'\n')) }
    
    # solve each of the blocks
    for (i in 1:length(idx)){
      # i = 1
      p.block <- length(idx[[i]])
      numVarArray <- seq(0,p.block)
      
      # subset of variables from the block strategy
      x = data.raw$x[,idx[[i]]]
      xval = data.raw$xval[,idx[[i]]]
      
      time_block[i] <- Sys.time()
      
      if (verbose>1) {  cat(paste0('Iteration ',numIter,'\t number of variables: ', length(indexSelected), '\t subproblem ', round(i*100/length(idx)), '%\n' )) }
      
      if (warmstart) {
        # cat("\nWARMSTART\n")
        result.block <- BOSO.multiple.warmstart(x = x, y = y, xval = xval, yval = yval,
                                                lambda=lambda.blocks, 
                                                intercept=intercept, 
                                                standardize=F,
                                                dfmin = 0, dfmax = p.block,
                                                costErrorVal = costErrorVal, costErrorTrain = costErrorVal, 
                                                costVars = costVars,
                                                Threads=Threads, timeLimit = timeLimit, verbose = max(verbose-2,0),
                                                IC = IC.blocks, n.IC = n+nval, p.IC = p,
                                                TH_IC = TH_IC)
      } else {
        # cat("\ncold start\n")
        result.block <- BOSO.multiple.coldstart(x = x, y = y, xval = xval, yval = yval,
                                                lambda=lambda.blocks, 
                                                intercept=intercept, 
                                                standardize=F,
                                                dfmin = 0, dfmax = p.block,
                                                costErrorVal = costErrorVal, costErrorTrain = costErrorVal, 
                                                costVars = costVars,
                                                Threads=Threads, timeLimit = timeLimit, verbose = max(verbose-2,0),
                                                IC = IC.blocks, n.IC =  n+nval, p.IC = p,
                                                TH_IC = TH_IC)
      }
      
      if (nrow(result.block$betas)>length(idx[[i]])) {result.block$betas <- result.block$betas[-1,]}
      indexSelectedIter[[numIter]][[i]] <- idx[[i]][result.block$betas[,which.min(result.block$score)]!=0]
      
      time_block[i] <- as.numeric(Sys.time() - time_block[i])
      
      if (verbose>1) { cat(paste0('Iteration ',numIter,'\t number of variables: ',length(idx[i]),
                                  '\t block size: ',maxVarsBlock,'\tSelected = ',length(indexSelectedIter[[numIter]][[i]]),
                                  '\tElapsed time= ',round(time_block[i],3),'\n'))}
      
    } 
    indexSelectedIter[[numIter]] <- sort(unlist(indexSelectedIter[[numIter]]))
    indexSelected <- indexSelectedIter[[numIter]]
    
    if (verbose>=3){ print(indexSelected) }
    
    # check if we need to stay in the loop
    if (length(indexSelected) <= maxVarsBlock*1.5){
      ContinueBlocks = F
    } else if (numIter>2) {
      if(length(indexSelectedIter[[numIter]])<length(indexSelectedIter[[numIter-1]])){
        ContinueBlocks = T
      } else if (length(union(indexSelected,indexSelectedIter[[numIter-1]]))==length(indexSelected) &
                 length(union(indexSelected,indexSelectedIter[[numIter-2]]))==length(indexSelected)){
        ContinueBlocks = F
      }
    }
  }
  
  
  ## if there is no variable after block strategy (eg: low SNR), exit the problem ####
  
  if (length(indexSelected)==0){
    result <- list(x = x,
                   y = y,
                   xval = xval,
                   yval = yval, 
                   IC = IC,
                   nlambda = nlambda,
                   lambda = lambda,
                   intercept = intercept,
                   standardize = standardize,
                   mx = mx, my = my, sx = sx,
                   dfmax = dfmax,
                   lambda.selected = 0,
                   p = p, n = n, nval = nval)
    
    result$betas <- c(mean(y), rep(0, p))
    if (!intercept) {result$betas <- result$betas[-1]}
    result$betas <- matrix(result$betas, ncol = 1)
    
    if (intercept){
      result$errorTrain = y-mean(y)
      result$errorVal = yval-mean(y)
    } else {
      result$errorTrain = y 
      result$errorVal = yval
    }
    
    class(result) = "BOSO"
    object <- result
    return(result)
  }
  
  
  
  ### Final problem ####
  
  
  dfmax.raw <- dfmax
  p.final <- length(indexSelected)
  dfmax <-  ifelse(p.final > dfmax , dfmax, p.final)
  numVarArray <- seq(0,dfmax)
  
  # subset of variables from the block strategy
  x = data.raw$x[,indexSelected]
  xval = data.raw$xval[,indexSelected]
  
  if (verbose) {  cat(paste0('Final problem:\t number of variables: ', dim(x)[2], ' \t \n' )) }
  
  if (warmstart) {
    # cat("\nWARMSTART\n")
    result.final <- BOSO.multiple.warmstart(x = x, y = y, xval = xval, yval = yval,
                                            lambda=lambda, 
                                            intercept=intercept, 
                                            standardize=F,
                                            dfmin = 0, dfmax = dfmax,
                                            costErrorVal = costErrorVal, costErrorTrain = costErrorVal, 
                                            costVars = costVars,
                                            Threads=Threads, timeLimit = timeLimit, verbose = max(verbose-1,0),
                                            IC = IC, n.IC = n+nval, p.IC = p,
                                            TH_IC = TH_IC)
  } else {
    # cat("\ncold start\n")
    result.final <- BOSO.multiple.coldstart(x = x, y = y, xval = xval, yval = yval,
                                            lambda=lambda, 
                                            intercept=intercept, 
                                            standardize=F,
                                            dfmin = 0, dfmax = dfmax,
                                            costErrorVal = costErrorVal, costErrorTrain = costErrorVal, 
                                            costVars = costVars,
                                            Threads=Threads, timeLimit = timeLimit, verbose = max(verbose-1,0),
                                            IC = IC, n.IC =  n+nval, p.IC = p,
                                            TH_IC = TH_IC)
  }
  
  
  # object <- obj
  
  idx = which.min(result.final$score)
  
  # Append a few things to the returned object
  result <- list(x = x,
                 y = y,
                 xval = xval,
                 yval = yval, 
                 IC = IC,
                 nlambda = nlambda,
                 lambda = lambda,
                 intercept = intercept,
                 standardize = standardize,
                 mx = mx, my = my, sx = sx,
                 dfmax = dfmax,
                 result.final = result.final, 
                 errorTrain = result.final$errorTrain[,idx], 
                 errorVal = result.final$errorVal[,idx],
                 lambda.selected = result.final$lambda.selected[idx],
                 p = p, n = n, nval = nval,
                 blockStrategy = indexSelectedIter)
  
  
  result$betas <- rep(0, p + 1)
  result$betas[c(1,indexSelected+1)] <- result.final$betas[,idx]
  
  result$betas <- matrix(result$betas, ncol = 1)
  
  class(result) = "BOSO"
  
  return(result)
}
