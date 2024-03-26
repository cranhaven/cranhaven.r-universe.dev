#' BOSO.single and associates functions
#'
#' Compute the BOSO for use one block. This function calls ILOG IBM CPLEX with 
#' 'cplexAPI' to solve the optimization problem.
#' 
#'
#' @param x Input matrix, of dimension 'n' x 'p'. This is the data from the 
#' training partition. Its recommended to be class "matrix".
#' 
#' @param y Response variable for the training dataset. A matrix of one column 
#' or a vector, with 'n' elements
#' 
#' @param xval Input matrix, of dimension 'n' x 'p'. This is the data from the 
#' validation partition. Its recommended to be class "matrix".
#' 
#' @param yval Response variable for the validation dataset. A matrix of one 
#' column or a vector, with 'n' elements
#' 
#' @param intercept Boolean variable to indicate if intercept should be added 
#' or not. Default is false.
#' 
#' @param standardize Boolean variable to indicate if data should be scaled 
#' according to mean(x) mean(y) and sd(x) or not. Default is false.
#' 
#' @param nlambda The number of lambda values. Default is 100.
#' 
#' @param lambda.min.ratio Smallest value for lambda, as a fraction of 
#' lambda.max, the (data derived) entry value
#' 
#' @param lambda A user supplied lambda sequence. Typical usage is to have the 
#' program compute its own lambda sequence based on nlambda and 
#' lambda.min.ratio. Supplying a value of lambda overrides this. 
#' WARNING: use with care
#' 
#' @param IC information criterion to be used. Default is 'eBIC'.
#' @param n.IC number of events for the information criterion. 
#' @param p.IC number of initial variables for the  information criterion.
#' 
#' @param dfmin Minimum number of variables to be included in the problem. The 
#' intercept is not included in this number. Default is 0.
#' 
#' @param dfmax Maximum number of variables to be included in the problem. The 
#' intercept is not included in this number. Default is min(p,n).
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
#' @param Threads CPLEX parameter, number of cores that cplex is allowed to use.
#' Default is 0 (automatic).
#' 
#' @param timeLimit CPLEX parameter, time limit per problem provided to CPLEX.
#' Default is 1e75 (infinite time).
#' 
#' @param verbose print progress. Default is FALSE
#' 
#' @param TH_IC is the ratio over one that the information criterion must 
#' increase to be STOP. Default is 1e-3.
#'   
#' @description Function to run a single block BOSO problem, generating one 
#' CPLEX object and re-runing it for the different K.
#' 
#' @return A `BOSO` object.
#' 
#'
#' @import Matrix
#' @import methods
#' @author Luis V. Valcarcel
#' @export BOSO.multiple.warmstart

BOSO.multiple.warmstart = function(x, y, xval, yval, nlambda=100,
                                   IC = "eBIC", n.IC = NULL, p.IC = NULL,
                                   lambda.min.ratio=ifelse(nrow(x)<ncol(x),0.01,0.0001),
                                   lambda=NULL, intercept=TRUE, standardize=FALSE,
                                   dfmin = 0, dfmax = NULL,
                                   costErrorVal = 1, costErrorTrain = 0, costVars = 0,
                                   Threads=0, timeLimit = 1e75, verbose = F,
                                   TH_IC = 1e-3) {
  
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
  if(!is(lambda.min.ratio,"numeric")){stop("lambda.min.ratio must be numeric")}
  if(!is(nlambda,"numeric")){stop("nlambda must be numeric")}
  if(!is(TH_IC,"numeric")){stop("TH_IC must be numeric")}

  # Set up data
  x = as.matrix(x)
  y = as.numeric(y)
  xval = as.matrix(xval)
  yval = as.numeric(yval)
  n = nrow(x)
  p = ncol(x)
  
  # standarze?
  if (standardize) {
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
  } else {
    # Reset nlambda if a specific lambda sequence is passed
    nlambda <- length(lambda)
  }
  
  # Generate the intercept in the x matrix if necessary 
  x <- cbind(1, x)
  xval <- cbind(1, xval)

  
  ## Generate the index for constraints ###
  nFeatures <- dim(x)[2]
  nEventsTrain <- dim(x)[1]
  nEventsVal <- dim(xval)[1]
  
  # First constrain, Yval = Xval*beta + errorVal
  Eq1 <- 1:nEventsVal
  # Second constrain, Ytr = Xtr*beta + errorTrain
  Eq2 <- Eq1[length(Eq1)] + (1:nEventsTrain)
  # Third constrain: minimum and maximum number of betas activated
  Eq3 <- Eq2[length(Eq2)] + 1:2
  # Fourth constrain: only one alpha activated
  Eq4 <- Eq3[length(Eq3)] + 1
  nCon <- Eq4
  
  
  
  ## Generate the index for variables ###
  
  # First set of variables: betas (first beta is the intercept)
  betas <- 1:nFeatures
  # Second: atomic fluxes of unlabeled carbons
  Zbetas <- betas[length(betas)] + (1:nFeatures)
  # Third: Errors
  errorVal <- Zbetas[length(Zbetas)] + (1:nEventsVal)
  errorTrain <- errorVal[length(errorVal)] + (1:nEventsTrain)
  # Fourth: ridge
  alphas <- errorTrain[length(errorTrain)] + (1:nFeatures)
  Zalphas <- alphas[length(alphas)] + (1:nlambda)
  nVar <- Zalphas[length(Zalphas)]
  
  
  ## Define the constraints ###
  A <- Matrix(data = 0, nrow = nCon, ncol = nVar,sparse = T)
  rhs <- rep(0, nCon)
  sense <- rep("", nCon)
  
  #First constrain, Yval = xval*beta + errorVal
  A[Eq1,betas] = xval
  A[Eq1,errorVal] = + diag(nEventsVal)
  rhs[Eq1] = yval
  sense[Eq1] <- "E"
  
  # Second constrain, Ytr = Xtr*beta + errorTrain
  A[Eq2,betas] = x
  A[Eq2,errorTrain] = + diag(nEventsTrain)
  rhs[Eq2] = y
  sense[Eq2] <- "E"
  
  # Third constrain: minimum and maximum number of betas activated
  A[Eq3,Zbetas[-1]] <- 1
  rhs[Eq3] <- c(dfmin, dfmax)
  sense[Eq3] <- c("G", "L")
  
  # Fourth constrain: only one alpha activated
  A[Eq4,Zalphas] <- 1
  rhs[Eq4] <- 1
  sense[Eq4] <- c("E")
  
  
  ## Generate the data for the variables ###
  
  # Prepare data structures for the problem object
  # Variable lower bounds
  lb <- c(rep(-cplexAPI::CPX_INFBOUND, length(betas)),
          rep(0, length(Zbetas)),
          rep(-cplexAPI::CPX_INFBOUND, length(errorVal)),
          rep(-cplexAPI::CPX_INFBOUND, length(errorTrain)),
          rep(-cplexAPI::CPX_INFBOUND, length(alphas)),
          rep(0, length(Zalphas)))
  # Variable upper bounds
  ub <- c(rep(+cplexAPI::CPX_INFBOUND, length(betas)),
          rep(1, length(Zbetas)),
          rep(+cplexAPI::CPX_INFBOUND, length(errorVal)),
          rep(+cplexAPI::CPX_INFBOUND, length(errorTrain)),
          rep(+cplexAPI::CPX_INFBOUND, length(alphas)),
          rep(1, length(Zalphas)))
  
  xctype <- c(rep("C", length(betas)),
              rep("B", length(Zbetas)),
              rep("C", length(errorVal)),
              rep("C", length(errorTrain)),
              rep("C", length(alphas)),
              rep("B", length(Zalphas)))
  
  
  
  # change parameters if to include or exclude intercetp
  if (intercept){
    lb[Zbetas[1]] <- 1
    ub[Zbetas[1]] <- 1
  } else {
    lb[betas[1]] <- 0
    ub[betas[1]] <- 0
    lb[Zbetas[1]] <- 0
    ub[Zbetas[1]] <- 0
  }
  
  
  ## Solve the problem using cplexAPI
  
  # Open a CPLEX environment
  env <- cplexAPI::openEnvCPLEX()
  
  # Create a problem object
  prob <- cplexAPI::initProbCPLEX(env, pname = "BOSO ridge" )
  
  # Set the problem 
  cplexAPI::copyLpwNamesCPLEX(env = env, lp = prob, 
                              nCols = nVar, nRows = nCon,
                              lpdir = cplexAPI::CPX_MIN, # set as minimization problem
                              objf = rep(0, nVar), # objective function
                              rhs = rhs, sense = sense, # right hand size and sign
                              matbeg = A@p[-length(A@p)],
                              matcnt = apply(A!=0,2,sum),
                              matind = A@i, 
                              matval = A@x, # set matrix A using sparse Matrix
                              lb = lb, ub = ub, # upper and lower bound
                              rngval = NULL)
  
  # Set the type of variables 
  cplexAPI::copyColTypeCPLEX(env = env, lp = prob, xctype = xctype)
  
  # Add the quadratic part
  cplexAPI::copyQPsepCPLEX(env = env,
                           lp = prob,
                           qsepvec = c(rep(0, length(betas)), 
                                       0, rep(2 * costVars, length(Zbetas)-1),  # cost of variables
                                       rep(2 * costErrorVal, length(errorVal)),  # cost of Error in the validation set
                                       rep(2 * costErrorTrain, length(errorTrain)),  # cost of Error in the Training set
                                       rep(0, length(alphas)),  
                                       rep(0, length(Zalphas))))  
  
  # add the indicator contraints
  XtX = t(x) %*%  x
  XtY = t(x) %*%  y
  
  ## add constraint for equation 3-4
  # z = 1  -->  Xtr'*Ytr == Xtr'*Xtr*beta + alpha
  for (i in 1:nFeatures){
    cplexAPI::addIndConstrCPLEX(env = env,
                                lp = prob,
                                indvar = Zbetas[i]-1,
                                complemented = F,
                                nzcnt = nFeatures+1,
                                rhs = XtY[i],
                                sense = "E",
                                linind = c(betas, alphas[i])-1,
                                linval = c(XtX[i,], 1))
  }
  
  ## add constraint for equation 5-6
  # z = 0 <-> beta = 0
  for (i in 1:nFeatures){
    cplexAPI::addIndConstrCPLEX(env = env,
                                lp = prob,
                                indvar = Zbetas[i]-1,
                                complemented = T,
                                nzcnt = 1,
                                rhs = 0,
                                sense = "E",
                                linind = betas[i]-1,
                                linval = 1)
  }
  
  ## add constraint for equation 7-8
  # Zalpha(j) = 1  -->  alpha(i) = lambda(j)*beta(i), any 'i'
  for (j in 1:nlambda){
    for (i in 1:nFeatures){
      cplexAPI::addIndConstrCPLEX(env = env,
                                  lp = prob,
                                  indvar = Zalphas[j]-1,
                                  complemented = F,
                                  nzcnt = 2,
                                  rhs = 0,
                                  sense = "E",
                                  linind = c(betas[i], alphas[i])-1,
                                  linval = c(lambda[j], -1))
    }
  }
  
  
  # set the parameters
  cplexAPI::setDefaultParmCPLEX(env)
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_Threads, value = Threads )# number of cores
  cplexAPI::setDblParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_TimeLimit, value = timeLimit ) # timelimit
  
  # parameters from the tune
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_Branch, value = 1)
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_VariableSelect, value = 3)
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_PresolveNode, value = 2)
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Cuts_FlowCovers, value = 1 )
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_Probe, value = 3 )
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Cuts_MIRCut, value = 1 )
  
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_VariableSelect, value = 4 )
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Limits_CutPasses, value = 1 )
  cplexAPI::setIntParmCPLEX(env = env, parm = cplexAPI::CPXPARAM_MIP_Strategy_RINSHeur, value = 100 )
  
  
  # solve for each K ####
  numVarArray <- seq(0,dfmax)
  # if (!intercept) {numVarArray <- numVarArray[-1]}
  
  
  result <- list(score = rep(Inf, length(numVarArray)),
                 time = rep(Inf, length(numVarArray)),
                 lambda.selected = rep(NA, length(numVarArray)),
                 betas = matrix(NA, nrow = p + 1, ncol = length(numVarArray)),
                 Zbetas = matrix(NA, nrow = p + 1, ncol = length(numVarArray)),
                 errorVal = matrix(NA, nrow = nrow(xval), ncol = length(numVarArray)),
                 errorTrain = matrix(NA, nrow = nrow(x), ncol = length(numVarArray)))
  
  # mipStart.cplexAPI <- NULL
  
  for (kk in 1:length(numVarArray)){
    # for (kk in 3){
    # kk <- 2
    # print(cplexAPI::getNumMIPstartsCPLEX(env = env, lp = prob))
    k = as.integer(numVarArray[kk])
    if (verbose) {
      cat(paste0(IC, ':\t Force ',k,'\t nVar: ',dim(x)[2]))
    }
    
    result$time[kk] <- cplexAPI::getTimeCPLEX(env = env)
    # set the apropiate k
    cplexAPI::chgRhsCPLEX(env = env,
                          lp = prob,
                          nrows = 2,
                          ind = Eq3-1,
                          val = c(k, k))
    
    
    
    # Solve the problem using the simplex algorithm
    cplexAPI::mipoptCPLEX(env = env, lp = prob)
    
    # Retrieve solution after optimization
    sol.cplexAPI <- cplexAPI::solutionCPLEX(env = env, lp = prob)
    sol.cplexAPI$status <- cplexAPI::status_codeCPLEX(env, sol.cplexAPI$lpstat)
    sol.cplexAPI$indexVar <- list(betas = betas,
                                  Zbetas = Zbetas,
                                  errorVal = errorVal,
                                  errorTrain = errorTrain,
                                  alphas = alphas,
                                  Zalphas = Zalphas) 
    sol.cplexAPI$indexCon <- list(Eq1 = Eq1,
                                  Eq2 = Eq2,
                                  Eq3 = Eq3,
                                  Eq4 = Eq4) 
    
    obj <- list(sol = sol.cplexAPI,
                nlambda = nlambda,
                lambda = lambda,
                intercept = intercept,
                standardize = standardize)
    
    obj$x = x[,-1]; obj$xval = xval[,-1] # remove intercept from the x matrix
    obj$y = y; obj$yval = yval 
    obj$mx = mx; obj$my = my; obj$sx = sx; 
    obj$dfmin = k; obj$dfmax = k
    obj$lambda.selected = which(sol.cplexAPI$x[sol.cplexAPI$indexVar$Zalphas]>0.5)
    obj$lambda.selected = lambda[obj$lambda.selected]
    obj$betas <- obj$sol$x[obj$sol$indexVar$betas]
    obj$n = n; obj$p = p
    class(obj) = "BOSO"
    result$time[kk] <- cplexAPI::getTimeCPLEX(env = env) - result$time[kk]
    
    obj$standardize <- standardize # redefine, in the function BOSO.single to standardize again
    
    # save and calculate scores of each iteration
    result$betas[,kk] <- coef.BOSO(obj, beta0=T)
    result$Zbetas[,kk] <- coef.BOSO(obj, beta0=T)!=0
    result$errorVal[,kk] <- yval - predict.BOSO(obj, obj$xval) 
    result$errorTrain[,kk] <- y - predict.BOSO(obj, obj$x)
    result$lambda.selected[kk] <- obj$lambda.selected
    result$score[kk] <- ICscoreCalculation(obj, IC, n.IC, p.IC)
    
    
    if (verbose){
      cat(paste0('\t',IC,'=', round(result$score[kk],2),
                 ' \t',IC,'_min= ', round(min(result$score[1:kk]),2),'\tElapsed time = ', round(result$time[kk],3), '\n'))
    } #else (verbose){
      #cat('\n')
    #}
    
    if (kk>1){
      if (result$score[kk] > (min(result$score[1:(kk-1)]) + abs(min(result$score[1:(kk-1)]))*TH_IC))
        break
    }
  }
  
  # Free memory, allocated to the problem object
  cplexAPI::delProbCPLEX(env, prob)
  cplexAPI::closeEnvCPLEX(env)
  
  return(result)
}
