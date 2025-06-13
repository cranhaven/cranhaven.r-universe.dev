#' Bayesian Dynamic Elastic Net
#'
#' Full Bayesian algorithm to detect hidden inputs in ODE based models.The algorithm 
#' is an extension of the Dynamic Elastic Net algorithm (Engelhardt et al. 2016) inspired by the Elastic-Net Regression.
#' 
#' Ordinary differential equations (ODEs) are a popular approach to quantitatively model molecular networks based on biological knowledge. 
#' However, such knowledge is typically restricted. Wrongly modeled biological mechanisms as well as relevant external influence factors 
#' that are not included into the model likely manifest in major discrepancies between model predictions and experimental data. 
#' Finding the exact reasons for such observed discrepancies can be quite challenging in practice. 
#' In order to address this issue we suggest a Bayesian approach to estimate hidden influences in ODE based models. 
#' The method can distinguish between exogenous and endogenous hidden influences. Thus, we can detect wrongly specified as well as missed 
#' molecular interactions in the model. 
#' The BDEN as a new and fully probabilistic approach, supports the modeler in an algorithmic manner to identify possible sources of errors 
#' in ODE based models on the basis of experimental data.  THE BDEN does not require pre-specified hyper-parameters. 
#' BDEN thus provides a systematic Bayesian computational method to identify target nodes and reconstruct the corresponding 
#' error signal including detection of missing and wrong molecular interactions within the assumed model. 
#' The method works for ODE based systems even with uncertain knowledge and noisy data. 
#' In contrast to approaches based on point estimates the Bayesian framework incorporates the given uncertainty and circumvents 
#' numerical pitfalls which frequently arise from optimization methods (Engelhardt et al. 2017).
#' 
#' For a complete example of the usage take a look into the vignette of the package.
#' 
#' 
#' @param odeModel             a object of class odeModel from the package seeds. The class saves the details of an experiment for easier manipulation and analysis. 
#' @param settings             initial model specific settings (automatically calculated based on the nominal model and data)
#' @param mcmc_component       sampling algorithm
#' @param loglikelihood_func   likelihood function
#' @param gibbs_update         gibbs algorithm
#' @param ode_sol              ode solver
#' @param NegativeStates       Negative states are allowed
#' @param numbertrialsstep     number of gibbs updates per timepoint. This should be at least 10. Values have direct influence on the runtime. 
#' @param numbertrialseps      number of samples per mcmc step. This should be greater than numberStates*500.Values have direct influence on the runtime.
#' @param numbertrialinner     number of inner samples. This should be greater 15 to guarantee a reasonable exploration of the sample space. Values have direct influnce on the runtime.
#' @param lambda               initial shrinkage parameter.
#' @param Grad_correct         correction factor for initial sigma estimate
#' @param alpha                mcmc tuning parameter (weighting of observed states)
#' @param beta_init                 mcmc tuning parameter (weighting of observed states)
#' @param printstatesignore    states ignored in final output (default = FALSE)

#' @return                     returns a results-object with default plot function
#' 
#' @examples 
#' \donttest{
#' data(bden_uvb)
#' 
#' results <- BDEN(odeModel          = Model,
#'                 lambda            = .001,
#'                 beta_init         = c(1,1,1,1,1),
#'                 numbertrialsstep  = 15,
#'                 numbertrialseps   = 2000,
#'                 numbertrialinner  = 10)
#' }
#' @export
#' 
#' 



BDEN <- function(odeModel,
                 settings,
                 mcmc_component,
                 loglikelihood_func,
                 gibbs_update,
                 ode_sol,
                 NegativeStates   = FALSE,
                 numbertrialsstep  = 15,
                 numbertrialseps   = NA,
                 numbertrialinner  = 25,
                 lambda            = .001,
                 Grad_correct      = 0,
                 alpha             = c(1,1,1,1),
                 beta_init         = c(1,1,1,1),
                 printstatesignore = FALSE){
  
  if (missing(settings)) {
    settings = SETTINGS
  }
  if (missing(mcmc_component)) {
    mcmc_component = MCMC_component
  }
  if (missing(loglikelihood_func)) {
    loglikelihood_func = LOGLIKELIHOOD_func
  }
  
  if (missing(gibbs_update)) {
    gibbs_update = GIBBS_update
  }
  
  if (missing(ode_sol)) {
    ode_sol = ode_solv
  }
  
  if(!missing(odeModel)){
    modelFunc <- odeModel@func
    parameters <- odeModel@parms
    systemInput <- odeModel@input
    #check if an input is assigned
    if (sum(colSums(systemInput)) == 0) {
      # the default initial value is a matrix with zero entries
      inputData <- NULL # no values assigned
    } else {
      inputData <- as.matrix(systemInput) # convert to matrix
    }
    measFunc <- odeModel@measFunc
    x0 <- odeModel@y
    measData <- odeModel@meas
    sd <- odeModel@sd
    
    
    observation_time <- measData[,1]
    observations     <- measData
    
    initialvalues    <- x0
    model            <- modelFunc
    numberstates     <- length(x0)
  }
 
  if (is.null(inputData)) {
    inputData=cbind(measData[,1],measData[,1]*0)
    colnames(inputData)=c('t','u')
  }

  if (length(alpha) != length(observations[1,])-1)     {alpha=rep(1,length(observations[1,]))}
  if (length(beta_init) != length(observations[1,])-1) {beta_init=rep(1,length(observations[1,]))}
  

  
  if(NegativeStates){
    createCompModel(modelFunc = model, parameters = parameters, bden = TRUE, nnStates = rep(0, numberstates))}
  else{
    createCompModel(modelFunc = model, parameters = parameters, bden = TRUE, nnStates = rep(1, numberstates))}
    
  temp_compiled_model <- compileModel()
  

  ##################################################################################
  X_MODEL        <- ode_sol(observation_time,initialvalues,parameters,inputData,matrix(rep(0,2*numberstates),2))


  base::print('Algorithm started. Sampling may take a while')
  print('################# BDEN INITIALIZED ################')
  
  X_ERROR        <- abs(abs(observations[,-1])-abs(measFunc(X_MODEL,parameters)))

  GRADIENT        <- matrix(0,length(observation_time)-1,length(observations[1,])-1-1)
  
  for (i in 1:(length(observations[1,])-1-(1+Grad_correct))){   
    GRADIENT[,i]       <- abs(diff(X_ERROR[,i])/diff(observation_time))
  }


  ##################################################################################
  COUNTER = 1
  COUNTER2 = 1
  
  MCMC_SET                  <- list()
  GIBBS_PAR_IT              <- list()
  EPSILON_IT                <- list()
  VAR                       <- list()
  MCMC_SET$STEP_trials            <- numbertrialsstep
  MCMC_SET$EPS_step_size          <- numbertrialseps
  MCMC_SET$EPS_step_size_inner    <- numbertrialinner
  MCMC_SET$BURNIN                 <- floor(numbertrialseps/numberstates*(3/5))
     
  
  EPSILON          <- matrix(0, nrow = dim(X_MODEL)[1]  , ncol = numberstates)
  EPSILONLOW       <- matrix(0, nrow = dim(X_MODEL)[1]  , ncol = numberstates)
  EPSILONUP        <- matrix(0, nrow = dim(X_MODEL)[1]  , ncol = numberstates)
  SIGMA            <- vector("list",dim(X_MODEL)[1]) 
  S                <- mean(GRADIENT)*1.2

  for (i in 1:length(SIGMA)){

SIGMA[[i]]      <- max(abs(diff(GRADIENT)))*0.25
  }
  BETA_LAMBDA      <- lambda
  
  
  GIBBS_PAR        <- SETTINGS(sd,numberstates,BETA_LAMBDA,alpha,beta_init)

  EPS_TIME         <- observation_time
  YINIT            <- initialvalues

  EPSILON_IT$CONT  <- matrix(0, nrow = MCMC_SET$STEP_trials  , ncol = numberstates)
  EPSILON_IT$ACT   <- matrix(0, nrow = 2  , ncol = numberstates)
  SOLUTION         <- YINIT
  SOLUTIONLOW      <- YINIT
  SOLUTIONUP       <- YINIT
  EPSILON_IT$NEW   <- YINIT

  stateUnscertainlower_OUT <- dim(measData[,-1])[2]*0
  stateUnscertainupper_OUT <- dim(measData[,-1])[2]*0


  
  X_OUTPUT                 <- measFunc(t(as.matrix(YINIT)),parameters)



  ##################################################################################
  
  
  for (STEP in 2:length(EPS_TIME)){
    
    print(paste0('Step ',STEP-1,' of ',length(EPS_TIME)-1))

    PROGRESS <- R.utils::ProgressBar(max=MCMC_SET$STEP_trials-1, ticks=MCMC_SET$STEP_trials-1 , stepLength=1, newlineWhenDone=TRUE)
   
    
    EPSILON_IT$Y0            <- EPSILON_IT$NEW
    GIBBS_PAR_IT$TAU         <- GIBBS_PAR$TAU
    GIBBS_PAR_IT$LAMBDA1     <- GIBBS_PAR$LAMBDA1
    GIBBS_PAR_IT$LAMBDA2     <- GIBBS_PAR$LAMBDA2
    EPSILON_IT$ACT[1,]       <- EPSILON[STEP-1,]
    EPSILON_IT$CONT[1,]      <- EPSILON[STEP-1,]
    VAR$SIGMA                <- SIGMA[[STEP]]  
    
    DummyB                   <- EPSILON_IT$CONT[1,]
    
    
    for (TRIALS in 2:MCMC_SET$STEP_trials){
      R.utils::increase(PROGRESS)
      
      VAR$DIAG                    <- diag((GIBBS_PAR_IT$TAU+GIBBS_PAR_IT$LAMBDA2)^-1)

      
      MCMC_RESULTS                 <- mcmc_component(loglikelihood_func, MCMC_SET$EPS_step_size, MCMC_SET$EPS_step_size_inner, EPSILON_IT$CONT[TRIALS-1,],S,
                                                     STEP,observations,EPSILON_IT$Y0,inputData,parameters,EPSILON_IT$ACT,VAR$SIGMA,VAR$DIAG,GIBBS_PAR,numberstates,MCMC_SET$BURNIN_inner,measFunc)
      
      

      

      EPSILON_IT$CONT[TRIALS,]    <- MCMC_RESULTS[length(MCMC_RESULTS[,1]),]

   

      EPSILON_IT$ACT[2,]          <- EPSILON_IT$CONT[TRIALS,] 
      
      G_U                         <- gibbs_update(VAR$DIAG,EPSILON_IT$ACT,
                                                  GIBBS_PAR$R,GIBBS_PAR$ROH,SIGMA[[1]],numberstates,VAR$SIGMA,
                                                  GIBBS_PAR_IT$LAMBDA2,GIBBS_PAR_IT$LAMBDA1,GIBBS_PAR_IT$TAU)  


      VAR$SIGMA                   <- G_U$SIGMA 
      GIBBS_PAR_IT$LAMBDA2        <- G_U$LAMBDA2 
      GIBBS_PAR_IT$LAMBDA1        <- G_U$LAMBDA1 
      GIBBS_PAR_IT$TAU            <- G_U$TAU 
      
      MCMC_RESULT_THIN            <- coda::mcmc(MCMC_RESULTS, start = MCMC_SET$BURNIN,end=dim(MCMC_RESULTS)[1],thin=5)
      DummyB                      <- rbind(DummyB,MCMC_RESULT_THIN[-1,])  
      
      
    }

    EPSILON[STEP,]              <- colMeans(EPSILON_IT$CONT[max(4,floor(MCMC_SET$STEP_trials/3)):length(EPSILON_IT$CONT[,1]),], na.rm = TRUE)

 
    
    
    
    EPSILONLOW[STEP,]           <- matrixStats::colQuantiles(DummyB, probs = 0.025, na.rm = TRUE) 
    EPSILONUP[STEP,]            <- matrixStats::colQuantiles(DummyB, probs = 0.975, na.rm = TRUE)
    
    
  
    EPSILON_IT$NEW              <- as.numeric(tail(ode_sol(EPS_TIME[c(STEP-1,STEP)],EPSILON_IT$Y0,parameters,inputData,EPSILON[c(STEP-1,STEP),]),1))
    
    C <- EPSILON_IT$NEW
    
    SOLUTION                    <- rbind(SOLUTION,C)
    
    
    DUMMY_SOLUTION_CI               <- matrix(0,dim(DummyB)[1],numberstates)
    for (iii in 2:dim(DummyB)[1]){
      DUMMY_SOLUTION_CI[iii,] <- as.numeric(tail(ode_sol(EPS_TIME[c(STEP-1,STEP)],EPSILON_IT$Y0,parameters,inputData,rbind(EPSILON[STEP-1,],DummyB[iii,])),1))
    }
    
    A <- matrixStats::colQuantiles(DUMMY_SOLUTION_CI[-1,], probs = 0.025, na.rm = TRUE)
    B <- matrixStats::colQuantiles(DUMMY_SOLUTION_CI[-1,], probs = 0.975, na.rm = TRUE) 
    

    SOLUTIONLOW                 <- rbind(SOLUTIONLOW,A)
    SOLUTIONUP                  <- rbind(SOLUTIONUP ,B)
    
    
    stateUnscertainlower_OUT    <- rbind(stateUnscertainlower_OUT,measFunc(t(as.matrix(A)),parameters))
    stateUnscertainupper_OUT    <- rbind(stateUnscertainupper_OUT,measFunc(t(as.matrix(B)),parameters))
    
    X_OUTPUT                    <- rbind(X_OUTPUT,measFunc(t(as.matrix(C)),parameters))
    
    
    
    SIGMA[[STEP]]               <- VAR$SIGMA
    
    
    
  }
  
  
  
  
  hiddenInpUnsclower             <- as.data.frame(cbind(observation_time,EPSILONLOW))
  colnames(hiddenInpUnsclower)  <- c("t",paste0('w',1: numberstates))
  
  hiddenInpUnscupper <- as.data.frame(cbind(observation_time,EPSILONUP))
  colnames(hiddenInpUnscupper)   <- c("t",paste0('w',1: numberstates))
  
  stateUnscertainlower <- as.data.frame(cbind(observation_time,SOLUTIONLOW))
  colnames(stateUnscertainlower) <- c("t",paste0('x',1: numberstates))
  
  stateUnscertainupper <- as.data.frame(cbind(observation_time,SOLUTIONUP))
  colnames(stateUnscertainupper) <- c("t",paste0('x',1: numberstates))
  
  outputEstimatesuncLower <- as.data.frame(cbind(observation_time,stateUnscertainlower_OUT))
  colnames(outputEstimatesuncLower) <- c("t",paste0('y',1:(ncol(measData[,-1]))))
  
  outputEstimatesuncUpper <- as.data.frame(cbind(observation_time,stateUnscertainupper_OUT))
  colnames(outputEstimatesuncUpper) <- c("t",paste0('y',1:(ncol(measData[,-1]))))
  
  states <- as.data.frame(cbind(observation_time,SOLUTION))
  colnames(states) <- c("t",paste0('x',1: numberstates))
  
  hiddenInp <- as.data.frame(cbind(observation_time,EPSILON))
  colnames(hiddenInp) <- c("t",paste0('w',1: numberstates))
  
  outputMeas <- as.data.frame(cbind(observation_time,X_OUTPUT))
  colnames(outputMeas) <- c("t",paste0('y',1:(ncol(measData[,-1]))))
  
  nomStates <- as.data.frame(cbind(observation_time,X_MODEL))
  colnames(nomStates) <- c("t",paste0('x',1: numberstates))
  

  
  dataError <- sd
  colnames(dataError) <- c("t",paste0('s',1:(ncol(sd)-1)))
  
  measData <- observations
  colnames(measData) <- c("t",paste0('y',1:(ncol(measData[,-1]))))
  
 
  if (!identical(printstatesignore, FALSE)){
    
    dataError                 <- dataError[-sort(printstatesignore+1)]
    measData                  <- measData[-sort(printstatesignore+1)]
    outputMeas                <- outputMeas[-sort(printstatesignore+1)]
    outputEstimatesuncLower   <- outputEstimatesuncLower[-sort(printstatesignore+1)]
    outputEstimatesuncUpper   <- outputEstimatesuncUpper[-sort(printstatesignore+1)]
  }
  
  
 
  res <- seeds::resultsSeeds(stateNominal = nomStates,
                             stateEstimates = states,
                             stateUnscertainLower =  stateUnscertainlower,
                             stateUnscertainUpper =  stateUnscertainupper,
                             hiddenInputEstimates = hiddenInp,
                             hiddenInputUncertainLower = hiddenInpUnsclower,
                             hiddenInputUncertainUpper = hiddenInpUnscupper,
                             outputEstimatesUncLower = outputEstimatesuncLower,
                             outputEstimatesUncUpper = outputEstimatesuncUpper,
                             outputEstimates = outputMeas,
                             Data = measData,
                             DataError = dataError
  )
  
  dyn.unload(temp_compiled_model)

  return(res)
  
}




