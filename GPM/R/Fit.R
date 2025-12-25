Fit <-  function(X, Y, CorrType = 'G', Eps = 10^(seq(-1, -12)), AnaGr = NULL, Nopt = 5,TraceIt = 0,
                 MaxIter = 100, Seed = 1, LowerBound = NULL, UpperBound = NULL,
                 StopFlag = 1, Progress = 0, DoParallel = 0, Ncores = NULL) {

  set.seed(Seed)

  ## Check the inputs
  if (Progress != 0){
    cat('*** Checking the inputs ...\n')
  }
  if (missing(X) || missing(Y)){
    stop('     X and Y must be provided.')
  }
  if (!all(is.finite(X)) || !is.numeric(X)){
    stop('     All the elements of X must be finite numbers.')
  }
  if (!all(is.finite(Y)) || !is.numeric(Y)){
    stop('     All the elements of Y must be finite numbers.')
  }
  if (is.matrix(X) == FALSE) {
    X <- as.matrix(X)
  }
  if (is.matrix(Y) == FALSE) {
    Y <- as.matrix(Y)
  }
  if (nrow(X)!= nrow(Y)){
    stop('     The number of rows (i.e., observations) in X and Y should match!')
  }
  if (is.character(CorrType)){
    CorrType <- toupper(CorrType)
  } else {
    stop('     CorrType should be a character (options are: "G", "PE", "LB", and "LBG")')
  }
  if (CorrType!= 'G' && CorrType!= 'PE' && CorrType!= 'LBG' && CorrType!='LB'){
    stop(paste('     The type of the correlation function is not determined correctly.', 
               'Supported functions types are "G", "PE", "LB", and "LBG".', sep = ''))
  }
  if (any(Eps < (10^(round(log10(.Machine$double.eps))+3)))){
    stop(paste('     Increase the smallest member of Eps. The minimum allowable is ', 
               toString(10^(round(log10(.Machine$double.eps))+3))))
  }
  if (any(diff(Eps) > 0)){
    stop('     The elements of Eps should be in a descending order.')
  }

  n <- nrow(X); dx <- ncol(X); dy <- ncol(Y);
  
  if (is.null(AnaGr)){
    if (CorrType == 'G' || CorrType == 'PE'){
      AnaGr <- 1
    }else{
      AnaGr <- 0
    }
  }else{
    if (!is.numeric(AnaGr)){
      stop('     AnaGr Should be numeric; non-zero to use analytical gradients if CorrType == "G".\n')
    }
    if (CorrType != 'G' && CorrType != 'PE'){
      cat('     Analytical Gradient is only available when CorrType == "G". AnaGr is set to 0.\n')
      AnaGr <- 0
    }
  }
  
  if (AnaGr ==0){
    GrF = NULL
  }else{
    GrF = NLogL_G
  }
  
  Omega_min_default <- -2
  Omega_max_default <- 2
  Zeta_min_default <- 1e-6
  Zeta_max_default <- 1 - 1e-4
  Gamma_min_default <- 1e-6
  Gamma_max_default <- 1

  if (is.null(LowerBound)){
    LB_Def <- 1
    if (CorrType == 'G') {LowerBound = rep(x = Omega_min_default, times = dx)}
    else if (CorrType == 'PE') {LowerBound = c(rep(x = Omega_min_default, times = dx), 1)}
    else if (CorrType == 'LBG') {LowerBound = c(rep(x = Omega_min_default, times = dx), 
                                                Zeta_min_default)}
    else if (CorrType == 'LB') {LowerBound = c(rep(x = Omega_min_default, times = dx), 
                                               Zeta_min_default, Gamma_min_default)}
  } else {
    LB_Def <- 0
    if (!is.vector(LowerBound)) LowerBound = as.vector(LowerBound)
    if (CorrType == 'G' && length(LowerBound) != dx){
      stop(paste('     The size of the provided LowerBound is incorrect! It should be', toString(dx)))}
    else if (CorrType == 'PE' && length(LowerBound) != (1 + dx)){
      stop(paste('     The size of the provided LowerBound is incorrect! It should be', toString(dx + 1)))}
    else if (CorrType == 'LBG' && length(LowerBound) != (1 + dx)){
      stop(paste('     The size of the provided LowerBound is incorrect! It should be', toString(dx + 1)))}
    else if (CorrType == 'LB' && length(LowerBound) != (2 + dx)){
      stop(paste('     The size of the provided LowerBound is incorrect! It should be', toString(dx + 2)))}
  }

  if (is.null(UpperBound)){
    UB_Def <- 1
    if (CorrType == 'G') {UpperBound = rep(x = Omega_max_default, times = dx)}
    else if (CorrType == 'PE') {UpperBound = c(rep(x = Omega_max_default, times = dx), 2)}
    else if (CorrType == 'LBG') {UpperBound = c(rep(x = Omega_max_default, times = dx), 
                                                Zeta_max_default)}
    else if (CorrType == 'LB') {UpperBound = c(rep(x = Omega_max_default, times = dx), 
                                               Zeta_max_default, Gamma_max_default)}
  } else {
    UB_Def <- 0
    if (!is.vector(UpperBound)) UpperBound = as.vector(UpperBound)
    if (CorrType == 'G' && length(UpperBound) != dx){
      stop(paste('     The size of the provided UpperBound is incorrect! It should be', toString(dx)))}
    else if (CorrType == 'PE' && length(UpperBound) != (1 + dx)){
      stop(paste('     The size of the provided UpperBound is incorrect! It should be', toString(dx + 1)))}
    else if (CorrType == 'LBG' && length(UpperBound) != (1 + dx)){
      stop(paste('     The size of the provided UpperBound is incorrect! It should be', toString(dx + 1)))}
    else if (CorrType == 'LB' && length(UpperBound) != (2 + dx)){
      stop(paste('     The size of the provided UpperBound is incorrect! It should be', toString(dx + 2)))}
  }
  if (CorrType == 'PE' && (LowerBound[dx + 1] < 1 || UpperBound[dx+ 1] > 2)){
    stop(paste('     The exponent in PE should be between ', toString(c(1, 2)), ' (inclusive).'))
  } else if (CorrType == 'LBG' && (LowerBound[dx + 1] <= 0 || UpperBound[dx+ 1] > Zeta_max_default)){
    stop(paste('     Beta in LBG should be between ', toString(c(0, 1)), ' (exclusive).'))
  } else if (CorrType == 'LB' && (LowerBound[dx+1]<=0 || UpperBound[dx+1]>Zeta_max_default || 
                  LowerBound[dx+2]<Gamma_min_default || UpperBound[dx+2]>Gamma_max_default)){
    stop('     The provided range for Beta and/or Gamma is not acceptable.')
  }
  if (DoParallel != 0){
    AvailableCores <- detectCores()
    if (AvailableCores == 1){
      cat('    Only 1 core is available. Parallel computing not possible.\n')
      DoParallel <- 0
      Ncores <- AvailableCores
    }else {
      if (is.null(Ncores)){
        Ncores <- AvailableCores
      }
      if (AvailableCores < Ncores){
        cat('    More cores are requested than available. ', toString(AvailableCores), 
            ' cores are used.\n')
        Ncores <- AvailableCores
      }
      cl <- makeCluster(Ncores[1])
      registerDoParallel(cl)
      cat('    ', toString(Ncores), ' cores are used.\n')
    }
  }else{
    DoParallel <- 0
    Ncores <- 1
  }


  Setting <- list("CorrType" = CorrType, "Eps" = Eps, "MaxIter" = MaxIter, "AnaGr" = AnaGr,
                 "Seed" = Seed, "StopFlag" = StopFlag, "Nopt" = Nopt,
                 "LowerBound" = LowerBound, "UpperBound" = UpperBound, "TraceIt" = TraceIt, 
                 "DoParallel" = DoParallel, "Ncores" = Ncores)

  ## Normalize the data
  N_hyperC <- length(LowerBound)
  Xmin <- apply(X, 2, min)
  Xmax <- apply(X, 2, max)

  XN <- t((t(X)-Xmin)/(Xmax-Xmin))
  Ymin <- apply(Y, 2, min)
  Yrange <- apply(Y, 2, max) - Ymin
  YN <- t((t(Y)-Ymin)/Yrange)
  if (CorrType == 'LBG' || CorrType == 'LB'){
    XN0 <- XN[1, ]
    YN0 <- YN[1, ]
    XN <- t(t(XN) - XN0)[2:n, ]
    YN <- t(t(YN) - YN0)[2:n, , drop = FALSE]
    n <- n - 1
  }
  Fn <- matrix(data = 1, nrow = n, ncol = 1)

  ptm <- proc.time()

  A <- maximinLHS(Nopt, N_hyperC)
  #A <- sobol(Nopt, dim = N_hyperC, scrambling = 3, seed = Seed)
  Tries <- length(Eps)
  N_unique_local_opt <- matrix(0, Tries, 1)
  OptimHist <- NULL
  OptimHist$Var_int[[1]] <- t(t(A)*(UpperBound - LowerBound) + LowerBound)
  CTRL <- c(trace = TraceIt, maxit = MaxIter,  REPORT = 1, lmm = 15, factr = 1e6, pgtol = 1e-8)
  
  for (i in 1:Tries){
    Var_int <- OptimHist$Var_int[[i]]
    Eps_i <- Eps[i]
    Nopt_i <- nrow(Var_int)
    Var <- matrix(0, Nopt_i, N_hyperC)
    Obj_Fun <- matrix(0, Nopt_i, 1)
    Exit_Flag <- matrix(0, Nopt_i, 1)
    if (LB_Def == 1 && i > 1){
      LowerBound[1: dx] <- -20
    }
    if (UB_Def == 1 && i > 1){
      UpperBound[1: dx] <- 5
    }
    if (DoParallel == 0 || Nopt_i == 1){
      for (j in 1: Nopt_i){
        temp <- stats::optim(Var_int[j, ], NLogL, gr = GrF, XN, YN, CorrType, Eps_i, Fn, n, dy,
                             method = 'L-BFGS-B', lower = LowerBound, upper = UpperBound, control = CTRL)
        Var[j, ] <- temp$par
        Obj_Fun[j] <- temp$value
        Exit_Flag[j] <- temp$convergence
      }
    }else{
        temp <- foreach (j = 1: Nopt_i, .inorder=FALSE) %dopar% {
          stats::optim(Var_int[j, ], NLogL, gr = GrF, XN, YN, CorrType, Eps_i, Fn, n, dy,
                       method = 'L-BFGS-B', lower = LowerBound, upper = UpperBound, control = CTRL)
        }
        for (j in 1: Nopt_i){
          Var[j, ] <- temp[[j]]$par
          Obj_Fun[j] <- temp[[j]]$value
          Exit_Flag[j] <- temp[[j]]$convergence
        }
    }

    
    ID = sort(Obj_Fun, index.return=TRUE)$ix
    Obj_Fun <- as.matrix(Obj_Fun[ID, drop = FALSE])
    Exit_Flag <- as.matrix(Exit_Flag[ID, drop = FALSE])
    Var <- as.matrix(Var[ID, , drop = FALSE])

    if (Progress != 0){
      Table <- cbind(apply(round(Var, 3), 1, toString), round(Obj_Fun, 2))
      colnames(Table)<- c('Estimated Hyperparameters', ' NLogL')
      Table <- as.table(Table)
      rownames(Table) <- 1:Nopt_i
      cat(sprintf('\n\t\tOptimization for Eps = %.1e\n', Eps[i]))
      print(Table, row.names = FALSE)
    }

    rownames(Obj_Fun) <- 1:Nopt_i
    Obj_Fun_unique  <-  as.matrix(unique(round(Obj_Fun, 2)))
    Exit_unique <- as.matrix(as.data.frame(Exit_Flag)[c(rownames(Obj_Fun_unique)), ])
    Var_unique <- as.matrix(as.data.frame(Var)[c(rownames(Obj_Fun_unique)), ])

    if (Progress != 0 && Nopt_i > 1){
      Table <- cbind(apply(round(Var_unique, 3), 1, toString), round(Obj_Fun_unique, 2))
      colnames(Table)<- c('Estimated Hyperparameters', ' NLogL')
      Table <- as.table(Table)
      rownames(Table) <- 1:nrow(Obj_Fun_unique)
      cat('\n\t\tThe unique local optima are:\n')
      print(Table, row.names = FALSE)
    }
    
    ALL <-  Auxil(Var_unique[1, ], XN, YN, CorrType, Eps[i], Fn, n, dy)

    OptimHist$Nug_opt[[i]] <- ALL$Nug_opt
    OptimHist$Raw_MinEig[[i]] <-  ALL$Raw_MinEig
    OptimHist$Var[[i]] <-  Var
    OptimHist$Var_unique[[i]] <-  Var_unique
    OptimHist$Obj_Fun[[i]] <-  Obj_Fun
    OptimHist$Obj_Fun_unique[[i]] <- Obj_Fun_unique
    OptimHist$Obj_Fun_best[[i]] <-  Obj_Fun[1]
    OptimHist$Exit_unique[[i]] <- Exit_unique

    if (i > 1){
      if ((StopFlag != 0) && (OptimHist$Obj_Fun_unique[[i]][1] > OptimHist$Obj_Fun_unique[[i-1]][1])){
        break
      }
    }
    if (i < Tries){
      OptimHist$Var_int[[i + 1]]  <-  Var_unique
    }
  }
  if (DoParallel != 0){
    stopCluster(cl) #stop cluster
  }
  
  Cost <- proc.time()[3] - ptm[3]
  # Organize the optimization results
  #cat('*** Summary of the optimization results (sorted and rounded):\n')
  ID <- which.min(OptimHist$Obj_Fun_best)
  w <- as.matrix(OptimHist$Var[[ID]][1, ])

  ## Post-processing
  ALL <-  Auxil(w, XN, YN, CorrType, Eps[ID], Fn, n, dy)

  if (Progress != 0){
    cat('\nThe best model is fitted via Eps = ', toString(Eps[ID]), '. A nugget of ', 
        toString(ALL$Nug_opt), ' has been used.')
  }

  L <- ALL$L
  Rinv_YN <- CppSolve(t(L), CppSolve(L, YN))
  if (CorrType == 'PE' || CorrType=='G'){
    RinvFn <- CppSolve(t(L), CppSolve(L, Fn))
    FnTRinvFn <- t(Fn)%*%RinvFn
    B <- ALL$B
    Sigma2 <- ALL$Sigma2
    if (CorrType=='G'){
      Theta <- w; Power <- 2;
    } else {
      Theta <- w[1:dx]; Power <- w[dx+1]
      if (Power > 1.999){
        Power <- 2
      }
    }
    Parameters <- list('FnTRinvFn' = FnTRinvFn[1], 'B' = B, 'Sigma2' = Sigma2, 'RinvFn' = RinvFn,
                      'Rinv_YN' = Rinv_YN, 'Theta' = Theta, 'Power' = Power)
  } else {
    Alpha <- ALL$Alpha
    if (CorrType == 'LBG'){
      A <- w[1:dx]; Beta <- w[dx+1]; Gamma <- 1
    } else {
      A <- w[1:dx]; Beta <- w[dx+1]; Gamma <- w[dx+2]
      if (Gamma>0.999) Gamma <- 1
    }
    Parameters <- list('XN0' = XN0, 'YN0' = YN0, 'Alpha' = Alpha, 'Rinv_YN' = Rinv_YN,
                      'A' = A, 'Beta' = Beta, 'Gamma' = Gamma)
  }
  ## Save the results
  Model <- NULL
  Model$CovFunc <- list('CorrType' = CorrType, 'Parameters' = Parameters, 
                        'LowerBound' = LowerBound, 'UpperBound' = UpperBound)
  Model$Data <- list('XN' = XN, 'n' = n, 'Xmin' = Xmin, 'Xmax' = Xmax,  'YN' = YN, 
                     'dy' = dy, 'Yrange' = Yrange, 'Ymin' = Ymin)
  Model$Details <- list('Fn' = Fn, 'L' = L, 'Raw_MinEig' = ALL$Raw_MinEig, 'Nug_opt' = ALL$Nug_opt, 
                        'Cost' = Cost, 'MinNLogL' = OptimHist$Obj_Fun_best[ID])
  Model$OptimHist <- OptimHist
  Model$Setting <- Setting
  class(Model) <- 'GPM'

  return(Model)
}
