crossvalidation = function(objclogitLasso,
                           K = 10,
                           gpe = NULL) {
  x = objclogitLasso$x_rec
  x = as.matrix(x)
  fraction = objclogitLasso$fraction
  standardize = objclogitLasso$arg$standardize
  nopenalize = objclogitLasso$arg$nopenalize
  adaptive = objclogitLasso$arg$adaptive
  separate = objclogitLasso$arg$separate
  ols = objclogitLasso$arg$ols
  maxit = objclogitLasso$arg$maxit
  maxitB = objclogitLasso$arg$maxitB
  thr = objclogitLasso$arg$thr
  tol = objclogitLasso$arg$tol
  epsilon = objclogitLasso$arg$epsilon
  log = objclogitLasso$arg$log
  trace = objclogitLasso$arg$trace
  M = objclogitLasso$arg$M
  y = objclogitLasso$arg$y
  strata = objclogitLasso$arg$strata
  p.fact = objclogitLasso$arg$p.fact
  nbfraction = length(fraction)
  
  d = dim(x)
  n = d[1]
  m = d[2]

  #groups
  if (is.null(gpe)) {
    K1 = length(unique(strata))
    nFolds = min(c(K, K1))
    folds = split(sample(seq(K1)), rep(1:nFolds, length = K1))
    #save(folds,file=paste(address,"/gpe.Rdata",sep=''))
  } else{
    folds = gpe
  }
  
  predmat = NULL
  nblambda = 0
  prob_min = 1e-09
  prob_max = 1 - prob_min
  
  
  #Cross-validation
  for (i in seq(K)) {
    #cat("Calcul groupe ",i,"\n")
    omit_strata = folds[[i]]
    omit = (1:n)[strata %in% omit_strata]
    #LEARNING
    if (!adaptive) {
      fit = reg.diff(
        x[-omit, ],
        y[-omit],
        strata[-omit],
        standardize = FALSE,
        fraction = fraction,
        nopenalize = nopenalize,
        adaptive = adaptive,
        separate = separate,
        ols = ols,
        maxit = maxit,
        maxitB = maxitB,
        thr = thr,
        tol = tol,
        epsilon = epsilon,
        log = log,
        trace = F
      )
    } else{
      fit = reg.diff(
        x[-omit, ],
        y[-omit],
        strata[-omit],
        standardize = FALSE,
        fraction = fraction,
        nopenalize = nopenalize,
        p.fact = p.fact,
        adaptive = adaptive,
        separate = separate,
        ols = ols,
        maxit = maxit,
        maxitB = maxitB,
        thr = thr,
        tol = tol,
        epsilon = epsilon,
        log = log
      )
    }
    
    #TESTING
    pred = sigmoid(x[omit, ], t(fit$beta))
    if (is.null(predmat)) {
      predmat = matrix(0, n, nbfraction)
    }
    predmat[omit, ] = pred
  }
  
  #Calcul of deviance
  cvraw = {
    predmat = pmin(pmax(predmat, prob_min), prob_max)
    lp = log(predmat)
    - 2 * lp
  }
  cvm = apply(cvraw, 2, mean)
  ind = which.min(cvm)
  
  #Estimation with optimal lambda and all the x's
  if (!adaptive) {
    fit = reg.diff(
      x,
      y,
      strata,
      standardize = FALSE,
      fraction = fraction[ind],
      nopenalize = nopenalize,
      adaptive = adaptive,
      separate = separate,
      ols = ols,
      maxit = maxit,
      maxitB = maxitB,
      thr = thr,
      tol = tol,
      epsilon = epsilon,
      log = log,
      trace = F,
      remove = T
    )
  } else{
    fit = reg.diff(
      x,
      y,
      strata,
      standardize = FALSE,
      fraction = fraction[ind],
      nopenalize = nopenalize,
      p.fact = p.fact,
      adaptive = adaptive,
      separate = separate,
      ols = ols,
      maxit = maxit,
      maxitB = maxitB,
      thr = thr,
      tol = tol,
      epsilon = epsilon,
      log = log,
      remove = T
    )
  }
  beta = fit$beta
  lambdaopt = fraction[ind]
  
  
  list(
    lambda = fraction,
    mean_cv = cvm,
    beta = beta,
    lambdaopt = lambdaopt,
    folds = folds
  )
}