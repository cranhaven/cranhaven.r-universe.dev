fitnessVarsel <- function(roles, par)
  # In roles vector:
  # 1 -- clustering variable
  # 0 -- redundant or noise variable
{
  X <- par$X
  covariates <- par$covariates
  names(roles) <- par$varnames
  G <- par$G
  checkG <- par$checkG

  clu <- which(roles == 1)
  oth <- which(roles == 0)
  Clu <- names(clu)
  Oth <- names(oth)

  # model on clustering variables -----------------------------------
  GG <- if ( checkG ) try( suppressWarnings( maxG(X[,Clu], G) ), silent = TRUE ) else G
  if ( class(GG) == "try-error" | all(GG == 1) ) {
    # no lca model can be fitted on the current set
    out <- structure(NA, variables = Clu, model = NA)
    return(out)
  }
  fitClus <- fitLCA(X[,Clu], G = GG, X = covariates, ctrlLCA = par$ctrlLCA)
  if ( fitClus$G == 1 ) {
    # no evidence of clustering on the current set
    out <- structure(NA, variables = Clu, model = NA)
    return(out)
  }
  #------------------------------------------------------------------

  # model on redundant and other variables --------------------------
  if ( length(Oth) != 0 ) {
    fitRedInd <- lapply( 1:length(Oth), function(i) {
      regressionStep(y = X[,Oth[i]], X = X[,Clu],
                     ctrlReg = par$ctrlReg, independence = par$independence)
      } )
  } else fitRedInd <- list(0)
  #------------------------------------------------------------------

  out <- structure(fitClus$bic + sum( unlist(fitRedInd) ),
                   variables = Clu, model = fitClus)
  return(out)
}



selGA <- function(X, G = 1:3, covariates = NULL,
                  ctrlGA = controlGA(), ctrlLCA = controlLCA(), ctrlReg = controlReg(),
                  checkG = TRUE, independence = FALSE, start = NULL, parallel = FALSE)
{
  varnames <- setdiff( colnames(X), colnames(covariates) )
  N <- nrow(X)
  M <- length(varnames)
  
  # select clustering variables, thus G > 1
  G <- setdiff(G, 1)
  
  par <- list(X = X, N = N, M = M, G = G, varnames = varnames,
              covariates = covariates, independence = independence,
              ctrlLCA = ctrlLCA, ctrlReg = ctrlReg, checkG = checkG)

  # selection
  mFitness <- memoise::memoise(fitnessVarsel)
  sel <- GA::ga(type = "binary",
                fitness = mFitness,
                par = par,
                nBits = M,
                suggestions = start,
                popSize = ctrlGA$popSize,
                maxiter = ctrlGA$maxiter,
                run = ctrlGA$run,
                pcrossover = ctrlGA$pcrossover,
                pmutation = ctrlGA$pmutation,
                elitism = ctrlGA$elitism,
                parallel = parallel,
                monitor = monitorGA,
                names = varnames)
  memoise::forget(mFitness)

  fit <- fitnessVarsel( c(sel@solution), par )
  out <- list( variables = attr(fit, "variables"), model = attr(fit, "model"),
               info = if ( nrow(sel@summary > 10) ) sel@summary[1:10,] else sel@summary,
               search = "ga", swap = NULL, independence = independence, GA = sel)
  return(out)
}


