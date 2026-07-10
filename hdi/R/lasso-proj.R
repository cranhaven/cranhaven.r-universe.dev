lasso.proj <- function(x, y, family = "gaussian",
                       standardize = TRUE,
                       multiplecorr.method = "holm",
                       N = 10000,
                       parallel = FALSE, ncores = getOption("mc.cores", 2L),
                       betainit = "cv lasso",
                       sigma = NULL, ## sigma estimate provided by the user
                       Z = NULL,     ## Z or Thetahat provided by the user
                       verbose = FALSE,
                       return.Z = FALSE,
                       suppress.grouptesting = FALSE,
                       robust = FALSE,
                       do.ZnZ = FALSE)
{
  ## Purpose:
  ## An implementation of the LDPE method http://arxiv.org/abs/1110.2563
  ## which is identical to
  ## http://arxiv.org/abs/1303.0518
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Return values:
  ## pval: p-values for every parameter (individual tests)
  ## pval.corr:  multiple testing corrected p-values for every parameter
  ## betahat:    initial estimate by the scaled lasso of \beta^0
  ## bhat:       de-sparsified \beta^0 estimate used for p-value calculation
  ## sigmahat:   \sigma estimate coming from the lasso
  ## ----------------------------------------------------------------------
  ## Author: Ruben Dezeure, Date: 18 Oct 2013 (initial version),
  ## in part based on an implementation of the ridge projection method
  ## ridge-proj.R by P. Buehlmann + adaptations by L. Meier.

  ####################
  ## Get data ready ##
  ####################
  n <- nrow(x)
  p <- ncol(x)

  if(standardize)
    sds <- apply(x, 2, sd)
  else
    sds <- rep(1, p)

  pdata <- prepare.data(x = x,
                        y = y,
                        standardize = standardize,
                        family = family)
  x <- pdata$x
  y <- pdata$y
  
  ## force sigmahat to 1 when doing glm!
  if(family == "binomial")
    sigma <- 1

  ######################################
  ## Calculate Z using nodewise lasso ##
  ######################################
  
  Zout <- calculate.Z(x = x,
                      parallel = parallel,
                      ncores = ncores,
                      verbose = verbose,
                      Z = Z,
                      do.ZnZ = do.ZnZ)
  Z <- Zout$Z
  scaleZ <- Zout$scaleZ
  
  ###################################
  ## de-sparsified Lasso estimator ##
  ###################################
  
  ## Initial Lasso estimate
  initial.estimate <- initial.estimator(betainit = betainit, sigma = sigma,
                                        x = x, y = y)
  betalasso <- initial.estimate$beta.lasso
  sigmahat <- initial.estimate$sigmahat

  ## de-sparsified Lasso
  bproj <- despars.lasso.est(x = x,
                             y = y,
                             Z = Z,
                             betalasso = betalasso)
  
  #########################
  ## p-Value calculation ##
  #########################

  se <- est.stderr.despars.lasso(x = x,
                                 y = y,
                                 Z = Z,
                                 betalasso = betalasso,
                                 sigmahat = sigmahat,
                                 robust = robust)
  scaleb <- 1/se
  
  bprojrescaled <- bproj * scaleb
  
  ## Calculate p-value
  pval <- 2 * pnorm(abs(bprojrescaled), lower.tail = FALSE)

  cov2 <- crossprod(Z)

  #################################
  ## Multiple testing correction ##
  #################################

  pcorr <- if(multiplecorr.method == "WY") {
             ## Westfall-Young like procedure as in ridge projection method,
             ## P.Buhlmann & L.Meier
             ## method related to the Westfall - Young procedure
             ## constants left out since we'll rescale anyway
             ## otherwise cov2 <- crossprod(Z)/n
             p.adjust.wy(cov = cov2, pval = pval, N = N)
           } else if(multiplecorr.method %in% p.adjust.methods) {
             p.adjust(pval,method = multiplecorr.method)
           } else
             stop("Unknown multiple correction method specified")


  ##############################################
  ## Function to calculate p-value for groups ##
  ##############################################
  if(suppress.grouptesting){
    group.testing.function <- NULL
    cluster.group.testing.function <- NULL
  }else{
    pre <- preprocess.group.testing(N = N, cov = cov2, conservative = FALSE)
    
    group.testing.function <- function(group, conservative = TRUE){
      calculate.pvalue.for.group(brescaled    = bprojrescaled,
                                 group        = group,
                                 individual   = pval,
                                 ##correct    = TRUE,
                                 conservative = conservative,
                                 zz2          = pre)
    }
    
    cluster.group.testing.function <-
      get.clusterGroupTest.function(group.testing.function =
                                      group.testing.function, x = x)
  }
  
  ############################
  ## Return all information ##
  ############################

  out <- list(pval        = as.vector(pval),
              pval.corr   = pcorr,
              groupTest   = group.testing.function,
              clusterGroupTest = cluster.group.testing.function,
              sigmahat    = sigmahat,
              standardize = standardize,
              sds         = sds,
              bhat        = bproj / sds,
              se          = se / sds,
              betahat     = betalasso / sds,
              family      = family,
              method      = "lasso.proj",
              call        = match.call())
  if(return.Z)
    out <- c(out,
             list(Z = scale(Z,center=FALSE,scale=1/scaleZ)))##unrescale the Zs

  names(out$pval) <- names(out$pval.corr) <- names(out$bhat) <-
    names(out$sds) <- names(out$se) <- names(out$betahat) <-
      colnames(x)

  class(out) <- "hdi"
  return(out)
}

