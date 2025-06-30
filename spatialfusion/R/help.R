# set default values for the priors
setPrior <- function(point.betaPrior, area.betaPrior, tausqPrior, phiPrior, designmatPrior){
  if(missing(point.betaPrior)) point.betaPrior = list(distr = "normal",pars=c(0, 10))
  if(missing(area.betaPrior)) area.betaPrior = list(distr = "normal",pars=c(0, 10))
  if(missing(tausqPrior)) tausqPrior <- list(distr="inv_gamma", pars=c(2, 1))
  if(missing(phiPrior)) stop("A prior for the spatial range parameter 'phi' must be provided.")
  if(missing(designmatPrior)) designmatPrior = list(distr = "normal",pars=c(1, 1))
  for (prior in list(point.betaPrior, area.betaPrior, tausqPrior, phiPrior, designmatPrior)){
    if(! all(names(prior) %in% c("distr", "pars"))) stop("the priors must be a named list consisting of 'distr' for the distribution name and 'pars' for the hyperparameter values")
  }
  return(list(point.beta = point.betaPrior, area.beta = area.betaPrior, tausq = tausqPrior, phi = phiPrior, designmat = designmatPrior))
}


summaries <- function(x){
  data.frame(mean = mean(x), sd = sd(x), `0.025quant` = quantile(x, probs = 0.025), `0.5quant` = median(x), `0.975quant` = quantile(x, probs = 0.975))
}

genOutcome <- function(dist, n, x, beta, w, variance){
  switch(dist,
         "normal" = rnorm(n, x %*% beta + w, sd = sqrt(variance)),
         "poisson" = rpois(n, exp(x %*% beta + w)),
         "bernoulli" = rbinom(n, 1, exp(x %*% beta + w)/(1 + exp(x %*% beta + w))),
         stop("no match was found for the specified distribution."))
}

# Stan NNGP Supporting functions ----------------------------------------------------

# get the neighborhood index for each location
# s: location matrix, each row records coordinates of one point
# m: number of neighborhood
i_index <- function(i, s, n.neighbor) {
  if(n.neighbor >= (i - 1)) {im <- 1:(i - 1)}
  else 	{
    dist <- rdist(s[c(1,i),], s[c(1:(i-1)), ])[-1,]
    im <- sort(order(dist)[1:n.neighbor])
  }
  return(im)
}
# distance matrix for location i and its neighbors
i_dist <- function(i, neighbor_index, s)	dist(s[c(i, neighbor_index[[i - 1]]), ])
get_index_dist <- function(s, n.neighbor) {
  n = nrow(s)
  n.neighbor = min(n.neighbor, n-1)
  # get index of neighborhood
  neighbor_index <- sapply(2:n, i_index, s, n.neighbor)
  # get distance matrix for each i
  neighbor_dist <- sapply(2:n, i_dist, neighbor_index, s)
  return(list(i = neighbor_index, d = neighbor_dist))
}
# get the output for c function
get_neardistM <- function (ind, ind_distM_d, n.neighbor) {
  if (ind < n.neighbor ){l = ind } else {l = n.neighbor};
  M_i <- rep(0, n.neighbor * (n.neighbor - 1) / 2);
  if (l == 1) {}
  else{
    M_i[1: (l * (l - 1) / 2)] <-
      c(ind_distM_d[[ind]])[(l + 1): (l * (l + 1) / 2)]
  }
  return(M_i)
}
get_neardist <- function (ind, ind_distM_d, n.neighbor) {
  if (ind < n.neighbor ){l = ind } else {l = n.neighbor};
  D_i <- rep(0, n.neighbor);
  D_i[1:l]<-c(ind_distM_d[[ind]])[1:l]
  return( D_i)
}
get_nearind <- function (ind, ind_distM_i, n.neighbor) {
  if (ind < n.neighbor ){l = ind } else {l = n.neighbor};
  D_i <- rep(0, n.neighbor);
  D_i[1:l]<-c(ind_distM_i[[ind]])[1:l]
  return( D_i)
}
# Stan model diagnostics --------------------------------------------------

checkConvergence <- function(samples){
  pars <- c(grep("beta",samples@model_pars, value = T),
            grep("Z_[1-9]+",samples@model_pars, value = T),
            grep("tau_sq",samples@model_pars, value = T), "phi")
  rhats <- data.frame(summary(samples, pars = pars)$summary)
  rhats <- rhats[-nrow(rhats),] # delete the loglikelihood row
  rhatPar <- rownames(rhats)[which(rhats$Rhat > 1.1)]

  if (length(rhatPar) > 0){
    warning("there are evidence of non-convergence for parameter: ", paste(rhatPar, collapse = ", "), ". Their potential scale reduction factors (Brooks and Gelman, 1998) are greater than 1.1.")
  } else {
    cat("\nNOTE: there are no evidence of non-convergence since all parameters have potential scale reduction factors (Brooks and Gelman, 1998) less than 1.1.")
  }
}

checkDivergence <- function(samples, adaptDelta){
  sp <- get_sampler_params(samples, inc_warmup = FALSE)
  n_d <- sum(sapply(sp, FUN = function(x) {
    if ("divergent__" %in% colnames(x)) return(sum(x[, "divergent__"])) else return(0)
  }))
  if (n_d > 0) {
    warning("there were ", n_d, " divergent transitions after warmup, the joint posterior distribution is not sufficiently explored.\n",
            " Re-run the model with adaptDelta > ", adaptDelta, " may help, or results can be unreliable.", call. = FALSE)}
}


# functions to support fusionPlot -----------------------------------------

getPriorFixedStan <- function(priors, n.pointbeta, n.areabeta){
  if (length(n.pointbeta) == 0) n.pointbeta <- 0
  if (length(n.areabeta) == 0) n.areabeta <- 0

  keys <- data.frame(rsyntax <- c("dnorm", "dcauchy", "dlnorm", "dinvgamma", "dunif"),
                     stansyntax <- c("normal", "cauchy", "lognormal", "inv_gamma", "uniform"))

  pointbeta <- paste0(keys$rsyntax[keys$stansyntax == priors$point.beta$distr],
                      "(x, ", priors$point.beta$pars[1], ", ", priors$point.beta$pars[2],")")

  areabeta <- paste0(keys$rsyntax[keys$stansyntax == priors$area.beta$distr],
                      "(x, ", priors$area.beta$pars[1], ", ", priors$area.beta$pars[2],")")

  return(c(rep(pointbeta, n.pointbeta), rep(areabeta, n.areabeta)))
}


getPriorFixedINLA <- function(priors){
  out <- c()
  for (prior in priors){
    out <- c(out, paste0("dnorm(x, ",prior$prior.mean, ", ", sqrt(1/prior$prior.prec), ")"))
  }
  return(out)
}


getPriorLatentStan <- function(priors, names){
  keys <- data.frame(rsyntax <- c("dnorm", "dcauchy", "dlnorm", "dgamma", "dinv_gamma", "dunif"),
                     stansyntax <- c("normal", "cauchy", "lognormal", "gamma", "inv_gamma", "uniform"))

  out <- c()
  for (name in names){
    if (grepl("tau", name)){
      out <- c(out, paste0(keys$rsyntax[keys$stansyntax == priors$tausq$distr],
                           "(x, ", priors$tausq$pars[1], ", ", priors$tausq$pars[2],")"))
    } else if (grepl("phi", name)){
      out <- c(out, paste0(keys$rsyntax[keys$stansyntax == priors$phi$distr],
                           "(x, ", priors$phi$pars[1], ", ", priors$phi$pars[2],")"))
    } else {
      out <- c(out, paste0(keys$rsyntax[keys$stansyntax == priors$designmat$distr],
                           "(x, ", priors$designmat$pars[1], ", ", priors$designmat$pars[2],")"))
    }
  }

  return(out)
}

# priors range
drange <- function(x, range0, Prange){
  lambda <- -log(Prange) * range0
  lambda * x^-2 * exp(-lambda * x^-1)
}

# priors stdev
dstdev <- function(x, sigma0, Psigma){
  lambda <- -log(Psigma)/sigma0
  lambda * exp(-lambda * x)
}


getPriorLatentINLA <- function(model, priors, n_w){
  out <- c()
  # priors on Gaussian precision (only default)
  g.indices <- which(model$.args$family == "gaussian")
  if (length(g.indices) > 0){
    for (g.index in g.indices){
      out <- c(out, paste0("dgamma(x, ", model$all.hyper$family[[g.index]]$hyper$theta1$param[1],
                           ", ", model$all.hyper$family[[g.index]]$hyper$theta1$param[2], ")"))
    }
  }

  # priors on range
  out <- c(out, rep(paste0("drange(x, ", priors$prior.range[1], ", ", priors$prior.range[2], ")"), n_w))

  # priors on stdev
  out <- c(out, rep(paste0("dstdev(x, ", priors$prior.sigma[1], ", ", priors$prior.sigma[2], ")"), n_w))

  # priors on betas of copied latent process
  n.betas <- length(model$all.hyper$random) - 1
  beta <- paste0("dnorm(x, ", model$all.hyper$random[[n_w+1]]$hyper$theta$param[1], ", ", model$all.hyper$random[[n_w+1]]$hyper$theta$param[2], ")")
  out <- c(out, rep(beta, n.betas))

  return(out)
}

# inverse gamma density
dinv_gamma <- function(x, shape, scale){
  exp(dgamma(1/x, shape, 1/scale, log = TRUE) - 2 * log(x))
}

# return dimensions of posterior vs. prior plot
getDim <- function(n){
  if (n < 2) return(c(1,1))
  n1 <- ceiling(sqrt(n))
  if (n1^2 - n1 < n) {
    n2 <- n1
  } else {
    n2 <- n1 - 1
  }
  if (abs(n1 - 3) > abs(n2 - 3)){
    return(c(n1, n2))
  } else {
    return(c(n2, n1))
  }
}

# return xlims of posterior distribution for latent parameters
getLim <- function(x){
  # x is a matrix in the form of density
  range(x[x[,2] > max(x[,2]) * 1e-6, 1])
}

