#' Censored log-likelihood function of the extremal Student model
#'
#' Compute the peaks-over-threshold censored negative log-likelihood function for the extremal Student model.
#'
#' The function computes the censored log-likelihood function based on the representation
#' developed by Ribatet (2013); see also Thibaud and Opitz (2015). Margins must have been
#' standardized, for instance to unit Frechet.
#'
#' @param obs List of vectors for which at least one component exceeds a high threshold.
#' @param loc Matrix of coordinates as given by \code{expand.grid()}.
#' @encoding UTF8
#' @param corrFun correlation function taking a vector of coordinates as input.
#' @param u Vector of thresholds under which to censor components.
#' @param p Number of samples used for quasi-Monte Carlo estimation. Must be a prime number.
#' @param vec Generating vector for the quasi-Monte Carlo procedure. For a given \code{p} and dimensionality,
#' can be computed using \code{genVecQMC}.
#' @param nu degrees of freedom of the Student process
#' @param nCores Number of cores used for the computation
#' @param cl Cluster instance as created by \code{makeCluster} of the \code{parallel} package.
#' @param likelihood vector of string specifying the contribution. Either \code{"mgp"} for multivariate generalized Pareto, 
#'  \code{"poisson"} for a Poisson contribution for the observations falling below or \code{"binom"} for a binomial contribution.
#' @param ntot integer number of observations below and above the threshold, to be used with Poisson or binomial likelihood
#' @param std logical; if \code{std = TRUE}, consider \code{obs/u} for scalar u and exceedances over 1 rather than \code{obs} \eqn{>} \code{u} for potentially vector \code{u}. This affects the value of the log-likelihood function. Default to \code{FALSE}.
#' @param ... Additional arguments passed to Cpp routine.
#' @references Thibaud, E. and T. Opitz (2015). Efficient inference and simulation for elliptical Pareto processes. Biometrika, 102(4), 855-870.
#' @references Ribatet, M. (2013). Spatial extremes: max-stable processes at work. JSFS, 154(2), 156-177.
#' @author Leo Belzile
#' @return Negative censored log-likelihood function for the set of observations \code{obs} and correlation function \code{corrFun}, with \code{attributes}  \code{exponentMeasure} for all of the \code{likelihood} type selected, in the order \code{"mgp"}, \code{"poisson"}, \code{"binom"}..
#' @examples
#' #Define correlation function
#' corrFun <- function(h, alpha = 1, lambda = 1){
#'    exp(-norm(h, type = "2")^alpha/lambda)
#' }
#'
#' #Define locations
#' loc <- expand.grid(1:4, 1:4)
#' 
#' #Compute generating vector
#' p <- 499L
#' latticeRule <- genVecQMC(p, (nrow(loc) - 1))
#' primeP <- latticeRule$primeP
#' vec <- latticeRule$genVec
#' 
#' #Simulate data
#' Sigma <- exp(-as.matrix(dist(loc))^0.8)
#' obs <- rExtremalStudentParetoProcess(n = 1000, nu = 5, Sigma = Sigma)
#' obs <- split(obs, row(obs))
#'
#' #Evaluate risk functional
#' maxima <- sapply(obs, max)
#' thresh <- quantile(maxima, 0.9)
#'
#' #Select exceedances
#' exceedances <- obs[maxima > thresh]
#'
#' #Compute log-likelihood function
#' eval <- censoredLikelihoodXS(exceedances, loc, corrFun, nu = 5, u = thresh, primeP, vec)
#' 
#' @export
censoredLikelihoodXS = function(obs,
                                loc,
                                corrFun,
                                nu,
                                u,
                                p = 499L,
                                vec = NULL,
                                nCores = 1L,
                                cl = NULL,
                                likelihood = "mgp", 
                                ntot = NULL,
                                std = FALSE,
                                ...){
  likelihood <- match.arg(likelihood, choices = c("mgp", "poisson","binom"), several.ok = TRUE)
  whichlik <- c("mgp", "poisson","binom") %in% likelihood
  #Duplicate threshold vector if too short
  if(std && length(u) > 1L){
    warning("Invalid threshold `u`: must be univariate. Switching to `std = FALSE`")
    std <- FALSE
  }
  if(length(u) == 1L){
    u <- rep(u, nrow(loc))
  }
  #Default for total number of observations is length of list
  if(is.null(ntot) && is.list(obs)){
    ntot <- length(obs)
  }
  if(is.matrix(obs)){ #Not converted to list
    if(is.null(ntot)){
      ntot <- nrow(obs)
    }
    #Keep only values above the threshold
    obs <- obs[apply(obs, 1, function(vec){isTRUE(any(vec > u))}),]
    obs <- split(obs, row(obs)) #create list
  }
  if(is.null(vec) && isTRUE(all.equal(p, 499L))){
    vec <- c(1, 209, 109, 191, 67, 51, 120, 93, 87, 157, 178, 45, 137, 84, 198,
             61, 232, 113, 182, 150, 57, 169, 141, 79, 132, 163, 38, 85, 131, 106,
             96, 165, 233, 179, 32, 228, 73, 233, 96, 131, 147, 32, 179, 165, 179,
             228, 32, 147, 165, 106, 228, 32, 179, 131, 32, 131, 228, 179, 106,
             165, 147, 179, 106, 147, 228, 165, 165, 179, 147, 228, 106, 106, 32,
             228, 147, 179, 32, 228, 106, 147, 32, 147, 228, 106, 179, 106, 179,
             228, 32, 147, 179, 32, 228, 106, 147, 147, 106, 179, 228, 32, 106,
             228, 147, 179, 32, 228, 147, 32, 179, 106, 32, 106, 179, 147, 147,
             179, 228, 106, 32, 106, 228, 179, 32, 147, 228, 179, 106, 32, 147,
             228, 106, 179, 32, 106, 147, 228, 179, 32, 228, 106, 179, 147, 32,
             147, 179, 106, 228, 106, 179, 147, 228, 32, 228, 179, 147, 106, 147,
             32, rep(179, 42)) / 499
  } else if(is.null(vec) && !isTRUE(all.equal(p, 499L))){
    stop("Invalid generating vector `vec`")
  }
  if(!inherits(obs, "list") || length(obs) < 1 || !inherits(obs[[1]], c("numeric","integer"))){
    stop('obs must be a list of vectors')
  }
  if(!inherits(loc, c("data.frame","matrix"))) {
    stop('`loc` must be a data frame of coordinates as generated by `expand.grid()` or a matrix of locations (one site per row)')
  }
  n <- length(obs)
  D <- nrow(loc)
  if((D - 1) > length(vec)){
    stop("Prime number and generating vector must be bigger than the dimension.")
  }
  if(D != length(obs[[1]])){
    stop('The size of the vectors of observations does not match grid size.')
  }
  if(!is.numeric(u)  || length(u) != D) {
    stop('`u` must be a vector with a length equal to the number of location.')
  }
  if(!is.numeric(p)) {
    stop('`p` must be a numeric.')
  }
  if(!is.numeric(vec)  || length(vec) < (D - 1)) {
    stop('`vec` must be generating vector with length at least equal to the number of locations.')
  }
  if(!is.numeric(nCores) || nCores < 1) {
    stop('`nCores` must a positive number of cores to use for parallel computing.')
  }
  if(nCores > 1 && !inherits(cl, "cluster")) {
    stop('For parallel computation, `cl` must an cluster created by `makeCluster` of the package parallel.')
  }
  ellipsis <- list(...)
  if(!is.null(ellipsis$nrep)){
    nrep <- as.integer(ellipsis$nrep)
    #number of Monte-Carlo replications over which to average calculations to estimate error. Default to 10.
  } else{ 
    nrep <- 10L
  }
  Sigma <- tryCatch({
    dists <- lapply(1:ncol(loc), function(i) {
      outer(loc[, i], loc[, i], "-")
    })
    
    computeCorrMat <- sapply(1:length(dists[[1]]), function(i){
      h <- rep(0, ncol(loc))
      for(j in 1:ncol(loc)){
        h[j] = dists[[j]][i]
      }
      corrFun(h)
    })
    matrix(computeCorrMat, D, D)
  }, warning = function(war) {
    war
  }, error = function(err) {
    stop('The correlation function is not valid for the locations provided.')
  })
  if(!isTRUE(all.equal(as.vector(diag(Sigma)), rep(1, D)))){
    if(all(c(isSymmetric(Sigma), eigen(Sigma, only.values = TRUE)$eigen > 1e-10))){
      warning("`corrFun` is not a correlation function.")
      Sigma <- cov2cor(Sigma)
    } else{
      stop("The correlation function provided by the user does not generate a positive definite matrix.")
    }
  }
  
   mleEst = function(i){
    #print(i)
    if(i < (D + 1)) {
      #Computation for the exponent measure
      #With std == TRUE, all u are the same components, so we get zero
      upperBound <- switch(std + 1L, exp((log(u[-i]) - log(u[i])) / nu) - Sigma[-i, i], 1- Sigma[-i, i])
      cov <- (Sigma[-i, -i] - Sigma[-i, i, drop = FALSE] %*% Sigma[i, -i, drop = FALSE]) / (nu + 1)
      # return(mvTProbQuasiMonteCarlo(p = p, upperBound = upperBound, cov = cov, nu = nu, genVec = vec [1:length(upperBound)])[1])
      # return(TruncatedNormal::mvTcdf(l = rep(-Inf, length(upperBound)), u = upperBound, Sig = cov, df = nu, n = 1e4)$prob)
      tmp <-.C(mvTProbCpp,
               as.integer(p),
               as.integer(length(upperBound)),
               as.double(cov),
               as.double(upperBound),
               as.double(nu + 1),
               as.double(vec[1:length(upperBound)]),
               as.integer(nrep),
               as.integer(FALSE),
               est = double(length=1),
               err = double(length=1),
               PACKAGE = "mvPot"
      )
      return(tmp$est)
      
    } else {
      j = i - D
      if(std){
        observation <- .subset2(obs, j) / u
        posUnder <- which(observation <= 1)
        posAbove <- which(observation > 1)
      } else{
        observation <- .subset2(obs, j)
        posUnder <- which(observation <= u)
        posAbove <- which(observation > u)
      }
      #Computation of the density function
      k <- D - length(posUnder) #number of points above threshold
      if(k == 0){
        stop("Invalid input. The list `obs` must contain vectors with at least one exceedance!") 
      }
      #Multivariate log normal density for uncensored
      if(k > 1){
        qrSigma <- qr(Sigma[posAbove, posAbove, drop = FALSE])
        #cholSigma <- chol(Sigma[posAbove,posAbove]) # Cholesky decomposition
        SigmaUncensInv <- solve.qr(qrSigma) #chol2inv(cholSigma)
        logdetSigma <- sum(log(abs(diag(qrSigma$qr)))) #sum(2*log(diag(chol(Sigma))))
        quad <- c(t(observation[posAbove]^(1/nu)) %*% SigmaUncensInv %*% (observation[posAbove]^(1/nu)))
        nll1 <-  - ((1 - k) * log(nu) + ((1 - k) / 2) * log(pi) - 0.5 * logdetSigma + lgamma((nu + k) / 2) - lgamma((nu + 1) / 2) +
                      (1 / nu - 1) * sum(log(observation[posAbove])) + (- (k + nu) / 2) * log(quad))
      } else {
        #One exceedance only -> parameters have no impact
        nll1 <- 2 * log(observation[posAbove])
        quad <- observation[posAbove]^(2/nu)
      }
      
      if(k < D){
        #If there are censored components
        if(k > 1) {
          muC = c(Sigma[posUnder, posAbove, drop = FALSE] %*% SigmaUncensInv %*% observation[posAbove]^(1/nu))
          sigmaC = quad / (k + nu) * (Sigma[posUnder, posUnder, drop = FALSE] - Sigma[posUnder, posAbove, drop = FALSE] %*%
                                        SigmaUncensInv %*% Sigma[posAbove, posUnder, drop = FALSE])
        } else { #k == 1
          muC = c(Sigma[posUnder, posAbove, drop = FALSE] %*% observation[posAbove]^(1/nu))
          sigmaC = quad / (k + nu) * (Sigma[posUnder, posUnder, drop = FALSE] - Sigma[posUnder, posAbove, drop = FALSE] %*%
                                        Sigma[posAbove, posUnder, drop = FALSE])
        }
        
        if(k == (D - 1)){ #1-dimensional censored component, use pt rather than QMC
          #rather than take (observation[posUnder]^(1 / nu)-as.vector(muC)) / sqrt(sigmaC)
          tmp = switch(std + 1L, stats::pt((u[posUnder]^(1/nu) - as.vector(muC)) / sqrt(sigmaC[1]), df = nu + k),
                       stats::pt((1 - as.vector(muC)) / sqrt(sigmaC[1]), df = nu + k))
          nll2 = - log(max(1e-323, tmp))
        } else {
          tmp <- switch(std + 1L, 
                        .C(mvTProbCpp,
                           as.integer(p),
                           as.integer(length(muC)),
                           as.double(sigmaC),
                           as.double(u[posUnder]^(1/nu)-as.vector(muC)),
                           as.double(nu + k),
                           as.double(vec[1:length(muC)]),
                           as.integer(nrep),
                           as.integer(FALSE),
                           est = double(length=1),
                           err = double(length=1),
                           PACKAGE = "mvPot"
                        ), .C(mvTProbCpp,
                              as.integer(p),
                              as.integer(length(muC)),
                              as.double(sigmaC),
                              as.double(1-as.vector(muC)),
                              as.double(nu + k),
                              as.double(vec[1:length(muC)]),
                              as.integer(nrep),
                              as.integer(FALSE),
                              est = double(length=1),
                              err = double(length=1),
                              PACKAGE = "mvPot"
                        ))
          
          
          # tmp <- TruncatedNormal::mvTcdf(l = rep(-Inf, nrow(sigmaC)), u = u[posUnder]^(1/nu)-as.vector(muC),
          #                                Sig = sigmaC, df = nu + k, n = 1e4)
          # tmp$est <- tmp$prob
          if(isTRUE(all.equal(tmp$est, 0, tolerance = 1e-30))){
            nll2 = - log(.Machine$double.xmin)
          } else {
            nll2 = - log(tmp$est)
          }
        }
      } else { #if all above
        nll2 = 0
      }
      return(nll1 + nll2)
    }
  }
  
  if(nCores > 1){
    # ------------ Parallel computation of the mle -------------------
    blockedMLE = function(i){
      blockStart <- (i-1) * blockSize + 1
      if(blockStart < (n + D + 1)){
        blockEnd  <- min( i * blockSize, (n + D))
        lapply(blockStart:blockEnd, mleEst)
      }
    }
    
    blockSize <- floor((D + n) / nCores) + 1
    pro <- parallel::parLapply(cl, 1:(nCores), blockedMLE)
  } else {
    pro <- lapply(1:(D + n), mleEst)
  }
  
  if(any(whichlik[2:3]) && ntot == n){
    warning("Total number of observations currently same as number of exceedances.")
  }
  exponentMeasure <- sum(unlist(pro)[1:D]/u)
  if(whichlik[3] && exponentMeasure > 1){
    warning("Exponent measure is greater than 1.")
  }
   res <- sum(unlist(pro)[(D + 1):(n + D)]) + 
     suppressWarnings(c(n * log(exponentMeasure), 
                      -ntot * exponentMeasure, 
                      (ntot - n) * log(1 - exponentMeasure)
                      )[whichlik])
   attributes(res) <- list("ExponentMeasure" = exponentMeasure, names = c("mgp", "poisson", "binom")[whichlik])
  return(res)
}
