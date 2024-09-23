#' Censored log-likelihood function for the Brown--Resnick model.
#'
#' Compute the peaks-over-threshold censored negative log-likelihood function for the Brown--Resnick model.
#'
#' The function computes the censored negative log-likelihood function based on the representation
#' developed by Wadsworth et al. (2014) and Engelke et al. (2015). Margins must have been
#' standardized first, for instance to the unit Frechet scale.
#'
#' @author Raphael de Fondeville
#' @param obs List of vectors for which at least one component exceeds a high threshold.
#' @param loc Matrix of coordinates as given by \code{expand.grid()}.
#' @param vario Semi-variogram function taking a vector of coordinates as input.
#' @param u Vector of threshold under which to censor components.
#' @param p Number of samples used for quasi-Monte Carlo estimation. Must be a prime number.
#' @param vec Generating vector for the quasi-Monte Carlo procedure. For a given prime \code{p} and dimension,
#' can be computed using \code{genVecQMC}.
#' @param nCores Number of cores used for the computation
#' @param cl Cluster instance as created by \code{makeCluster} of the \code{parallel} package.
#' @param likelihood vector of strings specifying the contribution. Either \code{"mgp"} for multivariate generalized Pareto,
#'  \code{"poisson"} for a Poisson contribution for the observations falling below or \code{"binom"} for a binomial contribution.
#' @param ntot integer number of observations below and above the threshold, to be used with Poisson or binomial likelihood
#' @param ... Additional arguments passed to Cpp routine.
#' @return Negative censored log-likelihood for the set of observations \code{obs} and semi-variogram \code{vario} with \code{attributes}  \code{exponentMeasure} for all of the \code{likelihood} type selected, in the order \code{"mgp"}, \code{"poisson"}, \code{"binom"}.
#' @examples
#' #Define semi-variogram function
#' vario <- function(h){
#'    0.5 * norm(h, type = "2")^1.5
#' }
#'
#' #Define locations
#' loc <- expand.grid(1:4, 1:4)
#'
#' #Simulate data
#' obs <- simulPareto(1000, loc, vario)
#'
#' #Evaluate risk functional
#' maxima <- sapply(obs, max)
#' thres <- quantile(maxima, 0.9)
#'
#' #Select exceedances
#' exceedances <- obs[maxima > thres]
#'
#' #Compute generating vector
#' p <- 499
#' latticeRule <- genVecQMC(p, (nrow(loc) - 1))
#' primeP <- latticeRule$primeP
#' vec <- latticeRule$genVec
#'
#'
#' #Compute log-likelihood function
#' censoredLikelihoodBR(obs = exceedances, loc = loc, vario = vario,
#'  u = thres, p = primeP, vec = vec, ntot = 1000)
#' @export
#' @useDynLib mvPot mvtNormCpp
#' @references Wadsworth, J. L. and J. A. Tawn (2014). Efficient inference for spatial extreme value
#'  processes associated to log-Gaussian random functions. Biometrika, 101(1), 1-15.
#' @references Asadi, P., Davison A. C. and S. Engelke (2015). Extremes on River Networks.
#'  Annals of Applied Statistics, 9(4), 2023-2050.
censoredLikelihoodBR <- function(obs,
                              loc,
                              vario,
                              u,
                              p = 499L,
                              vec = NULL,
                              nCores = 1L,
                              cl = NULL,
                              likelihood = "mgp",
                              ntot = NULL,
                              ...){
  likelihood <- match.arg(likelihood, choices = c("mgp", "poisson","binom"), several.ok = TRUE)
  whichlik <- c("mgp", "poisson","binom") %in% likelihood
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
  if(!inherits(loc, c("matrix", "data.frame"))) {
    stop('`loc` must be a data frame of coordinates as generated by `expand.grid()` or a matrix of locations (one site per row)')
  }
  if(length(u) == 1L){
    u <- rep(u, nrow(loc))
  } else{
   if(abs(max(u) - min(u)) >  1e-10){
     warning("The threshold level on the unit Frechet scale in vector `u` should be the same for all components!")
  }
  }
  n <- length(obs)
  D <- nrow(loc)

  if(D != length(obs[[1]])){
    stop('The size of the vectors of observations does not match grid size.')
  }
  if(!is.numeric(u)  || length(u) != D) {
    stop('`u` must be a vector whose length is equal to the number of location.')
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
  gamma <- tryCatch({
    dists <- lapply(1:ncol(loc), function(i) {
      outer(loc[, i], loc[, i], "-")
    })

    computeVarMat <- sapply(1:length(dists[[1]]), function(i){
      h <- rep(0, ncol(loc))
      for(j in 1:ncol(loc)){
        h[j] = dists[[j]][i]
      }
      vario(h)
    })
    matrix(computeVarMat, D, D)
  }, warning = function(war) {
    war
  }, error = function(err) {
    stop('The semi-variogram is not valid for the locations provided.')
  })

  identityVector = matrix(1, (D - 1), 1)

  mleEst = function(i){
    if(i < (D + 1)) {
      #Computation for the exponent measure
      #thres = rep(1, D)
      #Expression can be found in Huser and Davison, Biometrika (2013)
      upperBound = sqrt(gamma[-i, i]/2) # - log(thres[i]/thres[-i])/sqrt(2*gamma[-i, i]) ##superfluous b/c log(1)=0
      cov = (gamma[-i, i] %*% t(identityVector) + t(gamma[i, -i] %*% t(identityVector)) - gamma[-i, -i]) / (2*sqrt(gamma[-i, i] %*% t(identityVector)*t(gamma[i, -i] %*% t(identityVector))))

      tmp <-.C(mvtNormCpp,
               as.integer(p),
               as.integer(length(upperBound)),
               as.double(cov),
               as.double(upperBound),
               as.double(vec[1:length(upperBound)]),
               as.integer(nrep),
               as.integer(FALSE),
               est = double(length=1),
               err = double(length=1),
               PACKAGE = "mvPot"
      )
      tmp$est

    } else {
      j = i - D

      observation <- .subset2(obs, j) / u

      #Computation of the density function
      posUnder <- which(observation < 1)
      posAbove <- which(observation >= 1)

      k <- D - length(posUnder)

      #Multivariate log normal density for uncensored
      identityVector = matrix(1, (D - 1), 1)
      sigma = ( outer(gamma[-posAbove[1], posAbove[1]], gamma[posAbove[1], -posAbove[1]], "+") - gamma[-posAbove[1], -posAbove[1]])

      #Shift indexes to match the new covariance matrix indexes
      posAboveShifted = posAbove[-1] - 1
      posUnderShifted = posUnder
      posUnderShifted[posUnder > posAbove[1]] = posUnder[posUnder > posAbove[1]] - 1

      if(k > 1){
        invCovMat = MASS::ginv(sigma[posAboveShifted, posAboveShifted, drop = FALSE])
        logdetA = determinant(sigma[posAboveShifted, posAboveShifted, drop = FALSE], logarithm = TRUE)$modulus
        omega <- log(observation[posAbove][-1]/observation[posAbove][1]) + gamma[posAbove[-1], posAbove[1]]

        mle1 <- 1 / 2 * (logdetA + (k-1) * log(2 * pi) + t(omega)  %*%  invCovMat  %*%  omega)  + log(observation[posAbove][1]) + sum(log(observation[posAbove]))
      } else {
        #One exceedance only -> parameters have no impact
        mle1 <- 2 * log(observation[posAbove][1]) 
      }

      if(k < D){
        if(k > 1) {
          muC = ( - log(observation[posAbove][1]) + gamma[posAbove[1], posUnder]) - sigma[posUnderShifted, posAboveShifted, drop = FALSE]  %*%  invCovMat  %*%  omega
          sigmaC = sigma[posUnderShifted, posUnderShifted, drop = FALSE] - sigma[posUnderShifted, posAboveShifted, drop = FALSE]  %*%  invCovMat  %*%  sigma[posAboveShifted, posUnderShifted, drop = FALSE]
        } else {
          muC = ( - log(observation[posAbove][1]) + gamma[posAbove[1], posUnder, drop = FALSE])
          sigmaC = sigma
        }
        if(k == (D-1)){
          tmp = stats::pnorm(as.vector(muC), sd = sigmaC)
          mle2 = max(1e-323, tmp)
          mle2 = - log(mle2)
        } else {
          tmp <- .C(mvtNormCpp,
                    as.integer(p),
                    as.integer(length(muC)),
                    as.double(sigmaC),
                    as.double(as.vector(muC)),
                    as.double(vec[1:length(muC)]),
                    as.integer(nrep),
                    as.integer(FALSE),
                    est = double(length=1),
                    err = double(length=1),
                    PACKAGE = "mvPot"
          )
          if(tmp$est == 0){
            mle2 = - log(.Machine$double.xmin)
          }else{
            mle2 = - log(tmp$est)
          }
        }
      } else {
        mle2 = 0
      }
      mle1 + mle2
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
  exponentMeasure <- sum(unlist(pro)[1:D] / u)
  if(any(whichlik[2:3]) && ntot == n){
    warning("Total number of observations currently same as number of exceedances.")
  }
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


#' @export
#' @rdname censoredLikelihoodBR
censoredLikelihood <- function(obs, loc, vario, u, p = 499L, vec = NULL, nCores = 1L, cl = NULL){
  .Deprecated(new = "censoredLikelihood", package = "mvPot",
              msg = "Please use the function `censoredLikelihoodBR` to estimate the censored likelihood of the Brown-Resnick process.",
              old = "censoredLikelihood")
  censoredLikelihoodBR(obs = obs, loc = loc, vario = vario, u = u, p = p, vec = vec, nCores= nCores, cl = cl)
}
