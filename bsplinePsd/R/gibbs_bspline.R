#' @title Metropolis-within-Gibbs sampler for spectral inference of a stationary time series using a B-spline prior
#' @description This function updates the B-spline prior using the Whittle likelihood and obtains samples from the pseudo-posterior to infer the spectral density of a stationary time series.
#' @details The function \code{gibbs_bspline} is an implementation of the (serial version of the) MCMC algorithm presented in Edwards et al. (2018).  This algorithm uses a nonparametric B-spline prior to estimate the spectral density of a stationary time series and can be considered a generalisation of the algorithm of Choudhuri et al. (2004), which used the Bernstein polynomial prior.  A Dirichlet process prior is used to find the weights for the B-spline densities used in the finite mixture and a seperate and independent Dirichlet process prior used to place knots.  The algorithm therefore allows for a data-driven choice of the number of knots/mixtures and their locations.
#' @param data numeric vector
#' @param Ntotal total number of iterations to run the Markov chain
#' @param burnin number of initial iterations to be discarded
#' @param thin thinning number (post-processing)
#' @param k.theta prior parameter for number of B-spline densities k (proportional to exp(-k.theta*k^2)) in mixture
#' @param MG Dirichlet process base measure constant for weights of B-spline densities in mixture (> 0)
#' @param G0.alpha,G0.beta parameters of Beta base measure of Dirichlet process for weights of B-spline densities in mixture (default is Uniform[0, 1])
#' @param LG truncation parameter of Dirichlet process in stick breaking representation for weights of B-spline densities
#' @param MH Dirichlet process base measure constant for knot placements of B-spline densities (> 0)
#' @param H0.alpha,H0.beta parameters of Beta base measure of Dirichlet process for knot placements of B-spline densities (default is Uniform[0, 1])
#' @param LH truncation parameter of Dirichlet process in stick breaking representation for knot placements of B-spline densities
#' @param tau.alpha,tau.beta prior parameters for tau (Inverse-Gamma)
#' @param kmax upper bound for number of B-spline densities in mixture
#' @param k1 starting value for k.  If \code{k1} = NA then a random starting value between \code{degree} + 2 and \code{kmax} is selected
#' @param degree positive integer specifying the degree of the B-spline densities (default is 3) 
#' @return A list with S3 class 'psd' containing the following components:
#'    \item{psd.median,psd.mean}{psd estimates: (pointwise) posterior median and mean}
#'    \item{psd.p05,psd.p95}{90\% pointwise credibility interval}
#'    \item{psd.u05,psd.u95}{90\% uniform credibility interval}
#'    \item{k,tau,V,Z,U,X}{posterior traces of model parameters}
#'    \item{knots.trace}{trace of knot placements}
#'    \item{ll.trace}{trace of log likelihood}
#'    \item{pdgrm}{periodogram}
#'    \item{n}{integer length of input time series}
#' @seealso \link{plot.psd}
#' @references Edwards, M. C., Meyer, R., and Christensen, N. (2018), Bayesian nonparametric spectral density estimation using B-spline priors, \emph{Statistics and Computing}, <https://doi.org/10.1007/s11222-017-9796-9>.
#' 
#' Choudhuri, N., Ghosal, S., and Roy, A. (2004), Bayesian estimation of the spectral density of a time series, \emph{Journal of the American Statistical Association}, 99(468):1050--1059.
#' 
#' @examples 
#' \dontrun{
#' 
#' set.seed(123456)
#' 
#' # Generate AR(1) data with rho = 0.9
#' n = 128
#' data = arima.sim(n, model = list(ar = 0.9))
#' data = data - mean(data)
#'
#' # Run MCMC (may take some time)
#' mcmc = gibbs_bspline(data, 10000, 5000)
#'
#' require(beyondWhittle)  # For psd_arma() function
#' freq = 2 * pi / n * (1:(n / 2 + 1) - 1)[-c(1, n / 2 + 1)]  # Remove first and last frequency
#' psd.true = psd_arma(freq, ar = 0.9, ma = numeric(0), sigma2 = 1)  # True PSD
#' plot(mcmc)  # Plot log PSD (see documentation of plot.psd)
#' lines(freq, log(psd.true), col = 2, lty = 3, lwd = 2)  # Overlay true PSD
#' }
#' @importFrom Rcpp evalCpp
#' @useDynLib bsplinePsd, .registration = TRUE
#' @export
gibbs_bspline <- function(data,
                          Ntotal,
                          burnin,
                          thin = 1,
                          k.theta = 0.01,
                          MG = 1,
                          G0.alpha = 1,
                          G0.beta = 1,
                          LG = 20,
                          MH = 1,
                          H0.alpha = 1,
                          H0.beta = 1,
                          LH = 20,
                          tau.alpha = 0.001,
                          tau.beta = 0.001,
                          kmax = 100,
                          k1 = 20,
                          degree = 3) {
  
  n <- length(data)
  
  # Which boundary frequencies to remove from likelihood computation and tau sample
  if (n %% 2) {  # Odd length time series
    bFreq <- 1  # Remove first
  } 
  else {  # Even length time series
    bFreq <- c(1, n)  # Remove first and last
  }
  
  # Tolerance for mean centering
  tol <- 1e-4
  
  # Mean center
  if (abs(mean(data)) > tol) {
    data <- data - mean(data)
    warning("data has been mean-centered")
  }
  
  # Optimal rescaling to prevent numerical issues
  rescale = stats::sd(data)
  data = data / rescale  # Data now has standard deviation 1
  
  if (burnin >= Ntotal) stop("burnin must be less than Ntotal")
  if (any(c(MG, MH, G0.alpha, G0.beta, H0.alpha, H0.beta, tau.alpha, tau.beta, k.theta) <= 0)) stop("MG, MH, G0.alpha, G0.beta, H0.alpha, H0.beta, tau.alpha, tau.beta, and k.theta must be strictly positive")
  if (any(c(Ntotal, thin, kmax, LG, LH) %% 1 != 0) || any(c(Ntotal, thin, kmax, LG, LH) <= 0)) stop("Ntotal, thin, kmax, LG, and LH must be strictly positive integers")
  if ((burnin %% 1 != 0) || (burnin < 0)) stop("burnin must be a non-negative integer")

  FZ <- fast_ft(data)  # FFT data to frequency domain.  NOTE: Must be mean-centred.
  
  pdgrm <- abs(FZ) ^ 2   # Periodogram: NOTE: the length is n here.
  
  omega <- 2 * (1:(n / 2 + 1) - 1) / n  # Frequencies on unit interval
  lambda <- pi * omega  # Angular frequencies on [0, pi]
  
  # Open objects for storage
  tau <- rep(NA, Ntotal)
  V <- matrix(NA, nrow = LG, ncol = Ntotal)
  W <- matrix(NA, nrow = LG + 1, ncol = Ntotal)
  U <- matrix(NA, nrow = LH, ncol = Ntotal)
  Z <- matrix(NA, nrow = LH + 1, ncol = Ntotal)
  k <- rep(NA, Ntotal)
  
  # Starting values
  tau[1] <- stats::var(data) / (2 * pi)

  if (is.na(k1)) {  # If k1 is NA, user does not specify starting value for k
    k[1] = sample((degree + 2):kmax, 1)  # Need at least k = 5, 4, 3 for cubic, quadratic, linear B-splines respectively
  }
  else {  # User specified starting value for k
    if ((k1 < (degree + 2)) || (k1 > kmax)) stop("k1 must be at least degree + 2 and no more than kmax") 
    k[1] <- k1
  }
  
  # Optimise starting values for DP parameters
  V[, 1] = vFromP(rep(1 / (LG + 1), LG))
  W[, 1] = seq(from=1 / (2 * k[1]), to = 1 - 1 / (2 * k[1]), length.out = LG + 1)
  U[, 1] = vFromP(rep(1 / (LH + 1), LH))
  Z[, 1] = seq(from = 1 / (2 * k[1]), to = 1 - 1 / (2 * k[1]), length.out = LH + 1)

  # Store log likelihood
  ll.trace <- rep(NA, Ntotal)
  ll.trace[1] <- llike(omega, FZ, k[1], V[, 1], W[, 1], U[, 1], Z[, 1], tau[1], pdgrm, degree, recompute = TRUE)$llike
  
  # Metropolis proposal parameters for V, U, W, Z.
  epsG <- seq(1, LG + 1) / (seq(1, LG + 1) + 2 * sqrt(n))  
  epsH <- seq(1, LH + 1) / (seq(1, LH + 1) + 2 * sqrt(n))  # IS THIS A GOOD PROPOSAL?
  
  ptime = proc.time()[1]
  
  # Metropolis-within-Gibbs sampler
  for (i in 1:(Ntotal-1)) {
    
    if (i %% 100 == 0) {
      print(paste("Iteration", i, ",", "Time elapsed", 
                  round(as.numeric(proc.time()[1] - ptime) / 60, 2),
                  "minutes"))
    }
    
    lp.list <- lpost(omega,
                     FZ,
                     k[i], 
                     V[, i], 
                     W[, i], 
                     U[, i],
                     Z[, i],
                     tau[i],
                     k.theta,
                     MG,
                     G0.alpha,
                     G0.beta,
                     MH,
                     H0.alpha,
                     H0.beta,
                     tau.alpha,
                     tau.beta,
                     pdgrm,
                     degree,
                     recompute = TRUE)
    
    f.store <- lp.list$lp
    db.list <- lp.list$db.list
    
    #####
    # Step 1: Metropolis proposal for k
    #####
    bold <- stats::runif(1)
    if (bold < 0.75) {  # 75/25 split
      jump <- sample(-1:1, 1, prob = rep(1 / 3, 3))  # Ordinary proposal
    }
    else {
      jump <- round(stats::rt(1, 1))  # Bold proposal - discrete Cauchy
    }
    k.star <- k[i] + jump
    while (k.star < (degree + 2) || k.star > kmax) {  # A bit hacky to ensure k doesn't go out of bounds
      if (bold < 0.75) {
        jump <- sample(-1:1, 1, prob = rep(1 / 3, 3))  # Ordinary proposal
      }
      else {
        jump <- round(stats::rt(1, 1))  # Bold proposal
      }
      k.star <- k[i] + jump
    }
    
    # log posterior for proposal
    lp.list <- lpost(omega,
                     FZ,
                     k.star,  
                     V[, i], 
                     W[, i],
                     U[, i],
                     Z[, i],
                     tau[i],
                     k.theta,
                     MG,
                     G0.alpha,
                     G0.beta,
                     MH,
                     H0.alpha,
                     H0.beta,
                     tau.alpha,
                     tau.beta,
                     pdgrm,
                     degree,
                     recompute = TRUE)   
    
    f.k.star <- lp.list$lp
    
    # log posterior of previous iteration
    f.k <- f.store
    
    #####
    # Accept/reject
    alpha1 <- min(0, f.k.star - f.k)  # log acceptance ratio
    if (log(stats::runif(1, 0, 1)) < alpha1) {
      k[i + 1] <- k.star  # Accept k.star
      f.store <- f.k.star
      db.list <- lp.list$db.list
    }
    else {
      k[i + 1] <- k[i]  # Reject and use previous
    }
    #####
    # End: Step 1
    #####
    
    #####
    # Step 2: Metropolis-within-Gibbs step for V (EXPENSIVE)
    #####
    for (l in 1:LG) {
      
      V.star <- V.old <- V[, i]
      if (l > 1) {
        for (il in 1:(l - 1)) {
          V.star[il] <- V.old[il] <- V[il, i + 1]
        }
      }
      
      # Uniform proposal (V[,i] - eps, V[,i] + eps) on (0,1)
      V.star[l] <- stats::runif(1, V.star[l] - epsG[l], V.star[l] + epsG[l])
      V.star[l][V.star[l] > 1] <- V.star[l] - 1  # Puts in [0, 1]
      V.star[l][V.star[l] < 0] <- V.star[l] + 1  # Puts in [0, 1]
      
      # log posterior for proposal
      lp.list <- lpost(omega,
                       FZ,
                       k[i + 1], 
                       V.star,  
                       W[, i],
                       U[, i],
                       Z[, i],
                       tau[i],
                       k.theta,
                       MG,
                       G0.alpha,
                       G0.beta,
                       MH,
                       H0.alpha,
                       H0.beta,
                       tau.alpha,
                       tau.beta,
                       pdgrm,
                       degree,
                       recompute = FALSE,
                       db.list)  # Do not need to recompute B-splines for these parameters    
      
      f.V.star <- lp.list$lp
      
      # log posterior of previous iteration
      f.V <- f.store
      
      # Accept/reject
      alpha2 <- min(0, f.V.star - f.V)  # log acceptance ratio
      if (log(stats::runif(1, 0, 1)) < alpha2) {
        V[l, i + 1] <- V.star[l]  # Accept V.star
        f.store <- f.V.star
      }
      else {
        V[l, i + 1] <- V[l, i]  # Reject and use previous
      }
      
    }  
    #####
    # End: Step 2
    #####
    
    #####
    # Step 3: Metropolis-within-Gibbs step for W (EXPENSIVE)
    #####
    for (l in 1:(LG + 1)) {
      
      W.star <- W.old <- W[, i]
      if (l > 1) {
        for (il in 1:(l - 1)) {
          W.star[il] <- W.old[il] <- W[il, i + 1]
        }
      }
      
      # Uniform proposal from (W[,i] - eps, W[,i] + eps) on (0,1)
      W.star[l] <- stats::runif(1, W.star[l] - epsG[l], W.star[l] + epsG[l])
      W.star[l][W.star[l] > 1] <- W.star[l] - 1  # Puts in [0, 1]
      W.star[l][W.star[l] < 0] <- W.star[l] + 1  # Puts in [0, 1]
      
      # log posterior for proposal
      lp.list <- lpost(omega,
                       FZ,
                       k[i + 1], 
                       V[, i + 1],  
                       W.star,  
                       U[, i],
                       Z[, i],
                       tau[i],
                       k.theta,
                       MG,
                       G0.alpha,
                       G0.beta,
                       MH,
                       H0.alpha,
                       H0.beta,
                       tau.alpha,
                       tau.beta,
                       pdgrm,
                       degree,
                       recompute = FALSE,
                       db.list)  # Do not need to recompute B-splines for these parameters  
      
      f.W.star <- lp.list$lp
      
      # log posterior for previous iteration
      f.W <- f.store
      
      # Accept/reject
      alpha3 <- min(0, f.W.star - f.W)  # log acceptance ratio
      if(log(stats::runif(1, 0, 1)) < alpha3) {
        W[l, i + 1] <- W.star[l]  # Accept W.star
        f.store <- f.W.star
      }
      else {
        W[l, i + 1] <- W[l, i]  # Reject and use previous
      }
      
    }  
    #####
    # End: Step 3
    #####
    
    # Next 2 steps relate to the knot placements
    
    #####
    # Step 4: Metropolis-within-Gibbs step for U (EXPENSIVE)
    #####
    for (l in 1:LH) {
      
      U.star <- U.old <- U[, i]
      if (l > 1) {
        for (il in 1:(l - 1)) {
          U.star[il] <- U.old[il] <- U[il, i + 1]
        }
      }
      
      # Uniform proposal (U[,i] - eps, U[,i] + eps) on (0,1)
      U.star[l] <- stats::runif(1, U.star[l] - epsH[l], U.star[l] + epsH[l])
      U.star[l][U.star[l] > 1] <- U.star[l] - 1  # Puts in [0, 1]
      U.star[l][U.star[l] < 0] <- U.star[l] + 1  # Puts in [0, 1]
      
      # log posterior for proposal
      lp.list <- lpost(omega,
                       FZ,
                       k[i + 1], 
                       V[, i + 1],  
                       W[, i + 1],
                       U.star,
                       Z[, i],
                       tau[i],
                       k.theta,
                       MG,
                       G0.alpha,
                       G0.beta,
                       MH,
                       H0.alpha,
                       H0.beta,
                       tau.alpha,
                       tau.beta,
                       pdgrm,
                       degree,
                       recompute = TRUE) 
      
      f.U.star <- lp.list$lp
      
      # log posterior of previous iteration
      f.U <- f.store
      
      # Accept/reject
      alpha4 <- min(0, f.U.star - f.U)  # log acceptance ratio
      if (log(stats::runif(1, 0, 1)) < alpha4) {
        U[l, i + 1] <- U.star[l]  # Accept V.star
        f.store <- f.U.star
        db.list <- lp.list$db.list
      }
      else {
        U[l, i + 1] <- U[l, i]  # Reject and use previous
      }
      
    }
    #####
    # End: Step 4
    #####
    
    #####
    # Step 5: Metropolis-within-Gibbs step for Z (EXPENSIVE)
    #####
    for (l in 1:(LH + 1)) {
      
      Z.star <- Z.old <- Z[, i]
      if (l > 1) {
        for (il in 1:(l - 1)) {
          Z.star[il] <- Z.old[il] <- Z[il, i + 1]
        }
      }
      
      # Uniform proposal from (Z[,i] - eps, Z[,i] + eps) on (0,1)
      Z.star[l] <- stats::runif(1, Z.star[l] - epsH[l], Z.star[l] + epsH[l])
      Z.star[l][Z.star[l] > 1] <- Z.star[l] - 1  # Puts in [0, 1]
      Z.star[l][Z.star[l] < 0] <- Z.star[l] + 1  # Puts in [0, 1]
      
      # log posterior for proposal
      lp.list <- lpost(omega,
                       FZ,
                       k[i + 1], 
                       V[, i + 1], 
                       W[, i + 1], 
                       U[, i + 1],
                       Z.star,
                       tau[i],
                       k.theta,
                       MG,
                       G0.alpha,
                       G0.beta,
                       MH,
                       H0.alpha,
                       H0.beta,
                       tau.alpha,
                       tau.beta,
                       pdgrm,
                       degree,
                       recompute = TRUE)  
      
      f.Z.star <- lp.list$lp
      
      # log posterior for previous iteration
      f.Z <- f.store  
      
      # Accept/reject
      alpha5 <- min(0, f.Z.star - f.Z)  # log acceptance ratio
      if(log(stats::runif(1, 0, 1)) < alpha5) {
        Z[l, i + 1] <- Z.star[l]  # Accept W.star
        f.store <- f.Z.star
        db.list <- lp.list$db.list
      }
      else {
        Z[l, i + 1] <- Z[l, i]  # Reject and use previous
      }
      
    }
    #####
    # End: Step 5
    #####
    
    #####
    # Step 6: Directly sample tau from conjugate Inverse-Gamma density
    #####
    q.psd <- qpsd(omega, k[i + 1], V[, i + 1], W[, i + 1], U[, i + 1], Z[, i + 1], 
                  degree, recompute = FALSE, db.list)$psd
    q <- unrollPsd(q.psd, n)
    
    # Note: (n - 1) and (n - 2) here.  Remove the first and last terms for even and first for odd
    if (n %% 2) {  # Odd length series
      tau[i + 1] <- 1 / stats::rgamma(1, tau.alpha + (n - 1) / 2, 
                                      tau.beta + sum(pdgrm[-bFreq] / q[-bFreq]) / (2 * pi) / 2)
    }
    else {  # Even length series
      tau[i + 1] <- 1 / stats::rgamma(1, tau.alpha + (n - 2) / 2, 
                                      tau.beta + sum(pdgrm[-bFreq] / q[-bFreq]) / (2 * pi) / 2)
    }
    #####
    # End: Step 6
    #####
    
    #####
    # Step 7: Compute log likelihood
    #####
    ll.trace[i + 1] <- llike(omega, FZ, k[i + 1], V[, i + 1], W[, i + 1], U[, i + 1], Z[, i + 1], tau[i + 1], pdgrm,
                             degree, recompute = FALSE, db.list)$llike
    #####
    # End: Step 7
    #####
    
  }  # END: MCMC loop
  
  # Which iterations to keep
  keep <- seq(burnin + 1, Ntotal, by = thin)
  k <- k[keep]
  tau <- tau[keep]
  V <- V[, keep]
  W <- W[, keep]
  U <- U[, keep]
  Z <- Z[, keep]
  ll.trace <- ll.trace[keep]
  
  fpsd.sample <- log.fpsd.sample <- matrix(NA, nrow = length(omega), ncol = length(keep))
  knots.trace <- matrix(NA, nrow = kmax, ncol = length(keep))
  
  # Store PSDs
  for (isample in 1:length(keep)) {
    q.psd <- qpsd(omega, k[isample], V[, isample], W[, isample], U[, isample], Z[, isample], 
                  degree, recompute = TRUE)
    fpsd.sample[, isample] <- tau[isample] * q.psd$psd
    knots.trace[1:length(q.psd$knots), isample] <- q.psd$knots
    log.fpsd.sample[, isample] <- logfuller(fpsd.sample[, isample])  # Create transformed version
  }
  
  #ci.l = (1 - ci) / 2
  #ci.u = 1 - ci.l
  
  # Compute point estimates and 90% Pointwise CIs
  psd.median <- apply(fpsd.sample, 1, stats::median)
  psd.mean <- apply(fpsd.sample, 1, mean)
  psd.p05 <- apply(fpsd.sample, 1, stats::quantile, probs=0.05)
  psd.p95 <- apply(fpsd.sample, 1, stats::quantile, probs=0.95)
  #psd.p.lower <- apply(fpsd.sample, 1, stats::quantile, probs=ci.l)
  #psd.p.upper <- apply(fpsd.sample, 1, stats::quantile, probs=ci.u)
  
  # Transformed versions of these for uniform CI construction
  log.fpsd.s <- apply(log.fpsd.sample, 1, stats::median)
  log.fpsd.mad <- apply(log.fpsd.sample, 1, stats::mad)
  log.fpsd.help <- apply(log.fpsd.sample, 1, uniformmax)
  log.Cvalue <- stats::quantile(log.fpsd.help, 0.9)
  #log.Cvalue <- stats::quantile(log.fpsd.help, ci)
  
  # Compute Uniform CIs
  psd.u95 <- exp(log.fpsd.s + log.Cvalue * log.fpsd.mad)
  psd.u05 <- exp(log.fpsd.s - log.Cvalue * log.fpsd.mad)
  
  # Compute periodogram
  N = length(psd.median)  # N = (n + 1) / 2 (ODD) or N = n / 2 + 1 (EVEN)
  pdgrm = (abs(stats::fft(data)) ^ 2 / (2 * pi * n))[1:N]
  # kappa = rep(2, N)  # Open kappa object.  Most multiply by 2 but ends not.
  # if (n %% 2) {  # Odd length time series
  #   kappa[1] = 1  # Zero frequency
  # } 
  # else {  # Even length time series
  #   kappa[c(1, length(kappa))] = 1  # 0 and Nyquist frequency
  # }
  # pdgrm = kappa * (abs(stats::fft(data)) ^ 2 / (2 * pi * n))[1:N]
  
  # List to output
  output = list(psd.median = psd.median * rescale ^ 2,
                psd.mean = psd.mean * rescale ^ 2,
                psd.p05 = psd.p05 * rescale ^ 2,
                psd.p95 = psd.p95 * rescale ^ 2,
                psd.u05 = psd.u05 * rescale ^ 2,
                psd.u95 = psd.u95 * rescale ^ 2,
                k = k,
                tau = tau,
                V = V,
                Z = W,  # W here is called Z in paper
                U = U,
                X = Z,  # Z here is called X in paper
                knots.trace = knots.trace,
                ll.trace = ll.trace,
                pdgrm = pdgrm * rescale ^ 2,
                n = n)
  
  class(output) = "psd"  # Assign S3 class to object
  
  return(output)  # Return output
  
}  # Close function
