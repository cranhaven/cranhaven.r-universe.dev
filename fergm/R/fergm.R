#' Estimation of Frailty Exponential Random Graph Model (FERGM) via MPLE using Stan
#'
#' This function estimates a FERGM
#' @param net A network object that is to be explained using the right_hand_vars argument.
#' @param form A character string specified as "ergm.term1 + ergm.term2", must be terms supported for ERGMs.
#' @param seed An integer that sets the seed for the random number generator to assist in replication.  Defaults to 12345.  Set to null to prevent internal seed setting.
#' @param chains An integer that sets the number of Markov chains that should be used by Stan.
#' @param warmup The number of warm up or burn-in iterations that should be used before posterior draws are taken.  Defaults to 100.
#' @param iter The number of total number of samples that should be taken including warm ups  Defaults to 600 total iterations, leading to a posterior sample size of 500.
#' @param cores The number of cores to used should parallel processing be desired.  Defaults to 1.
#' @return This function returns a list that includes the Stan output (stan.fit), the data object passed to Stan (stan.dta), and the original formula (form).
#' @keywords FERGM
#' @references Box-Steffensmeier, Janet M., Dino P. Christenson, and Jason W. Morgan. 2018. ``Modeling Unobserved Heterogeneity in Social Networks with the Frailty Exponential Random Graph Model." \emph{Political Analysis}. (26)1:3-19.
#' @references Stan Development Team (2016). RStan: the R interface to Stan. R package version 2.14.1. \url{http://mc-stan.org/}.
#' @examples
#' \dontrun{
#' set.seed(1)
#'
#' data("faux.mesa.high")
#'
#' mesa <- faux.mesa.high
#'
#' mesa.fit <- ergm::ergm(mesa ~ edges +
#'                   nodematch('Sex') +
#'                   nodematch('Grade', diff = FALSE) +
#'                   nodematch('Race', diff = FALSE) +
#'                   gwesp(decay = 0.2, fixed = TRUE) +
#'                   altkstar(lambda = 0.6, fixed = TRUE))
#'
#' library(fergm)
#' form <- c("edges + nodematch('Sex') + nodematch('Grade', diff = FALSE) +
#'           nodematch('Race', diff = FALSE) +
#'           gwesp(decay = 0.2, fixed = TRUE) + altkstar(lambda = 0.6, fixed = TRUE)")
#'
#' fergm.fit <- fergm(net = mesa, form = form, chains = 2)
#'}
#' @export

fergm <- function(net = NULL, form = NULL, seed = 12345, chains = 4, warmup = 100, iter = 600, cores = 1){
  # load necessary packages
  extrafont::loadfonts()

  to_indicator <- function(X, directed=FALSE)
  {
    if (isTRUE(directed)) {
      stop("Directed networks not yet supported.")
    } else {
      X[,1] <- paste0("Sociality", X[,1])
      X[,2] <- paste0("Sociality", X[,2])
      factor.levels <- unique(unlist(X))
      X[,1] <- factor(X[,1], levels=factor.levels)
      X[,2] <- factor(X[,2], levels=factor.levels)
    }

    X
  }

  prepare_fergm_data <- function(net, form, verbose=FALSE)
  {
    ## Temporary until directed code is added.
    if (network::is.directed(net)) stop("Directed networks not yet supported.")

    if (isTRUE(verbose)) cat("\n## Preparing FERGM dataset...")
    nodes <- nrow(as.matrix(net))
    ndyads <- network::network.dyadcount(net)
    form <- stats::as.formula(paste("net ~", form))

    if (isTRUE(verbose)) cat("\n##   building array...")
    dta.array <- ergm::ergmMPLE(form, output="array", maxMPLEsamplesize=+Inf,
                          control=ergm::control.ergm(MPLE.max.dyad.types=ndyads*10))

    if (isTRUE(verbose)) cat("\n##   building data.frame...")
    ncoef <- length(dta.array$predictor[1,2,])
    dta <- matrix(0, nrow=ndyads, ncol=3+ncoef)

    idx <- 1
    for (tail in 1:(nodes-1)) {
      for (head in (tail+1):nodes) {
        dta[idx,] <- c(dta.array$response[tail, head],
                       dta.array$predictor[tail, head, ],
                       tail,
                       head)
        idx <- idx+1
      }
    }

    dta <- data.frame(dta)
    nm <- c("Y", names(dta.array$predictor[tail, head, ]),
            "Sociality1", "Sociality2")
    names(dta) <- nm

    if (isTRUE(verbose)) cat("\n##   setting random effects indicators...\n")
    if (network::is.directed(net)) {
      stop("Directed networks not yet supported.")
    } else {
      Soc <- to_indicator(dta[,c("Sociality1", "Sociality2")])
      dta[, "Sociality1"] <- Soc[,1]
      dta[, "Sociality2"] <- Soc[,2]
    }

    dta
  }

  prepare_stan_data <- function(net, dta)
  {
    x <- dta[,2:(ncol(dta)-2)]
    y <- dta[,1]
    idx1 <- as.numeric(dta$Sociality1)
    idx2 <- as.numeric(dta$Sociality2)

    list(K = ncol(x),
         N = ncol(net[,]),
         D = nrow(x),
         x = x,
         y = y,
         node1_idx = idx1,
         node2_idx = idx2)
  }

  # Prepare data
  if(!is.null(seed)){
    set.seed(seed)
    cat("Setting seed at the default value of 12345 for the seed argument.")
  } else {
    warning("Note: This function relies simulation.  Consider specifying a seed to set to ensure replicability.")
  }

  cat("Starting data preparation")

  fergm.dta <- prepare_fergm_data(net, form)
  stan.dta  <- prepare_stan_data(net, fergm.dta)
  stan.dta$x <- as.matrix(stan.dta$x)      # deal with a recent change in rstan

  # Allow for multiple cores to be specified
  rstan_options(auto_write = TRUE)
  options(mc.cores = cores)

  # fit model using STAN
  scode <- "data {
    int<lower=1> K;		// number of predictors
    int<lower=1> N;		// number of nodes
    int<lower=0> D;		// number of potential ties

    row_vector[K] x[D];		// predictors
    int<lower=0,upper=1> y[D];	// ties (outcome)
    int<lower=1> node1_idx[D];	// first node index
    int<lower=1> node2_idx[D];	// second node index
  }
  parameters {
    real<lower=0> sigma;		// sociality dispersion
    vector[N] theta_raw;		// sociality effects
    vector[K] beta;		// predictor coefficients
  }
  transformed parameters {
    vector[N] theta;
    theta = sigma * (theta_raw - mean(theta_raw));
  }
  model {
    sigma ~ cauchy(0, 2);
    theta_raw ~ normal(0, 1);
    beta  ~ normal(0, 10);

    {
      vector[D] phi; // container for linear predictor
      for (i in 1:D) {
        // The 0.5 is to make the model consistent with weights multiple
        // membership models place on the random effects. It has no effect on the
        // fixed-effect results, just the scale of sigma and theta.
        phi[i] = dot_product(beta, x[i]) +
          0.5 * (theta[node1_idx[i]] + theta[node2_idx[i]]);
      }

      y ~ bernoulli_logit(phi);
    }
  }
  generated quantities {
    vector[D] predictions;
    for (i in 1:D) {
      predictions[i] = bernoulli_rng(inv_logit(dot_product(beta, x[i]) +
                                                 0.5 * (theta[node1_idx[i]] + theta[node2_idx[i]])));
    }
  }
  "

  stan.fit <- stan(model_code = scode,
                   data=stan.dta, chains=chains, seed=seed, warmup=warmup, iter=iter)

  return(list(stan.fit = stan.fit, stan.dta = stan.dta, form = form))

}
