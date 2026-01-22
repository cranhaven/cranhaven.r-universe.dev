#--------------------------------------------------
#' @title Validate Artificial Neural Networks.
#'
#' @description Compute metrics and statistics for predictive, replicative
#'    and/or structural validation of artificial neural networks (ANNs).
#'
#' @param net   an object of class `ann' (as returned by function
#'    \code{\link{ann}}) or `nnet' (as returned using \code{\link[nnet]{nnet}}).
#'    This is a list object comprising information about the fitted ANN model,
#'    including values of weights, fitted target values, number of layers and
#'    numbers of nodes in each layer, for example.
#' @param obs,sim   vectors comprising observed (\code{obs}) and simulated
#'    (\code{sim}) examples of a single response variable. These vectors are
#'    used to compute model fit statistics. Optional if \code{net} is supplied
#'    (see `Details').
#' @param x    matrix, data frame or vector of input data used for
#'    fitting \code{net} object. A vector is considered to comprise examples of
#'    a single input or predictor variable. While \code{x} is optional,
#'    sensitivity analyses useful for structural validation cannot be performed
#'    if it is not supplied.
#' @param wts    vector of ANN weights used to compute input
#'    `relative importance' measures if \code{net} object is not supplied. Must
#'    be supplied together with \code{nodes} in order to compute such metrics.
#'    See `Details' for ordering of \code{wts} vector.
#' @param nodes    vector indicating the number of nodes in each layer
#'    of the ANN model. This vector should have 3 elements: nodes in input
#'    layer, nodes in hidden layer (can be 0), and nodes in output layer.
#'    If \code{net} object is not supplied, \code{nodes} must be supplied
#'    together with \code{wts} if any structural validation metrics are to be
#'    computed.
#' @param na.rm    logical; should missing values (including NaN)
#'    be removed from calculations? Default = TRUE.
#' @param \dots   arguments to be passed to different validann methods,
#'     see specific formulations for details.
#' @return   list object of class `validann' with components dependent on
#'    arguments passed to \code{validann} function:
#'
#' \item{metrics}{a data frame consisting of metrics:
#'
#'    AME, PDIFF, MAE, ME, RMSE, R4MS4E, AIC, BIC, NSC, RAE, PEP, MARE,
#'    MdAPE, MRE, MSRE, RVE, RSqr, IoAd, CE, PI, MSLE, MSDE, IRMSE, VE,
#'    KGE, SSE and R.
#'
#'    See Dawson et al. (2007) for definitions.}
#' \item{obs_stats}{a data frame consisting of summary statistics about the
#'    \code{obs} dataset including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{sim_stats}{a data frame consisting of summary statistics about the
#'    \code{sim} dataset including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{residuals}{a 1-column matrix of model residuals (\code{sim - obs}).}
#' \item{resid_stats}{a data frame consisting of summary statistics about the
#'    model \code{residuals} including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{ri}{a data frame consisting of `relative importance' values for each
#'    input. Only returned if \code{net} or \code{wts} and \code{nodes} are
#'    supplied.
#'
#'    If \code{net} is supplied, relative importance values computed using the
#'    following 4 methods are returned:
#'
#'    Garson's (Garson); connection weight (CW); Profile sensitivity
#'    analysis (Profile); and partial derivative sensitivity analysis (PaD).
#'
#'    In addition, if \code{net} is of class `ann' (as returned by function
#'    \code{\link{ann}}) and the activation function used at the hidden
#'    layer (\code{act_hid}) is "tanh", relative importance
#'    values computed using the modified CW (MCW) are also returned.
#'    This method requires that the hidden layer activation function be
#'    symmetric about the origin.
#'
#'    If \code{wts} and \code{nodes} are supplied, only relative importance
#'    values computed using the Garson and CW methods are returned.
#'
#'    See Gevrey et al. (2003), Olden et al. (2004) and Kingston et al. (2006)
#'    for details of the relative importance methods.}
#' \item{y_hat}{a matrix of dimension \code{c(101, ncol(x) * 6)} of model
#'    response values indicating the local sensitivity of the model to each
#'    input in \code{x}. Only returned if \code{net} and \code{x} are supplied.
#'
#'    The response values returned in \code{y_hat} are calculated using the
#'    `Profile' sensitivity analysis method described in Gevrey et al. (2003).
#'    Using this method, the local sensitivity of each input in \code{x} is
#'    considered successively. For each input \code{x[,i]}, 5 synthetic data
#'    sets are generated where inputs \code{x[,-i]} are successively fixed at
#'    their minimum, 1st quartile, median, 3rd quartile and maximum values
#'    (as calculated from \code{x}), while input \code{x[,i]} is varied between
#'    its minimum and maximum value, increasing in increments of 1\% (giving
#'    101 synthetic values of \code{x[,i]} for each of the 5 sets of fixed
#'    \code{x[,-i]}). These data are input into \code{net} and model response
#'    values corresponding to the 5 summary statistics are computed.
#'    These 5 sets of response values, together with a set of computed median
#'    responses, are returned as y_hat[,(i - 1) * 6 + 1:6]. This process is
#'    repeated for each input variable in \code{x}. See Gevrey et al. (2003)
#'    for further details.}
#' \item{as}{a matrix of dimension \code{dim(x)} of `absolute sensitivity'
#'    values for each input in \code{x} given the model output values
#'    (i.e. \code{sim}). Only returned if \code{net} and \code{x} are
#'    supplied and \code{net} is of class `ann'.
#'
#'    The values in \code{as} are calculated according to the partial
#'    derivative (PaD) sensitivity analysis method described in Gevrey et al.
#'    (2003), which involves computing the first-order partial derivatives of
#'    the ANN output with respect to each input. \code{net} must be of class
#'    `ann' in order to access partial derivatives of the hidden layer nodes as
#'    returned by \code{\link{ann}}.}
#' \item{rs}{a matrix of dimension \code{dim(x)} of `relative sensitivity'
#'    values for each input in \code{x} given the model output values
#'    (i.e. \code{sim}). Only returned if \code{net} and \code{x} are
#'    supplied and \code{net} is of class `ann'.
#'
#'    To compute the values in \code{rs}, the \code{as} values are normalised
#'    by multiplying by \code{x[,i]}/\code{sim} as described in Mount et al.
#'    (2013). As for \code{as}, \code{net} must be of class
#'    `ann' in order to access partial derivatives of the hidden layer nodes as
#'    returned by \code{\link{ann}}.}
#' @details   To compute all possible validation metrics and statistics,
#'    \code{net} must be supplied and must be of class `ann' (as returned by
#'    \code{\link{ann}}) or `nnet' (as returned by \code{\link[nnet]{nnet}}).
#'    However, a partial derivative (PaD) sensitivity analysis (useful for
#'    structural validation) will only be carried out if \code{net} is of class
#'    `ann'.
#'
#'    If \code{obs} and \code{sim} data are supplied in addition to \code{net},
#'    validation metrics are computed based on these. Otherwise, metrics and
#'    statistics are computed based on \code{obs} and \code{sim} datasets
#'    derived from the \code{net} object (i.e. the data used to fit \code{net}
#'    and the fitted values). As such, both \code{obs} and \code{sim} must be
#'    supplied if validation is to be based either on data not used for
#'    training or on unprocessed training data (if training data were
#'    preprocessed). If either \code{obs} or \code{sim} is specified but the
#'    other isn't, both \code{obs} and \code{sim} will be derived from
#'    \code{net} if supplied (and a warning will be given). Similarly, this
#'    will occur if \code{obs} and \code{sim} are of different lengths.
#'
#'    If \code{net} is not supplied, both \code{obs} and \code{sim} are
#'    required. This may be necessary if validating an ANN model not built
#'    using either the \code{\link[nnet]{nnet}} or \code{\link{ann}} functions.
#'    In this case, both \code{wts} and \code{nodes} are also required if any
#'    structural validation metrics are to be returned. If an ANN model has
#'    \emph{K} input nodes, \emph{J} hidden nodes and a single output \emph{O},
#'    with a bias node for both the hidden and output layers, the \code{wts} vector must be ordered
#'    as follows:
#'
#'    \code{c(Wi1h1,Wi1h2,...Wi1hJ,Wi2h1,...Wi2hJ,...,WiKh1,...,WiKhJ,Wi0h1,...,Wi0hJ,}\cr
#'    \code{  Wh1O,...,WhJO,Wh0O)}
#'
#'    where \code{Wikhj} is the weight between the \emph{k}th input and the
#'    \emph{j}th hidden node and \code{WhjO} is the weight between the
#'    \emph{j}th hidden node and the output. The bias weight on the \emph{j}th
#'    hidden layer node is labelled \code{Wi0hj} while the bias weight on the
#'    output is labelled \code{Wh0O}. The \code{wts} vector assumes the network
#'    is fully connected; however, missing connections may be substituted by
#'    zero weights. Skip-layer connections are not allowed.
#'
#' @references
#' Dawson, C.W., Abrahart, R.J., See, L.M., 2007. HydroTest: A web-based
#'    toolbox of evaluation metrics for the standardised assessment of
#'    hydrological forecasts. Environmental Modelling & Software, 22(7),
#'    1034-1052. \url{http://dx.doi.org/10.1016/j.envsoft.2006.06.008}.
#'
#' Olden, J.D., Joy, M.K., Death, R.G., 2004. An accurate comparison of
#'    methods for quantifying variable importance in artificial neural networks
#'    using simulated data. Ecological Modelling 178, 389-397.
#'    \url{http://dx.doi.org/10.1016/j.ecolmodel.2004.03.013}.
#'
#' Gevrey, M., Dimopoulos, I., Lek, S., 2003. Review and comparison of methods
#'    to study the contribution of variables in artificial neural network
#'    models. Ecological Modelling 160, 249-264.
#'    \url{http://dx.doi.org/10.1016/S0304-3800(02)00257-0}.
#'
#' Kingston, G.B., Maier, H.R., Lambert, M.F., 2006. Forecasting cyanobacteria
#'    with Bayesian and deterministic artificial neural networks, in: IJCNN '06.
#'    International Joint Conference on Neural Networks, 2006., IEEE.
#'    pp. 4870-4877. \url{http://dx.doi.org/10.1109/ijcnn.2006.247166}.
#'
#' Mount, N.J., Dawson, C.W., Abrahart, R.J., 2013. Legitimising
#'    data-driven models: exemplification of a new data-driven mechanistic
#'    modelling framework. Hydrology and Earth System Sciences 17, 2827-2843.
#'    \url{http://dx.doi.org/10.5194/hess-17-2827-2013}.
#'
#' @seealso \code{\link{ann}}, \code{\link{plot.validann}},
#' \code{\link{predict.ann}}
#' @examples
#' # get validation results for 1-hidden node `ann' model fitted to ar9 data
#' # based on training data.
#' # ---
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#' results <- validann(fit, x = x)
#'
#' # get validation results for above model based on a new sample of ar9 data.
#' # ---
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' obs <- y
#' sim <- predict(fit, newdata = x)
#' results <- validann(fit, obs = obs, sim = sim, x = x)
#'
#' # get validation results for `obs' and `sim' data without ANN model.
#' # In this example `sim' is generated using a linear model. No structural
#' # validation of the model is possible, but `wts' are provided to compute the
#' # number of model parameters needed for the calculation of certain
#' # goodness-of-fit metrics.
#' # ---
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- as.matrix(x[, c(1,4,9)])
#' lmfit <- lm.fit(x, y)
#' sim <- lmfit$fitted.values
#' obs <- y
#' results <- validann(obs = obs, sim = sim, wts = lmfit$coefficients)
#'
#' # validann would be called in the same way if the ANN model used to generate
#' # `sim' was not available or was not of class `ann' or `nnet'. Ideally in
#' # this case, however, both `wts' and `nodes' should be supplied such that
#' # some structural validation metrics may be computed.
#' # ---
#' obs <- c(0.257, -0.891, -1.710, -0.575, -1.668, 0.851, -0.350, -1.313,
#'          -2.469, 0.486)
#' sim <- c(-1.463, 0.027, -2.053, -1.091, -1.602, 2.018, 0.723, -0.776,
#'          -2.351, 1.054)
#' wts <- c(-0.05217, 0.08363, 0.07840, -0.00753, -7.35675, -0.00066)
#' nodes <- c(3, 1, 1)
#' results <- validann(obs = obs, sim = sim, wts = wts, nodes = nodes)
#'
#' @export
#' @importFrom stats cor
#' @importFrom stats fitted
#' @importFrom stats median
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats var
#--------------------------------------------------
validann <- function(...) {

    UseMethod("validann")
}
# -------------------------------------------------------------------------------
#' @describeIn validann Compute validation metrics when \code{net}
#' is of class `ann'.
#' @export
validann.ann <- function(net, obs = NULL, sim = NULL, x = NULL,
                         na.rm = TRUE, ...) {

  results <- list()

  if (is.null(obs) & is.null(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
  } else if (is.null(obs)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'obs' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (is.null(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'sim' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (length(obs) != length(sim)) {
    message1 <- "'obs' and 'sim' must be the same length : "
    message2 <- "'obs' and 'sim' both derived from 'net'"
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }

# Goodness-of-fit.
#----
  npar <- length(net$wts)
  packageStartupMessage("Computing goodness-of-fit...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  packageStartupMessage("Done.")

# Residuals analysis.
#----
  packageStartupMessage("Residuals analysis...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  packageStartupMessage("Done.")

# Structural validation.
#----
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  packageStartupMessage("Computing structural validation metrics...")
  results_struct <- structural_valid(net, x = x)
  results <- append(results, results_struct)
  packageStartupMessage("Done.")

  class(results) <- "validann"
  return(results)
}
# -------------------------------------------------------------------------------
#' @describeIn validann Compute validation metrics when \code{net}
#' is of class `nnet'.
#' @export
validann.nnet <- function(net, obs = NULL, sim = NULL, x = NULL,
                          na.rm = TRUE, ...) {

  results <- list()

  if (is.null(obs) & is.null(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
  } else if (is.null(obs)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'obs' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (is.null(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'sim' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (length(obs) != length(sim)) {
    message1 <- "'obs' and 'sim' must be the same length : "
    message2 <- "'obs' and 'sim' both derived from 'net'"
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }

  # Goodness-of-fit.
  #----
  npar <- length(net$wts)
  packageStartupMessage("Computing goodness-of-fit...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  packageStartupMessage("Done.")

  # Residuals analysis.
  #----
  packageStartupMessage("Residuals analysis...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  packageStartupMessage("Done.")

  # Structural validation.
  #----
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  packageStartupMessage("Computing structural validation metrics...")
  results_struct <- structural_valid(net, x = x)
  results <- append(results, results_struct)
  packageStartupMessage("Done.")

  class(results) <- "validann"

  return(results)
}
# -------------------------------------------------------------------------------
#' @describeIn validann Useful for predictive validation only or when ANN model
#' has not been developed using either \code{\link{ann}} or
#' \code{\link[nnet]{nnet}}. Limited structural validation metrics may be
#' computed and only if \code{wts} and \code{nodes} are supplied.
#' @export
validann.default <- function(obs, sim, wts = NULL, nodes = NULL,
                             na.rm = TRUE, ...) {

  results <- list()

  if (missing(obs) | missing(sim)) {
    stop("Required 'obs' or 'sim' data missing")
  } else if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  }

  if (is.null(wts)) {
    if (!is.null(nodes)) {
      npar <- (nodes[1] + 1) * nodes[2] + (nodes[2] + 1) * nodes[3]
    } else {
      npar <- NULL
    }
  } else {
    npar <- length(wts)
  }

  # Goodness-of-fit.
  #----
  packageStartupMessage("Computing goodness-of-fit...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  packageStartupMessage("Done.")

  # Residuals analysis.
  #----
  packageStartupMessage("Residuals analysis...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  packageStartupMessage("Done.")

  # Structural validation.
  #----
  if(!is.null(wts) && !is.null(nodes)) {
    packageStartupMessage("Computing structural validation metrics...")
    results_struct <- structural_valid(wts = wts, nodes = nodes)
    results <- append(results, results_struct)
    packageStartupMessage("Done.")
  } else {
    message1 <- "'wts' and/or 'nodes' not supplied : "
    message2 <- "structural validity metrics not computed."
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }


  class(results) <- "validann"

  return(results)
}
#-------------------------------------------------------------------------------
predictive_valid <- function(obs, sim, npar = NULL, na.rm = TRUE) {

  if (is.null(npar)) {
    message1 <- "No information on model dimension provided : "
  }
  rem <- vector()
  if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  } else if (any(is.na(obs))) {
    message2 <- "missing values in 'obs'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- which(is.na(obs))
  } else if (any(is.na(sim))) {
    message2 <- "missing values in 'sim'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- c(rem, which(is.na(sim)))
  }
  # Remove NA values if na.rm = TRUE
  if(na.rm & length(rem) > 0) {
    obs <- obs[-rem]
    sim <- sim[-rem]
  }

  resid <- obs - sim #sim - obs
# Compute metrics from HydroTest
  nsamps <- length(obs)
  ame <- max(abs(resid))
  pdiff <- max(obs) - max(sim)
  mae <- sum(abs(resid)) / nsamps
  me <- sum(resid) / nsamps
  rmse <- sqrt( sum( (resid) ^ 2) / nsamps)
  r4ms4e <- ( sum( (resid) ^ 4) / nsamps) ^ (1 / 4)
  if (!is.null(npar)) {
    aic <- nsamps * log(rmse) + 2 * npar
    bic <- nsamps * log(rmse) + npar * log(nsamps)
  } else {
    aic <- NA
    bic <- NA
    message3 <- "AIC and BIC not computed."
    warning(message1, message3, call. = FALSE, immediate. = FALSE)
  }
  #-----
  nsc_calc <- function(resid) {
    sc <- diff(sign(resid))
    sc[sc != 0] <- 1
    sum(sc)
  }
  #-----
  nsc <- nsc_calc(resid)
  rae <- sum( abs(resid) ) / sum( abs(obs - mean(obs)) ) * 100
  pep <- (max(obs) - max(sim)) / max(obs) * 100
  mare <- mean(abs(resid) / obs) * 100
  mdape <- median(abs( (resid) / obs)) * 100
  mre <- mean( (resid) / obs) * 100
  msre <- mean( ( (resid) / obs) ^ 2) * 100
  rve <- sum(resid) / sum(obs) * 100
  rsqr <- (sum( (obs - mean(obs)) * (sim - mean(sim)) ) /
           sqrt( sum( (obs - mean(obs)) ^ 2) *
                 sum( (sim - mean(sim)) ^ 2))) ^ 2
  ioad <- 1 - sum( (resid) ^ 2) /
                  sum((abs(obs - mean(obs)) +
                       abs(sim - mean(obs))) ^ 2)
  ce <- 1 - sum( (resid) ^ 2) /
            sum( (obs - mean(obs)) ^ 2)
  pi <- 1 - sum( (resid[-1]) ^ 2) /
            sum( (obs[-1] - obs[-nsamps]) ^ 2)

  msle <- (log(obs + 1e-08) - log(sim + 1e-08)) ^ 2
  msle <- sum(msle, na.rm = TRUE) / (nsamps - sum(is.na(msle)))
  msde <- sum( ( (obs[-1] - obs[-nsamps]) -
                 (sim[-1] - sim[-nsamps])) ^ 2) / (nsamps - 1)
  delta <- obs[-1] - obs[-nsamps]
  irmse <- sqrt(sum( (delta - mean(delta)) ^ 2) / (nsamps - 1))
  irmse <- rmse / irmse
  ve <- 1 - sum(abs(resid)) / sum(obs)
  alpha <- sd(sim) / sd(obs)
  beta <- mean(sim) / mean(obs)
  r <- cor(sim, obs)
  kge <- 1 - sqrt( (r - 1) ^ 2 + (alpha - 1) ^ 2 + (beta - 1) ^ 2)
  sse <- sum( (resid) ^ 2)


  metrics <- data.frame(AME = ame, PDIFF = pdiff, MAE = mae, ME = me,
                        RMSE = rmse, R4MS4E = r4ms4e, AIC = aic,
                        BIC = bic, NSC = nsc, RAE = rae, PEP = pep,
                        MARE = mare, MdAPE = mdape, MRE = mre,
                        MSRE = msre, RVE = rve, RSqr = rsqr,
                        IoAd = ioad, CE = ce, PI = pi, MSLE = msle,
                        MSDE = msde, IRMSE = irmse, VE = ve,
                        KGE = kge, SSE = sse, R = r)
  obs_stats <- stats(obs)
  sim_stats <- stats(sim)

  return(list(metrics = metrics, obs_stats = obs_stats, sim_stats = sim_stats))
}
# ------------------------------------------------------------------------------
replicative_valid <- function(obs, sim, na.rm = TRUE) {

  rem <- vector()
  if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  } else if (any(is.na(obs))) {
    message2 <- "missing values in 'obs'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- which(is.na(obs))
  } else if (any(is.na(sim))) {
    message2 <- "missing values in 'sim'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- c(rem, which(is.na(sim)))
  }
  # Remove NA values if na.rm = TRUE
  if(na.rm & length(rem) > 0) {
    obs <- obs[-rem]
    sim <- sim[-rem]
  }

  resid <- (obs - sim)
  resid_stats <- stats(resid)

  return(list(residuals = resid, resid_stats = resid_stats))
}
# -------------------------------------------------------------------------------
structural_valid <- function(net, wts = NULL, nodes = NULL, x = NULL) {

  results <- list()

# Compute relative importance of inputs via ocw, modified ocw and Garson's
# methods
  if(missing(net)) {
    net <- NULL
  }
  if(!is.null(net)) {
    ninputs <- ifelse(is.null(net$n), net$nodes[1], net$n[1])
    ri_gars <- garson_fn(net)
    ri_cw <- cw_fn(net)
    ri <- as.data.frame(matrix(c(unlist(ri_gars), unlist(ri_cw)),
                               ncol = ninputs, byrow = TRUE))
    if(!is.null(x)) {
      colnames(ri) <- colnames(x)
    } else {
      colnames(ri) <- paste("inp_", 1:ninputs, sep = "")
    }
    row.names(ri) <- c(names(ri_gars), names(ri_cw))

    if(!is.null(x)) {
      tmp <- profile_sa(net, x)
      y_hat <- tmp$y_hat
      ri_sa <- tmp$ri
      results$y_hat <- y_hat
      ri_old <- ri
      ri <- rbind(ri, ri_sa)
      row.names(ri) <- c(row.names(ri_old), "ri_Profile")
    # If data$net is of class "ann", perform PD sensitivity analysis
      if (inherits(net, "ann")) {
        tmp <- PaD_sa(net, x)
        results$as <- tmp$as
        results$rs <- tmp$rs
        ri_pdsa <- tmp$ri_pdsa
        ri_old <- ri
        if(ncol(x) == 1) {
          ri <- rbind(ri, ri_pdsa[1,])
        } else {
          ri <- rbind(ri, ri_pdsa)
        }
        row.names(ri) <- c(row.names(ri_old), "ri_PaD")
      } else {
        message3 <-
          "'net' not of class \"ann\" : "
        message4 <- "PD sensitivity analysis not performed."
        warning(message3, message4, call. = FALSE, immediate. = FALSE)
      }
    } else {
      message1 <- "Input data (x) missing : "
      message2 <- "No sensitivity analyses performed."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    }
    results$ri <- ri
  } else if (!is.null(wts) && !is.null(nodes)) {
    ninputs <- nodes[1]
    ri_gars <- garson_fn(wts = wts, nodes = nodes)
    ri_cw <- cw_fn(wts = wts, nodes = nodes)
    ri <- as.data.frame(matrix(c(unlist(ri_gars), unlist(ri_cw)),
                               ncol = ninputs, byrow = TRUE))
    if(!is.null(x)) {
      colnames(ri) <- colnames(x)
    } else {
      colnames(ri) <- paste("inp_", 1:ninputs, sep = "")
    }
    row.names(ri) <- c(names(ri_gars), names(ri_cw))
    results$ri <- ri
  }
  return(results)
}
# ------------------------------------------------------------------------------
stats <- function(x) {
  stats_mean <- mean(x)
  stats_min <- min(x)
  stats_max <- max(x)
  stats_var <- var(x)
  stats_sd <- sd(x)
  stats_skew <- moments::skewness(x)
  stats_kurt <- moments::kurtosis(x)
  return(data.frame(mean = stats_mean, min = stats_min, max = stats_max,
                    var = stats_var, sd = stats_sd, skewness = stats_skew,
                    kurtosis = stats_kurt))
}
#-------------------------------------------------------------------------------
profile_sa <- function(net, x) {
  if (!(inherits(net, "nnet") | inherits(net, "ann"))) {
    stop("'net' must be of class \"nnet\" or \"ann\" to perform local
         sensitivity analysis.")
  }

  quarts_x <- apply(x, 2, quantile, probs = seq(0, 1, 0.25))
  y_hat <- vector()
  for (k in 1:ncol(x)) {
    y_hat_0 <- vector()
    for(q in 1:5) {
      x_fix <- matrix(rep(quarts_x[q,], 101), ncol = ncol(x), byrow = TRUE)
      x_tmp <- x_fix
      x_tmp[, k] <- as.vector(quantile(x[, k], prob = seq(0, 1, by = 0.01)))
      if (inherits(net, "nnet")) {
        y_hat_0 <- cbind(y_hat_0, predict(net, newdata = x_tmp,
                                          type = "raw"))
      } else if (inherits(net, "ann")) {
        y_hat_0 <- cbind(y_hat_0, predict(net, newdata = x_tmp))
      }
    }
    y_hat_1 <- apply(y_hat_0, 1, median)
    y_hat <- cbind(y_hat, y_hat_0, y_hat_1)
  }
  colnames(y_hat) <- paste(rep(colnames(x), each = 6),
                           c(seq(0, 1, 0.25)*100, "med"), sep = "_")
  y_hat_meds <- y_hat[,seq(6, 6*ncol(x), 6)]
  y_hat_rng <- apply(y_hat_meds, 2, range)
  ri <- y_hat_rng[2,] - y_hat_rng[1,]
  ri <- ri / sum(abs(ri)) * 100
  return(list(y_hat = y_hat, ri_sa = ri))
}
#-------------------------------------------------------------------------------
PaD_sa <- function(net, x) {
  if (inherits(net, "nnet")) {
    stop("'net' must be of class \"ann\" to perform PD sensitivity analysis.")
  }
  if (!inherits(net, "ann")) stop("'net' not of class \"ann\"")
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  npatterns <- dim(x)[1]
  ninputs <- dim(x)[2]
  O <- net$fitted.values
  if(ncol(net$derivs) == 1) {
    nhn <- 0
    out_derivs <- matrix(net$derivs[, 1], ncol = 1)
    hid_derivs <- NULL
  } else {
    out_derivs <- matrix(net$derivs[, ncol(net$derivs)], ncol = 1)
    hid_derivs <- matrix(net$derivs[, -ncol(net$derivs)],
                         ncol = ncol(net$derivs) - 1)
    nhn <- ncol(hid_derivs)
  }
  relsens <- abssens <- vector()
  if(nhn > 0) {
    for (i in 1:ninputs) {
      sum1 <- rep(0, npatterns)
      for (j in 1:net$nodes[2]) {
        sum1 <- sum1 +
                net$wts[(i - 1) * net$nodes[2] + j] * hid_derivs[, j] *
                net$wts[net$nodes[1] * net$nodes[2] + net$nodes[2] + j] *
                out_derivs[, 1]
      }
      relsens <- cbind(relsens, x[, i] / O * sum1)
      abssens <- cbind(abssens, sum1)
    }
  } else {
    for (i in 1:ninputs) {
      sum1 <- net$wts[i] * out_derivs[, 1]
      relsens <- cbind(relsens, x[, i] / O * sum1)
      abssens <- cbind(abssens, sum1)
    }
  }
  ssd <- matrix(colSums(abssens^2), nrow = 1)
  rmsd <- sqrt(ssd/dim(abssens)[1])
  rmsd <- rmsd / sum(rmsd) * 100
  colnames(relsens) <- colnames(abssens) <- colnames(rmsd) <- names(x)
  return(list(rs = relsens, as = abssens, ri_pdsa = rmsd))
}
#-------------------------------------------------------------------------------
cw_fn <- function(net, wts = NULL, nodes = NULL) {
# -------
# function for calculating overall connection weight (OCW) and
# relative importance (RI) of inputs
# -------

  if (missing(net)) {
    net <- NULL
  }
  if (!is.null(net)) {
    if (inherits(net, "nnet")) {
      nodes <- net$n
      nhn <- nodes[2]
      ninp <- nodes[1]
      wts <- net$wts
      act_fn <- "sigmoid"
      indices <- matrix(seq(1, nodes[1] * nodes[2] + nodes[2]),
                        ncol = nodes[2])
      out_ls <- list()
      for (i in 1:ncol(indices)) {
        out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
      }
      out_ls[["out 1"]] <- wts[(max(indices) + 1):length(wts)]
      ocw <- ocw_mod <- vector()
      for (i in 1:nhn) {
        ocw <- rbind(ocw, out_ls[[i]][2:(ninp + 1)] * out_ls[[nhn + 1]][i + 1])
      }
      ri <- colSums(ocw)
      ri_denom <- sum(abs(ri))
      ri <- ri / ri_denom
      ri <- ri * 100

      return(list(ri_CW = ri))
    } else if (inherits(net, "ann")) {
      nodes <- net$nodes
      nhn <- ifelse(length(nodes) == 3, nodes[2], 0)
      ninp <- nodes[1]
      wts <- net$wts
      act_fn <- net$act_fn[2]
      out_ls <- list()
      if(nhn > 0) {
        indices <- matrix(seq(1, nodes[1] * nodes[2]), ncol = nodes[2],
                          byrow = TRUE)
        for (i in 1:ncol(indices)) {
          out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
        }
        out_ls[["out 1"]] <- wts[(max(indices) + nodes[2] + 1):length(wts)]
        ocw <- ocw_mod <- vector()
        for (i in 1:nhn) {
          ocw_mod <- rbind(ocw_mod, actfn(out_ls[[i]][1:ninp], method = act_fn) *
                             out_ls[[nhn + 1]][i])
          ocw <- rbind(ocw, out_ls[[i]][1:ninp] * out_ls[[nhn + 1]][i])
        }
        ri <- colSums(ocw)
        ri_mod <- colSums(ocw_mod)
      } else {
        out_ls[["out 1"]] <- wts
        ocw_mod <- actfn(out_ls[[1]][1:ninp], method = act_fn)
        ocw <- out_ls[[1]][1:ninp]
        ri <- ocw
        ri_mod <- ocw_mod
      }
      ri_denom <- sum(abs(ri))
      ri <- ri / ri_denom
      ri <- ri * 100

      ri_denom <- sum(abs(ri_mod))
      ri_mod <- ri_mod / ri_denom
      ri_mod <- ri_mod * 100
      if(act_fn == "tanh") {
        return(list(ri_CW = ri, ri_MCW = ri_mod))
      } else {
        return(list(ri_CW = ri))
      }
    } else {
      stop("'net' must be of class \"nnet\" or \"ann\"")
    }
  } else if (!is.null(wts) && !is.null(nodes)) {
    nhn <- nodes[2]
    ninp <- nodes[1]
    out_ls <- list()
    if(nhn > 0) {
      indices <- matrix(seq(1, nodes[1] * nodes[2]), ncol = nodes[2],
                        byrow = TRUE)
      for (i in 1:ncol(indices)) {
        out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
      }
      out_ls[["out 1"]] <- wts[(max(indices) + nodes[2] + 1):length(wts)]
      ocw <- ocw_mod <- vector()
      for (i in 1:nhn) {
        ocw <- rbind(ocw, out_ls[[i]][1:ninp] * out_ls[[nhn + 1]][i])
      }
      ri <- colSums(ocw)
    } else {
      out_ls[["out 1"]] <- wts
      ocw <- out_ls[[1]][1:ninp]
      ri <- ocw
    }
    ri_denom <- sum(abs(ri))
    ri <- ri / ri_denom
    ri <- ri * 100

    return(list(ri_CW = ri))
  } else {
    stop("either 'net' or 'wts' and 'nodes' must be supplied")
  }

}
# ------------------------------------------------------------------------------
garson_fn <- function(net, wts = NULL, nodes = NULL) {
# -------
# Function to calculate Garson's measure of relative importance
# -------

  if (missing(net)) {
    net <- NULL
  }
  if (!is.null(net)) {
    if (inherits(net, "nnet")) {
      nodes <- net$n
      nhn <- ifelse(length(nodes) == 3, nodes[2], 0)
      ninp <- nodes[1]
      wts <- net$wts
      out_ls <- list()
      if(nhn > 0) {
        indices <- matrix(seq(1, nodes[1] * nodes[2] + nodes[2]),
                          ncol = nodes[2])
        for (i in 1:ncol(indices)) {
          out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
        }
        out_ls[["out 1"]] <- wts[(max(indices) + 1):length(wts)]
        ri <- vector()
        for (i in 1:nhn) {
          sum_wi <- sum(abs(out_ls[[i]][2:(ninp + 1)]))
          sum_wo <- sum(abs(out_ls[[nhn + 1]][2:(nhn + 1)]))
          ri <- rbind(ri, abs(out_ls[[i]][2:(ninp + 1)]) / sum_wi *
                        abs(out_ls[[nhn + 1]][i + 1]) / sum_wo)
        }
        ri <- colSums(ri) * 100
      } else {
        out_ls[["out 1"]] <- wts
        sum_wi <- sum(abs(out_ls[[1]][1:ninp]))
        ri <- abs(out_ls[[1]][1:ninp]) / sum_wi * 100
      }
    } else if (inherits(net, "ann")) {
      nodes <- net$nodes
      nhn <- ifelse(length(nodes) == 3, nodes[2], 0)
      ninp <- nodes[1]
      wts <- net$wts
      out_ls <- list()
      if(nhn > 0) {
        indices <- matrix(seq(1, nodes[1] * nodes[2]), ncol = nodes[2],
                          byrow = TRUE)
        for (i in 1:ncol(indices)) {
          out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
        }
        out_ls[["out 1"]] <- wts[(max(indices) + nodes[2] + 1):length(wts)]
        ri <- vector()
        for (i in 1:nhn) {
          sum_wi <- sum(abs(out_ls[[i]][1:ninp]))
          sum_wo <- sum(abs(out_ls[[nhn + 1]][1:nhn]))
          ri <- rbind(ri, abs(out_ls[[i]][1:ninp]) / sum_wi *
                        abs(out_ls[[nhn + 1]][i]) / sum_wo)
        }
        ri <- colSums(ri) * 100
      } else {
        out_ls[["out 1"]] <- wts
        sum_wi <- sum(abs(out_ls[[1]][1:ninp]))
        ri <- abs(out_ls[[1]][1:ninp]) / sum_wi * 100
      }
    } else {
      stop("'net' must be of class \"nnet\" or \"ann\"")
    }
  } else if (!is.null(wts) && !is.null(nodes)) {
    nhn <- nodes[2]
    ninp <- nodes[1]
    out_ls <- list()
    if(nhn > 0) {
      indices <- matrix(seq(1, nodes[1] * nodes[2]), ncol = nodes[2],
                        byrow = TRUE)
      for (i in 1:ncol(indices)) {
        out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
      }
      out_ls[["out 1"]] <- wts[(max(indices) + nodes[2] + 1):length(wts)]
      ri <- vector()
      for (i in 1:nhn) {
        sum_wi <- sum(abs(out_ls[[i]][1:ninp]))
        sum_wo <- sum(abs(out_ls[[nhn + 1]][1:nhn]))
        ri <- rbind(ri, abs(out_ls[[i]][1:ninp]) / sum_wi *
                      abs(out_ls[[nhn + 1]][i]) / sum_wo)
      }
      ri <- colSums(ri) * 100
    } else {
      out_ls[["out 1"]] <- wts
      sum_wi <- sum(abs(out_ls[[1]][1:ninp]))
      ri <- abs(out_ls[[1]][1:ninp]) / sum_wi * 100
    }
  } else {
    stop("either 'net' or 'wts' and 'nodes' must be supplied")
  }

  return(list(ri_Garson = ri))
}
# ------------------------------------------------------------------------------
