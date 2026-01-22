#--------------------------------------------------
#' @title Fit Artificial Neural Networks.
#'
#' @description
#' Fits a single hidden layer ANN model to input data \code{x} and output data
#' \code{y}.
#'
#' @param x  matrix, data frame or vector of numeric input values, with
#'    \code{ncol(x)} equal to the number of inputs/predictors and \code{nrow(x)}
#'    equal to the number of examples. A vector is considered to comprise examples
#'    of a single input or predictor variable.
#' @param y  matrix, data frame or vector of target values for examples.
#' @param size number of hidden layer nodes. Can be zero.
#' @param act_hid   activation function to be used at the hidden layer.
#'    See `Details'.
#' @param act_out   activation function to be used at the output layer.
#'    See `Details'.
#' @param Wts   initial weight vector. If \code{NULL} chosen at random.
#' @param rang   initial random weights on [-rang,rang].
#'    Default value is 0.5.
#' @param objfn   objective function to be minimised when fitting
#'    weights. This function may be user-defined with the first two arguments
#'    corresponding to \code{y} (the observed target data) and \code{y_hat}
#'    (the ANN output). If this function has additional parameters which require
#'    optimizing, these must be defined in argument \code{par_of}
#'    (see AR(1) case in `Examples'). Default is \code{sse} (internal
#'    function to compute sum squared error, with error given by
#'    \code{y - y_hat}) when \code{objfn = NULL}.
#' @param method   the method to be used by \code{\link[stats]{optim}}
#'    for minimising the objective function. May be ``Nelder-Mead'', ``BFGS'',
#'    ``CG'', ``L-BFGS-B'', ``SANN'' or ``Brent''. Can be abbreviated.
#'    Default is ``BFGS''.
#' @param maxit maximum number of iterations used by \code{\link[stats]{optim}}.
#'    Default value is 1000.
#' @param abstol   absolute convergence tolerance (stopping criterion)
#'    used by \code{\link[stats]{optim}}. Default is \code{1e-4}.
#' @param reltol   relative convergence tolerance (stopping criterion)
#'    used by \code{\link[stats]{optim}}. Optimization stops if the value
#'    returned by \code{objfn} cannot be reduced by a factor of
#'    \code{reltol * (abs(val) + reltol)} at a step. Default is \code{1e-8}.
#' @param trace   logical. Should optimization be traced?
#'    Default = TRUE.
#' @param \dots  arguments to be passed to user-defined \code{objfn}. Initial
#'    values of any parameters (in addition to the ANN weights) requiring
#'    optimization must be supplied in argument \code{par_of} (see AR(1) case
#'    in `Examples').
#' @return object of class `ann' with components describing the ANN structure
#'    and the following output components:
#' \item{wts}{best set of weights found.}
#' \item{par_of}{best values of additional \code{objfn} parameters. This
#'    component will only be returned if a user-defined \code{objfn} is supplied
#'    and argument \code{par_of} is included in the function call (see AR(1)
#'    case in `Examples').}
#' \item{value}{value of objective function.}
#' \item{fitted.values}{fitted values for the training data.}
#' \item{residuals}{residuals for the training data.}
#' \item{convergence}{integer code returned by \code{\link[stats]{optim}}.
#'    0 indicates successful completion, see \code{\link[stats]{optim}} for
#'    possible error codes.}
#' \item{derivs}{matrix of derivatives of hidden (columns \code{1:size})
#'    and output (final column) nodes.}
#' @details
#'    The ``linear'' activation, or transfer, function is the
#'    identity function where the output of a node is equal to its input
#'    \eqn{f(x) = x}.
#'
#'    The ``sigmoid'' function is the standard logistic sigmoid function given
#'    by \eqn{f(x) = \frac{1}{1+e^{-x}}}{f(x) = 1 / (1 + exp(-x))}.
#'
#'    The ``tanh'' function is the hyperbolic tangent function given by
#'    \eqn{f(x) = \frac{e^{x}-e^{-x}}{e^{x}+e^{-x}}}{
#'       f(x) = (exp(x) - exp(-x)) / (exp(x) + exp(-x))}
#'
#'    The ``exp'' function is the exponential function given by
#'    \eqn{f(x) = e^{x}}{f(x) = exp(x)}
#'
#'    The default configuration of activation functions is
#'    \code{act_hid = "tanh"} and \code{act_out = "linear"}.
#'
#'    Optimization (minimization) of the objective function (\code{objfn}) is
#'    performed by \code{\link[stats]{optim}} using the method specified.
#'
#'    Derivatives returned are first-order partial derivatives of the hidden and
#'    output nodes with respect to their inputs. These may be useful for
#'    sensitivity analyses.
#'
#' @seealso \code{\link{predict.ann}}, \code{\link{validann}}
#'
#' @examples
#' ## fit 1-hidden node ann model with tanh activation at the hidden layer and
#' ## linear activation at the output layer.
#' ## Use 200 random samples from ar9 dataset.
#' ## ---
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#'
#' ## fit 3-hidden node ann model to ar9 data with user-defined AR(1) objective
#' ## function
#' ## ---
#' ar1_sse <- function(y, y_hat, par_of) {
#'   err <- y - y_hat
#'   err[-1] <- err[-1] - par_of * err[-length(y)]
#'   sum(err ^ 2)
#' }
#' fit <- ann(x, y, size = 3, act_hid = "tanh", act_out = "linear", rang = 0.1,
#'            objfn = ar1_sse, par_of = 0.7)
#'
#' @export
#' @importFrom stats optim
#' @importFrom stats predict
#' @importFrom stats residuals
#' @importFrom stats runif
#--------------------------------------------------
ann <- function(x, y, size, act_hid = c("tanh", "sigmoid", "linear", "exp"),
                act_out = c("linear", "sigmoid", "tanh", "exp"), Wts = NULL,
                rang = 0.5, objfn = NULL, method = "BFGS", maxit = 1000,
                abstol = 1.0e-4, reltol = 1.0e-8, trace = TRUE, ...) {

  # --------
  sse <- function(y, y_hat) {
    err <- y - y_hat
    sum(err ^ 2)
  }
  if(is.null(objfn)) objfn <- sse
  # --------
  m <- match.call(expand.dots = TRUE)
  parof_present <- FALSE
  if("par_of" %in% names(m)) {
    parof_present <- TRUE
    par_of <- eval(m$par_of)
  }

  obj_fn <- function(y, y_hat) {
    objfn(y, y_hat, ...)
  }

  act_hid <- match.arg(NULL, act_hid)
  act_out <- match.arg(NULL, act_out)

  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  if (is.vector(y)) {
    y <- matrix(y, ncol = 1)
  }
  if (any(is.na(x))) stop("missing values in 'x'")
  if (any(is.na(y))) stop("missing values in 'y'")
  if (dim(x)[1] != dim(y)[1]) stop("nrows of 'x' and 'y' must match")

  n_patterns <- dim(x)[1]
  n_inputs <- dim(x)[2]
  n_outputs <- dim(y)[2]

  # Set up network
  net <- list()
  class(net) <- "ann"
  if (size == 0) {
    net$layers <- 2
    net$nodes <- c(n_inputs, n_outputs)
    net$act_fn <- c("linear", act_out)
    net$nwts <- as.integer( (net$nodes[1] + 1) * net$nodes[2])
  } else {
    net$layers <- 3
    net$nodes <- c(n_inputs, size, n_outputs)
    net$act_fn <- c("linear", act_hid, act_out)
    net$nwts <- as.integer( (net$nodes[1] + 1) * net$nodes[2] +
                              (net$nodes[2] + 1) * net$nodes[3])
  }
  net$bias <- rep(1, net$layers)
  if (is.null(Wts)) {
    if (rang > 0) {
      wts <- runif(net$nwts, -rang, rang)
    } else {
      wts <- rep(0, net$nwts)
    }
  } else {
    wts <- Wts
  }
  if (length(wts) != net$nwts) stop("weights vector of incorrect length")

  # ----------
  objfn_1 <- function(par, y, obj_fn, ...) {

    y_hat <- array(0, dim = c(n_patterns, n_outputs))
    Zin <- Z <- list(net$layers)

    # Forward propagation of info through network ---
    # For each layer determine input and output to/from nodes
    Zin[[1]] <- as.matrix(x)
    Z[[1]] <- as.matrix(x)
    par_ind <- 0
    for (i in 2:net$layers) {
      i_ind <- rep(1:net$nodes[i], each = net$nodes[i - 1])
      j_ind <- rep(1:net$nodes[i - 1], net$nodes[i])
      par_ind <- max(par_ind) + net$nodes[i] * (j_ind - 1) + i_ind
      Zin[[i]] <- Z[[i - 1]] %*% t(matrix(par[par_ind],
                                          ncol = net$nodes[i - 1],
                                          byrow = TRUE))
      # add bias terms
      par_ind <- max(par_ind) + 1:net$nodes[i]
      Zin[[i]] <- Zin[[i]] + matrix(rep(net$bias[i] * par[par_ind],
                                        nrow(Zin[[i]])),
                                    ncol = net$nodes[i],
                                    byrow = TRUE)
      Z[[i]] <- actfn(Zin[[i]], net$act_fn[i])
    }
    # Calculate model output
    y_hat <- Z[[net$layers]]

    # Evaluation of objective function
    if(parof_present) {
      par_of <- par[(net$nwts + 1):length(par)]
    }
    obj_fn(y, y_hat)
  }
  # ----------
  if(parof_present) {
    par <- c(wts, par_of)
  } else {
    par <- wts
  }
  tmp <- optim(par = par, fn = objfn_1, y = y, obj_fn = obj_fn,
               method = method,
               control = list(maxit = maxit, abstol = abstol, reltol = reltol,
                              trace = trace, REPORT = 20))
  net$value <- tmp$value
  net$wts <- tmp$par[1:(net$nwts)]
  if(parof_present) net$par_of <- tmp$par[(net$nwts + 1):length(tmp$par)]
  net$convergence <- tmp$convergence
  tmp <- predict(net, x, derivs = TRUE)
  net$fitted.values <- tmp$values
  net$derivs <- tmp$derivs
  tmp <- as.matrix(y - tmp$values)
  dimnames(tmp) <- list(rownames(x), colnames(y))
  net$residuals <- tmp
  net$call <- match.call()
  net
}
#-------------------------------------------------------------------------------
#' @title Predict new examples using a trained neural network.
#'
#' @description Predict new examples using a trained neural network.

#' @param object  an object of class `ann' as returned by function \code{ann}.
#' @param newdata    matrix, data frame or vector of input data.
#'    A vector is considered to comprise examples of a single input or
#'    predictor variable. If \code{x} is \code{NULL}, fitted outputs derived
#'    from \code{object} will be returned.
#' @param derivs   logical; should derivatives of hidden and output nodes be
#'    returned? Default is \code{FALSE}.
#' @param \dots  additional arguments affecting the predictions produced (not
#'    currently used).
#' @return if \code{derivs = FALSE}, a vector of predictions is returned.
#'
#' Otherwise, a list with the following components is returned:
#' \item{values}{matrix of values returned by the trained ANN.}
#' \item{derivs}{matrix of derivatives of hidden (columns \code{1:object$size})
#'    and output (final column) nodes.}
#' @details This function is a method for the generic function \code{predict()}
#'    for class `ann'. It can be invoked by calling \code{predict(x)} for an
#'    object \code{x} of class `ann'.
#'
#'    \code{predict.ann} produces predicted values, obtained by evaluating the
#'    `ann' model given \code{newdata}, which contains the inputs to be used
#'    for prediction. If \code{newdata} is omitted, the
#'    predictions are based on the data used for the fit.
#'
#'    Derivatives may be returned for sensitivity analyses, for example.
#'
#' @seealso \code{\link{ann}}
#'
#' @examples
#' ## fit 1-hidden node `ann' model to ar9 data
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#'
#' ## get model predictions based on a new sample of ar9 data.
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' sim <- predict(fit, newdata = x)
#'
#' ## if derivatives are required...
#' tmp <- predict(fit, newdata = x, derivs = TRUE)
#' sim <- tmp$values
#' derivs <- tmp$derivs
#' @export
# ----
predict.ann <- function(object, newdata = NULL, derivs = FALSE, ...) {

# Description: This function runs the model and calculates the
# objective function value for a particular set
# of parameter values
#-----
  if (!inherits(object, "ann"))
    stop("object not of class \"ann\"")
  if (is.null(newdata)) {
    y_hat <- fitted(object)
    return(y_hat)
  } else {
    x <- newdata
    if (is.vector(x)) {
      x <- matrix(x, ncol = 1)
    }
    n_patterns <- dim(x)[1]
    n_inputs <- dim(x)[2]
    x <- matrix(unlist(x), ncol = n_inputs, nrow = n_patterns)
    if (any(is.na(x))) stop("missing values in 'x'")
    n_patterns <- dim(x)[1]
    n_inputs <- dim(x)[2]
    n_outputs <- object$nodes[object$layers]

    y_hat <- array(0, dim = c(n_patterns, n_outputs))
    Zin <- Z <- list(object$layers)

  # Forward propagation of info through network ---
  # For each layer determine input and output to/from nodes
    Zin[[1]] <- x
    Z[[1]] <- x
    par_ind <- 0
    for (i in 2:object$layers) {
      i_ind <- rep(1:object$nodes[i], each = object$nodes[i - 1])
      j_ind <- rep(1:object$nodes[i - 1], object$nodes[i])
      par_ind <- max(par_ind) + object$nodes[i] * (j_ind - 1) + i_ind
      Zin[[i]] <- Z[[i - 1]] %*% t(matrix(object$wts[par_ind],
                                          ncol = object$nodes[i - 1],
                                          byrow = TRUE))
    # add bias terms
      par_ind <- max(par_ind) + 1:object$nodes[i]
      Zin[[i]] <- Zin[[i]] + matrix(rep(object$bias[i] * object$wts[par_ind],
                                        nrow(Zin[[i]])),
                                    ncol = object$nodes[i],
                                    byrow = TRUE)
      Z[[i]] <- actfn(Zin[[i]], object$act_fn[i])
    }
  # Calculate model output and error
    y_hat <- Z[[object$layers]]
    if(derivs) {
      z_hat <- NULL
      o_hat <- NULL
      if (object$layers == 3)
        z_hat <- der_actfn(Zin[[2]], object$act_fn[2])
      if (object$layers == 3) {
        o_hat <- der_actfn(Zin[[3]], object$act_fn[3])
      } else {
        o_hat <- der_actfn(Zin[[2]], object$act_fn[2])
      }
      return(list(values = y_hat, derivs = cbind(z_hat, o_hat)))
    } else {
      return(y_hat)
    }
  }
}
# -------------------------------------------------------------------------------
#' @title Return observed target values.
#' @description
#' Return observed target values used for fitting `ann' or `nnet' ANN models.
#' @param object  an object of class `ann' as returned by \code{\link{ann}} or
#' of class `nnet' as returned by \code{\link[nnet]{nnet}}.
#' @return a 1-column matrix of observed target values.
#' @details This function can be invoked by calling \code{observed(x)} for an
#' object \code{x} of class `ann' or `nnet'.
#'
#' @examples
#' # Get observed values of y used to train ann object `fit'.
#' # ---
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#' y_obs <- observed(fit)
#' @export
# --------
observed <- function(object) {
  UseMethod("observed")
}
# -----------
#' @export
observed.ann <- function(object) {
  fitted(object) + residuals(object)
}
# -----------
#' @export
observed.nnet <- function(object) {
  fitted(object) + residuals(object)
}
#-------------------------------------------------------------------------------
print.ann <- function(x, ...) {
  if (!inherits(x, "ann")) stop("not a legitimate neural net fit")
  if (x$layers == 3) {
    cat("a ", x$nodes[1L], "-", x$nodes[2L], "-", x$nodes[3L],
        " network", sep = "")
    cat(" with", length(x$wts), "weights\n")
    cat(x$act_fn[2L], "hidden units\n")
    cat(x$act_fn[3L], "output units\n")
  } else {
    cat("a ", x$nodes[1L], "-", x$nodes[2L],
        " network (no hidden layer)", sep = "")
    cat(" with", length(x$wts), "weights\n")
    cat(x$act_fn[2L], "output units\n")
  }
  cat("\n")
  invisible(x)
}
#-------------------------------------------------------------------------------
actfn <- function(x, method = c("tanh", "sigmoid", "linear", "exp")) {
  if (method == "tanh") {
    val <- tanh(x)
  } else if (method == "sigmoid") {
    val <- 1 / (1 + exp(-x))
  } else if (method == "linear") {
    val <- x
  } else if (method == "exp") {
    val <- exp(x)
  } else {
    stop("Invalid activation fn : Must be \"tanh\", \"sigmoid\", \"linear\" or \"exp\".")
  }
  return(val)
}
#-------------------------------------------------------------------------------
der_actfn <- function(x, method = c("tanh", "sigmoid", "linear", "exp")) {
  if (method == "tanh") {
    val <- (1 / cosh(x)) ^ 2
  } else if (method == "sigmoid") {
    val <- actfn(x, "sigmoid") * (1 - actfn(x, "sigmoid"))
  } else if (method == "linear") {
    val <- matrix(rep(1, dim(x)[1] * dim(x)[2]), ncol = ncol(x))
  } else if (method == "exp") {
    val <- exp(x)
  } else {
    stop("Invalid activation fn : Must be \"tanh\", \"sigmoid\", \"linear\" or \"exp\".")
  }
  return(val)
}
#-------------------------------------------------------------------------------
