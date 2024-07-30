#' plots for \code{netcox} and \code{netcox_cv}
#'
#' @description
#' Plot the solution path produced by \code{\link{netcox}} or \code{\link{netcox_cv}}.
#' 
#' @param netcox_obj The \code{\link{netcox}} or \code{\link{netcox_cv}} object.
#' @param plot_min Logical, whether a vertical line at lambda.min acquired by \code{netcox_cv} is plotted. Not available if \code{netcox_obj} is a \code{netcox} fit.
#' @param plot_1se Logical, whether a vertical line at lambda.1se acquired by \code{netcox_cv} is plotted. Not available if \code{netcox_obj} is a \code{netcox} fit.
#' @param type Graphical argument to be passed to \link{matplot}, a character string (length 1 vector) or vector of 1-character strings indicating the type of plot for each column of y, see \link{plot.default} for all possible types. Default is "l" for lines.
#' @param log Graphical argument to be passed to \link{matplot}, a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic, "" if neither, "xy" or "yx" if both axes are to be logarithmic. Default is "x".
#' @param ... Further arguments of \link{matplot} and ultimately of \link{plot.default} for some.
#' @return
#' Produces a coefficient profile plot of the coefficient paths for a fitted \code{\link{netcox}} or \code{\link{netcox_cv}} object.
#' 
#' @examples 
#' grp <- matrix(c(0, 0, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 1, 1, 0, 0, 0,
#'                 0, 0, 0, 0, 0,
#'                 0, 1, 0, 1, 0),
#'               ncol = 5, byrow = TRUE)
#' grp.var <- matrix(c(1, 0, 0, 0, 0, #A1
#'                     1, 0, 0, 0, 0, #A2
#'                     0, 0, 0, 1, 0, #C1
#'                     0, 0, 0, 1, 0, #C2
#'                     0, 1, 0, 0, 0, #B
#'                     0, 0, 1, 0, 0, #A1B
#'                     0, 0, 1, 0, 0, #A2B
#'                     0, 0, 0, 0, 1, #C1B
#'                     0, 0, 0, 0, 1  #C2B
#'                    ), ncol = 5, byrow = TRUE)
#' eta_g <- rep(1, 5)
#' x <- as.matrix(sim[, c("A1","A2","C1","C2","B",
#'                        "A1B","A2B","C1B","C2B")])
#' lam.seq <- 10^seq(0, -2, by = -0.2)
#' # plot solution path from a netcox fit
#' fit <- netcox(x = x,
#'               ID = sim$Id,
#'               time = sim$Start,
#'               time2 = sim$Stop,
#'               event = sim$Event,
#'               lambda = lam.seq,
#'               group = grp,
#'               group_variable = grp.var,
#'               penalty_weights = eta_g,
#'               tol = 1e-4,
#'               maxit = 1e3,
#'               verbose = FALSE)
#' plot_netcox_sp(netcox_obj = fit)
#'               
#' # plot solution path from a netcox_cv fit              
#' cv <- netcox_cv(x = x,
#'                 ID = sim$Id,
#'                 time = sim$Start,
#'                 time2 = sim$Stop,
#'                 event = sim$Event,
#'                 lambda = lam.seq,
#'                 group = grp,
#'                 group_variable = grp.var,
#'                 penalty_weights = eta_g,
#'                 nfolds = 5,
#'                 tol = 1e-4,
#'                 maxit = 1e3,
#'                 verbose = FALSE)
#' plot_netcox_sp(netcox_obj = cv, plot_min = TRUE, plot_1se = TRUE)               
#' @seealso \code{\link{netcox}}, \code{\link{netcox_cv}}.

plot_netcox_sp <- function(netcox_obj,
                           plot_min = FALSE,
                           plot_1se = FALSE,
                           type = "l",
                           log = "x", ...) {
  lambdas <- netcox_obj$lambdas
  
  estimates <- netcox_obj$estimates
  
  if (is.null(estimates)) {
    estimates <- netcox_obj$netcox.fit$estimates
  }
  
  matplot(x = lambdas, y = t(estimates),
          xlab = expression(paste(lambda)),
          ylab = "",
          type = type, log = log, ...)
  abline(h = 0, lty = 2)
  
  if (plot_min) {
    lambda.min <- netcox_obj$lambda.min
    if (is.null(lambda.min)) {
      stop("lambda.min needs to be acquired by `netcox_cv()`.")
    }
    abline(v = lambda.min, lty = 3)
  }
  
  if (plot_1se) {
    lambda.1se <- netcox_obj$lambda.1se
    if (is.null(lambda.min)) {
      stop("lambda.1se needs to be acquired by `netcox_cv()`.")
    }
    abline(v = lambda.1se, lty = 3)
  }
}
