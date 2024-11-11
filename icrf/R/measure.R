#' @rdname measure
#' @name measure
#'
#' @title Prediction error measures
#'
#' @description This function measures the prediction errors including
#' the IMSE (integrated mean squared error) of type 1 and 2, the integrated
#' absolute error, and the supremum absolute error.
#' When the true survival curve is unknown but the observed interval is available,
#' IMSE is used. When the true survival curve is known, the integrated and supremum
#' absolute errors are used.
#'
#' @param surv.hat the estimated survival curve matrix with rows representing
#' the observations and the columns representing the time points at which the
#' survival curve is estimated.
#' @param timepoints a vector of time points at which the survival curve is estimated.
#' @param tau the study end time. ([0, \code{tau}] is the window for the analysis.)
#' @param method Which measure will be used? Either \code{imse}, \code{int.error} (\code{int.error}
#' returns both integrated and supremum absolute errors), or \code{all} (both) should be entered.
#' @param L,R the left and right interval endpoints. These are required when \code{method == "imse"}
#' or \code{"all"}.
#' @param surv.true the true survival curve matrix with rows representing
#' the observations and the columns representing the time points at which the
#' survival curve is evaluated. This is required when \code{method == "int.error"}
#' or \code{"all"}.
#'
#' @return A vector of prediction errors:
#' \itemize{
#'  \item \code{imse.type1} and \code{imse.type2}
#' when \code{method == "imse"}
#'  \item \code{int.error} and \code{sup.error}
#' when \code{method == "int.error"}
#'  \item \code{imse.type1}, \code{imse.type2}, \code{int.error}, and \code{sup.error}
#' when \code{method == "all"}
#' }
#'
#'
#' @details
#' For details of the error measures, see
#' Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forest"
#'
#'
#' @examples
#' # rats data example.
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' library(survival)  # for Surv()
#' data(rat2)
#' L = ifelse(rat2$tumor, 0, rat2$survtime)
#' R = ifelse(rat2$tumor, rat2$survtime, Inf)
#' \donttest{
#' set.seed(1)
#' rats.icrf <-
#'   icrf(Surv(L, R, type = "interval2") ~ dose.lvl + weight + male + cage.no,
#'        data = rat2, ntree = 10, nfold = 3)
#'
#' measure(rats.icrf$predicted.Sm, timepoints = rats.icrf$time.points,
#'         tau = rats.icrf$tau, method = "imse", L = L, R = R)
#' }
#' \dontshow{
#' set.seed(2)
#'
#' rats.icrf <-
#'   icrf(Surv(L, R, type = "interval2") ~ dose.lvl + weight + male + cage.no,
#'        data = rat2, ntree = 2, nfold = 2)
#'
#' measure(rats.icrf$predicted.Sm, timepoints = rats.icrf$time.points.smooth,
#'         tau = rats.icrf$tau, method = "imse", L = L, R = R)
#' }
#' @author Hunyong Cho hunycho@live.unc.edu, based on the code and the documents of
#' \code{randomForest} by Andy Liaw and Matthew Wiener.
#'
#' @references
#'  \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
#' @export
#' @useDynLib icrf
#'
"measure" <-
  function(surv.hat, timepoints, tau, method = c("all", "imse", "int.error"),
           L = NULL, R = NULL,  # for imse
           surv.true = NULL) { # for integrated error, sup error
    # surv.hat, surv.true must to be n by ntime matrix.

    method = match.arg(method)
    if (method == "all") {
      isIMSE <- isINT <- TRUE
    } else {
      isIMSE = "imse" %in% method
      isINT = "int.error" %in% method
    }

    if ((is.null(L) | is.null(R)) & isIMSE) stop("L and R are needed for imse")
    if ((is.null(surv.true)) & isINT) stop("surv.true is needed for int.error")

    n <- dim(surv.hat)[1]
    ntime <- length(timepoints)
    if (ntime != dim(surv.hat)[2]) stop("sample size of surv.hat differ from that of L and R")
    if (isIMSE) {
      if (n != length(L)| n != length(R)) stop("length of L or R differs from the sample size of surv.hat")
    } else {
      L <- R <- double(1)
    }
    if (isINT) {
      if (n != dim(surv.true)[1]) stop("the sample sizes of surv.hat and surv.true differ")
      if (ntime != dim(surv.true)[2]) stop("the number of timepoints of surv.hat and surv.true differ")
    } else {
      surv.true <- double(1)
    }

    r.inf = is.infinite(R)  # Inf index for R
    lr = c(L, R)
    lr[n + which(r.inf)] <- -1

    t.inf = is.infinite(timepoints) # Inf index for time_interest
    time_interest_fin = timepoints
    time_interest_fin[which(t.inf)] <- -1

# out <- list(
#          as.double(lr),
#          as.double(surv.hat),
#          as.double(surv.true),
#          as.double(time_interest_fin),
#          as.integer(r.inf),
#          as.integer(t.inf),
#          as.integer(n),
#          as.integer(ntime),
#          imse = double(2),
#          interr = double(2), #int.err and sup.err
#          as.double(tau),
#          as.integer(isIMSE),
#          as.integer(isINT))
# return(out)
    out <- .C("survError",
      as.double(lr),
      as.double(surv.hat),
      as.double(surv.true),
      as.double(time_interest_fin),
      as.integer(r.inf),
      as.integer(t.inf),
      as.integer(n),
      as.integer(ntime),
      imse = double(2),
      interr = double(2), #int.err and sup.err
      as.double(tau),
      as.integer(isIMSE),
      as.integer(isINT),
      PACKAGE="icrf")[9:10]
    c("imse.type1" = if (isIMSE) out$imse[1], "imse.type2" = if (isIMSE) out$imse[2],
      "int.error" = if (isINT) out$interr[1], "sup.error" = if (isINT) out$interr[2])
  }
