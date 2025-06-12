#' @name summary.OEFPIL
#' @title Summary from an OEFPIL object
#' @description Function for fast and clean output of all basic information of an \code{"OEFPIL"} object.
#'
#' @param object an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param signif.level a significance level for the confidence interval. If missing, a value from the input \code{OEFPIL} object is used.
#' @param print print out result summaries in the console (default \code{TRUE}).
#' @param ...   other arguments.
#'
#' @return Returns an object of type list containing following components
#'
#'  \item{param_Est}{the (numerical) vector of estimated model parameters.}
#'  \item{sd}{standard deviations for estimated model parameters.}
#'  \item{cov.m_Est}{the covariance matrix of estimated model parameters.}
#'  \item{it_num}{number of iterations.}
#'  \item{CI_parameters}{the matrix of lower and upper bounds for confidence intervals.}
#'
#'
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' ##Use of summary function with default parameters
#' summary(st1)
#'
#' ##Use of summary function with different parameters
#' summary(st1, signif.level = 0.01, print = FALSE)
#'
#' @method summary OEFPIL
#' @export
summary.OEFPIL <- function(object, signif.level = object$contents$signif.level,
                           print = TRUE,...) {
  ## Function for fast and clean output of all basic information
  ## object  . . . output from OEFPIL
  ## signif.level . . . significance level
  ## print        . . . states, that if we want to output table as well
  ## . . .        . . . additional arguments


  parm <- NULL
  if (IsListOK(object$cov.m_Est)) {

    l <- dim(object$cov.m_Est)[1] ## number of parameters

    summary.form <- object[c(1:l, 2*l+1, 2*l+5, 3*l+6)]
    ## summary.form is portion of list, which contains just components required
    ## for summary function
    summary.form$input.form.string <- object$contents$input.form.string
    ## choice of inevitable parameters from list

    if (IsListOK(summary.form)) {

      l <- dim(summary.form$cov.m_Est)[1] ## number of parameters

      if (length(signif.level) != length(object$contents$signif.level)) {
        summary.form$CI_parameters <- confInt.OEFPIL(object, signif.level = signif.level)
        ## We are calculating new CI, in case of the confidence level was assigned

      } else if (all(signif.level != object$contents$signif.level)) {
        summary.form$CI_parameters <- confInt.OEFPIL(object, signif.level = signif.level)
        ## We are calculating new CI, in case of the confidence level was assigned

      }

      d <- dim(summary.form$CI_parameters)[2]

      Param.mat <- matrix(0, nrow = l, ncol = 2 + d)
      Param.mat[,1] <- unlist(summary.form[1:l]) ## parameters
      Param.mat[,2] <- sqrt(diag(summary.form$cov.m_Est)) ## sd
      Param.mat[,3:(2 + d)] <- summary.form$CI_parameters ## CI

      rownames(Param.mat) <- rownames(summary.form$cov.m_Est)
      colnames(Param.mat) <- c("Param Est", "        Std Dev",
                               paste("  CI Bound", colnames(summary.form$CI_parameters)))

      if (print == TRUE) {
        cat(paste(c("Summary of the result: ", "\n", "\n"), sep = ""))
        cat(paste(summary.form$input.form.string, "\n\n", sep = ""))
        print(Param.mat)
        cat(paste(c("\n", "Estimated covariance matrix:", "\n"), sep = ""))
        print(summary.form$cov.m_Est)
        cat(paste(c("\n", "Number of iterations:", summary.form$it_num, "\n"), sep = ""))
      }

      output <- list(param_Est = Param.mat[,1], sd =  Param.mat[,2],
                     cov.m_Est = summary.form$cov.m_Est, it_num = summary.form$it_num,
                     CI_parameters = Param.mat[,3:(2 + d)])

      return(invisible(output))

    } else {
      logg <- paste("The summary cannot be calculated because of some NaN, NA, Inf or -Inf values",
                    "\n", "in the OEFPIL object. Logs from OEFPIL:", sep = "")
      message(logg)
      message(object$logs)
    }
  }
}
