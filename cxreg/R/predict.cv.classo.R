#' make predictions from a "cv.classo" object.
#'
#' This function makes predictions from a cross-validated classo model, using
#' the stored \code{"classo.fit"} object.
#'
#' This function makes it easier to use the results of cross-validation to make
#' a prediction.
#'
#' @aliases coef.cv.classo predict.cv.classo
#' @param object Fitted \code{"cv.classo"} object.
#' @param newx Matrix of new values for \code{x} at which predictions are to be
#' made. Must be a matrix
#' @param s Value(s) of the penalty parameter \code{lambda} at which
#' predictions are required. Default is the value \code{s="lambda.1se"} stored
#' on the CV \code{object}. Alternatively \code{s="lambda.min"} can be used. If
#' \code{s} is numeric, it is taken as the value(s) of \code{lambda} to be
#' used. (For historical reasons we use the symbol 's' rather than 'lambda' to
#' reference this parameter)
#' @param \dots Not used. Other arguments to predict.
#' @return The object returned depends on the \dots{} argument which is passed
#' on to the \code{predict} method for \code{classo} objects.
#' @author Younghoon Kim, Navonil Deb, Sumanta Basu \cr Maintainer:
#' Younghoon Kim <yk748@cornell.edu>
#' @seealso \code{classo}, and \code{print}, and \code{coef} methods, and
#' \code{cv.classo}.
#' @keywords models regression
#'
#' @method predict cv.classo
#' @export
predict.cv.classo <- function(object,newx,s=c("lambda.1se","lambda.min"),...){
  if(is.numeric(s)){
    lambda <- s
  }
  else
    if(is.character(s)){
      s <- match.arg(s)
      lambda <- object[[s]]
      names(lambda) <- s
    }
  else {
    stop("Invalid form for s")
  }
  predict(object$classo.fit,newx,s=lambda,...)
}
