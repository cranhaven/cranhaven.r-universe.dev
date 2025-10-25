#'
#' Generate descriptive summary for objects returned by functions in EHRsampling
#'
#' Generate descriptive summary for objects returned by functions in EHRsampling.
#'
#' @method summary planningML
#'
#' @param object the object returned by other functions.
#' @param ... ignored arguments
#'
#'
#' @return \code{summary()} prints the objects returned by other functions.
#'
#'
#' @export

summary.planningML = function(object, ...){

  ## featureselection -----
  if (!is.null(object$features)) {
    cat("The selected important features are (ranked by importance): \n")
    print(object$features)

  }

  ## sample size determination -----
  if (!is.null(object$outtable)){
    cat("The minimum sample size(s) are: \n")
    print(object$minimum.samplesize)
  }


}
