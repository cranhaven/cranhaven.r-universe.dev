summclust <- function(obj, ...) {

  #' Compute Influence and Leverage Metrics
  #'
  #' Compute influence and leverage metrics for clustered inference
  #' based on the Cluster Jackknife described in MacKinnon, Nielsen & Webb
  #' (2022).
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' @param ... Other arguments
  #' @export
  #'
  #'
  #' @return An object of type `summclust`, including
  #' a CRV3 variance-covariance estimate as described in
  #' MacKinnon, Nielsen & Webb (2022)
  #'
  #' @seealso
  #'\link[summclust]{summclust.lm},
  #'\link[summclust]{summclust.fixest}
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @importFrom cli cli_abort
  #'
  #' @examples
  #'
  #' library(summclust)
  #' data(mtcars)
  #' mtcars
  #'
  #' fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
  #' summ <- summclust(fit, params = ~cyl + disp, cluster = ~carb)
  #' summary(summ)
  #' tidy(summ)
  #' plot(summ)

  UseMethod("summclust")
}
