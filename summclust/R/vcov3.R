vcov_CR3J <- function(obj, ...) {

  #' Compute CRV3 covariance matrices via a cluster
  #' jackknife as described in MacKinnon, Nielsen & Webb
  #' (2022)
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @param obj An object of class `lm` or `fixest`
  #' computed?
  #' @param ... misc function argument
  #' @export
  #'
  #' @return An object of type 'vcov_CR3J'
  #'
  #'@seealso
  #'\link[summclust]{vcov_CR3J.lm},
  #'\link[summclust]{vcov_CR3J.fixest}
  #'
  #' @examples
  #'
  #' library(summclust)
  #' data(mtcars)
  #' mtcars
  #'
  #' fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
  #' summ <- vcov_CR3J(fit, cluster = ~carb)


  UseMethod("vcov_CR3J")
}
