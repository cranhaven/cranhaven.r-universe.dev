#' @importFrom generics tidy
#' @export
generics::tidy


tidy.summclust <- function(x, ...) {


  #' S3 method to summarize objects of class boottest into tidy data.frame
  #'
  #' Obtain results from a `summclust` object in a tidy data frame.
  #'
  #'@references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
  #'
  #' @param x An object of class 'summclust'
  #' @param ... Other arguments
  #' @export
  #' @method tidy summclust
  #' @importFrom stats qt pt
  #' @importFrom generics tidy
  #'
  #'
  #' @return
  #'
  #' A data.frame containing coefficient estimates,
  #' t-statistics, standard errors, p-value, and confidence
  #' intervals based on CRV3 variance-covariance matrix
  #' and t(G-1) distribution
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


  param <- x$params
  N <- x$N
  vcov <- x$vcov
  G <- length(x$cluster)
  param_ <- which(names(x$coef_estimates) %in% param)
  param_vals <- x$coef_estimates[param_]
  se <- sqrt(diag(vcov[param_, param_, drop = FALSE]))
  cv3t <- param_vals / se

  conduct_inference <- function(x, param_vals, se, cv3t) {
    conf_int <- param_vals[x] + c(-1, 1) * qt(0.025, df = G - 1) * se[x]
    p_val <- 2 * min(pt(cv3t[x], G - 1), 1 - pt(cv3t[x], G - 1))

    res <-
      data.frame(
        coef = param_vals[x],
        tstat = cv3t[x],
        se = se[x],
        p_val = p_val,
        conf_int_l = conf_int[2],
        conf_int_u = conf_int[1]
      )

    res
  }


  vals <-
    lapply(
      seq_along(param),
      function(x) {
        conduct_inference(x,
          param_vals = param_vals,
          se = se,
          cv3t = cv3t
        )
      }
    )

  res <- as.data.frame(Reduce("rbind", vals))

  res
}


