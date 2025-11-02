summary.summclust <- function(object, ...) {

  #' A `summary()` method for objects of type `summclust`
  #'
  #' @param object An object of type summclust
  #' @param ... misc arguments
  #' @method summary summclust
  #' @export
  #'
  #' @references
  #' MacKinnon, James G., Morten Ørregaard Nielsen, and Matthew D. Webb.
  #' "Leverage, influence, and the jackknife in clustered regression models:
  #' Reliable inference using summclust."
  #' arXiv preprint arXiv:2205.03288 (2022).
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
  #'
  #' @return A printed summary with pvalues, t-statistics, confidence intervals,
  #' and leverage statistics


  param <- object$params
  N <- object$N

  # get all coefficients of variation
  coef_var_leverage_g <- object$coef_var_leverage_g
  coef_var_partial_leverage <- object$coef_var_partial_leverage
  coef_var_N_G <- object$coef_var_N_G
  coef_var_beta_jack <- object$coef_var_beta_jack


  # jackknifed'd betas
  beta_jack <- as.data.frame(t(object$beta_jack[param, , drop = FALSE]))
  beta <-
    lapply(
      seq_along(param),
      function(x) {
        summ <- summary(beta_jack[, x, drop = TRUE])
        res <- c(summ, coef_var_beta_jack[x])
        names(res)[7] <- "coefvar"
        res

      }
    )

  if (length(param) == 1) {
    beta <- as.data.frame(as.vector(unlist(beta)))
  } else {
    beta <- as.data.frame(Reduce("cbind", beta))
  }

  names(beta) <- paste0("beta-", param)

  # leverage
  leverage <- c(summary(unlist(object$leverage_g)))

  leverage <-  c(
      leverage,
      coef_var_leverage_g
    )

  names(leverage)[7] <- "coefvar"

  # partial leverage
  partial_leverage <- as.data.frame(
    t(
      object$partial_leverage[, , drop = FALSE]
    )
  )

  partial <-
    lapply(
      seq_along(param),
      function(x) {
        summ <- c(
          summary(
            partial_leverage[, x, drop = TRUE]
            )
          )
        res <- c(summ, coef_var_partial_leverage[x])
        names(res)[7] <- "coefvar"
        res
      }
    )

  if (length(param) == 1) {
    partial <- as.data.frame(as.vector(unlist(partial)))
  } else {
    partial <- as.data.frame(Reduce("cbind", partial))
  }

  names(partial) <- paste0("partial-leverage-", param)



  # get number of clusters
  N_G <- c(object$N_G)
  N_G <- c(summary(N_G))
  N_G <- c(N_G, coef_var_N_G)
  names(N_G)[7] <- "coefvar"
  G <- length(N_G)
  N <- object$N
  call <- object$call
  # combine all results
  res <- cbind(c(N_G), leverage, partial, beta)
  colnames(res)[1] <- "N_G"
  # return the results

  print(call)
  cat("", "\n")
  cat("Number of observations:", N, "\n")
  cat("Number of clusters:", G, "\n")
  cat("", "\n")

  print(tidy(object))
  cat("", "\n")
  print(res)
}
