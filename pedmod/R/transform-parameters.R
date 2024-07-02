#' Transform Between Parameterizations
#'
#' Transform the parameters between the parameterizations that are used in the
#' package.
#'
#' @param par concatenated vector with the fixed effect slopes and the scale
#' parameters that should be transformed.
#' @param n_scales integer with the number of scale parameters.
#' @param jacobian logical indicating if the Jacobian matrix of transformation
#' should be computed.
#'
#' @return \code{standardized_to_direct}:
#' returns the parameters using the direct parameterizations. See
#' \code{vignette("pedmod", package = "pedmod")} for the definition. There is
#' an attribute called 'variance proportions' with the proportion of variance
#' of each effect assuming that all the scale matrices are correlation matrices.
#' There is an attribute called jacobian with the Jacobian matrix if
#' \code{jacobian} is \code{TRUE}.
#'
#' @examples
#' # transform backwards and forwards
#' set.seed(1)
#' smp <- runif(10, -1, 1)
#' res <- standardized_to_direct(smp, 2L, jacobian = TRUE)
#' back_val <- direct_to_standardized(res, 2L)
#'
#' all.equal(smp, back_val, check.attributes = FALSE)
#' res
#'
#' @importFrom utils head tail
#' @export
standardized_to_direct <- function(par, n_scales, jacobian = FALSE){
  stopifnot(is.integer(n_scales), length(n_scales) == 1L,
            n_scales > 0, length(par) >= n_scales)
  betas_org <- head(par, -n_scales)
  scales <- tail(par, n_scales)

  exp_scales <- exp(scales)
  denom <- 1 + sum(exp_scales)
  vars_prev <- exp_scales / denom
  residual_var <- 1 - sum(vars_prev)
  if(residual_var < .Machine$double.eps)
    residual_var <- .Machine$double.eps

  phi <- 1 / residual_var
  betas <- betas_org * sqrt(phi)
  vars <- vars_prev * phi

  jac <- if(jacobian){
    out <- matrix(0., length(par), length(par))

    # fill in the part for the betas
    n_betas <- length(betas)
    diag(out)[seq_len(n_betas)] <- rep(sqrt(phi), n_betas)

    # fill in the parts for the scales and the scales x betas
    d_factors <- exp_scales / 2 / sqrt(denom)
    out[seq_len(n_betas), n_betas + seq_len(n_scales)] <-
      betas_org * rep(d_factors, each = n_betas)

    diag(out)[seq_len(n_scales) + n_betas] <- 1

    out
  } else
    NULL

  structure(c(betas, scales),
            "jacobian" = jac,
            "variance proportions" = c(Residual = residual_var, vars_prev))
}

#' @rdname standardized_to_direct
#' @return \code{direct_to_standardized}:
#' the parameters using the standardized parameterizations. See
#' \code{vignette("pedmod", package = "pedmod")} for the definition.
#' @export
direct_to_standardized <- function(par, n_scales){
  stopifnot(is.integer(n_scales), length(n_scales) == 1L,
            n_scales > 0, length(par) >= n_scales)
  betas <- head(par, -n_scales)
  scales <- tail(par, n_scales)
  exp_scales <- exp(scales)
  total_var <- sum(exp_scales) + 1
  phi <- 1/total_var

  c(betas * sqrt(phi), scales)
}
