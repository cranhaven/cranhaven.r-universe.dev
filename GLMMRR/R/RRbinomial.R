#' Binomial family adjusted for Randomized Response parameters.
#'
#' The upper and lower limits for mu's depend on the Randomized Response parameters.
#'
#' @param link
#' a specification for the model link function. Must be an object of class "link-glm".
#' @param c
#' a numeric vector containing the parameter c.
#' @param d
#' a numeric vector containing the parameter d.
#' @param ...
#' other potential arguments to be passed to \code{\link{binomial}}.
#' @return
#' A binomial family object.
#'
#' @export
#' @seealso \code{\link{family}}
RRbinomial <- function (link, c, d, ...)
{
  if (missing(c) || missing(d))
    stop("Randomized Response parameters must be specified")
  linktemp <- substitute(link)
  if (!is.character(linktemp))
    linktemp <- deparse(linktemp)

  if (inherits(link, "link-glm")) {
    stats <- link
    if (!is.null(stats$name))
      linktemp <- stats$name
  }
  else
    stop("link not of type link-glm")

  # Make RR specific adjustments for validmu and mustart, keep the rest default
  bin <- binomial(link = stats, ...)

  # Adjust valid mu for RR parameters
  validmu <- function(mu)
  {
    limits <- data.frame(c + d, c)
    limits[which(limits[1] > limits[2]), c(1,2)] <- limits[which(limits[1] > limits[2]), c(2,1)]
    return (all(is.finite(mu)) && all(mu > as.numeric(limits[[1]]) & mu < as.numeric(limits[[2]])))
  }

  initialize <- expression({
    if (NCOL(y) == 1) {
      if (is.factor(y)) y <- y != levels(y)[1L]
      n <- rep.int(1, nobs)
      y[weights == 0] <- 0
      if (any(y < 0 | y > 1)) stop("y values must be 0 <= y <= 1")

      # Adjust mustart for RR parameters
      limits <- data.frame(family$rr[["c"]] + family$rr[["d"]], family$rr[["c"]])
      limits[which(limits[1] > limits[2]), c(1,2)] <- limits[which(limits[1] > limits[2]), c(2,1)]
      mustart <- ((weights * as.numeric(limits[[2]]) * y + 0.5) / (weights + 1)) + ((weights * as.numeric(limits[[1]]) * (1 - y)) / (weights + 1))
      m <- weights * y
      if (any(abs(m - round(m)) > 0.001)) warning("non-integer #successes in a binomial glm!")
    } else stop("for the 'RRbinomial' family, y must be a vector of 0 and 1's")
  })

  structure(list(family = "binomial", link = linktemp, linkfun = stats$linkfun,
                 linkinv = stats$linkinv, variance = bin$variance, dev.resids = bin$dev.resids,
                 aic = bin$aic, mu.eta = stats$mu.eta, initialize = initialize,
                 validmu = validmu, valideta = stats$valideta, simulate = bin$simfun, rr = list("c" = c, "d" = d)),
            class = "family")
}
