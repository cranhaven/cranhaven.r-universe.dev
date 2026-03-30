#' Support of Distributions
#'
#' @description
#' Returns the support of a distribution. This function is adapted from
#' the 'SAMprior' package (v1.1.1) to support the \code{\link{runSAM}} function.
#'
#' @param mix Mixture distribution.
#' @param x Values to be transformed.
#' @param l Link values.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds
#'   of the support for the specified mixture distribution. If the mixture
#'   type is unknown (default method), it returns the character string
#'   "Unknown mixture".
#'
#' @name support
#' @keywords internal
NULL

#' @rdname support
#' @keywords internal
mixlink <- function(mix, x)
  attr(mix, "link")$link(x)

#' @rdname support
#' @keywords internal
mixinvlink <- function(mix, x)
  attr(mix, "link")$invlink(x)

#' @rdname support
#' @keywords internal
mixJinv_orig <- function(mix, x)
  attr(mix, "link")$Jinv_orig(x)

#' @rdname support
#' @keywords internal
mixlJinv_orig <- function(mix, x)
  attr(mix, "link")$lJinv_orig(x)

#' @rdname support
#' @keywords internal
mixlJinv_link <- function(mix, l)
  attr(mix, "link")$lJinv_link(l)

#' @rdname support
#' @keywords internal
is.dlink <- function(x)
  inherits(x, "dlink")

#' @rdname support
#' @keywords internal
is.dlink_identity <- function(x)
  is.dlink(x) & x$name == "identity"

#' @rdname support
#' @keywords internal
is.mixidentity_link <- function(mix, l)
  is.dlink_identity(attr(mix, "link"))

#' @rdname support
#' @export
support <- function(mix) UseMethod("support")

#' @rdname support
#' @export
support.default <- function(mix) "Unknown mixture"

#' @rdname support
#' @export
support.betaMix <- function(mix) mixlink(mix, c(0,1))

#' @rdname support
#' @export
support.gammaMix <- function(mix) mixlink(mix, c(0,Inf))

#' @rdname support
#' @export
support.normMix <- function(mix) mixlink(mix, c(-Inf,Inf))

