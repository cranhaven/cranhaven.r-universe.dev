#' @title Multiple integral over a polyhedron
#' @description Multiple integral over a convex polyhedron given by a set of
#'   linear inequalities. See the vignette for explanations and examples.
#'
#' @param f either a function, a \strong{spray} polynomial, or a
#'   \strong{qspray} polynomial; its number of variables must match the
#'   number of columns of the matrix \code{A}
#' @param A,b matrix and vector defining the linear inequalities which must be
#'   in numeric mode or, for exactness, in character mode, with an integer or
#'   a fraction as each entry; if \code{f} is a \strong{qspray} polynomial,
#'   then \code{A} and \code{b} will be converted to character mode if they
#'   are in numeric mode, with the function \code{\link[rcdd]{d2q}}
#'
#' @return There are three possible values: an output of
#'   \code{\link[SimplicialCubature]{adaptIntegrateSimplex}} if
#'   \code{f} is a function, an output of
#'   \code{\link[SimplicialCubature]{integrateSimplexPolynomial}} if
#'   \code{f} is a \strong{spray} polynomial, or a character representing
#'   the value of the integral as a fraction if
#'   \code{f} is a \strong{qspray} polynomial.
#' @export
#'
#' @importFrom rcdd makeH validcdd scdd q2d d2q qsum
#' @importFrom tessellation delaunay getDelaunaySimplicies
#' @importFrom SimplicialCubature adaptIntegrateSimplex definePoly integrateSimplexPolynomial
#' @importFrom spray is.spray index
#' @importFrom qspray integratePolynomialOnSimplex
#'
#' @examples
#' A <- rbind(
#'   c(-1, 0, 0), # -x
#'   c( 1, 0, 0), # x
#'   c( 0,-1, 0), # -y
#'   c( 1, 1, 0), # x+y
#'   c( 0, 0,-1), # -z
#'   c( 1, 1, 1)  # x+y+z
#' )
#' b <- c(
#'   5, 4,  # -5 < x < 4       <=> -x < 5  &  x < 4
#'   5, 3,  # -5 < y < 3-x     <=> -y < 5  &  x+y < 3
#'   10, 6  # -10 < z < 6-x-y  <=> -z < 10  &  x+y+z < 6
#' )
#' f <- function(x, y, z) {
#'   x*y + 5*cos(z)
#' }
#' integrateOverPolyhedron(f, A, b)
integrateOverPolyhedron <- function(f, A, b) {
  stopifnot(is.matrix(A))
  stopifnot(nrow(A) == length(b))
  stopifnot(is.numeric(A) || is.character(A))
  stopifnot(is.numeric(b) || is.character(b))
  if(mode(A) != mode(b)) {
    stop(
      "The matrix `A` and the vector `b` must have the same mode, ",
      "numeric or character."
    )
  }
  is_qspray <- inherits(f, "qspray")
  isnot_qspray <- is.function(f) || is.spray(f)
  if(!(is_qspray || isnot_qspray)) {
    stop("Invalid argument `f`.")
  }
  # make H-representation
  if(is_qspray) {
    if(!is.character(A)) {
      A <- d2q(A)
    }
    if(!is.character(b)) {
      b <- d2q(b)
    }
  }
  H <- makeH(A, b)
  . <- validcdd(H)
  # make V-representation
  V <- scdd(H)[["output"]]
  if(isnot_qspray && is.character(V)) {
    V <- q2d(V)
  }
  if(any(V[, 1L] != 0) || any(V[, 2L] != 1)) {
    stop(
      "The arguments `A` and/or `b` do not define a convex polyhedron."
    )
  }
  # Delaunay
  vertices <- V[, -c(1L, 2L)]
  if(is_qspray) {
    dlny <- delaunay(q2d(vertices), atinfinity = TRUE)
  } else {
    dlny <- delaunay(vertices, atinfinity = TRUE)
  }
  simplices <- getDelaunaySimplicies(dlny)
  nsimplices <- length(simplices)
  if(isnot_qspray) {
    # union of the simplices
    d <- ncol(A)
    U <- array(NA_real_, dim=c(d, d+1L, nsimplices))
    for(i in seq_len(nsimplices)) {
      U[, , i] <- t(simplices[[i]])
    }
    # integrate
    if(is.function(f)) {
      if(length(formals(f)) != d) {
        stop(
          "The number of arguments of `f` does not match the dimension."
        )
      }
      g <- function(v) NULL
      bdy <- sprintf(
        "f(%s)", paste0(sprintf("v[%d]", 1L:d), collapse = ", ")
      )
      body(g) <- parse(text = bdy)
      adaptIntegrateSimplex(g, U)
    } else if(is.spray(f)) {
      powers <- index(f)
      if(ncol(powers) != d) {
        stop(
          "The number of variables in `f` does not match the dimension."
        )
      }
      P <- definePoly(f[["value"]], powers)
      integrateSimplexPolynomial(P, U)
    }
  } else { # qspray
    results <- character(nsimplices)
    for(i in seq_len(nsimplices)) {
      S <- simplices[[i]]
      Svs <- as.integer(rownames(S))
      qS <- vertices[Svs, ]
      results[i] <- as.character(integratePolynomialOnSimplex(f, qS))
    }
    qsum(results)
  }
}
