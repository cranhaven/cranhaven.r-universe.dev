#' @importFrom stats quantile
#' @importFrom splines splineDesign
#' @importFrom utils head tail
monotone_ns <- function(times, df, knots = NULL, boundary_knots = NULL,
                        boundary_quantiles = c(.025, .975)){
  stopifnot(is.numeric(times), all(is.finite(times)),
            length(df) == 1, is.finite(df), df > 0,
            length(boundary_quantiles) == 2,
            is.numeric(boundary_quantiles), boundary_quantiles[1] >= 0,
            boundary_quantiles[2] <= 1, diff(boundary_quantiles) > 0)

  if(is.null(knots) || is.null(boundary_knots)){
    qs <- quantile(
      times,
      probs = seq(boundary_quantiles[1], boundary_quantiles[2],
                  length.out = df + 1))

    if(is.null(boundary_knots))
      boundary_knots <- c(qs[1], tail(qs, 1))
    if(is.null(knots))
      knots <- head(qs[-1], -1)
  } else
    df <- length(knots) + 1L

  # construct matrix for the monotonicity constraint for the B-spline
  constrian_mat <- matrix(0., df + 1L, df + 2L)
  for(i in seq_len(df + 1L))
    constrian_mat[i, i:(i + 1L)] <- c(1, -1)

  # construct the constraint for the natural cubic spline
  a_knots <- sort(c(rep(boundary_knots, 4L), knots))
  const <- splineDesign(a_knots, boundary_knots, ord = 4L,
                        derivs = c(2L, 2L))[, -1]

  qr_const <- qr(t(const))
  const_ns <- t(qr.qy(qr_const, t(constrian_mat)))[, -(1:2)]

  # create the function to evaluate the splines
  ns_obj <- ns_ptr(
    boundary_knots = boundary_knots, interior_knots = knots)

  expansion <- function(x, ders = 0L)
    ns_eval(ptr = ns_obj, points = x, ders = ders)

  list(expansion = expansion, boundary_knots = boundary_knots, knots = knots,
       constraints = const_ns)
}
