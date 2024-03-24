
#' Identification of equispaced central points
#'
#' Given the effect \eqn{\Delta^TB\in\textbf{R}^{1\times 2}} of a change
#' \eqn{\Delta\in\textbf{R}^k} in the vector of covariates
#' \eqn{x\in\textbf{R}^k} on the linear predictor
#' \eqn{x^TB\in\textbf{R}^{n\times 2}}, it computes
#' the set of points that makes the curves of the field equally spaced.
#'
#' @param DeltaB either a matrix \eqn{\Delta^TB\in\textbf{R}^{1\times 2}}
#'   or a vector of length 2, if the model is *categorical*; otherwise
#'   a matrix \eqn{\Delta^TB\in\textbf{R}^{1\times 1}} or a `numeric`,
#'   if the model is *ordinal*.
#' @param n number of points (curves of the field).
#' @param edge width of the border of the ternary plot.
#'
#' @returns A named `list` with three components:
#' 
#' \item{status}{a `character` which may be either equal to
#'	 `"p0"` or `"pc"`. The former value (`"p0"`) is taken
#'   when the point is the origin of the curve, whereas
#'   the latter (`"pc"`) means that the point is *over* the
#'   curve, and the origin should be computed (see
#'   [`pc2p0`]).}
#' \item{fo}{the filter of the sides where the field
#'   originates from.}
#' \item{pp}{a `list` of ternary coordinates.}
#'
#' @name DeltaB2pc
NULL


#' @rdname DeltaB2pc
#' @keywords internal
DeltaB2pc_cat3logit <- function(DeltaB, n = 8, edge = 0.01) {
  out <- NULL
  DeltaB %>%
    c(0,.) %>%
    { outer(., ., '==') } %>%
    { .[lower.tri(.)]} -> xcmp
  
  switch(as.character(sum(xcmp)),
    '0' = DeltaB2pc_cat3logit_dim3(DeltaB, n, edge),
    '1' = DeltaB2pc_cat3logit_dim2(DeltaB, n, edge),
    '3' = DeltaB2pc_cat3logit_dim1(DeltaB, n, edge)
  )
}



#' @rdname DeltaB2pc
#' @keywords internal
DeltaB2pc_cat3logit_dim1 <- function(DeltaB, n, edge) {
  out <- list(status = 'p', fo = NULL)

  { edge + (0 : (n - 1)) / (n - 1) * (1 - 3 * edge) } %>%
  	{ expand.grid(p1 = ., p2 = .) } %>%
  	{ .$p3 <- 1 - .$p1 - .$p2; . } %>%
  	{ .[which((.$p3 >= 0) & (.$p3 <= 1)), ] } %>%
  	lapply(I) %>%
  	{ mapply(c, .$p1, .$p2, .$p3, SIMPLIFY = FALSE) } -> out$pp
  
  out
}



#' @rdname DeltaB2pc
#' @keywords internal
DeltaB2pc_cat3logit_dim2 <- function(DeltaB, n, edge) {
  out <-
  if ((DeltaB[1] == 0) & (DeltaB[2] > 0)) {
	ww <- list(wA = c(1, 0, 0), wB = c(0, 1, 0))
	out <- list(status = 'p0')
  } else if ((DeltaB[1] > 0) & (DeltaB[2] == 0)) {
	ww <- list(wA = c(1, 0, 0), wB = c(0, 0, 1))
	out <- list(status = 'p0')	
  } else if ((DeltaB[1] > 0) & (DeltaB[2] > 0)) {
	ww <- list(wA = c(0, 1, 0), wB = c(0, 0, 1))
	out <- list(status = 'pc')	
  } else if ((DeltaB[1] == 0) & (DeltaB[2] < 0)) {
	ww <- list(wA = c(1, 0, 0), wB = c(0, 1, 0))
	out <- list(status = 'pc')	
  } else if ((DeltaB[1] < 0) & (DeltaB[2] == 0)) {
	ww <- list(wA = c(1, 0, 0), wB = c(0, 0, 1))
	out <- list(status = 'pc')	
  } else if ((DeltaB[1] < 0) & (DeltaB[2] < 0)) {
	ww <- list(wA = c(0, 1, 0), wB = c(0, 0, 1))
	out <- list(status = 'p0')	
  }
  
  out['fo'] <- switch(out$status,
    'p0' = list(NULL),
    'pc' = list(ww$wA + ww$wB))
    
  ww %<>% lapply(v2vedge, edge = edge)
  out$pp <- convex_comb((0.5 + (0 : (n - 1))) / n, ww$wA, ww$wB, simplify = FALSE)
  out
}



#' @rdname DeltaB2pc
#' @keywords internal
DeltaB2pc_cat3logit_dim3 <- function(DeltaB, n, edge) {
  vv <- DeltaB2vroles_cat3logit(DeltaB)
  wA <- v2vedge(vv$vt, edge)
  wB <- 0.5 * (v2vedge(vv$vo, edge) + v2vedge(vv$vs, edge))

  convex_comb((0.5 + (0 : (n - 1))) / n, wA, wB, simplify = FALSE) %>% 
    { list(status = 'pc', fo = 1 - vv$vo, pp = .) } %>%
    return()
}



#' @rdname DeltaB2pc
#' @keywords internal
DeltaB2pc_ord3logit <- function(DeltaB, alpha, n = 8, edge = 0.01) {
  if (DeltaB != 0) {
  	c(0.5, NA, NA) %>%
  	  linkfun_ord3logit(alpha) %>%
  	  linkinv_ord3logit(alpha) %>%
  	  apply(1, list) %>%
  	  Reduce(c, .) %>%
  	  { list(status = 'pc', fo = NULL, pp = .) } -> out
  	  if (DeltaB > 0) { out$fo <- c(0, 1, 1) } else { out$fo <- c(1, 1, 0) }
  } else {
  	cbind((0.5 + (0 : (n - 1))) / n, NA, NA) %>%
  	  linkfun_ord3logit(alpha) %>%
  	  linkinv_ord3logit(alpha) %>%
  	  apply(1, list) %>%
  	  Reduce(c, .) %>%
  	  { list(status = 'p', fo = NULL, pp = .) } -> out
  }
  
  out
}


