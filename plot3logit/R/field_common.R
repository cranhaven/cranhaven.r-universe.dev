
#' Computes the coordinates of a vertex on the edge
#'
#' Given the coordinates of one of vertices, [v2vedge()] computes the
#' coordinates of the vertex on the edge.
#'
#' @param v `numeric` vector of ternary coordinates.
#' @param edge `numeric` value of the edge.
#'
#' @returns `numeric` vector of ternary coordinates of the vertex on the edge.
#'
#' @keywords internal
v2vedge<- function(v, edge) {
  return((1 - 3 * edge) * v + edge)
}



#' Computation of starting points of curves of the field
#'
#' Given the output of [DeltaB2pc_cat3logit()] or
#' [DeltaB2pc_ord3logit()], the coordinates of points on the
#' edge are computed for each curve of the field of points
#' given in input.
#'
#' @inheritParams DeltaB2pc
#' @param pc `list` of ternary coordinates, as returned by
#'   [DeltaB2pc_cat3logit()] or [DeltaB2pc_ord3logit()].
#'   Function `pc2p0_single` accepts only a single point
#'   (that is a `numeric` vector of length three).
#' @param flink named `list` of two functions: `linkfun`
#'   and `linkinv`. The former is the link function, whereas
#'   the latter is its inverse.
#'
#' @returns A named `list` with two components:
#' 
#' \item{status}{a `character` always equal to `"p0"`
#'   (see section *Value* of [DeltaB2pc()]).}
#' \item{pp}{a `list` of ternary coordinates.}
#'
#' @keywords internal
pc2p0<- function(pc, DeltaB, edge = 0.01, flink) {
  out <- pc

  if (pc$status == 'pc') {
  	out$pp <- lapply(pc$pp, pc2p0_single, DeltaB = DeltaB, w = pc$fo,
  	  edge = edge, flink = flink)
  	out$status <- 'p0'
  	out$fo <- NULL
  } 
  
  out
}



#' @rdname pc2p0
#' @keywords internal
pc2p0_single<- function(pc, DeltaB, w, edge, flink) {
  XBc<- flink$linkfun(pc)

  fnorma<- function(x) {
  	flink$linkinv(XBc - x * DeltaB) %>%
  	  extract(w == 1) %>%
  	  min %>%
  	  subtract(edge) %>%
  	  return()
  }

  alpha <- 10
  while (all(flink$linkinv(XBc - alpha * DeltaB) > edge)) { alpha %<>% multiply_by(3) }

  uniroot(fnorma, lower = 0, upper = alpha * max(abs(DeltaB)), extendInt = 'downX') %>%
    use_series('root') %>%
    { flink$linkinv(XBc - . * DeltaB) } %>%
    return()
}



#' Generates a curve of the field
#'
#' Given the ternary coordinates of the starting point of the curve, it
#' generates the path of arrows until the edge of the ternary plot is
#' reached.
#'
#' @inheritParams pc2p0
#' @param p0 starting point of the curve.
#' @param nmax maximum number of vectors.
#'
#' @returns Object of class `list`, where each component is a `list` of two
#' components: the ternary coordinates of the starting point of the arrow, and
#' the ternary coordinates of the tip of the arrow.
#'
#' @seealso [linkfun()], [linkinv()].
#'
#' @keywords internal
gen_path <- function(p0, DeltaB, edge = 0.01, nmax = Inf, flink) {
  xB <- flink$linkfun(p0)
  out <- NULL
  n <- 1
  continua <- TRUE
  inter<- 0.1 * DeltaB / max(abs(DeltaB))

  while(continua & (n <= nmax)) {
  	out %<>% rbind(cbind(n, flink$linkinv(xB)))
  	xB %<>% add(DeltaB)
  	out %<>% rbind(cbind(n, flink$linkinv(xB)))
  	xB %<>% add(inter)
  	n %<>% add(1)
  	if (any(tail(out, n = 1)[-1] < edge)) { continua <- FALSE }
  }
  
  out %<>%
    {
    	  by(.[ , -1],factor(paste0('A',.[ , 1])),as.matrix) %>%
  	    lapply(function(x) list(from = unname(x[1, ]), to = unname(x[2, ])))
  	}
  
  return(out)
}


