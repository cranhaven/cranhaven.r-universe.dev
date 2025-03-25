#' @title Apollonius diagram and Apollonius graph
#' @description Computation of the Apollonius diagram and the Apollonius graph
#'   of some weighted 2D points. The Apollonius graph is the dual of the
#'   Apollonius diagram. It is also called the additively weighted Voronoï
#'   diagram.
#'
#' @param sites the 2D points, a numeric matrix with two columns (one point
#'   per row)
#' @param radii the weights, a numeric vector of length equal to the number of
#'   points (i.e. the number of rows of \code{sites})
#' @param tmax a positive number passed to \code{\link[gyro]{gyroray}},
#'   controlling the length of the infinite edges (i.e. the hyperbolic rays)
#'   of the Apollonius graph
#' @param nsegs a positive integer, the desired number of points of each
#'   finite edge of the Apollonius graph
#' @param nrays a positive integer, the desired number of points of each
#'   infinite edge of the Apollonius graph
#'
#' @return A list with two fields \code{diagram} and \code{graph}. The
#'   \code{diagram} field is a list providing the sites and the faces of the
#'   Apollonius diagram. The \code{graph} field is a list providing the sites
#'   and the edges of the Apollonius graph.
#' @export
#'
#' @details
#' See the \href{https://doc.cgal.org/latest/Apollonius_graph_2/index.html#Chapter_2D_Apollonius_Graphs}{CGAL documentation}.
#'
#'
#' @importFrom gyro gyromidpoint gyrosegment gyroray gyroABt
#' @importFrom abind abind
#' @importFrom stats uniroot
#'
#' @examples
#' library(Apollonius)
#' sites <- rbind(
#'   c(0, 0),
#'   c(4, 1),
#'   c(2, 4),
#'   c(7, 4),
#'   c(8, 0),
#'   c(5, -2),
#'   c(-4, 4),
#'   c(-2, -1),
#'   c(11, 4),
#'   c(11, 0)
#' )
#' radii <- c(1, 1.5, 1.25, 2, 1.75, 0.5, 0.4, 0.6, 0.7, 0.3)
#' apo <- Apollonius(sites, radii)
#' opar <- par(mar = c(4, 4, 1, 1))
#' plotApolloniusGraph(apo, xlab = "x", ylab = "y")
#' par(opar)
#'
#' # Example of a non-valid graph ####
#' library(Apollonius)
#' sites <- rbind(
#'   c(-1, -1),
#'   c(-1, 1),
#'   c(1, 1),
#'   c(1, -1),
#'   c(0, 0)
#' )
#' angle_ <- seq(0, 2*pi, length.out = 13L)[-1L]
#' circle <- cbind(2 * cos(angle_), 2 * sin(angle_))
#' sites <- rbind(sites, circle)
#' radii <- c(rep(2, 5), rep(1, 12))
#' \dontrun{apo <- Apollonius(sites, radii)}
Apollonius <- function(
    sites, radii, tmax = 30, nsegs = 100L, nrays = 300L
) {
  stopifnot(
    is.numeric(sites), is.matrix(sites), ncol(sites) == 2L, nrow(sites) >= 3L
  )
  storage.mode(sites) <- "double"
  if(anyNA(sites)) {
    stop("Missing values are not allowed.")
  }
  if(anyDuplicated(sites)) {
    stop("Found duplicated sites.")
  }
  stopifnot(is.numeric(radii), length(radii) == nrow(sites), all(radii != 0))
  storage.mode(radii) <- "double"
  if(anyNA(radii)) {
    stop("Found missing value(s) in `radii`.")
  }
  stopifnot(isNumber(tmax), tmax > 1)
  #
  stuff <- ApolloniusCpp(sites, radii)
  neighbors <- stuff[["neighbors"]]
  if(nrow(neighbors) == 0L) {
    stop("The Apollonius diagram is empty.")
  }
  #
  Neighs <- vector("list", nrow(neighbors))
  for(i in 1L:nrow(neighbors)) {
    Neighs_i <- 1L:3L
    neighs_i <- neighbors[i, ]
    remove <- which(is.na(neighs_i))
    for(j in seq_len(i-1L)) {
      if(j %in% neighs_i && i %in% neighbors[j, ]) {
        remove <- c(remove, which(neighs_i == j))
      }
    }
    if(length(remove) > 0L) {
      Neighs_i <- Neighs_i[-remove]
    }
    Neighs[[i]] <- Neighs_i
  }
  #
  commonVertices <-
    abind(stuff[["cvertex1"]], stuff[["cvertex2"]], along = 3L)
  #
  duals    <- stuff[["duals"]]
  vertices <- stuff[["vertices"]]
  #
  nedges <- sum(lengths(Neighs))
  edges  <- vector("list", nedges)
  type   <- character(nedges) # store the type segment or ray
  h <- 1L
  for(i in 1L:nrow(duals)) {
    P1 <- duals[i, ] # it's a point or a line
    infinite <- !is.na(P1[3L]) # it's a line
    Neighs_i <- Neighs[[i]]
    vert_i <- vertices[[i]]
    for(k in seq_along(Neighs_i)) {
      vs <- commonVertices[i, Neighs_i[k], ]
      A  <- vert_i[vs[1L], 1L:2L]
      rA <- vert_i[vs[1L], 3L]
      B  <- vert_i[vs[2L], 1L:2L]
      rB <- vert_i[vs[2L], 3L]
      ctr <- (A + B)/2
      P2 <- duals[neighbors[i, Neighs_i[k]], c(1L, 2L)]
      if(infinite) {
        # we take a point P on the hyperbolic ray, different from P2
        type[h] <- "rays"
        AB <- sqrt(c(crossprod(B-A)))
        u <- (B-A) / AB
        P <- A + (rA + (AB - (rA + rB))/2) * u
      } else {
        # we take a point P on the hyperbolic segment, different from P2
        type[h] <- "segments"
        P <- P1[c(1L, 2L)]
      }
      if(rA != rB) {
        # we solve an equation to find the gyrocurvature s
        f <- function(log_s) {
          d <- ctr + gyromidpoint(P-ctr, P2-ctr, exp(log_s))
          sqrt(c(crossprod(d-A))) - sqrt(c(crossprod(d-B))) - (rA - rB)
        }
        uroot <- uniroot(f, lower = -5, upper = 2, extendInt = "yes")
        s <- exp(uroot[["root"]])
      }
      if(infinite) {
        # P1 = (a, b, c) stores the parameters of the line ax+by+c=0
        a <- P1[1L]
        b <- P1[2L]
        c <- P1[3L]
        cP  <- a*P[1L]  + b*P[2L]  + c
        cP2 <- a*P2[1L] + b*P2[2L] + c
        if(cP < 0) {
          reverse <- cP2 > cP
        } else {
          reverse <- cP2 < cP
        }
        PP2 <- sqrt(c(crossprod(P - P2)))
        tmaxi <- tmax / min(1, PP2)
        nrays2 <- floor(nrays / min(1, PP2))
        if(rA == rB) {
          edges[[h]] <- ray(P2, P, OtoA = !reverse, tmax = tmaxi, n = 2L)
        } else {
          edges[[h]] <-
            t(ctr + t(gyroray(
              P2-ctr, P-ctr, s = s, OtoA = !reverse, tmax = tmaxi, n = nrays2
            )))
        }
      } else {
        if(rA == rB) {
          edges[[h]] <- segment(P, P2, n = 2L)
        } else {
          edges[[h]] <- t(ctr + t(gyrosegment(
            P-ctr, P2-ctr, s = s, n = nsegs
          )))
        }
      }
      h <- h + 1L
    }
  }
  #
  wsites           <- cbind(sites, radii)
  colnames(wsites) <- c("x", "y", "weight")
  dsites <- duals[is.na(duals[, 3L]), ]
  list(
    "diagram" = list("sites" = wsites, "faces" = stuff[["faces"]]),
    "graph"   = list("sites" = dsites, "edges" = split(edges, type))
  )
}
