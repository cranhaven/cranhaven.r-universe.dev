#' @useDynLib gyro, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

checkTriangle <- function(A, B, C){
  q <- c(crossprod(A), crossprod(B), crossprod(C))
  ABC <- rbind(A, B, C)
  Dx <- det(cbind(q, ABC[,2L], 1))
  Dy <- -det(cbind(q, ABC[,1L], 1))
  center <- c(Dx,Dy) / det(cbind(ABC, 1)) / 2
  radius <- distance(center, A)
  sqrt(dotprod(center)) + radius < 1
}

#' @title Hyperbolic Delaunay triangulation
#' @description Computes the hyperbolic Delaunay triangulation of a set of
#'   points.
#'
#' @encoding UTF-8
#'
#' @param points points in the unit disk given as a numeric matrix with
#'   two columns
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#'
#' @return A list with five fields \code{vertices}, \code{edges},
#'   \code{triangles}, \code{ntriangles}, and \code{centroids}, a matrix
#'   giving the gyrocentroids of the triangles.
#'   The input \code{points} matrix and the output \code{vertices} matrix
#'   are the same up to the order of the rows if \code{model="M"}, and if
#'   \code{model="U"}, the points in the output \code{vertices} matrix are
#'   obtained by isomorphism.
#' @export
#'
#' @seealso \code{\link{plotHdelaunay}}
#'
#' @importFrom RCDT delaunay
#' @importFrom rgl tmesh3d
#' @importFrom Rvcg vcgGetEdge
#'
#' @examples
#' library(gyro)
#' library(uniformly)
#' set.seed(666)
#' points <- runif_in_sphere(10L, d = 2)
#' hdelaunay(points)
hdelaunay <- function(
    points, model = "M"
){
  stopifnot(is.matrix(points))
  stopifnot(ncol(points) == 2L)
  stopifnot(nrow(points) >= 3L)
  stopifnot(is.numeric(points))
  if(anyNA(points)){
    stop("Missing values in `points`.")
  }
  model <- match.arg(model, c("M", "U"))
  sqnorms <- apply(points, 1L, dotprod)
  if(any(sqnorms >= 1)){
    stop("The points must be in the unit disk.")
  }
  del <- delaunay(points)
  # edges <- del[["edges"]][, c(1L, 2L)]
  del <- del[["mesh"]]
  vertices   <- t(del[["vb"]][c(1L, 2L), ])
  triangles  <- t(del[["it"]])
  ntriangles <- nrow(triangles)
  check <- logical(ntriangles)
  for(i in seq_len(ntriangles)){
    triangle <- triangles[i, ]
    pts <- vertices[triangle, ]
    check[i] <- checkTriangle(pts[1L, ], pts[2L, ], pts[3L, ])
  }
  ntriangles <- sum(check)
  if(ntriangles == 0L){
    stop("Empty Delaunay triangulation.")
  }
  triangles <- triangles[check, ]
  tmesh <- tmesh3d(
    vertices = matrix(1, nrow = 4L, ncol = max(triangles)),
    indices  = t(triangles)
  )
  edges <- vcgGetEdge(tmesh)
  edges <- as.matrix(edges[, c("vert1", "vert2")])
  trianglesList <- split(triangles, seq_len(ntriangles))
  if(model == "M"){
    centroids <-
      t(vapply(trianglesList, function(ids){
        pts <- vertices[ids, ]
        Mgyrocentroid(pts[1L, ], pts[2L, ], pts[3L, ], s = 1)
      }, numeric(2L)))
  }else{
    centroids <-
      t(vapply(trianglesList, function(ids){
        pts <- vertices[ids, ]
        Ugyrocentroid(pts[1L, ], pts[2L, ], pts[3L, ], s = 1)
      }, numeric(2L)))
  }
  if(model == "U"){
    vertices <- t(apply(vertices, 1L, function(A) .PhiUM(A, s = 1)))
  }
  hdel <- list(
    "vertices"   = vertices,
    "edges"      = edges,
    "triangles"  = triangles,
    "ntriangles" = ntriangles,
    "centroids"  = centroids
  )
  class(hdel) <- "hdelaunay"
  attr(hdel, "model") <- model
  hdel
}

#' @title Plot hyperbolic Delaunay triangulation
#' @description Plot a hyperbolic Delaunay triangulation obtained
#'   with \code{\link{hdelaunay}}.
#'
#' @param hdel an output of \code{\link{hdelaunay}}
#' @param vertices Boolean, whether to plot the vertices
#' @param edges Boolean, whether to plot the edges
#' @param circle Boolean, whether to plot the unit circle; ignored for the
#'   Ungar model
#' @param color this argument controls the colors of the triangles; it can be
#'   \code{NA} for no color, \code{"random"} for random colors generated
#'   with \code{\link[colorsGen]{randomColor}}, \code{"distinct"} for
#'   distinct colors generated with
#'   \code{\link[Polychrome]{createPalette}}, a single color,
#'   a vector of colors (color \code{i} attributed to the \code{i}-th
#'   triangle), or a vectorized function mapping each point in the unit
#'   interval to a color
#' @param distinctArgs if \code{color = "distinct"}, a list of arguments
#'   passed to \code{\link[Polychrome]{createPalette}}
#' @param randomArgs if \code{color = "random"}, a list of arguments passed
#'   to \code{\link[colorsGen]{randomColor}}
#'
#' @return No returned value, just generates a plot.
#' @export
#'
#' @importFrom plotrix draw.circle
#' @importFrom graphics par polypath lines points
#'
#' @examples
#' library(gyro)
#' library(uniformly)
#' set.seed(666)
#'
#' points <- runif_in_sphere(35L, d = 2)
#' hdel <- hdelaunay(points, model = "M")
#' plotHdelaunay(hdel)
#'
#' points <- runif_in_sphere(35L, d = 2, r = 0.7)
#' hdel <- hdelaunay(points, model = "U")
#' plotHdelaunay(hdel)
#'
#' # example with colors given by a function ####
#' library(gyro)
#' if(require("trekcolors")) {
#'   pal <- trek_pal("klingon")
#' } else {
#'   pal <- hcl.colors(32L, palette = "Rocket")
#' }
#'
#' phi <- (1 + sqrt(5)) / 2
#' theta <- head(seq(0, pi/2, length.out = 11), -1L)
#' a <- phi^((2*theta/pi)^0.8 - 1)
#' u <- a * cos(theta)
#' v <- a * sin(theta)
#' x <- c(0, u, -v, -u, v)
#' y <- c(0, v, u, -v, -u)
#' pts <- cbind(x, y) / 1.03
#'
#' hdel <- hdelaunay(pts, model = "M")
#'
#' fcolor <- function(t){
#'   RGB <- colorRamp(pal)(t)
#'   rgb(RGB[, 1L], RGB[, 2L], RGB[, 3L], maxColorValue = 255)
#' }
#'
#' plotHdelaunay(
#'   hdel, vertices = FALSE, circle = FALSE, color = fcolor
#' )
plotHdelaunay <- function(
    hdel, vertices = TRUE, edges = TRUE, circle = TRUE,
    color = "random",
    distinctArgs = list(seedcolors = c("#ff0000", "#00ff00", "#0000ff")),
    randomArgs = list(hue = "random", luminosity = "bright")
){
  if(!inherits(hdel, "hdelaunay")){
    stop("The `hdel` argument must be an output of the `hdelaunay` function.")
  }
  model <- attr(hdel, "model")
  opar <- par(mar = c(0, 0, 0, 0))
  pts <- hdel[["vertices"]]
  if(model == "M"){
    plot(
      NULL, type = "n", asp = 1, xlim = c(-1, 1), ylim = c(-1, 1),
      xlab = NA, ylab = NA, axes = FALSE
    )
    if(circle){
      draw.circle(0, 0, radius = 1, border = "black")
    }
  }else{
    plot(
      pts, type = "n", asp = 1,
      xlab = NA, ylab = NA, axes = FALSE
    )
  }
  ntriangles <- hdel[["ntriangles"]]
  if(
    is.function(color) || length(color) > 1L || !is.na(color)
  ){
    triangles <- hdel[["triangles"]]
    if(is.function(color)){
      cnorms <- sqrt(apply(hdel[["centroids"]], 1L, dotprod))
      mincnorm <- min(cnorms)
      colors <- color((cnorms - mincnorm) / (max(cnorms) - mincnorm))
    }else if(length(color) > 1L){
      colors <- color
    }else{
      if(color == "random"){
        colors <- rcolors(ntriangles, randomArgs)
      }else if(color == "distinct"){
        colors <- distinctColors(ntriangles, distinctArgs)
      }else{
        colors <- rep(color, ntriangles)
      }
    }
    for(i in 1L:ntriangles){
      trgl <- triangles[i, ]
      if(model == "M"){
        hpolypath <- rbind(
          Mgyrosegment(pts[trgl[1L], ], pts[trgl[3L], ], s = 1, n = 50)[-1L, ],
          Mgyrosegment(pts[trgl[3L], ], pts[trgl[2L], ], s = 1, n = 50)[-1L, ],
          Mgyrosegment(pts[trgl[2L], ], pts[trgl[1L], ], s = 1, n = 50)[-1L, ]
        )
      }else{
        hpolypath <- rbind(
          Ugyrosegment(pts[trgl[1L], ], pts[trgl[3L], ], s = 1, n = 50)[-1L, ],
          Ugyrosegment(pts[trgl[3L], ], pts[trgl[2L], ], s = 1, n = 50)[-1L, ],
          Ugyrosegment(pts[trgl[2L], ], pts[trgl[1L], ], s = 1, n = 50)[-1L, ]
        )
      }
      polypath(hpolypath, border = NA, col = colors[i])
    }
  }
  if(edges){
    hedges <- hdel[["edges"]]
    for(i in 1L:nrow(hedges)){
      hedge <- hedges[i, ]
      if(model == "M"){
        hseg <- Mgyrosegment(pts[hedge[1L], ], pts[hedge[2L], ], s = 1, n = 50)
      }else{
        hseg <- Ugyrosegment(pts[hedge[1L], ], pts[hedge[2L], ], s = 1, n = 50)
      }
      lines(hseg, lty = "solid", col = "black", lwd = 1.5)
    }
  }
  if(vertices){
    points(pts, pch = 19, cex = 0.9)
  }
  par(opar)
  invisible(NULL)
}
