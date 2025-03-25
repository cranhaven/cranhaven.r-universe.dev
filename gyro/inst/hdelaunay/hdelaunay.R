#' @useDynLib gyro, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

#' @title Hyperbolic Delaunay triangulation
#' @description Computes the hyperbolic Delaunay triangulation of a set of
#'   points in the Poincaré disk.
#'
#' @encoding UTF-8
#'
#' @param points points in the unit disk given as a numeric matrix with
#'   two columns
#' @param isolations Boolean, whether to identify isolated vertices and edges
#' @param centroids Boolean, whether to return the hyperbolic centroids of
#'   the triangles
#' @param exact Boolean, whether to perform exact calculations; slower but
#'   more accurate
#'
#' @return A list with a minima four fields \code{vertices}, \code{edges},
#'   \code{triangles} and \code{ntriangles}, two additional fields
#'   \code{mvertices} and \code{medges} if \code{isolations=TRUE}, giving
#'   the non-isolated vertices and edges ("m" for "multivalent"), and one
#'   additional field \code{centroids} if \code{centroids=TRUE}, a matrix
#'   giving the centroids of the triangles.
#'   The input \code{points} matrix and the output \code{vertices} matrix
#'   are the same up to the order of the rows.
#' @export
#'
#' @note This function uses the C++ library CGAL. As graphs, the hyperbolic
#'   Delaunay triangulation and the Euclidean Delaunay triangulation are the
#'   same, except that the authors of CGAL choosed to discard the non-compact
#'   triangles, i.e. the triangles whose circumcircle is not included in the
#'   Poincaré disk. This is why there can be some isolated vertices (in the
#'   sense that they do not belong to a triangl). The hyperbolic Delaunay
#'   triangulation is always connected (as a graph).
#'
#' @seealso \code{\link{plotHdelaunay}}
#'
#' @examples
#' library(gyro)
#' Htest()
#' library(uniformly)
#' set.seed(666)
#' points <- runif_in_sphere(10L, d = 2)
#' hdelaunay(points)
hdelaunay <- function(
  points, isolations = FALSE, centroids = FALSE, exact = FALSE
){
  stopifnot(is.matrix(points))
  stopifnot(ncol(points) == 2L)
  stopifnot(nrow(points) >= 3L)
  stopifnot(is.numeric(points))
  if(anyNA(points)){
    stop("Missing values in `points`.")
  }
  sqnorms <- apply(points, 1L, dotprod)
  if(any(sqnorms >= 1)){
    stop("The points must be in the unit disk.")
  }
  stopifnot(isBoolean(isolations))
  stopifnot(isBoolean(centroids))
  stopifnot(isBoolean(exact))
  if(exact){
    hdel <- hdelaunay_EK(t(points), isolations)
  }else{
    hdel <- hdelaunay_K(t(points), isolations)
  }
  vertices <- t(hdel[["vertices"]])
  hdel[["vertices"]] <- vertices
  edges <- t(hdel[["edges"]])
  hdel[["edges"]] <- edges
  hdel[["triangles"]] <- t(hdel[["faces"]])
  ntriangles <- ncol(hdel[["faces"]])
  hdel[["faces"]] <- NULL
  hdel[["ntriangles"]] <- ntriangles
  if(isolations){
    mvertices <- hdel[["mvertices"]]
    mv <- apply(edges, 1L, function(edge){
      (edge[1L] %in% mvertices) && (edge[2L] %in% mvertices)
    })
    hdel[["medges"]] <- edges[mv, ]
  }
  if(centroids){
    hdel[["centroids"]] <-
      t(vapply(split(hdel[["triangles"]], seq_len(ntriangles)), function(ids){
        pts <- vertices[ids, ]
        Mgyrocentroid(pts[1L, ], pts[2L, ], pts[3L, ], s = 1)
      }, numeric(2L)))
  }
  class(hdel) <- "hdelaunay"
  hdel
}

#' @title Plot hyperbolic Delaunay triangulation
#' @description Plot a hyperbolic Delaunay triangulation obtained
#'   with \code{\link{hdelaunay}}.
#'
#' @param hdel an output of \code{\link{hdelaunay}}
#' @param remove what you want to remove, \code{NULL} for nothing,
#'   \code{"ivertices"} for the isolated vertices, \code{"iedges"}
#'   for the isolated edges or \code{c("ivertices", "iedges")} for
#'   the isolated vertices and the isolated edges; if not \code{NULL},
#'   this assumes you ran \code{\link{hdelaunay}} with the
#'   option \code{isolations=TRUE}
#' @param vertices Boolean, whether to plot the vertices
#' @param edges Boolean, whether to plot the edges
#' @param circle Boolean, whether to plot the unit circle
#' @param color this argument controls the colors of the triangles; it can be
#'   \code{NA} for no color, \code{"random"} for random colors generated
#'   with \code{\link[randomcoloR]{randomColor}}, \code{"distinct"} for
#'   distinct colors generated with
#'   \code{\link[randomcoloR]{distinctColorPalette}}, a single color,
#'   a vector of colors (color \code{i} attributed to the \code{i}-th
#'   triangle), or a vectorized function mapping each point in the unit
#'   interval to a color
#' @param hue,luminosity passed to \code{\link[randomcoloR]{randomColor}}
#'   if \code{color="random"}
#'
#' @return No returned value.
#' @export
#'
#' @importFrom plotrix draw.circle
#' @importFrom graphics par polypath lines points
#' @importFrom randomcoloR randomColor distinctColorPalette
#'
#' @examples
#' library(gyro)
#' library(uniformly)
#' set.seed(666)
#' points <- runif_in_sphere(35L, d = 2)
#' hdel <- hdelaunay(points, exact = TRUE)
#' plotHdelaunay(hdel)
#'
#' # example with colors given by a function ####
#' library(gyro)
#' library(trekcolors)
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
#' hdel <- hdelaunay(pts, centroids = TRUE, exact = TRUE)
#'
#' fcolor <- function(t){
#'   RGB <- colorRamp(trek_pal("klingon"))(t)
#'   rgb(RGB[, 1L], RGB[, 2L], RGB[, 3L], maxColorValue = 255)
#' }
#'
#' plotHdelaunay(
#'   hdel, vertices = FALSE, circle = FALSE, color = fcolor
#' )
plotHdelaunay <- function(
    hdel, remove = NULL, vertices = TRUE, edges = TRUE, circle = TRUE,
    color = "distinct", hue = "random", luminosity = "random"
){
  if(!inherits(hdel, "hdelaunay")){
    stop("The `hdel` argument must be an output of the `hdelaunay` function.")
  }
  if(!is.null(remove)){
    isolations <- "medges" %in% names(hdel)
    if(!isolations){
      stop(
        "In order to use the `remove` argument you have to run the ",
        "`hdelaunay` function with `isolations=TRUE`."
      )
    }
  }
  opar <- par(mar = c(0, 0, 0, 0))
  plot(
    NULL, type = "n", asp = 1, xlim = c(-1, 1), ylim = c(-1, 1),
    xlab = NA, ylab = NA, axes = FALSE
  )
  if(circle){
    draw.circle(0, 0, radius = 1, border = "black")
  }
  pts <- hdel[["vertices"]]
  ntriangles <- hdel[["ntriangles"]]
  if(
    ntriangles > 0 &&
    (is.function(color) || length(color) > 1L || !is.na(color))
  ){
    triangles <- hdel[["triangles"]]
    if(is.function(color)){
      if(!is.element("centroids", names(hdel))){
        stop(
          "In order to use a function for the `colors` argument you have to ",
          "run the `hdelaunay` function with `centroids=TRUE`."
        )
      }
      cnorms <- sqrt(apply(hdel[["centroids"]], 1L, dotprod))
      mincnorm <- min(cnorms)
      colors <- color((cnorms - mincnorm) / (max(cnorms) - mincnorm))
    }else if(length(color) > 1L){
      colors <- color
    }else{
      if(color == "random"){
        colors <- randomColor(ntriangles, hue = hue, luminosity = luminosity)
      }else if(color == "distinct"){
        colors <- distinctColorPalette(ntriangles)
      }else{
        colors <- rep(color, ntriangles)
      }
    }
    for(i in 1L:ntriangles){
      trgl <- triangles[i, ]
      hpolypath <- rbind(
        Mgyrosegment(pts[trgl[1L], ], pts[trgl[3L], ], s = 1, n = 50)[-1L, ],
        Mgyrosegment(pts[trgl[3L], ], pts[trgl[2L], ], s = 1, n = 50)[-1L, ],
        Mgyrosegment(pts[trgl[2L], ], pts[trgl[1L], ], s = 1, n = 50)[-1L, ]
      )
      polypath(hpolypath, border = NA, col = colors[i])
    }
  }
  if(edges){
    if("iedges" %in% remove){
      hedges <- hdel[["medges"]]
    }else{
      hedges <- hdel[["edges"]]
    }
    for(i in 1L:nrow(hedges)){
      hedge <- hedges[i, ]
      hseg <- Mgyrosegment(pts[hedge[1L], ], pts[hedge[2L], ], s = 1, n = 50)
      lines(hseg, lty = "solid", col = "black", lwd = 1.5)
    }
  }
  if(vertices){
    if("ivertices" %in% remove){
      pts <- pts[hdel[["mvertices"]], ]
    }
    points(pts, pch = 19, cex = 0.9)
  }
  par(opar)
  invisible(NULL)
}
