################################################################################
#' Heading between spatial points.
#'
#' Determines the heading between spatial points.
#'
#' @param from The starting position; an object of class `SpatVector`.
#'
#' @param to The ending position; an object of class `SpatVector`.
#'
#' @return The heading between the points, in degrees.
#'
#' @author Eliot McIntire
#' @export
#' @rdname heading
#'
#' @examples
#' library(terra)
#'
#' origDTThreads <- data.table::setDTthreads(2L)
#' origNcpus <- options(Ncpus = 2L)
#'
#' N <- 10L                       # number of agents
#' x1 <- stats::runif(N, -50, 50) # previous X location
#' y1 <- stats::runif(N, -50, 50) # previous Y location
#' x0 <- stats::rnorm(N, x1, 5)   # current X location
#' y0 <- stats::rnorm(N, y1, 5)   # current Y location
#'
#' # using SpatVector
#' prev <- terra::vect(cbind(x = x1, y = y1))
#' curr <- terra::vect(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
#' # using matrix
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' #using both
#' prev <- terra::vect(cbind(x = x1, y = y1))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- terra::vect(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
#' # clean up
#' data.table::setDTthreads(origDTThreads)
#' options(Ncpus = origNcpus)
#'
heading <- function(from, to) {
  from <- coords(from)
  to <- coords(to)
  ys <- to[, 2] - from[, 2]
  xs <- to[, 1] - from[, 1]
  heading <- deg2(atan(xs / ys)) ## 0/0 produces NaN; correct this below
  heading[xs == 0 & ys == 0] <- 0
  ys <- (ys < 0)
  heading[(ys) & (xs) < 0] <- heading[(ys) & (xs) < 0] - 180
  heading[(ys) & (xs) > 0] <- heading[(ys) & (xs) > 0] + 180
  return(heading %% 360)
}

coords <- function(crds) {
  if (is.matrix(crds)) {
    if (isS4(crds)) {
      crds <- crds@.Data[, 1:2, drop = FALSE]
      if (!identical(colnames(crds), xycolNames))
        colnames(crds) <- xycolNames
    } else {
      crds <- crds[, 1:2, drop = FALSE]
    }

  } else if (inherits(crds, "SpatVector")) {
    .requireNamespace("terra")
    crds <- terra::crds(crds)
  } else if (inherits(crds, "sf")) {
    .requireNamespace("sf")
    crds <- sf::st_coordinates(crds)
  } else if (inherits(crds, "Spatial")) {
    .requireNamespace("sp")
    crds <- sp::coordinates(crds)
  }

 crds
}

`coords<-` <- function(obj, value) {
  if (is.matrix(obj)) {
    if (isS4(obj)) {
      obj@.Data[, 1:2] <- value
    } else {
      obj[, 1:2] <- value
    }
  } else if (inherits(obj, "SpatVector")) {
    .requireNamespace("terra")
    crdsdf <- data.frame(value, as.data.frame(obj))
    if (!identical(colnames(crdsdf)[1:2], xycolNames)) {
      colnames(crdsdf)[1:2] <- xycolNames
    }
    obj <- terra::vect(crdsdf, geom = xycolNames)
  } else if (inherits(obj, "sf")) {
    .requireNamespace("sf")
    obj2 <- sf::st_as_sfc(sf::st_as_sf(as.data.frame(value), coords = xycolNames))
    obj <- sf::st_set_geometry(obj, value = obj2)
    obj
  } else if (inherits(obj, "Spatial")) {
    .requireNamespace("sp")
    obj@coords <- value
  }

  obj
}

x1y1colNames <- c("x1", "y1")
xycolNames <- c("x", "y")
