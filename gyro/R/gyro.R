#' @title Point on a gyroline
#' @description Point of coordinate \code{t} on the gyroline passing through
#'   two given points \code{A} and \code{B}. This is \code{A} for \code{t=0}
#'   and this is \code{B} for \code{t=1}. For \code{t=1/2} this is the
#'   gyromidpoint of the gyrosegment joining \code{A} and \code{B}.
#'
#' @encoding UTF-8
#'
#' @param A,B two distinct points
#' @param t a number
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#'
#' @return A point.
#' @export
gyroABt <- function(A, B, t, s = 1, model = "U"){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPositiveNumber(s))
  stopifnot(isPoint(A))
  stopifnot(isPoint(B))
  stopifnot(isNumber(t))
  stopifnot(areDistinct(A, B))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    MgyroABt(A, B, t, s)
  }else{
    UgyroABt(A, B, t, s)
  }
}

#' @title Isomorphism from Möbius gyrovector space to Ungar gyrovector space
#' @description Isomorphism from the Möbius gyrovector space to
#'   the Ungar gyrovector space.
#'
#' @encoding UTF-8
#'
#' @param A a point whose norm is lower than \code{s}
#' @param s positive number, the radius of the Poincaré ball
#'
#' @return The point of the Ungar gyrovector space corresponding to \code{A}
#'   by isomorphism.
#' @export
PhiUM <- function(A, s = 1){
  stopifnot(isPoint(A))
  stopifnot(isPositiveNumber(s))
  if(dotprod(A) >= s*s){
    stop(
      "In the M\u00f6bius gyrovector space, points must be ",
      "strictly inside the centered ball of radius `s`.",
      call. = TRUE
    )
  }
  .PhiUM(A, s)
}

#' @title Isomorphism from Ungar gyrovector space to Möbius gyrovector space
#' @description Isomorphism from the Ungar gyrovector space to
#'   the Möbius gyrovector space.
#'
#' @encoding UTF-8
#'
#' @param A a point in the Ungar vector space with curvature \code{s}
#' @param s a positive number, the hyperbolic curvature of the Ungar
#'   vector space
#'
#' @return The point of the Poincaré ball of radius \code{s} corresponding
#'   to \code{A} by isomorphism.
#' @export
PhiMU <- function(A, s = 1){
  stopifnot(isPoint(A))
  stopifnot(isPositiveNumber(s))
  PhiME(PhiEU(A, s), s)
}

#' @title Gyromidpoint
#' @description The gyromidpoint of a \code{\link{gyrosegment}}.
#'
#' @encoding UTF-8
#'
#' @param A,B two distinct points (of the same dimension)
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#'
#' @return A point, the gyromidpoint of a the \code{\link{gyrosegment}}
#'   joining \code{A} and \code{B}.
#' @export
#' @note This is the same as \code{gyroABt(A, B, 1/2, s)} but the
#'   calculation is more efficient.
gyromidpoint <- function(A, B, s = 1, model = "U"){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPoint(A))
  stopifnot(isPoint(B))
  stopifnot(length(A) == length(B))
  stopifnot(isPositiveNumber(s))
  stopifnot(areDistinct(A, B))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    Mgyromidpoint(A, B, s)
  }else{
    Ugyromidpoint(A, B, s)
  }
}

#' @title Gyrosegment
#' @description Gyrosegment joining two given points.
#'
#' @encoding UTF-8
#'
#' @param A,B two distinct points (of the same dimension)
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param n number of points forming the gyrosegment from \code{A} to \code{B}
#'
#' @return A numeric matrix with \code{n} rows. Each row is a point of the
#'   gyrosegment from \code{A} (the first row) to \code{B} (the last row).
#' @export
#'
#' @note The gyrosegment is obtained from \code{\link{gyroABt}} by varying
#' \code{t} from \code{0} to \code{1}.
#'
#' @examples
#' library(gyro)
#' # a 2D example ####
#' A <- c(1, 2); B <- c(1, 1)
#' opar <- par(mfrow = c(1, 2), mar = c(2, 2, 2, 0.5))
#' plot(rbind(A, B), type = "p", pch = 19, xlab = NA, ylab = NA,
#'      xlim = c(0, 2), ylim = c(0, 2), main = "s = 0.2")
#' s <- 0.2
#' AB <- gyrosegment(A, B, s)
#' lines(AB, col = "blue", lwd = 2)
#' text(t(A), expression(italic(A)), pos = 2)
#' text(t(B), expression(italic(B)), pos = 3)
#' # this is an hyperbola whose asymptotes meet at the origin
#' # approximate asymptotes
#' lines(rbind(c(0, 0), gyroABt(A, B, t = -20, s)), lty = "dashed")
#' lines(rbind(c(0, 0), gyroABt(A, B, t = 20, s)), lty = "dashed")
#' # plot the gyromidoint
#' points(
#'  rbind(gyromidpoint(A, B, s)),
#'  type = "p", pch = 19, col = "red"
#' )
#' # another one, with a different `s`
#' plot(rbind(A, B), type = "p", pch = 19, xlab = NA, ylab = NA,
#'      xlim = c(0, 2), ylim = c(0, 2), main = "s = 0.1")
#' s <- 0.1
#' AB <- gyrosegment(A, B, s)
#' lines(AB, col = "blue", lwd = 2)
#' text(t(A), expression(italic(A)), pos = 2)
#' text(t(B), expression(italic(B)), pos = 3)
#' # approximate asymptotes
#' lines(rbind(c(0, 0), gyroABt(A, B, t = -20, s)), lty = "dashed")
#' lines(rbind(c(0, 0), gyroABt(A, B, t = 20, s)), lty = "dashed")
#' # plot the gyromidoint
#' points(
#'  rbind(gyromidpoint(A, B, s)),
#'  type = "p", pch = 19, col = "red"
#' )
#'
#' # a 3D hyperbolic triangle ####
#' library(rgl)
#' A <- c(1, 0, 0); B <- c(0, 1, 0); C <- c(0, 0, 1)
#' s <- 0.3
#' AB <- gyrosegment(A, B, s)
#' AC <- gyrosegment(A, C, s)
#' BC <- gyrosegment(B, C, s)
#' view3d(30, 30, zoom = 0.75)
#' lines3d(AB, lwd = 3); lines3d(AC, lwd = 3); lines3d(BC, lwd = 3)
gyrosegment <- function(A, B, s = 1, model = "U", n = 100){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPoint(A))
  stopifnot(isPoint(B))
  stopifnot(length(A) == length(B))
  stopifnot(isPositiveNumber(s))
  stopifnot(isPositiveInteger(n))
  stopifnot(n >= 2)
  stopifnot(areDistinct(A, B))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    Mgyrosegment(A, B, s, n)
  } else {
    Ugyrosegment(A, B, s, n)
  }
}

#' @title Gyroray
#' @description Gyroray given an origin and a point.
#'
#' @encoding UTF-8
#'
#' @param O,A two distinct points (of the same dimension); the point
#'   \code{O} is the origin of the gyroray
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature
#' @param tmax positive number controlling the length of the gyroray
#' @param OtoA Boolean, whether the gyroray must be directed from
#'   \code{O} to \code{A} or must be the opposite one
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param n number of points forming the gyroray
#'
#' @return A numeric matrix with \code{n} rows. Each row is a point of the
#'   gyroray with origin \code{O} (the first row) and passing through \code{A}
#'   or not, according to \code{OtoA}.
#' @export
#' @examples
#' library(gyro)
#' # a 2D example ####
#' O <- c(1, 2); A <- c(1, 1)
#' opar <- par(mar = c(2, 2, 2, 0.5))
#' plot(rbind(O, A), type = "p", pch = 19, xlab = NA, ylab = NA,
#'      xlim = c(0, 2), ylim = c(0, 3), main = "s = 0.3")
#' s <- 0.3
#' ray <- gyroray(O, A, s)
#' lines(ray, col = "blue", lwd = 2)
#' text(t(O), expression(italic(O)), pos = 2)
#' text(t(A), expression(italic(A)), pos = 3)
#' # opposite gyroray
#' yar <- gyroray(O, A, s, OtoA = FALSE)
#' lines(yar, col = "red", lwd = 2)
#' par(opar)
gyroray <- function(O, A, s = 1, tmax = 20, OtoA = TRUE, model = "U", n = 300) {
  stopifnot(isPoint(O))
  stopifnot(isPoint(A))
  stopifnot(length(O) == length(A))
  stopifnot(isPositiveNumber(s))
  stopifnot(isPositiveNumber(tmax))
  stopifnot(isBoolean(OtoA))
  stopifnot(isPositiveInteger(n))
  stopifnot(n >= 2)
  stopifnot(areDistinct(O, A))
  if(OtoA) {
    t_ <- seq(0, tmax, length.out = n)
  } else {
    t_ <- seq(-tmax, 0, length.out = n)
  }
  model <- match.arg(model, c("M", "U"))
  if(model == "M") {
    s2 <- s * s
    if(dotprod(O) >= s2 || dotprod(A) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    t(vapply(t_, function(t) {MgyroABt(O, A, t, s)}, numeric(length(A))))
  } else {
    t(vapply(t_, function(t) {UgyroABt(O, A, t, s)}, numeric(length(A))))
  }
}

#' @title Gyrotube (tubular gyrosegment)
#' @description Tubular gyrosegment joining two given 3D points.
#'
#' @encoding UTF-8
#'
#' @param A,B distinct 3D points
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature (higher value, less curved)
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param n number of points forming the gyrosegment
#' @param radius radius of the tube around the gyrosegment
#' @param sides number of sides in the polygon cross section
#' @param caps Boolean, whether to put caps on the ends of the tube
#'
#' @return A \code{\link[rgl]{mesh3d}} object.
#' @export
#'
#' @importFrom rgl cylinder3d
#'
#' @examples library(gyro)
#' library(rgl)
#' A <- c(1, 2, 0); B <- c(1, 1, 0)
#' tube <- gyrotube(A, B, s = 0.2, radius = 0.02)
#' shade3d(tube, color = "orangered")
#'
#' # a 3D hyperbolic triangle ####
#' library(rgl)
#' A <- c(1, 0, 0); B <- c(0, 1, 0); C <- c(0, 0, 1)
#' s <- 0.3
#' r <- 0.03
#' AB <- gyrotube(A, B, s, radius = r)
#' AC <- gyrotube(A, C, s, radius = r)
#' BC <- gyrotube(B, C, s, radius = r)
#' view3d(30, 30, zoom = 0.75)
#' shade3d(AB, color = "gold")
#' shade3d(AC, color = "gold")
#' shade3d(BC, color = "gold")
#' spheres3d(rbind(A, B, C), radius = 0.04, color = "gold")
gyrotube <- function(
    A, B, s = 1, model = "U", n = 100, radius, sides = 90, caps = FALSE
){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPositiveNumber(s))
  stopifnot(is3dPoint(A))
  stopifnot(is3dPoint(B))
  stopifnot(isPositiveInteger(n))
  stopifnot(isPositiveInteger(sides))
  stopifnot(isBoolean(caps))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    points <- Mgyrosegment(A, B, s, n)
  }else{
    points <- Ugyrosegment(A, B, s, n)
  }
  closed <- ifelse(caps, -2, 0)
  cylinder3d(points, radius = radius, sides = sides, closed = closed)
}

#' @title Gyrocentroid
#' @description Gyrocenroid of a triangle.
#'
#' @encoding UTF-8
#'
#' @param A,B,C three distinct points
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature (the smaller, the more curved)
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#'
#' @return A point, the gyrocentroid of the triangle \code{ABC}.
#' @export
gyrocentroid <- function(A, B, C, s = 1, model = "U"){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPositiveNumber(s))
  stopifnot(isPoint(A))
  stopifnot(isPoint(B))
  stopifnot(isPoint(C))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2 || dotprod(C) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    Mgyrocentroid(A, B, C, s)
  }else{
    Ugyrocentroid(A, B, C, s)
  }
}

#' @title Gyrotriangle in 3D space
#' @description 3D gyrotriangle as a mesh.
#'
#' @encoding UTF-8
#'
#' @param A,B,C three distinct 3D points
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature (the smaller, the more curved)
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param iterations the gyrotriangle is constructed by iterated subdivisions,
#'   this argument is the number of iterations
#' @param palette a vector of colors to decorate the triangle, or \code{NULL}
#'   if you don't want to use a color palette
#' @param bias,interpolate if \code{palette} is not \code{NULL}, these
#'   arguments are passed to \code{\link[grDevices]{colorRamp}}
#' @param g a function from [0,1] to [0,1]; if \code{palette} is not
#'   \code{NULL}, this function is applied to the scalars defining the colors
#'   (the normalized gyrodistances to the gyrocentroid of the gyrotriangle)
#'
#' @return A \code{\link[rgl]{mesh3d}} object.
#' @export
#'
#' @importFrom purrr flatten
#' @importFrom rgl tmesh3d
#' @importFrom Rvcg vcgClean
#' @importFrom grDevices colorRamp rgb
#'
#' @examples library(gyro)
#' library(rgl)
#' A <- c(1, 0, 0); B <- c(0, 1, 0); C <- c(0, 0, 1)
#' ABC <- gyrotriangle(A, B, C, s = 0.3)
#' \donttest{open3d(windowRect = c(50, 50, 562, 562))
#' view3d(30, 30, zoom = 0.75)
#' shade3d(ABC, color = "navy", specular = "cyan")}
#'
#' # using a color palette ####
#' if(require("trekcolors")) {
#'   pal <- trek_pal("klingon")
#' } else {
#'   pal <- hcl.colors(32L, palette = "Rocket")
#' }
#' ABC <- gyrotriangle(
#'   A, B, C, s = 0.5,
#'   palette = pal, bias = 1.5, interpolate = "spline"
#' )
#' \donttest{open3d(windowRect = c(50, 50, 562, 562))
#' view3d(zoom = 0.75)
#' shade3d(ABC)}
#'
#' # hyperbolic icosahedron ####
#' library(rgl)
#' library(Rvcg) # to get the edges with the `vcgGetEdge` function
#' icosahedron <- icosahedron3d() # mesh with 12 vertices, 20 triangles
#' vertices <- t(icosahedron$vb[-4, ])
#' triangles <- t(icosahedron$it)
#' edges <- as.matrix(vcgGetEdge(icosahedron)[, c("vert1", "vert2")])
#' s <- 0.3
#' \donttest{open3d(windowRect = c(50, 50, 562, 562))
#' view3d(zoom = 0.75)
#' for(i in 1:nrow(triangles)){
#'   triangle <- triangles[i, ]
#'   A <- vertices[triangle[1], ]
#'   B <- vertices[triangle[2], ]
#'   C <- vertices[triangle[3], ]
#'   gtriangle <- gyrotriangle(A, B, C, s)
#'   shade3d(gtriangle, color = "midnightblue")
#' }
#' for(i in 1:nrow(edges)){
#'   edge <- edges[i, ]
#'   A <- vertices[edge[1], ]
#'   B <- vertices[edge[2], ]
#'   gtube <- gyrotube(A, B, s, radius = 0.03)
#'   shade3d(gtube, color = "lemonchiffon")
#' }
#' spheres3d(vertices, radius = 0.05, color = "lemonchiffon")}
gyrotriangle <- function(
    A, B, C, s = 1, model = "U", iterations = 5,
    palette = NULL, bias = 1, interpolate = "linear", g = identity
){
  model <- match.arg(model, c("M", "U"))
  stopifnot(isPositiveNumber(s))
  stopifnot(is3dPoint(A))
  stopifnot(is3dPoint(B))
  stopifnot(is3dPoint(C))
  if(model == "M"){
    s2 <- s * s
    if(dotprod(A) >= s2 || dotprod(B) >= s2 || dotprod(C) >= s2){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
    subd <- Mgyrosubdiv(A, B, C, s)
    for(i in seq_len(iterations-1)){
      subd <- flatten(lapply(subd, function(triplet){
        Mgyrosubdiv(triplet[[1L]], triplet[[2L]], triplet[[3L]], s)
      }))
    }
  }else{
    subd <- Ugyrosubdiv(A, B, C, s)
    for(i in seq_len(iterations-1)){
      subd <- flatten(lapply(subd, function(triplet){
        Ugyrosubdiv(triplet[[1L]], triplet[[2L]], triplet[[3L]], s)
      }))
    }
  }
  vertices <-
    do.call(cbind, lapply(subd, function(triplet) do.call(cbind, triplet)))
  indices <- matrix(1L:ncol(vertices), nrow = 3L)
  mesh0 <- tmesh3d(
    vertices = vertices,
    indices = indices
  )
  mesh <- vcgClean(mesh0, sel = c(0, 7), silent = TRUE)
  mesh[["remvert"]] <- NULL
  mesh[["remface"]] <- NULL
  if(!is.null(palette)){
    fpalette <- colorRamp(palette, bias = bias, interpolate = interpolate)
    if(model == "M"){
      gyroG <- Mgyrocentroid(A, B, C, s)
      dists <- sqrt(apply(mesh$vb[-4L, ], 2L, function(v){
        dotprod(Mgyroadd(-gyroG, v, s))
      }))
    }else{
      gyroG <- Ugyrocentroid(A, B, C, s)
      dists <- sqrt(apply(mesh$vb[-4L, ], 2L, function(v){
        dotprod(Ugyroadd(-gyroG, v, s))
      }))
    }
    dists <- (dists - min(dists))/diff(range(dists))
    RGB <- fpalette(g(dists))
    colors <- rgb(RGB[, 1L], RGB[, 2L], RGB[, 3L], maxColorValue = 255)
    mesh[["material"]] = list(color = colors)
  }
  mesh
}
