#' @title Spherical Delaunay triangulation
#' @description Computes a spherical Delaunay triangulation.
#'
#' @param vertices vertices, a numeric matrix with three columns
#' @param radius radius of the sphere, a positive number; the vertices will
#'   be projected on this sphere
#' @param center center of the sphere, a numeric vector of length three; the
#'   vertices will be projected on this sphere
#' @param iterations positive integer, the number of iterations used to
#'   construct the meshes of the spherical faces
#'
#' @return A named list with four fields:
#'  \itemize{
#'    \item \code{vertices}, the matrix of vertices obtained by projecting the
#'    original vertices to the sphere;
#'
#'    \item \code{faces}, an integer matrix providing by row the indices of
#'    the faces of the triangulation;
#'
#'    \item \code{solidFaces}, an integer vector providing the indices of the
#'    solid faces; faces are either solid faces or ghost faces, see details
#'
#'    \item \code{meshes}, a list of meshes of the solid faces used for
#'    plotting in \code{\link{plotDelaunayOnSphere}}.
#'  }
#'
#' @export
#'
#' @details See \href{https://doc.cgal.org/latest/Triangulation_on_sphere_2/index.html}{2D Triangulations on the Sphere}.
#'
#' @seealso \code{\link{plotDelaunayOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#'
#' if(require(cooltools)) {
#' vertices <- fibonaccisphere(30L)
#' del <- DelaunayOnSphere(vertices)
#' \donttest{open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)}
#' }
#'
#' if(require(uniformly)) {
#' # sample vertices on a hemisphere, so there will be some ghost faces
#' set.seed(421L)
#' vertices <- rphong_on_hemisphere(6L)
#' del <- DelaunayOnSphere(vertices)
#' # the ghost faces are not plotted
#' \donttest{open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)}
#' }
DelaunayOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5L
) {
  stopifnot(is.matrix(vertices), ncol(vertices) == 3L, is.numeric(vertices))
  storage.mode(vertices) <- "double"
  if(anyNA(vertices)) {
    stop("Found missing values in the `vertices` matrix.")
  }
  if(anyDuplicated(vertices)) {
    stop("Found duplicated rows in the `vertices` matrix.")
  }
  stopifnot(isPositiveNumber(radius))
  stopifnot(isVector3(center))
  stopifnot(isStrictPositiveInteger(iterations))
  del <- delaunay_cpp(
    t(vertices), as.double(radius), as.double(center), as.integer(iterations)
  )
  attr(del, "radius") <- radius
  attr(del, "center") <- center
  del
}

#' @importFrom rgl tmesh3d shade3d
#' @noRd
plotDelaunayFace <- function(mesh, color, ...) {
  rmesh <- tmesh3d(
    vertices = mesh[["vertices"]],
    indices  = mesh[["faces"]],
    normals  = t(mesh[["normals"]])
  )
  shade3d(rmesh, color = color, ...)
}

#' @importFrom rgl arc3d
#' @noRd
plotDelaunayEdges <- function(vertices, radius, center, color, lwd) {
  coords <- rbind(vertices, vertices[1L, ])
  for(i in 1L:3L) {
    arc3d(coords[i, ], coords[i+1L, ], center, radius, n = 50,
          color = color, lwd = lwd, depth_test = "lequal")
  }
}

#' @title Plot spherical Delaunay triangulation
#' @description Plot a spherical Delaunay triangulation.
#'
#' @param del an output of \code{\link{DelaunayOnSphere}}
#' @param colors controls the filling colors of the triangles, either
#'   \code{NA} for no color, or a single color, or \code{"random"} to get
#'   multiple colors with \code{\link[colorsGen]{randomColor}}, or
#'   \code{"distinct"} to get multiple colors with
#'   \code{\link[Polychrome]{createPalette}}
#' @param distinctArgs if \code{colors = "distinct"}, a list of arguments
#'   passed to \code{\link[Polychrome]{createPalette}}
#' @param randomArgs if \code{colors = "random"}, a list of arguments passed
#'   to \code{\link[colorsGen]{randomColor}}
#' @param edges Boolean, whether to plot the edges
#' @param vertices Boolean, whether to plot the vertices
#' @param ecolor a color for the edges
#' @param lwd line width for the edges, if they are plotted
#' @param vcolor a color for the vertices
#' @param vradius a radius for the vertices, which are plotted as spheres (if
#'   they are plotted); \code{NA} for a default value
#' @param ... arguments passed to \code{\link[rgl]{shade3d}} to plot the
#'   spherical triangles
#'
#' @return No value is returned.
#' @export
#' @importFrom rgl spheres3d
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#'
#' vertices <- t(cuboctahedron3d()$vb[-4L, ])
#' del <- DelaunayOnSphere(vertices, radius = sqrt(2))
#'
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)
plotDelaunayOnSphere <- function(
    del, colors = "random",
    distinctArgs = list(seedcolors = c("#ff0000", "#00ff00", "#0000ff")),
    randomArgs = list(hue = "random", luminosity = "bright"),
    edges = FALSE, vertices = FALSE,
    ecolor = "black", lwd = 3,
    vcolor = "black", vradius = NA, ...
) {
  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(vertices))
  radius <- attr(del, "radius")
  center <- attr(del, "center")
  Vertices   <- del[["vertices"]]
  Meshes     <- del[["meshes"]]
  solidFaces <- del[["solidFaces"]]
  Faces      <- del[["faces"]][solidFaces, ]
  if(isString(colors)) {
    if(colors == "random") {
      colors <- rcolors(length(Meshes), randomArgs)
    } else if(colors == "distinct") {
      colors <- distinctColors(length(Meshes), distinctArgs)
    } else{
      colors <- rep(colors, length(Meshes))
    }
  } else if(all(is.na(colors)) || is.null(colors)) {
    colors <- rep(NA, length(Meshes))
  } else if(!isStringVector(colors)) {
    stop("Invalid `colors` argument.")
  }
  for(i in seq_along(Meshes)) {
    plotDelaunayFace(
      Meshes[[i]], colors[i], ...
    )
    if(edges) {
      face <- Faces[i, ]
      verts <- Vertices[face, ]
      plotDelaunayEdges(verts, radius, center, ecolor, lwd)
    }
  }
  if(vertices) {
    if(is.na(vradius)) {
      vradius <- radius / 50
    } else {
      stopifnot(isPositiveNumber(vradius))
    }
    spheres3d(Vertices, radius = vradius, color = vcolor)
  }
}
