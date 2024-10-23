#' @title Spherical Voronoï tessellation
#' @description Computes a spherical Voronoï tessellation.
#'
#' @param vertices vertices, a numeric matrix with three columns
#' @param radius radius of the sphere, a positive number; the vertices will
#'   be projected on this sphere
#' @param center center of the sphere, a numeric vector of length three; the
#'   vertices will be projected on this sphere
#' @param iterations positive integer, the number of iterations used to
#'   construct the meshes of the spherical faces
#'
#' @return An unnamed list whose each element corresponds to a Voronoï face and
#'   is a named list with three fields:
#'   \itemize{
#'     \item \code{site}, the coordinates of the Voronoï site of the face;
#'
#'     \item \code{cell}, a numeric matrix providing the coordinates of the
#'     vertices of the face;
#'
#'     \item \code{mesh}, a mesh of the face used for plotting in the function
#'     \code{\link{plotVoronoiOnSphere}}.
#'   }
#'
#' @export
#'
#' @details First the Delaunay triangulation is computed, then the Voronoï
#'   tessellation is obtained by duality.
#'
#' @seealso \code{\link{plotVoronoiOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#' if(require(cooltools)) {
#' vertices <- fibonaccisphere(150L)
#' vor <- VoronoiOnSphere(vertices)
#' \donttest{open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotVoronoiOnSphere(vor, colors = "random")}
#' }
VoronoiOnSphere <- function(
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
  vor <- voronoi_cpp(
    t(vertices), as.double(radius), as.double(center), as.integer(iterations)
  )
  attr(vor, "radius") <- radius
  attr(vor, "center") <- center
  vor
}


geodist <- function(A, B, radius, center) {
  radius * acos(sum((A-center)*(B-center)) / (radius*radius))
}

#' @importFrom grDevices colorRamp rgb
#' @importFrom rgl tmesh3d shade3d
#' @noRd
plotVoronoiCell <- function(
    site, cell, mesh, radius, center, palette, bias, color, ...
) {

  if(is.na(color)) {
    dists <- apply(cell, 2L, function(xyz) {
      geodist(xyz, site, radius, center)
    })
    maxDist <- max(dists)
    fcol <- colorRamp(palette, bias = bias, interpolate = "spline")
    clr <- function(xyz) {
      RGB <- fcol(min(1, geodist(xyz, site, radius, center) / maxDist))
      rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
    }
    colors <- c(apply(mesh[["vertices"]], 2L, clr))
    rmesh <- tmesh3d(
      vertices = mesh[["vertices"]],
      indices  = mesh[["faces"]],
      normals  = t(mesh[["normals"]]),
      material = list(color = colors)
    )
    shade3d(rmesh, meshColor = "vertices", ...)
  } else {
    rmesh <- tmesh3d(
      vertices = mesh[["vertices"]],
      indices  = mesh[["faces"]],
      normals  = t(mesh[["normals"]])
    )
    shade3d(rmesh, color = color, ...)
  }

}

#' @importFrom rgl arc3d
#' @noRd
plotVoronoiEdges <- function(cell, radius, center, color, lwd) {
  cellsize <- ncol(cell)
  cell <- cbind(cell, cell[, 1L])
  for(i in 1L:cellsize) {
    arc3d(cell[, i], cell[, i+1L], center, radius, n = 50,
          color = color, lwd = lwd, depth_test = "lequal")
  }
}

#' @title Plot spherical Voronoï tessellation
#' @description Plot a spherical Voronoï tessellation.
#'
#' @param vor an output of \code{\link{VoronoiOnSphere}}
#' @param colors controls the filling colors of the triangles, either
#'   \code{NA} for no color, or a single color, or \code{"random"} to get
#'   multiple colors with \code{\link[colorsGen]{randomColor}}, or
#'   \code{"distinct"} to get multiple colors with
#'   \code{\link[Polychrome]{createPalette}}, or \code{"gradient"}
#' @param distinctArgs if \code{colors = "distinct"}, a list of arguments
#'   passed to \code{\link[Polychrome]{createPalette}}
#' @param randomArgs if \code{colors = "random"}, a list of arguments passed
#'   to \code{\link[colorsGen]{randomColor}}
#' @param palette this argument is used only when \code{colors="gradient"}; it
#'   can be either a character vector of colors, or the name of a palette
#'   which will be passed to the \code{palette} argument of the function
#'   \code{\link[grDevices]{hcl.colors}}
#' @param bias this argument is used only when \code{colors="gradient"}; it
#'   is passed to the \code{bias} argument of the function
#'   \code{\link[grDevices]{colorRamp}}
#' @param edges Boolean, whether to plot the edges
#' @param sites Boolean, whether to plot the Voronoï sites
#' @param ecolor a color for the edges
#' @param lwd graphical parameter for the edges, if they are plotted
#' @param scolor a color for the sites
#' @param sradius a radius for the sites, which are plotted as spheres (if
#'   they are plotted); \code{NA} for a default value
#' @param ... arguments passed to \code{\link[rgl]{shade3d}} to plot the
#'   spherical faces
#'
#' @return No value is returned.
#'
#' @importFrom rgl spheres3d
#' @importFrom grDevices hcl.colors
#'
#' @export
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#' # take the vertices of the cuboctahedron and Voronoïze
#' vertices <- t(cuboctahedron3d()$vb[-4L, ])
#' vor <- VoronoiOnSphere(vertices)
#' # plot
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotVoronoiOnSphere(vor, specular = "black", edges = TRUE)
#'
#' # effect of the `bias` argument ###
#' \donttest{library(sphereTessellation)
#' library(rgl)
#' vertices <- t(cuboctahedron3d()$vb[-4L, ])
#' vor <- VoronoiOnSphere(vertices)
#' open3d(windowRect = 50 + c(0, 0, 900, 300), zoom = 0.8)
#' mfrow3d(1, 3)
#' plotVoronoiOnSphere(vor, palette = "Viridis", bias = 0.5)
#' next3d()
#' plotVoronoiOnSphere(vor, palette = "Viridis", bias = 0.8)
#' next3d()
#' plotVoronoiOnSphere(vor, palette = "Viridis", bias = 1.1)}
plotVoronoiOnSphere <- function(
    vor, colors = "gradient",
    distinctArgs = list(seedcolors = c("#ff0000", "#00ff00", "#0000ff")),
    randomArgs = list(hue = "random", luminosity = "bright"),
    palette = "Rocket", bias = 1,
    edges = FALSE, sites = FALSE,
    ecolor = "black", lwd = 3,
    scolor = "black", sradius = NA, ...
) {
  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(sites))
  radius <- attr(vor, "radius")
  center <- attr(vor, "center")
  if(isString(colors)) {
    if(colors == "gradient") {
      colors <- rep(NA, length(vor))
      if(isString(palette)) {
        palette <- hcl.colors(255L, palette = palette)
      }
    } else if(colors == "random") {
      colors <- rcolors(length(vor), randomArgs)
    } else if(colors == "distinct") {
      colors <- distinctColors(length(vor), distinctArgs)
    } else {
      colors <- rep(colors, length(vor))
    }
  } else if(all(is.na(colors)) || is.null(colors)) {
    colors <- NULL
  } else if(!isStringVector(colors)) {
    stop("Invalid `colors` argument.")
  }
  for(i in seq_along(vor)) {
    vor_i <- vor[[i]]
    if(!is.null(colors)) {
      plotVoronoiCell(
        vor_i[["site"]], vor_i[["cell"]], vor_i[["mesh"]],
        radius, center, palette, bias, colors[i], ...
      )
    }
    if(edges) {
      plotVoronoiEdges(vor_i[["cell"]], radius, center, ecolor, lwd)
    }
    if(sites) {
      if(is.na(sradius)) {
        sradius <- radius / 50
      } else {
        stopifnot(isPositiveNumber(sradius))
      }
      spheres3d(rbind(vor_i[["site"]]), radius = sradius, color = scolor)
    }
  }
}
