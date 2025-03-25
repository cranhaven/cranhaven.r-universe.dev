
#' @importFrom cxhull cxhull VerticesXYZ dihedralAngles TrianglesXYZ
#' @noRd
.cxhull <- function(points){
  hull <- cxhull(points, triangulate = TRUE)
  Vertices <- VerticesXYZ(hull)
  edges <- dihedralAngles(hull)
  Edges <- as.matrix(subset(edges, angle < 179)[, c("i1", "i2")])
  Triangles <- TrianglesXYZ(hull)
  ntriangles <- length(hull[["facets"]])
  Triangles <-
    lapply(split(as.data.frame(Triangles), gl(ntriangles, 3L)), as.matrix)
  return(list(vertices = Vertices, edges = Edges, triangles = Triangles))
}

#' @title Plot hyperbolic convex hull
#' @description Plot the hyperbolic convex hull of a set of 3D points.
#'
#' @encoding UTF-8
#'
#' @param points matrix of 3D points, one point per row
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature (the smaller, the more curved)
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param iterations argument passed to \code{\link{gyrotriangle}}
#' @param n argument passed to \code{\link{gyrotube}} or
#'   \code{\link{gyrosegment}}, the number of points for each edge
#' @param edgesAsTubes Boolean, whether to represent tubular edges
#' @param verticesAsSpheres Boolean, whether to represent the vertices as
#'   spheres
#' @param edgesColor a color for the edges
#' @param spheresColor a color for the spheres, if
#'   \code{verticesAsSpheres = TRUE}
#' @param tubesRadius radius of the tubes, if \code{edgesAsTubes = TRUE}
#' @param spheresRadius radius of the spheres,
#'   if \code{verticesAsSpheres = TRUE}
#' @param facesColor this argument sets the color of the faces; it can be
#'   either a single color or a color palette, i.e. a vector of colors; if it
#'   is a color palette, it will be passed to the argument \code{palette} of
#'   \code{\link{gyrotriangle}}
#' @param bias,interpolate,g these arguments are passed to
#'   \code{\link{gyrotriangle}} in the case when \code{facesColor} is a color
#'   palette
#'
#' @return No value, called for plotting.
#' @export
#'
#' @importFrom rgl shade3d spheres3d lines3d
#' @importFrom Morpho mergeMeshes
#' @importFrom Rvcg vcgClean
#'
#' @examples library(gyro)
#' library(rgl)
#' # Triangular orthobicopula ####
#' points <- rbind(
#'   c(1, -1/sqrt(3), sqrt(8/3)),
#'   c(1, -1/sqrt(3), -sqrt(8/3)),
#'   c(-1, -1/sqrt(3), sqrt(8/3)),
#'   c(-1, -1/sqrt(3), -sqrt(8/3)),
#'   c(0, 2/sqrt(3), sqrt(8/3)),
#'   c(0, 2/sqrt(3), -sqrt(8/3)),
#'   c(1, sqrt(3), 0),
#'   c(1, -sqrt(3), 0),
#'   c(-1, sqrt(3), 0),
#'   c(-1, -sqrt(3), 0),
#'   c(2, 0, 0),
#'   c(-2, 0, 0)
#' )
#' \donttest{open3d(windowRect = c(50, 50, 562, 562))
#' view3d(zoom = 0.7)
#' plotGyrohull3d(points, s = 0.4)}
#'
#' # a non-convex polyhedron with triangular faces ####
#' vertices <- rbind(
#'   c(-2.1806973249, -2.1806973249, -2.1806973249),
#'   c(-3.5617820682, 0.00000000000, 0.00000000000),
#'   c(0.00000000000, -3.5617820682, 0.00000000000),
#'   c(0.00000000000, 0.00000000000, -3.5617820682),
#'   c(-2.1806973249, -2.1806973249, 2.18069732490),
#'   c(0.00000000000, 0.00000000000, 3.56178206820),
#'   c(-2.1806973249, 2.18069732490, -2.1806973249),
#'   c(0.00000000000, 3.56178206820, 0.00000000000),
#'   c(-2.1806973249, 2.18069732490, 2.18069732490),
#'   c(2.18069732490, -2.1806973249, -2.1806973249),
#'   c(3.56178206820, 0.00000000000, 0.00000000000),
#'   c(2.18069732490, -2.1806973249, 2.18069732490),
#'   c(2.18069732490, 2.18069732490, -2.1806973249),
#'   c(2.18069732490, 2.18069732490, 2.18069732490))
#' triangles <- 1 + rbind(
#'   c(3, 2, 0),
#'   c(0, 1, 3),
#'   c(2, 1, 0),
#'   c(4, 2, 5),
#'   c(5, 1, 4),
#'   c(4, 1, 2),
#'   c(6, 7, 3),
#'   c(3, 1, 6),
#'   c(6, 1, 7),
#'   c(5, 7, 8),
#'   c(8, 1, 5),
#'   c(7, 1, 8),
#'   c(9, 2, 3),
#'   c(3, 10, 9),
#'   c(9, 10, 2),
#'   c(5, 2, 11),
#'   c(11, 10, 5),
#'   c(2, 10, 11),
#'   c(3, 7, 12),
#'   c(12, 10, 3),
#'   c(7, 10, 12),
#'   c(13, 7, 5),
#'   c(5, 10, 13),
#'   c(13, 10, 7))
#' edges0 <- do.call(c, lapply(1:nrow(triangles), function(i){
#'   face <- triangles[i, ]
#'   list(
#'     sort(c(face[1], face[2])),
#'     sort(c(face[1], face[3])),
#'     sort(c(face[2], face[3]))
#'   )
#' }))
#' edges <- do.call(rbind, edges0)
#' edges <- edges[!duplicated(edges), ]
#' s <- 2
#' \donttest{library(rgl)
#' open3d(windowRect = c(50, 50, 1074, 562))
#' mfrow3d(1, 2)
#' view3d(zoom = 0.65)
#' for(i in 1:nrow(triangles)){
#'   triangle <- triangles[i, ]
#'   A <- vertices[triangle[1], ]
#'   B <- vertices[triangle[2], ]
#'   C <- vertices[triangle[3], ]
#'   gtriangle <- gyrotriangle(A, B, C, s)
#'   shade3d(gtriangle, color = "violetred")
#' }
#' for(i in 1:nrow(edges)){
#'   edge <- edges[i, ]
#'   A <- vertices[edge[1], ]
#'   B <- vertices[edge[2], ]
#'   gtube <- gyrotube(A, B, s, radius = 0.06)
#'   shade3d(gtube, color = "darkviolet")
#' }
#' spheres3d(vertices, radius = 0.09, color = "deeppink")
#' # now plot the hyperbolic convex hull
#' next3d()
#' view3d(zoom = 0.65)
#' plotGyrohull3d(vertices, s)}
#'
#' # an example of color palette ####
#' if(require("trekcolors")) {
#'   pal <- trek_pal("lcars_series")
#' } else {
#'   pal <- hcl.colors(32L, palette = "Rocket")
#' }
#' set.seed(666) # 50 random points on sphere
#' if(require("uniformly")) {
#'   points <- runif_on_sphere(50L, d = 3L)
#' } else {
#'   points <- matrix(rnorm(50L * 3L), nrow = 50L, ncol = 3L)
#'   points <- points / sqrt(apply(points, 1L, crossprod))
#' }
#' \donttest{open3d(windowRect = c(50, 50, 562, 562))
#' plotGyrohull3d(
#'   points, edgesColor = "brown",
#'   facesColor = pal, g = function(u) 1-u^2
#' )}
plotGyrohull3d <- function(
    points, s = 1, model = "U", iterations = 5, n = 100, edgesAsTubes = TRUE,
    verticesAsSpheres = edgesAsTubes, edgesColor = "yellow",
    spheresColor = edgesColor, tubesRadius = 0.03, spheresRadius = 0.05,
    facesColor = "navy", bias = 1, interpolate = "linear", g = identity
){
  model <- match.arg(model, c("M", "U"))
  if(model == "M"){
    snorms <- apply(points, 1L, dotprod)
    if(any(snorms >= s)){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
  }
  stopifnot(isBoolean(edgesAsTubes))
  stopifnot(isBoolean(verticesAsSpheres))
  hull <- .cxhull(points)
  Triangles <- hull[["triangles"]]
  Edges     <- hull[["edges"]]
  Vertices  <- hull[["vertices"]]
  ntriangles <- length(Triangles)
  Gtriangles <- vector("list", ntriangles)
  palette <- if(length(facesColor) > 1L) facesColor
  for(i in 1L:ntriangles){
    triangle <- Triangles[[i]]
    Gtriangles[[i]] <- gyrotriangle(
      triangle[1L, ], triangle[2L, ], triangle[3L, ],
      s = s, model = model, iterations = iterations, palette = palette,
      bias = bias, interpolate = interpolate, g = g
    )
  }
  mesh <- vcgClean(mergeMeshes(Gtriangles), sel = 0, silent = TRUE)
  if(is.null(palette)){
    shade3d(mesh, color = facesColor)
  }else{
    shade3d(mesh)
  }
  if(edgesAsTubes){
    for(i in 1L:nrow(Edges)){
      edge <- Edges[i, ]
      gtube <- gyrotube(
        Vertices[edge[1L], ], Vertices[edge[2L], ], s = s, model = model,
        n = n, radius = tubesRadius
      )
      shade3d(gtube, color = edgesColor)
    }
  }else{
    if(model == "M"){
      for(i in 1L:nrow(Edges)){
        edge <- Edges[i, ]
        gsegment <- Mgyrosegment(
          Vertices[edge[1L], ], Vertices[edge[2L], ], s = s, n = n
        )
        lines3d(gsegment, color = edgesColor, lwd = 2)
      }
    }else{
      for(i in 1L:nrow(Edges)){
        edge <- Edges[i, ]
        gsegment <- Ugyrosegment(
          Vertices[edge[1L], ], Vertices[edge[2L], ], s = s, n = n
        )
        lines3d(gsegment, color = edgesColor, lwd = 2)
      }
    }
  }
  if(verticesAsSpheres){
    spheres3d(Vertices, radius = spheresRadius, color = spheresColor)
  }
  invisible(NULL)
}


#' @title Plot hyperbolic mesh
#' @description Plot the hyperbolic version of a triangle 3D mesh.
#'
#' @encoding UTF-8
#'
#' @param mesh there are two possibilities for this argument; it can be a
#'   triangle \strong{rgl} mesh (class \code{mesh3d}) or a list with (at least)
#'   two fields: \code{vertices}, a numeric matrix with three columns, and
#'   \code{faces}, an integer matrix with three columns
#' @param s positive number, the radius of the Poincaré ball if
#'   \code{model="M"}, otherwise, if \code{model="U"}, this number
#'   defines the hyperbolic curvature (the smaller, the more curved)
#' @param model the hyperbolic model, either \code{"M"} (Möbius model, i.e.
#'   Poincaré model) or \code{"U"} (Ungar model, i.e. hyperboloid model)
#' @param iterations argument passed to \code{\link{gyrotriangle}}
#' @param n argument passed to \code{\link{gyrotube}} or
#'   \code{\link{gyrosegment}}, the number of points for each edge
#' @param edges Boolean, whether to plot the edges (as tubes or as lines)
#' @param edgesAsTubes Boolean, whether to plot tubular edges; if \code{FALSE},
#'   the edges are plotted as lines
#' @param edgesColor a color for the edges
#' @param tubesRadius radius of the tubes, if \code{edgesAsTubes = TRUE}
#' @param verticesAsSpheres Boolean, whether to plot the vertices as
#'   spheres; if \code{FALSE}, the vertices are not plotted
#' @param spheresColor a color for the spheres, if
#'   \code{verticesAsSpheres = TRUE}
#' @param spheresRadius radius of the spheres,
#'   if \code{verticesAsSpheres = TRUE}
#' @param facesColor this argument sets the color of the faces; it can be
#'   either a single color or a color palette, i.e. a vector of colors; if it
#'   is a color palette, it will be passed to the argument \code{palette} of
#'   \code{\link{gyrotriangle}}
#' @param bias,interpolate,g these arguments are passed to
#'   \code{\link{gyrotriangle}} in the case when \code{facesColor} is a color
#'   palette
#'
#' @return No value, called for plotting.
#' @export
#'
#' @importFrom rgl tmesh3d shade3d spheres3d lines3d
#' @importFrom Morpho mergeMeshes
#' @importFrom Rvcg vcgClean vcgGetEdge
#'
#' @examples # hyperbolic great stellated dodecahedron
#' library(gyro)
#' library(rgl)
#' GSD <- system.file(
#'   "extdata", "greatStellatedDodecahedron.ply", package = "gyro"
#' )
#' mesh <- Rvcg::vcgPlyRead(GSD, updateNormals = FALSE, clean = FALSE)
#' \donttest{open3d(windowRect = c(50, 50, 562, 562), zoom = 0.7)
#' plotGyroMesh(
#'   mesh,
#'   edgesAsTubes = FALSE, edgesColor = "black",
#'   facesColor = "firebrick1"
#' )}
plotGyroMesh <- function(
    mesh, s = 1, model = "U", iterations = 5, n = 100, edges = TRUE,
    edgesAsTubes = TRUE, edgesColor = "yellow", tubesRadius = 0.03,
    verticesAsSpheres = edgesAsTubes, spheresColor = edgesColor,
    spheresRadius = 0.05,
    facesColor = "navy", bias = 1, interpolate = "linear", g = identity
){
  if(!inherits(mesh, "mesh3d")) {
    stopifnot("vertices" %in% names(mesh), "faces" %in% names(mesh))
    mesh <- tmesh3d(
      vertices = t(mesh[["vertices"]]),
      indices  = t(mesh[["faces"]]),
      homogeneous = FALSE
    )
  }
  Vertices <- mesh[["vb"]][-4L, ]
  Triangles <- mesh[["it"]]
  Edges <- as.matrix(vcgGetEdge(mesh)[, c(1L, 2L)])
  model <- match.arg(model, c("M", "U"))
  if(model == "M"){
    snorms <- apply(points, 1L, dotprod)
    if(any(snorms >= s)){
      stop(
        "In the M\u00f6bius gyrovector space, points must be ",
        "strictly inside the centered ball of radius `s`.",
        call. = TRUE
      )
    }
  }

  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(edgesAsTubes))
  stopifnot(isBoolean(verticesAsSpheres))

  ntriangles <- ncol(Triangles)
  Gtriangles <- vector("list", ntriangles)
  palette <- if(length(facesColor) > 1L) facesColor
  for(i in 1L:ntriangles){
    triangle <- Vertices[, Triangles[, i]]
    Gtriangles[[i]] <- gyrotriangle(
      triangle[, 1L], triangle[, 2L], triangle[, 3L],
      s = s, model = model, iterations = iterations, palette = palette,
      bias = bias, interpolate = interpolate, g = g
    )
  }
  mesh <- vcgClean(mergeMeshes(Gtriangles), sel = 0, silent = TRUE)

  if(is.null(palette)) {
    shade3d(mesh, color = facesColor, polygon_offset = 1)
  } else {
    shade3d(mesh, meshColor = "vertices", polygon_offset = 1)
  }

  if(edges) {
    if(edgesAsTubes) {
      for(i in 1L:nrow(Edges))  {
        edge <- Edges[i, ]
        gtube <- gyrotube(
          Vertices[, edge[1L]], Vertices[, edge[2L]], s = s, model = model,
          n = n, radius = tubesRadius
        )
        shade3d(gtube, color = edgesColor)
      }
    } else {
      if(model == "M") {
        for(i in 1L:nrow(Edges)){
          edge <- Edges[i, ]
          gsegment <- Mgyrosegment(
            Vertices[, edge[1L]], Vertices[, edge[2L]], s = s, n = n
          )
          lines3d(gsegment, color = edgesColor, lwd = 2, line_antialias = TRUE)
        }
      } else {
        for(i in 1L:nrow(Edges)) {
          edge <- Edges[i, ]
          gsegment <- Ugyrosegment(
            Vertices[, edge[1L]], Vertices[, edge[2L]], s = s, n = n
          )
          lines3d(gsegment, color = edgesColor, lwd = 2, line_antialias = TRUE)
        }
      }
    }
  }

  if(verticesAsSpheres) {
    spheres3d(t(Vertices), radius = spheresRadius, color = spheresColor)
  }

  invisible(NULL)
}
