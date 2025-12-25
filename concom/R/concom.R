#' @useDynLib concom, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @noRd
NULL

#' @title Connected components
#' @description Fast computation of the connected components of an undirected 
#'   graph.
#'
#' @param edges a matrix with two columns, whose rows represent the edges of 
#'   the graph; each edge is given by two vertex indices, and it is assumed 
#'   that the vertex indices are 1, 2, 3, ...
#'
#' @return A list with four elements: \code{indices}, an integer vector 
#'   whose \code{i}-th element gives the label of the connected component of 
#'   vertex \code{i}; \code{sizes}, an integer vector giving the number of 
#'   elements of each connected component; \code{ncomponents}, the number 
#'   of connected components; \code{components}, a list of length 
#'   \code{ncomponents}, whose \code{j}-th element is the integer vector made 
#'   of the labels of the \code{j}-th connected component.
#' @export
#'
#' @examples
#' library(concom)
#' edges <- cbind(
#'   1:7,
#'   c(2, 3, 1, 5, 6, 7, 4)
#' )
#' concom(edges)
concom <- function(edges){
  stopifnot(is.matrix(edges))
  stopifnot(ncol(edges) == 2L)
  storage.mode(edges) <- "integer"
  stopifnot(all(edges >= 1L))
  cppOutput <- concom_cpp(edges)
  indices <- cppOutput[["indices"]]
  c(
    cppOutput, 
    list("components" = split(seq_along(indices), indices))
  )
}

#' @title Connected components of a 'rgl' mesh
#' @description Computes the connected components of a triangular 'rgl' mesh.
#'
#' @param tmesh a triangular \strong{rgl} mesh (of class \strong{mesh3d})
#'
#' @return A list of \strong{rgl} meshes, each one corresponding to a 
#'   connected component. 
#' 
#' @importFrom rgl tmesh3d
#' @importFrom Rvcg vcgGetEdge vcgClean
#' @importFrom english Words
#' 
#' @export
#'
#' @examples
#' library(concom)
#' library(rgl)
#' library(rmarchingcubes)
#' 
#' # credit to 'ICN5D' for this isosurface 
#' f <- function(x, y, z, a, cosb, sinb){
#'     (sqrt((sqrt(x*x + (y*sinb + a*cosb)^2) - 2)^2) - 1)^2 +
#'       (sqrt((sqrt(z*z + (y*cosb - a*sinb)^2) - 2)^2) - 1)^2
#' }
#' a <- 0.6
#' b <- 0.785
#' cosb <- cos(b)
#' sinb <- sin(b)
#' 
#' \donttest{
#' x <- z <- seq(-3.5, 3.5, len = 150L)
#' y <- seq(-4.2, 4.2, len = 150L)
#' g <- expand.grid(X = x, Y = y, Z = z)
#' voxel <- array(
#'   with(g, f(X, Y, Z, a, cosb, sinb)),
#'   dim = c(150L, 150L, 150L)
#' )
#' 
#' contour_shape <- contour3d(
#'   griddata = voxel,
#'   level = 0.1,
#'   x = x,
#'   y = y,
#'   z = z
#' )
#' 
#' tmesh <- tmesh3d(
#'   vertices = t(contour_shape[["vertices"]]),
#'   indices = t(contour_shape[["triangles"]]),
#'   normals = contour_shape[["normals"]],
#'   homogeneous = FALSE
#' )
#' 
#' components <- concom3d(tmesh)
#' colors <- hcl.colors(length(components))
#' open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' lapply(1:length(components), function(i){
#'   shade3d(components[[i]], color = colors[i])
#' })
#' }
concom3d <- function(tmesh){
  if(!inherits(tmesh, "mesh3d")){
    stop("The `tmesh` argument must be a triangle 'rgl' mesh.")
  }
  edges <- as.matrix(vcgGetEdge(tmesh)[, c("vert1", "vert2")])
  comps <- concom(edges)
  components <- comps[["components"]]
  ncc <- comps[["ncomponents"]]
  if(ncc == 1L){
    message("Only one connected component has been found.")
  }else{
    message(Words(ncc), " connected components found.")
  }
  meshes <- vector("list", ncc)
  for(i in 1L:ncc){
    meshes[[i]] <- vcgClean(tmesh3d(
      vertices = tmesh$vb,
      normals = if("normals" %in% names(tmesh)) t(tmesh$normals),
      indices = tmesh$it[, tmesh$it[1L, ] %in% components[[i]]]
    ), sel = 1, silent = TRUE)
  }
  meshes
}

#' @title Connected components from adjacency matrix
#' @description Connected components of an undirected graph from its 
#'   adjacency matrix.
#'
#' @param M adjacency matrix; it must be a square symmetric matrix with 
#'   numeric or Boolean entries, whose non-zero or \code{TRUE} entries 
#'   indicate the connections (connection between \code{i}-th vertex and 
#'   \code{j}-th vertex if the entry is located at row \code{i} and 
#'   column \code{j})
#'
#' @return The output is the same as the one of the \code{\link{concom}} 
#'   function.
#' @export
#'
#' @examples
#' matAdj <- rbind(
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0),
#'   c(0, 0, 0, 0),
#'   c(0, 0, 0, 1)
#' )
#' concomFromMatAdj(matAdj)
concomFromMatAdj <- function(M){
  if(!is.matrix(M) || length(M) == 0L || (nrow(M) != ncol(M))){
    stop("The adjacency matrix must be a square symmetric matrix.")
  }
  storage.mode(M) <- "logical"
  if(!isSymmetric(M)){
    stop("The adjacency matrix must be a square symmetric matrix.")
  }
  M[lower.tri(M)] <- FALSE
  edges <- which(M, arr.ind = TRUE)
  concom(edges)
}
