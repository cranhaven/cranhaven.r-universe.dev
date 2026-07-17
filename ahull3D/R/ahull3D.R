#' Fast 3D Alpha Hull with Label Propagation
#'
#' Computes the alpha hull (alpha shape) of a 3D point cloud with optional
#' label propagation for point classification.
#'
#' @param points Nx3 matrix of points
#' @param alpha Alpha value (radius parameter)
#' @param input_truth Optional labels (point id) for each input point
#' @param volume Compute volume (default: FALSE)
#' @return A mesh object (from \code{rgl::tmesh3d}) with attributes:
#'   - input_truth: Propagated labels
#'   - face_truth: Labels for each face
#'   - alpha: Alpha value used
#'   - volume: Volume (if requested)
#' @references
#' Edelsbrunner, H., Kirkpatrick, D., & Seidel, R. (1983). 
#' On the shape of a set of points in the plane. 
#' IEEE Transactions on Information Theory, 29(4), 551-559.
#' <doi:10.1007/BF02579193>
#' @export
#' @examples
#' # Small example for testing 
#' # Generate random points on a sphere
#' pts <- matrix(rnorm(100), ncol = 3)
#' pts <- pts / sqrt(rowSums(pts^2))
#' 
#' # Compute alpha hull
#' mesh <- ahull3D(pts, alpha = 0.5)
#' 
#' # Visualize
#' library(rgl)
#' shade3d(mesh, col = "lightblue")
#' 
#' \donttest{
#' # Larger example with visualization (may take longer)
#' pts <- matrix(rnorm(300), ncol = 3)
#' pts <- pts / sqrt(rowSums(pts^2))
#' mesh <- ahull3D(pts, alpha = 0.5)
#' 
#' # Visualize
#' library(rgl)
#' shade3d(mesh, col = "lightblue")
#' 
#' # Example with labels
#' pts <- matrix(rnorm(60), ncol = 3)
#' pts <- pts / sqrt(rowSums(pts^2))
#' labels <- rep(1:2, each = 10)
#' mesh_labeled <- ahull3D(pts, alpha = 0.5, input_truth = labels)
#' attr(mesh_labeled, "input_truth")
#' 
#' # Compute volume
#' mesh_vol <- ahull3D(pts, alpha = 0.8, volume = TRUE)
#' attr(mesh_vol, "volume")
#' }
ahull3D <- function(points, alpha, input_truth = NULL, volume = FALSE) {
  # Input validation
  stopifnot(is.matrix(points), ncol(points) == 3L)
  stopifnot(alpha >= 0)
  
  if (is.null(input_truth)) {
    input_truth <- rep(1, nrow(points))
    return_labels <- FALSE
  } else {
    stopifnot(length(input_truth) == nrow(points))
    return_labels <- TRUE
  }
  
  # Call C++ function
  result <- .Call(
    "_ahull3D_FAS_cpp_with_labels", 
    t(points), alpha, input_truth, volume,
    PACKAGE = "ahull3D"
  )
  
  vertices <- result$vertices
  vertex_labels <- result$vertex_labels
  
  # Create mesh (original makeMesh logic)
  nvertices <- ncol(vertices)
  if (nvertices == 0L) {
    message("The alpha shape is empty.")
    return(NULL)
  }
  
  # Use rgl::tmesh3d (already imported)
  mesh0 <- rgl::tmesh3d(
    vertices    = vertices,
    indices     = matrix(1L:nvertices, nrow = 3L),
    homogeneous = FALSE
  )
  
  # Clean mesh using Rvcg
  mesh <- Rvcg::vcgClean(mesh0, sel = c(0L, 7L), silent = TRUE)
  mesh[["remvert"]] <- NULL
  mesh[["remface"]] <- NULL
  
  # Add labels after deduplication
  if (return_labels) {
    # Map labels to deduplicated vertices
    if (requireNamespace("RANN", quietly = TRUE)) {
      cleaned_vertices <- t(mesh$vb[1:3, ])
      orig_vertices <- t(vertices[1:3, ])
      nn_result <- RANN::nn2(orig_vertices, cleaned_vertices, k = 1)
      final_labels <- vertex_labels[nn_result$nn.idx[, 1]]
    } else {
      # Simple fallback - use first vertex labels
      final_labels <- vertex_labels[1:ncol(mesh$vb)]
    }
    
    attr(mesh, "input_truth") <- final_labels
    attr(mesh, "face_truth") <- matrix(final_labels[mesh$it], nrow = 3)
  }
  
  # Add volume if requested
  if (volume) {
    attr(mesh, "volume") <- attr(vertices, "volume")
  }
  
  attr(mesh, "alpha") <- alpha
  
  return(mesh)
}

#' @useDynLib ahull3D, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom rgl tmesh3d
#' @importFrom Rvcg vcgClean
#' @importFrom utils packageVersion
NULL

# Declare global variables to avoid R CMD check NOTES
utils::globalVariables(c("vb", "it"))