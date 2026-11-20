#' Calculate the normals from tangents of a trajectory object
#'
#' This function computes the normal vectors at each point of an object of class 
#' `trajectory` by rotating the tangent vectors 90 degrees.
#'
#' @param obj An object of class `trajectory` that contains a component `points`.
#' @return A matrix of the same dimensions as `obj$points` with the normal vectors. 
#' Each row represents a normal vector at a given point, which is obtained by rotating 
#' the corresponding tangent vector by 90 degrees.
#' @export

normalVectors <- function(obj) {
	if(!is.trajectory(obj)){
    stop("'obj' is not a valid trajectory object.")
  }
  tangents <- tangentVectors(obj)
  normals <- cbind(-tangents[,2], tangents[,1]) # rotación de 90 grados
  return(normals)
}