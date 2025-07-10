#' Collapse array along one of the dimensions by adding the elements along that
#' dimension.
#'
#' @param array Array to collapse along one dimension.
#' @param dimension Dimension to collapse the array.
#'
#' @return An array that has been collapsed along the given dimension.
sumDims = function(array, dimension){
  dim1 = length(dim(array))
  keep_dims = setdiff(seq(1, dim1, length.out = dim1), c(dimension))
  array_output = apply(array, MARGIN = keep_dims, FUN = 'sum')
  array_output = kronecker(array_output, array(1, dim=array(1, dim=dim1)))
  dim_reshape = seq(1, dim1, length.out = dim1)
  dim_reshape[(dimension+1):length(dim_reshape)] = dim_reshape[dimension:(length(dim_reshape)-1)]
  dim_reshape[dimension] = dim1
  array_output = aperm(array_output, dim_reshape);
  
  return(array_output)
}