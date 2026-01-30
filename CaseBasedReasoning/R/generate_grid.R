#' Generate Grid
#' 
#' Generates a uniform grid over the distribution of the time2event variable, 
#' calculates closest point and returns this point for each input time2event 
#' element. Memory consumption will increase when performing the randomForest 
#' model with many unique time2event values. Therefore, we offer a reduction of 
#' the time2event values by choosing closest elements in a grid. 
#' 
#' @param t2e numeric vector with time2event values
#' @param grid_length number of grid elements
#' 
#' @return a list with new_t2e and grid_error
#' 
#' @export
generate_grid <- function(t2e, grid_length = 250) {
  t2e_grid <- seq(0, max(t2e), length.out = grid_length)
  d <- cpp_weightedDistanceXY(matrix(t2e_grid), matrix(t2e), 1)
  o <- cpp_orderMatrix(d, 0, 1) |> 
    as.numeric()
  new_grid <- t2e_grid[o]
  d |> 
    as.data.frame() |> 
    as.list() |> 
    purrr::map2(.y = o, .f = function(x, y) {
      x[y]
    }) |> 
    purrr::reduce(c) -> grid_error
  
  list(grid_error = grid_error,
       new_t2e    = new_grid)
}