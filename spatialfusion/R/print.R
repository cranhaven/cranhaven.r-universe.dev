
# set up generic for dstan ----------------------------------------------------------

"print.dstan" <- function(x, ...){
  cat("data object for spatial fusion modeling with Stan consisting of: \n")
  cat(paste("-", x$n_point_var, "geostatistical variable(s), with", x$n_point, "locations \n"))
  cat(paste("-", x$n_area_var, "lattice variable(s), with", x$n_sample, "sampling point locations \n"))
  cat(paste("-", x$n_pp_var, "point pattern variable(s), with", x$n_grid, "gridded locations \n\n"))
  cat("Provide this object as 'data' argument in fusion() to fit a spatial fusion model.")
}

# set up generic for dinla ----------------------------------------------------------

"print.dinla" <- function(x, ...){
  cat("data object for spatial fusion modeling with INLA consisting of: \n")
  cat(paste("-", x$n_point_var, "geostatistical variable(s)\n"))
  cat(paste("-", x$n_area_var, "lattice variable(s)\n"))
  cat(paste("-", x$n_pp_var, "point pattern variable(s)\n\n"))
  cat("Provide this object as 'data' argument in fusion() to fit a spatial fusion model.")
}
