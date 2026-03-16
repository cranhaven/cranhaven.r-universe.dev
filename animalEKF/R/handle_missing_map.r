handle_missing_map <- function(env_obj) {

	if (is.null(env_obj$area_map)) {
		xr <- range(env_obj$d$X)
		yr <- range(env_obj$d$Y)
		boundary_pad <- 0.1
		lx <- diff(xr)
		ly <- diff(yr)
			
		print("Creating a default shapefile to use within the bounds of the observed points")
		env_obj$area_map <- sf::st_geometry(rectangular_shapefile(xmin=xr[1] - boundary_pad * lx,
																  xmax=xr[2] + boundary_pad * lx,
																  ymin=yr[1] - boundary_pad * ly,
																  ymax=yr[2] + boundary_pad * ly))

	}


invisible(NULL)

}