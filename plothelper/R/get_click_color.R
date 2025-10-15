#' Obtaining the Colors of Positions Clicked
#' 
#' The function draws an image and asks the user 
#' to click on the positions whose colors the user 
#' wants to know. NOTE: after clicking, you must 
#' press Esc button to continue. The result is a vector
#' of colors in hex mode.
#' 
#' @param x a raster object, or an image loaded by
#' \code{magick::image_read} or the filename of 
#' that image. 
#'
#' @export
get_click_color=function(x){
	if (grDevices::dev.capabilities()$locator == FALSE) stop("Your device does not support mouse locator.")
	if ( ! grDevices::is.raster(x)){
		if (is.character(x)) x=magick::image_read(x)
		x=grDevices::as.raster(x)
	}
	omar=graphics::par()$mar
	graphics::par(mar=rep(0.5, 4))
	on.exit(graphics::par(mar=omar))
	height=dim(x)[1]
	graphics::plot(x)
	clicking=graphics::locator()
	if (is.null(clicking)) stop("When choosing colors, you must click at least 1 time.")
	posxy=cbind(height+1-ceiling(clicking$y), ceiling(clicking$x)) # y and then x, NOT the opposite
	GETXIJ=function(X, MAT) MAT[X[1], X[2]]
	apply(posxy, 1, FUN=GETXIJ, MAT=x)
}
	