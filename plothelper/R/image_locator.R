#' Get the Width and Height of the
#' Mouse Clicked Points
#' 
#' This function simply gets the width and 
#' height values of the points on which you click.
#' The result is a list of two vectors, the first 
#' vector is for width, the second for height.
#' 
#' @param x a raster object, or an image loaded by
#' \code{magick::image_read} or the filename of 
#' that image. 
#' @param rectangle if it is FALSE (default), 
#' the result list 
#' contains the width and height values. 
#' If it is TRUE, only the left, right, top, bottom values 
#' of the rectangle 
#' designated by your clicking are returned.
#' 
#' @export
image_locator=function(x, rectangle=FALSE){
	if (grDevices::dev.capabilities()$locator == FALSE) stop("Your device does not support mouse locator.")
	if ( ! grDevices::is.raster(x)){
		if (is.character(x)) x=magick::image_read(x)
		x=grDevices::as.raster(x)
	}
	omar=graphics::par()$mar
	graphics::par(mar=rep(0.5, 4))
	on.exit(graphics::par(mar=omar))
	height=dim(x)[1]
	width=dim(x)[2]
	graphics::plot(x)
	clicking=graphics::locator()
	xpos=ceiling(clicking$x)
	xpos[which(xpos > width)]=width
	xpos[which(xpos < 1)]=1
	ypos=height+1-ceiling(clicking$y)
	ypos[which(ypos > height)]=height
	ypos[which(ypos < 1)]=1
	if (identical(rectangle, FALSE)){
		return(list(x=xpos, y=ypos))
	} else {
		result=c(range(xpos), range(ypos))
		names(result)=c("left", "right", "top", "bottom")
		return(result)
	}
}
