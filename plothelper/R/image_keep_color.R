#' Keep Some Colors Unchanged and Make 
#' Others into Grayscale
#' 
#' This function keeps pixels with certain 
#' colors unchanged and transforms 
#' others into grayscale.
#' The function is in fact a wrapper of 
#' \code{magick::image_transparent}, so it 
#' uses the latter's \code{color} and \code{fuzz}
#' parameters. NOTE: the function only works 
#' for fully opaque or fully 
#' transparent (labelled as "transparent") pixels.
#' 
#' @param x an image read into R 
#' by \code{magick::image_read}.
#' @param color the same 
#' as \code{magick::image_transparent}.
#' You can use 1 or more colors.
#' @param fuzz the same 
#' as \code{magick::image_transparent}. 
#' However, Its length must either be 1 or the same 
#' as \code{color}.
#' @param result if it is "magick" (default), the result is 
#' a magick image, if it is "raster", the result is a matrix.
#' 
#' @export
image_keep_color=function(x, color=NULL, fuzz=10, result="magick"){
	if ( ! grepl("magick", class(x)[1])) stop("x must be a picture read into R by magick::image_read.")
	if (identical(color, "click")) color=get_click_color(x)
	if (length(color)==0) stop("You must specify at least 1 color.")
	if ("transparent" %in% color) stop("Please do not click on transparent area.")
	img_format=as.character(image_info(x)[, 1])
	
	# only png, tiff, tif need not change format
	if (! img_format %in% c("JPG", "JPEG", "jpg", "jpeg")){
		xtransp=image_transparent_inverse(x, color=color, fuzz=fuzz)
	} else {
		x=magick::image_convert(x, format="PNG", matte=TRUE)
		xtransp=image_transparent_inverse(x, color=color, fuzz=fuzz)
	}
	x=magick::image_convert(x, colorspace="gray")
	x=magick::image_composite(x, xtransp, operator="atop")
	if (result=="magick"){
		now_format=as.character(image_info(x)[, 1])
		if (now_format != img_format) x=magick::image_convert(x, format=img_format, matte=TRUE) # restore the original format
		return(x)
	} else {
		return(as.matrix(as.raster(x)))
	}
}
