#' Modify both a Subregion  and the Whole
#' of an Image
#' 
#' The function is similar to 
#' \code{image_modify_local} but with different 
#' parameters. It modifies both a subregion  
#' of the image and the whole image, and then 
#' combines them. The subregion can be chosen 
#' either by numeric values or by mouse click ,which 
#' is the same as \code{image_modify_local}.
#' 
#' @param x an image read into R by 
#' \code{magick::image_read} or an image  
#' modified by functions in the magick 
#' package.
#' @param FUN1 a function to modify 
#' a subregion of \code{x}. NOTE: the result 
#' of these functions must be of the same class 
#' as \code{x} and should not change the sizes 
#' of the subregion.
#' @param FUN2 a function to modify 
#' the whole image, which must not change 
#' the size of the image. If it is NULL (default), 
#' nothing will do to the whole image.
#' @param geometry this parameter is 
#' different from the one used in package 
#' magick. Here, in this function, you can 
#' set \code{geometry = "click"} if you want 
#' to show which part is the subregion 
#' by mouse click (see 
#' function \code{image_crop_click} for how to 
#' use mouse click). Otherwise, you can use 
#' a length 4 vector with the exact order: left, 
#' right, top, bottom. 
#' @param rectangle if it is TRUE (default), 
#' the subregion is a rectangle area. If 
#' it is FALSE, the subregion is 
#' an irregular polygon area, and, now 
#' \code{geometry} is ignored, you must 
#' designate the area by mouse click. 
#' 
#' @export
image_modify_local2=function(x, FUN1, FUN2=NULL, geometry="click", rectangle=TRUE){
	stopifnot(is.function(FUN1))
	if (is.null(FUN2)) FUN2=function(x) x
	stopifnot(is.function(FUN2))
	myfun1=match.fun(FUN1)
	myfun2=match.fun(FUN2)
	stopifnot(rectangle %in% c(TRUE, FALSE))
	
	if (rectangle==TRUE){
		Y=IMAGE_MODIFY_LOCAL2_RECT(x=x, FUN1=myfun1, FUN2=myfun2, geometry=geometry)
	} else {
		Y=IMAGE_MODIFY_LOCAL2_FREE(x=x, FUN1=myfun1, FUN2=myfun2)
	}
	
	Y
}
	
IMAGE_MODIFY_LOCAL2_RECT=function(x, FUN1, FUN2, geometry="click"){
	if (identical(geometry, "click")){
		POS=image_crop_click(x=x, only_value=TRUE)
	} else {
		if (length(geometry) != 4 || geometry[2]-geometry[1] <= 0 || geometry[4]-geometry[3] <= 0) stop("geometry must be either click or a integer vector of length 4 with the excact order of left, right, top, bottom.")
		POS=geometry
	}
	cha=paste(POS[2]-POS[1]+1, "x", POS[4]-POS[3]+1, "+", POS[1]-1, "+", POS[3]-1, sep="")
	small=magick::image_crop(x, geometry=cha)
	magick::image_composite(FUN2(x), FUN1(small), operator="atop", offset=paste("+", POS[1]-1, "+", POS[3]-1, sep=""))
}

IMAGE_MODIFY_LOCAL2_FREE=function(x, FUN1, FUN2){
	small=image_crop_click(x=x, rectangle=FALSE, trim=FALSE)
	magick::image_composite(FUN2(x), FUN1(small), operator="atop")
}
