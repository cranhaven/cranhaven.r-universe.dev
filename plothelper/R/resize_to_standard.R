#' Resize an Image According to the Other Image or to Ratios
#'
#' Simple wrapper of \code{magick::image_resize}.
#' See the parameters below.
#'
#' @param x the image you want to resize.
#' @param standard either the image whose size is the 
#' standard or two ratios. When it specifies two ratios, 
#' it should be a numeric vector whose first and second 
#' elements are multipliers for width and height. For 
#' example, x's width and height are 100 and 60, and 
#' \code{standard = c(0.5, 3)}, then the result 
#' image's width and height will be 50 and 180. If one of 
#' the two number is NA, then the dimension represented 
#' by this NA will be modified automatically.
#' @param what this parameter is used only when 
#' \code{standard} is an image. It specifies 
#' the way to resize. When it 
#' is "width", let x's width be the same 
#' as \code{standard}; whether its height is 
#' automatically scaled depends on \code{scale}.
#' When it is "height", 
#' let x's height be the same as \code{standard}; whether
#' its width is automatically scaled 
#' depends on \code{scale}. When it is  "all" (or "both"), 
#' the default, 
#' let x's width and height
#' be the same as \code{standard}. When it is two number 
#' linked with a "_", it means resizing a according to 
#' b's width and height multiplied. For example, 
#' if it is "3_2" and b's width and height are 50, 70, then 
#' the result's width and height are 50 * 3 = 150, 
#' 70 * 2 = 140. Forms like "_2" or "3_" are also accepted.
#' @param scale Default is TRUE. It is only used when 
#' only one of width and height is to 
#' be modified. This parameter 
#' decides whether the image is automatically scaled.
#' 
#' @export
resize_to_standard=function(x, standard=0.5, what="all", scale=TRUE){
	if (scale == TRUE){
		kept=""
	} else if (scale == FALSE){
		kept="!"
	} else {
		stop("scale must be either TRUE or FALSE.")
	}
	cla=class(standard)[1]

	if (grepl("magick", cla)){
		sinfo=as.numeric(magick::image_info(standard)[1, 2: 3])
		swidth=sinfo[1]
		sheight=sinfo[2]
		if (what == "width"){
			ssize=paste(swidth, "x", kept, sep="")
		} else if (what == "height"){
			ssize=paste("x", sheight, kept, sep="")
		} else if (what == "all" | what == "both"){
			ssize=paste(swidth, "x", sheight, "!", sep="")
		} else if (grepl("_", what)){
			two_num=unlist(strsplit(what, "_"))
			left_num=two_num[1]
			right_num=if (length(two_num) == 2) two_num[2] else "" # MUST DO THIS
			if ( left_num != "" & right_num != ""){
				swidth2=ceiling(swidth*as.numeric(left_num))
				sheight2=ceiling(sheight*as.numeric(right_num))
				ssize=paste(swidth2, "x", sheight2, "!", sep="")
			} else if ( left_num == "" & right_num != ""){
				sheight2=ceiling(sheight*as.numeric(right_num))
				ssize=paste("x", sheight2, kept, sep="")				
			} else if ( left_num != "" & right_num == ""){
				swidth2=ceiling(swidth*as.numeric(left_num))
				ssize=paste(swidth2, "x", kept, sep="")		
			} else {
				stop("At least one side of _ should be given.")
			}
		} else {
			stop("The what argument should be written correctly.")
		}
		magick::image_resize(x, ssize)
	} else {
		stopifnot(is.numeric(standard))
		if (length(standard) == 1) standard=rep(standard, 2)
		left_num=standard[1]
		right_num=standard[2]
		xinfo=as.numeric(magick::image_info(x)[1, 2: 3])
		xwidth=xinfo[1]
		xheight=xinfo[2]	
		if ( ! is.na(left_num) & ! is.na(right_num)){
			new_width=ceiling(xwidth*left_num)
			new_height=ceiling(xheight*right_num)
			ssize=paste(new_width, "x", new_height, "!", sep="")
		} else if ( is.na(left_num) & ! is.na(right_num)){
			new_height=ceiling(xheight*right_num)
			ssize=paste("x", new_height, kept, sep="")
		} else if ( ! is.na(left_num) & is.na(right_num)){
			new_width=ceiling(xwidth*left_num)
			ssize=paste(new_width, "x", kept, sep="")			
		} else {
			stop("The two number of standard should not be all NA.")
		}
		magick::image_resize(x, ssize)
	}
}
