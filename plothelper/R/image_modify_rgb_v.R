#' Modify R, G, B Values according to 
#' V values
#' 
#' While the \code{\link{image_modify_rgb}} 
#' function modifies R, G, B with reference to 
#' the original values, 
#' \code{image_modify_rgb_v} also takes into 
#' account the brightness (V) values. It is similar 
#' to those apps which divide an image into 
#' a bright part and a dark part (and, for 
#' example, you can increase red in the 
#' bright part and decrease red in the 
#' dark part.
#' 
#' @details
#' This function uses custom functions 
#' or internal curves to 
#' make modification. See the Details part 
#' of \code{\link{image_modify_hsv}} to 
#' know how to use them. Note: values will 
#' be coerced to be in the [0, 255] range with 
#' no warning. For example, the original value 
#' is 240 and it becomes 280 in the output, then 
#' it will be set to 255 automatically.
#' 
#' @param x an image created 
#' by \code{magick::image_read} or 
#' other functions in package magick. 
#' @param fun_r,fun_g,fun_b a function or a list which 
#' designates an internal curve. See the Details part of 
#' \code{\link{image_modify_hsv}}.
#' @param alpha whether to allow 
#' the output colors have transparency. Default is FALSE.
#' @param rescale_v You can rescale the V values before 
#' modifying colors. A desired range of V values can 
#' be given, 
#' e. g., \code{rescale_v = c(0.2, 1)} which 
#' will make the smallest original value to 
#' be 0.2, and the largest, 1. Alternatively, 
#' it can be your own scaling function.
#' @param result the default is "magick", the output is 
#' a magick picture. When it is "raster", a matrix is created 
#' which can be use as a raster 
#' for \code{ggplot2::annotation_raster}.
#' @param res when the result is a magick picture, the 
#' \code{res} parameter used by \code{magick::image_graph}.
#' Default is 144.
#' 
#' @export
image_modify_rgb_v=function(x, fun_r=NULL, fun_g=NULL, fun_b=NULL, 	
	alpha=FALSE, rescale_v=NULL, result="magick", res=144){
	
	stopifnot(result %in% c("magick", "raster"))
	if ( ! grepl("magick", class(x)[1])) stop("x must be a picture read into R by magick::image_read.")
	x=as.raster(x)
	nrpic=nrow(x)
	ncpic=ncol(x)
	x=as.character(x)
	
	vv=farver::get_channel(x, channel="v", space="hsv")/360
	if (!is.null(rescale_v)) vv=RESCALE_FUN_VEC(vv, para=rescale_v)
	
	## change r
	if ( ! is.null(fun_r)){
		rr=farver::get_channel(x, channel="r", space="rgb")

		if (is.function(fun_r)){
			rr=rr*(1+(match.fun(fun_r)(vv)-vv))
		} else if (is.list(fun_r)){
			rr=rr*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_r, cat_text=NULL)-vv))
		}
	
		x=farver::set_channel(x, channel="r", value=rr, space="rgb")
		rr=NULL
	}

	## change g	
	if ( ! is.null(fun_g)){
		gg=farver::get_channel(x, channel="g", space="rgb")

		if (is.function(fun_g)){
			gg=gg*(1+(match.fun(fun_g)(vv)-vv))
		} else if (is.list(fun_g)){
			gg=gg*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_g, cat_text=NULL)-vv))
		}
	
		x=farver::set_channel(x, channel="g", value=gg, space="rgb")
		gg=NULL
	}

	## change b	
	if ( ! is.null(fun_b)){
		bb=farver::get_channel(x, channel="b", space="rgb")

		if (is.function(fun_b)){
			bb=bb*(1+(match.fun(fun_b)(vv)-vv))
		} else if (is.list(fun_b)){
			bb=bb*(1+(USE_INTERNAL_CURVE(vv, LIST=fun_b, cat_text=NULL)-vv))
		}
	
		x=farver::set_channel(x, channel="b", value=bb, space="rgb")
		bb=NULL
	}
	
	x=matrix(x, nrow=nrpic, byrow=TRUE)
	if (result=="raster"){
		return(x)
	} else {
		# canv=magick::image_graph(width=ncpic, height=nrpic, bg="transparent", res=res, clip=FALSE)
		# grid::grid.raster(image=x, width=1, height=1)
		# grDevices::dev.off()
		# return(canv)
		magick::image_read(x)
	}
}
