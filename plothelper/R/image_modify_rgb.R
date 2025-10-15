#' Modify R, G, B Values of an Image
#' 
#' 
#' The function modifies the R, G, B values 
#' of an image and is used in the same 
#' way as \code{image_modify_hsv} 
#' in this package. The 
#' three channels can be modified separately.
#' The ways to modify include: setting values 
#' to some specified values (set_*), adding (add_*), 
#' multiplying the original values (mult_*), 
#' rescaling the original values (rescale_*), 
#' using a function to recompute values (fun_*). 
#' The most useful way is to use some internal 
#' curves that mimic those PS-like apps.
#' 
#' @details
#'  Several internal curves can be used. 
#' Please see the Details part 
#' of \code{\link{image_modify_hsv}}.
#' 
#' @param x an image created 
#' by \code{magick::image_read} or 
#' other functions in package magick. 
#' @param set_r set r values with specific values.
#' @param add_r add specific 
#' values to current R values.
#' @param mult_r multiply the current values 
#' with specific values.
#' @param rescale_r a length 2 numeric vector 
#' specifying the desired range of R values, 
#' e. g., \code{rescale_r = c(180, 240)} which 
#' will make the smallest original value to 
#' be 180, and the largest, 240. Alternatively, 
#' it can be your own scaling function.
#' @param fun_r your own modifying function 
#' (e. g., \code{fun_r = sqrt}). Alternatively, it can 
#' be a list that designates how to use internal 
#' curves. See \code{\link{image_modify_hsv}}.
#' @param set_g,add_g,mult_g,rescale_g,fun_g parameters 
#' to change G values. Used in the same way as those 
#' for R. See above. 
#' @param set_b,add_b,mult_b,rescale_b,fun_b parameters 
#' to change B values. Used in the same way as those 
#' for R. See above.
#' @param result the default is "magick", the output is 
#' a magick picture. When it is "raster", a matrix is created 
#' which can be use as a raster 
#' for \code{ggplot2::annotation_raster}.
#' @param res when the result is a magick picture, the 
#' \code{res} parameter used by \code{magick::image_graph}.
#' Default is 144.
#' 
#' @export
image_modify_rgb=function(x, 
	set_r=NULL, add_r=NULL, mult_r=NULL, rescale_r=NULL, fun_r=NULL, 
	set_g=NULL, add_g=NULL, mult_g=NULL, rescale_g=NULL, fun_g=NULL, 
	set_b=NULL, add_b=NULL, mult_b=NULL, rescale_b=NULL, fun_b=NULL, 	
	result="magick", res=144){
	
	stopifnot(result %in% c("magick", "raster"))
	it_is_pic=FALSE
	# In fact x can be a color vector. But this is not encouraged, so the manual does not mention it.
	if (grepl("magick", class(x)[1])){
		it_is_pic=TRUE
		x=as.raster(x)
		nrpic=nrow(x)
		ncpic=ncol(x)
		x=as.character(x)
	}
	napos=which(x=="transparent")

	## change r
	if ((is.null(set_r))+(is.null(add_r))+(is.null(mult_r))+(is.null(rescale_r))+(is.null(fun_r)) != 5){
		if (!is.null(set_r)) x=farver::set_channel(x, channel="r", value=set_r, space = "rgb")
		if (!is.null(add_r)) x=farver::add_to_channel(x, channel="r", value=add_r, space = "rgb")
		if (!is.null(mult_r)) x=multiply_channel(x, channel="r", value=mult_r, space = "rgb")
		if (!is.null(rescale_r)){
			x=farver::set_channel(
				x, channel="r", 
				value=RESCALE_FUN_VEC(farver::get_channel(x, channel="r", space="rgb"), para=rescale_r), 
				space="rgb"
			)
		}
		
		if (!is.null(fun_r)){
			rr=farver::get_channel(x, channel="r", space="rgb")
			if (is.function(fun_r)){
				rr=match.fun(fun_r)(rr) 
			} else if (is.list(fun_r)){
				rr=rr/255
				rr=USE_INTERNAL_CURVE(rr, LIST=fun_r, cat_text=NULL)
				rr=rr*255
			}
			x=farver::set_channel(x, channel="r", value=rr, space="rgb")
			rr=NULL
		}
	}

	## change g	
	if ( (is.null(set_g))+(is.null(add_g))+(is.null(mult_g))+(is.null(rescale_g))+(is.null(fun_g)) != 5){
		if (!is.null(set_g)) x=farver::set_channel(x, channel="g", value=set_g, space = "rgb")
		if (!is.null(add_g)) x=farver::add_to_channel(x, channel="g", value=add_g, space = "rgb")
		if (!is.null(mult_g)) x=multiply_channel(x, channel="g", value=mult_g, space = "rgb")
		if (!is.null(rescale_g)){
			x=farver::set_channel(
				x, channel="g", 
				value=RESCALE_FUN_VEC(farver::get_channel(x, channel="g", space="rgb"), para=rescale_g), 
				space="rgb"
			)
		}
		
		if (!is.null(fun_g)){
			gg=farver::get_channel(x, channel="g", space="rgb")
			if (is.function(fun_g)){
				gg=match.fun(fun_g)(gg) 
			} else if (is.list(fun_g)){
				gg=gg/255
				gg=USE_INTERNAL_CURVE(gg, LIST=fun_g, cat_text=NULL)
				gg=gg*255
			}
			x=farver::set_channel(x, channel="g", value=gg, space="rgb")
			gg=NULL
		}
	}

	## change b	
	if ( (is.null(set_b))+(is.null(add_b))+(is.null(mult_b))+(is.null(rescale_b))+(is.null(fun_b)) != 5){
		if (!is.null(set_b)) x=farver::set_channel(x, channel="b", value=set_b, space = "rgb")
		if (!is.null(add_b)) x=farver::add_to_channel(x, channel="b", value=add_b, space = "rgb")
		if (!is.null(mult_b)) x=multiply_channel(x, channel="b", value=mult_b, space = "rgb")
		if (!is.null(rescale_b)){
			x=farver::set_channel(
				x, channel="b", 
				value=RESCALE_FUN_VEC(farver::get_channel(x, channel="b", space="rgb"), para=rescale_b), 
				space="rgb"
			)
		}
		
		if (!is.null(fun_b)){
			bb=farver::get_channel(x, channel="b", space="rgb")
			if (is.function(fun_b)){
				bb=match.fun(fun_b)(bb) 
			} else if (is.list(fun_b)){
				bb=bb/255
				bb=USE_INTERNAL_CURVE(bb, LIST=fun_b, cat_text=NULL)
				bb=bb*255
			}
			x=farver::set_channel(x, channel="b", value=bb, space="rgb")
			bb=NULL
		}
	}

	if (length(napos)>0) x[napos]="transparent"
	
	if (it_is_pic == FALSE){
		return(x)
	} else {
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
}
