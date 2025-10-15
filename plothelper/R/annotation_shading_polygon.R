#' Layer for Drawing a Single 
#' Irregular Polygon 
#' with Shading Colors
#' 
#' \code{ggplot2::annotation_raster} can only 
#' draw shading rectangles. However, this 
#' function can draw polygons of any shape 
#' with shading colors. See the \code{shape} 
#' argument and the \code{raster} argument.
#'
#' @param shape the polygon can be 
#' a data frame (or matrix object, or tbl_df object) 
#' with x and y coordinates (that is, with two columns), 
#' a plot created by ggplot or an image 
#' read into R by \code{magick::image_read}.
#' If it is a plot created by ggplot, its axes can 
#' be of numeric, discrete or date/datetime type; 
#' however, when the type is date/datetime, 
#' the plot should not use \code{ggplot2::coord_fixed}.
#' @param xmin the left side of the position to 
#' put the polygon. When 
#' \code{shape} is something like a data frame, 
#' you do not need to set xmin, xmax, ymin and ymax, 
#' for the function will generate these values according 
#' to the coordinates in the polygon.
#' @param xmax the right side. 
#' @param ymin the bottom side.
#' @param ymax the top side.
#' @param raster the shading colors. 
#' It can be a raster object, 
#' a matrix of colors, a ggplot plot or an 
#' image read into R by 
#' \code{magick::image_read}.
#' @param interpolate the \code{interpolate}
#' argument used by \code{ggplot2::annotation_raster}
#' when the \code{raster} argument is a matrix or 
#' raster.
#' @param result_interpolate whether to interpolate in the 
#' final result which is essentially an output of 
#' \code{ggplot2::annotation_raster}. Default is TRUE.
#' @param shape_trim this argument 
#' decides whether to trim edges 
#' of \code{shape}.
#' It should be a number 
#' between 0 and 100. Default is NULL. If it is NULL, 
#' no trimming will be done. 
#' @param raster_trim whether to trim raster. 
#' Most of the time we do want to trim the raster.
#' However, the \code{magick::image_trim} function 
#' sometimes trims wrongly. So you may want to turn 
#' it off. Default is NULL.
#' @param result_trim how to trim the 
#' final result. If you find your 
#' figure loses some parts, you can try to turn this off. 
#' Default is NULL.
#' @param result when it is "layer", the function is a  
#' ggplot layer. When it is "magick", the function only 
#' create an image.
#' @param width the width which will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is 800. HOWEVER, if the 
#' final polygon has fuzzy edges, try to enlarge \code{width} 
#' to make them look better.
#' @param height the height which will be passed 
#' to \code{magick::image_graph}. DO SEE Details below 
#' to see how to use this parameter.
#' @param res resolution in pixels which will be passed to 
#' \code{magick::image_graph}. Default is 72.
#'
#' @details
#' \code{height} can be used in the 
#' following ways: 
#' \itemize{
#'   \item (1) an integer which will be 
#' directly passed to \code{image::graph}.
#'   \item (2) a character-like integer, 
#' e.g., \code{height = "0.5"}. Suppose \code{width = 400}, 
#' the height that will be used is 400*0.5 = 200. 
#' This effectively prevents the image from becoming 
#' too large.
#'   \item (3) \code{height = "coord_fixed"}. 
#' the ratio between height and width will 
#' be (top-bottom)/(right-left). And top, bottom,  
#' right and left are extreme values of \code{shape} 
#' when the latter is of class data.frame/matrix/gg.
#'   \item (4) \code{height = "image"}. the width and height 
#' will be the width and height of raster when raster is 
#' a magick object.
#'   \item (5) \code{height = NULL}, the default. 
#' Now height is computed automatically. 
#' A ratio is computed first,  
#' ratio = (top-bottom)/(right-left). if the ratio is larger 
#' than 5 or smaller than 0.2, then height will be 
#' width*5 or width*0.2; else, the height will be treated 
#' in the same way as in (3) above. If \code{shape} is 
#' of class gg and it has uses \code{coord_flip()}, the 
#' height will be automatically adjusted. All these works 
#' are needed to prevent the image from becoming too large.
#' }
#'
#' @export
#' @examples
#' \donttest{
#' # Example 1
#' poly=ellipsexy(-1, 0, a=1, b=1)
#' m=matrix(rainbow(7))
#' ggplot()+
#' 	coord_fixed()+
#' 	annotation_shading_polygon(
#' 		poly, raster=m
#' 	)+
#' 	annotation_shading_polygon(
#' 		poly, raster=m, 
#' 		xmin=1, xmax=5, 
#' 		ymin=-1, ymax=1, 
#' 	)
#' #
#' # Example 2, only an image
#' tt=annotation_shading_polygon(
#' 	poly, result="magick", 
#' 	width=280, height=280
#' )
#' #
#' # Example 3, both shape and raster are 
#' # ggplot plots.
#' p1=ggplot()+geom_tile(aes(x=1: 5, y=1: 5))
#' p2=ggplot()+geom_polygon(aes(x=c(0, 1, 1, 0), 
#' 		y=c(0, 0, 1, 1)), fill="red")+theme_void()
#' ggplot()+coord_fixed()+
#' 	annotation_shading_polygon(
#' 		shape=p1, 
#' 		xmin=1, xmax=10, 
#' 		ymin=1, ymax=5, 
#' 		raster=p2
#' 	)
#' }
annotation_shading_polygon=function(shape=data.frame(c(-1, 1, 0), c(0, 0, 1.732)), xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, raster=NULL, interpolate=TRUE, result_interpolate=TRUE, shape_trim=NULL, raster_trim=NULL, result_trim=NULL, result=c("layer", "magick"), width=800, height=NULL, res=72){

	result=result[1]
	stopifnot(result %in% c("layer", "magick"))
	if (is.null(raster)) raster=matrix(grDevices::rainbow(7))
	
	# check class of shape
	cla_shape=class(shape)[1]
	if (cla_shape %in% c("matrix", "data.frame", "tbl_df")){
		if (is.matrix(shape)) shape=data.frame(shape)
		if (ncol(shape) < 2) stop("shape must have at least 2 columns and only the first two columns will be used.")
		if (ncol(shape) > 2) shape=shape[, 1: 2]
		if (nrow(shape) < 3) stop("shape must have at least 3 rows after deleting NAs.")
	}
	if(grepl("magick", cla_shape)) cla_shape="magick-image"
	if (! cla_shape %in% c("matrix", "data.frame", "tbl_df", "magick-image", "gg")) stop("shape must be of class matrix, data.frame, tbl_df, magick-image or gg.")	
	
	# check class of raster
	cla_raster=class(raster)[1]
	if(grepl("magick", cla_raster)) cla_raster="magick-image"
	if (! cla_raster %in% c("matrix", "raster", "magick-image", "gg")) stop("raster must be of class matrix, raster, magick-image or gg.")
	if (cla_raster == "magick-image"){
		if (! is.null(raster_trim)) raster=magick::image_trim(raster, fuzz=raster_trim)
	}
	
	# shape
	## shape 1
	if (cla_shape %in% c("matrix", "data.frame", "tbl_df")){
		poly_x_min=min(shape[, 1])
		poly_x_max=max(shape[, 1])
		poly_y_min=min(shape[, 2])
		poly_y_max=max(shape[, 2])
		if (is.null(xmin)){
			xmin=poly_x_min
			xmax=poly_x_max
			ymin=poly_y_min
			ymax=poly_y_max
		}
		myshape=ggplot2::ggplot()+
			ggplot2::coord_cartesian(xlim=c(poly_x_min, poly_x_max), ylim=c(poly_y_min, poly_y_max), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::geom_polygon(aes(x=shape[, 1], y=shape[, 2]), color="black", fill="black")
		
		if (is.null(height)){
			decide_ratio=(poly_y_max-poly_y_min)/(poly_x_max-poly_x_min)
			if (decide_ratio<=5 & decide_ratio>=0.2) 	shape_adj_height=ADJUST_HEIGHT(W=width, XMIN=poly_x_min, XMAX=poly_x_max, YMIN=poly_y_min, YMAX=poly_y_max)
			if (decide_ratio>5) 	shape_adj_height=width*5
			if (decide_ratio<0.2) 	shape_adj_height=width*0.2
		} else if (is.numeric(height)){
			shape_adj_height=height
		} else if (is.character(height)){
			if (height=="coord_fixed"){
				shape_adj_height=ADJUST_HEIGHT(W=width, XMIN=poly_x_min, XMAX=poly_x_max, YMIN=poly_y_min, YMAX=poly_y_max)
			} else if (height=="image"){
				if (cla_raster != "magick-image") stop("Setting height = \"image\" is only used when raster is a magick-image object.")
				magick_raster_info=as.numeric(magick::image_info(raster)[1, 2: 3])
				width=magick_raster_info[1]
				shape_adj_height=magick_raster_info[2]			
			} else {
				shape_adj_height=width*as.numeric(height)
			}
		}

		img_shape=magick::image_graph(width=width, height=shape_adj_height, bg="transparent", res=res, clip=FALSE)
		print(myshape)
		grDevices::dev.off()
		if (! is.null(shape_trim)) img_shape=magick::image_trim(img_shape, fuzz=shape_trim)	
	}
	
	## shape 2
	if (cla_shape == "gg"){
		shape=shape+ggplot2::theme_void()
		# Now it is ok whether shape itself has expand=FALSE
		shape$coordinates$expand=FALSE
	
		## first check four edges
		panel_info=ggplot2::ggplot_build(shape)$layout$panel_params[[1]]
		panel_min_x=panel_info$x.range[1]
		panel_max_x=panel_info$x.range[2]		
		panel_min_y=panel_info$y.range[1]
		panel_max_y=panel_info$y.range[2]
		
		## MUST delete coord_fixed and aspect.ratio
		# delete the aspect.ratio element
		shape$theme$aspect.ratio=NULL
		# substitute coord_fixed with coord_cartesian
		if ("CoordFixed" %in% class(shape$coordinates)) shape=suppressMessages(shape <- shape+ggplot2::coord_cartesian(expand=FALSE, xlim=c(panel_min_x, panel_max_x), ylim=c(panel_min_y, panel_max_y)))
		
		## When axis is date/datetime, the method to change is 
		## to change gg$layers[[1]]$mapping. But it may 
		## have too many aesthetics, so it is not realistic to 
		## manually change them all. "change" here means 
		## to change a Date object into numeric. So just do not 
		## use date/datetime variables and coord_fixed at 
		## the same time.
		
		# Auto set xmin if it is not given
		if (result == "layer" & is.null(xmin)){
			xmin=panel_min_x
			xmax=panel_max_x
			ymin=panel_min_y
			ymax=panel_max_y
		}
		
		if (is.null(height)){
			decide_ratio=(panel_max_y-panel_min_y)/(panel_max_x-panel_min_x)
			if (decide_ratio<=5 & decide_ratio>=0.2) 	shape_adj_height=floor(width*decide_ratio) # same as ADJUST_HEIGHT
			if (decide_ratio>5) shape_adj_height=width*5
			if (decide_ratio<0.2) shape_adj_height=width*0.2
		} else if (is.numeric(height)){
			shape_adj_height=height
		} else if (is.character(height)){
			if (height=="coord_fixed"){
				shape_adj_height=ADJUST_HEIGHT(W=width, XMIN=panel_min_x, XMAX=panel_max_x, YMIN=panel_min_y, YMAX=panel_max_y)
			} else if (height=="image"){
				if (cla_raster != "magick-image") stop("Setting height = \"image\" is only used when raster is a magick-image object.")
				magick_raster_info=as.numeric(magick::image_info(raster)[1, 2: 3])
				width=magick_raster_info[1]
				shape_adj_height=magick_raster_info[2]			
			} else {
				shape_adj_height=width*as.numeric(height)
			}
		}
		
		img_shape=magick::image_graph(width=width, height=shape_adj_height, bg="transparent", res=res, clip=FALSE)
		# using panel_info already contain flipping, so the following is not needed 
		# ## gg may have coord_flip()
		# if ("CoordFlip" %in% class(shape$coordinates) & is.null(height)){
		# 	img_shape=magick::image_graph(width=shape_adj_height, height=width, bg="transparent", res=res, clip=FALSE)
		# } else {
		# 	img_shape=magick::image_graph(width=width, height=shape_adj_height, bg="transparent", res=res, clip=FALSE)
		# }
		
		print(shape)
		grDevices::dev.off()
		if (! is.null(shape_trim)) img_shape=magick::image_trim(img_shape, fuzz=shape_trim)	
	}
	
	## shape 3
	if (cla_shape == "magick-image"){
		if (result == "layer"){
			if (is.null(xmin)) stop("When shape is of class magick-image, xmin, xmax, ymin, ymax must not be NULL.")
		}
		img_shape=if (! is.null(shape_trim)) magick::image_trim(shape, fuzz=shape_trim) else shape
	}
	
	shape_info=as.numeric(magick::image_info(img_shape)[1, 2: 3])
	# recalculate width, shape_adj_height
	width=shape_info[1]
	shape_adj_height=shape_info[2]
	
	# coord for raster when shape is not df
	if (cla_shape %in% c("magick-image", "gg")){
		poly_x_min=0
		poly_x_max=width
		poly_y_min=0
		poly_y_max=shape_adj_height
	}
	
	# raster
	if (cla_raster  %in% c("raster", "matrix")){
		bggg=ggplot2::ggplot()+
			ggplot2::coord_cartesian(xlim=c(poly_x_min, poly_x_max), ylim=c(poly_y_min, poly_y_max), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::annotation_raster(raster=raster, xmin=poly_x_min, xmax=poly_x_max, ymin=poly_y_min, ymax=poly_y_max, interpolate=interpolate)
		raster_adj_height=shape_adj_height 
		img_raster=magick::image_graph(width=width, height=raster_adj_height, bg="transparent", res=res, clip=FALSE)
		print(bggg)
		grDevices::dev.off()
		if (! is.null(raster_trim)) img_raster=ReSiZe_tO_stAndArd(x=magick::image_trim(img_raster, fuzz=raster_trim), standard=img_shape)
	} 
	if (cla_raster == "gg"){
		## DO NOT add theme_void, because it may have a wanted background fill color.
		raster$coordinates$expand=FALSE
		raster_panel_info=ggplot2::ggplot_build(raster)$layout$panel_params[[1]]
		raster_panel_min_x=raster_panel_info$x.range[1]
		raster_panel_max_x=raster_panel_info$x.range[2]		
		raster_panel_min_y=raster_panel_info$y.range[1]
		raster_panel_max_y=raster_panel_info$y.range[2]
		raster$theme$aspect.ratio=NULL
		if ("CoordFixed" %in% class(raster$coordinates)) raster=suppressMessages(raster <- raster+ggplot2::coord_cartesian(expand=FALSE, xlim=c(raster_panel_min_x, raster_panel_max_x), ylim=c(raster_panel_min_y, raster_panel_max_y)))
	
		raster_adj_height=shape_adj_height
		img_raster=magick::image_graph(width=width, height=raster_adj_height, bg="transparent", res=res, clip=FALSE)
		print(raster)
		grDevices::dev.off()
		if (! is.null(raster_trim)) img_raster=ReSiZe_tO_stAndArd(x=magick::image_trim(img_raster, fuzz=raster_trim), standard=img_shape)
	}
	if (cla_raster == "magick-image"){
		## DO NOT trim again, it is trimmed when checking raster class
		img_raster=ReSiZe_tO_stAndArd(x=raster, standard=img_shape)		
	}		

	# composite
	if (! is.null(result_trim)){
		comp=magick::image_trim(magick::image_composite(img_shape, img_raster, "in", "+0+0"), fuzz=result_trim)
	} else {
		comp=magick::image_composite(img_shape, img_raster, "in", "+0+0")
	}
	if (result == "magick"){
		comp
	} else {
		list(
			ggplot2::geom_blank(ggplot2::aes(x=c(xmin, xmax), y=c(ymin, ymax))), 
			# ggplot2::annotation_raster(raster=comp, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, interpolate=result_interpolate)
			# When using annotation_raster here, geom_text may fail combined with annotation_raster. So here use 
			# geom_multi_raster instead.
			geom_multi_raster(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, raster=list(grDevices::as.raster(comp))), interpolate=result_interpolate)
		)
	}
}
