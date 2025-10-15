#' Layer for Transparent Text
#' 
#' Suppose there is a colored rectangle
#' with some texts and 
#' you want the texts to be transparent so that 
#' the colors of the background can be seen. Now 
#' you can use this function. The function 
#' can be used as a ggplot layer or a generator 
#' of image. NOTE: when the function is 
#' used as a layer, it uses 
#' \code{ggplot2::annotation_raster} to 
#' do the drawing, so you must 
#' set limits for the x axis and the y axis. See examples.
#' 
#' @param label the text.
#' @param xmin the left side of the rectangle.
#' @param xmax the right side of the rectangle.
#' @param ymin the bottom side of the rectangle.
#' @param ymax the top side of the rectangle.
#' @param bg the colors of the rectangle. It can be 
#' a character vector of colors, a matrix of colors, 
#' an object of raster class or even a image 
#' read into R through \code{magick::image_read}.
#' Default is color black.
#' @param alpha it is only used 
#' when \code{bg} is a character 
#' vector. Default is 0.5.
#' @param operator the argument used by 
#' \code{magick::image_composite}. It should be 
#' "out" (default) or "in". The former makes the texts 
#' transparent, the latter creates shading texts.
#' @param interpolate when \code{bg} is 
#' a matrix, a image or 
#' a raster, this parameter is used and 
#' will be passed to \code{ggplot2::annotation_raster} 
#' to draw a colored rectangle. Default is TRUE.
#' @param result_interpolate whether to use interpolate 
#' in the final result. Default is TRUE.
#' @param expand sometimes 
#' it is needed to slightly expand the x position and 
#' y position to put the text so that they can be 
#' shown nicely. 
#' It should be two values used by x and y
#' respectively. Default is 0.05 and 0.05. 
#' @param family family of text. Default is SimHei 
#' which ensures that Chinese texts can be shown.
#' However, you can change it to others, 
#' e. g., sans, serif, mono.
#' @param fontface fontface.
#' @param reflow whether to change lines 
#' automatically. It will be passed to 
#' \code{ggfittext::geom_fit_text}. Default is FALSE.
#' @param place position adjustment used by 
#' \code{ggfittext:;geom_fit_text}.
#' The value is one of "center", "middle" (= "center"), 
#' "topleft", "top", "topright", "right", "bottomright", 
#' "bottom", "bottomleft", "left".
#' @param label_trim whether to trim \code{label}. 
#' The default is NULL which means no trimming. 
#' But if you want to remove all edges around label, 
#' you should give \code{label_trim} a value which will 
#' be passed to \code{magick::image_trim}. However, 
#' most of the time you do not need this parameter.
#' @param bg_trim whether to trim \code{bg}. Most 
#' of the time we do want to trim it. However, the 
#' \code{magick::image_trim} function sometimes 
#' trims wrongly. So you can turn it off. NOTE: the default 
#' value of \code{bg_trim} is NULL, which means 
#' DO NOT TRIM.
#' @param result when it is "layer", the function can be 
#' used as a ggplot layer. When it is "magick", the result 
#' is only an image which is created by the magick package.
#' @param width the width of 
#' the text rectangle. It will be passed 
#' to \code{magick::image_graph}. Most of the time you do 
#' not need to modify this. Default is 800.
#' @param height the height of the 
#' text rectangle. It will be passed 
#' to \code{magick::image_graph}. 
#' Default is NULL, which means 
#' it will be computed automatically. DO SEE Details below 
#' to learn how to handle this parameter.
#' @param res resolution in pixels which will be passed 
#' to \code{magick::image_graph}. Default is 72.
#' @param ... arguments which will be passed to 
#' \code{ggfittex::geom_fit_text}. Most often used are 
#' \code{angle} (0 to 360), \code{lineheight}.
#'
#' @details
#' \code{height} can be used in the 
#' following ways: 
#' \itemize{
#'   \item (1) an integer which will be 
#' directly passed to \code{magick::image_graph}.
#'   \item (2) a character-like integer, 
#' e.g., \code{height = "0.5"}. Suppose \code{width = 400}, 
#' the height that will be used is 400*0.5 = 200. 
#' This effectively prevents the image from becoming 
#' too large.
#'   \item (3) \code{height = "coord_fixed"}. 
#' the ratio between height and width will 
#' be (ymax-ymin)/(xmax-xmin).
#'   \item (4) \code{height = "image"}. the width and height 
#' will be the width and height of \code{bg} when 
#' the latter is a magick object.
#'   \item (5) \code{height = NULL}, the default. 
#' Now height is computed automatically. 
#' If \code{bg} is a magick object, the width 
#' and height of the image will be used. 
#' If \code{bg} is not a magick object, 
#' a ratio is computed first,  
#' ratio = (ymax-ymin)/(xmax-xmin). if the ratio is larger 
#' than 5 or smaller than 0.2, then height will be 
#' width*5 or width*0.2; else, the height will be treated 
#' in the same way as in (3) above. All these works 
#' are needed to prevent the image from becoming too large.
#' }
#'
#' @export
#' @import scales
#' @examples
#' \donttest{
#' # Example 1
#' m=matrix(rainbow(7), nrow=1)
#' ggplot()+coord_fixed()+
#' 	xlim(0, 7)+ylim(-2, 4)+theme_void()+
#' 	annotation_raster(
#' 		raster=m, 
#' 		xmin=0, ymin=-3, 
#' 		xmax=7, ymax=5, 
#' 		interpolate=TRUE
#' 	)+
#' 	annotation_transparent_text(
#' 		label="R\nDATA\nVISUALIZATION", 
#' 		xmin=0, xmax=7, 
#' 		ymin=-1, ymax=3, 
#' 		family="sans", fontface=2, alpha=0.8, 
#' 		place="left", expand=c(0.08, 0.02)
#' 	)
#' # 
#' # Example 2, this time the result is only an image.
#' tt=annotation_transparent_text(
#' 	label="abcdefg", 
#' 	xmin=1, xmax=8, 
#' 	ymin=1, ymax=4, 
#' 	alpha=0.6, 
#' 	result="magick"
#' )
#' #
#' # Example 3, the rectangle is a matrix.
#' m=colorRampPalette(c("yellow", "purple"))(10)
#' ggplot()+coord_fixed(expand=FALSE)+
#' 	theme(panel.background=element_rect(fill="red"))+
#' 	annotation_transparent_text(
#' 		label="hehehaha", 
#' 		xmin=1, xmax=8, 
#' 		ymin=1, ymax=4, 
#' 		bg=m, alpha=1
#' 	)
#' #
#' # Example 4, height is too large.
#' # Now you should explicitly set 
#' # width and height, otherwise, the 
#' # characters will become too flat.
#' x=c(0, 5, 10)
#' y=c(0, 500, 1000)
#' ggplot()+ylim(0, 4000)+
#' 	geom_point(aes(x, y))+
#' 	annotation_transparent_text(label="ha ha\nhe he", 
#'			xmin=0, xmax=10, ymin=1000, ymax=4000, bg="black", 
#' 		width=300, height=150
#' 	) # do not set height=NULL here
#' }
annotation_transparent_text=function(label, xmin, xmax, ymin, ymax, bg="black", alpha=0.5, operator="out", interpolate=TRUE, result_interpolate=TRUE, expand=c(0.05, 0.05), family="SimHei", fontface=1, reflow=FALSE, place="center", label_trim=NULL, bg_trim=NULL, result=c("layer", "magick"), width=800, height=NULL, res=72, ...){
	
	result=result[1]
	stopifnot(result %in% c("layer", "magick"))
	bg_class=class(bg)[1]
	if (grepl("magick", bg_class)) bg_class="magick-image"
	if ( ! bg_class %in% c("character", "matrix", "raster", "magick-image", "gg")) stop("bg must be a character vector, a matrix, a  raster, a image or a gg object.")
	if (bg_class == "matrix"){
		if (! is.character(bg)) stop("When bg is a matrix, its elements must be names of colors. ")
	}
	if (bg_class == "character") bg=matrix(scales::alpha(bg, alpha), nrow=1)
	if (bg_class == "magick-image"){
		if (! is.null(bg_trim)) bg=magick::image_trim(bg, fuzz=bg_trim)
	}

	# expand
	expand=rep(expand, length.out=2)
	xexpand=(xmax-xmin)*expand[1]
	yexpand=(ymax-ymin)*expand[2]
	XMIN=xmin-xexpand
	XMAX=xmax+xexpand
	YMIN=ymin-yexpand
	YMAX=ymax+yexpand
	
	# text
	tegg=ggplot()+
		ggplot2::coord_cartesian(xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX), expand=FALSE)+
		ggplot2::theme_void()+ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
		ggfittext::geom_fit_text(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label=label), reflow=reflow, grow=TRUE, family=family, fontface=fontface, place=place, padding.x=grid::unit(0, "mm"), padding.y=grid::unit(0, "mm"), ...)
		
	if (is.null(height)){
		if (bg_class == "magick-image"){
			magick_bg_info=as.numeric(magick::image_info(bg)[1, 2: 3])
			width=magick_bg_info[1]
			text_adj_height=magick_bg_info[2]	
		} else {
			decide_ratio=(YMAX-YMIN)/(XMAX-XMIN)
			if (decide_ratio<=5 & decide_ratio>=0.2) text_adj_height=floor(width*decide_ratio)
			if (decide_ratio>5) 	text_adj_height=width*5
			if (decide_ratio<0.2) text_adj_height=width*0.2
		}
	} else if (is.numeric(height)){
		text_adj_height=height
	} else if (is.character(height)){
		if (height=="coord_fixed"){
			text_adj_height=ADJUST_HEIGHT(W=width, XMIN=XMIN, XMAX=XMAX, YMIN=YMIN, YMAX=YMAX)
		} else if (height=="image"){
			if (bg_class != "magick-image") stop("Setting height = \"image\" is only used when bg is a magick-image object.")
			magick_bg_info=as.numeric(magick::image_info(bg)[1, 2: 3])
			width=magick_bg_info[1]
			text_adj_height=magick_bg_info[2]			
		} else {
			text_adj_height=width*as.numeric(height)
		}
	}
	
	## clip MUST BE TRUE, otherwise the text may dispear
	## clip MUST BE TRUE !
	## clip MUST BE TRUE !
	img_tegg=magick::image_graph(width=width, height=text_adj_height, bg="transparent", res=res, clip=TRUE)
	print(tegg)
	grDevices::dev.off()
	
	if (! is.null(label_trim)){
		text_info=as.numeric(magick::image_info(img_tegg)[1, 2: 3])
		img_tegg=magick::image_trim(img_tegg, fuzz=label_trim)
		img_tegg=magick::image_resize(img_tegg, paste(text_info[1], "x", text_info[2], "!", sep=""))
	}
	
	# bg
	if (bg_class %in% c("character", "matrix", "raster")){
		bggg=ggplot2::ggplot()+ggplot2::coord_cartesian(xlim=c(XMIN, XMAX), ylim=c(YMIN, YMAX), expand=FALSE)+
			ggplot2::theme_void()+
			ggplot2::theme(plot.background=ggplot2::element_rect(color=NA, fill="transparent"), plot.margin=unit(c(0, 0, 0, 0), "inch"))+
			ggplot2::annotation_raster(raster=bg, xmin=XMIN, ymin=YMIN, xmax=XMAX, ymax=YMAX, interpolate=interpolate)
		img_bggg=magick::image_graph(width=width, height=text_adj_height, bg="transparent", res=res, clip=FALSE)
		print(bggg)
		grDevices::dev.off()	
		if ( ! is.null(bg_trim)) img_bggg=ReSiZe_tO_stAndArd(x=magick::image_trim(img_bggg, bg_trim), standard=img_tegg)
	} else if (bg_class == "magick-image"){
		## already trimmed before
		img_bggg=ReSiZe_tO_stAndArd(x=bg, standard=img_tegg)
	} else if (bg_class == "gg"){
		bg$coordinates$expand=FALSE 
		
		# Delete coord_fixed and aspect.ratio
		panel_info=ggplot2::ggplot_build(bg)$layout$panel_params[[1]]
		panel_min_x=panel_info$x.range[1]
		panel_max_x=panel_info$x.range[2]		
		panel_min_y=panel_info$y.range[1]
		panel_max_y=panel_info$y.range[2]
		
		## MUST delete coord_fixed and aspect.ratio
		# delete the aspect.ratio element
		bg$theme$aspect.ratio=NULL
		# substitute coord_fixed with coord_cartesian
		if ("CoordFixed" %in% class(bg$coordinates)) bg=suppressMessages(bg <- bg+ggplot2::coord_cartesian(expand=FALSE, xlim=c(panel_min_x, panel_max_x), ylim=c(panel_min_y, panel_max_y)))
			
		img_bggg=magick::image_graph(width=width, height=text_adj_height, bg="transparent", res=res, clip=FALSE)
		print(bg)
		grDevices::dev.off()	
		if ( ! is.null(bg_trim)) img_bggg=ReSiZe_tO_stAndArd(x=magick::image_trim(img_bggg, bg_trim), standard=img_tegg)
	}
	
	# composite
	comp=magick::image_composite(img_tegg, img_bggg, operator=operator, offset="+0+0") # DO NOT TRIM # out
	if (result == "magick"){
		comp
	} else {
		list(
			ggplot2::geom_blank(ggplot2::aes(x=c(xmin, xmax), y=c(ymin, ymax))), 
			# ggplot2::annotation_raster(raster=comp, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, interpolate=result_interpolate)
			geom_multi_raster(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, raster=list(grDevices::as.raster(comp))), interpolate=result_interpolate)
		)
	}
}
	
ReSiZe_tO_stAndArd=function(x, standard){
	sinfo=as.numeric(magick::image_info(standard)[1, 2: 3])
	ssize=paste(sinfo[1], "x", sinfo[2], "!", sep="")
	magick::image_resize(x, ssize)
}

ADJUST_HEIGHT=function(W, XMIN, XMAX, YMIN, YMAX) floor(W*((YMAX-YMIN)/(XMAX-XMIN)))
