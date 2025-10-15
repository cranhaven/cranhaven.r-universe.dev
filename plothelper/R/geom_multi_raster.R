#' Geom Layer for Drawing Multiple Rasters
#'
#' Unlike \code{annotation_raster} which 
#' draws only 1 raster, this layer 
#' draws one or more rasters at the same time. 
#' The data must be a tbl object created by 
#' package tibble and the reason is that, 
#' as we must give each rectangle a 
#' vector of colors, the column that 
#' contains these vectors of colors must 
#' be a list rather than a vector. A list can 
#' be a column for tbl object, not for 
#' a normal data frame. See examples.
#' Accepted properties are: 
#' \itemize{
#'   \item (1) \code{xmin}.
#'   \item (2) \code{xmax}.
#'   \item (3) \code{ymin}.
#'   \item (4) \code{ymax}.
#'   \item (5) \code{raster}. a list with 1 or more 
#' rasters. If you have only 1 raster, you also 
#' have to put it into a list. 
#' Each raster should be 
#' a matrix, a raster object, a character vector 
#' or a magick object read into R by 
#' \code{magick::image_read}. 
#' You can also use a data frame created by 
#' package tibble to combine
#' \code{xmin, xmax, ymin, ymax, raster}.
#'   \item (6) \code{interpolate}. It is the same 
#' as that in \code{annotation_raster} except 
#' that the default value is TRUE. It can be 
#' used either inside or outside 
#' the \code{aes(...)} function. Its length must be 
#' either 1 or the same as the number of 
#' rasters.
#'   \item (7) \code{flip}. The default is FALSE. 
#' You only need to use TRUE when you 
#' use \code{coord_flip}. Used outside the 
#' \code{aes(...)} function.
#' }
#' 
#' @param mapping aes mapping.
#' @param data data. It should be a tbl object.
#' @param stat stat.
#' @param position position.
#' @param na.rm logical, whether to remove NA values.
#' @param show.legend This will not be used 
#' because the layer does not 
#' create any legend.
#' @param inherit.aes logical, 
#' whether to inherit aes from ggplot().
#' @param flip see description.
#' @param ... additional parameters.
#'
#' @export
#' @examples
#' # Example 1: use vectors and a list. 
#' mycolor=list(
#' 	c1=matrix(c("red", "blue", "green", "yellow"), nrow=2), 
#' 	c2=matrix(c("green", "yellow")), 
#' 	c3=matrix(c("purple", "red")))
#' xmin=1: 3
#' xmax=(1: 3)+0.8 
#' ymin=c(0, 1, 2)
#' ymax=c(1, 3, 5)
#' ggplot()+
#' 	geom_multi_raster(aes(xmin=xmin, xmax=xmax, 
#' 	ymin=ymin, ymax=ymax, raster=mycolor))
#' #
#' # Example 2: the same as example 1
#' # except flip=TRUE.
#' ggplot()+coord_flip()+
#' 	geom_multi_raster(aes(xmin=xmin, xmax=xmax, 
#' 	ymin=ymin, ymax=ymax, raster=mycolor), flip=TRUE)
geom_multi_raster=function(
	mapping=NULL, data=NULL, stat="identity", position="identity", 
	na.rm=FALSE, show.legend=NA, inherit.aes=TRUE, 
	flip=FALSE, 
	...) {
	ggplot2::layer(
		geom=GeomMultiRaster, mapping=mapping,  data=data, stat=stat, 
		position=position, show.legend=show.legend, inherit.aes=inherit.aes,
		params=list(flip=flip, na.rm=na.rm, ...)
	)
}

GeomMultiRaster=ggplot2::ggproto("GeomMultiRaster", Geom,
	required_aes=c("raster", "xmin", "xmax", "ymin", "ymax"),
	default_aes=c(interpolate=TRUE), 
	# both GeomMultiRaster and GeomShadingBar must add default_aes to let GeomShadingBar know interpolate
	draw_key=NULL,  
	
	draw_panel=function(data, panel_params, coord, na.rm=FALSE, flip=FALSE) {
		coords=coord$transform(data, panel_params)
		MIDX=(coords$xmax+coords$xmin)/2
		MIDY=(coords$ymax+coords$ymin)/2
		WIDTH=coords$xmax-coords$xmin
		HEIGHT=coords$ymax-coords$ymin
		if (! is.list(coords$raster)) stop("raster must be a list. If you have only one matrix, say, M, you must put it in a list, that is, list(M). ")

		MAPPLY_GROB=mapply(
			grid::rasterGrob, 
			image=if (flip==FALSE) plyr::llply(coords$raster, .fun=CHECK_RASTER_TIBBLE) else plyr::llply(coords$raster, .fun=CHECK_RASTER_TIBBLE_FLIP), 
			x=MIDX, y=MIDY, 
			width=WIDTH, height=HEIGHT, default.units="native", 
			interpolate=coords$interpolate, 
			SIMPLIFY=FALSE
		)
		MAPPLY_GROB=do.call(grid::gList, MAPPLY_GROB)
		MAPPLY_GROB
		#gTree(children=MAPPLY_GROB) 
	}
)

CHECK_RASTER_TIBBLE=function(X) if (is.null(X)) stop("Rasters must not be NULL or NA.") else if (grDevices::is.raster(X) | is.matrix(X)) X else grDevices::as.raster(X)
		
CHECK_RASTER_TIBBLE_FLIP=function(X){
	if (is.null(X)){
		stop("Rasters must not be NULL or NA.")
	} else if (grDevices::is.raster(X)){
		t(X[nrow(X): 1, ])
	} else if (grepl("magick", class(X)[1])){
		magick::image_rotate(X, degrees=90)
	} else {
		X=grDevices::as.raster(X)
		t(X[nrow(X): 1, ])
	}
}	
	