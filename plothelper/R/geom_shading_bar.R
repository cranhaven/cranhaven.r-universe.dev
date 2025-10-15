#' Geom Layer for Drawing Shading Barplot
#'
#' This function is similar to 
#' \code{geom_bar(aes(x, y), stat="identity")} 
#' except that it draws bars with shading colors. 
#' Unlike \code{gg_shading_bar} which 
#' is a convenient function, this function
#' is used as a ggplot layer. 
#' Accepted properties are different from those 
#' in \code{geom_multi_raster} and 
#' \code{gg_shading_bar}. 
#' \itemize{
#'   \item (1) \code{x}. It is the same as that in 
#' \code{geom_bar}.
#'   \item (2) \code{y}. It is the same as that in 
#' \code{geom_bar}.
#'   \item (3) \code{raster}. It should be 
#' a list with 1 or more character vectors of 
#' colors. If the list only has 1 vector, 
#' all the bars will use the same shading pattern. 
#' If you 
#' have, for example, 5 bars to draw, then you 
#' have to put 5 vectors of colors into a list.
#' If you use a data frame, it must be a 
#' data frame made by package tibble, and 
#' the column for \code{raster} should be 
#' a list.
#'   \item (4) \code{width}. It is the same 
#' as that in \code{geom_bar}.
#'   \item (5) \code{flip}. The default is FALSE. 
#' You only need to use TRUE when you 
#' use \code{coord_flip}. Use outside the 
#' \code{aes(...)} function.
#'   \item (6) \code{modify_raster}. If 
#' it is TRUE (default), colors 
#' will be smoothed using the value of \code{smooth}. 
#' If \code{raster} has enough colors, you can set 
#' this to FALSE. It is the same as that in 
#' \code{gg_shading_bar}.
#'   \item (7) \code{equal_scale}. The 
#' default is FALSE. When it is 
#' TRUE, a bar will use a certain part of the shading 
#' colors according to a global scale. It is the same 
#' as that in \code{gg_shading_bar}.
#'   \item (8) \code{smooth}. The 
#' default is 15. The number of 
#' shading colors each bar has. The bigger, the better.
#' It is the same as that in \code{gg_shading_bar}.
#'   \item (9) \code{space}. The color space that is 
#' used. It can be "rgb" (default) or "Lab".
#'   \item (10) \code{orientation}. This parameter 
#' mimics the same parameter used in 
#' \code{geom_bar}, though acts differently. 
#' This enables to flip the x axis and y axis without 
#' using \code{coord_flip}. If it is NA or "x" (default), 
#' it supposes x = SOME LABELS and y = SOME VALUES. 
#' If it is "y", you must set x = SOME VALUES and 
#' y = SOME LABELS. These effects are the same as 
#' \code{geom_bar}.
#' }
#' NOTE: the function does interpolation as default, so  
#' you does not need to 
#' use \code{interpolate} parameter.
#' And, unlike \code{gg_shading_bar}, this function 
#' does not draw lines around rectangles.
#' 
#' @param mapping aes mapping.
#' @param data data. It should be a tbl object. 
#' @param stat stat.
#' @param position position. The parameter will 
#' not be used here.
#' @param na.rm logical, whether to remove NA values.
#' @param show.legend This will not be used 
#' because the layer does not 
#' create any legend.
#' @param inherit.aes logical, 
#' whether to inherit aes from ggplot().
#' @param flip see description.
#' @param width see description. 
#' @param modify_raster see description 
#' or \code{gg_shading_bar}. 
#' @param smooth see description.
#' @param equal_scale see description
#' or \code{gg_shading_bar}.
#' @param space see description.
#' @param orientation see description.
#' @param ... additional parameters.
#' 
#' @export
#' @examples
#' # Example 1: use vectors.
#' x=c("b", "a", "c", "d", "e")
#' y=c(2, 1, 3, 5, 4)
#' raster=list(c("blue", "red"), c("green", "orange"), 
#' 	c("cyan", "yellow"), c("purple", "orangered"), c("grey", "red"))
#' ggplot()+
#' 	geom_shading_bar(aes(x=x, y=y, raster=raster), smooth=40)
#' #
#' # Example 2: other parameters
#' x=1: 5
#' y=c(1, 2, -3, 5, 4)
#' raster=list(c("blue", "red"))
#' ggplot()+
#' 	geom_shading_bar(aes(x=x, y=y, raster=raster), 
#' 		smooth=50, width=0.6, equal_scale=TRUE)+
#' 	scale_x_continuous(breaks=1: 5, labels=letters[1: 5])
geom_shading_bar=function(mapping=NULL, data=NULL,
                     stat="identity", position="identity", na.rm=FALSE, 
					 show.legend=NA, inherit.aes=TRUE,
					 width=0.9, 
					 flip=FALSE, 
					 modify_raster=TRUE, 
					 smooth=15, 
					 equal_scale=FALSE, 
					 space="rgb", 
					 orientation="x", 
                     ...) {
  ggplot2::layer(
    data=data,
    mapping=mapping,
    stat=stat,
    geom=GeomShadingBar,
    position=position,
    show.legend=show.legend,
    inherit.aes=inherit.aes,
    params=list(
      width=width,
	  flip=flip, 
	  modify_raster=modify_raster, 
	  smooth=smooth, 
	  equal_scale=equal_scale, 
	  space=space, 
      na.rm=na.rm,
	  orientation=orientation, 
      ...
    )
  )
}

GeomShadingBar=ggplot2::ggproto("GeomShadingBar", GeomMultiRaster,
	required_aes=c("x", "y", "raster"),
	default_aes=c(interpolate=TRUE), 
	# both GeomMultiRaster and GeomShadingBar must add default_aes to let GeomShadingBar know interpolate
	non_missing_aes = c("raster", "xmin", "xmax", "ymin", "ymax"),

	setup_data=function(data, params){
		width=params$width
		# data$width <- data$width %|borrow|% params$width %|borrow|% (ggplot2::resolution(data$x, FALSE) * 0.9)
		flip=params$flip
		modify_raster=params$modify_raster
		smooth=params$smooth
		equal_scale=params$equal_scale 
		space=params$space
		
		orientation=params$orientation # added 2020-03-08
		if (is.na(orientation)) orientation="x"
		if (orientation != "x"){
			flip=TRUE
			raw_data=if (equal_scale == TRUE ) data$x else NULL
		} else {
			raw_data=if (equal_scale == TRUE ) data$y else NULL
		}
			
		data$raster=if (modify_raster==TRUE) transform_raster_list(data$raster, smooth=smooth, equal_scale=equal_scale, raw_data=raw_data, SPACE=space) else data$raster
		data$raster=if (flip==FALSE) lapply(data$raster, FUN=function(x) grDevices::as.raster(rev(x))) else lapply(data$raster, FUN=function(x) matrix(x, nrow=1))
		
		if (orientation=="x"){
			transform(data,
				ymin=pmin(y, 0), ymax=pmax(y, 0),
				xmin=x-width/2, xmax=x+width/2
			)
		} else {
			transform(data,
				ymin=y-width/2, ymax=y+width/2,
				xmin=pmin(x, 0), xmax=pmax(x, 0)
			)
		}
	},

	draw_panel = function(self, data, panel_params, coord, width = 0.9, flip=FALSE, modify_raster=TRUE, smooth=15, equal_scale=FALSE, space="rgb", orientation="x") {
		ggplot2::ggproto_parent(GeomMultiRaster, self)$draw_panel(data, panel_params, coord)
	}
)

# "%|borrow|%" <- function(a, b) {
#   if (!is.null(a)) a else b
# }
