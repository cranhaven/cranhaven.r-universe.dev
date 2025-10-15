#' Geom Layer for Rectangle with Absolute Size
#'
#' This layer uses centimeter as unit to draw rectangles so 
#' that the size and shape will not be influenced by 
#' the coordinate systems 
#' (even when a polar system is used). 
#' 
#' Accepted properties are: 
#' \itemize{
#'   \item (1) \code{width} width in centimeter.
#'   \item (2) \code{height} height in centimeter.
#'   \item (3) \code{color} color of the outline.
#'   \item (4) \code{fill} color inside the shape.
#'   \item (5) \code{alpha} alpha of color and fill.
#'   \item (6) \code{size} line width of outline.
#'   \item (7) \code{linetype} line type.
#'   \item (8) \code{hjust} horizontal adjustment, 
#' default is 0.5 which means no adjustment.
#'   \item (9) \code{vjust} vertical adjustment, 
#' default is 0.5 which means no adjustment.
#'   \item (10) \code{x} x coordinates of middle points. 
#'   \item (11) \code{y} y coordinates of middle points.
#' }
#' 
#' @param mapping aes mapping.
#' @param data data.
#' @param stat stat.
#' @param position position.
#' @param na.rm logical, whether to remove NA values.
#' @param show.legend whether to show legend.
#' @param inherit.aes logical, whether to inherit aes from ggplot().
#' @param ... additional parameters.
#'
#' @export
#' @examples
#' library(ggplot2)
#' ggplot()+xlim(-0.5, 10.5)+
#' 	geom_rect_cm(aes(x=1: 10, y=rep(4, 10)), fill="red", height=rep(1: 2, each=5), 
#'			vjust=rep(c(0, 0.5), 5))+
#' 	geom_point(aes(x=1: 10, y=rep(4, 10)), color="green")
geom_rect_cm=function(mapping=NULL, data=NULL, stat="identity", position="identity", na.rm=FALSE, show.legend=NA, inherit.aes=TRUE, ...) {
	ggplot2::layer(
		geom=GeomRectCm, mapping=mapping,  data=data, stat=stat, 
		position=position, show.legend=show.legend, inherit.aes=inherit.aes,
		params=list(na.rm=na.rm, ...)
	)
}

GeomRectCm=ggplot2::ggproto("GeomRectCm", Geom,
	required_aes=c("x", "y"),
	default_aes=ggplot2::aes(width=1, height=1, colour="black", fill=NA, alpha=1, size=0.5, linetype=1, hjust=NULL, vjust=NULL),
	# hjust, vjust ignore unit setting in ggplot, say, vjust=unit(1, "cm") or unit(1, "mm")
	# vjust=0.5 is without move, and vjust=0 means an upward moving whose distance is half of the total rect height
	# do NOT use just as it sometimes is of length 2
	draw_key=ggplot2::draw_key_polygon, # use polygon's elements

	draw_panel=function(data, panel_params, coord) {
		coords<-coord$transform(data, panel_params)
		grid::rectGrob(coords$x, coords$y, width=unit(coords$width, "cm"), height=unit(coords$height, "cm"), 
			hjust=coords$hjust, vjust=coords$vjust, 
			gp=grid::gpar(col=coords$colour, fill=coords$fill, alpha=coords$alpha, lty=coords$linetype, lwd=coords$size*.pt) 
		)
	}
)
