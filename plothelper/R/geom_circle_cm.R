#' Geom Layer for Circle with Absolute Size
#'
#' This layer uses centimeter as unit to draw circles so 
#' that the size and shape will not be influenced by 
#' the change of the coordinate systems 
#' (even when a polar system is used). 
#' Note: this function does not have 
#' \code{linetype} and \code{n} arguments.
#' 
#' Accepted properties are: 
#' \itemize{
#'   \item (1) \code{rcm} radius in centimeter.
#'   \item (2) \code{color} color of the outline.
#'   \item (3) \code{fill} color inside the shape.
#'   \item (4) \code{alpha} alpha of color and fill.
#'   \item (5) \code{size} line width of the outline.
#'   \item (6) \code{x} x coordinates of the middle points. 
#'   \item (7) \code{y} y coordinates of the middle points.
#' }
#' 
#' @param mapping aes mapping.
#' @param data data.
#' @param stat stat.
#' @param position position.
#' @param na.rm logical, whether to remove NA values.
#' @param show.legend whether to show legend.
#' @param inherit.aes logical, whether to inherit aes from ggplot().
#' @param linetype should always be NULL.
#' because it will not be used.
#' @param ... additional parameters.
#'
#' @export
#' @import ggplot2
#' @import grid
#' @examples
#' library(ggplot2)
#' dat=data.frame(x=1: 10, y=rep(5, 10), R=rep(c(0.5, 1), 5))
#' ggplot(dat)+xlim(0, 11)+ylim(1, 9)+
#' 	geom_circle_cm(aes(x=x, y=y, fill=factor(R)), rcm=dat$R, alpha=0.5)
geom_circle_cm=function(mapping=NULL, data=NULL, stat="identity", position="identity", na.rm=FALSE, show.legend=NA, inherit.aes=TRUE, linetype=NULL, ...) {
	if (! is.null(linetype)) message("This function does NOT use linetype and the reason is that grid::circleGrob does not use lty.")
	ggplot2::layer(
		geom=GeomCircleCm, mapping=mapping,  data=data, stat=stat, 
		position=position, show.legend=show.legend, inherit.aes=inherit.aes,
		params=list(na.rm=na.rm, ...)
	)
}

GeomCircleCm=ggplot2::ggproto("GeomCircleCm", Geom,
	required_aes=c("x", "y"),
	default_aes=ggplot2::aes(rcm=1, colour="black", fill=NA, alpha=1, size=0.5),
	draw_key=ggplot2::draw_key_polygon, # use polygon's elements

	draw_panel=function(data, panel_params, coord) {
		coords<-coord$transform(data, panel_params)
		grid::circleGrob(coords$x, coords$y, r=unit(coords$rcm, "cm"), 
			gp=grid::gpar(col=coords$colour, fill=coords$fill, alpha=coords$alpha, lwd=coords$size*.pt) # NO lty because grid.circle fails do use this
		)
	}
) 
 