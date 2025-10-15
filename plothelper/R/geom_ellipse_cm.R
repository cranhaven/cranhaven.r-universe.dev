#' Geom Layer for Ellipse with Absolute Size
#'
#' This layer uses centimeter as unit to draw ellipse so 
#' that its size and shape will not be influenced by 
#' the coordinate systems 
#' (even when a polar system is used). 
#' 
#' Accepted properties are: 
#' \itemize{
#'   \item (1) \code{rcm} radius in centimeter.
#'   \item (2) \code{ab} it means to what extent 
#' radius a of an ellipse is larger than radius b.
#' However, its true meaning is the aspect ratio which 
#' is used by \code{gridExtra::ellipseGrob} and indicates 
#' the extent to which y dimension is flattened. So, say, 
#' when \code{ab = 2}, radius a is larger than b, but it is 
#' not exactly 2 times larger. 
#'   \item (3) \code{color} color of the the outline.
#'   \item (4) \code{fill} color inside the shape.
#'   \item (5) \code{alpha} alpha of color and fill.
#'   \item (6) \code{size} line width of the outline.
#'   \item (7) \code{linetype} line type.
#'   \item (8) \code{angle} angle of rotation from 0
#' degree and in anti-clockwise direction.
#'   \item (9) \code{n} the number of points to 
#' draw the shape. Note: it must be written inside 
#' the \code{aes(...)} function.
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
#' @import gridExtra
#' @examples
#' library(ggplot2)
#' dat=data.frame(x=c(1, 3, 5, 7, 9), y=rep(5, 5)) 
#' ggplot(dat)+xlim(0, 11)+ylim(1, 9)+
#' 	geom_ellipse_cm(aes(x=x, y=y), fill="red", ab=seq(1, 4, length.out=5))  
#' ggplot(dat)+xlim(0, 11)+ylim(1, 9)+
#' 	geom_ellipse_cm(aes(x=x, y=y, fill=factor(x)), ab=3, angle=c(0, pi/4, pi/3, pi/2, 0.75*pi))
geom_ellipse_cm=function(mapping=NULL, data=NULL, stat="identity", position="identity", na.rm=FALSE, show.legend=NA, inherit.aes=TRUE, ...) {
	ggplot2::layer(
		geom=GeomEllipseCm, mapping=mapping,  data=data, stat=stat, 
		position=position, show.legend=show.legend, inherit.aes=inherit.aes,
		params=list(na.rm=na.rm, ...)
	)
}

GeomEllipseCm=ggplot2::ggproto("GeomEllipseCm", Geom,
	required_aes=c("x", "y"),
	default_aes=ggplot2::aes(rcm=1, ab=1, angle=0, n=40, colour="black", fill=NA, alpha=1, size=0.5, linetype=1),
	draw_key=ggplot2::draw_key_polygon, # use polygon's elements
	
	# ab is passed to ar, which means to what extent a, the r parallel to x axis, is larger than b, the r parallel to y axis.
	# but it cannot exactly control how many time a is larger than b.
	# This can be seen in: 
	# grid.ellipse(0.5, 0.5, size = 1, angle = 0, ar = 1, size.units = "cm")
	# grid.ellipse(0.5, 0.5, size = 1, angle = 0, ar = 2, size.units = "cm")
	# the 2nd circle is not so large as expected.
	
	# angle: the default value is pi/4, here it is set to 0
	# but in ellipseGrob, angle controls clockwise rotation,
	# so here in draw_panel a negative sign is added to make 
	# it rotate anti-clockwise.

	draw_panel=function(data, panel_params, coord) {
		coords<-coord$transform(data, panel_params)
		gridExtra::ellipseGrob(
			coords$x, coords$y, 
			size=coords$rcm, size.units = "cm", ar=coords$ab, angle = -coords$angle, n=coords$n,   
			gp=grid::gpar(col=coords$colour, fill=coords$fill, alpha=coords$alpha, lwd=coords$size*.pt, lty=coords$linetype) 
		)
	}
)
