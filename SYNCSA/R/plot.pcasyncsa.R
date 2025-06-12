#' @rdname pca
#' @encoding UTF-8
#' @export
plot.pcasyncsa <- function(x, show = c("variables", "individuals"), axis = c(1, 2), xlab = axis[1],
                         ylab = axis[2], arrows = TRUE, text = TRUE, points = FALSE, ...)
{
	if (length(show) > 1) {
		stop("\n Only one argument is accepted in show \n")
	}
	SHOW <- c("variables", "individuals")
	show <- pmatch(show, SHOW)
	if(show == 1){
		circle <- seq(0, 2*pi, length = 200)
		graphics::plot(cos(circle), sin(circle), type = 'l', col = "gray", xlab = xlab, ylab = ylab, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, ...)
		graphics::abline(h = 0, v = 0, lty = 3, col = "gray")
		if(arrows){
			graphics::arrows(0 ,0, x1 = x$variables[, axis[1]]*0.9, y1 = x$variables[, axis[2]]*0.9, length = 0.09, ...)
		}
		graphics::text(x$variables[,axis[1]], x$variables[,axis[2]], labels = rownames(x$variables), ...)
	}
	if(show == 2){
		graphics::plot(x$individuals[, axis[1]], x$individuals[, axis[2]], xlab = xlab, ylab = ylab, type = "n", asp = 1, ...)
		if(text){
			graphics::text(x$individuals[, axis[1]], x$individuals[, axis[2]], labels = rownames(x$individuals), ...)
		}
		if(points){
			graphics::points(x$individuals[, axis[1]], x$individuals[, axis[2]], ...)
		}
	}
}
