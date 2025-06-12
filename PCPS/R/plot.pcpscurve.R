#' @rdname pcps.curve
#' @encoding UTF-8
#' @export
plot.pcpscurve<-function(x, draw.model = c("none", "ts", "bm"), type = "b", probs = c(0.025, 0.975), col = "black", model.col = "black", ...){
	if (length(draw.model) != 1) {
		stop("\n Only one argument is accepted in draw.model \n")
	}
	res <- summary.pcpscurve(x, probs = probs)
	graphics::plot(-1, -1, xlim = c(0,1), ylim = c(0,1), xlab = "Cumulative PCPS eigenvalues (%)", ylab = "Coefficient of determination (R2)")
	if(!is.null(res$null.model.ts) & draw.model=="ts"){
		graphics::points(res$null.model.ts[,1], res$null.model.ts[,5], col = model.col, type = "l")
		graphics::points(res$null.model.ts[,1], res$null.model.ts[,6], col = model.col, type = "l")
	}
	if(!is.null(res$null.model.bm) & draw.model=="bm"){
		graphics::points(res$null.model.bm[,1], res$null.model.bm[,5], col = model.col, type = "l")
		graphics::points(res$null.model.bm[,1], res$null.model.bm[,6], col = model.col, type = "l")
	}
	graphics::segments(0, 0, 1, 1, lty = 2)
	graphics::points(x$curve.obs[,1], x$curve.obs[,2], type = type, col = col, ...)
}