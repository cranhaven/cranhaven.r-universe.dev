#' plot the surface under the cumulative ranking curve (SUCRA)
#' @param x the output model from fitting a network meta analysis/regression model
#' @param legend.position the position of the legend that will be passed onto ggplot
#' @param ... additional arguments for plot
#' @return No return value
#' @importFrom grDevices devAskNewPage rgb
#' @importFrom graphics axis lines plot
#' @importFrom ggplot2 ggplot geom_line aes labs theme_gray theme ylab
#' @importFrom gridExtra grid.arrange
#' @export
"plot.sucra" <- function(x, legend.position = "none", ...) {
	nT <- length(x$SUCRA)
	cumeffectiveness <- apply(x$rankprob, 2, cumsum)
	names <- x$names
	gglist <- vector(mode = "list", nT)
	for (TRT in 1:nT) {
		Area=round(x$SUCRA[TRT], 3)
		ddd <- data.frame(trt = names, CDF = cumeffectiveness[,TRT], PDF = x$rankprob[,TRT], stringAsFactors = FALSE)
		ddd$trt <- factor(ddd$trt, levels = ddd$trt)
		bb <- ggplot(ddd, aes(x = trt, group = 1)) +
			 geom_line(aes(y = CDF), color = "#eab159", size = 1) +
			 geom_line(aes(y = PDF), color = rgb(0, 157, 114, maxColorValue = 255), linetype = "twodash", size = 1) +
			 theme_gray() + theme(legend.position = legend.position) +
			 ylab("Probability") + labs(title = paste0("Trt (", names[TRT], "): ", Area))
		gglist[[TRT]] <- bb
	}
	# on.exit(devAskNewPage(oask))
	do.call(grid.arrange, gglist)
	invisible(gglist)
}
