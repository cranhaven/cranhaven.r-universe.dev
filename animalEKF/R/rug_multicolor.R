#allow for rug with multicolor ticks
 rug_multicolor <- function(x, plot_side=3, ticksize=-0.04, col_vec=rep(1, length(x))) {
	   

	for (cc in unique(col_vec)) {
	 rug(x[ col_vec==cc], ticksize=ticksize, side=plot_side, col=cc, quiet=TRUE, xpd=FALSE)
	 }
	
}	