# Similar to heatmap()
# Author : Sylvain Mareschal <maressyl@gmail.com>
clusterize <- function(
	expr,                                  # heat.map()
	side = NULL,                           # heat.map()
	cex.col = NA,                          # heat.map()
	cex.row = NA,                          # heat.map()
	mai.left = NA,                         # heat.map()
	mai.bottom = NA,                       # heat.map()
	mai.right = 0.1,                       # heat.map()
	mai.top = 0.1,                         # heat.map()
	side.height = 1,                       # heat.map()
	side.col = NULL,                       # heat.map()
	side.srt = 0,                          # heat.map()
	side.cex = 1,                          # heat.map()
	col.heatmap = heat(),                  # heat.map()
	zlim = "0 centered",                   # heat.map()
	zlim.trim = 0.02,                      # heat.map()
	norm = c("rows", "columns", "none"),   # heat.map()
	norm.clust = TRUE,
	norm.robust = FALSE,                   # heat.map()
	customLayout = FALSE,                  # heat.map()
	getLayout = FALSE,                     # heat.map()
	plot = TRUE,
	widths = c(1, 4),
	heights = c(1, 4),
	order.genes = NULL,
	order.samples = NULL,
	fun.dist = dist.COR,
	fun.hclust = hclust.ward,
	clust.genes = NULL,
	clust.samples = NULL
	) {
	# Arg check
	norm <- match.arg(norm)
	
	# Layout
	if(isTRUE(plot) && !isTRUE(customLayout)) {
		if(!is.null(side) && ncol(side) > 0) {
			mat <- matrix(c(5,5,4,3,1,2), ncol=2)
			heights <- c(heights[1], lcm(ncol(side)*side.height), heights[2])
		} else {
			mat <- matrix(4:1, ncol=2)
		}
	} else {
		# No layout call
		mat <- as.integer(NA)
		heights <- as.integer(NA)
		widths <- as.integer(NA)
	}
	
	# Stop returning layout
	if(isTRUE(getLayout)) {
		return(list(mat=mat, heights=heights, widths=widths))
	}
	
	if(isTRUE(norm.clust)) {
		# Centering and scaling output heatmap (heatmap() uses norm.robust=FALSE)
		if(isTRUE(norm.robust)) {
			center <- median
			scale <- mad
		} else {
			center <- mean
			scale <- sd
		}
		if(norm == "columns")     { expr <- (expr - apply(expr, 1, center, na.rm=TRUE)) / apply(expr, 1, scale, na.rm=TRUE)         # samples
		} else if(norm == "rows") { expr <- t((t(expr) - apply(expr, 2, center, na.rm=TRUE)) / apply(expr, 2, scale, na.rm=TRUE))   # genes
		}
	}
	
	if(!isFALSE(clust.genes)) {
		# Perform clustering
		if(is.null(clust.genes)) clust.genes <- fun.hclust(fun.dist(t(expr)))
		
		# Coerce to dendrogram
		clust.genes <- as.dendrogram(clust.genes)
		
		# Custom tree reordering
		if(is.function(order.genes)) clust.genes <- order.genes(clust.genes, expr)
		
		# Synchronize tree and table orders ('side' will be ordered according to 'expr' row names in heat.map())
		expr <- expr[, labels(clust.genes) ]
	}
	
	if(!isFALSE(clust.samples)) {
		# Perform clustering
		if(is.null(clust.samples)) clust.samples <- fun.hclust(fun.dist(expr))
		
		# Coerce to dendrogram
		clust.samples <- as.dendrogram(clust.samples)
		
		# Custom tree reordering
		if(is.function(order.samples)) clust.samples <- order.samples(clust.samples, expr)
		
		# Synchronize tree and table orders ('side' will be ordered according to 'expr' row names in heat.map())
		expr <- expr[ labels(clust.samples) ,]
	}
	
	if(isTRUE(plot)) {
		# Layout call
		if(!isTRUE(customLayout)) {
			layout(mat=mat, heights=heights, widths=widths)
			on.exit(layout(1))
		}
		
		# Annotation and heatmap (middle right for annotation, bottom right for heatmap)
		out <- heat.map(
			expr = expr,
			customLayout = TRUE,
			cex.col = cex.col,
			cex.row = cex.row,
			mai.left = mai.left,
			mai.bottom = mai.bottom,
			mai.right = mai.right,
			mai.top = 0.1,
			side = side,
			side.height = side.height,
			side.col = side.col,
			side.srt = side.srt,
			side.cex = side.cex,
			col.heatmap = col.heatmap,
			zlim = zlim,
			zlim.trim = zlim.trim,
			norm = ifelse(isTRUE(norm.clust), "none", norm),
			norm.robust = norm.robust
		)
		
		# Sample tree (top right)
		par(mai=c(0, out$mai.left, mai.top, mai.right))
		if(isFALSE(clust.samples)) { plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
		} else                     { plot(clust.samples, leaflab="none", xaxs="i", yaxs="i", yaxt="n")
		}
		
		# Gene tree (bottom left)
		par(mai=c(out$mai.bottom, 0.1, 0.1, 0))
		if(isFALSE(clust.genes)) { plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
		} else                   { plot(clust.genes, leaflab="none", xaxs="i", yaxs="i", yaxt="n", horiz=TRUE)
		}
		
		# Legend (top+middle left)
		if(length(unlist(out$legend)) > 0) {
			par(mai=c(0.1, 0.1, mai.top, 0.1), xpd=NA)
			plot(x=NA, y=NA, xlim=0:1, ylim=0:1, xaxt="n", yaxt="n", xaxs="i", yaxs="i", bty="n", xlab="", ylab="")
			val <- paste(rep(names(rev(out$legend)), sapply(rev(out$legend), length)), unlist(lapply(rev(out$legend), names)), sep=" : ")
			col <- unlist(rev(out$legend))
			legend(x="topleft", legend=val, fill=col, bg="#EEEEEE")
		}
	} else {
		# No graphical parameter to return without plot
		out <- list()
	}
	
	# Invisibly return parameters for heatScale()
	out$genes <- clust.genes
	out$samples <- clust.samples
	invisible(out)
}

