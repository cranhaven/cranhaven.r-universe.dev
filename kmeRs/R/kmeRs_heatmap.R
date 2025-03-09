#' @title K-mer similarity score heatmap
#'
#' @description
#' The \code{kmeRs_heatmap} function generates a heatmap from similarity score matrix
#'
#' @aliases kmeRs_heatmap
#'
#' @param x matrix calculated by \code{kmeRs_similarity_matrix} function
#' @param cexRow = NULL
#' @param cexCol = NULL
#' @param col color palette, when NULL the default palette is applied   
#' @param Colv when different from NA, the column dendrogram is shown
#' @param Rowv when different from NA, the row dendrogram is shown 
#' 
#' @return heatmap from results
#'
#' @examples
#' # Use RColorBrewer to generate a figure similar to publication
#' library(RColorBrewer)
#' h.palette <- rev(brewer.pal(9, "YlGnBu"))
#' q0 <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
#' example <- kmeRs_similarity_matrix(q0, submat = "BLOSUM62")
#' kmeRs_heatmap(kmeRs_score(example), col = h.palette)
#' @importFrom "grDevices" "colorRampPalette"
#' @importFrom "graphics" "legend"
#' @importFrom "stats" "heatmap"
#' 
#' @export
kmeRs_heatmap <- function(x, cexRow = NULL, cexCol = NULL, col = NULL, Colv=NA, Rowv=NA) {
	x.exclude <- c("Min", "Max", "Mean", "SD")
	x <- x[!(rownames(x) %in% x.exclude),]
	x <- x[,(!colnames(x) %in% x.exclude)]
	
	cex.row <- .2 + 1/log10(dim(x)[1])
	cex.col <- .2 + 1/log10(dim(x)[2])
	cex.all <- min(1, cex.row, cex.col)
	if (is.null(cexRow)) {
		cexRow <- cex.all
	}
	if (is.null(cexCol)) {
		cexCol <- cex.all
	}
	if (is.null(col)) {
		col <- colorRampPalette(c("blue4", "aquamarine3", "chartreuse3"))(256)
	}
	heatmap(as.matrix(x), scale = "none", col = col, cexRow = cexRow, cexCol = cexCol, Colv=Colv, Rowv=Rowv)
	# Plot a corresponding legend 
	legend(x="right", legend=c("min", "max"),fill=c((col)[1], (col)[length(col)]))
}