#' @title Calculate row and column statistics for a k-mer similarity matrix
#'
#' @description
#' The \code{kmeRs_statistics} function calculates basic statistics and returns the similarity matrix
#' with calculated results or summarized table with statistics only when margin.only
#' is set to TRUE
#'
#' @aliases kmeRs_statistics
#'
#' @param x Similarity matrix computed by \code{kmeRs_similarity_matrix}
#' @param margin.only Should only margin statistics be displayed? Defaults to \code{FALSE}
#' @param digits rounding digits, defaults to 2
#'
#' @return data.frame with results
#'
#' @examples
#' # Simple BLOSUM62 similarity matrix for DNA nucleotides
#' # Sample heptamers
#' q0 <- c("GATTACA", "ACAGATT", "GAATTAC", "GAAATCT", "CTATAGA", "GTACATA", "AACGATT")
#' # Compute similarity matrix 
#' example <- kmeRs_similarity_matrix(q0, submat = "BLOSUM62")
#' # Result as a full matrix
#' kmeRs_statistics(example)
#'
#' # Result a summary statistics table
#' kmeRs_statistics(example, margin.only = TRUE)
#'
#' @export

kmeRs_statistics <- function(x, margin.only = FALSE, digits = 2) {
	# Remove score column if present and calculate row stats
	x <- x[,!(colnames(x) %in% 'Sum')]
	x.srow <- dim(x)[1]
	x.scol <- dim(x)[2]
	
	Sum  <- apply(x, 1, sum, na.rm = TRUE)
	Min  <- apply(x, 1, min, na.rm = TRUE)
	Max  <- apply(x, 1, max, na.rm = TRUE)
	Mean <- apply(x, 1, mean, na.rm = TRUE)
	SD   <- apply(x, 1, stats::sd, na.rm = TRUE)	
	x <- cbind(x, Sum, Min, Max, Mean, SD)
	
	# Calculate the stats for cols and append
	Min  <- apply(x, 2, min, na.rm = TRUE)
	Max  <- apply(x, 2, max, na.rm = TRUE)
	Mean <- apply(x, 2, mean, na.rm = TRUE)
	SD   <- apply(x, 2, stats::sd, na.rm = TRUE)
	
	x <- rbind(x, t(data.frame(Min, Max, Mean, SD)))
	x <- round(x, digits = digits)
	
	# Display row & col margin statistics only
	if (margin.only) {
		x <- x[(x.srow + 1):dim(x)[1], (x.scol + 1):dim(x)[2]]
	}
	return(x)
}