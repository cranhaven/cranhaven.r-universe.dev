
# Function to extract the biggest cross-correlations from an mcmcOutput object

bigCrosscorr <- function(x, big = 0.6, digits=3) {
  # Function to extract the biggest cross-correlations
  #  from an mcmcOutput object.
  #
  # big : only values outside the range -big to +big will be returned
  # digits : number of digits to return.
  #
  # Returns a data frame with 3 columns, for the names of the two parameters and
  #  the correlation coefficient.

  x <- try(mcmcOutput(x), silent=TRUE)
  if(inherits(x, "try-error"))
    stop("Sorry, can't convert your input to a mcmcOutput object.", call.=FALSE)
  if(ncol(x) < 2)
    stop("Need at least 2 columns to do cross-correlations.", call.=FALSE)

  xcor <- try(suppressWarnings(cor(x)), silent=TRUE)
  if(inherits(xcor, "try-error"))
    stop("Sorry, can't extract a correlation matrix from your input.", call.=FALSE)
  xcor[lower.tri(xcor, diag=TRUE)] <- 0
  BIG <- which(abs(xcor) > big, arr.ind=TRUE)
  if(nrow(BIG) == 0) {
    message(paste0("No cross-correlations exceed ", big,
        "; the output data frame has 0 rows."))
  }
  nms <- rownames(xcor)
  tmp <- data.frame(node1=nms[BIG[, 1]], node2=nms[BIG[, 2]],
      corr=round(xcor[BIG], digits), stringsAsFactors=FALSE)
  ord <- order(abs(tmp$corr), decreasing = TRUE)
  out <- tmp[ord, ]
  rownames(out) <- NULL
  return(out)
}

