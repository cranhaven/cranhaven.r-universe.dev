
# Function to do kernel density fitting for a VECTOR
#   with folding for probability and non-negative values.

# This is now used for diagnostic plots with multiple chains in a matrix.

densityFolded <- function(x, bw = "nrd0", adjust = 1, from=NA, to=NA, ...) {
  name <- deparse(substitute(x))
  stopifnot(is.numeric(x))
  nx <- length(x)
  if (is.character(bw)) {  # this code from stats::density
    if (nx < 2)
        stop("need at least 2 points to select a bandwidth automatically")
    bw <- switch(tolower(bw), nrd0 = bw.nrd0(x), nrd = bw.nrd(x),
        ucv = bw.ucv(x), bcv = bw.bcv(x), sj = , `sj-ste` = bw.SJ(x,
            method = "ste"), `sj-dpi` = bw.SJ(x, method = "dpi"),
        stop("unknown bandwidth rule"))
  }
  bw <- bw * adjust

  # x must be a matrix
  if(is.null(dim(x)))
    x <- matrix(x, ncol=1)

  dens <- densFold0(x, bw=bw, from=from, to=to,...)
  dens$bw <- bw
  dens$n <- nx
  dens$call <- match.call()
  dens$data.name <- name
  if(dim(dens$y)[2] == 1) {
    dens$y <- as.vector(dens$y)
    class(dens) <- "density"
  }
  return(dens)
}
