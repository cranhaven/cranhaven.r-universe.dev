
# Function to plot activity centres

plotACs <- function(
    object,           # mcmcOutput object with ACs attribute
    which=NA,         # which ACs to plot (don't usually want to do all in one plot)
    howMany=3000,     # number of points to plot for each animal
    showLabels=TRUE  # whether to label plot with animal IDs
  )  {

  # Check input
  if(!inherits(object, "mcmcOutput"))
    stop("Input object is not class 'mcmcOutput'.")
  ACs <- attr(object, "ACs")
  if(is.null(ACs))
    stop("Can't find Activity Centre information.")

  # Reduce number of iterations
  if(dim(ACs)[1] > howMany) {
    keep <- seq(1, dim(ACs)[1], length = howMany)
    ACs <- ACs[keep,,]
  }
  # Get posterior means for locations
  x <- colMeans(ACs[, ,1], na.rm=TRUE)
  y <- colMeans(ACs[, ,2], na.rm=TRUE)
  # Get number if animals captured
  ncaps <- sum(!is.na(colMeans(ACs[, , 1])))
  # Recover animal IDs
  M <- dim(ACs)[2] # total, incl. uncaptures
  animalIDs <- rep(NA, M)
  animalIDs[1:ncaps] <- dimnames(ACs)[[2]][1:ncaps]
  if(any(is.na(which)))
    which <- 1:M
  # do the plot
  MASS::eqscplot(x, y, type='n', xlim=range(ACs[, , 1], na.rm=TRUE),
    ylim=range(ACs[, , 2], na.rm=TRUE),
    ann=FALSE, axes=FALSE)

  traps <- attr(object, "traps")
  if(!is.null(traps))
    points(traps, pch=3, col='red')
  colors <- palette()[-1]
  colno <- 1
  for(i in which) {
    col <- colors[colno]
    points(ACs[, i, ], cex=0.1, col=adjustcolor(col, 0.3))
    colno <- colno+1
    if(colno > length(colors))
    colno <- 1
  }
  if(showLabels)
    plotrix::boxed.labels(x[which], y[which], labels=animalIDs[which])
  invisible(0)
}
