
# Helper function to do density plot (or lollipops) for 1 parameter

# Used by plot.mcmcOutput and densityPlot.

# mat : matrix of MCMC output with 1 column per chain
# plotArgs : list with plotting parameters
densPlot0 <- function(mat, plotArgs, ...)  {

  bw <- bw.nrd0(mat)
  # lollipops or density plot?
  # unik <- unique.default(mat)
  # if(length(unik) == 1) {
  if(bw == 0 || diff(range(mat)) < sqrt(.Machine$double.eps)) {
    plot(1, 1, type = "n", ann = FALSE, axes = FALSE)
    text(1,1, paste("All values are the same:\n", signif(mat[1], 4)))
  } else if(all(mat %% 1 == 0) && all(mat >= 0) && diff(range(mat)) < 50) {
    # "lollipops"
    t1 <- apply(mat+1, 2, tabulate, nbins=max(mat)+1)/nrow(mat) # +1 cos tabulate ignores 0
    if(min(mat) > 0)
      t1 <- t1[-(1:min(mat)), , drop=FALSE]
    ymax <- apply(t1, 1, max)
    xx <- min(mat):max(mat)
    xlim <- c(min(mat)-0.5, max(mat)+0.5)
    plot(xx, ymax, type='h', col='grey', xlim=xlim, xlab="")
    abline(h=0)
    abline(v=colMeans(mat), col=1:ncol(mat), lwd=2, lty=3)
    segments(x0 = rep(xx - 0.4, ncol(mat)),
             y0 = t1,
             x1 = rep(xx + 0.4, ncol(mat)),
             y1 = t1,
             col = col(t1))
  } else {
    # density plot
    dens <- densFold0(mat=mat, bw=bw, from=NA, to=NA,...)

    plotArgs$x <- dens$x
    plotArgs$y <- dens$y
    do.call(matplot, plotArgs)
    abline(h=0, col='grey')
    abline(v=colMeans(mat), col=1:ncol(mat), lwd=2, lty=3)
  }
}
# ..........................................................

