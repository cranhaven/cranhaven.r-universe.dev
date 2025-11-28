Linechart <- function(vars, groups, xticks=TRUE, xmarks=TRUE, mad=FALSE, pch=19, se.lwd=1, se.col=1, ...)
{
 if (!is.factor(groups)) stop("Grouping variable must be a factor")
 svars <- scale(vars)
 nvars <- ncol(vars)
 groups <- droplevels(groups)
 ngroups <- length(levels(groups))
 #
 if (mad) {
  centers <- aggregate(svars, list(groups), median, na.rm=TRUE); row.names(centers) <- centers$Group.1; centers <- as.matrix(centers[-1])
  starts <- as.matrix(aggregate(svars, list(groups), function(.x) median(.x, na.rm=TRUE)-mad(.x, na.rm=TRUE))[-1])
  ends <- as.matrix(aggregate(svars, list(groups), function(.x) median(.x, na.rm=TRUE)+mad(.x, na.rm=TRUE))[-1])
 } else {
  centers <- aggregate(svars, list(groups), function(.x) fivenum(.x)[3]); row.names(centers) <- centers$Group.1; centers <- as.matrix(centers[-1])
  starts <- as.matrix(aggregate(svars, list(groups), function(.x) fivenum(.x)[2])[-1])
  ends <- as.matrix(aggregate(svars, list(groups), function(.x) fivenum(.x)[4])[-1])
 }
 #
 oldpar <- par(xaxt="n")
 Dotchart1(centers, xlim=c(min(starts), max(ends)), pch=pch, ...)
 par(oldpar)
 yval <- rev(c(1:(nvars * (ngroups+2)))[c(rep(TRUE, ngroups), FALSE, FALSE)])
 yval <- unlist(lapply(split(yval, rep(1:nvars, each=ngroups)), rev))
 segments(starts, yval, ends, yval, lwd=se.lwd, col=se.col)
 if(xticks) axis(side=1, labels=xmarks)
 invisible(list(starts=starts, medians=centers, ends=ends))
}
