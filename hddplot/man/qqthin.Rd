\name{qqthin}
\alias{qqthin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{a version of qqplot() that thins out points that overplot}
\description{
QQ-plots with large numbers of points typically generate graphics files
that are unhelpfully large.  This function handles the problem by
removing points that are, for all practical purposes, redundant
}
\usage{
qqthin(x, y, ends = c(0.01, 0.99), eps = 0.001, xlab = deparse(substitute(x)),
       adj.xlab = NULL, ylab = deparse(substitute(y)), show.line = TRUE,
       print.thinning.details=TRUE, centerline = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ordered values of \code{x} will be plotted on the x-axis}
  \item{y}{ordered values of \code{y} will be plotted on the y-axis}
  \item{ends}{outside these cumulative proportions of numbers of points,
all points will be included in the graph}
  \item{eps}{controls the extent of overplotting}
  \item{xlab}{label for x-axis}
  \item{adj.xlab}{positioning of x-label}
  \item{ylab}{label for y-axis}
  \item{show.line}{logical; show the line y=x?}
  \item{print.thinning.details}{logical; print number of points after thinning?}
  \item{centerline}{logical; draw a line though the part of the graph where
some points have been omitted?}
  \item{\dots}{additional graphics parameters}
}
\value{
Gives a qqplot. The number of points retained is returned invisibly.
}
\references{ ~put references to the literature/web site here ~ }
\author{John Maindonald}
\examples{
mat <- matrix(rnorm(1000), ncol=20)
cl <- factor(rep(1:3, c(7,9,4)))
Fstats <- aovFbyrow(x = mat, cl = cl)
qqthin(qf(ppoints(length(Fstats)), 2, 17), Fstats, eps=0.01)


## The function is currently defined as
function(x, y, ends=c(.01,.99), eps=0.001,
           xlab = deparse(substitute(x)), adj.xlab=NULL,
           ylab = deparse(substitute(y)), show.line=TRUE,
           print.thinning.details=TRUE,
           centerline=TRUE, ...){
    ## qqthin() is a substitute for qqplot(), that thins
    ## out plotted points from the region where they are
    ## dense.  Apart from the overlaid curve that shows
    ## the region where points have been thinned, it may
    ## be hard to distinguish the result of qqthin()
    ## from that of qqplot()
    xlab <- xlab
    ylab <- ylab
    x <- sort(x)
    y <- sort(y)
    dx<-diff(x)
    epsdist <- sqrt(diff(range(x))^2+diff(range(y))^2)*eps
    dx<-0.5*(c(dx[1],dx)+c(dx,dx[length(dx)]))
    dy<-diff(y)
    dy<-0.5*(c(dy[1],dy)+c(dy,dy[length(dy)]))
    dpoints <- epsdist/sqrt(dx^2+dy^2)
    ## dpoints is a local measure of the number of points
    ## per unit distance along the diagonal, with the unit
    ## set to approximately eps*(length of diagonal)
    dig<-floor(dpoints)+1
    ## dig is, roughly, the number of points per unit distance.
    ## We wish to retain one point per unit distance.  For this
    ## retain points where cdig rounds to an integer. For such
    ## points, cdig has increased by approx 1, relative to the
    ## previous point that is retained.
    cdig<-round(cumsum(1/dig))
    subs<-match(unique(cdig), cdig)
    if(is.null(adj.xlab))
    plot(x[subs], y[subs], xlab=xlab, ylab=ylab)
    else {
      plot(x[subs], y[subs], xlab="", ylab=ylab)
      mtext(side=1, xlab, adj=adj.xlab, line=par()$mgp[1])
    }
    if(any(diff(subs)>1)){
    n1 <- min(subs[c(diff(subs),0)>1])
    n2 <- max(subs[c(0,diff(subs))>1])
    ns1 <- match(n1, subs)
    ns2 <- match(n2, subs)
    if(print.thinning.details)
       print(paste("Graph retains", length(subs), "points."))
    if(centerline)
      lines(smooth.spline(x[subs[ns1:ns2]], y[subs[ns1:ns2]]),
            col="grey", lwd=2)
	    }
    if(show.line)abline(0, 1, col="red")
invisible(length(subs))
  }
}
\keyword{hplot}% at least one, from doc/KEYWORDS

