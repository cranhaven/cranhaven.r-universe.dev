\name{scoreplot}
\alias{scoreplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot discriminant function scores, with various identification}
\description{
There is provision for the plottting of two sets of scores on the same
graph, possibly with different classifying factors.  The function is
designed for use with output from \code{cvscores()} or from
\code{simulateScores()}. 
}
\usage{
scoreplot(scorelist, plot.disc = 1:2, xlab = NULL, ylab = NULL, params = NULL,
          circle = NULL, cl.circle = NULL, circle.pos = c(1, 1), adj.circle = 1,
          adj.title = 0.5, join.legends = TRUE, prefix.title = "", cex.title = 1,
          ratio = 1, plot.folds = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scorelist}{list, with elements \code{scores} (a matrix of scores)
\code{cl} (a classifying factor), \code{other} (optional, a further sets
of scores), \code{cl.other} (a a classifying factor for \code{other},
optional) and \code{nfeatures} (optional, used to label the graph)}
  \item{plot.disc}{choice of columns of \code{scorelist} to plot}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
  \item{params}{List, with optional elements (lists) \code{points},
\code{other}, \code{circle} and \code{legend}.  Allowed list elements
for \code{points} and \code{other} are \code{cex}, \code{lwd}, \code{pch}
and \code{col}. For \code{circle} they are \code{cex}, \code{lwd} and
\code{col}.  For \code{legend}, they are \code{cex} and \code{cex.other}}
  \item{circle}{identifies points that are to be circled}
  \item{cl.circle}{different colors may be used for different
points, according to levels of \code{cl.circle}}
  \item{circle.pos}{This is a vector of length 2, that specifies where
to place the legend information for the circling of points.
Possibilities are \code{c(0,0)} (left, below), \code{c(1,1)} (right, above),
etc.}
  \item{adj.circle}{controls positioning of circle legend}
  \item{adj.title}{controls positioning of title}
  \item{join.legends}{logical; should legends for \code{points} and
\code{other} be combined?}
  \item{prefix.title}{prefix, to place before title}
  \item{cex.title}{\code{cex} for title}
  \item{ratio}{\code{y}-scale to \code{x}-scale ratio for graph}
  \item{plot.folds}{Plot individual fold information, comparing projected
training scores with their projections onto the global space.  This is
not at present implemented}
\item{...}{Other parameters to be passed to \code{eqscplot()}}
}
\value{
A graph is plotted.
}
\author{John Maindonald}
\seealso{See also \code{\link{cvdisc}}, \code{\link{cvscores}}}
\examples{
## Use first 500 rows (expression values) of Golub, for demonstration.
data(Golub)
data(golubInfo)
attach(golubInfo)
miniG.BM <- Golub[1:500, BM.PB=="BM"]  # 1st 500 rows only
cancer.BM <- cancer[BM.PB=="BM"]
miniG.cv <- cvdisc(miniG.BM, cl=cancer.BM, nfeatures=1:10,
                    nfold=c(3,1))
miniG.scores <- cvscores(cvlist=miniG.cv, nfeatures=4,
                         cl.other=NULL)
subsetB <- (cancer=="allB") & (tissue.mf \%in\% c("BM:f","BM:m","PB:m"))
tissue.mfB <- tissue.mf[subsetB, drop=TRUE]
scoreplot(scorelist=miniG.scores, cl.circle=tissue.mfB,
       circle=tissue.mfB\%in\%c("BM:f","BM:m"),
       params=list(circle=list(col=c("cyan","gray"))),
       prefix="BM samples -")
detach(golubInfo)

## The function is currently defined as
  function(scorelist, plot.disc=1:2,
           xlab=NULL, ylab=NULL, params=NULL,
           circle=NULL, cl.circle=NULL, circle.pos=c(1,1),
           adj.circle=1,
           adj.title=0.5, join.legends=T, prefix.title="Golub data - ",
           cex.title=1.0, ratio=1, plot.folds=FALSE, ...){
    library(MASS)
    combine.params <-
      function(params=list(circle=list(col=c("cyan","gray")))){
        default.params=list(points=list(cex=1, lwd=1.25, pch=1:8, col=1:8),
          other=list(cex=0.65, lwd=1.25, pch=13:9, col=c(6:8,5:1)),
          circle=list(cex=2, lwd=1, pch=1.75, col="gray40"),
          legend=list(cex=1, cex.other=1))
        nam <- names(params)
        if(!is.null(nam))
          for(a in nam){
            nam2 <- names(params[[a]])
            for(b in nam2)default.params[[a]][[b]] <- params[[a]][[b]]
          }
        default.params
      }
    params <- combine.params(params=params)
    cl <- scorelist$cl
    cl.other <- scorelist$cl.other
    if(!is.null(cl.other)) cl.other <- factor(cl.other)
    nfeatures <- scorelist$nfeatures
    if(length(plot.disc)==2){
      n1 <- plot.disc[1]
      n2 <- plot.disc[2]
      if(is.null(xlab))xlab <- paste("Discriminant function", n1)
      if(is.null(ylab))ylab <- paste("Discriminant function", n2)
    } else stop("plot.disc must be a vector of length 2")
    if(!is.factor(cl))cl <- factor(cl)
    levnames <- levels(cl)
    fitscores <- scorelist$scores
    other.scores <- scorelist$other
    ngp <- length(levnames)
    n1lim <- range(fitscores[,n1])
    n2lim <- range(fitscores[,n2])
    if(!is.null(cl.other)){
      n1lim <- range(c(n1lim, other.scores[,n1]))
      n2lim <- range(c(n2lim, other.scores[,n2]))
      levnum <- unclass(cl.other)
      levnames.other <- levels(cl.other)
      intlev.other <- unclass(cl.other)
      ngp.other <- length(levels(cl.other))
    }
    n1 <- plot.disc[1]; n2 <- plot.disc[2]
    intlev <- unclass(cl)
    oldpar <- par(lwd=1)
    on.exit(par(oldpar))
    eqscplot(n1lim, n2lim, type="n",
             xlab=xlab, ylab=ylab, ratio=ratio, ...)
    with(params$points,
         points(fitscores[,n1], fitscores[,n2], col=col[intlev],
                pch=pch[intlev], cex=cex, lwd=lwd))
    if(!is.null(cl.other))
      with(params$other,
           points(other.scores[,n1], other.scores[,n2],
                  pch=pch[intlev.other],
                  col=col[intlev.other],
                  cex=cex, lwd=lwd))
    if(!is.null(cl.circle)){
      cl.circle <- factor(cl.circle[circle])
      lev.circle <- levels(cl.circle)
      with(params$circle,
           points(fitscores[circle, n1], fitscores[circle,n2], pch=pch,
                  cex=cex, col=col[unclass(cl.circle)], lwd=lwd))
    }
    par(xpd=TRUE)
    chw <- par()$cxy[1]
    chh <- par()$cxy[2]
    par(lwd=1.5)
    ypos <- par()$usr[4]
    xmid <- mean(par()$usr[1:2])
    top.pos <- 0
    mtext(side=3, line=(top.pos+1), paste(prefix.title,
            nfeatures, "features"), cex=cex.title, adj=adj.title)
    ypos.legend <- ypos+(top.pos-0.45)*chh*0.8

    if(join.legends&!is.null(cl.other)){
      leg.info <- legend(xmid, ypos.legend, xjust=0.5, yjust=0, plot=FALSE,
                         x.intersp=0.5, ncol=ngp, legend=levnames,
                         pt.lwd=params$points$lwd,
                         pt.cex=params$points$cex,
                         cex=params$legend$cex,
                         pch=params$points$pch)
      legother.info <- legend(xmid, ypos.legend, xjust=0.5, yjust=0,
                              plot=FALSE, x.intersp=0.5,
                              ncol=ngp.other, legend=levnames.other,
                              pt.lwd=params$other$lwd,
                              pt.cex=params$other$cex,
                              cex=params$legend$cex.other,
                              pch=params$other$pch)
      leftoff <- 0.5*legother.info$rect$w-0.5*chw
      rightoff <- 0.5*leg.info$rect$w+0.5*chw
      ypos.other <- ypos.legend
    }
    else {
      leftoff <- 0
      rightoff <- 0
      ypos.other <- ypos+(top.pos-1.5)*chh*0.8
    }
    legend(xmid-leftoff, ypos.legend, xjust=0.5, yjust=0,
           bty="n", pch=params$points$pch,
           x.intersp=0.5, col=params$points$col, ncol=ngp,
           legend=levnames,
           pt.lwd=params$points$lwd,
           pt.cex=params$points$cex,
           cex=params$legend$cex)
    par(lwd=1)
    if(!is.null(cl.other))
      lego.info <- legend(xmid+rightoff, ypos.other, xjust=0.5, yjust=0,
                          pch=params$other$pch, x.intersp=0.5,
                          col=params$other$col, ncol=ngp.other,
                          pt.lwd=params$other$lwd,
                          pt.cex=params$other$cex,
                          legend=levnames.other,
                          cex=params$legend$cex.other,
                          bty="n")
    if(!is.null(cl.other)&join.legends)
      text(lego.info$rect$left+c(0.4*chw,lego.info$rect$w-0.25*chw),
           rep(ypos.other,2)+0.8*chh, labels=c("(",")"),
           cex=params$legend$cex,
           lwd=params$legend$lwd, bty="n")
    par(lwd=params$circle$lwd)
    if(!is.null(cl.circle))if(lev.circle[1]!=""){
      pch.circle <- params$circle$pch
      xy <- par()$usr[circle.pos+c(1,3)]
      legend(xy[1], xy[2],
             xjust=adj.circle[1], yjust=circle.pos[2], bty="n", x.intersp=0.5,
             pch=rep(pch.circle,length(lev.circle)), col=params$circle$col,
             ncol=1, legend=lev.circle, cex=0.85, pt.cex=1.5)
    }
    par(lwd=1, xpd=FALSE)
    if(plot.folds){
      mtext(side=1, line=1.25, "Discriminant function 1", outer=T)
      mtext(side=2, line=1.25, "Discriminant function 2", outer=T)
    }
  }
}
\keyword{hplot}% at least one, from doc/KEYWORDS

