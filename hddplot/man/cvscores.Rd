\name{cvscores}
\alias{cvscores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{For high-dimensional data with known groups, derive scores for plotting}
\description{This is designed to used with the output from
\code{cvdisc}.  Test and training scores from successive
cross-validation steps determine, via a principal components
calculation, a low-dimensional global space onto which test scores are
projected, in order to plot them.}
\usage{
cvscores(cvlist, nfeatures, ndisc = NULL, cl.other,
         x.other, keepcols = NULL, print.progress = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cvlist}{Output object from \code{cvdisc}}
  \item{nfeatures}{Number of features to use}
  \item{ndisc}{Dimension of space in which scores will be formed, at most
one less than the number of groups}
  \item{cl.other}{Classifies additional observations that are to be
projected onto the same low-dimensional space}
  \item{x.other}{Matrix from which additional observations will be taken}
  \item{keepcols}{Number of sets of principal component scores to use in
discriminant calculations and consequent evaluation of scores that will
determine the low-dimensional global space}
  \item{print.progress}{Set to \code{TRUE} (default) for printing out,
as calculations proceed, the number of the current fold}
}
\value{
  \item{scores}{Scores that can be plotted}
  \item{cl}{Factor that was used to classify observations into groups}
  \item{other.scores}{Other scores, if any, for plotting}
  \item{cl.other}{Factor that was used to classify the 'other' data into groups}
  \item{nfeatures}{Number of features used}
}
\references{J. H. Maindonald and C. J. Burden, 2005. Selection bias in plots of
microarray or other data that have been sampled from a high-dimensional space.
In R. May and A.J. Roberts, eds., \emph{Proceedings of 12th
  Computational Techniques and Applications Conference CTAC-2004}, volume 46,
  pp. C59--C74.

\url{https://journal.austms.org.au/V46/CTAC2004/Main/home.html} [March 15, 2005]}
\author{John Maindonald}
\note{The methodology used here has developed beyond that described in
Maindonald and Burden (2005)
}
\seealso{See also \code{\link{cvdisc}}, \code{\link{scoreplot}}}
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
detach(golubInfo)

## The function is currently defined as
function(cvlist, nfeatures, ndisc=NULL, cl.other, x.other,
           keepcols=NULL, print.progress=TRUE
           ){
    library(MASS)
    foldids <- cvlist$foldids
    nfold <- c(length(unique(foldids)), dim(foldids)[2])

    ugenes <- unique(as.vector(cvlist$genelist[1:nfeatures, ,]))
    df <- cvlist$xUsed[, ugenes]
    cl <- cvlist$cl
    if(!length(cl)==dim(df)[1])
      stop(paste("length(cl) =", length(cl),"does not equal",
                 "dim(cvlist$df)[1] =", dim(df)[1]))
    levnames <- levels(cl)
    if(is.null(ndisc))ndisc <- length(levnames)-1
    ngp <- length(levnames)
    nobs <- dim(df)[1]
    allscores <- array(0, dim=c(nrow=nobs, ncol=ndisc*nfold[1], nleaf=nfold[2]))
    if(!is.null(cl.other)){
      cl.other <- factor(cl.other)
      if(is.null(dim(x.other)))stop("x.other must have dimension 2")
      if(!length(cl.other)==dim(x.other)[2])
        stop(paste("length(cl.other) =", length(cl.other),"does not equal",
                   "dim(x.other)[2] =", dim(x.other)[2]))
      df.other <- data.frame(t(x.other[ugenes, ,drop=FALSE]))
      colnames(df.other) <- ugenes
    }
    else other.scores <- NULL
    for(k in 1:nfold[2]){
      foldk <- foldids[,k]
      ufold <- sort(unique(foldk))
      j <- 0
      for(i in ufold){
        j <- j+1
        if(print.progress)cat(paste(if(j>1) ":" else "", i,sep=""))
        testi <- (1:nobs)[foldk==i]
        traini <- (1:nobs)[foldk!=i]
        ntest <- length(testi)
        ntrain <- nobs-ntest
        genes.i <- cvlist$genelist[1:nfeatures, i, k]
        dfi <- as.data.frame(df[-testi, genes.i, drop=FALSE])
        newdfi <- as.data.frame(df[testi, genes.i, drop=FALSE])
        cli <- cl[-testi]
        xy.xda <- lda(cli~., data=dfi)
        allscores[, ((i-1)*ndisc)+(1:ndisc), k] <-
          predict(xy.xda, newdata=df, dimen=ndisc)$x
      }
    }
    cat("\n")
    dim(allscores) <- c(nobs, ndisc*prod(nfold))
    if(is.null(keepcols))keepcols <- min(nfeatures, dim(allscores)[2])
    allscores.pcp <- data.frame(pcp(allscores, varscores=FALSE)$g[, 1:keepcols])
    globals <- predict(lda(cl ~ ., data=allscores.pcp))$x[,1:ndisc]
    fitscores <- array(0, dim=c(nrow=nobs, ncol=ndisc, nleaf=nfold[2]))
    for(k in 1:nfold[2]){
      foldk <- foldids[,k]
      ufold <- sort(unique(foldk))
##      ntimes.genes <- table(cvlist$genelist[1:nfeatures,,k])
      av <- colMeans(df)
      j <- 0
      for(i in ufold){
        j <- j+1
        cat(paste(if (j>1) ":" else "", i,sep=""))
        testi <- (1:nobs)[foldk==i]
        traini <- (1:nobs)[foldk!=i]
        genes.i <- cvlist$genelist[1:nfeatures, i, k]
        dfi <- data.frame(df[-testi, genes.i, drop=FALSE])
        newdfi <- data.frame(df[testi, genes.i, drop=FALSE])
        cli <- cl[-testi]
        traini.xda <- lda(cli~., data=dfi)
        scorei <- predict(traini.xda)$x[,1:ndisc]
        newpred.xda <- predict(traini.xda, newdata=newdfi)
        scorei.out <- newpred.xda$x[, 1:ndisc, drop=FALSE]
        scorei.all <- globals[-testi, 1:ndisc]
        avcol <- colMeans(scorei.all)
        scorei.all <- sweep(scorei.all, 2, avcol,"-")
        avi <- colMeans(scorei)
        scorei <- sweep(scorei, 2, avi,"-")
        trans <- qr.solve(scorei, scorei.all)
        scorei.out <- sweep(scorei.out, 2, avi, "-")
        fitscores[testi, , k] <- sweep(scorei.out\%*\%trans, 2, avcol, "+")
      }
    }
    fitscores <- apply(fitscores, 1:2, mean)

    if(!is.null(cl.other)){
      Fmatrix <- cvlist$Fmatrix
      ord <- order(Fmatrix)[1:nfeatures]
      rowcol <- cbind(as.vector(row(Fmatrix))[ord],as.vector(col(Fmatrix))[ord])
      ugenes <- unique(as.vector(cvlist$genelist[rowcol]))
      df <- cvlist$xUsed[, ugenes]
      xy.xda <- lda(cl~., data=df)
      train.scores <- predict(xy.xda, dimen=ndisc)$x
      other.scores <- predict(xy.xda, newdata=df.other,
                              dimen=ndisc)$x
      avcol <- colMeans(globals)
      all.scores <- sweep(globals, 2, avcol,"-")
      av.train <- colMeans(train.scores)
      train.scores <- sweep(train.scores, 2, av.train, "-")
      trans <- qr.solve(train.scores, all.scores)
      other.scores <- sweep(other.scores\%*\%trans, 2, avcol, "+")
    }
    if(print.progress)cat("\n")
    invisible(list(scores=fitscores, cl=cl, other=other.scores,
                   cl.other=cl.other, nfeatures=nfeatures))
  }
}
\keyword{dplot}% at least one, from doc/KEYWORDS

