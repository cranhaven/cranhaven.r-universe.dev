\name{cvdisc}
\alias{cvdisc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross-validated accuracy, in linear discriminant calculations}
\description{
Determine cross-validated accuracy, for each of a number of features in
a specified range, with feature selection repeated at each step of the
cross-validation.
}
\usage{
cvdisc(x, cl, nfold = c(10,1), test = "f", nfeatures = 2, seed = 31,
       funda = lda, print.progress = TRUE, subset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Matrix; rows are features, and columns are observations
('samples')}
  \item{cl}{Factor that classifies columns into groups}
  \item{nfold}{Number of folds for the cross-validation. Optionally, a second
number species the number of repeats of the cross-validation.
}
  \item{test}{What statistic will be used to measure separation between
groups? Currently \code{"f"} is the only possibility.}
  \item{nfeatures}{Specifies the different numbers of features (e.g., 1:10)
that will be tried, to determine cross-validation accuracy in each
instance}
  \item{seed}{This can be used to specify a starting value for the random
number generator, in order to make calculations repeatable}
  \item{funda}{Function that will be used for discrimination.  Currently
\code{lda} is the only option}
  \item{print.progress}{Set to \code{TRUE} (default) for printing out,
as calculations proceed, the number of the current fold}
  \item{subset}{Allows the use of a subset of the samples (observations)}
}
\value{
  \item{folds}{Each column gives, for one run of the cross-validation,
numbers that identify the \code{nfold} distinct folds of the
cross-validation}
  \item{xUsed}{returns the rows of \code{x} that were used,
in at least one fold}
  \item{cl}{Factor that classifies columns into groups}
  \item{acc.cv}{Cross-validated accuracy}
  \item{genelist}{Array: \code{max(nfeatures)} by number of folds by
number of repeats, identifying the features chosen at each repeat of
each fold.  (for \code{k} < \code{max(nfeatures)} features, take the
initial \code{k} rows}
  \item{Fmatrix}{Array, with the same dimensions as \code{genelist},
that gives the anova F-statistic when that feature is used on its own
to separate groups}
  \item{nfeatures}{Specifies the different numbers of features that were
tried, to determine cross-validation accuracy in each instance}
}
\author{John Maindonald}
\seealso{See also \code{\link{cvscores}}, \code{\link{scoreplot}}}
\examples{
## Use first 500 rows (expression values) of Golub, for demonstration.
data(Golub)
data(golubInfo)
attach(golubInfo)
miniG.BM <- Golub[1:500, BM.PB=="BM"]  # 1st 500 rows only
cancer.BM <- cancer[BM.PB=="BM"]
miniG.cv <- cvdisc(miniG.BM, cl=cancer.BM, nfeatures=1:10,
                    nfold=c(3,1))
## Plot cross-validated accuracy, as a function of number of features
plot(miniG.cv$acc.cv, type="l")


## The function is currently defined as
function(x, cl, nfold=NULL, test="f",
           nfeatures=2, seed=31, funda=lda, print.progress=TRUE,
           subset=NULL){
    ## If nfold is not specified, use leave-one-out CV
    if(is.null(nfold))nfold <- sum(!is.na(cl))
    ## Option to omit one or more points
    if(!is.null(subset)){cl[!is.na(cl)][!subset] <- NA
                         nfold[1] <- min(nfold[1], sum(!is.na(cl)))
                       }
    if(any(is.na(cl))){x <- x[,!is.na(cl)]
                       cl <- cl[!is.na(cl)]
                     }
    if(length(nfold)==1)nfold <- c(nfold,1)
    cl <- factor(cl)
    ngp <- length(levels(cl))
    genes <- rownames(x)
    nobs <- dim(x)[2]
    if(is.null(genes)){
      genes <- paste(1:dim(x)[1])
      print("Input rows (features) are not named. Names")
      print(paste(1,":", dim(x)[1], " will be assigned.", sep=""))
      rownames(x) <- genes
    }
    require(MASS)
    if(!is.null(seed))set.seed(seed)
    Fcut <- NULL
    maxgenes <- max(nfeatures)
    ## Cross-validation calculations
    if(nfold[1]==nobs)foldids <- matrix(sample(1:nfold[1]),ncol=1) else
    foldids <- sapply(1:nfold[2], function(x)
                     divideUp(cl, nset=nfold[1]))
    genelist <- array("", dim=c(nrow=maxgenes, ncol=nfold[1], nleaf=nfold[2]))
    Fmatrix <- array(0, dim=c(nrow=maxgenes, ncol=nfold[1], nleaf=nfold[2]))
    testscores <- NULL
    acc.cv <- numeric(maxgenes)
    if(print.progress)
      cat("\n", "Preliminary per fold calculations","\n")
    for(k in 1:nfold[2])
      {
        foldk <- foldids[,k]
    ufold <- sort(unique(foldk))
    for(i in ufold){
      if(print.progress) cat(paste(i,":",sep=""))
      trainset <- (1:nobs)[foldk!=i]
      cli <- factor(cl[trainset])

      stat <- aovFbyrow(x=x[, trainset], cl=cli)
      ordi <- order(-abs(stat))[1:maxgenes]
      genelist[,i, k] <- genes[ordi]
      Fmatrix[, i, k] <- stat[ordi]
    }
  }
    ulist <- unique(as.vector(genelist))
    df <- data.frame(t(x[ulist, , drop=FALSE]))
    names(df) <- ulist
#######################################################################
    if(print.progress)cat("\n", "Show each choice of number of features:","\n")
    for(ng in nfeatures){
      hat <- cl
      if(print.progress)cat(paste(ng,":",sep=""))
    for(k in 1:nfold[2])
      {
        foldk <- foldids[,k]
        ufold <- sort(unique(foldk))
      for(i in ufold){
        testset <- (1:nobs)[foldk==i]
        trainset <- (1:nobs)[foldk!=i]
        ntest <- length(testset)
        ntrain <- nobs-ntest
        genes.i <- genelist[1:ng, i, k]
        dfi <- df[-testset, genes.i, drop=FALSE]
        newdfi <- df[testset, genes.i, drop=FALSE]
        cli <- cl[-testset]
        xy.xda <- funda(cli~., data=dfi)
        subs <- match(colnames(dfi), rownames(df))
        newpred.xda <- predict(xy.xda, newdata=newdfi, method="debiased")
        hat[testset] <- newpred.xda$class
      }
        tabk <- table(hat,cl)
        if(k==1)tab <- tabk else tab <- tab+tabk
      }
      acc.cv[ng] <- sum(tab[row(tab)==col(tab)])/sum(tab)
    }
    cat("\n")
    if(length(nfeatures)>1&all(diff(nfeatures)==1)){
      nobs <- length(cl)
      ng1 <- length(acc.cv)
      maxacc1 <- max(acc.cv)
      sub1 <- match(maxacc1, acc.cv)
      nextacc1 <- max(acc.cv[acc.cv<1])
      lower1 <- maxacc1-sqrt(nextacc1*(1-nextacc1)/nobs)
      lsub1 <- min((1:ng1)[acc.cv>lower1])
      lower <- c("Best accuracy, less 1SD  ",
                 paste(paste(round(c(lower1),2), c(lsub1),
                             sep=" ("), " features)   ", sep=""))
      best <- c("Best accuracy",
                paste(paste(round(c(maxacc1),2), c(sub1),
                            sep=" ("), " features)", sep=""))
      acc.df <- cbind(lower, best)
      dimnames(acc.df) <- list(c("Accuracy",
                                 "(Cross-validation)"),c("",""))
      print(acc.df, quote=FALSE)
    }
    invisible(list(foldids=foldids, xUsed=df, cl=cl, acc.cv=acc.cv,
                   genelist=genelist, Fmatrix=Fmatrix, nfeatures=nfeatures))
  }
}
\keyword{htest}% at least one, from doc/KEYWORDS

