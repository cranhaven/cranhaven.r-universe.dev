\name{plotTrainTest}
\alias{plotTrainTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot predictions for both a I/II train/test split, and the reverse}
\description{
A division of data is specified, for use of linear discriminant analysis,
into a training and test set. Feature selection and model fitting is
formed, first with I/II as training/test, then with II/I as training/test.
Two graphs are plotted -- for the I (training) /II (test) scores, and for
the II/I scores.
}
\usage{
plotTrainTest(x, nfeatures, cl, traintest,
              titles = c("A: I/II (train with I, scores are for II)",
                         "B: II/I (train with II, scores are for I)"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Matrix; rows are features, and columns are observations
('samples')}
  \item{nfeatures}{integer: numbers of features for which calculations are
required}
  \item{cl}{Factor that classifies columns into groups that will classify
the data for purposes of discriminant calculations}
  \item{traintest}{Values that specify a division of observations into
two groups. In the first pass (fold), one to be training and the other test,
with the roles then reversed in a second pass or fold.}
  \item{titles}{A character vector of length 2 giving titles for the two graphs}
}
\value{
Two graphs are plotted.
}
\author{John Maindonald}
\examples{
mat <- matrix(rnorm(1000), ncol=20)
cl <- factor(rep(1:3, c(7,9,4)))
gp.id <- divideUp(cl, nset=2)
plotTrainTest(x=mat, cl=cl, traintest=gp.id, nfeatures=c(2,3))



## The function is currently defined as
function(x, nfeatures, cl, traintest,
           titles=c("A: I/II (train with I, scores are for II)",
             "B: II/I (train with II, scores are for I)")){
    oldpar <- par(mfrow=c(1,2), pty="s")
    on.exit(par(oldpar))
    if(length(nfeatures)==1)nfeatures <- rep(nfeatures,2)
    traintest <- factor(traintest)
    train <- traintest==levels(traintest)[1]
    testset <- traintest==levels(traintest)[2]
    cl1 <- cl[train]
    cl2 <- cl[testset]
    nf1 <- nfeatures[1]
    ord1 <- orderFeatures(x, cl, subset=train)
    df1 <- data.frame(t(x[ord1[1:nf1], train]))
    df2 <- data.frame(t(x[ord1[1:nf1], testset]))
    df1.lda <- lda(df1, cl1)
    scores <- predict(df1.lda, newdata=df2)$x
    scoreplot(scorelist=list(scores=scores, cl=cl2,
             nfeatures=nfeatures[1], other=NULL, cl.other=NULL),
           prefix.title="")
    mtext(side=3, line=2, titles[1], adj=0)
    nf2 <- nfeatures[2]
    ord2 <- orderFeatures(x, cl, subset=testset)
    df2 <- data.frame(t(x[ord2[1:nf2], testset]))
    df1 <- data.frame(t(x[ord2[1:nf2], train]))
    df2.lda <- lda(df2, cl2)
    scores <- predict(df2.lda, newdata=df1)$x
    scoreplot(scorelist=list(scores=scores, cl=cl1,
             nfeatures=nfeatures[2], other=NULL, cl.other=NULL),
           prefix.title="")
    mtext(side=3, line=2, titles[2], adj=0)
  }

}
\keyword{hplot}% at least one, from doc/KEYWORDS

