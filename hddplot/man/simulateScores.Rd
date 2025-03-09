\name{simulateScores}
\alias{simulateScores}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate linear discriminant scores from random data, after selection}
\description{
Simulates the effect of generating scores from random data, possibly
with predicted scores calculates also for additional 'observations'
}
\usage{
simulateScores(nrows = 7129, cl = rep(1:3, c(19, 10, 2)), x = NULL, cl.other = NULL,
               x.other = NULL, nfeatures = 15, dimen=2, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrows}{number of rows of random data matrix}
  \item{cl}{classifying factor}
  \item{x}{data matrix, by default randomly generated}
  \item{cl.other}{classifying factor for additional observations}
  \item{x.other}{additional observations}
  \item{nfeatures}{number of features to select (by default uses
aov F-statistic)}
  \item{dimen}{number of sets of discriminant scores to retain (at most
one less than number of levels of \code{cl})}
  \item{seed}{set, if required, so that calculations can be reproduced}
}
\value{
  \item{scores}{matrix of scores}
  \item{cl}{classifying factor}
  \item{other}{matrix of 'other' scores}
  \item{cl.other}{classifying factor for \code{scores.other}}
  \item{nfeatures}{number of features used in generating the scores}
}
\author{John Maindonald}
\note{NB: Prior to 0.53, this function made (wrongly) a random
  selection of features.
}
\examples{
scorelist <- simulateScores(nrows=500, cl=rep(1:3, c(19,10,2)))
plot(scorelist$scores, col=unclass(scorelist$cl), pch=16)


## The function is currently defined as
simulateScores <-
  function (nrows = 7129, cl = rep(1:3, c(19, 10, 2)), x = NULL,
            cl.other = NULL, x.other = NULL, nfeatures = 15, dimen = 2,
            seed = NULL)
{
  if (!is.null(seed))
    set.seed(seed)
  m <- length(cl)
  m.other <- length(cl.other)
  if (is.null(x)) {
    x <- matrix(rnorm(nrows * m), nrow = nrows)
    rownames(x) <- paste(1:nrows)
  }
  else nrows <- dim(x)[1]
  if (is.null(x.other)) {
    x.other <- matrix(rnorm(nrows * m.other), nrow = nrows)
    rownames(x.other) <- paste(1:nrows)
  }
  if (is.numeric(cl))
    cl <- paste("Gp", cl, sep = "")
  if(!is.null(cl.other)){
    if (is.numeric(cl.other))
      cl.other <- paste("Gp", cl.other, sep = "")
    cl.other <- factor(cl.other)
  }
  cl <- factor(cl)
  if (dimen > length(levels(cl)) - 1)
    dimen <- length(levels(cl)) - 1
  ordfeatures <- orderFeatures(x, cl = cl, values = TRUE)
  stat <- ordfeatures$stat[1:nfeatures]
  ord.use <- ordfeatures$ord[1:nfeatures]
  xUse.ord <- data.frame(t(x[ord.use, ]))
  xUseOther.ord <- data.frame(t(x.other[ord.use, ]))
  ordUse.lda <- lda(xUse.ord, grouping = cl)
  scores <- predict(ordUse.lda, dimen = dimen)$x
  if(!is.null(cl.other))
    scores.other <- predict(ordUse.lda, newdata = xUseOther.ord,
                            dimen = dimen)$x else
  scores.other <- NULL
  invisible(list(scores = scores, cl = cl, other = scores.other,
                 cl.other = cl.other, nfeatures = nfeatures))
}
}
\keyword{datagen}% at least one, from doc/KEYWORDS

