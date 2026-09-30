\name{nobaaddon}
\alias{nobaaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
No addon batch effect adjustment
}
\description{
This function is merely included for consistency. It does the following:
1) takes the output of \code{\link{noba}} applied to a training 
data set together with new batch data; 
2) checks whether the training data has also not been adjusted using a batch effect adjustment method and whether the same number of variables is present in training and new data;
3) returns the new batch data not adjusted for batch effects.
}
\usage{
nobaaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{noba}.
}
  \item{x}{
matrix. The covariate matrix of the new data. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable of the new data. Currently has to have levels: '1', '2', '3' and so on.
}
}

\value{
The unadjusted covariate matrix \code{x} of the test data.
}
\references{
Hornung, R., Causeur, D., Bernau, C., Boulesteix, A.-L. (2017). Improving cross-study prediction through addon batch effect adjustment and addon normalization. Bioinformatics 33(3):397–404, <\doi{10.1093/bioinformatics/btw650}>.
}
\author{
Roman Hornung
}
\note{
It is \bold{not recommended} to perform no addon batch effect adjustment in cross-study prediction settings. Given a not too small test set, the following methods are recommended (Hornung et al., 2016): \code{\link{combatba}}, \code{\link{meancenter}}, \code{\link{ratioa}}, \code{\link{ratiog}}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

trainind <- which(batch \%in\% c(1,2))

Xtrain <- X[trainind,]
ytrain <- y[trainind]
batchtrain <- factor(as.numeric(batch[trainind]), levels=c(1,2))


testind <- which(batch \%in\% c(3,4))

Xtest <- X[testind,]
ytest <- y[testind]

batchtest <- as.numeric(batch[testind])
batchtest[batchtest==3] <- 1
batchtest[batchtest==4] <- 2
batchtest <- factor(batchtest, levels=c(1,2))


params <- noba(x=Xtrain, batch=batchtrain)

Xtestaddon <- nobaaddon(params=params, x=Xtest, batch=batchtest)


all(as.vector(Xtestaddon)==as.vector(Xtest))
}
