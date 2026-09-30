\name{standardizeaddon}
\alias{standardizeaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment for standardization
}
\description{
Performs addon batch effect adjustment for standardization: 
1) takes the output of \code{\link{standardize}} applied to a training 
data set together with new batch data; 
2) checks whether the training data was also adjusted using standardization and whether the same number of variables is present in training and new data;
3) performs standardization on the new batch data.
}
\usage{
standardizeaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{standardize}.
}
  \item{x}{
matrix. The covariate matrix of the new data. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable of the new data. Currently has to have levels: '1', '2', '3' and so on.
}
}

\value{
The adjusted covariate matrix of the test data.
}
\references{
Hornung, R., Causeur, D., Bernau, C., Boulesteix, A.-L. (2017). Improving cross-study prediction through addon batch effect adjustment and addon normalization. Bioinformatics 33(3):397–404, <\doi{10.1093/bioinformatics/btw650}>.

Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.
}
\author{
Roman Hornung
}
\note{
It is \bold{not recommended} to perform standardization in cross-study prediction settings, because for some classifiers the prediction performance can be strongly impaired by this method. Given a not too small test set, the following methods are recommended (Hornung et al., 2016a): \code{\link{combatba}}, \code{\link{meancenter}}, \code{\link{ratioa}}, \code{\link{ratiog}}.

Because standardization is performed "batch by batch" the "addon procedure" for standardization consists of plain standardization on the new test batches.
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


params <- standardize(x=Xtrain, batch=batchtrain)

Xtestaddon <- standardizeaddon(params=params, x=Xtest, batch=batchtest)
}
