\name{meancenteraddon}
\alias{meancenteraddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment for mean-centering
}
\description{
Performs addon batch effect adjustment for mean-centering: 
1) takes the output of \code{\link{meancenter}} applied to a training 
data set together with new batch data; 
2) checks whether the training data was also adjusted using mean-centering and whether the same number of variables is present in training and new data;
3) performs mean-centering on the new batch data.
}
\usage{
meancenteraddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{meancenter}.
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
Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.
}
\author{
Roman Hornung
}
\note{
Because mean-centering is performed "batch by batch" the "addon procedure" for mean-centering consists of plain mean-centering on the new test batches.
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


params <- meancenter(x=Xtrain, batch=batchtrain)

Xtestaddon <- meancenteraddon(params=params, x=Xtest, batch=batchtest)
}
