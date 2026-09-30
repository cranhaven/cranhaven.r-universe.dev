\name{ratioaaddon}
\alias{ratioaaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment for Ratio-A
}
\description{
Performs addon batch effect adjustment for Ratio-A: 
1) takes the output of \code{\link{ratioa}} applied to a training 
data set together with new batch data; 
2) checks whether the training data was also adjusted using Ratio-A and whether the same number of variables is present in training and new data;
3) performs Ratio-A on the new batch data.
}
\usage{
ratioaaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{ratioa}.
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
Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010). A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal 10:278-291, <\doi{10.1038/tpj.2010.57}>.

Hornung, R., Boulesteix, A.-L., Causeur, D. (2016). Combining location-and-scale batch effect adjustment with data cleaning by latent factor adjustment. BMC Bioinformatics 17:27, <\doi{10.1186/s12859-015-0870-z}>.
}
\author{
Roman Hornung
}
\note{
Because Ratio-A is performed "batch by batch" the "addon procedure" for Ratio-A consists of plain Ratio-A on the new test batches.
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


params <- ratioa(x=Xtrain, batch=batchtrain)

Xtestaddon <- ratioaaddon(params=params, x=Xtest, batch=batchtest)
}
