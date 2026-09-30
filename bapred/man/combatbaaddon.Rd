\name{combatbaaddon}
\alias{combatbaaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment using ComBat
}
\description{
Performs addon batch effect adjustment using ComBat. Takes the output of performing \code{\link{combatba}} on a training data set and new batch data and correspondingly adjusts the test data to better match the adjusted training data according to the ComBat model
}
\usage{
combatbaaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{combat}. Contains parameters necessary for addon batch effect adjustment.
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
Johnson, W. E., Rabinovic, A., Li, C. (2007). Adjusting batch effects in microarray expression data using empirical bayes methods. Biostatistics 8:118-127, <\doi{10.1093/biostatistics/kxj037}>.

Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010). A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal 10:278-291, <\doi{10.1038/tpj.2010.57}>.
}
\author{
Roman Hornung
}
\note{
The original ComBat-code is used in \code{combatbaaddon}: 
\url{http://www.bu.edu/jlab/wp-assets/ComBat/Download.html} (Access date: 2015/06/19)
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


params <- combatba(x=Xtrain, batch=batchtrain)

Xtestaddon <- combatbaaddon(params, x=Xtest, batch=batchtest)
}
