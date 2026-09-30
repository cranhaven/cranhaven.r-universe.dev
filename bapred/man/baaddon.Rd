\name{baaddon}
\alias{baaddon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Addon batch effect adjustment
}
\description{
Performs addon batch effect adjustment for a method of choice: takes the output of \code{\link{ba}} or that of one of the functions performing a specific batch effect adjustment method (e.g. \code{fabatch} or \code{svaba}) and new batch data. Then performs the respective batch effect adjustment method on the new batch data.
}
\usage{
baaddon(params, x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
object of class \code{fabatch}, \code{combat}, \code{svatrain}, \code{meancenter}, \code{standardize}, \code{ratioa}, \code{ratiog} or \code{noba}. Contains parameters necessary for addon batch effect adjustment according to the respective method.
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

Hornung, R., Causeur, D., Bernau, C., Boulesteix, A.-L. (2017). Improving cross-study prediction through addon batch effect adjustment and addon normalization. Bioinformatics 33(3):397–404, <\doi{10.1093/bioinformatics/btw650}>.

Johnson, W. E., Rabinovic, A., Li, C. (2007). Adjusting batch effects in microarray expression data using empirical bayes methods. Biostatistics 8:118-127, <\doi{10.1093/biostatistics/kxj037}>.

Leek, J. T., Storey, J. D. (2007). Capturing Heterogeneity in Gene Expression Studies by Surrogate Variable Analysis. PLoS Genetics 3:1724-1735, <\doi{10.1371/journal.pgen.0030161}>.

Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010). A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal 10:278-291, <\doi{10.1038/tpj.2010.57}>.

Parker, H. S., Bravo, H. C., Leek, J. T. (2014). Removing batch effects for prediction problems with frozen surrogate variable analysis. PeerJ 2:e561, <\doi{10.7717/peerj.561}>.
}
\author{
Roman Hornung
}
\note{

\bold{The following methods are NOT recommended in cross-study prediction settings}: FAbatch (\code{\link{fabatch}}), frozen SVA (\code{\link{svaba}}), standardization (\code{\link{standardize}}) as well as no addon batch effect adjustment (\code{\link{noba}}). 

Given a not too small test set, \bold{the following methods are recommended} (Hornung et al., 2016b): ComBat (\code{\link{combatba}}), mean-centering (\code{\link{meancenter}}), Ratio-A (\code{\link{ratioa}}), Ratio-G (\code{\link{ratiog}}).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

# Random subset of 150 variables:
set.seed(1234)
Xsub <- X[,sample(1:ncol(X), size=150)]

# In cases of batches with more than 20 observations
# select 20 observations at random:
subinds <- unlist(sapply(1:length(levels(batch)), function(x) {
  indbatch <- which(batch==x)
  if(length(indbatch) > 20)
    indbatch <- sort(sample(indbatch, size=20))
  indbatch
}))
Xsub <- Xsub[subinds,]
batchsub <- batch[subinds]
ysub <- y[subinds]



trainind <- which(batchsub \%in\% c(1,2))

Xsubtrain <- Xsub[trainind,]
ysubtrain <- ysub[trainind]
batchsubtrain <- factor(as.numeric(batchsub[trainind]), levels=c(1,2))


testind <- which(batchsub \%in\% c(3,4))

Xsubtest <- Xsub[testind,]
ysubtest <- ysub[testind]

batchsubtest <- as.numeric(batchsub[testind])
batchsubtest[batchsubtest==3] <- 1
batchsubtest[batchsubtest==4] <- 2
batchsubtest <- factor(batchsubtest, levels=c(1,2))



somemethods <- c("fabatch", "combat", "meancenter", "none")

adjustedtestdata <- list()

for(i in seq(along=somemethods)) {
  cat(paste("Adjusting training data using method = \"", somemethods[i], 
    "\"", sep=""), "\n")
  paramstemp <- ba(x=Xsubtrain, y=ysubtrain, batch=batchsubtrain, 
    method = somemethods[i])
  cat(paste("Addon adjusting test data using method = \"", 
    somemethods[i], "\"", sep=""), "\n")
  adjustedtestdata[[i]] <- baaddon(params=paramstemp, x=Xsubtest, 
    batch=batchsubtest)
}
}
