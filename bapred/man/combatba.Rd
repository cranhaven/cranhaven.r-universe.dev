\name{combatba}
\alias{combatba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment using ComBat
}
\description{
Performs batch effect adjustment using the parametric version of ComBat and additionally returns information necessary for addon batch effect adjustment with ComBat.
}
\usage{
combatba(x, batch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix. The covariate matrix. Observations in rows, variables in columns.
}
  \item{batch}{
factor. Batch variable. Each factor level (or 'category') corresponds to one of the batches. For example, if there are four batches, this variable would have four factor levels and observations with the same factor level would belong to the same batch.
}
}

\value{
\code{combatba} returns an object of class \code{combat}.
An object of class "\code{combat}" is a list containing the following components: 
\item{xadj}{ matrix of adjusted (training) data }
\item{meanoverall}{ vector containing the overall means of the variables. Used in addon adjustment. }
\item{var.pooled}{ vector containing the pooled variances of the variables. Used in addon adjustment. }
\item{batch}{ batch variable }
\item{nbatches}{ number of batches }
}
\references{
Johnson, W. E., Rabinovic, A., Li, C. (2007). Adjusting batch effects in microarray expression data using empirical bayes methods. Biostatistics 8:118-127, <\doi{10.1093/biostatistics/kxj037}>.

Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010). A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal 10:278-291, <\doi{10.1038/tpj.2010.57}>.
}
\author{
Roman Hornung
}
\note{
The original ComBat-code is used in \code{combatba}:
\url{http://www.bu.edu/jlab/wp-assets/ComBat/Download.html} (Access date: 2015/06/19)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

combatba(x=X, batch=batch)
}
