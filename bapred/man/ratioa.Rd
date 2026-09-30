\name{ratioa}
\alias{ratioa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch effect adjustment using Ratio-A
}
\description{
Performs batch effect adjustment using Ratio-A. Here, the variable values are divided by the batch-specific arithmetic mean of the corresponding variable.
}
\usage{
ratioa(x, batch)
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
\code{ratioa} returns an object of class \code{ratioa}.
An object of class "\code{ratioa}" is a list containing the following components: 
\item{xadj}{ matrix of adjusted (training) data }
\item{nbatches}{ number of batches }
\item{batch}{ batch variable }
}
\references{
Luo, J., Schumacher, M., Scherer, A., Sanoudou, D., Megherbi, D., Davison, T., Shi, T., Tong, W., Shi, L., Hong, H., Zhao, C., Elloumi, F., Shi, W., Thomas, R., Lin, S., Tillinghast, G., Liu, G., Zhou, Y., Herman, D., Li, Y., Deng, Y., Fang, H., Bushel, P., Woods, M., Zhang, J. (2010). A comparison of batch effect removal methods for enhancement of prediction performance using maqc-ii microarray gene expression data. The Pharmacogenomics Journal 10:278-291, <\doi{10.1038/tpj.2010.57}>.
}
\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

params <- ratioa(x=X, batch=batch)
}
