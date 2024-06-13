\name{goodnessOfFit}
\alias{goodnessOfFit}
\alias{goodnessOfFit.iC10}
\title{
Goodness of fit results of the iC10 classifier
}
\description{
Goodness of fit results of the iC10 classifier: this function computes correlations between the signatures
of the training dataset and the classified features.
}
\usage{
goodnessOfFit(obj, iC10=1:10, newdata=NULL,...)
\method{goodnessOfFit}{iC10}(obj, iC10=1:10, newdata=NULL,...)
}
\arguments{
  \item{obj}{
An object of \code{iC10} class.
}
  \item{iC10}{
Groups to compute goodness of fit.
}
  \item{newdata}{
The feature data to compute the goodness of fit. Must be the samples classified in \code{obj}.
It can be a call to \code{matchFeatures} or \code{normalizeFeatures}. If NULL, \code{obj$fitted} is
used.
}
  \item{\dots}{
Additional arguments passed to \code{cor} (like \code{method};  Default is pearson)
}
}
\value{
It prints the correlation for each iC10.
}
\references{
Ali HR et al. Genome-driven integrated classification of breast cancer
validated in over 7,500 samples. Genome Biology 2014; 15:431.
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
}
\author{
Oscar M Rueda
}

\seealso{
\code{iC10}
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
goodnessOfFit(res, newdata=features)
}