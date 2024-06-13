\name{print.iC10}
\alias{print.iC10}
\title{
Print results of the iC10 classifier
}
\description{
Print results of the iC10 classifier
}
\usage{
\method{print}{iC10}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of \code{iC10} class:
}
  \item{\dots}{
Additional arguments passed to \code{print}.
}
}
\value{
It returns a call to \code{str}.
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
res
}