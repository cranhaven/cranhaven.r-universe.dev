\name{normalizeFeatures}
\alias{normalizeFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalization of expression features
}
\description{
Normalization of expression features. Several methods available in the package \code{CONOR} can be used.
}
\usage{
normalizeFeatures(x, method=c("none", "scale"))
}
\arguments{
  \item{x}{
An object result of a call to \code{matchFeatures}
}
  \item{method}{
Several methods are available:
"none": No normalization is done
"scale": Each expression feature is scaled to have zero mean and standard deviation 1
}
}
\details{
No further normalization is needed on the copy number, as log2 ratios
are comparable between platforms.
}
\value{
A list of the same format as \code{matchFeatures}, but with \code{train.Exp} anfd \code{Exp} normalized.
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
\note{
As \code{CONOR} package is no longer maintained, the methods are not available temporarily. We will include more normalization methods in the next version of this package.
}

\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp,
Exp.by.feat="probe", ref="hg18")
features <- normalizeFeatures(features, "scale")
}