\name{summary.iC10}
\alias{summary.iC10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary results of the iC10 classifier
}
\description{
Summary results of the iC10 classifier: shows the distribution of samples classified into each iC10 group and
a summary of the maximum posterior probablity for each sample. Small values pinpoint samples with no clear
group assigned.
}
\usage{
\method{summary}{iC10}(object, ...)
}
\arguments{
  \item{object}{
An object of \code{iC10} class.
}
  \item{\dots}{
Additional arguments passed to \code{summary}.
}
}
\value{
The function prints a table of the classification ad a summary of the maximum posterior probability for
each sample.
}
\references{
Ali HR et al. Genome-driven integrated classification of breast cancer
validated in over 7,500 samples. Genome Biology 2014; 15:431.
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
Tibshirani et al. Diagnosis of multiple cancer types by shrunken
centroids of gene expression. PNAS 2002; 99(10):6567-6572.
}
\author{
Oscar M Rueda
}
\seealso{
See \code{iC10} and \code{pamr.train}, \code{pamr.cv} and \code{pamr.predict} in package \code{pamr}.
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp,
Exp.by.feat="probe", ref="hg18")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
summary(res)
}
