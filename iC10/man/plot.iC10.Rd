\name{plot.iC10}
\alias{plot.iC10}
\title{
Plot results of the iC10 classifier
}
\description{
Plot results of the iC10 classifier, in two different formats: either the signatures of the training
set or the signatures of the new data classified.
}
\usage{
\method{plot}{iC10}(x, sample.name=1, newdata = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of \code{iC10} class:
}
\item{sample.name}{
  Number of sample to plot (if \code{newdata} is NULL). It can be either a number or a character with the
sample name.
}
  \item{newdata}{
  An object result to call to \code{matchFeatures} or \code{normalizeFeatures} containing the features of the samples
to plot.
}
  \item{\dots}{
Additional arguments passed to \code{plot}.
}
}
\details{
Two types of plots can be produced. If \code{newdata} is NULL, a panel 6x2 is drawn with the 10 profiles
of the signatures of the training set and the profile of the features of \code{sample.name} and the
distribution of the probabilities of classification to each iC10 for that sample.
If \code{newdata} is not nutll, a panel 6x2 (with the 11th panel empty) is drawn with the 10 profiles of
\code{newdata} samples and their distribution into the clusters.
The features are sorted by type: copy number (if available) are drawn in grey, and then expression, each
of them are sorted by genomic position.
}
\value{
A 6x2 plot is produced.
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
plot(res, sample.name=10)
plot(res, newdata=features)
}