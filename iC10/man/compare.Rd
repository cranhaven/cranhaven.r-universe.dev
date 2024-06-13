\name{compare}
\alias{compare}
\alias{compare.iC10}
\title{
Compare results of the iC10 classifier
}
\description{
This function plots the centroids of the training set versus the average profiles
of the new data classified in each group.
}
\usage{
compare(obj, iC10=1:10, newdata, name.test="Test",...)
\method{compare}{iC10}(obj, iC10=1:10, newdata, name.test="Test",...)
}
\arguments{
  \item{obj}{
  An object of class \code{iC10}, a result of a call to \code{iC10()}
}
  \item{iC10}{
Groups to plot
}
  \item{newdata}{
  Set of features of the new data to compare. They must be the same samples classified and contained in
\code{x}. A result of a call to \code{matchFeatures()} or \code{normalizeFeatures()}
}
  \item{name.test}{
Name of the new data set to appear in the text of the plot
}
  \item{\dots}{
Additional arguments passed to \code{plot()}
}
}
\value{
A plot is returned with two plots per groups requested.
}
\references{
Ali HR et al. Genome-driven integrated classification of breast cancer
validated in over 7,500 samples. Genome Biology 2014; 15:431.
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
}
\author{
Oscar M. Rueda
}
\seealso{
\code{\link{iC10}}, \code{\link{plot.iC10}}, \code{\link{matchFeatures}}, \code{\link{normalizeFeatures}}
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
compare(res, 1:3, newdata=features)
}
