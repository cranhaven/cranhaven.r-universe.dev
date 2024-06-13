\name{iC10}
\alias{iC10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A copy number and expression-based classfier for breast cancers}
\description{
iC10 implements the classifier described in the paper 'Genome-driven integrated classification of breast cancer validated in over 7,500 samples' (Ali HR et al., Genome Biology 2014). It uses copy number and/or expression form breast cancer data, trains a pamr classifier (Tibshirani et al.) with the features available and predicts the iC10 group.
}
\usage{
iC10(x, seed=25435)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object with class \code{iC10features}: A list with elements 'train.CN', 'train.Exp', 'train.iC10',
    'CN', 'Exp', 'map.cn', 'map.exp'}
  \item{seed}{seed to initialize random number generator. It is passed to \code{set.seed()}.
  See details.}
}
\details{
This function trains a \code{pamr} classifier and predicts the set of
samples. The shrinkage parameter is obtained with crossvalidation,
therefore different runs can give different results (unless a seed is specified).
}
\value{
  An object of class \code{iC10}. A list with the following elements:
  \item{class}{Prediction classes for the samples}
  \item{posterior}{Probablitites for each sample to belong to each of
    the 10 groups}
  \item{centroids}{Shrunken Centroids for each of the 10 groups.}
  \item{fitted}{Normalized features for the samples classified.}
 \item{map.cn}{Annotation data for the copy number features}
 \item{map.exp}{Annotation data for the expression features}
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
Oscar M. Rueda
}

\seealso{
See \code{pamr.train}, \code{pamr.cv} and \code{pamr.predict} in package \code{pamr}.
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)

}
