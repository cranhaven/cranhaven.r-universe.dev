\name{matchFeatures}
\alias{matchFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matching features from the classifier to the test data.
}
\description{
This function matches available copy number and/or expression data features to the training signatures;
using either genomic position or HUGO gene name for copy number features and either Illumina
probe names or HUGO gene name for expression features.
}
\usage{
matchFeatures(CN = NULL, Exp = NULL,
CN.by.feat = c("gene", "probe"),
Exp.by.feat = c("gene", "probe"),
ref="hg19")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CN}{
Data must be log2 copy number ratios. Two formats are allowed:
- a matrix where each row represents a gene and each column a sample.
  In this case \code{CN.by.feat} must be "gene" and the rownames must be the hgnc gene names.
- a data.frame with segmented data. The following columns must exist: 'ID' for the sample name,
'chromosome_name' for the chromosome (must be numeric), 'loc.start' for the start position of the region,
'loc.end' for the end position of the region, 'seg.mean' for the log2ratio of the segment.
If NULL, copy number is not used in the classifier.
}
  \item{Exp}{
Matrix with the expression data to classify. Each row must be a gene or an Illumina probe, and each
column must correspond to a sample.
Rownames must be either Illumina probes, in which case \code{Exp.by.feat} must be "probe";  or hgnc gene names,
in which case \code{Exp.by.feat} must be "gene".
If NULL, expression is not used in the classifier.
}
  \item{CN.by.feat}{
Either "probe" or "gene", Default is "probe".
}
  \item{Exp.by.feat}{
Either "probe" or "gene", Default is "gene".
}
  \item{ref}{
Etiher "hg18" or "hg19". It is used to match the copy number probes if \code{CN.by.feat} is "probe"
}
}
\details{
One of \code{CN} or \code{Exp} must be not NULL.
If matching is done by gene, hgnc gene name is used to match the rownames of the features. A list of
synonym gene names is used (see \code{Map.All}).
For copy number features matched by probe, the maximum log ratio in absolute value inside the limits
of the feature is used. If there is no copy number in that region, the value of the probe before it is used.
}
\value{
A list with the following elements is returned:
 \item{CN}{copy number data to classify}
 \item{train.CN}{copy number training data}
 \item{Exp}{expression data to classify}
 \item{train.Exp}{expression training data}
 \item{train.iC10}{iC10 assignments for the training data}
 \item{map.cn}{annotation data for the copy number features}
 \item{map.exp}{annotation data for the expression features}
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
Note that the training set will be different, depending on the features
matched.
Genomic annotation for the training dataset has been obtained from Mark
Dunning's lluminaHumanv3.db package.
}

\seealso{
\code{\link{normalizeFeatures}}, \code{\link{iC10}}
}
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp,Exp.by.feat="probe", ref="hg18")
str(features)
}