\name{iC10-package}
\alias{iC10-package}
\docType{package}
\title{
\packageTitle{iC10}
}
\description{
\packageDescription{iC10}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{iC10}
\packageIndices{iC10}
iC10 implements the classifier described in the paper 'Genome-driven integrated classification of breast cancer validated in over 7,500 samples' (Ali HR et al., Genome Biology 2014). It uses copy number and/or expression form breast cancer data, trains a pamr classifier (Tibshirani et al.) with the features available and predicts the iC10 group.
}
\author{
\packageAuthor{iC10}

Maintainer: \packageMaintainer{iC10}
}
\references{
Ali HR et al. Genome-driven integrated classification of breast cancer
validated in over 7,500 samples. Genome Biology 2014; 15:431.
Curtis et al. The genomic and transcriptomic architecture of 2,000
breast tumours reveals novel subgroups. Nature 2012; 486:346-352.
Tibshirani et al. Diagnosis of multiple cancer types by shrunken
centroids of gene expression. PNAS 2002; 99(10):6567-6572.
}
\keyword{ package }
\examples{
require(iC10TrainingData)
data(train.CN)
data(train.Exp)
features <- matchFeatures(Exp=train.Exp, Exp.by.feat="probe")
features <- normalizeFeatures(features, "scale")
res <- iC10(features)
summary(res)
goodnessOfFit(res, newdata=features)
compare(res, iC10=1:2, newdata=features)
compare(res, iC10=2:4, newdata=features)
}