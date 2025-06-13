\name{test}
\alias{test}
\docType{data}
\title{
Test dataset. 
}
\description{

A test dataset containing 54 circadian protein sequences collected from literature. This dataset has been used as an independent test dataset for assessing the predition accuracy of \code{PredCRG} model.
}
\usage{data("test")}

\seealso{
\link{PredCRG}, \link{PredCRG_Enc}, \link{PredCRG_data}
}
\examples{
data(test)
PredCRG(test[1:10])
}