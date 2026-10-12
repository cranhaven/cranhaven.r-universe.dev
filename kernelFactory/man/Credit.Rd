\name{Credit}
\alias{Credit}
\docType{data}
\title{ Credit approval (Frank and Asuncion, 2010)
}
\description{
    \code{Credit} contains credit card applications. The dataset has a good mix of continuous and categorical features.
      
}
\usage{data(Credit)}

\format{
  A data frame with 653 observations, 15 predictors and a binary criterion variable called \code{Response}
 }
\details{
All observations with missing values are deleted.
}
\source{
Frank, A. and Asuncion, A. (2010). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science. 
}
\references{
The original dataset can be downloaded at http://archive.ics.uci.edu/ml/datasets/Credit+Approval
}
\examples{
data(Credit)
str(Credit)
table(Credit$Response)
}
\keyword{datasets}
