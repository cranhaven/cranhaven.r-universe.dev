\name{remove_missingdata}
\alias{remove_missingdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function used in acoarm to remove missing data
}
\description{
It is used to remove samples which have NA/missing data in covariates.
}
\usage{
remove_missingdata(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data is a dataframe.
}
}

\details{
The function removes samples (by rows) which have NA/missing data.
}

\value{
A list of the following components.
\item{idx}{The indices of rows without missing values}
\item{data}{The dataframe without missing values}
}

\author{
James Y. Dai
}

\examples{
## Load the example data
data(acodata)
result <- remove_missingdata(acodata[, c("vacc1_evinf","fcgr2a.3")])
}
\keyword{ QC }