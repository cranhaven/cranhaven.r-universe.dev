\name{remove_rarevariants}
\alias{remove_rarevariants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function used in spmle and acoarm to remove rare-variant covariates
}
\description{
It is used to remove rare-variant covariates, which can cause divergence problem.
}
\usage{
remove_rarevariants(data, cutoff = 0.02)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataframe composed of covariates.
}
  \item{cutoff}{
Proportion cutoff. If data composed of more than (1-cutoff) proportion of a constant 
value, we call it rare-variant.
}
}

\details{
The function removes rare-variant covariates.
}

\value{
A logical vector composed of True or False. True means a covariate is rare-variant.
}

\author{
James Y. Dai
}

\examples{
## Load the example data
data(acodata)
result <- remove_rarevariants(acodata[, c("vacc1_evinf","fcgr2a.3")])
}

\keyword{ QC }