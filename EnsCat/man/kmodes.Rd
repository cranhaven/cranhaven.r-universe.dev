\name{kmodes}
\alias{kmodes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Run Kmodes
%%  ~~function to do ... ~~
}
\description{This function runs Kmodes.  The user must choose the number of clusters and the initial modes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kmodes(data, k, k2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ data should be a matrix or data frame, columns include the variables.
%%     ~~Describe \code{data} here~~
}
  \item{k}{ number of clusters
%%     ~~Describe \code{k} here~~
}
  \item{k2}{ set of initial modes; indices of data points
%%     ~~Describe \code{k2} here~~
}
}
\details{
This function clusters the rows of the data.
}

\references{
Huang, Z. (1998). Extensions to the v-means Algorithm for Clustering Large Data Sets with Categorical Values, Data Mining and Knowledge Discovery, 2, 283-304.
}

\examples{
data("zoo")
### Run Kmodes on zoo data with 7 clusters and the first seventh observations as initial modes
kmodes(zoo$obs,k=7,1:7)
### Run Kmodes with seven random initial modes selected from data points
kmodes(zoo$obs,k=7,sort(sample(dim(zoo$obs)[1],7)))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{~kwd1}
%\keyword{~kwd2}% __ONLY ONE__ keyword per line
\keyword{lowdimension}
