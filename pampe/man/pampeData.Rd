\name{pampeData}
\encoding{ascii}
\alias{pampeData}
\title{
Data Setup for the Implementation of the Panel Data Approach for Program Evaluation
}
\description{
Helps set up the data in a wide and time-series format, to then apply the \code{\link{pampe}} function.}
\usage{
pampeData(data, start=1, frequency=1, timevar=NA, idvar=NA, yvar=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The name of the data matrix to be used, e.g. growth. The data can be in long or wide format.
}

  \item{start}{
The starting period of the data set, in standard \code{\link{ts}} format. For example, 1993 for yearly data; if quarterly data, c(1993,1) refers to 1993 Q1; if monthly data, c(1993,1) refers to 1993 Jan, etc.
}

  \item{frequency}{
The frequency of the time series, i.e., the number of observations per unit of time. For example, 1 for yearly data, 4 for quarterly data, 12 for monthly data.
}

  \item{timevar}{
If the data is in long format, the name of the variable indicating the time index (e.g. "Quarter").
}

  \item{idvar}{
If the data is in long format, the name of the variable indicating the id index (e.g. "Country").
}

  \item{yvar}{
If the data is in long format, and there is more than one variable in the dataset, the name of the variable that we want to keep in the wide format (e.g. "GDP").
}
}


\author{
Ainhoa Vega-Bayo
}
\note{
Check the references in \code{\link{pampe}} for more information. The leaps package by Lumley is required for pampe to run properly.
}


\seealso{
\code{\link{pampe}}
\code{\link{robustness}}
\code{\link{leaps}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.