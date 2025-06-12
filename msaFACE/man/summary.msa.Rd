\name{summary.MSA_coef}
\alias{summary.MSA_coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for MSA_coef
}
\description{
Summary function for output of moving subset analysis
}
\usage{
\method{summary}{MSA_coef}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
    List of class \code{MSA_coef}
}
\item{...}{
    Parameters passed to generic summary function
}
}
\details{
  \itemize{
     \item {Variable: }{Name of the forcing experiment-support variable}
     \item {CFEmax: }{Maximum CO2 fertilization effect}
     \item {CFEmin: }{Minimum CO2 fertilization effect}
     \item {Cond_for_CFEmax: }{Forcing variable at maximum CO2 fertilization effect}
     \item {Cond_for_CFEmin: }{Forcing variable at minimum CO2 fertilization effect}
     \item {Dependent_Max: }{Maximum of dependent variable}
     \item {Dependent_Min: }{Minimum of dependent variable}
}
}


\author{
Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}

\seealso{
  \code{\link{plot.MSA_coef}}, \code{\link{moving_subset_analysis}}, \code{\link{GiFACE}}
}

\examples{

## Load data
data(GiFACE)

## Perform Moving Subset Analysis
msa_data <- moving_subset_analysis(GiFACE$data, "CO2A_Mean", "BYT", 30, group = GiFACE$year)

## Show summary
summary(msa_data)
}
\keyword{ utilities }
 
