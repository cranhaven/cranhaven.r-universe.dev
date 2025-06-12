\name{print.MSA_coef}
\alias{print.MSA_coef}
\alias{show.MSA_coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print MSA_coef
}
\description{
Print function for output of moving subset analysis
}
\usage{
\method{print}{MSA_coef}(x, ...)
\method{show}{MSA_coef}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x, object}{
    List of class \code{MSA_coef}
}
\item{...}{
    Parameters passed to generic functions (currently ignored)
}
}
\details{
  This function prints the column numbers for each experiment-support variable
}

\author{
Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}

\seealso{
  \code{\link{moving_subset_analysis}}, \code{\link{GiFACE}}
}

\examples{
data(GiFACE)

## Perform Moving Subset Analysis
msa_data <- moving_subset_analysis(GiFACE$data, "CO2A_Mean", "BYT", 30, group = GiFACE$year)

msa_data

}
\keyword{ utilities }
