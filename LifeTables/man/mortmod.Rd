\name{mortmod}
\alias{mortmod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Underlying model in the HMD life table system
}
\description{
This function generates a complete (up to age 110+) human mortality rate profile given a certain family and level of mortality
}
\usage{
mortmod(pattern, sex="female", alpha=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{pattern}{
An integer from 1 to 7 indicating the underlying family pattern
}
  \item{sex}{
"male" or "female". Indicates the sex of the desired schedule 
  	}
  \item{alpha}{
A scalar which determines the quantity to add or subtract from the underlying cluster pattern. Typical alpha values will not exceed an absolute value of 4, although it is possible to take on larger values. Defaults to 0. 
}
}
\details{
The \code{pattern} argument identifies the shape or pattern of mortality and then \code{alpha} determines the level by weighting a set of age-specific deviations to add or subtract (depending on the sign of alpha) to the underlying cluster pattern.
}
\value{

  \item{model.patt}{A vector of length 24 which contains a complete set of age-specific logged mortality rates for age intervals 0-1, 1-4, 5-9, 10-14, ..., 110+}

}
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Paper presented at the annual meeting of the Population Association of America. \url{http://paa2011.princeton.edu/abstracts/111808}

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded February 2015).
}
\author{
David J. Sharrow
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hmd.DA}}, \code{\link{alpha.e0}}
}
\examples{

mortmod(pattern=2, alpha=0.5, sex="female")
mortmod(pattern=5, alpha=-0.1, sex="male")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ misc }% __ONLY ONE__ keyword per line
