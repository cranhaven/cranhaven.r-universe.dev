\name{wowa.WAM}
\alias{wowa.WAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{WAM computation 
}
\description{Function for calculating the Weighted Arithmetic Mean 
}
\usage{
  wowa.WAM(n, x, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{  
   \item{n}{Dimension of the array x}
  \item{x}{The vector of inputs}
  \item{w}{The vector of weights}
}

\value{
  \item{output}{The value of the WAM function }
}
\references{
  %% ~put references to the literature/web site here ~
[1]G. Beliakov, H. Bustince, and T. Calvo. A Practical Guide to Averaging Functions.
	Springer, Berlin, Heidelberg, 2016. 
}
\author{
  %%  ~~who you are~~
    Gleb Beliakov, Daniela L. Calderon, Deakin University
}

\examples{
  n <- 4
  wowa.WAM(n, c(0.3,0.4,0.8,0.2), c(0.3,0.25,0.3,0.15) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ WAM}
\keyword{WOWA}% __ONLY ONE__ keyword per line