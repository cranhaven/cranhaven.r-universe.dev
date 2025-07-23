\name{plot.MTest}
\alias{plot.MTest}
\encoding{UTF-8}
\title{Plot density or empirical cumulative distribution from MTest}

\description{
  Plot density or empirical cumulative distribution from \code{Bvals} in  \code{\link{MTest} output}.
}

\usage{
\method{plot}{MTest}(x, type=1,plotly = FALSE,...)
}

\arguments{

  \item{x}{an object of the class \code{"MTest"} }
  \item{type}{Numeric; 1 if density, 2  if ecdf plot is returned}
  \item{plotly}{Logical; if \code{FALSE}, a \code{ggplotly} plot is returned}
  \item{\dots}{other arguments to be passed to the function
    \code{\link{ggplot}}   }
}


\value{
  Produces a plot.
  No values are returned.
}

\details{
  This function plots density or empirical cumulative distribution function from MTest bootstrap replications.
}



\seealso{
  \code{\link{MTest}} for procedure and examples.
}

\keyword{MTest}
