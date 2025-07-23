\name{plot.clusEvol}
\alias{plot.clusEvol}
\encoding{UTF-8}
\title{Plot output results from clusEvol}

\description{
  Plot density or empirical cumulative distribution from \code{Bvals} in  \code{\link{clusEvol} output}.
}

\usage{
\method{plot}{clusEvol}(x,target,type = "heat",plotly=FALSE,...)
}

\arguments{

  \item{x}{an object of the class \code{"clusEvol"} }
  \item{target}{Numeric; 1 if density, 2  if ecdf plot is returned}
  \item{type}{Character; \code{heat} (default), \code{line}, \code{boxplot} are the options}
  \item{plotly}{Logical; if \code{FALSE}, a \code{ggplotly} plot is returned}
  \item{\dots}{other arguments to be passed to the function
    \code{\link{ggplot}}   }
}


\value{
  Produces a plot.
  No values are returned.
}

\details{
  This function plots outputs from clusEvol taking into account its panel data structure.
}



\seealso{
  \code{\link{clusEvol}} for procedure and examples.
}

\keyword{clusEvol}
