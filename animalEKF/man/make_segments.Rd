\name{make_segments}
\alias{make_segments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot path connecting points on ggplot.
}
\description{
Convert x-y coordinates to a \code{data.frame} for \code{geom_path} plotting on \code{\link[ggplot2]{ggplot}}.
}
\usage{
make_segments(xy, N=nrow(xy))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
Matrix or \code{data.frame} of x-y points to plot as a path.  If there are any rows that are NA-valued,
the resulting path will consist of disconnected segments in those locations.
}
  \item{N}{
Number of rows of \code{xy} to plot as a path.   By default, the number of rows, so all of \code{xy}.
}
}
\author{
Samuel Ackerman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

#generate toy dataset
library(ggplot2)
d <- as.data.frame(cbind(X=runif(50), Y=runif(50)))

#create segments with some missing lines so will be some gaps)
dseg <- d
dseg[ sample(20),] <- NA

g <- ggplot(d, aes(x=.data$X, y=.data$Y)) + theme_bw() 
g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)
g <- g + scale_fill_gradient(low="white", high="black") + theme(legend.position="bottom")
g <- g + geom_path(data=make_segments(xy=dseg), aes(x=.data$X, y=.data$Y), colour="red", lwd=1.5) 
g

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot}% use one of  RShowDoc("KEYWORDS")
