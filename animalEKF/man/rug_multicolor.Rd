\name{rug_multicolor}
\alias{rug_multicolor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multicolor rug of tick marks.
}
\description{
Adapt \code{rug} function to allow tick marks to be of different colors.
}
\usage{
rug_multicolor(x, plot_side=3, ticksize=-0.04, col_vec=rep(1, length(x)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector of axis tick mark locations.
}
  \item{plot_side}{
Which side to plot on.  1=bottom, 2=left, 3=top, and 4=right.
}
  \item{ticksize}{
Size of tick marks.  Negative values mean ticks are on outside of plot.  This feeds into the \code{tck} parameter of the \code{\link{axis}} function.
}
  \item{col_vec}{
Vector of color definitions, corresponding to each value of \code{x}.
}
}
\author{
Samuel Ackerman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

d <- data.frame(X=runif(20), Y=runif(20))
plot(d, xlim=c(0,1), ylim=c(0,1))

# draw rug of ticks on each axis where the coordinates are
rug_multicolor(x=d$X, col_vec=colorspace::rainbow_hcl(n=20), ticksize=-0.05)
rug_multicolor(x=d$Y, plot_side=4, col_vec=colorspace::rainbow_hcl(n=20), ticksize=-0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ color }% __ONLY ONE__ keyword per line
