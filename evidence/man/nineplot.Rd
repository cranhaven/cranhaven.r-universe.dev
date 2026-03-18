\name{nineplot}
\alias{nineplot}
\title{
produces a Normality plot for the argument surrounded by eight other Normality plots for Normal distributions having the same mean and standard deviation as the argument
}
\description{
Normality plots can be hard to judge if one is not experienced. This
function plots a Normality plot for the data surrounded by eight other
Normality plots for samples with the same mean and standard deviation
that were randomly generated. The eight plots provide an idea of the
variability to be expected in Normally distributed data.
}
\usage{
nineplot(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of observations to be examined for Normality
}
}
\value{
none produced: the function is used for the plot it produces
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{qqnorm}}
}
\examples{
nineplot(rt(100, 2))
}

\keyword{ hplot }
