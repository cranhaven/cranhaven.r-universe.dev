\name{meansplot}
\alias{meansplot}
\title{
Plots a simple strip chart of the observations with group means and
grand mean.
}
\description{
A strip chart of the first argument grouped by the second argument is
produced. This function is useful for looking at experimental data with
a numeric response and a factorial predictor.
}
\usage{
meansplot(y, grp)
}
\arguments{
  \item{y}{
a vector of observed values
}
  \item{grp}{
a factor of the same length as the observation vector indicating the
treatment under which each observation was obtained
  }
}
\value{
none returned: the function is used for the plot it produces
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{stripchart}}
}
\examples{
data(PlantGrowth)
with(PlantGrowth, meansplot(weight, group))
}

