\name{envplot}
\alias{envplot}
\title{Plot Empirical Variogram and its Pointwise Envelope}
\description{
Plot empirical variogram and its pointwise variogram envelope.
}
\usage{
envplot(envlist, shade = TRUE, shade.color = "lightgrey",
        show.variance = FALSE, xlim = NULL, ylim = NULL,
        main = NULL, xlab = "Distance", ylab = "Semivariance")
}

\arguments{
\item{envlist}{output from \code{envelope} function.}
\item{shade}{logical. If \code{TRUE}, the envelope is shaded.}
\item{shade.color}{string. Color of the envelope if it is shaded.}
\item{show.variance}{logical. Plot horizontal lines showing the sample variance and its confidence interval of residuals ignoring spatial information.}
\item{xlim, ylim}{x-axis and y-axis range. If \code{NULL}, default values are chosen.}
\item{main}{string. Title of the plot.}
\item{xlab, ylab}{string. Labels for x- and y-axis.}
}

\value{ Nothing is returned, generates a plot.}

\author{ Craig Wang }
\seealso{
\code{\link{envelope}} to use Monte Carlo permutations for generating variogram envelope.
}
\examples{
\dontrun{
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
vario0 <- gstat::variogram(log(zinc)~1, meuse)
varioEnv <- envelope(vario0, data = meuse, formula = log(zinc)~1, nsim = 499)
envplot(varioEnv)
}
}
