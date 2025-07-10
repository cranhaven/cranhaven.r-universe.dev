\name{mcmcplotsPalette}
\alias{mcmcplotsPalette}
\title{Color Palette for the mcmcplots Package }

\description{ Creates a color palette for plotting functions in the \pkg{mcmcplots} package using functions in the \pkg{colorspace} package.  }

\usage{mcmcplotsPalette(n, type = c("rainbow", "sequential",
"grayscale"), seq = NULL)}

\arguments{
  \item{n}{number of colors}
  \item{type}{denotes the type of color palette to create.}
  \item{seq}{deprecated}
}

\value{
  A color palette of \code{n} colors.
}

\author{
  S. McKay Curtis
}

\references{
  Zeileis, A., Hornik, K. and Murrell, P. (2009) "Escaping RGBland: Selecting colors for statistical graphs." Compuational Statistics & Data Analysis, 53, 3259--3270.
}

\seealso{
  \code{rainbow_hcl}, \code{sequential_hcl}
}

\examples{
colorpie <- function(n, type="rainbow") pie(rep(1, n), col=mcmcplotsPalette(n, type=type))
colorpie(1)
colorpie(8)
colorpie(4, type="sequential")
colorpie(4, type="grayscale")

## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- coda::as.mcmc.list(
    lapply(1:3,
           function(i) coda::mcmc(matrix(rnorm(nc*nr, rep(means,each=nr)),
                                         nrow=nr, dimnames=list(NULL,pnames)))))

denplot(fakemcmc)
denplot(fakemcmc, style="plain", col=mcmcplotsPalette(3, type="sequential"))
denplot(fakemcmc, style="plain", col=mcmcplotsPalette(3, type="grayscale"))

}

\keyword{ color }