\encoding{UTF-8}
\name{fire}
\alias{fire}
\title{Danish Fire Insurance Claims Database}
\description{
  The Danish Fire Insurance Claims Database includes 2167 industrial
  fire losses gathered from the Copenhagen Reinsurance Company over the
  period 1980-1990.
}
\usage{fire}
\format{A dataframe with 2167 observations on five variables, namely:
  \itemize{
    \item{\code{Positions}}{: date.}
    \item{\code{building}}{: loss to buildings.}
    \item{\code{content}}{: loss to content.}
    \item{\code{profits}}{: loss to profits.}
    \item{\code{total}}{: total loss.}
  }
}
\references{
  de Carvalho, M. & Marques, F. (2012) Jackknife Euclidean
  likelihood-based inference for Spearman's rho.  \emph{North American
    Actuarial Journal}, \bold{16}, 487-492.
}
\seealso{\url{https://www.maths.ed.ac.uk/~mdecarv/decarvalho2012bsh.html}}
\examples{
data(fire)
attach(fire)
plot(building, contents, pch = 20, xlim = c(0, 95), ylim = c(0, 133),
     xlab = "Loss of Building", ylab = "Loss of Contents",
     main = "Danish Fire Insurance Claims")

\dontrun{
## Confidence intervals for Spearman rho; install the package
## spearmanCI, if not installed
if (!require("spearmanCI")) install.packages("spearmanCI")
spearmanCI(building, contents)
}
}
\keyword{Actuarial}
