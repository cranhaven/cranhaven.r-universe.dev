\name{fdr.adjust}
\alias{fdr.adjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to calculate FDR adjusted p-values}
\description{Calculates FDR adjusted p-values similar to R-function
  p.adjust but *without* adjustment for multiplicity.}
\usage{
fdr.adjust(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{Vector of p-values.}
}
\details{It is assumed that the p-values are already corrected for
  multiplicity. P-values with a value of 1 are currently ignored.}
\value{Vector of p-values.}
\references{
  Meinshausen, N., Meier, L. and \enc{Bühlmann}{Buhlmann}, P. (2009),
  \emph{P-values for high-dimensional regression}, Journal of the
  American Statistical Association 104, 1671-1681.
}

\author{Lukas Meier}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{p.adjust}}}
\examples{
x <- matrix(rnorm(100 * 1000), nrow = 100, ncol = 1000)
y <- x[,1] * 2 + x[,2] * 2.5 + rnorm(100)

## Multi-splitting with lasso.firstq as model selector function
fit.multi <- multi.split(x, y, model.selector =lasso.firstq,
                         args.model.selector = list(q = 10))
p.adjust <- fdr.adjust(fit.multi$pval.corr)
}

\keyword{models}
\keyword{regression}% __ONLY ONE__ keyword per line
