\name{print.bestmodels}
\alias{print.bestmodels}

\title{
Print Method for "bestmodels" Object
}
\description{
Methods function for bestModels.
}

\usage{\method{print}{bestmodels}(x, ...)}

\arguments{
  \item{x}{
produced by bestModels
}
  \item{\dots}{
additional arguments
}
}

\details{
The plausibility is shown. This is defined for AIC by the eqn
\eqn{p(AIC) = exp(0.5*(min(AIC)-AIC))},
where AIC is the vector of AIC values. Similarly for the BIC.
}
\value{
Data frame with 6 rows and 5 columns. The first column corresonds to best 
models,
second the second best, etc. The rows corresond respectively to the
chosen AIC models, AIC values, AIC plausibility, BIC models, BIC values and BIC
plausibility
}
\author{
A. I. McLeod
}

\seealso{
\code{\link{bestModels}}
}
\examples{
\dontrun{ #takes about 10 seconds
data(ogden)
ans<-bestModels(ogden)
ans
}
}
\keyword{ ts }
