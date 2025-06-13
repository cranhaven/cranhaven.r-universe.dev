\name{bestModels}
\alias{bestModels}

\title{Best BIC Models}

\description{ARIMA(p,0,q), ARFIMA(p,0,q) and ARTFIMA(p,0,q) models are fit for
various p=0,1,..., and q=0,1,... and the best models according to the BIC
criterion are selected.}

\usage{
bestModels(z, parMax = 4, nbest = 4, likAlg = c("exact", "Whittle"), 
     d=0, ...)
}

\arguments{
  \item{z}{
time series data
}
  \item{parMax}{
maximum number of parameters - see Details
}

  \item{nbest}{
number of models in selection
}
  \item{likAlg}{
likelihood method to use
}
  \item{d}{
regular differencing parameter indicating the number of times to difference
}

  \item{...}{optional arguments for artfima such as lambdaMax}
}
\details{
\eqn{numPar = K}, where K is 
the number of structural models defined by
\eqn{K = p+q+n(glp)},
where \eqn{n(glp) = 0, 1, 2} according as the model is ARIMA, ARFIMA or
ARTFIMA respectively.

These models are ranked according to the AIC/BIC criterion and the best ones are 
shown.

The plausibility is shown. This is defined for AIC by the eqn
\eqn{p(AIC) = exp(0.5*(min(AIC)-AIC))},
where AIC is the vector of AIC values. Similarly for the BIC.
}


\value{
An S3 list object, "bestmodels".
Output is provided using the print method for the
"bestmodels"
}

\note{
There are often small differences in the likelihood among a group
of 5 or more of the best models. So the "exact" and "Whittle" likelihood
methods may produce a different ranking of the models. For this reason the
"exact" likelihood method may be preferred.
}
\author{
A.I. McLeod
}

\seealso{
\code{\link{best_glp_models}}
\code{\link{print.bestmodels}}
}

\examples{
\dontrun{
data(ogden)
\dontrun{ #about 10 seconds
bestModels(ogden)
}
}
}

\keyword{ ts }
