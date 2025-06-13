\name{best_glp_models}
\alias{best_glp_models}

\title{
Best AIC/BIC Models for Specified GLP
}
\description{
This function is used by bestModels
}
\usage{
best_glp_models(z, glp = c("ARTFIMA", "ARFIMA", "ARIMA"), p = 2, q = 2, 
   likAlg = c("exact", "Whittle"), d=0, ...)
}

\arguments{
  \item{z}{
time series
}
  \item{glp}{
 glp is equal to one of the following choices: "ARTFIMA", "ARFIMA" or "ARIMA"
}
  \item{p}{
maximum order of AR component
}
  \item{q}{
maximum order of MA component
}
  \item{likAlg}{
   likAlg = c("exact", "Whittle"))
either "exact" or "Whittle"
}
  \item{d}{
regular integer differencing parameter
}
  \item{...}{optional arguments for artfima such as lambdaMax}
}

\value{
A list with 4 entries:
\item{LL}{log-likelihood of models}
\item{artfima_time}{total time}
\item{aic}{list with best aic models}
\item{bic}{list with best bic models}
Each of the components aic and bic is a list with three components:
\item{bestaic}{best aic models}
\item{bestbicModel}{best model}
\item{aic}{plausability}
Similarly for the bic component.
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{bestModels}}
}

\examples{
\dontrun{
#takes about 4 minutes. Checking result for bestmodels()
z<-tseg(1000, "BJARMA11")
ansARIMA <- best_glp_models(z, glp = "ARIMA", p=2, q=2)
ansARFIMA <- best_glp_models(z, glp = "ARFIMA", p=2, q=2)
ansARTFIMA <- best_glp_models(z, glp = "ARTFIMA", p=2, q=2)
ansARIMA$bic$bic
ansARFIMA$bic$bic
ansARTFIMA$bic$bic
bestModels(z)
}
}

\keyword{ ts }
