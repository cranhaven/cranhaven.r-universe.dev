\name{dse-package}
\alias{dse.Intro}
\alias{dse-package}
\alias{dse}

\docType{package}

\title{Dynamic Systems Estimation - Multivariate Time Series Package}

\description{Functions for time series modeling, 
including multi-variate state-space and \acronym{ARMA} (\acronym{VAR}, 
\acronym{ARIMA}, \acronym{ARIMAX}) models.
}

\section{Usage}{
   library("dse") 

   library("EvalEst") 
}

\details{
A \emph{Brief User's Guide} is distributed with \pkg{dse} as a vignette.
The package implements an R/S style object approach to time series
modeling. This means that different
model and data representations can be implemented with fairly simple 
extensions to the package. 

The package includes methods for simulating, estimating, and converting 
among different model representations. These are mainly in \pkg{dse}. 
Package \pkg{EvalEst} has methods for studying estimation techniques and 
for examining the forecasting properties of models.
There are also functions for forecasting and for
evaluating the performance of forecasting models, as well as 
functions for evaluating model estimation techniques.

\tabular{ll}{
Package: \tab dse\cr
Depends: \tab R, setRNG, tframe\cr
License: \tab free, see LICENSE file for details.\cr
URL: \tab http://tsanalysis.r-forge.r-project.org/\cr
}

The main objects are:
\describe{
   \item{\link{TSdata}}{time series input and output data structure}
   \item{\link{TSmodel}}{a \acronym{DSE} model structure}
   \item{\link{TSestModel}}{model, data and some estimation information}
}

The main general methods are:
\describe{
   \item{\link{TSdata}}{create, extract a \acronym{DSE} data structure}
   \item{\link{TSmodel}}{create, extract a \acronym{DSE} model structure}
   \item{\link{simulate}}{simulate a model to produce artifical data}
   \item{\link{toSS}}{convert to a state-space model}
   \item{\link{toARMA}}{convert to an ARMA model}
   \item{\link{ARMA}}{construct an ARMA model}
   \item{\link{SS}}{construct a state-space model}
   \item{\link{l}}{evaluate a model with data}
   \item{\link{smoother}}{calculate the smoothed state estimate}
}

The main estimation methods are:
\describe{
   \item{\link{estVARXls}}{estimate an ARMA model with least squares}
   \item{\link{estVARXar}}{estimate an ARMA model with ar}
   \item{\link{estSSfromVARX}}{calculate a state-space model from an estimated VAR model}
   \item{\link{bft}}{a (usually) good \dQuote{black-box} estimated model}
   \item{\link{estMaxLik}}{estimate a model using maximum likelihood}
}


The main diagnositic methods are:
\describe{
   \item{\link{checkResiduals}}{autocorrelation diagnostics}
   \item{\link{informationTests}}{calculate several information tests for a model}
   \item{\link{McMillanDegree}}{calculate the McMillanDegree of a model}
   \item{\link{stability}}{calculate the stability of a model}
   \item{\link{roots}}{calculate the roots of a model}
}

The methods for producing and evaluating forecasts are:
\describe{
   \item{\link{l}}{evaluate a model with data (and simple forecasts)}
   \item{\link{forecast}}{calculate forecasts }
   \item{\link{featherForecasts}}{calculate forecasts starting at different periods}
   \item{\link{horizonForecasts}}{calculate forecasts at different horizons}
   \item{\link{forecastCov}}{calculate the covariance of forecasts}
   \item{\link[EvalEst]{MonteCarloSimulations}}{multiple simulations}
}

The methods for evaluating estimation methods are:
\describe{
   \item{\link[EvalEst]{EstEval}}{evaluate estimation methods}
}

The functions described in the
\emph{Brief User's Guide} and examples in the help pages should work 
fairly reliably (since they are tested regularly), however, the
code is distributed on an \dQuote{as-is} basis. 
This is a compromise which allows me to make the software
available with minimum effort. This software is not a commercial
product. It is the by-product of ongoing research.
Error reports, constructive suggestions, and comments are welcomed. 
}
\references{
    Anderson, B. D. O. and Moore, J. B. (1979) \emph{Optimal Filtering}. 
    Prentice-Hall.   
   
    Gilbert, P. D. (1993) State space and ARMA models: An overview of
    the equivalence. Working paper 93-4, Bank of Canada. Available at   \url{http://www.bankofcanada.ca/1993/03/publications/research/working-paper-199/}

    Gilbert, P. D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions. \emph{J. of Forecasting: 
    Special Issue on VAR Modelling.} \bold{14}:229--250.

    Gilbert, P.D. (2000) A note on the computation of time series model roots. 
    \emph{Applied Economics Letters}, \bold{7}, 423--424

   Jazwinski, A. H. (1970) \emph{Stochastic Processes and Filtering Theory}. 
   Academic Press.
}

\seealso{
\code{\link{TSdata}},
\code{\link{TSmodel}},
\code{\link{TSestModel.object}}
}
\concept{DSE}
\keyword{ts}


