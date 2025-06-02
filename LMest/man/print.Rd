\name{print}
\alias{print.LMbasic}
\alias{print.LMbasiccont}
\alias{print.LMlatent}
\alias{print.LMlatentcont}
\alias{print.LMmanifest}
\alias{print.LMmixed}
\alias{print.MCbasic}
\alias{print.MCcov}
\alias{print.LMsearch}

\title{Print the output}
\description{Given the output, it is written in a readable form}

\usage{
\method{print}{LMbasic}(x, ...)
\method{print}{LMbasiccont}(x, ...)
\method{print}{LMlatent}(x, ...)
\method{print}{LMlatentcont}(x, ...)
\method{print}{LMmanifest}(x, ...)
\method{print}{LMmixed}(x, ...)
\method{print}{MCbasic}(x, ...)
\method{print}{MCcov}(x, ...)
\method{print}{LMsearch}(x, modSel = "BIC",...)
}


\arguments{
\item{x}{output from \code{\link{lmest}},\code{\link{lmestCont}},\code{\link{lmestMixed}}, and \code{\link{lmestMc}}}
\item{modSel}{a string indicating the model selection criteria: \code{"BIC"} (default) for Bayesian Information Criterion and \code{"AIC"} for Akaike Information Criterion Criterion}
\item{...}{further arguments passed to or from other methods}
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}
