\name{binom_two_singlestage-class}
\Rdversion{1.1}
\docType{class}
\alias{binom_two_singlestage}
\alias{binom_two_singlestage-class}
\title{Class \code{"binom_two_singlestage"}}
\description{
This class is created from the function \code{\link{freq_binom_two_singlestage}}. This is an intermediate stage to generate an object of class \code{\linkS4class{trialDesign_binom_two}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("binom_two_singlestage", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{optimal}:}{Object of class \code{"data.frame"}, Optimal trial design }
    \item{\code{output}:}{Object of class \code{"data.frame"}, list of all acceptable trial designs }
  }
}
\section{Methods}{
  \describe{
    \item{properties}{\code{signature(x = "binom_two_singlestage")}(x, t, r, pra, prb,
	pta, ptb, futility_critical_value = 0.2, 
    efficacy_critical_value = 0.35, toxicity_critical_value = 0.3, 
    no_toxicity_critical_value = 0.1) }
	
	\item{x}{Class object which you wish to get properties for}
\item{t,r}{A vector of the probability of response and toxicity for the simulation scenarios used to compute frequentist properties. The print function requires the first to be the alternative hypothesis and subsequent entries to be the three null hypotheses. This can be run with any scenario when not using the print method}
\item{reviews}{A vector of the number of patients each interim and final analysis will occur at}
\item{pra, prb, pta, ptb}{Numeric values for the beta prior distribution to be used}
\item{futility_critical_value, efficacy_critical_value,
toxicity_critical_value,
no_toxicity_critical_value}{Four values, for the critical values to be used as thresholds for the posterior distribution}

Returns an object of class \code{\linkS4class{trialDesign_binom_two}}.

	 }
}
\examples{
showClass("binom_two_singlestage")
}
\keyword{classes}
