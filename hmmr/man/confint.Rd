\name{confint}

\docType{data}

\alias{confint}

\title{Confidence intervals Visser et al (2000)}

\description{

	Values from Table 1 of Visser et al (2000) with confidence intervals obtained using
	four different methods; 1) likelihood profiles, 2) bootstrap with 500 samples, 
	3) bootstrap with 1000 samples, 4) finite differences approximation of the hessian. 
	
}

\usage{data(confint)}

\format{
  A data.frame with 9 variables for each parameter of a 2-state hidden Markov model fitted
  to data \code{\link{simplehmm}}:
  \describe{
    \item{par}{parameter name, see Visser et al (2000) for details.}
	\item{true}{true value of the parameter.}
	\item{mle}{maximum likelihood estimate of the parameter.}
	\item{prof}{the size of the confidence interval resulting from the profile likelihood method.}
	\item{left}{the left hand boundary of the (95\%) confidence interval using the likelihood profile method.}
	\item{right}{the right hand boundary of the (95\%) confidence interval using the likelihood profile method.}
	\item{b500}{the size of the confidence interval resulting from 500 bootstrap samples.}
	\item{b1000}{the size of the confidence interval resulting from 1000 bootstrap samples.}
	\item{fdh}{the size of the confidence interval based on the finite differences approximation of the hessian.}
							
  }
}

\source{
	
	Ingmar Visser, Maartje E. J. Raijmakers, and Peter C. M. Molenaar (2000).
	Confidence intervals for hidden Markov model parameters. \emph{British journal
	of mathematical and statistical psychology}, 53, p. 317-327.
	
}

\examples{
data(confint)
}

\keyword{datasets}
