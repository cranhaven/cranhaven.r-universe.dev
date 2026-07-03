\name{simplehmm}

\docType{data}

\alias{simplehmm}

\title{Hmm toy data set from Visser et al (2000)}

\description{

	Data are 2000 observations generated from a 2-state hidden Markov model with 
	three observation categories. These data were used in the simuation study in 
	Visser et al. (2000) for testing the quality of several methods of computing 
	parameter standard errors. 
	
}

\usage{data(simplehmm)}

\format{
  A dataframe with 2000 observations on a single variable: 
  \describe{
    \item{\code{obs}}{a factor with levels 1, 2, and 3.}
  }
}

\source{
	
	Ingmar Visser, Maartje E. J. Raijmakers, and Peter C. M. Molenaar (2000).
	Confidence intervals for hidden Markov model parameters. \emph{British journal
	of mathematical and statistical psychology}, 53, p. 317-327.
	
}

\examples{
data(simplehmm)
set.seed(1)
md2 <- hmm(simplehmm, 2)
summary(md2)
}

\keyword{datasets}
