\name{Priors}
\alias{Priors}
\alias{Standardisation}
\alias{Standardization}

\title{ Standardisation and priors }

\description{

This page documents the priors used in the Bayesian analysis. For logistic models, sensible priors depend on the range of values and thus on the standardisation scheme used, which is also detailed here.

At present, this represents good intentions! It has not been implemented in all the functions in \pkg{wiqid}.
}

\section{Standardisation}{

Continuous variables are standardised to facilitate writing models, and optimisation. Standardisation also means that the size of regression coefficients directly reflect the importance of the corresponding variables.

Binary variables coded as TRUE/FALSE and dummy variables are not changed. To make continuous variables comparable with these, they are centred by subtracting the mean, and then divided by their standard deviation.

Update: In versions prior to 0.2.x, continuous variables were centred by subtracting the mean, and then divided by \emph{two times} their standard deviation (Gelman, 2008). With the new default, beta coefficients will be exactly half the size. There may be some rounding errors in the fourth decimal place for other estimates.

Note that all numerical inputs (ie, \code{is.numeric == TRUE}) that appear in the \code{data} argument will be standardised, including binary variables coded as 0/1. Variables coded as TRUE/FALSE or as factors are not affected.

The same standardisation strategy is used for both Bayesian and maximum likelihood functions.
}

\section{Priors for logistic regression coefficients}{

Following Gelman et al (2008), we use independent Cauchy priors with centre 0 and scale 10 for the intercept and scale 2.5 for all other coefficients.

}

\section{Priors for probabilities}{

We use independent Uniform(0, 1) priors for probabilities.

}

\references{

Gelman, A. (2008) Scaling regression inputs by dividing by two standard deviations. \emph{Statistics in Medicine}, 27, 2865-2873

Gelman, Jakulin, Pittau and Su (2008) A weakly informative default prior distribution for logistic and other regression models. \emph{Annals of Applied Statistics} 2, 1360-1383.

}

\keyword{ manip }
\keyword{ models }

