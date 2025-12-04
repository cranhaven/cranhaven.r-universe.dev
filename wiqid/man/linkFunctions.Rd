\name{Links}
\alias{Links}
\alias{links}
\alias{links}

\title{ Logit and probit links for generalised linear modelling }

\description{

Generalised linear models with binomial response variables ("logistic regression") use a link function to link the response on a (0,1) scale to a linear predictor on (-Inf, Inf). The canonical link is the logistic ("logit") function, which has some nice theoretical properties and can be interpreted as the log of the odds of success. Other links are available, notably the cumulative standard normal ("probit") link, which allows for Gibbs sampling with truncated normal distributions. For that reason, several of the Bayesian estimation functions in \pkg{wiqid} use the probit link.

The form of the logit and probit links are shown in the figure below.

\figure{linkFunctions.jpg}

Both curves are symmetric, with probability = 0.5 when the linear predictor = 0. The probit curve is steeper, so coefficients in a probit regression will be smaller than those in a logit regression (by a factor of about 1.7).

}

\keyword{ manip }
\keyword{ models }

