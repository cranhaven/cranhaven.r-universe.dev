\name{example_data_mmdml}
\alias{example_data_mmdml}
\title{Generate data from partially linear mixed-effects model}
\usage{
example_data_mmdml(beta0, N = 10L, n = 5L)
}
\arguments{
\item{beta0}{Numeric vector of length 1 or 2 representing the linear
    coefficient/fixed effects of the model.}

\item{N}{Number of experimental units. Equals 10 per default.}

\item{n}{Expected number of observations per experimental unit,
    needs to be at least 5. Equals 5 per default.}
}
\value{
A data frame with the columns \code{resp} (the response), \code{id} and
    \code{cask} (random effects), \code{w1}, \code{w2}, and \code{w3}
    (nonparametric confounders), and \code{x1} if \code{beta0} is of length
    1 and \code{x1} and \code{x2} if \code{beta0} is of length 2.
    The random effects are modelled with \code{"(1|id) + (1|cask:id)"}.
}
\description{
Generate data from a partially linear mixed-effects model with one or two
    fixed effects, 2 random effects, and 3 nonparametric variables.
    The true underlying function of the nonparametric variables are step
    functions.
    The random effects and error terms are from a Gaussian distribution.
}
\examples{
## See example(mmdml) for examples
}
\seealso{
\code{\link{mmdml}}
}

