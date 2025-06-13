\name{pqq}
\alias{pqq}
\alias{pqq.ci}
\title{
    Quantile-Quantile Plot for \eqn{p}-values
}
\description{
    Computes value to produce a quantile-quantile plot in \eqn{-log_10} scale of
    the given \eqn{p}-values.
}
\usage{
    pqq(ps)

    pqq.ci(n, level=0.95)
}
\arguments{
    \item{ps}{a (non-empty) numeric vector of \eqn{p}-values.}
    \item{n}{number of \eqn{p}-values.}
    \item{level}{significance level.}
}
\value{
    For \code{pqq}, a list containing the following components (all in
    \eqn{-log_10} scale):
    \item{x}{a vector of (sorted) expected \eqn{p}-values.}
    \item{y}{a vector of (sorted) observed \eqn{p}-values.}

    For \code{pqq.ci}, a matrix with the lower and upper bounds for the
    confidence interval at the given significance level.
}
