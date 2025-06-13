\name{cpma}
\alias{cpma}
\title{Cross Phenotype Meta-Analysis}
\description{
    Performs (signed) cross phenotype meta-analysis.
}
\usage{
    cpma(ps)
}
\arguments{
    \item{ps}{a (non-empty) numeric vector of \eqn{p}-values.}
}
\value{
    A list with class \code{htest} containing the following components:
    \item{statistic}{the value of the \eqn{\chi^2} test statistic.}
    \item{parameter}{the estimated rate \eqn{\lambda}.}
    \item{p.value}{the \eqn{p}-value for the test.}
    \item{method}{a character string indicating the type of test performed.}
    \item{data.name}{a character string giving the name of the data.}
}
\references{
Cotsapas, C., et al. 2011
Pervasive Sharing of Genetic Effects in Autoimmune Disease.
\emph{PLOS Genetics} \bold{7}(8):e1002254.
}
