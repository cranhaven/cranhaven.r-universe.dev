\name{balanceMittnik}
\alias{balanceMittnik}
\alias{SVDbalanceMittnik}

\title{Balance a state space model}
\description{Balance a state space model a la Mittnik.}

\usage{
    balanceMittnik(model, n=NULL)
    SVDbalanceMittnik(M, m, n=NULL)
}
\arguments{
    \item{model}{An TSmodel object.}
    \item{M}{a matrix. See details in \code{MittnikReduction}.}
    \item{m}{an integer indicating the number of input series in the model.}
    \item{n}{see details}
}
\value{A state space model in a TSestModel object.}
\details{
\code{balanceMittnik} calculate a state space model balance a la Mittnik.
\code{n} is intended primarily for producing a state space model from the markov
parameters of an \code{ARMA} model, but if it is supplied with an \code{SS} model the
result will be a model with state dimension n based on the n largest
singular values of the svd of a Hankel matrix of markov parameters generated 
by the original model. If \code{n} is not supplied then the singular values are
printed and the program prompts for \code{n}. \code{balanceMittnik} calls
\code{SVDbalanceMittnik}

\code{SVDbalanceMittnik} calculates a nested-balanced state space 
model by svd a la Mittnik. If state dim n is supplied then svd 
criteria are not calculated and the given n is used. Otherwise, 
the singular values are printed and the program prompts for n.
M is a matrix with p x (m+p)  blocks giving the markov parameters,
that is, the first row of the Hankel matrix. It can be generated from the
model as in the function markovParms, or from the data, as in the function
estSSMittnik.
m is the dimension of input series, which is needed to decompose M.
The output dimension p is taken from nrow(M).

See also \code{MittnikReduction} and references.
}
\references{
    See references for \code{\link{MittnikReduction}}. 
}
\seealso{
    \code{\link{estVARXls}},
    \code{\link{estVARXar}}
    \code{\link{MittnikReduction}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- toSS(TSmodel(estVARXls(eg1.DSE.data.diff)))
    # this prints information about singular values and prompts with
    #Enter the number of singular values to use for balanced model:
    \donttest{newmodel <-balanceMittnik(model)}
    # 18 might be a good choice in this example. 
    newmodel <-balanceMittnik(model, n=18)

}
\concept{DSE}
\keyword{ts}


