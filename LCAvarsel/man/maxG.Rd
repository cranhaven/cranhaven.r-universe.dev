\name{maxG}
\alias{maxG}
\title{Maximum number of latent classes}

\description{
Finds the number of latent classes that are allowed to be fitted on a dataset in order for the latent class analysis model to be identifiable.
}

\usage{
maxG(Y, Gvec)
}

\arguments{
\item{Y}{A categorical data matrix.}

\item{Gvec}{A numeric vector denoting the range of number of latent classes to be fitted.}
}

\value{
A numeric vector containing the subset of number of latent classes that are allowed to be fitted on the data in order for the model to be identifiable. If no model is identifiable for the range of values provided, the function returns \code{NULL} and throws a warning.
}

\details{
In practice, different latent class analysis models are fitted by attributing different values to \eqn{G}, usually ranging from 1 to \eqn{G_{max}}. However, for a set of variables, not all the models corresponding to increasing values of \eqn{G} are identifiable. Indeed, a necessary (but not sufficient) condition for a latent class analysis model to be identifiable is:
\deqn{\prod_{j=1}^M C_j > G\Biggl(\, \sum_{j=1}^M C_j - M + 1\Biggr)}{prod(Cj) > G(sum(Cj) - M + 1)}
where \eqn{C_j}{Cj} denotes the number of categories of variable \eqn{j}, \eqn{j=1,...,M}, and \eqn{M} is the number of variables in the data \code{Y}. Another condition requires the number of observed distinct configurations of the variables in the data to be greater than the number of parameters of the model. The function returns the subset of values of vector \code{Gvec} such that both the above conditions are satisfied.
}

\references{
Bartholomew, D. and Knott, M. and Moustaki, I. (2011). Latent Variable Models and Factor Analysis: A Unified Approach. \emph{Wiley}.

Goodman, L. A. (1974). Exploratory latent structure analysis using both identifiable and
unidentifiable models. \emph{Biometrika}. 61, 215-231.
}

\examples{
data(carcinoma, package = "poLCA")
maxG(carcinoma, 1:4)
maxG(carcinoma, 2:3)
maxG(carcinoma, 5)     # the model is not identifiable
}
