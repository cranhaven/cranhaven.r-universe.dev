\name{getLambda}
\alias{getLambda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
select lambda based on AIC or BIC
}
\description{
Select the regularization parameter for HTK-means clustering based on information criteria.
}
\usage{
getLambda(HTKmeans.out, type  = "AIC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{HTKmeans.out}{ the output of a call to \code{\link{HTKmeans}.}
}
\item{type}{ either \code{"AIC"} (default) or 
\code{"BIC"}.}
}
\details{
This function selects the best lambda (based on information
criteria AIC or BIC) out of the \code{HTKmeans.out$inputargs$lambdas} sequence of values. 
}
\value{
The selected value for lambda
}
\references{
Raymaekers, Jakob, and Ruben H. Zamar. "Regularized K-means through hard-thresholding." arXiv preprint arXiv:2010.00950 (2020).
}
\author{
J. Raymaekers and R.H. Zamar
}

\seealso{
\code{\link{HTKmeans}}
}
\examples{
X <- mclust::banknote
y <- as.numeric(as.factor(X[, 1]))
lambdas <- seq(0, 1, by = 0.01)
X <- X[, -1]
HTKmeans.out <- HTKmeans(X, 2, lambdas)

# Both AIC and BIC suggest a lambda of 0.02 here:

\donttest{
getLambda(HTKmeans.out, "AIC")
getLambda(HTKmeans.out, "BIC")
}
}