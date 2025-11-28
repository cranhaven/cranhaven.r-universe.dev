\name{K}
\alias{K}
\alias{print.K}
\alias{summary.K}
\title{Coefficient of divergence}
\description{Lubischew's coefficient of divergence (SSMD^2)}

\usage{
K(x, y=NULL, data=NULL, mad=FALSE, na.rm=TRUE)
\method{print}{K}(x, ...)
\method{summary}{K}(object, ..., num=2)
}

\arguments{
\item{x}{Numeric vector, or formula, or object of the class 'K'}
\item{y}{Second numeric vector, or nothing}
\item{data}{Data with two columns (in case of formula)}
\item{mad}{Non-parametric variant of K (not Lubischew's)}
\item{na.rm}{Remove NAs?}
\item{object}{Object of the class 'K'}
\item{num}{Digits to round}
\item{...}{Additional arguments}
}

\details{
One of the effect size measures, Lubischew's K, coefficient of divergence (Lubischew, 1959).
Interestingly, the recently invented "striclty standardized mean difference"
SSMD (see, for example, "https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference")
is just a square root of K.
}

\value{
K() returns value of K, or nothing. summary.K() returns also magnitude and P, "probability of misclassification".
}

\references{Lubischew A. A. 1959. How to apply biometry to systematics. Leningrad University Herald. N 9. P. 128--136. [In Russian, English abstract].}

\author{Alexey Shipunov}

% \seealso{}

\examples{
K(1:3, 2:100)
sapply(eq[, -1], function(.x) K(.x ~ eq[, 1]))
summary(K(x17 ~ Species, data=haltica), num=5)
}

\keyword{univar}
