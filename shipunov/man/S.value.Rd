\name{S.value}
\alias{S.value}

\title{S-value}

\description{
  \code{S.value} returns S-values, Shannon information transforms of p-values.
}

\usage{
S.value(x)
}

\arguments{
  \item{x}{Either numerical vector of p-values, or list where at least one element has the name similar to "p.value".}
}

\details{
  Greenland (2019) proposes that researchers "think of p-values as measuring the _compatibility_ between
  hypotheses and datas." S-values should help to understand this concept better.

  From Wasserstein et al. (2019): S-values supplement a focal p-value p with its Shannon information transform
  (s-value or surprisal) s = -log2(p). This measures the amount of information supplied by the test against the tested
  hypothesis (or model): rounded off, the s-value shows the number of heads in a row one would need to see when tossing
  a coin to get the same amount of information against the tosses being ``fair'' (independent with ``heads'' probability
  of 1/2) instead of being loaded for heads. For example, if p = 0.03, this represents -log2(0.03) = 5 bits of information
  against the hypothesis (like getting 5 heads in a trial of ``fairness'' with 5 coin tosses); and if p = 0.25, this
  represents only -log2(0.25) = 2 bits of information against the hypothesis (like getting 2 heads in a trial of
  ``fairness'' with only 2 coin tosses).

  For the convenience, S.value() works directly with output of many statistical tests (see examples). If the output is a list
  which has more than one component with name similar to "pvalue", only first will be used.
}

\value{
  Numerical vector.
}

\references{
Wasserstein R.L., Schirm A.L., Lazar N.A. 2019. Moving to a World Beyond ``p < 0.05''.
The American Statistician. 73(S1): 1--19.

Greenland S. 2019. Valid P-Values Behave Exactly as They Should: Some Misleading Criticisms of P-Values
and Their Resolution With S-Values. The American Statistician. 73(S1): 106--114.
}

\author{Alexey Shipunov}

% \seealso{}

\examples{
S.value(0.05)

S.value(0.01)
S.value(0.1)
S.value(0.00000000001)

S.value(t.test(extra ~ group, data = sleep))
S.value(list(pvalues=c(0.01, 0.002)))
}

\keyword{htest}
