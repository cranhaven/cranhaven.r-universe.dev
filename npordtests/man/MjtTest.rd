\name{MjtTest}
\alias{MjtTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modified Jonckheere-Terpstra (MJT) Test}
\description{
\code{MjtTest} performs MJT test.
}

\usage{MjtTest(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}}
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
  \code{information.gain} is \deqn{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Class) + H(Attribute) - H(Class, Attribute)}.

  \code{gain.ratio} is \deqn{\frac{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Attribute)}}{(H(Class) + H(Attribute) - H(Class, Attribute)) / H(Attribute)}

  \code{symmetrical.uncertainty} is \deqn{2\frac{H(Class) + H(Attribute) - H(Class, Attribute)}{H(Attribute) + H(Class)}}{2 * (H(Class) + H(Attribute) - H(Class, Attribute)) / (H(Attribute) + H(Class))}
}


\value{
A list with class "owt" containing the following components:

\item{statistic}{the MJT test statistic.}
\item{mean}{the mean of the MJT test statistic.}
\item{variance}{the variance of the MJT  test statistic.}
\item{Z}{the standardized test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance.}
\item{method}{the character string "MJT test ".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Jonckheere, A. R. (1954). A Distribution-Free k-Sample Test Against Ordered Alternatives. \emph{Biometrika}, \bold{41}, 133-145.

Neuhauser, M., Liu, P.Y., Hothorn, L.A.(1998). Nonparametric Tests for Trend: Jonckheere's Test, a Modification and a Maximum Test. \emph{Biometrical Journal}, \bold{40:8}, 899-909.

Tryon, V. P., Hettmansperger, T. P. (1973). A class of nonparametric tests for homogeneity against ordered alternatives. \emph{Annals of Statistics}, \bold{1},  1061-1070.

}


\author{
Bulent Altunkaynak}



\examples{

library(npordtests)

## Data from Jonckheere (1954)
data(jdata)
MjtTest(Y~X,jdata)

## Data from Lehmann (1975)
data(lehmann)
MjtTest(Values~Group,lehmann)

}

\keyword{functions}
