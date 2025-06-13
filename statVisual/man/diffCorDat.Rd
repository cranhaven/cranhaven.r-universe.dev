\name{diffCorDat}
\alias{diffCorDat}
\docType{data}
\title{
A Dataset for Differential Correlation Analysis
}
\description{
A dataset for differential correlation analysis.
}
\usage{data("diffCorDat")}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{probe1}}{numeric. expression level for probe1}
    \item{\code{probe2}}{numeric. expression level for probe2}
    \item{\code{grp}}{character. a factor with levels \code{cases} \code{controls}}
  }
}
\details{
The simulated data set contains expression levels of 2 gene probes for 50 cases and 50 controls. The expression levels of probe1 are generated from \eqn{N(0, 1)}. The expression levels of probe2 for controls are also generated from \eqn{N(0, 1)}. The expression levels of probe 2 for cases are generated from the formula \eqn{probe2_{i} = -probe1_{i} + e_i}{probe2i = - probe1i + ei}, \eqn{i=1, \ldots, nCases}{i=1, ..., nCases}, where \eqn{e_i\sim N(0, 0.3^2)}{ei ~ N(0, 0.3^2)}.

That is, the expression levels of probe 1 and probe 2 are negatively correlated in cases, but not correlated in controls.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(diffCorDat)

print(dim(diffCorDat))
print(diffCorDat[1:2,])
}
\keyword{datasets}
