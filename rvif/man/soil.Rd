\encoding{utf8}
\name{soil}
\alias{soil}
\docType{data}
\title{
Soil characteristics data
}
\description{
Data used in Bondell and Reich's paper on soil characteristics used as predictors of forest diversity.
}
\usage{data("soil")}
\format{
  A data frame with 20 observations on the following 16 variables.
  \describe{
    \item{\code{BaseSat}}{\% Base Saturation.}
    \item{\code{SumCation}}{Sum Cations (sums of cations like calcium, magnesium, potassium and sodium).}
    \item{\code{CECbuffer}}{CEC.}
    \item{\code{Ca}}{Calcium.}
    \item{\code{Mg}}{Magnesium.}
    \item{\code{K}}{Potassium.}
    \item{\code{Na}}{Sodium.}
    \item{\code{P}}{Phosphorus.}
    \item{\code{Cu}}{Copper.}
    \item{\code{Zn}}{Zinc.}
    \item{\code{Mn}}{Manganese.}
    \item{\code{HumicMatter}}{Humic Matter.}
    \item{\code{Density}}{Density.}
    \item{\code{pH}}{pH.}
    \item{\code{ExchAc}}{Exchangeable Acidity.}
    \item{\code{Diversity}}{Forest diversity (dependent variable).}
  }
}
\details{
This dataset is originally used by Bondell and Reich (2008).
}
\references{
Bondell, H.D. and Reich. B.J. (2008). Simultaneous regression shrinkage, variable selection, and supervised 
clustering of predictors with OSCAR. Biometrics, 64 (1), 115--23, doi: https://doi.org/10.1111/j.1541-0420.2007.00843.x. 
}
\examples{
  head(soil, n=5)
  y = soil[,16]
  x = soil[,-16] 
  x = cbind(rep(1, length(y)), x) # the design matrix has to have the intercept in the first column
  multicollinearity(y, x)
  multicollinearity(y, x[,-3]) # eliminating the problematic variable (SumCation)
}
\keyword{datasets}
