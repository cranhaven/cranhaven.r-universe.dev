\name{hue}
\docType{data}
\alias{hue}
\encoding{UTF-8}
\title{Hue color data}
\description{
An observational study conducted at the Vegetable Production Department
at "Luiz de Queiroz" College of Agriculture/University of São Paulo in
2010/2011 to evaluate the peel color of 20 papaya fruits cv. Sunrise Solo over
time. The color hue was measured on the equatorial region of each fruit
using four points observed by the colorimeter and 1,000 points observed
by the scanner. Thus, the circular mean hue was calculated for each
fruit by each device at time \eqn{t}. The aim of the agreement study was to
assess how well the colorimeter agreed with the scanner over time. 
}
\usage{data(hue)}
\format{
  A data frame with 554 observations on the mean hue variable.
The format is:
  \tabular{lll}{
\code{H_mean} \tab numeric; mean hue of papaya's peel \cr 
\code{Method} \tab a factor with levels \code{Colorimeter},
\code{Scanner} \cr
\code{Time} \tab integer; time in days \cr
\code{Fruit} \tab a factor with 20 levels; from \code{1} to \code{20} \cr
\tab where each level is represented by one fruit.
}
}

\examples{
data(hue)
summary(hue)
str(hue)
## Second degree polynomial model with random intercept, slope and
## quadratic term including an exponential variance function using
## time as covariate.
model<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
          method = "Method", time = "Time", qf = 2, qr = 2,
          components = TRUE, time_lcc = list(from = min(hue$Time),
          to = max(hue$Time), n=40), var.class=varExp,
          weights.form="time")
summary(model, type="model")
summary(model, type="lcc")
## for discussion on the analysis of complete data set,
## see Oliveira et al. (2018)
}
\source{
Oliveira, T.P.; Hinde, J.; Zocchi S.S. Longitudinal Concordance
Correlation Function Based on Variance Components: An Application in
Fruit Color Analysis. Journal of Agricultural, Biological, and
Environmental Statistics, v. 23, n. 2, 233–254, 2018.

Oliveira, T.P.; Zocchi S.S.; Jacomino, A.P. Measuring color hue in
'Sunrise Solo' papaya using a flatbed
scanner. \emph{Rev. Bras. Frutic.}, v. 39, n. 2, e-911, 2017. 
}

\references{
Oliveira, T.P.; Hinde, J.; Zocchi S.S. Longitudinal Concordance
Correlation Function Based on Variance Components: An Application in
Fruit Color Analysis. Journal of Agricultural, Biological, and
Environmental Statistics, v. 23, n. 2, 233–254, 2018.
}
\seealso{
  \code{\link[lcc]{lcc}}.
}
\keyword{datasets}
