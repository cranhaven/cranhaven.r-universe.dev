\name{hypertension}
\alias{hypertension}
\docType{data}
\title{Hypertension data}
\description{
This data presented by Dmitrienko et al. (2006) is to examine the effect of different drug doses on diastolic blood pressure.
}
\usage{data("hypertension")}
\format{
  A data frame with 68 observations on the following 2 variables.
  \describe{
    \item{\code{doseLevel}}{Dose levels: \code{A}: Placebo, \code{B}: Dose 10 mg/day, \code{C}: Dose 20 mg/day, \code{D}: Dose 40 mg/day}
    \item{\code{rdbp}}{Mean reduction in diastolic blood pressure}
  }
}
\details{
Patients with hypertension were randomized into four groups with different dose levels, 0, 10, 20, and 40 mg/day, where the group with 0 mg/day was the placebo group. The number of patients in each group were 17, 17, 18, and 16, respectively. The complete data can be found at the Dmitrienko et al.(2006) or Shan et al. (2014).
}
\source{
Dmitrienko, A., Chuang-Stein, C. and D’Agostino, R. (2006). \emph{Pharmaceutical Statistics Using SAS: A Practical Guide (SAS Press)}. SAS Institute, 1 edition.
}
\references{
Shan, G., Young, D. and Kang, L. (2014). A New Powerful Nonparametric Rank Test for Ordered Alternative Problem. \emph{PLoS ONE}, \bold{9:11}, doi:10.1371/journal.pone.0112924.
}
\examples{
data(hypertension)
levels(hypertension$doseLevel) <- c("placebo", "10", "20", "40")
boxplot(rdbp~doseLevel,data=hypertension,xlab="Dose level",ylab="Mean reduction in dbp")
}
\keyword{datasets}
