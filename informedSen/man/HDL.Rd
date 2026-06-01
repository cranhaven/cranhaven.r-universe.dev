\name{HDL}
\alias{HDL}
\docType{data}
\title{
    Light Daily Alcohol and HDL Cholesterol Levels
}
\description{
    An observational study of light daily alcohol consumption (1-3 drinks per day) versus little or no alcohol, and its possible effects on HDL cholesterol levels.  The level of methylmercury is viewed as an unaffected outcome and used to test for biased treatment assignment.  Data is from NHANES 2013/2014 and 2015/2016.
}
\usage{data("HDL")}
\format{
  A data frame with 800 observations on the following 9 variables.
  \describe{
    \item{\code{SEQN}}{NHANES sequence number}
    \item{\code{nh}}{Either 1314 for NHANES 2013/2014 or 1516 for NHANES 2015/2016}
    \item{\code{z}}{Treatment indicator, z=1 for light daily alcohol or z=0 for little or no alcohol.}
    \item{\code{mset}}{Matched set indicator, 1 to 200, for 200 matched sets, each containing one treated and three controls.}
    \item{\code{age}}{Age in years.  Used in matching.}
    \item{\code{female}}{1 for female, 0 for male.  Used in matching.}
    \item{\code{education}}{NHANES 1-5 education scale.  1 is <9th grade, 3 is high school, 5 is at least a BA degree.}
    \item{\code{hdl}}{HDL cholesterol level}
    \item{\code{mmercury}}{Methylmercury level}
  }
}

\source{
US National Health and Nutrition Examination Survey.  Publicly available on-line.
}
\references{
LoConte, N. K., Brewster, A. M., Kaur, J. S., Merrill, J. K., and Alberg, A. J. (2018). Alcohol and cancer: a statement of the American Society of Clinical Oncology. Journal of Clinical Oncology 36, 83-93.

Rosenbaum, P. R. (2021). Sensitivity analyses informed by tests for bias in observational studies.  Manuscript.  See its data appendix.

Suh, I., Shaten, B. J., Cutler, J. A., and Kuller, L. H. (1992). Alcohol use and mortality from coronary heart disease: the role of high-density lipoprotein cholesterol. Annals of Internal Medicine 116, 881-887.
}
\examples{
data(HDL)
boxplot(HDL$age~HDL$z) # ages are similar
boxplot(HDL$hdl~HDL$z) # hdl is higher
boxplot(HDL$mmercury~HDL$z,log="y") # methylmercury is higher
}
\keyword{datasets}
