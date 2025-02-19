\encoding{UTF-8}
\name{diabetes}
\alias{diabetes}
\title{Diabetes Diagnosis Data}
\description{
  The \code{diabetes} data frame has 286 rows and 3 columns. The data
  were gathered from a population-based pilot survey of diabetes in
  Cairo, Egypt, in which postprandial blood glucose measurements were
  obtained from a fingerstick on 286 subjects. Based on the WHO (World
  Health Organization) criteria, 88 subjects were classified as diseased
  and 198 as healthy.
}
\usage{diabetes}
\format{
  The \code{diabetes} data frame contains the following columns:
  \itemize{
    \item{\code{marker}}{: 
      postprandial blood glucose measurements (mg/dl) obtained from a
      fingerstick.
    }
    \item{\code{status}}{: 
      disease status, with \code{1} identifying subjects diagnosed with
      diabetes.
    }
    \item{\code{age}}{
      age in years.
    }
  }
}
\references{
  Inácio de Carvalho, V., de Carvalho, M. and Branscum, A. (2017)
  Nonparametric Bayesian covariate-adjusted estimation of the Youden index.
  \emph{Biometrics}, \bold{73}, 1279-1288.
  
  Inácio de Carvalho, V., Jara, A., Hanson, T. E. and de Carvalho, M. (2013)
  Bayesian nonparametric ROC regression modeling.
  \emph{Bayesian Analysis}, \bold{8}, 623-646.
}
\examples{
data(diabetes)
plot(diabetes, pch = 20, main = "Diabetes Data")
}
\keyword{Medical}
