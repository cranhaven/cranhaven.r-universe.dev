\name{corndata}
\alias{corndata}
\docType{data}
\title{Corn data in 8 counties in Iowa.}
\description{ Survey and satellite data for corn and soy beans in 12 Iowa counties, obtained from the 1978 June Enumerative Survey of the U.S. Department of Agriculture and from land observatory satellites (LANDSAT) during the 1978 growing season.
 }
\usage{data("corndata")}
\format{
   A data frame with 8 observations on the following 6 variables.
  \describe{
    \item{\code{County}:}{ county names.}
    \item{\code{ni}:}{ sample size for each county.}
    \item{\code{Xi}:}{ mean of reported hectares of corn from the survey, direct survey estimate.}
    \item{\code{Z1i}:}{ mean of pixels of corn for each, from satellite data.}
    \item{\code{Z2i}:}{ mean of pixels of soy bean for each county, from satellite data.}
    \item{\code{Si}:}{ square root of sample variance of reported hectares of corn from the survey.}
  }
}

\details{ While the original dataset includes survey and satellite data for corn in 12 Iowa counties, this dataset contains only 8 counties' information where sample sizes are greater than 1.
}

\source{ 
- Battesse, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components Model for Prediction of County Crop Areas Using Survey and Satellite Data. \emph{ Journal of the American Statistical Association}, \bold{ 83}, 28-36.


- You, Y. and Chapman, B. (2006) Small Area Estimation Using Area Level Models and Estimated Sampling
Variances. \emph{ Survey Methodology}, \bold{ 32}, 97-103.
}

\keyword{datasets}