\name{SimHaz-package}
\alias{SimHaz-package}
\alias{SimHaz}
\docType{package}
\title{
Simulated Survival and Hazard Analysis for time-dependent
}
\description{
This package generates power for the Cox proportional hazards model by simulating survival events data with time dependent exposure status for subjects. A dichotomous exposure variable is considered with a single transition from unexposed to exposed status during the subject time's in the study.
}
\details{
\tabular{ll}{
Package: \tab SimHaz\cr
Type: \tab Package\cr
Version: \tab 0.1 \cr
Date: \tab 2015-09-29\cr
License: \tab GPL-2 \cr
Depends: R (>= 3.1.1)
Imports: survival
}
}
\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# Simulate a dataset of 600 subjects with time-dependent exposure without
# considering minimum follow-up time or minimum post-exposure follow-up time.
# Specifically, set the duration of the study to be 24 months; the median time to
# event for control group to be 24 months; exposure effect to be 0.3; median time
# to censoring to be 14 months; and exposure proportion to be 20%.

df1 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exp.prop = 0.2, 
   prop.fullexp  = 0, maxrelexptime = 1, min.futime = 0,
   min.postexp.futime = 0)
   
# We recommend setting nSim to at least 500. It is set to 10 in the example to
# reduce run time for CRAN submission.

ret <- getpower.method1(nSim = 10, N = 600, b = 0.3, exp.prop = 0.2, 
	type = "td", scenario = " ", maxrelexptime = 1/6, min.futime = 4,
	min.postexp.futime = 4, output.fn = "output.csv")

}
