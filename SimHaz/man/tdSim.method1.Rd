\name{tdSim.method1}
\alias{tdSim.method1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulate 1 dataframe (1 simulation) of time-dep exposure under method 1
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function generates a simulated dataset with time-dependent exposure under  
method 1 with a user-specified list of parameters as input. Survival times and censoring times are generated from the exponential distribution.
}
\usage{
tdSim.method1(N, duration = 24, lambda, rho = 1, beta, rateC, exp.prop, 
    prop.fullexp  = 0, maxrelexptime = 1, min.futime = 0, min.postexp.futime = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
  Number of subjects needs to be screened
  }
  \item{duration}{
  Length of the study in Months. The default value is 24 (months)
  }
  \item{lambda}{
  Scale parameter of the Weibull distribution, which is calculated as log(2) / median time to event for control group
  }
  \item{rho}{
  Shape parameter of the Weibull distribution, which is defaulted as 1, as we generate survival times by using the exponential distribution
  }
  \item{beta}{
  A numeric value that represents the exposure effect, which is the regression coefficient (log hazard ratio) that represent the magnitude of the relationship between the exposure covariate and the risk of an event
}
  \item{rateC}{
  Rate of the exponential distribution to generate censoring times, which is calculated as log(2) / median time to censoring
  }
  \item{exp.prop}{
  A numeric value between 0 and 1 (not include 0 and 1) that represents the proportion of subjects that are assigned with an exposure
  }
  \item{prop.fullexp}{
  A numeric value in interval [0, 1) that represents the proportion of exposed subjects that are fully exposed from the beginning to the end of the study. The default value is 0, which means all exposed subjects have an exposure status transition at some point during the study
  }
  \item{maxrelexptime}{
   A numeric value in interval (0, 1] that represents the maximum relative exposure time. Suppose this value is p, the exposure time for each subject is then uniformly distributed from 0 to p*subject's time in the study. The default value is 1, which means all exposed subjects have an exposure status transition at any point during the time in study.
  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
  }
}
\details{

If no minimum follow-up time or minimum post-exposure follow-up time is considered (min.fut = 0 and min.postexp.fut = 0), then the output data frame
will have N subjects. If we consider minimum follow-up time or both, then the
output data frame will have at most N subjects. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  A data.frame object with columns corresponding to
  \item{id}{
  Integer that represents a subject's identification number
  }
  \item{start}{
  For counting process formulation. Represents the start of each time interval
  }
  \item{stop}{
  For counting process formulation. Represents the end of each time interval
  }
  \item{status}{
  Indicator of event. status = 1 when event occurs and 0 otherwise
  }
  \item{x}{
  Indicator of exposure. x = 1 when exposed and 0 otherwise
  }
}
\references{
%% ~put references to the literature/web site here ~
Therneau and C. Crowson (2015). Using Time Dependent Covariates and Time Dependent Coefficients in the Cox Model.

https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
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
   beta = 0.3, rateC = log(2)/14, exp.prop = 0.2, prop.fullexp  = 0, 
   maxrelexptime = 1, min.fut = 0, min.postexp.fut = 0)

# Simulate a dataset of 600 subjects with time-dependent exposure with
# both minimum follow-up time (4 months) and minimum post-exposure
# follow-up time (4 months) imposed. Other parameters remain the same as
# in the first case.

df2 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exp.prop = 0.2, prop.fullexp  = 0, 
   maxrelexptime = 1, min.fut = 4, min.postexp.fut = 4)

# Simulate a dataset of 600 subjects with time-dependent exposure with
# both minimum follow-up time (4 months) and minimum post-exposure
# follow-up time (4 months) imposed. Also consider a quick exposure after
# entering the study for each exposed subject. Set the maximum relative
# exposure time to be 1/6. Other parameters remain the same as in the first case.

df3 <- tdSim.method1(N = 600, duration = 24, lambda = log(2)/24, rho = 1, 
   beta = 0.3, rateC = log(2)/14, exp.prop = 0.2, prop.fullexp  = 0,
   maxrelexptime = 1/6, min.fut = 4,min.postexp.fut = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}

