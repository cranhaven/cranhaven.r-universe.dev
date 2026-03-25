\name{tdSim.clst}
\alias{tdSim.clst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate 1 dataframe (1 simulation) of time-dependent exposure
under method 1 with a clustering data frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows the user to input a data frame with clustering
parameters and generates a simulated dataset with time-dependent exposure.
In particular, the output dataset has a column corresponding to the physician site id, which will be used as a clustering variable in the Cox regression model in power calculation.
}
\usage{
tdSim.clst(N, duration = 24, lambda, rho = 1, beta, rateC, df,
    prop.fullexp = 0, maxrelexptime = 1, min.futime = 0, min.postexp.futime = 0)
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
  A numeric value that represents the exposure effect, which is the
  regression coefficient (log hazard ratio) that represent the magnitude of
the relationship between the exposure covariate and the risk of an event
}
  \item{rateC}{
  Rate of the exponential distribution to generate censoring times, which is calculated as log(2) / median time to censoring
  }
  \item{df}{
  A user-specified n (n 3) by 3 clustering data frame with columns corresponding to cat_id (category id, which is the physician site id. It can be either text strings or integers), cat_prop (category proportion, which is the proportion of subjects in corresponding a category id), and cat_exprate (category exposure rate, which is the exposure proportion corresponding to a category id). n rows corresponds to n different physician sites
  }
  \item{prop.fullexp}{
  A numeric value in interval [0, 1) that represents the proportion of exposed subjects that are fully exposed from the beginning to the end of the study. The default value is 0, which means all exposed subjects have an exposure status transition at some point during the study
  }
  \item{maxrelexptime}{
   A numeric value in interval (0, 1] that represents the maximum relative exposure time. Suppose this value is p, the exposure time for each subject is then uniformly distributed from 0 to p*subject's time in the study. The default value is 1, which means all exposed subjects have an exposure status transition at any point during the time in study.  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
  }
  \item{min.postexp.futime}{A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The current version of this function allows the user to input a data frame with at
least 3 categories of physician sites, because the function uses a multinomial
distribution to assign subjects into each category according to the corresponding category proportion
}
\value{
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
  \item{clst_id}{
  For clustering in the Cox proportional hazard model. Represents label of each subject's corresponding physician site
  }
}
\references{
T. Therneau and C. Crowson (2015). Using Time Dependent Covariates and Time Dependent Coefficients in the Cox Model.

https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
}
\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# Create a clustering data frame as input with 3 categories and a 20% weighted
# exposure proportion.
  
input_df <- data.frame(cat_id = c('lo', 'med', 'hi'), 
	cat_prop = c(0.65, 0.2, 0.15), cat_exp.prop = c(0.1, 0.3, 0.5))

# Simulate a dataset of 600 subjects with time-dependent exposure. Consider
# both minimum follow-up time (4 months) and minimum post-exposure follow-up
# time (4 months). Also consider a quick exposure after entering the study for
# each exposed subject. Set the maximum relative exposure time to be 1/6. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months.

df_tdclst <- tdSim.clst(N = 600, duration = 24, lambda = log(2)/24, rho = 1,
    beta = 0.3, rateC = log(2)/14, df = input_df, prop.fullexp = 0,
    maxrelexptime = 1/6, min.futime = 4, min.postexp.futime = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}
