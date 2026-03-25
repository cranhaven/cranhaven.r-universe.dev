\name{getpower.clst}
\alias{getpower.clst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate power for the Cox proportional hazard model with
time-dependent exposure using method 1 with clustering
}
\description{
This functions runs nSim (number of simulations; specified by the user) Monte Carlo simulations on the Cox proportional model with a cluster option. At each simulation, the function calls tdSim.clst internally. The function returns a data frame of scenario-specific parameters (including statistical power) and appends the output to a file with file name specified in the input parameters list. The user also has an option to display an incidence plot.
}
\usage{
getpower.clst(nSim, N, duration = 24, med.TTE.Control = 24, rho = 1, beta,
    med.TimeToCensor = 14, df, type, scenario, prop.fullexp = 0,
    maxrelexptime = 1, min.futime = 0, min.postexp.futime = 0, output.fn,
    simu.plot = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nSim}{
  Number of simulations.
  }
  \item{N}{
  Number of subjects to be screened.
  }
  \item{duration}{
  Length of the study in months; the default value is 24 (months).
  }
  \item{med.TTE.Control}{
  Median time to event for control group; the default value is 24 (months).
  }
  \item{rho}{
  Shape parameter of the Weibull distribution. Default is 1, which will generate survival times by using the exponential distribution.
  }
  \item{beta}{
  A numeric value that represents the exposure effect, which is the regression coefficient (log hazard ratio) that represents the magnitude of the relationship between the exposure covariate and the risk of an event. 
  }
  \item{med.TimeToCensor}{
  Median time to censoring for all subjects. The default value is 14 (months). Also see help document for tdSim.method1.
  }
  \item{df}{
  A user-specified n by 3 clustering data frame with columns corresponding to cat_id (category id, which is the physician site id. It can be either text strings or integers), cat_prop (category proportion, which is the proportion of subjects in corresponding a category id), and cat_exprate (category exposure rate, which is the exposure proportion corresponding to a category id). n rows corresponds to n different physician sites.
  }
  \item{type}{
  A text string indicating the what type of dataset is of interest. Either "fixed" or "td" should be inputted.
  }
  \item{scenario}{
  A text string to name a scenario that is being simulated. The user can simply put " " if he/she decides to not name the scenario.
  }
  \item{prop.fullexp}{
  A numeric value in interval [0, 1) that represents the proportion of exposed subjects that are fully exposed from the beginning to the end of the study. The default value is 0, which means all exposed subjects have an exposure status transition at some point during the study.
  }
  \item{maxrelexptime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure.
  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study.
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure.
  }
  \item{output.fn}{
  A .csv filename to write in the output. If the filename does not exist, the function will create a new .csv file for the output.
  }
  \item{simu.plot}{
  A logical value indicating whether or not to output an incidence plot.The default value is FALSE.
  }
}

\details{
The function calculates power based on the Cox regression model, which calls the coxph function from the survival library using the the simulated data from tdSim.clst
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data.frame object with 3 rows and columns corresponding to

  \item{i_scenario}{
  Scenario name specified by the user.
  }
  \item{i_type}{
  Dataset type specified by the user.
  }
  \item{i_N}{
  Number of subjects to be screened, specified by the user.
  }
  \item{i_min.futime}{
  Minimum follow-up time to be considered, specified by the user.
  }
  \item{i_min.postexp.futime}{
  Minimum post-exposure follow-up time to be considered, specified by the user.
  }
  \item{i_cat}{
  Category id specified in user's input data frame.
  }
  \item{i_cat_prop}{
  Category proportion specified in user's input data frame.
  }
  \item{i_cat_exp.prop}{
  Category exposure proportion specified in user's input dataframe.
  }
  \item{i_exp.prop}{
  Weighted exposure proportion calculated from user's input dataframe.
  }
  \item{i_lambda}{
  Value of the scale parameter of the Weibull distribution to generate survival times. Calculated from median time to event for control group, which is specified by the user.
  }
  \item{i_rho}{
  User-specified Value of the shape parameter of the Weibull distribution to generate survival times.
  }
  \item{i_rateC}{
  Rate of the exponential distribution to generate censoring times. Calculated from median time to censoring, which is specified by the user.
  }
  \item{i_beta}{Input value of regression coefficient (log hazard ratio).
  }
  \item{N_eff}{
  Simulated number of evaluable subjects, which is the resulting number of subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time.
  }
  \item{N_effexp_p}{
  Simulated proportion of exposed subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time.
  }
  \item{bhat}{
  Simulated value of regression coefficient (log hazard ratio).
  }
  \item{HR}{
  Simulated value of hazard ratio.
  }
  \item{d}{
  Simulated number of events in total.
  }
  \item{d_c}{
  Simulated number of events in control group.
  }
  \item{d_exp}{
  Simulated number of events in exposed group.
  }
  \item{mst_c}{
  Simulated median survival time in control group.
  }
  \item{mst_exp}{
  Simulated median survival time in exposed group.
  }
  \item{pow}{Simulated statistical power from the Cox regression model on data with time-dependent exposure.
}
}
\references{
Savignoni et al.: Matching methods to create paired survival data based on an exposure occurring over time: a simulation study with application to breast cancer. 

BMC Medical Research Methodology 2014 14:83. 
}
\author{
%%  ~~who you are~~
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# Install the survival package if needed.

library(survival)

# Create a clustering data frame as input with 3 categories and a 20% weighted
# exposure proportion.
  
input_df <- data.frame(cat_id = c('lo', 'med', 'hi'),
	cat_prop = c(0.65, 0.2, 0.15), cat_exp.prop = c(0.1, 0.3, 0.5))

# We recommend setting nSim to at least 500. It is set to 10 in the example to
# reduce run time for CRAN submission.

# Run 10 simulations. Each time simulate a dataset of 600 subjects with
# time-dependent exposure with both minimum follow-up time (4 months) and
# minimum post-exposure follow-up time (4 months) imposed. Also consider a
# quick exposure after entering the study for each exposed subject. Set the
# maximum relative exposure time to be 1/6. 

# Set the duration of the study to be 24 months; the median time to event for
# control group to be 24 months; exposure effect to be 0.3; median time to
# censoring to be 14 months.

ret <- getpower.clst(nSim = 10, N = 600, beta = 0.3, df = input_df,
    type = "td", scenario = "clustering", maxrelexptime = 1/6, min.futime = 4,
    min.postexp.futime = 4, output.fn = "output_clst.csv",) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Power_Calculation}
