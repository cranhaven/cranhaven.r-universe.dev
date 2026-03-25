\name{tdSim.method2}
\alias{tdSim.method2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulate 1 dataframe (1 simulation) of time-dep exposure under method 2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function simulate Survival Data. It generates a simulated dataset with time-dependent exposure under method 2 with a user-specified list of parameters as input.
}
\usage{

tdSim.method2(N,duration, lambda12, lambda23=NULL, lambda13, 
    HR=NULL, exp.prop,rateC, min.futime = 0, min.postexp.futime = 0)
}

\arguments{
  \item{N}{
  Number of subjects
  }
  \item{duration}{
  Duration of the study. This is used in censoring
  }
  \item{lambda12}{
  Lambda12 parameter to control time to exposure
  }
  \item{lambda23}{
  Lambda23 parameter to control time to event after exposure
  }
  \item{lambda13}{
  Lambda13 parameter to control time to event in the control group
  }
  \item{HR}{
  Hazard Ratio. This input is optional. If HR is set and lambda23 is not set, lambda23 = lambda13*HR
  }
  \item{exp.prop}{
  A numeric value between 0 and 1 (not include 0 and 1) that represents the proportion of subjects that are assigned with an exposure
  }
  \item{rateC}{
  Rate of the exponential distribution to generate censoring times
  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Simulata a Survival dataset using a modified version of illness-death model controlled by lambda12, lambda23, lambda13
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

\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
sim_data <- tdSim.method2(500, duration=24,lambda12=1.3,lambda23=0.04, 
    lambda13=0.03, exp.prop=0.2,rateC=0.05, min.futime=4, min.postexp.futime=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simulation}

