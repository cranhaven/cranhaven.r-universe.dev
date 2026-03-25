\name{plot_simuData}
\alias{plot_simuData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make an incidence plot from simulated data.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create an incidence plot ordered by follow-up time from a survival data simulated.
}
\usage{
plot_simuData(data, title="Sample Survival Data")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A dataframe of survival data containing the following columns: id, start, stop, status, x
  }
  \item{title}{
  Title of the graph
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This makes the incidence plot of the survival data based on the input dataframe from the tdSim.method1 or tdSim.method2 function. More generally, this function would also works with a dataframe containing survival data with the same columns name as indicated above.
}

\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
dat <- tdSim.method2(500, duration=24,lambda12=1.3,lambda23=0.04, 
    lambda13=0.03, exp.prop=0.2,rateC=0.05, min.futime=4, min.postexp.futime=4)
	
plot_simuData(dat, title='method2_filter')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Plot}

