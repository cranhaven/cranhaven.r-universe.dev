\name{plot_power}
\alias{plot_power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot power curves for survival analysis with time-dependent exposure
}
\description{
This function plots a power curve at each time and returns a subsetted data
frame that match the list of input parameters.
}
\usage{
plot_power(table_df, N, type, exp.prop, min.futime, min.postexp.futime,
    show.plot = FALSE, newplot = FALSE, col = NULL, lty, lwd, pch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_df}{
  A data frame read from a .csv file of a format output from the getpower.method1 function.
  }
  \item{N}{
  Number of subjects needs to be screened
  }
  \item{type}{
  A text string indicating the what type of dataset is of interest.   Either one of
"fixed" or "td" should be inputted
}
  \item{exp.prop}{
  A numeric value between 0 and 1 (not include 0 and 1) that represents the proportion of subjects that are assigned with an exposure
  }
  \item{min.futime}{
  A numeric value that represents minimum follow-up time (in months). The default value is 0, which means no minimum follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study
  }
  \item{min.postexp.futime}{
  A numeric value that represents minimum post-exposure follow-up time (in months). The default value is 0, which means no minimum post-exposure follow-up time is considered. If it has a positive value, this argument will help exclude subjects that only spend a short amount of time in the study after their exposure
  }
  \item{show.plot}{
  A logical value indicating whether to output a power curve or not. The default value is TRUE
}
  \item{newplot}{
  A logical value indicating whether to create a new plot or add to an existing plot
}
  \item{col,lty,lwd,pch}{
  Graphical parameters as in the regular plot function in R
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
The gist of this function is that the user can check the plot with the values in the output data frame. Moreover, it is flexible that the user can choose to only output the data frame in order to plot their own graph (such as add titles or legends) based on the data. The user can also add as many lines as possible to an existing power curve plot so that he/she is allowed to compare different scenarios.
}
\value{
A data.frame object with columns corresponding to
  \item{i_N}{
  Number of subjects needs to be screened, specified by the user
  }
  \item{N_eff}{
  Simulated number of evaluable subjects, which is the resulting number of subjects with or without considering minimum follow-up time and/or minimum post-exposure follow-up time
  }
  \item{i_beta}{
  Input value of regression coefficient (log hazard ratio)
  }
  \item{pow}{
  Simulated statistical power from the Cox regression model on data with time-dependent exposure
  }
}


\author{
Danyi Xiong, Teeranan Pokaprakarn, Hiroto Udagawa, Nusrat Rabbee \cr
Maintainer: Nusrat Rabbee <rabbee@berkeley.edu>
}

\examples{
# We recommend setting nSim to at least 500. It is set to 10 in the example to
# reduce run time for CRAN submission.

ret <- getpower.method1(nSim = 10, N = 600, b = 0.3, exp.prop = 0.2, 
    type = "td", scenario =  " ", maxrelexptime = 1/6, min.futime = 4, 
    min.postexp.futime = 4, output.fn = "output.csv")
	
ret2 <- getpower.method1(nSim = 10, N = 600, b = 0.3, exp.prop = 0.2, 
    type = "td", scenario = " ", maxrelexptime = 1/6, min.futime = 4, 
    min.postexp.futime = 0, output.fn ="output.csv")
	
# Read in .csv file as a data frame

tb <-  read.csv("output.csv", header = TRUE, sep = ",")

	# Visualize the subsetted data frame of interest and create a new plot

visualize1 <- plot_power(table_df = tb, N = 600, type = "td", exp.prop = 0.2,
    min.futime = 4, min.postexp.futime = 4, show.plot = TRUE, newplot = TRUE,
    col = "red", lty = 1, lwd = 2, pch = 16)

# Add a different power curve to the previously created plot

visualize2 <- plot_power(table_df = tb, N = 600, type = "td", exp.prop=0.2, 
    min.futime = 4, min.postexp.futime = 0, show.plot = TRUE, newplot = FALSE,
    col = "blue", lty = 1, lwd = 2, pch = 16)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Plot}

