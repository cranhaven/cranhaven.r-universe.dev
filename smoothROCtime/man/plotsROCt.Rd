\name{plot.sROCt}
\alias{plot.sROCt}
\title{Plots of time-dependent ROC curve estimations}
\description{
Plots of both Cumulative and Incident/Dynamic ROC curve estimations, provided by function \code{\link{stRoc}}.
}
\usage{
\method{plot}{sROCt}(x, tcr, xlab, ylab, type = "l", lwd = 5, ...)
}
\arguments{
  \item{x}{object of class \link[=stRoc]{sROCt} generated with \code{\link{stRoc}} function and containing the estimations of the time-dependent ROC curves for one single point or a vector of points.}
  \item{tcr}{type of time-dependent ROC curve estimation that will be plotted:\itemize{
                \item \dQuote{C} for \bold{C}umulative/Dynamic,
                \item \dQuote{I} for \bold{I}ncident/Dynamic,
                \item \dQuote{B} for \bold{B}oth time-dependent ROC curve estimations.}
              }
  \item{xlab}{a tittle for the x axis. The default value is "False - Positive Rate".}
  \item{ylab}{a tittle for the y axis. The default value is "True - Negative Rate".}
  \item{type}{what type of plot is going to be drawn. The default value is "l" and a line will be plotted.}
  \item{lwd}{line width. As a default value "5" is taken.}
  \item{...}{\link{plot} function arguments can also be used for customizing the plot.}
}

\details{
Parameter \code{tcr} is mandatory with no default values. If a "B" is indicated and the \link[=stRoc]{sROCt} object placed as \code{x} parameter contains only one type of time-dependent ROC curve estimation, an error message will be returned. Another error message will appear in case of placing either "C" or "I" when the \link[=stRoc]{sROCt} object does not contain the suitable ROC curve estimation.

When one single type of ROC curve estimation is chosen, one graphic will be drawn for each point of time in the \link[=stRoc]{sROCt} object, having as many independent plots as number of points of time. Graphic parameters like axis labels or line width will be the same for all the plots.

In case of choosing both time-dependent ROC curve estimations, they will be plotted in a single graphic for each point of time in \link[=stRoc]{sROCt} object. As before, we will have as many independent plots as points of time and the graphic parameters will be the same in all plots.
}

\examples{
library(smoothROCtime)
require(survival)

# Monoclonal Gammapothy of Undetermined Significance dataset
  data(mgus)

# Time-to-event
  time   <- ifelse(is.na(mgus$pctime), mgus$futime,mgus$pctime)

# Status
  status <- ifelse(is.na(mgus$pctime), 0, 1)

# Preparing data
  DT <-as.data.frame(cbind(log(time), status, mgus$alb))
  colnames(DT) <- c("futime", "pcm", "alb")
  dta <-  na.omit(cbind(DT$futime, DT$pcm, -DT$alb))

# Point of Time
  t10 <- log(10*365.25) # ten years in logarithm scale

# Cumulative/Dynamic and Incident dynamic ROC curve estimations at t=10 years
  rcu <- stRoc(data=dta, t=t10, tcr="B", meth = "1", verbose=TRUE)

# Plots of both ROC curve estimations
  plot(rcu, tcr="B", frame=FALSE)
}
