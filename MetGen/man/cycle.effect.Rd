\name{Cycle.effect}
\alias{diurnal.effect}
\alias{seasonal.effect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{diurnal and seasonal effects 
}

\description{
Create covariates to reproduce seasonal and diurnal variability basing on the time

of the year or of the day.

Seasonal effects are expressed by cos(2*pi*d/k) and sin(2*pi*d/k),

with d is the day of the year and k=(365, 183, 96 or 30),

Diurnal effects are expressed by cos(2*pi*h/k) and sin(2*pi*h/k),

with h is the hour of the day and k=(24, 12 or 6).
}

\usage{
diurnal.effect(var.mat, period = 24)
seasonal.effect(var.mat, period = 365.25)
}

\arguments{
  \item{var.mat}{
data frame that contains chron variable, possibly including times, geographical information for the measurement site and climatic variables for each time step,
}
  \item{period}{
a numeric vector that contains the period of the cycle desired, could be (183, 365, 91 or 30) for seasonal variation or (24, 12 or 6) for diurnal variation
}
}
\value{
The same data frame defined in the arguments is returned with supplementary number of columns that contains cycle effects. The number of additional columns depends on the period defined in arguments.
}

\seealso{\code{\link{myclimatic_data}} 
}

\examples{
##Perform cycle effects
seasonal_effects <- seasonal.effect(myclimatic_data, period=c(365,183,91,30))
diurnal_effects <- diurnal.effect(myclimatic_data, period=c(24,12,6))
}

