\name{sch}
\alias{sch}
\docType{data}
\title{Schizophrenic Behaviour Data
}
\description{
Rubin andWu (1997) analyzed schizophrenic behaviour data from an eye-tracking experiment with
a visual target moving back and forth along a horizontal line on a screen. The outcome measurement
is called the gain ratio, which is eye velocity divided by target velocity, and it is recorded repeatedly
at the peak velocity of the target during eye-tracking under three conditions. The first condition is
plain sine (PS), which means the target velocity is proportional to the sine of time and the colour
of the target is white. The second condition is colour sine (CS), which means the target velocity is
proportional to the sine of time, as for PS, but the colours keep changing from white to orange or
blue. The third condition is triangular (TR), in which the target moves at a constant speed equal
to the peak velocity of PS, back and forth, but the colour is always white. There are 43 nonschizophrenic
subjects, 22 females and 21 males, and 43 schizophrenic subjects, 13 females and 30
males. In the experiment, each subject is exposed to five trials, usually three PS, one CS, and one
TR. During each trial, there are 11 cycles, and a gain ratio is recorded for each cycle. However,
for some cycles, the gain ratios are missing because of eye blinks, so that there are, on average,
34 observations out of 55 cycles for each subject. In this paper, we show how to analyze the data,
assuming missing at random which means the probability of being missing does not depend on the
values of missing data.s
}
\usage{data("sch")}
\format{
  A data frame with 2906 observations on the following 7 variables.
  \describe{
    \item{\code{y}}{Gain Ratios for the each measurements of the subject}
    \item{\code{x1}}{Effect of PS versus CS (-0.5=CS, 0.5=PS, 0=TR)}
    \item{\code{x2}}{Effect of TR versus CS and PS (-1/3=CS, -1/3=PS, 2/3=TR)}
    \item{\code{sex}}{Gender of subjects (1=male, 0=female)}
    \item{\code{time}}{Linear trend coded -5, ..., 5 for 11 measurement times of each subject}
    \item{\code{schiz}}{Indicator of schizophrenic subjects (=1) or not (=0)}
    \item{\code{v1}}{Subject number for 43 non-schizophrenic and 43 schizophrenic subjects}
  }
}
\references{
Rubin, D.B. and Wu, Y.N. (1997) Modeling schizophrenic behavior using general mixture components.
Biometrics 53, 243--261.
}

