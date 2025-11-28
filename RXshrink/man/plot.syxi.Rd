\name{plot.syxi}
\alias{plot.syxi}
\title{Plot method for syxi objects}
\description{
 Display plots for Visual Validation of the syxi() approach that attempts to 
 straighten nonlinear relationships between two variables.
}
\usage{
  \method{plot}{syxi}(x, type = "xy", ...)
}
\arguments{
  \item{x}{syxi output list object of class syxi.}
  \item{type}{One of two possible options: "xy" to display a plot of the
   x-predictor versus the y-outcome with the lm() fitted line in RED
   and the corresponding gam() Spline fit in BLUE, "sy" (or any string different
   from "xy") displays a plot of s(x) versus y with only the lm() fit in BLUE.}
  \item{\dots}{Optional argument(s) passed on to plot().}
}
\value{NULL}
\author{Bob Obenchain <wizbob@att.net>}
