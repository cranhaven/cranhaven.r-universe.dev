\name{apc.plot.fit.all}
\alias{apc.plot.fit.all}
\title{Make all fit plots.}
\description{Plots estimates using \code{\link{apc.plot.fit}}.
			 Probability transform plot of residuals using \code{\link{apc.plot.fit.pt}}.
			 Level plot of residuals using \code{\link{apc.plot.fit.residuals}}.
			 Level plot of fitted values using \code{\link{apc.plot.fit.fitted.values}}.
			 Level plot of linear predictors using \code{\link{apc.plot.fit.linear.predictors}}.
			 Level plots of responses and rates (if dose is availble) using \code{\link{apc.plot.data.level}}.}
\usage{apc.plot.fit.all(apc.fit.model,log ="",rotate=FALSE)}
\arguments{
  \item{apc.fit.model}{List. Output from \code{\link{apc.fit.model}}. See there for a description of the format.}
  \item{log}{Optional \code{\link{plot}} argument. Character. "y" if y-scale is logarithmic, otherwise "". Default is "".}
  \item{rotate}{Optional. Logical. If TRUE rotates level plots 90 degrees clockwise (or anti-clockwise if data.format is "CL"). Default is FALSE.}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 2t Apr 2015}
\seealso{The example below uses Italian bladder cancer data, see \code{\link{data.Italian.bladder.cancer}}}
\examples{
#####################
#	EXAMPLE with Italian bladder cancer data

#	get data list, then make all descriptive plots.
# 	Note that warnings are given in relation to the data chosen thinning
#	This can be avoided by working with the individual plots, and in particular
#	with apc.plot.data.within where the thinning happens.

data.list	<- data.Italian.bladder.cancer()
fit			<- apc.fit.model(data.list,"poisson.dose.response","APC")
apc.plot.fit.all(fit)
}