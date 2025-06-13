\name{apc.plot.data.all}
\alias{apc.plot.data.all}
\title{Make all descriptive plots.}
\description{Plots data sums using \code{\link{apc.plot.data.sums}}.
			 Sparsity plots of data using \code{\link{apc.plot.data.sparsity}}.
			 Plots data using all combinations of two time scales using \code{\link{apc.plot.data.within}}.
			 Level plots of data using \code{\link{apc.plot.data.level}}.
			 The latter plot is done for responses and if applicable also for doses and mortality rates.}
\usage{apc.plot.data.all(apc.data.list,log ="",rotate=FALSE)}
\arguments{
  \item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
  \item{log}{Optional \code{\link{plot}} argument. Character. "y" if y-scale is logarithmic, otherwise "". Default is "y".}
  \item{rotate}{Optional. Logical. If TRUE rotates \code{\link{apc.plot.data.level}} 90 degrees clockwise (or anti-clockwise if data.format is "CL"). Default is FALSE.}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 25 Apr 2015}
\section{Warning}{A warning is produced if dimension is not divisible by thin, so that one group is smaller than other groups.}
\seealso{The example below uses Italian bladder cancer data, see \code{\link{data.Italian.bladder.cancer}}}
\examples{
#####################
#  EXAMPLE with artificial data
#  generate a 3x4 matrix in "AP" data.format with the numbers 1..12

m.data  	<- matrix(data=seq(length.out=12),nrow=3,ncol=4)
m.data
data.list	<- apc.data.list(m.data,"AP")
apc.plot.data.all(data.list,log="")				

#####################
#	EXAMPLE with Italian bladder cancer data
#
#	get data list, then make all descriptive plots.
# 	Note that warnings are given in relation to the data chosen thinning
#	This can be avoided by working with the individual plots, and in particular
#	with apc.plot.data.within where the thinning happens.
#
#	data.list	<- data.Italian.bladder.cancer()	
#	apc.plot.data.all(data.list)
}