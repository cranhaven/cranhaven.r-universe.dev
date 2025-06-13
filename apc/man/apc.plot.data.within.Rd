\name{apc.plot.data.within}
\alias{apc.plot.data.within}
\alias{apc.plot.data.within.all.six}
\title{This plot shows time series of matrix within age, period or cohort.}
\description{\code{apc.plot.data.within} produces plot showing time series of matrix
	within age, period or cohort against one of the other two indices.
	\code{apc.plot.data.within.all.six} produces all six plots in one panel plot.
	
	These plots are sometimes used to gauge how many of the age, period, cohort factors are needed:
	If lines are parallel when dropping one index the corresponding factor may not be needed.
	In practice these plots should possibly be used with care, see Italian bladder cancer example below.}
\usage{apc.plot.data.within(apc.data.list,
					data.type="r",plot.type="awc",
					average=FALSE,
					thin=NULL,apc.index=NULL,
					ylab=NULL,type="o",log="",legend=TRUE,
					lty=1:5,col=1:6,bty="n",main=NULL,
					x="topleft",return=FALSE)
apc.plot.data.within.all.six(apc.data.list,
					data.type="r",
					average=FALSE,
					thin=NULL,apc.index=NULL,
					ylab=NULL,type="o",log="",legend=TRUE,
					lty=1:5,col=1:6,bty="n",main.outer=NULL,
					x="topleft")
}				
\arguments{
	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
 	\item{data.type}{Optional. Character. "r"="response" / "d"="dose" / "m"="mortality"="rates"
						if sums are computed for responses/dose/rates,
						where rates are found through division response/dose. "r" is default.}
	\item{plot.type}{Optional. "awp", "pwa" "awc", "cwa, "cwp", "pwc":
						for example: "awp" gives time series in age within each period level:
						for an AP data-array these are the column sums.}					
	\item{average}{Optional.  Logical. If TRUE/FALSE reports averages/sums. Default is FALSE.}
	\item{thin}{Optional. Numerical.  age/periods/cohorts are grouped in groups of size thin.
						Default is computed from dimensions of data.  A warning is produced if dimension is not divisible by thin, so that one group is smaller than other groups.}
	\item{apc.index}{Optional. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed.}
  	\item{ylab}{Optional \code{\link{plot}} argument.  Character.  Common label for y-axes. Default is "".}
	\item{type}{Optional \code{\link{plot}} argument. Character. "o" if overlaid points and lines. "l" if lines. "p" if points. Default is "o".}
  	\item{log}{Optional \code{\link{plot}} argument. Character. "y" if y-scale is logarithmic, otherwise "". Default is ""}
  	\item{legend}{Optional \code{\link{plot}} argument. Logical. Should legends be drawn? Default is TRUE.}
  	\item{lty}{Optional \code{\link{plot}} argument. Vector of line types.
			The first element is for the first column, the second element for the second column, etc.,
			even if lines are not plotted for all columns. Line types will be used cyclically
			until all plots are drawn.  Default is 1:5}
  	\item{col}{Optional \code{\link{plot}} argument. Vector of colors.
			The first element is for the first column, the second element for the second column, etc.,
			even if lines are not plotted for all columns. Colors will be used cyclically
			until all plots are drawn.  Default is 1:6.}
  	\item{bty}{Optional \code{\link{plot}} argument. Character.
			The type of box to be drawn around the legend.
			The allowed values are "n" and "o".	Default is "n".}
  	\item{main}{Optional. Character. Main title for single plot.  Default is NULL, in which case a title is generated internally.}
  	\item{main.outer}{Optional. Character. Main title for panel of six plots, to be shown in outer margin.  Default is NULL, in which case a title is generated internally.}
	\item{x}{Optional \code{\link{legend}} argument. Default is "topleft".}
	\item{return}{Optional. If TRUE return matrix that is plotted. Default is FALSE}
}
\section{Warning}{A warning is produced if dimension is not divisible by thin, so that one group is smaller than other groups.}
\references{
Clayton, D. and Schifflers, E. (1987a)
Models for temperoral variation in cancer rates. I: age-period and age-cohort models. 
\emph{Statistics in Medicine} 6, 449-467.

Clayton, D. and Schifflers, E. (1987b) Models for temperoral variation in cancer rates. II: age-period-cohort models. \emph{Statistics in Medicine} 6, 469-481.

Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \href{https://doi.org/10.1111/rssa.12051}{Article}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 17 Nov 2016 (25 Apr 2015)}
\seealso{
\code{\link{data.Japanese.breast.cancer}},
\code{\link{data.Italian.bladder.cancer}} and
\code{\link{data.asbestos}} for information on the data used in the example.
}
\examples{
#####################
#  	EXAMPLE with artificial data
#  	Generate a 3x4 matrix in "AP" data.format with the numbers 1..12
#	Then make a data list
#	Then plot data.
#  	Note: this deterministic matrix has neither age, period, or cohort factors,
#		 only linear trends.  Thus all 6 plots have parallel lines.	

m.data  	<- matrix(data=seq(length.out=12),nrow=3,ncol=4)
m.data
data.list	<- apc.data.list(m.data,"AP")
apc.plot.data.within(data.list,log="")

#	It also works with a single argument, but then a default log scale is used.

apc.plot.data.within(data.list)			

#####################
#	EXAMPLE with Japanese breast cancer data
#	Clayton and Shifflers (1987b) use APC design
#	Make a data list
#	Then plot data.
#	Note: No plot appears to have approximately parallel lines.

data.list	<- data.Japanese.breast.cancer()	
apc.plot.data.within(data.list,"m",1,log="y")
												
#	It also works with a single argument, but then a default log scale is used.
# 	Note that warnings are given in relation to the data chosen thinning

apc.plot.data.within(data.list)

#####################
#	EXAMPLE with Italian bladder cancer data
#	Clayton and Shifflers (1987a) use AC design
#	Note: plot of within cohort against age appears to have approximately parallel lines.
#		  This is Figure 2 in Clayton and Shifflers (1987a)
#	Note: plot of within age against cohort appears to have approximately parallel lines.
#		  Indicates that interpretation should be done carefully.

data.list	<- data.Italian.bladder.cancer()	
apc.plot.data.within(data.list,"m",1,log="y")

#####################
#	EXAMPLE with asbestos data
#	Miranda Martinex, Nielsen and Nielsen (2014).
#	This is Figure 1d 

data.list	<- data.asbestos()	
apc.plot.data.within(data.list,type="l",lty=1)
}