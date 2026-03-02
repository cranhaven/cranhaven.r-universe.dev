\name{apc.plot.data.level}
\alias{apc.plot.data.level}
\title{Level plot of data matrix.}
\description{This plot shows level plot of data matrix based on
	\code{\link[lattice]{levelplot}} in the package \code{\link[lattice]{lattice}}.}
 \usage{apc.plot.data.level(apc.data.list,data.type="r",
 						rotate=FALSE,apc.index=NULL,
						main=NULL,lab=NULL,
						contour=FALSE,colorkey=TRUE)
}
\arguments{
	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
 	\item{data.type}{Optional. Character.
						"r"="response" /
						"d"="dose" /
						"m"="mortality"="rates" 
						if sums are computed for responses/dose/rates,
						where rates are found through division response/dose.
						It also takes data types 						
						"residual" /
						"fitted.values" /
						"linear.predictors"
						when the argument \code{apc.data.list} is the output of	the fitting function
						\code{\link{apc.fit.model}}, which is an extended apc.data.list.
						"r" is default.}
	\item{rotate}{Optional. Logical. If TRUE rotates plot 90 degrees clockwise (or anti-clockwise if data.format is "CL"). Default is FALSE.}
  	\item{apc.index}{Optional. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed.}
	\item{main}{Optional. Character. Main title.}
	\item{lab}{Optional \code{\link{plot}} parameter.	A numerical vector of the form c(x, y, len)
						which modifies the default way that axes are annotated.
						The values of x and y give the (approximate) number of tickmarks on
						the x and y axes. len is not implemented.}
	\item{contour}{ Optional \code{\link[lattice]{levelplot}} (\code{\link[lattice]{lattice}}) parameter.
						Logical. Contour lines drawn if TRUE. Default FALSE.}
	\item{colorkey}{Optional \code{\link[lattice]{levelplot}} (\code{\link[lattice]{lattice}}) parameter.
						Logical or list. Determines color key. Default TRUE.}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 26 Apr 2015}
\seealso{
\code{\link{data.Japanese.breast.cancer}}
for information on the data used in the example.
}
\examples{

#####################
#	EXAMPLE with Japanese breast cancer data
#	Clayton and Shifflers (1987b) use APC design
#	Make a data list
#	Then plot data.
#	Note: No plot appears to have approximately parallel lines.

data.list	<- data.Japanese.breast.cancer()
apc.plot.data.level(data.list,"r")
dev.new()
apc.plot.data.level(data.list,"d",contour=TRUE)
												
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