\name{apc.plot.fit.residuals}
\alias{apc.plot.fit.residuals}
\alias{apc.plot.fit.fitted.values}
\alias{apc.plot.fit.linear.predictors}
\title{Level plots of residuals / fitted values / linear predictors}
\description{Level plots of residuals / fitted values / linear predictors.
Returns residuals / fitted values / linear predictors as matrices when requested.
The plots use \code{\link{apc.plot.data.level}}.
They plot are given in the original coordinate system. }
\usage{apc.plot.fit.residuals(apc.fit.model,
					rotate=FALSE,main=NULL,lab=NULL,
					contour=FALSE,colorkey=TRUE,return=FALSE)
	   apc.plot.fit.fitted.values(apc.fit.model,
					rotate=FALSE,main=NULL,lab=NULL,
					contour=FALSE,colorkey=TRUE,return=FALSE)
	   apc.plot.fit.linear.predictors(apc.fit.model,
					rotate=FALSE,main=NULL,lab=NULL,
					contour=FALSE,colorkey=TRUE,return=FALSE)
}
\arguments{
	\item{apc.fit.model}{List. Output from \code{\link{apc.fit.model}}. See there for a description of the format.}
	\item{rotate}{Optional. Logical. If TRUE rotates plot 90 degrees clockwise (or anti-clockwise if data.format is "CL"). Default is FALSE.}
	\item{main}{Optional. Character. Main title.}
	\item{lab}{Optional \code{\link{plot}} parameter.	A numerical vector of the form c(x, y, len)
						which modifies the default way that axes are annotated.
						The values of x and y give the (approximate) number of tickmarks on
						the x and y axes. len is not implemented.}
	\item{contour}{ Optional \code{\link{levelplot}} (\code{\link{lattice}}) parameter.
						Logical. Contour lines drawn if TRUE. Default FALSE.}
	\item{colorkey}{Optional \code{\link{levelplot}} (\code{\link{lattice}}) parameter.
						Logical or list. Determines color key. Default TRUE.}
	\item{return}{Optional. Logical. If TRUE returns matrix with values.  Default is FALSE.}						
}
\value{
	Matrix of the original format with residuals / fitted values /linear predictors as entries.
	Only produced if \code{return} is set to TRUE.
}
\seealso{
\code{\link{data.Italian.bladder.cancer}} for information on the data used in the example.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 26 Apr 2015}
\examples{
#####################
#	Example with Italian bladder cancer data

data.list	<- data.Italian.bladder.cancer()
fit			<- apc.fit.model(data.list,"poisson.dose.response","APC")
apc.plot.fit.fitted.values(fit,return=TRUE)

#       1955-1959   1960-1964   1965-1969   1970-1974   1975-1979
# 25-29   3.04200    3.368944    2.261518    2.327538   12.000000
# 30-34  13.11980   12.835733   13.955859   10.416142    9.672462
# 35-39  24.15536   33.591644   33.388355   37.542301   26.322340
# 40-44  69.89262   68.842728   96.652963   98.478793  113.132896
# 45-49 217.97285  189.375728  189.115063  272.281239  285.255119
# 50-54 450.44864  529.823519  462.504305  469.869189  701.354350
# 55-59 724.88451  904.298410 1069.452434  969.346982  966.017661
# 60-64 877.17820 1226.088350 1532.521380 1877.331703 1807.880364
# 65-69 950.36106 1296.011123 1798.196048 2336.012274 3028.419493
# 70-74 903.94495 1187.708772 1598.021907 2302.605072 3222.719298
# 75-79 831.00000  953.055049 1280.930166 1755.788768 2678.226017
}