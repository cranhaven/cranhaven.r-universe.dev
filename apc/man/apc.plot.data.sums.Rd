\name{apc.plot.data.sums}
\alias{apc.plot.data.sums}
\title{This plot shows sums of data matrix by age, period or cohort.}
\description{Produces plots showing age, period and cohort sums.
	As a default this is done both for responses and dose, giving a total of six plots.}
\usage{apc.plot.data.sums(apc.data.list,data.type="a",
			average=FALSE,keep.incomplete=TRUE,apc.index=NULL,
			type="o",log="",main.outer=NULL,main.sub=NULL)}
\arguments{
	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
	\item{data.type}{Optional. Character. "r","d","m","a" if sums are computed for responses, dose, (mortality rates), all. Rates are computed as responses/doses. Default is "a".}
	\item{average}{Optional. Logical. Sums are reported if FALSE, Averages are reported if TRUE. Default is FALSE.}
    \item{keep.incomplete}{Optional. Logical. If true perform calculation for incomplete sequences by removing NA.
  			If false incomplete sequences are NA.  See example in \code{\link{apc.data.sums}}. Default=TRUE.}
	\item{apc.index}{Optional. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed.}
	\item{type}{Optional \code{\link{plot}} argument. Character. "o" if overlaid points and lines. "l" if lines. "p" if points. Default is "o".}
	\item{log}{Optional \code{\link{plot}} argument. Character. "y" if y-scale is logarithmic, otherwise "". Default is "".}
  	\item{main.outer}{Optional. Character. Main title for plot, to be shown in outer margin.  Default is NULL, in which case a title is generated internally.}
	\item{main.sub}{Optional. Titles for sub plots. Use with data.type "r","d","m".  For data.type "a" use default.  Default is NULL, in which case a title is generated internally.}
}
\details{The data sums are computed using \code{\link{apc.data.sums}}.  Then plotted as requested.}
\references{
Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \href{https://doi.org/10.1111/rssa.12051}{Article}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 15 Aug 2018 (15 Dec 2013)}
\note{Use \code{\link{apc.data.sums}} if numerical values needed.}
\seealso{The example below uses Japanese breast cancer data, see \code{\link{data.Japanese.breast.cancer}}}
\examples{
#####################
#  	EXAMPLE with artificial data
#  	Generate a 3x4 matrix in "AP" data.format with the numbers 1..12
#	Then make a data list
#	Then plot data sums.
#	Note only 3 plots are made as there are no doses

m.data  	<- matrix(data=seq(length.out=12),nrow=3,ncol=4)
m.data
data.list	<- apc.data.list(m.data,"AP")
apc.plot.data.sums(data.list)					 
apc.plot.data.sums(data.list,average=TRUE)
apc.plot.data.sums(data.list,keep.incomplete=FALSE)					 

#####################
#	EXAMPLE with Japanese breast cancer data
#	Make a data list
#	Then plot data sums for both responses and doses.

data.list	<- data.Japanese.breast.cancer()	
apc.plot.data.sums(data.list)					

# 	Or plot data sums for responses only

apc.plot.data.sums(data.list,data.type="r")		

#####################
#	EXAMPLE with asbestos data
#	Miranda Martinex, Nielsen and Nielsen (2013).
#	This is Figure 1,a-c 

data.list	<- data.asbestos()	
apc.plot.data.sums(data.list,type="l")
}