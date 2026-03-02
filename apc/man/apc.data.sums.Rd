\name{apc.data.sums}
\alias{apc.data.sums}
\title{Computes age, period and cohort sums of a matrix}
\description{Computes age, period and cohort sums of a matrix. This is the same as taking column, row and diagonal sums. The match between the age, period and cohort sums and column, row and diagonal sums depends on the data format}
\usage{apc.data.sums(apc.data.list,data.type="r",
			average=FALSE,keep.incomplete=TRUE,apc.index=NULL)}
\arguments{
  	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
  	\item{data.type}{Optional. Character. "r","d","m" if sums are computed for responses,dose,(mortality) rates.  Rates are computed as responses/doses. "r" is default.} 
  	\item{average}{Optional.  Logical. If TRUE/FALSE reports averages/sums. Default is FALSE.}
  	\item{keep.incomplete}{Optional. Logical. If true perform calculation for incomplete sequences by removing NA.
  			If false incomplete sequences are NA.  See example. Default=TRUE.}
 	\item{apc.index}{Optional. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed.}
}
\section{Arguments: Notes}{
If apc.index is supplied then the input can be simplified.
For instance if \code{data.type="r"} then, for the first argument, it suffices to write
\code{apc.data.list = list(response=response)}.
Likewise \code{apc.index} does not need to be a full \code{apc.index list}. It suffices to construct a list with entries
\code{age.max},                                                         
\code{per.max},                                                         
\code{coh.max},                                                         
\code{index.trap},                                                      
\code{index.data},                                                      
\code{per.zero}.                                                        
}
\value{
  \item{sums.age}{Vector. Sums/Averages over data.matrix by age.}
  \item{sums.per}{Vector. Sums/Averages over data.matrix by period.}
  \item{sums.coh}{Vector. Sums/Averages over data.matrix by cohort.}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 15 Aug 2018 (15 Dec 2013)}
\seealso{The example below uses Japanese breast cancer data, see \code{\link{data.Japanese.breast.cancer}}}
\examples{
#####################
#  EXAMPLE with artificial data
#  generate a 3x4 matrix in "AP" data.format with the numbers 1..12

m.data  	<- matrix(data=seq(length.out=12),nrow=3,ncol=4)
m.data
data.list	<- apc.data.list(m.data,"AP")
apc.data.sums(data.list)

#	$sums.age
#	 [1] 22 26 30
#	$sums.per
#	[1]  6 15 24 33
#	$sums.coh
#	[1]  3  8 15 24 18 10

apc.data.sums(data.list,average=TRUE)
#	$sums.age
#	[1] 5.5 6.5 7.5
#	$sums.per
#	[1]  2  5  8 11
#	$sums.coh
#	[1]  3  4  5  8  9 10

apc.data.sums(data.list,keep.incomplete=FALSE)
#	$sums.age
#	 [1] 22 26 30
#	$sums.per
#	[1]  6 15 24 33
#	$sums.coh
#	[1]  NA NA 15 24 NA NA

#####################
#	EXAMPLE with Japanese breast cancer data

data.list	<- data.Japanese.breast.cancer()	#	function gives data list
apc.data.sums(data.list)

#	$sums.age
#	[1]  573 2089 4053 6220 8083 8726 7796 6318 5117 3986 3005
#	$sums.per
#	[1]  7519  8332 10064 13183 16868
#	$sums.coh
#	[1]  497 1103 1842 2858 4474 5550 6958 7471 7531 6931 5111 3080 1666  715  179

#	Compare with the response matrix

data.list$response

#	      1955-1959 1960-1964 1965-1969 1970-1974 1975-1979
#	25-29        88        78       101       127       179
#	30-34       299       330       363       509       588
#	35-39       596       680       798       923      1056
#	40-44       874       962      1171      1497      1716
#	45-49      1022      1247      1429      1987      2398
#	50-54      1035      1258      1560      2079      2794
#	55-59       970      1087      1446      1828      2465
#	60-64       820       861      1126      1549      1962
#	65-69       678       738       878      1140      1683
#	70-74       640       628       656       900      1162
#	75-79       497       463       536       644       865

}
