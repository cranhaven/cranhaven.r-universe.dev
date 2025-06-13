\name{apc.plot.data.sparsity}
\alias{apc.plot.data.sparsity}
\title{This plot shows heat map of the sparsity of a data matrix.}
\description{The plot shows where the data matrix is sparse.}
\usage{apc.plot.data.sparsity(apc.data.list,
						data.type="a",swap.axes=FALSE,
						apc.index=NULL,
						sparsity.limits=c(1,2),
						cex=NULL,pch=15,
						main.outer=NULL)}
\arguments{
  	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
  	\item{data.type}{Optional. Character. "r"/"d"/"m" if sums are computed for responses/dose/all. "r" is default.}
	\item{swap.axes}{Optional. Logical. If true swap axes in plot.  Default is FALSE unless data.format="CL"}
  	\item{apc.index}{Optional. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed.}
  	\item{sparsity.limits}{Optional. vector with two values in increasing order.  Default is c(1,2). The sparsity plot is a heat map with three colours: black if the observation is smaller than first index (default 1), grey if the observation is smaller than the second index (default 2) and otherwise white.}
  	\item{cex}{Optional \code{\link{plot}} argument. A numerical value giving the amount by which plotting text and symbols should be magnified. Default is NULL in which case program chooses.}
  	\item{pch}{Optional. vector with two values. Either integers specifying a symbol or characters.
				See \code{\link{points}} for possible values and their interpretation. Default is c(15,15), which is filled square.}
  	\item{main.outer}{Optional. Character. Main title for plot, to be shown in outer margin.  Default is NULL, in which case a title is generated internally.}
}
\details{The default values is used to highlight where a matrix of counts has values of zero and one. Estimation can be very noise in those areas.}
\note{Note that the axes for plots grow from bottom left while axes for matrices grow from top left. The exception is when data.format="CL", in which case both grow from top left.}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 25 Apr 2015 updated 27 Apr 2015}
\seealso{The example below uses asbestos data, see \code{\link{data.asbestos}}}
\examples{
#####################
#  EXAMPLE with artificial data
#  generate a 3x4 matrix in "AP" data.format with the numbers 1..12

m.data  	<- matrix(data=seq(length.out=12),nrow=3,ncol=4)
m.data
data.list	<- apc.data.list(m.data,"AP")
apc.plot.data.sparsity(data.list)

#####################
#	EXAMPLE with Japanese breast cancer data
#	get data list, then make sparsity plots.

data.list	<- data.asbestos()					
apc.plot.data.sparsity(data.list)

}