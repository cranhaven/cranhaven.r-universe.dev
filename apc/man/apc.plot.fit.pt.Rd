\name{apc.plot.fit.pt}
\alias{apc.plot.fit.pt}
\title{Plot probability transform of responses given fitted values}
\description{Constructs probability transforms of responses given fitted values from \code{\link{apc.fit.model}}.
The plot is given in the original coordinate system. Colours and symbols are used to indicate
whether responses are central to the fitted distribution or in the tails of the fitted distribution.}
\usage{apc.plot.fit.pt(apc.fit.model,
					   do.plot=TRUE,do.value=FALSE,
					   pch=c(21,24,25),
					   col=c("black","green","blue","red"),
					   bg=NULL,cex=NULL,main=NULL)}
\arguments{
  \item{apc.fit.model}{List. See \code{\link{apc.fit.model}} for a description of the format.}
  \item{do.plot}{Optional. Logical.  If FALSE plot is not produced. Default is TRUE.}
  \item{do.value}{Optional. Logical.  If TRUE value is produced. Default is FALSE.}
  \item{pch}{Optional \code{\link{points}} argument.  Numeric.  Default is 21/24/25.  21 is a circle used for the central 80\% of distribution. 24/25 are triangle point up/down used for right tail and left tail.} 
  \item{col}{Optional \code{\link{plot}} argument.  Character or Numeric.  Default is "black"/"green"/"blue"/"red".  Black is use for central 80\%, Green is used for 90-95\% and 5-10\%, Blue is used for 95-99\% and 1-5\%, Red is used for tails.}
  \item{bg}{Optional \code{\link{plot}} argument.  Character or Numeric.  Default is bg=col.}
  \item{cex}{Optional \code{\link{plot}} argument.  Numeric.  Magnification.  Default is internally computed.}
  \item{main}{Optional \code{\link{plot}} argument.  Character.  Main title.  Default is internally computed.}
}
\value{
	Vector of probability transforms.  Only produced if \code{do.value} is set to TRUE. See example below.
}
\seealso{
\code{\link{data.Italian.bladder.cancer}} for information on the data used in the example.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 2 Dec 2013}
\examples{
#####################
#	Example with Italian bladder cancer data
#	HOW TO USE VALUE

data.list	<- data.Italian.bladder.cancer()
fit			<- apc.fit.model(data.list,"poisson.dose.response","APC")
v.pt		<- apc.plot.fit.pt(fit,do.value=TRUE)
m.pt		<- matrix(data=NA,nrow=fit$data.xmax,ncol=fit$data.ymax)
m.pt[fit$index.data]	<- v.pt
m.pt

#	            [,1]      [,2]       [,3]       [,4]      [,5]
#	 [1,] 0.63782311 0.5651585 0.33982477 0.91299734 0.5759652
#	 [2,] 0.82676269 0.8992667 0.26378120 0.28795884 0.3708787
#	 [3,] 0.54139571 0.2445995 0.51923747 0.63451773 0.7955547
#	 [4,] 0.87364488 0.8228499 0.07219437 0.38789788 0.5938305
#	 [5,] 0.86797473 0.3934085 0.34525271 0.38955656 0.5097203
#	 [6,] 0.65027598 0.8377994 0.29018594 0.03694977 0.7990229
#	 [7,] 0.43769468 0.1099946 0.50261364 0.56777485 0.8916552
#	 [8,] 0.67518708 0.5519831 0.67817803 0.19793887 0.5354669
#	 [9,] 0.02717016 0.2066092 0.77035122 0.89047749 0.5017919
#	[10,] 0.71037782 0.9464356 0.36897847 0.41790169 0.2080577
#	[11,] 0.50922468 0.3085978 0.55261186 0.77592343 0.3597815
}