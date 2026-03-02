\name{apc.data.list.subset}
\alias{apc.data.list.subset}
\title{Cut age, period and cohort groups from data set.}
\description{For a recursive analysis it is useful to be able to cut age, period and cohort groups from a data set.
Function returns an \code{\link{apc.data.list}} with data.format "trapezoid".

When used with default values the function turns an \code{\link{apc.data.list}} into a new \code{\link{apc.data.list}}
with data.format "trapezoid" without reducing dataset.
}
\usage{apc.data.list.subset(apc.data.list,
							age.cut.lower=0,age.cut.upper=0,
							per.cut.lower=0,per.cut.upper=0,
							coh.cut.lower=0,coh.cut.upper=0,
							apc.index=NULL,
							suppress.warning=FALSE)}
\arguments{
  	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
	\item{age.cut.lower}{\emph{Optional}. Numeric. Specifies how many age    groups to cut at lower end. Default is zero.}
	\item{per.cut.lower}{\emph{Optional}. Numeric. Specifies how many period groups to cut at lower end. Default is zero.}
	\item{coh.cut.lower}{\emph{Optional}. Numeric. Specifies how many cohort groups to cut at lower end. Default is zero.}
	\item{age.cut.upper}{\emph{Optional}. Numeric. Specifies how many age    groups to cut at upper end. Default is zero.}
	\item{per.cut.upper}{\emph{Optional}. Numeric. Specifies how many period groups to cut at upper end. Default is zero.}
	\item{coh.cut.upper}{\emph{Optional}. Numeric. Specifies how many cohort groups to cut at upper end. Default is zero.}
	\item{apc.index}{\emph{Optional}. List. See \code{\link{apc.get.index}} for a description of the format.  If not provided this is computed internally.}
	\item{suppress.warning}{\emph{Optional}. Logical. Suppresses warnings. This is useful when generating data sums using
							\code{\link{apc.data.sums}} but reducing the data set so much that models cannot be fitted.}
}
\section{Arguments: Notes}{
If apc.index is supplied then the input can be simplified.
It suffices to write
\code{apc.data.list = list(response=response,data.format=data.format,dose=dose)},
where dose could be \code{dose=NULL}.
Likewise \code{apc.index} does not need to be a full \code{apc.index list}. It suffices to construct a list with entries
\code{age.max},                                                         
\code{per.max},                                                         
\code{coh.max},
\code{age1},
\code{per1},
\code{coh1},
\code{unit},
\code{per.zero},                                                        
\code{index.trap},                                                      
\code{index.data}.                                                      
}
\value{
	  	\item{response}{matrix (or vector).  Numbers of responses.}
	 	\item{dose}{matrix (or NULL).  Numbers of doses.}
		\item{data.format}{"trapezoid"}
	    \item{age1}{Numeric.}		
	    \item{per1}{Numeric.}		
	    \item{coh1}{Numeric.}		
	    \item{unit}{Numeric.}		
		\item{per.zero}{Numeric.}
		\item{per.max}{Numeric.}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 4 Dec 2013 recoded 26 Apr 2017}
\seealso{The below example uses artificial data.  For an example using
\code{\link{data.asbestos}}
see 
\code{\link{apc.plot.fit}}.
}
\examples{
###############
#	Artificial data
#	Generate a 5x7 matrix and make arbitrary decisions for rest

response <- matrix(data=seq(1:35),nrow=5,ncol=7)
data.list	<- list(response=response,dose=NULL,data.format="AP",
					age1=25,per1=1955,coh1=NULL,unit=5,
					per.zero=NULL,per.max=NULL,time.adjust=0)
data.list

apc.data.list.subset(data.list,1,1,0,0,0,0)


}