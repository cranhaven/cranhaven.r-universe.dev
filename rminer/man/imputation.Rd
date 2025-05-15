\name{imputation}
\alias{imputation}
\title{Missing data imputation (e.g. substitution by value or hotdeck method).}
\description{Missing data imputation (e.g. substitution by value or hotdeck method).  }
\usage{
imputation(imethod = "value", D, Attribute = NULL, Missing = NA, Value = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imethod}{imputation method type:
                 \itemize{
                          \item \code{value} -- substitutes missing data by \code{Value} (with single element or several elements); 
                          \item \code{hotdeck} -- searches first the most similar example (i.e. using a k-nearest neighbor method -- \code{knn}) in the dataset 
                                                  and replaces the missing data by the value found in such example;  
                         }
}
  \item{D}{dataset with missing data (data.frame)}
  \item{Attribute}{if \code{NULL} then all attributes (data columns) with missing data are replaced. Else, \code{Attribute} is the attribute number (numeric) or name (character).}
  \item{Missing}{missing data symbol}
  \item{Value}{the substitution value (if \code{imethod=value}) or number of neighbors (\emph{k} of \code{knn}).}
}
\details{Check the references.}
\value{
A data.frame without missing data.
}
\references{
\itemize{
\item M. Brown and J. Kros.\cr
Data mining and the impact of missing data.\cr
In Industrial Management & Data Systems, 103(8):611-621, 2003.\cr

\item This tutorial shows additional code examples:\cr
P. Cortez.\cr
A tutorial on using the rminer R package for data mining tasks.\cr
Teaching Report, Department of Information Systems, ALGORITMI Research Centre, Engineering School, University of Minho, Guimaraes, 
Portugal, July 2015.\cr
\url{http://hdl.handle.net/1822/36210}\cr
}
}
\author{
Paulo Cortez \url{http://www3.dsi.uminho.pt/pcortez/}
}
\seealso{
\code{\link{fit}} and \code{\link{delevels}}.
}
\note{
See also \url{http://hdl.handle.net/1822/36210} and \url{http://www3.dsi.uminho.pt/pcortez/rminer.html}
}
\examples{
d=matrix(ncol=5,nrow=5)
d[1,]=c(5,4,3,2,1)
d[2,]=c(4,3,4,3,4)
d[3,]=c(1,1,1,1,1)
d[4,]=c(4,NA,3,4,4)
d[5,]=c(5,NA,NA,2,1)
d=data.frame(d); d[,3]=factor(d[,3])
print(d)
print(imputation("value",d,3,Value="3"))
print(imputation("value",d,2,Value=median(na.omit(d[,2]))))
print(imputation("value",d,2,Value=c(1,2)))
print(imputation("hotdeck",d,"X2",Value=1))
print(imputation("hotdeck",d,Value=1))

\dontrun{
# hotdeck 1-nearest neighbor substitution on a real dataset:
require(kknn)
d=read.table(
   file="http://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data",
   sep=",",na.strings="?",stringsAsFactors=TRUE)
print(summary(d))
d2=imputation("hotdeck",d,Value=1)
print(summary(d2))
par(mfrow=c(2,1))
hist(d$V26)
hist(d2$V26)
par(mfrow=c(1,1)) # reset mfrow
}

}
\keyword{ manip }
