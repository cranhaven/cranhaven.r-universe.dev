\name{NMI}
\alias{NMI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized Mutual Information of Community Structure in Network
}
\description{
Calculates the normalized mutual information (NMI) of two community structures in network analysis.
}
\usage{
NMI(X,Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{X}{
  	a data frame or matrix whose first column is the node id and the second column is module 
}
  \item{Y}{
  	a data frame or matrix whose first column is the node id and the second column is module
}
}
\value{
\item{value}{
		 value of NMI
}
}
\examples{
# Suppose X and Y are exactly the same
X<-data.frame(c(1,2,3),c(2,1,1))
Y<-X
# There NMI is 1
NMI(X,Y)

# Suppose X and Y are completely independent
X<-data.frame(c(1,2,3),c(2,1,1))
Y<-data.frame(c(5,6,7),c(2,1,1))
# There NMI is 0
NMI(X,Y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.