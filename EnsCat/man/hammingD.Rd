\name{hammingD}
\alias{hammingD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the hamming distance between data points.
%%  ~~function to do ... ~~
}
\description{Hamming distance is defined on categorical vectors.  It counts the number of
times the coordinates in two data vectors differ, or the number of substitutions required
to convert one data vector into the other. Here the Hamming distance is normalized, so the
result is the number of coordinates that differ divided by the vector length.
}
\usage{
hammingD(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{dat should be a matrix or data frame of data. n is the number of observations (rows)
  and p is the number of dimensions (columns).}
  }
\details{
This function calculates the Hamming distance (normalized) between rows of the input data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The result is a nxn matrix whose (i,j) element is the Hamming distance between rows i and j
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \link{alphadata},
}
\examples{
### The running is time consuming
### Run hamming distance
#dis0<-hammingD(alphadata)
### Save as distance format
#REDIST<-as.dist(dis0)
### Run a hierarchical clustering using average linkage
#hc0 <- hclust(REDIST,method = "average")
### plot the dendrogram
#plot(hc0,label=xlab1,hang =-1)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
