% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\name{stabpath}
\alias{stabpath}
\alias{stabilitypath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stability paths plot
}
\description{
The function plots the stability path of a S4VD result
}
\usage{
stabpath(res,number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
the S4VD result  
}
  \item{number}{
the bicluster for which the stability path shall be plotted
}
}
\details{
Plots the stability path for the rows and the columns regarding the last iteration of the s4vd algorithm.
Note that if the pointwise error control was used or if savepath=FALSE the final selection probabilities for the rows and the columns will be plotted.  
}
%\value{
%}
%\references{
%
%kommt noch
%}
\author{
Martin Sill \
\email{m.sill@dkfz.de}
}
%\note{
%kommt noch
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}
\examples{
\donttest{
# example data set according to the simulation study in Lee et al. 2010
# generate artifical data set and a correspondig biclust object
u <- c(10,9,8,7,6,5,4,3,rep(2,17),rep(0,75))
v <- c(10,-10,8,-8,5,-5,rep(3,5),rep(-3,5),rep(0,34))
u <- u/sqrt(sum(u^2)) 
v <- v/sqrt(sum(v^2))
d <- 50
set.seed(1)
X <- (d*u\%*\%t(v)) + matrix(rnorm(100*50),100,50)
params <- info <- list()
RowxNumber <- matrix(rep(FALSE,100),ncol=1)
NumberxCol <- matrix(rep(FALSE,50),nrow=1)
RowxNumber[u!=0,1] <- TRUE 
NumberxCol[1,v!=0] <- TRUE
Number <- 1
ressim <- BiclustResult(params,RowxNumber,NumberxCol,Number,info)
#perform s4vd biclustering 
ress4vd <- biclust(X,method=BCs4vd,pcerv=0.5,
                        pceru=0.5,ss.thr=c(0.6,0.65),steps=500,
                        pointwise=FALSE,nbiclust=1,savepath=TRUE)
#perform s4vd biclustering with fast pointwise stability selection
ress4vdpw <- biclust(X,method=BCs4vd,pcerv=0.5,
                          pceru=0.5,ss.thr=c(0.6,0.65),steps=500,
                          pointwise=TRUE,nbiclust=1)
#stability paths
stabpath(ress4vd,1)
#selection probabilitys for the pointwise stability selection
stabpath(ress4vdpw,1)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
