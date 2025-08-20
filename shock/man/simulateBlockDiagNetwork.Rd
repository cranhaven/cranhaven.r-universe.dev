\name{simulateBlockDiagNetwork}
\alias{simulateBlockDiagNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a modular network 
}
\description{

This function simulates a modular network with \code{p} variables based on the partition of variables into blocks \code{labels}. 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simulateBlockDiagNetwork(p, labels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
  number of variables in the network
%%     ~~Describe \code{p} here~~
}

  \item{labels}{
  vector indicating the partition of variables into blocks
%%     ~~Describe \code{labels} here~~
}
}
\details{
	To simulate covariance matrices, we use the methodology detailed in Giraud, S. Huet, and N. Verzelen. Graph selection with GGMselect. 2009
    
	\url{http://fr.arxiv.org/abs/0907.0619}
  \url{https://cran.r-project.org/package=GGMselect}
	
	%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{A}{simulated adjacency matrix}
\item{C}{simulated correlation matrix}
\item{Pcor}{simulated partial correlation matrix}
\item{labels}{vector indicating the partition of variables into blocks provided as input of the function}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

 
\examples{
## number of variables
p <- 100
## number of blocks
K <- 15
## vector of partition into blocks
labels <- factor(rep(1:K, length.out=p))
## simulate network 
g <- simulateBlockDiagNetwork(p,labels)
        

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~network }
%\keyword{ ~simulation }% __ONLY ONE__ keyword per line
