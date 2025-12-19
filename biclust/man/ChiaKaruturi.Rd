\name{ChiaKaruturi}
\alias{ChiaKaruturi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chia and Karuturi Function
}
\description{
Function computing scores as described in the paper of Chia and Karuturi (2010)%%
}
\usage{
ChiaKaruturi(x, bicResult, number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data Matrix
}
  \item{bicResult}{
 \code{Biclust} object from \code{biclust} package
}
  \item{number}{
Number of bicluster in the output for computing the scores
}
}
\details{
The function computes row (T) and column (B) effects for a chosen bicluster. The scores for columns within bicluster have index 1, the scores 
for columns outside the bicluster have index 2. Ranking score is SB, stratification score is TS. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 Data.Frame with 6 slots: T, B scores for within and outside bicluster, SB and TS scores 
}
\references{
%% ~put references to the literature/web site here ~
Chia, B. K. H. and Karuturi, R. K. M. (2010) Differential co-expression framework to quantify goodness of biclusters and compare biclustering algorithms. Algorithms for Molecular Biology, 5, 23.
}
\author{
Tatsiana KHAMIAKOVA \email{tatsiana.khamiakova@uhasselt.be}
}

\seealso{
\code{\link{diagnosticPlot}}, \code{\link{computeObservedFstat}}, \code{\link{diagnoseColRow}}
}
\examples{
#---simulate dataset with 1 bicluster ---#
xmat<-matrix(rnorm(50*50,0,0.25),50,50) # background noise only 
rowSize <- 20 #number of rows in a bicluster 
colSize <- 10 #number of columns in a bicluster
a1<-rnorm(rowSize,1,0.1) #sample row effect from N(0,0.1) #adding a coherent values bicluster:
b1<-rnorm((colSize),2,0.25)  #sample column effect from N(0,0.05)
mu<-0.01 #constant value signal
 for ( i in 1 : rowSize){
 	for(j in 1: (colSize)){
 		xmat[i,j] <- xmat[i,j] + mu + a1[i] + b1[j] 	
 	}
 }
 #--obtain a bicluster by running an algorithm---# 
plaidmab <- biclust(x=xmat, method=BCPlaid(), cluster="b", fit.model = y ~ m + a+ b,  
background = TRUE, row.release = 0.6, col.release = 0.7, shuffle = 50, back.fit = 5, 
max.layers = 1, iter.startup = 100, iter.layer = 100, verbose = TRUE)

#Get Chia and Karuturi scores:
ChiaKaruturi(x=xmat, bicResult = plaidmab, number = 1)
}

\keyword{cluster}
