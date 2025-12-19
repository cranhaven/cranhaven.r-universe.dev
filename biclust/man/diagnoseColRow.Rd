\name{diagnoseColRow}
\alias{diagnoseColRow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Bootstrap Procedure for Bicluster Diagnostics
}
\description{
Calculate the significance of the discovered patter in the data based on the bootstrapping procedure.
}
\usage{
diagnoseColRow(x, bicResult, number, nResamplings, replace = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	data matrix, which \code{biclust} function was applied to
}
  \item{bicResult}{
	object of class \code{biclust}, containing result of a biclustering algorithm
}
  \item{number}{
	number of bicluster from the output for the diagnostics
}
  \item{nResamplings}{
	number of bootstrap replicates
}
  \item{replace}{
	logical flag for bootstrap (TRUE), or sampling without replacement (FALSE)
}
}
\details{
The function computes observed F statistics for row and column effect based on two-way ANOVA model. Bootstrap procedure is used to evaluate the significance of discovered bicluster.
Based on \code{nResamplings} replicates, the disribution of F statistics for row and column effects are obtained. The p-value is computed as

\deqn{
  P(A) =
  \frac{ \# \left \{ F^{*}(A)_{b} > F(A)^{obs} \right \} }
  {nResamplings+1}
}{P(A) =  F^*(A)_b > F(A)^{obs}  /(nResamplings+1)}


Low p-values denote non-random selection of columns for a given bicluster. Large p-values show that in other columns for a given set of genes in the bicluster structure is similar. 
Hence, bicluster columns were just randomly picked by an algorithm for a set of co-regulated genes.
}
\value{
	\item{bootstrapFstats}{
	matrix with two columns, containing values of bootstrap F-statistics. The first column corresponds to row, the second column corresponds to column.
	
}
	\item{observedFstatRow}{
	observed F-statistics for the row effect
} 
	\item{observedFstatCol}{
	observed F-statistics for the column effect
}
	\item{bootstrapPvalueRow}{
	bootstrap p value for row effect
}
	\item{bootstrapPvalueCol}{
	bootstrap p value for column effect
}
}
\author{
Tatsiana KHAMIAKOVA \email{tatsiana.khamiakova@uhasselt.be}
}
\seealso{
\code{\link{diagnosticTest}}, \code{\link{diagnosticPlot2}}, \code{\link{diagnosticPlot}}, \code{\link{computeObservedFstat}}, \code{\link{ChiaKaruturi}}
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

#Run boosotrap procedure:
Bootstrap <- diagnoseColRow(x=xmat, bicResult = plaidmab, number = 1, nResamplings = 999,
  replace = TRUE)
diagnosticPlot(bootstrapOutput = Bootstrap) 	# plotting distribution of bootstrap replicates

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bicluster}
\keyword{bootstrap}
