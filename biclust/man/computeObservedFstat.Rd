\name{computeObservedFstat}
\alias{computeObservedFstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diagnostic F Statistic Calculation
}
\description{
Functions for obtaining F statistics within bicluster and the significance levels. The main effects considered are row, column and interaction effect.
}
\usage{
computeObservedFstat(x, bicResult, number)
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
Number of bicluster in the output for computing observed statistics 
}
}
\details{
F-statistics are calculated from the two-way ANOVA mode with row anc column effect. The full model with interaction is unidentifiable, thus, Tukey's test for non-additivity is used to detect an  interaction within a bicluster. 
p-values are obtained from assymptotic F distributions.
}
\value{
Data frame with three rows ("Row Effect", "Column Effect", "Tukey test") and 2 columns for corresponding statistics (Fstat) and their p-values (PValue).
2

}
\author{
Tatsiana KHAMIAKOVA \email{tatsiana.khamiakova@uhasselt.be}
}


\seealso{
\code{\link{diagnosticTest}}, \code{\link{diagnosticPlot2}}, \code{\link{ChiaKaruturi}}, \code{\link{diagnoseColRow}}
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

#Calculate statistics and their p-values to infer about the structure within bicluster:
Structure <- computeObservedFstat(x=xmat, bicResult = plaidmab, number = 1)

}
