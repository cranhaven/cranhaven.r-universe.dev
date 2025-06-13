\name{dissmutmatrix}
\alias{dissmutmatrix}
\title{
Symmetric dissimilarity mutation matrix.
}
\description{
This function computes and returns the Euclidean distance matrix, where each cell represents the distance in base pairs between the chromosomal position of somatic mutations. The matrix can be used to graph the anti-Robinson matrix using the seriation technique (Hahsler and Hornik 2011). Plotting the distance matrix helps to visualize and identify mutation clusters in addition to locating the micro-clustered mutated regions within the macro-clustered mutated zones that occur during the oncogenic process.
}
\usage{
dissmutmatrix(data = NULL, chr = NULL, position, subset = NULL, upper = FALSE)
}
\arguments{
  \item{data}{: somatic substitution mutations of the cancer genome data set.}
  \item{chr}{: chromosome where the somatic mutation is located.}
  \item{position}{: position of somatic mutations in the DNA sequence of the cancer genome.}
  \item{subset}{: chromosome where the distance between all somatic mutations will be calculated.}
  \item{upper}{: logical value indicating whether the upper triangle of the distance matrix should be printed.}
}
\details{
Color, in the posterior dissimilarity plot, is selected to visually identify hyper-mutated zones (min = 6; max=5000). 
}
\value{
\code{dissmutmatrix()} returns a symmetric dissimilarity matrix in base 10.
}
\references{
Hahsler M, Hornik K and Buchta C (2008), Getting things in order: An introduction to the R package seriation. Journal of Statistical Software 25/3. Software available at \url{http://www.jstatsoft.org/v25/i03/}.  

Hahsler M and Hornik K. Dissimilarity plots: A visual exploration tool for partitional clustering. Journal of Computational and Graphical Statistics, 10(2):335--354, June 2011.
}
\author{
David Lora
}
\seealso{
\code{\link{dist}}, 
\code{\link{dissplot}} 
}
\examples{
require(seriation)

###Example 1:
example1<-c(1,101,201,299,301,306,307,317,318,320,418,518,528,628)
10**(dissmutmatrix(position=example1,upper=TRUE))
mut.matrix <- dissmutmatrix(position=example1)
dissplot(mut.matrix,method=NA, 
 options=list(col = c("white","white","orange","orange","red","red","red")))


###Example 2:
###One hypermutated zone with Two hypermutated areas sharing somatic mutations.
example2<-c(1,101,201,299,301,306,307,317,318,320,402,404,406,628)
10**(dissmutmatrix(position=example2,upper=TRUE))
mut.matrix <- dissmutmatrix(position=example2)
dissplot(mut.matrix,method=NA, 
 options=list(col = c("white","white","orange","orange","red","red","red")))


###Example 3:
#data(PD4107a)

###Visualizes a dissimilarity mutation matrix using seriation and matrix shading
###using the method developed by Hahsler and Hornik (2011).
###Chromosome 1;
#mut.matrix <- dissmutmatrix(data=PD4107a,chr=Chr,position=Position,subset=1)
#dissplot(mut.matrix, method=NA, options=list( col = 
# c("black","navy","blue","cyan","green","yellow","orange","red",
#	"darkred","darkred","white")))

###Chromosome 6;
#mut.matrix <- dissmutmatrix(data=PD4107a,chr=Chr,position=Position,subset=6)
#dissplot(mut.matrix, method=NA, options=list( col = 
# c("black","navy","blue","cyan","green","yellow","orange","red","darkred",
#	"darkred","white")))

###Chromosome 12;
#mut.matrix <- dissmutmatrix(data=PD4107a,chr=Chr,position=Position,subset=12)
#dissplot(mut.matrix, method=NA, options=list( col = 
# c("black","navy","blue","cyan","green","yellow","orange","red","darkred",
#   "darkred","white")))
}
