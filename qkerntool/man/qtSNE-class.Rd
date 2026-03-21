\name{qtSNE-class}
\docType{class}
\alias{qtSNE-class}
\alias{dimRed,qtSNE-method}
\alias{dimRed<-,qtSNE-method}



\title{Class "qtSNE"}
\description{
An S4 Class for qtSNE.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("qtSNE", ...)}.
    or by calling the function \code{qtSNE}.
}
\details{
The qtSNE is a method that uses Qkernel t-Distributed Stochastic Neighborhood Embedding between the distance matrices in high and low-dimensional space to embed the data. The method is very well suited to visualize complex structures in low dimensions.
}
\section{Slots}{
\describe{
\item{\code{dimRed}}{ Matrix containing the new representations for the objects after qtSNE}
\item{\code{cndkernf}}{ The kernel function used}
}}

\section{Method}{
\describe{
\item{\code{dimRed}}{\code{signature(object="qtSNE")}: return a new representation matrix}
\item{\code{cndkernf}}{\code{signature(object="qtSNE")}: return the kernel used}
}}

\seealso{
\code{\link{qtSNE}}
}

\author{Yusen Zhang \cr \email{yusenzhang@126.com}}



\examples{\dontrun{
#use iris data set
data(iris)
testset <- sample(1:150,20)
train <- as.matrix(iris[,1:4])

colors = rainbow(length(unique(iris$Species)))
names(colors) = unique(iris$Species)
#for matrix
ecb = function(x,y){
  plot(x,t='n');
  text(x,labels=iris$Species, col=colors[iris$Species])
}
kpc2 <- qtSNE(train, kernel = "rbfbase", qpar = list(sigma=1,q=0.8),
              epoch_callback = ecb, perplexity=10, max_iter = 500)

#cndernf
cndkernf(kpc2)

#dimRed
plot(dimRed(kpc2),col=train)

}
}
\references{
Maaten, L. van der, 2014. Accelerating t-SNE using Tree-Based
Algorithms. Journal of Machine Learning Research 15, 3221-3245.

van der Maaten, L., Hinton, G., 2008. Visualizing Data using
t-SNE. J. Mach. Learn. Res. 9, 2579-2605.
}

