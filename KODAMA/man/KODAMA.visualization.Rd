\name{KODAMA.visualization}

\alias{KODAMA.visualization}

\title{Visualization of KODAMA output}

\description{
	Provides a simple function to transform the KODAMA dissimilarity matrix in a low-dimensional space.
}
\usage{
KODAMA.visualization(kk,
                     method=c("UMAP", "t-SNE", "MDS"),
                     config=NULL)
  
}

\arguments{
  \item{kk}{output of \code{\link{KODAMA.matrix}} function.}
  \item{method}{
  	method to be considered for transforming the dissimilarity matrix into a low-dimensional space. Choices are "\code{t-SNE}", "\code{MDS}", and "\code{UMAP}".
}
  \item{config}{
  	object of class umap.config or tsne.config.
}

}


\value{
    The function returns a matrix that contains the coordinates of the datapoints in a low-dimensional space.
    
}

\author{Stefano Cacciatore and Leonardo Tenori}

\references{
Abdel-Shafy EA, Kassim M, Vignol A, \emph{et al.} \cr
KODAMA enables self-guided weakly supervised learning in spatial transcriptomics. \cr
\emph{bioRxiv} 2025. doi: 10.1101/2025.05.28.656544. \doi{10.1101/2025.05.28.656544} \cr\cr

Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}\cr\cr

L.J.P. van der Maaten and G.E. Hinton.\cr
Visualizing High-Dimensional Data Using t-SNE. \cr
\emph{Journal of Machine Learning Research} 9 (Nov) : 2579-2605, 2008.
\cr	\cr
L.J.P. van der Maaten. \cr
Learning a Parametric Embedding by Preserving Local Structure. \cr
\emph{In Proceedings of the Twelfth International Conference on Artificial Intelligence and Statistics (AISTATS), JMLR W&CP} 5:384-391, 2009.
\cr	\cr
McInnes L, Healy J, Melville J. \cr
Umap: Uniform manifold approximation and projection for dimension reduction. \cr
\emph{arXiv preprint}:1802.03426. 2018 Feb 9.
}


\seealso{\code{\link{KODAMA.visualization}}}

\examples{
\donttest{

 data(iris)
 data=iris[,-5]
 labels=iris[,5]
 kk=KODAMA.matrix(data,ncomp=2)
 cc=KODAMA.visualization(kk,"t-SNE")
 plot(cc,col=as.numeric(labels),cex=2)

}
}
