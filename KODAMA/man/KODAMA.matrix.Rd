\name{KODAMA.matrix}

\alias{KODAMA.matrix}

\title{Knowledge Discovery by Accuracy Maximization}

\description{
	KODAMA (KnOwledge Discovery by Accuracy MAximization) is an unsupervised and semi-supervised learning algorithm that performs feature extraction from noisy and high-dimensional data. 
}
\usage{
KODAMA.matrix (data,                 
               spatial = NULL,     
               samples = NULL,
               M = 100, Tcycle = 20, 
               FUN = c("fastpls","simpls"), 
               ncomp = min(c(50,ncol(data))),
               W = NULL, metrics="euclidean",
               constrain = NULL, fix = NULL,  landmarks = 10000,  
               splitting = ifelse(nrow(data) < 40000, 100, 300), 
               spatial.resolution = 0.3 , 
               simm_dissimilarity_matrix=FALSE,
               seed=1234) 


}

\arguments{
  \item{data}{A numeric matrix where rows are samples and columns are variables.}

  \item{spatial}{Optional matrix of spatial coordinates or NULL. Used to apply spatial constraints.}

  \item{samples}{An optional vector indicating the identity for each sample. Can be used to guide the integration of prior sample-level information.}

  \item{M}{Number of iterative processes.}

  \item{Tcycle}{Number of cycles to optimize cross-validated accuracy.}

  \item{FUN}{Classifier to be used. Options are \code{"fastpls"} or \code{"simpls"}.}

  \item{ncomp}{Number of components for the PLS classifier. Default is \code{min(50, ncol(data))}.}

  \item{W}{A vector of initial class labels for each sample (\code{length = nrow(data)}). Defaults to unique labels for each sample if NULL.}

  \item{metrics}{Distance metric to be used (default is \code{"euclidean"}).}

  \item{constrain}{An optional vector indicating group constraints. Samples sharing the same value in this vector will be forced to stay in the same cluster.}

  \item{fix}{A logical vector indicating whether each sample's label in \code{W} should be fixed during optimization. Defaults to all \code{FALSE}.}

  \item{landmarks}{Number of landmark points used to approximate the similarity structure. The default is 10000.}

  \item{splitting}{Number of random sample splits used during optimization. The default is 100 for small datasets (<40000 samples) and 300 otherwise.}

  \item{spatial.resolution}{A numeric value (default 0.3) controlling the resolution of spatial constraints.}

  \item{simm_dissimilarity_matrix}{Logical. If \code{TRUE}, the function returns a similarity/dissimilarity matrix. Default is \code{FALSE}.}

  \item{seed}{Random seed for reproducibility. The default is 1234.}

}

\details{
KODAMA consists of five steps. These can be in turn divided into two parts: (i) the maximization of cross-validated accuracy by an iterative process (step I and II), resulting in the construction of a proximity matrix (step III), and (ii) the definition of a dissimilarity matrix (step IV and V). The first part entails the core idea of KODAMA, that is, the partitioning of data guided by the maximization of the cross-validated accuracy. At the beginning of this part, a fraction of the total samples (defined by \code{FUN_SAM}) are randomly selected from the original data. The whole iterative process (step I-III) is repeated \code{M} times to average the effects owing to the randomness of the iterative procedure. Each time that this part is repeated, a different fraction of samples is selected. The second part aims at collecting and processing these results by constructing a dissimilarity matrix to provide a holistic view of the data while maintaining their intrinsic structure (steps IV and V). Then, \code{\link{KODAMA.visualization}} function is used to visualise the results of KODAMA dissimilarity matrix. 

}

\value{
    The function returns a list with 4 items:
    \item{dissimilarity}{a dissimilarity matrix.}
    \item{acc}{a vector with the \code{M} cross-validated accuracies.}
    \item{proximity}{a proximity matrix.}
    \item{v}{a matrix containing all classifications obtained maximizing the cross-validation accuracy.}
    \item{res}{a matrix containing all classification vectors obtained through maximizing the cross-validation accuracy.}
    \item{knn_Rnanoflann}{dissimilarity matrix used as input for the \code{\link{KODAMA.visualization}} function.}
    \item{data}{original data.}
    \item{res_constrain}{the constrins used.}
    
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
\emph{Journal of Machine Learning Research} 9 (Nov): 2579-2605, 2008.
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
