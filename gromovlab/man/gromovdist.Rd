\name{gromovdist}
\title{Gromov-Hausdorff-type distances of labelled metric spaces}
\alias{gromovdist}
\alias{gromovdist,list-method}
\alias{gromovdist,phylo-method}
\alias{gromovdist,multiPhylo-method}
\alias{gromovdist,dist-method}
\alias{gromovdist,dissimilarity-method}
\alias{gromovdist,matrix-method}
\alias{gromovdist,igraph-method}
\alias{gromovdist.list}
\alias{gromovdist.phylo}
\alias{gromovdist.multiPhylo}
\alias{gromovdist.dist}
\alias{gromovdist.dissimilarity}
\alias{gromovdist.matrix}
\alias{gromovdist.igraph}
\alias{gromovdist.default}


\usage{gromovdist(d1,d2=NULL,type="l1",p=NULL,...)

\S3method{gromovdist}{list}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{multiPhylo}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{phylo}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{dist}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{dissimilarity}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{matrix}(d1,d2=NULL,type="l1",p=NULL,...)
\S3method{gromovdist}{igraph}(d1,d2=NULL,type="l1",p=NULL,leavesonly=TRUE,...)
}
\arguments{
  \item{type}{type of metric to use}
  \item{d1,d2}{distance object(s).}
  \item{p}{if \code{type="lp"} the value of \eqn{p}. If not supplied, \eqn{p=2} is used.} 
  \item{leavesonly}{compute the distances between the leaves of the graph/tree only?} 
  \item{...}{further parameters}
}
\description{
  The function \code{gromovdist} calculates the matched Gromov-\eqn{\ell^p} distances of two metrics on a finite space \eqn{X}: 
  
  \deqn{D_p(\rho_1,\rho_2)=\inf\{\|(|d(\phi_1(x),\phi_2(x))|)_{x \in X}\|_p\}.}
  
  
There, the infimum  is taken over all isometric embeddings \eqn{\phi_1} of \eqn{(X,\rho_1)}, \eqn{\phi_2} of \eqn{(X,\rho_2)} into a common metric space \eqn{(Y,d)}. Only  \eqn{1\le p\le \infty} is considered. 
  
  At the basis is the reformulation of the metric as value of a convex program, see Liebscher (2015). 
  
  Methods for various classes are provided: 
  \itemize{  \item \code{dist,dissimilarity} which are distance matrices. 
  \item \code{matrix} for matrices containing the individual distances of the elements of \eqn{X}.
  \item \code{igraph} for connected graphs. The metric  on the nodes or just the leaves (nodes of degree 1) of the graph is  the  length of the shortest path.
  \item \code{phylo} for phylogenetic trees. Again the metric is induced by the graph.
}  
}
\details{
\code{type="l1"} yields \eqn{p=1}

\code{type="l2"} yields \eqn{p=2}

\code{type="linfinity"} yields \eqn{p=\infty}

\code{type="lp"} is for (not so efficient) computation using \link[stats]{constrOptim} for arbitrary \eqn{1\le p<\infty} 

If \code{d1} is a list, the distance matrix between  all elements of the list is computed. It is represented  as an object of class \code{dissimiliarity}, see \link[cluster]{dissimilarity.object}.   


The distance is only computed for that part of the objects where the labels are present in both objects. If there are no labels the elements are numbered consecutively. 
}
\value{The distance (one numeric) or a distance matrix for the \code{list} and \code{multiPhylo} methods
}
\references{
V.Liebscher, Gromov meets Phylogenetics - new Animals for the Zoo of Metrics on Tree Space. preprint 2015 arXiv:1504.05795
}
\author{Volkmar Liebscher}
\seealso{
\link[stats]{dist}, \link[cluster]{dissimilarity.object},\link[phangorn]{phangorn-package}, \link[igraph]{igraph-package},\link[stats]{constrOptim}.
}
\concept{Gromov-Hausdorff metric}
\concept{dissimilarity}
\concept{tree space}
\concept{finite labeled metric space}
\examples{library("ape")
tr1<-rtree(n=10)
tr2<-rtree(n=10)
gromovdist(tr1,tr2,"l1")
gromovdist(tr1,tr2,"l2")
#thesame, but slower
gromovdist(d1=tr1,d2=tr2,type="lp",p=2)
gromovdist(tr1,tr2,"linf")
}
