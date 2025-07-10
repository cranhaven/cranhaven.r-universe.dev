\name{sbphylo}
\alias{sbphylo}
\alias{summary.sbphylo}
\alias{print.sbphylo}
\alias{print.summary.sbphylo}
\title{Tables for phylogenetic inference}
\description{
Creating tables of p-values and tree/edge associaitons for phylogenetic inference.
Trees and edges are sorted by the likelihood value.
}
\usage{
sbphylo(relltest,ass,trees,edges,edge2tree,
                   treename=NULL,edgename=NULL,taxaname=NULL,mt=NULL,sort=TRUE) 

\method{summary}{sbphylo}(object, k = 2,...)

\method{print}{sbphylo}(x,...)

\method{print}{summary.sbphylo}(x,...)

}
\arguments{
  \item{relltest}{\code{relltest} output.}
  \item{ass}{\code{read.ass} output.}
  \item{trees}{\code{relltest} output for trees.}
  \item{edges}{\code{relltest} output for edges.}
  \item{edge2tree}{\code{read.ass} output for "edge to tree" association.}
  \item{treename}{character vector for tree descriptions.}
  \item{edgename}{character vector for edge descriptions.}
  \item{taxaname}{character vector for taxa names.}
  \item{mt}{\code{read.mt} output for the site-wise log-likelhiood values.}
  \item{sort}{sorting trees and edges by likelhiood when TRUE.}
  \item{object}{output of \code{sbphylo}.}
  \item{k}{integer of \eqn{k} for calculating p-values.}
  \item{x}{sbphylo or summary.sbphylo objects.}
  \item{...}{further arguments passed to and from other methods.} 
}
\details{
First, apply \code{sbphylo} to consel results, and \code{summary} will make tables.
Output tables are suitable for publication.
For the input of \code{sbphylo}, you should specify either of (\code{relltest}, \code{ass}) or
(\code{trees}, \code{edges}, \code{edge2tree}). 
}
\value{
\code{sbphylo} returns a list of several information of multiscale bootstrap.
It does not do actual computation, but only sort trees and edges in decreasing order of likelihood values. The compied information is then passed to 
\code{summary} method, which returns a list containing character tables and its numerical values of p-values.
}

\author{Hidetoshi Shimodaira}

\seealso{\code{\link{relltest}}, \code{\link{read.ass}}, \code{\link{read.mt}}}

\examples{
## working with CONSEL outputs
data(mam15)
mam15.trees <- mam15.relltest[attr(mam15.ass,"trees")]  # 15 trees
mam15.edges <- mam15.relltest[attr(mam15.ass,"edges")]  # 10 edges
mam15.edge2tree <- mam15.ass[attr(mam15.ass,"edges")] # 10 edges
mam15 <- sbphylo(trees=mam15.trees,edges=mam15.edges,
                edge2tree=mam15.edge2tree) # sort trees and edges by likelihood
mam15 # print method for sbphylo
tab <- summary(mam15) # summary method for sbphylo
tab # prints character table

## plot (beta0,beta1)
a1 <- attr(summary(mam15$trees,k=2),"table")
a2 <- attr(summary(mam15$edges,k=2),"table")
beta <- rbind(a1$value,a2$value)[,c("beta0","beta1")]
sbplotbeta(beta) # for diagnostics of p-values
}

\keyword{ phylogenetics }

