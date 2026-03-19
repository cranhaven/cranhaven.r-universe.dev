\name{disPairwise}
\alias{disPairwise}
\title{
Computation of the dissimilarity matrix (pairwise beta-diversity) for a set of networks
}
\description{
  Computation of the dissimilarity matrix for a set of networks. Each value of the matrix is the pairwise beta-diversity, computed using Hill numbers. It measures the dissimilarity in terms of groups, links, or probability of links.  
}
\usage{
disPairwise(gList, groups=NULL, eta=1,
            type=c('P','L','Pi'), abTable=NULL)
}
\arguments{
  \item{gList}{A \code{list} of graph objects of class
    \code{igraph}. The nodes must have a name.}
  \item{groups}{A named vector of class \code{character}  indicating the group to
    which each node belongs to. The length of \code{groups} must correspond to the number of different nodes present in \code{gList}. The names \code{names(groups)} must
    correspond to the nodes names in \code{gList}. If NULL, the groups are the initial nodes.}
  \item{eta}{A positive number that controls the weight given to abundant groups/links. Default value is 1.}
  \item{type}{The type of diversity used to measure dissimilarity. It can be groups diversity ('P'), links diversity ('L') or probability of links diversity ('Pi').}
  \item{abTable}{A matrix of size the number of nodes of the metanetwork times the number of networks. The rownames of this matrix must be the node names of metanetwork and the columns must
  be in an order corresponding to gList. The element (i,j) of this matrix is the abundance of species i in network j. Importantly, the non-nul elements in each column of \code{abTalbe} must 
  correspond to the nodes present in each element of \code{gList}}
}
\value{
  Return a \code{dist} object whose elements are the pairwise dissimilarities. 
}
\references{
Marc Ohlmann, Vincent Miele, Stephane Dray, Loic Chalmandrier, Louise O'Connor & Wilfried Thuiller, Diversity indices for ecological networks: a unifying framework using Hill numbers. Ecology Letters (2019) <doi:10.1111/ele.13221>
}
\author{
Authors: Stephane Dray, Vincent Miele, Marc Ohlmann, Wilfried Thuiller
Maintainer: Wilfried Thuiller <wilfried.thuiller@univ-grenoble-alpes.fr>
}
\examples{
# Generating a set of Erdos-Renyi graphs and give node names.
library(econetwork)
library(igraph)
nbGraph <- 3
gList <- c()
n <- 57 # number of nodes of each graph
C <- 0.1  # connectance of each graph
for(i in 1:nbGraph){
  graphLocal <- erdos.renyi.game(n, type='gnp', p.or.m=C, directed=TRUE)
  V(graphLocal)$name <- as.character(1:57)
  gList = c(gList,list(graphLocal))
}

# vector that gives the group of each node
groups <- c(rep("a",23),rep("b",34))
names(groups) <- as.character(1:57)
#generating random (non-nul) abundances data
abTable <- sapply(1:nbGraph,function(x) rpois(n,1)+1)
rownames(abTable) = unlist(unique(lapply(gList,function(g) V(g)$name)))

# Dissimilarity matrices based on links beta-diversity
# at a node level
disPairwise(gList, type = 'L')
# at a node level while taking into account node abundances
disPairwise(gList, type = 'L', abTable = abTable)
# at a group level
disPairwise(gList, groups, type = 'L')
# at a group level while taking into account node abundances
disPairwise(gList, groups, type = 'L', abTable = abTable) 

}
