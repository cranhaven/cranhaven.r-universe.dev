\name{embedding}
\alias{embedding}
\title{
Graph embeddings for comparing trophic network architectures
}
\description{
Computation of different embeddings from a list of directed graphs
}
\usage{
embedding(gList, method="metric2vec", groups=NULL)
}
\arguments{
  \item{gList}{A \code{list} of directed graph objects of class \code{igraph}. An edge must go from the predator to the prey.}
  \item{method}{Embedding method. This should be (an unambiguous
  abbreviation of) one of \code{"metric2vec"}, \code{"motif2vec"},
  \code{"group2vec"} or \code{"shortestpath2vec"}.}
  \item{groups}{A named vector of class \code{character} or
    \code{integer} indicating the group to
    which each node belongs to. The length of \code{groups} must correspond to the number of different nodes present in \code{gList}. The names \code{names(groups)} must
    correspond to the nodes names in \code{gList}. If \code{NULL}, the groups
    are not considered.}
}
\details{
  This function computes a graph embedding from a list of graph.

  Comparing the architecture of interaction networks in space or time is
  essential for understanding the assembly, trajectory, functioning and
  persistence of species communities. Graph embedding methods, which
  position networks into a vector space where nearby networks have
  similar architectures, could be ideal tools for this purposes.

  Here, the proposed embedding methods are:
  
  -\code{"metric2vec"}: each column is a network
  metric. \code{connectance}: network connectance,
  \code{modularity}: network modularity (undirected graph),
  \code{rangeTL}: trophic level range, (trophic level is computed as in Williams & Martinez, 2004), 
  \code{meanTL}: mean trophic level,
  \code{SWTL}: shortest weighted trophic level,
  \code{meanOmni}: mean level of omnivory (standard deviation of the
  trophic levels of the prey),
  \code{propOmni}: proportion of omnivorous species,
  \code{propCanib}: proportion of cannibal species,
  \code{meanNbPrey}: mean number of prey,
  \code{sdNbPrey}: standard deviation of the number of prey,
  \code{skewNbPrey}: skewness of the number of prey,
  \code{meanNbPred}: mean number of predators,
  \code{sdNbPred}: standard deviation of the number of predators,
  \code{skewNbPred}: skewness of the number of predators,
  \code{propBasal}: proportion of basal species (which have no prey),
  \code{propTop}: proportion of top species (which have no predator),
  \code{propInter}: proportion of intermediate species (which have prey and predator),
  \code{sdVulnerability}: standard deviation of the vulnerability (i.e. in-degree),
  \code{sdGeneralism}: standard deviation of the generalism (i.e. out-degree),
  \code{transitivity}: network transitivity,
  \code{diameter}: network diameter,
  \code{meanSP}: mean shortest path,
  \code{assortativity}: network assortativity.

  
  -\code{"motif2vec"}:  Graph motifs are small connected subgraphs with a well-defined
     structure. Each column is the proportion of each of the 13
     connected motifs as enumerated by the `motifs` function. 
  
  -\code{"group2vec"}: Groups proportion, where the groups are defined
   previously (e.g. taxonmic groups).
  
  -\code{"shortestpath2vec"}: Computing the proportion of shortest path
   lengths. Can use the groups as well, path length proportion is decomposed
   per source and target group in this case.
}
\value{
\code{embedding} returns an object of class \code{matrix} where rows
  corresponds to the differents graphs and columns to the different
  dimensions in the embedding space.
}
\references{
Christophe Botella, Stephane Dray, Catherine Matias, Vincent Miele &
Wilfried Thuiller, An appraisal of graph embeddings for comparing
trophic network architectures. Methods in Ecology and evolution (2021) <doi:10.1111/2041-210X.13738>

Williams, Richard J., and Neo D. Martinez. "Limits to trophic levels and omnivory in complex food webs: theory and data." The American Naturalist 163.3 (2004)
}
\author{
Authors: Christophe Botella, Stephane Dray, Catherine Matias, Vincent
Miele, Wilfried Thuiller

Maintainer: Vincent Miele <vincent.miele@univ-lyon1.fr>
}
\examples{
library(igraph)
# Generating a set of graphs with 3 groups
groups = rep(-1,60)
groups[1:20] = 1
groups[21:40] = 2
groups[41:60] = 3
names(groups) = as.character(1:60)

# A first set with a few links between groups 1 and 3
gList = c()
for(i in 1:5){
    pm <- rbind( c(0, .5, 0.05), c(0, 0, 0.5), c(0,0,0) )
    graphLoc <- sample_sbm(60, pref.matrix=pm, block.sizes=c(20,20,20), directed=TRUE)
    V(graphLoc)$name = as.character(1:60)
    gList = c(gList,list(graphLoc))
}
# A second set with many links between groups 1 and 3
for(i in (6:10)){
    pm <- rbind( c(0, .5, .5), c(0, 0, 0.5), c(0,0,0) )
    graphLoc <- sample_sbm(60, pref.matrix=pm, block.sizes=c(20,20,20), directed=TRUE)
    V(graphLoc)$name = as.character(1:60)
    gList = c(gList,list(graphLoc))
}
names(gList) = LETTERS[1:10]

# Computing different embeddings and
usermfrow <- par()$mfrow
par(mfrow=c(2,2))

embd <- embedding(gList, method="metric2vec")
pca <- prcomp(embd, rank=2)$x
plot(pca, main="metric2vec")
text(x=pca[,1], y=pca[,2], labels=rownames(embd), pos=2, col=c(rep("blue",5),rep("red",5)))

embd <- embedding(gList,  method="motif2vec")
pca <- prcomp(embd, rank=2)$x
plot(pca, main="motif2vec")
text(x=pca[,1], y=pca[,2], labels=rownames(embd), pos=2, col=c(rep("blue",5),rep("red",5)))

embd <- embedding(gList, method="shortestpath2vec")
pca <- prcomp(embd, rank=2)$x
plot(pca, main="shortestpath2vec")
text(x=pca[,1], y=pca[,2], labels=rownames(embd), pos=2, col=c(rep("blue",5),rep("red",5)))

embd <- embedding(gList, method="shortestpath2vec", groups)
pca <- prcomp(embd, rank=2)$x
plot(pca, main="shortestpath2vec_group")
text(x=pca[,1], y=pca[,2], labels=rownames(embd), pos=2, col=c(rep("blue",5),rep("red",5)))

par(mfrow=usermfrow)
}
