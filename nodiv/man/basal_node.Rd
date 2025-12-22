\name{basal_node}
\alias{basal_node}
\alias{nodenumbers}
\alias{nodes}
\alias{Parent}
\alias{Descendants}
\alias{Sister}
\alias{MostRecentAncestor}

\title{
Identify nodes in a phylogeny
}
\description{
Returns the internal (\code{ape}) node numbers corresponding to certain nodes in the phylogeny
}
\usage{
basal_node(tree)
nodenumbers(tree)
nodes(tree, all = FALSE)
Descendants(node, tree)
Parent(node, tree)
Sister(node, tree)
MostRecentAncestor(tips, tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
An object of type \code{phylo}, or alternatively an object of type \code{nodiv_data}
}
  \item{node}{
The internal node number of a focal node in the phylogeny
}
  \item{all}{
Give all node labels in order of occurrence. If FALSE, only non-empty node labels will be shown in alphabetical order
}
  \item{tips}{
The species to find the most recent common ancestor for
}
}
\details{
\code{Parent} gives the code immediately basal to the focal node, \code{Descendants} gives the nodes descending from the focal node, and \code{Sister} gives the sister node(s). \code{MostRecentAncestor} gives the most recent common node for the given tips.
}
\value{
The internal (\code{ape}) node number of the node
}

\author{
Michael Krabbe Borregaard
}

