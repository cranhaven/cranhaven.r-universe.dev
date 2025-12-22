\name{plot_nodes_phylo}
\alias{plot_nodes_phylo}

\title{
Plot a phylogeny with colored node labels
}
\description{
A wrapper for the plot.phylo function defined in package \code{ape}, which is useful for plotting the values at each node
}
\usage{
plot_nodes_phylo(variable, tree, label = variable, main = deparse(substitute(variable)),
  zlim = NULL, col, show.legend = TRUE, sig.cutoff, nodes, 
  roundoff = TRUE, show.tip.label = NULL, cex = NULL, ...)
}

\arguments{
  \item{variable}{
The variable to be plotted
}
  \item{label}{
The text to write in each node label
}
  \item{tree}{
The phylogeny to plot, in the \code{ape phylo} format
}
  \item{main}{
The title text
}
  \item{zlim}{
The values to use for the most extreme colors of the color scale
}
  \item{col}{
The color palette used to create the color scale
}
  \item{show.legend}{
Should a legend for the color scale be plotted?
}
  \item{sig.cutoff}{
The minimum value for nodes to be plotted on the tree, useful for trees with many nodes
}
  \item{nodes}{
The nodes to be plotted on the tree
}
  \item{roundoff}{
Whether to round values when writing them as nodelabel text
}
  \item{show.tip.label}{
Show tip labels on the tree
}
  \item{cex}{
Controls the size of the node symbols
}
  \item{\dots}{
Further arguments to pass to plot.phylo
}
}

\author{
Michael Krabbe Borregaard
}
\examples{
data(coquettes)
Clade_occupancy <- Node_occupancy(coquettes)
plot_nodes_phylo(Clade_occupancy, coquettes$phylo, cex = 0.7)
}

