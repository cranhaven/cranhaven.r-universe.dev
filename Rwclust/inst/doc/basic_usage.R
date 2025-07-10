## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(Rwclust)
library(igraph)

## ----load_example_data--------------------------------------------------------
data(example1, package="Rwclust")
head(example1)

## ----plot_example_data, fig.align='center', fig.width=7, fig.height=5---------
labels <- c(1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4)

G <- igraph::graph_from_edgelist(as.matrix(example1[, c(1,2)]), directed=FALSE)
G <- igraph::set_edge_attr(G, "weight", value=example1$weight)

plot(G, edge.label=E(G)$weight, vertex.color=labels, layout=layout_with_fr)

## ----sharpen_edge_weights-----------------------------------------------------
result <- rwclust(example1, iter=6, k=3)

## ----plot_edge_weights, fig.align='center', fig.width=7, fig.height=5---------
G_sharpened <- igraph::graph_from_edgelist(as.matrix(example1[, c(1,2)]), directed=FALSE)
E(G_sharpened)$weights <- round(weights(result),0)
plot(G_sharpened, edge.label=E(G_sharpened)$weights, vertex.color=labels, layout=layout_with_fr)

## ----plot_histogram, fig.align='center', fig.width=7, fig.height=5------------
plot(result, cutoff=25, breaks=20)

## ----compute_components, fig.align='center', fig.width=7, fig.height=5--------
# delete edges with weights below the threshold
edges_to_keep <- which(weights(result) > 25)
example1_c <- example1[edges_to_keep, ]
example1_c$weight <- weights(result)[edges_to_keep]

G_c <- igraph::graph_from_edgelist(as.matrix(example1_c[, c(1,2)]), directed=FALSE)

# compute the connected components
clusters <- igraph::components(G_c)$membership

plot(G_c, vertex.color=clusters)

