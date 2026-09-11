## -----------------------------------------------------------------------------
#| label: load-package
library(imaginarycss)


## -----------------------------------------------------------------------------
#| label: list-example
# True network (4 nodes)
true_net <- matrix(c(
  0, 1, 0, 1,
  1, 0, 1, 0,
  0, 1, 0, 1,
  1, 0, 1, 0
), nrow = 4, byrow = TRUE)

# Person 1 over-perceives ties (false positives)
perceived_net1 <- matrix(c(
  0, 1, 1, 1,
  1, 0, 1, 0,
  1, 1, 0, 1,
  1, 0, 1, 0
), nrow = 4, byrow = TRUE)

# Person 2 under-perceives ties (false negatives)
perceived_net2 <- matrix(c(
  0, 1, 0, 0,
  1, 0, 0, 0,
  0, 0, 0, 1,
  0, 0, 1, 0
), nrow = 4, byrow = TRUE)

graph <- new_barry_graph(list(true_net, perceived_net1, perceived_net2))
print(graph)


## -----------------------------------------------------------------------------
#| label: matrix-example
# Build an 8x8 block-diagonal matrix (two 4x4 networks)
source_ <- c(1, 2, 3, 1)
target_ <- c(2, 1, 4, 4)
source_ <- c(source_, source_[-1] + 4)
target_ <- c(target_, target_[-1] + 4)

adjmat <- matrix(0L, nrow = 8, ncol = 8)
adjmat[cbind(source_, target_)] <- 1L

graph2 <- new_barry_graph(adjmat, n = 4)
print(graph2)


## -----------------------------------------------------------------------------
#| label: graph-attributes
#| collapse: true
# Network size and layer boundaries
data.frame(
  Attribute = c("Network size", "Endpoints"),
  Value     = c(attr(graph, "netsize"),
                paste(attr(graph, "endpoints"), collapse = ", "))
)

# First rows of the edge list
head(barray_to_edgelist(graph))

