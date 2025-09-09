## ----eval=FALSE---------------------------------------------------------------
#  install.packages("spreadr")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("csqsiew/spreadr")

## ----message=FALSE------------------------------------------------------------
library(spreadr)
library(igraph)

data("pnet")  # load and inspect the igraph object

## ----eval=FALSE---------------------------------------------------------------
#  plot(pnet)

## ----echo=FALSE---------------------------------------------------------------
set.seed(1)
layout <- layout_with_lgl(pnet)
plot(pnet, layout=layout)

## ----message=FALSE------------------------------------------------------------
library(spreadr)
library(igraph)
set.seed(1)

data("pnetm")
pnetm[1:5, 1:5]  # inspect the first few entries

## ----eval=FALSE---------------------------------------------------------------
#  plot(graph_from_adjacency_matrix(
#    pnetm, mode="undirected")) # visualise the graph

## ----echo=FALSE---------------------------------------------------------------
plot(graph_from_adjacency_matrix(
    pnetm, mode="undirected"),
  layout=layout)

## -----------------------------------------------------------------------------
start_run <- data.frame(
  node=      c("beach", "speck"),
  activation=c(     20,      10))

## -----------------------------------------------------------------------------
start_run <- data.frame(
  node =       c("beach", "speck"),
  activation = c(     20,      10),
  time =       c(      0,       5))

## ----echo=FALSE---------------------------------------------------------------
start_run <- data.frame(
  node =       c("beach", "speck"),
  activation = c(     20,      10))

## -----------------------------------------------------------------------------
result <- spreadr(pnet, start_run, include_t0=TRUE)

## ----eval=TRUE----------------------------------------------------------------
head(result)  # inspect the first few rows
tail(result)  # inspect the last few rows

## ----eval=FALSE---------------------------------------------------------------
#  write.csv(result, file="result.csv")  # save result as CSV file

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(result, aes(x=time, y=activation, color=node)) +
  geom_point() +
  geom_line()

## -----------------------------------------------------------------------------
weighted_network <- matrix(
  c(0, 1, 9,
    1, 0, 0,
    9, 0, 0), nrow=3, byrow=TRUE)
colnames(weighted_network) <- c("a", "b", "c")
rownames(weighted_network) <- c("a", "b", "c")

# To visualise the network only --- this is not necessary for spreadr
weighted_igraph <- graph_from_adjacency_matrix(
  weighted_network, mode="undirected", weighted=TRUE)
plot(weighted_igraph, edge.width=E(weighted_igraph)$weight)

## -----------------------------------------------------------------------------
spreadr(
  weighted_network, data.frame(node="a", activation=10),
  time=1, retention=0, include_t0=TRUE)

## -----------------------------------------------------------------------------
directed_network <- matrix(
  c(0, 1, 0,
    0, 0, 1,
    0, 0, 0), nrow=3, byrow=TRUE)
colnames(directed_network) <- c("a", "b", "c")
rownames(directed_network) <- c("a", "b", "c")

# To visualise the network only --- this is not necessary for spreadr
directed_igraph <- graph_from_adjacency_matrix(
  directed_network, mode="directed")
plot(directed_igraph, edge.width=E(directed_igraph)$weight)

## -----------------------------------------------------------------------------
spreadr(
  directed_network, data.frame(node="b", activation=10),
  time=1, retention=0, include_t0=TRUE)

## -----------------------------------------------------------------------------
params <- data.frame(
  retention=c(0, 0.5,   0, 0.5),
  decay=    c(0,   0, 0.5, 0.5))

## -----------------------------------------------------------------------------
network <- matrix(
  c(0, 1,
    0, 0), nrow=2, byrow=TRUE)
start_run <- data.frame(node=1, activation=10)

## -----------------------------------------------------------------------------
apply(params, 1, function(row)
  spreadr(
    network, start_run,
    time=2, include_t0=TRUE,
    retention=row[1], decay=row[2]))

## -----------------------------------------------------------------------------
sessionInfo()

