## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.dim = c(6, 4)
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(phylospatial); library(terra); library(ape); library(sf)

# simulate data
set.seed(1234)
n_taxa <- 5
x <- y <- 10
tree <- rtree(n_taxa)
comm <- rast(array((sin(seq(0, pi*12, length.out = n_taxa * x * y)) + 1)/2, 
                   dim = c(x, y, n_taxa)))
names(comm) <- tree$tip.label

# create phylospatial object
ps <- phylospatial(comm, tree)
ps

## ----tree, message=FALSE, warning=FALSE---------------------------------------
names(ps)

ps$tree

plot(ps, "tree")

## ----comm, message=FALSE, warning=FALSE---------------------------------------
head(ps$comm)

plot(ps, "comm")

## ----moss, message=FALSE, warning=FALSE---------------------------------------
moss_comm <- rast(system.file("extdata", "moss_comm.tif", package = "phylospatial"))
moss_tree <- read.tree(system.file("extdata", "moss_tree.nex", package = "phylospatial"))
ps <- phylospatial(moss_comm, moss_tree)
plot(ps, "comm")
plot(ps, "tree", type = "fan", show.tip.label = FALSE)

## ----species, message=FALSE, warning=FALSE------------------------------------
ps <- phylospatial(comm)
plot(ps, "tree", type = "fan")

