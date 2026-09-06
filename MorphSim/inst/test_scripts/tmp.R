
tree <- ape::rtree(6)
tree$edge.length <-  tree$edge.length * 10

q <- list(rbind(c(-.5, .5), c(.5, -.5)))
geiger::sim.char(tree, q, model = "discrete", 10)

x <- tree

# count number of tips and branches
nedges <- nrow(x$edge) #nbranches
tips <- ape::Ntip(x) #nspecies
# identify the root
root = tips+1 # root

# "postorder" branch lengths
zphy <- ape::reorder.phylo(tree, "postorder")

el=zphy$edge.length
nchar<-length(model.matrix);
result<-array(0, dim=c(nspecies, nchar, nsim))
