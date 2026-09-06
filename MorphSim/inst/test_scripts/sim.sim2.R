#?  model.matrix = .make.modelmatrix(par, model)

Q <- list(rbind(c(-.5, .5), c(.5, -.5)))
Q <- list(rbind(c(-.75, .25, .25, .25), c(.25, -.75, .25, .25), c(.25, .25, -.75, .25), c(.25, .25, .25, -.75) ))

phy <- tree
root <- 1

# number of branches and tips
nbranches <- nrow(phy$edge)
nspecies <- ape::Ntip(phy)
# identify the root
rt <- nspecies + 1 # rename this root
# postorder nodes
zphy <- ape::reorder.phylo(phy, "postorder")
el <- zphy$edge.length

# we don't know what this is (yet)
nchar <- length(Q)

# create output object #TODO decide how to generate starting values
nsim <-1
result <- array(0, dim = c(nspecies, nchar, nsim))

# chooses a new state
.get.state <- function(s, p) { # state at node anc, p between anc and cur. NOT ACTUAL P
  pp = cumsum(p[s, ])
  min(which(runif(1) < pp))
}

# for each ?
for (j in 1:nchar) {
  m <- Q[[j]] # we probably don't want/need to do this

  if (!root %in% c(1:nrow(m))) stop(paste("'root' must be a character state from 1 to ", nrow(m), sep = ""))

  # probability of change along each branch
  p = lapply(el, function(l) ape::matexpo(m * l))

  # for each ?
  for (k in 1:nsim) {
    # identify root value - starts at route
    node.value <- numeric(nspecies + ape::Nnode(zphy)) # ?
    node.value[rt] <- root

    for (i in nbranches:1) {
      cur = zphy$edge[i, 2]
      anc = zphy$edge[i, 1]
      curp = p[[i]] # prob of change along branch between anc and cur
      s = node.value[anc] # state of the ancestral node
      node.value[cur] = .get.state(s, curp)
      print(node.value[cur])
    }
    result[, j, k] <- node.value[1:nspecies]
  }
}
rownames(result) <- zphy$tip.label

#####





make.modelmatrix=function(m, model=c("discrete")){

  if(is.matrix(m)){
      m=list(m)
      for(j in 1:length(m)){
        .check.Qmatrix(m[[j]])
      }
    }
  else {
    if(is.numeric(m)) m=as.matrix(m) else stop("Supply 'm' as a matrix of rates")
  }
  if(any(diag(m)<0)) stop("'m' appears to have negative variance component(s)")
  return(m)
}

q

make.modelmatrix(q, c("discrete"))

