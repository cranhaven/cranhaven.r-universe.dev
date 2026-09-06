
tree <- ape::rtree(6)
p = 2
states = as.character(c(0:p))
levels = states
x = tree

# phangorn
sim.site <- function(x, states, rate = 1, ancestral = FALSE, Q = NULL) {

  #TODO check if levels is vector, change to state

  # x = tree
  # levels = states
  # bf = base frequencies
  # Q
  states = 2
  Q = NULL
  bf = NULL
  l = 2 #alignment length

  lbf <- length(states)
  if (is.null(bf)) bf <- rep(1 / lbf, lbf)
  if (is.null(Q)) {
    Q <- rep(1, lbf * (lbf - 1) / 2) #?
  }
  if (is.matrix(Q)) Q <- Q[lower.tri(Q)]
  eig <- edQt(Q, bf) # decomposition #?

  m <- length(levels) # state number

  # simulate the root state
  rootseq <- sample(levels, l, replace = TRUE, prob = bf)

  # reorder branches in the phylogeny
  x <- reorder(x)
  edge <- x$edge
  nNodes <- max(edge)
  # create an empty matrix
  res <- matrix(NA, nNodes, l)
  parent <- as.integer(edge[, 1])
  child <- as.integer(edge[, 2])
  root <- as.integer(parent[!match(parent, child, 0)][1])
  # add the root state to the matrix
  res[root, ] <- rootseq
  # edge lengths
  tl <- x$edge.length
  # loop through all edges
  for (i in seq_along(tl)){
    from <- parent[i]
    to <- child[i]
    # generate a matrix of probabilities
    P <- getP(tl[i], eig, rate)[[1]]
    # avoid numerical problems for larger P and small t
    if (any(P < 0)) P[P < 0] <- 0
    #this is how FossilSim::sim.trait.values does it for a symmetic Mk matrix
    #P = (( (k - 1) / k ) * (1 - exp(-( k / (k - 1) ) * v * blength) ) ) )
    #r = sample(c(0:(k-1))[-(r+1)], 1)
    for (j in 1:m) {
      # boolean, if current state is TRUE then we calc the prob of changing from that state. Useful loop is sites > 1, otherwise some redundancy
      ind <- res[from, ] == levels[j]
      res[to, ind] <- sample(levels, sum(ind), replace = TRUE, prob = P[, j])
    }
  }
  k <- length(x$tip.label)
  label <- c(x$tip.label, as.character( (k + 1):nNodes))
  rownames(res) <- label
  # remove ancestral sequences
  if (!ancestral) res <- res[x$tip.label, , drop = FALSE]

  #return(phangorn::as.phyDat(res, type ="USER", levels = levels, return.index = TRUE))
  return(res)
}

edQt <- function(Q = c(1, 1, 1, 1, 1, 1), bf = c(0.25, 0.25, 0.25, 0.25)) {
  l <- length(bf)
  res <- matrix(0, l, l)
  res[lower.tri(res)] <- Q
  res <- res + t(res)
  res <- res * bf
  res2 <- res * rep(bf, each = l)
  diag(res) <- -colSums(res)
  res <- res / sum(res2)
  e <- eigen(res, FALSE)
  e$inv <- solve.default(e$vec)
  e
}

getP <- function(el, eig = edQt(), g = 1.0) {
 n <- length(eig$values)
 res <- .Call("getPM", eig, as.integer(n), as.double(el), as.double(g))
 attr(res, "dim") <- c(length(g), length(el))
 res
}

q <- list(rbind(c(-.5, .5), c(.5, -.5)))

# make.modelmatrix=function(m){
#     if(is.matrix(m)){
#       m=list(m)
#       for(j in 1:length(m)){
#         .check.Qmatrix(m[[j]])
#       }
#     }
#   return(m)
# }

### geiger



