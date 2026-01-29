#' @describeIn graphpcor
#' Each term to represent a node, and
#' each `~` to represent an edge.
#' @param ... a list of arguments
#' @importFrom stats as.formula
#' @export
#' @example demo/graphpcor.R
graphpcor.formula <- function(...) {
  fch <- as.character(match.call())[-1]
  m <- length(fch)
  if(m<1)
    stop("Please provide an argument!")
  ch <- lapply(fch, function(x)
    as.character(as.formula(x)))
  ## right side check, and collect terms
  terms.r <- lapply(ch, function(x) {
    x <- gsub(" ", "", x[3])
    schi <- strsplit(x, "-", fixed = TRUE)[[1]]
    schi <- unlist(strsplit(schi, "+", fixed = TRUE))
    if(schi[1]=="") schi <- schi[-1]
    return(schi)
  })
  nodesL <- sapply(ch, function(x) x[2])
  allNodes <- unique(unlist(lapply(1:m, function(i)
    c(nodesL[i], terms.r[[i]]))))
  nNodes <- length(allNodes)
  ## graph
  grel <- matrix(0, nNodes, nNodes,
                 dimnames = list(allNodes, allNodes))
  for(i in 1:m) {
    ii <- pmatch(nodesL[i], allNodes)
    jj <- pmatch(terms.r[[i]], allNodes)
    if((length(ii)>0) & (length(jj)>0)) {
      grel[ii, jj] <- 1
      grel[jj, ii] <- 1
    }
  }
  class(fch) <- 'graphpcor'
  attr(fch, 'nodes') <- allNodes
  attr(fch, 'graph') <- grel
  return(fch)
}
#' @describeIn graphpcor
#' Build a `graphpcor` from a matrix
#' @importFrom stats as.formula
#' @importFrom INLAtools is.zero
#' @export
graphpcor.matrix <- function(...) {
  x <- list(...)[[1]]
  stopifnot(all.equal(x, t(x)))
  ne <- c(nrow(x), NA)
  iz <- is.zero(x, tol = 1e-9)
  ne[2] <- sum(lower.tri(iz) & (!iz))
  vnams <- rownames(x)
  if(is.null(vnams)) {
    vnams <- letters[1:ne[1]]
  }
  argl <- list()
  adde <- 0
  nj <- integer(ne[1]-1)
  for(i in 1:(ne[1]-1)) {
    jj <- intersect(
      (i+1):ne[1],
      which(!iz[i, ]))
    nj[i] <- length(jj)
    if(nj[i]>0) {
      argl[[i]] <- paste(
        vnams[i], "~",
        paste(vnams[jj], collapse = " + "))
      adde <- adde + length(jj)
    }
  }
  stopifnot(adde==ne[2])
  argl <- argl[which(nj>0)]
  return(do.call(what = 'graphpcor',
                 args = lapply(argl, as.formula)))
}
#' @describeIn graphpcor
#' The print method for `graphpcor`
#' @param x graphpcor
#' @export
print.graphpcor <- function(x, ...) {
  n <- length(attr(x, 'nodes'))
  g <- !is.zero(attr(x, 'graph'))
  cat("A graphpcor for", n, "variables",
      "with", sum(lower.tri(g) & g), "edges.\n")
}
#' @describeIn graphpcor
#' The summary method for `graphpcor`
#' @param object graphpcor
#' @export
summary.graphpcor <- function(object, ...) {
  attr(object, "graph")
}
#' @describeIn graphpcor
#' The dim method for `graphpcor`
#' @export
dim.graphpcor <- function(x, ...) {
  c(nodes=length(attr(x, 'nodes')),
    edges=sum(attr(x, 'graph'))/2)
}
#' @describeIn graphpcor
#' Extract the edges of a `graphcor` to be used for plot
#' @param object graphpcor object
#' @param which not used
#' @importFrom graph edges
#' @export
setMethod(
  "edges",
  "graphpcor",
  function(object, which, ...) {
    ne <- dim(object)
    nodes <- attr(object, "nodes")
    stopifnot(!is.null(nodes))
    stopifnot(ne[1]==length(nodes))
    Q1 <- Laplacian(object)
    edgl <- vector("list", ne[1])
    for(i in 1:ne[1]) {
      jj <- setdiff(which(!is.zero(Q1[i, ])), i)
      ni <- length(jj)
      if(ni>0) {
        edgl[[i]] <- list(
          n = ni,
          edges = nodes[jj],
          weights = rep(1.0, ni))
        edgl[[i]]$term <- jj
      }
    }
    names(edgl) <- nodes
    return(edgl)
  }
)
#' @describeIn graphpcor
#' The plot method for `graphpcor`
#' @param y not used
#' @importFrom methods getMethod
#' @export
setMethod(
  "plot",
  "graphpcor",
  function(x, y, ...) {
    ne <- dim(x)
    nodes <- attr(x, "nodes")
    stopifnot(!is.null(nodes))
    stopifnot(ne[1]==length(nodes))
    edgl <- edges(x, which)
    gr <- graph::graphNEL(
      nodes = nodes,
      edgeL = edgl,
      edgemode = 'undirected')

    mc <- lapply(
      match.call(expand.dots = TRUE)[-1],
      eval)
    nargs <- names(mc)
    nattr <- list(color = {
      if(any(nargs=="color")) mc$color
      else rep("blue", ne[1])
    },
    fillcolor = {
      if(any(nargs == "fillcolor"))
        mc$fillcolor
      else rep("lightblue", ne[1])
    },
    shape = {
      if(any(nargs == "shape"))
        mc$shape
      else
        rep("circle", ne[1])
    },
    height = {
      if(any(nargs == "height"))
        mc$height
      else
        rep(0.5, ne[1])
    },
    width = {
      if(any(nargs == "width"))
        mc$width
      else
        rep(1.5, ne[1])
    },
    fontsize = {
      if(any(nargs == "fontsize"))
        mc$fontsize
      else
        rep(14, ne[1])
    }
    )
    for(i in 1:length(nattr))
      names(nattr[[i]]) <- nodes

    ag <- Rgraphviz::agopen(gr, "", nodeAttrs = nattr)

    for(k in 1:length(ag@AgEdge)) {
      ag@AgEdge[[k]]@color <- "red"
    }
    getMethod("plot", "Ragraph")(ag)
  }
)
#' @describeIn Laplacian
#' The Laplacian of a matrix
#' @export
Laplacian.matrix <- function(x) {
  if(inherits(x, "matrix")) {
    A <- 1 - is.zero(x)
    if(any(A!=t(A)))
      warning("Not symmetric!")
    L <- diag(rowSums(A)) - A
  } else {
    Laplacian.default(x)
  }
}
#' @describeIn graphpcor
#' The Laplacian method for a `graphpcor`
#' @export
Laplacian.graphpcor <- function(x) {
  ne <- dim(x)
  nodes <- attr(x, "nodes")
  L <- -attr(x, "graph")
  diag(L) <- -colSums(L)
  return(L)
}
#' @describeIn graphpcor
#' The `vcov` method for a `graphpcor`
#' @importFrom methods getMethod
#' @export
setMethod(
  "vcov",
  "graphpcor",
  function(object, ...) {
    ne <- dim(object)
    p <- ne[1]
    m <- ne[2]
    stopifnot(p>0)
    stopifnot(m>0)

    G <- Laplacian(object)
    names2 <- dimnames(G)
    stopifnot(ne[1]==nrow(G))
    stopifnot((2*ne[2])==(sum(!is.zero(G))-ne[1]))

    ## collect theta
    mc <- list(...)
    nargs <- names(mc)
    if(!any(nargs == "theta")) {
      stop("Please provide 'theta'!")
    }
    theta <- mc$theta
    ## setup full theta
    if(length(theta)==ne[2]) {
      theta <- c(rep(0.0, ne[1]), theta)
    } else {
      stopifnot(length(theta)==sum(ne))
    }

    ## collect/define 'd0'
    if(any(nargs == "d0")) {
      d0 <- mc$d0
    } else {
      d0 <- ne[1]:1
    }

    ## build lower Cholesky of Q0
    itheta <- which(lower.tri(G) & (!is.zero(G)))
    LQ0 <- Lprec0(
      theta = theta[-(1:ne[1])],
      p = ne[1],
      itheta = itheta,
      d0 = ne[1]:1)

    ## std
    V <- chol2inv(t(LQ0))
    si <- exp(theta[1:ne[1]]) / sqrt(diag(V))
    V <- diag(si) %*% V %*% diag(si)
    dimnames(V) <- names2

    return(V)
  }
)
#' @describeIn graphpcor
#' The precision method for 'graphpcor'
#' @param model graphpcor model object
#' @importFrom Matrix Matrix forceSymmetric
#' @importFrom INLAtools Sparse prec
#' @export
prec.graphpcor <- function(model, ...) {
  V <- vcov(model, ...)
  Q <- chol2inv(chol(V))
  Q[is.zero(Q)] <- 0
  return(Sparse(forceSymmetric(Matrix(Q)),
                zeros.rm = TRUE))
}
#' Evaluate the hessian of the KLD for a `graphpcor`
#' correlation model around a base model.
#' @param func model definition of a graphical model.
#' This can be either a matrix or a 'graphpcor'.
#' @param x either a reference correlation matrix
#' or a numeric vector with the parameters for the
#' reference 'graphpcor' model.
#' @param method see [numDeriv::hessian()]
#' @param method.args see [numDeriv::hessian()]
#' @param ... use to pass the decomposition method,
#' as a character to specify which one is to be used
#' to compute H^0.5 and H^(1/2).
#' @return list containing the hessian,
#' its 'square root', inverse 'square root' along
#' with the decomposition used
#' @importFrom stats cov2cor
#' @importFrom numDeriv hessian
#' @importFrom INLAtools is.zero
#' @export
hessian.graphpcor <- function(
    func,
    x,
    method = "Richardson",
    method.args = list(),
    ...) {

  if(TRUE) {
    d <- dim(func)
    iL <- which(lower.tri(diag(d[1])) &
                  !is.zero(Laplacian(func)))
    dotArgs <- list(...)
    if(any(names(dotArgs)=="d0")) {
      d0 <- dotArgs$d0
    } else {
      d0 <- d[1]:1
    }
    basepcor(base = x, p = d[1],
             itheta = iL, d0 = d0)$I0

  } else { ## old code
    decomposition <- c("svd", "eigen", "chol")
    if(is.null(list(...)$decomposition)) {
      decomposition <- "svd"
    } else {
      decomposition <- match.arg(
        list(...)$decomposition,
        decomposition)
    }
    Q0 <- Laplacian(func)
    nEdges <- sum((!is.zero(Q0, tol = 1e-9)) & lower.tri(Q0))
    z0 <- is.zero(Q0, tol = 1e-9)
    n <- nrow(Q0)
    l1 <- t(chol(Q0 + diag(1.0, n, n)))
    if(inherits(x, "Matrix")) {
      x <- as.matrix(x)
    }
    if(inherits(x, "matrix")) {
      ## maybe find theta that gives it close to C0?
      ## For now check the elements of L from C0^{-1}
      C0 <- cov2cor(x)
      qq0 <- chol2inv(chol(C0))
      ll0 <- t(chol(qq0))
      for(i in 1:n)
        ll0[i, ] <- (n+1-i)*ll0[i, ]/ll0[i, i]
      c0.ok <- all(which(abs(ll0)>sqrt(.Machine$double.eps)) %in%
                     which(abs(l1)>0))
      if(!c0.ok) {
        stop("Provided base correlation is not in the graphpcor model class!")
      }
      x <- ll0[lower.tri(ll0) & (!z0)]
    } else {
      stopifnot(length(x) == nEdges)
      C0 <- vcov(func, theta = x)
    }
    itheta <- which(lower.tri(Q0) & (!is.zero(Q0)))
    ## hessian uses graphpcor:::KLD10
    H <- Hcorrel(
      theta = x,
      p = n,
      parametrization = "itp",
      itheta = itheta,
      d0 = n:1,
      C0 = C0,
      decomposition = decomposition)
    return(H)
  }
}
#' @describeIn graphpcor
#' The `cgeneric` method for `graphpcor` uses [cgeneric_graphpcor()]
#' @importFrom INLAtools cgeneric
#' @export
cgeneric.graphpcor <- function(model, ...) {
  args <- list(...)
  args$model <- model
  do.call(what = 'cgeneric_graphpcor',
          args = args)
}
#' @describeIn graphpcor
#' The `cgeneric` method for `matrix` uses [cgeneric_graphpcor()]
#' @export
cgeneric.matrix <- function(model, ...) {
  args <- list(...)
  args$model <- graphpcor(model)
  do.call(what = 'cgeneric_graphpcor',
          args = args)
}
