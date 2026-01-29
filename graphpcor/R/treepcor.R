#' @describeIn treepcor
#' A tree from a formula for each parent.
#' @param ... a list of formula used as relationship
#' to define a three for correlation modeling, see [treepcor()].
#' Parent nodes shall be in the right side while children
#' (or parent with a parent) in the left side.
#' @details
#' In the formula, the left side are parent variables,
#' and the right side include all the children and
#' parents that are also children.
#' The children variables are those with an ancestor (parent),
#' and are identified as `c1`, ..., `cn`, where `n` is the
#' total number of children variables.
#' The parent variables are identified as `p1`, ..., `pm`,
#' where the `m` is the number of parent variables.
#' The main parent (first) should be identified as `p1`.
#' Except `p1` all the other parent variables
#' have an ancestor, which is a parent variable.
#' @return a `treepcor` object
#' @importFrom stats as.formula
#' @export
#' @example demo/treepcor.R
treepcor <- function(...) {

  fch <- as.character(match.call())[-1]
  if(length(fch)<1)
    stop("Please provide an argument!")

  lchar <- lapply(fch, function(x)
    as.character(as.formula(x)))

  ## left side check
  pp <- sapply(lchar, function(x) x[2])
  if (length(pp)>length(unique(pp))) {
    stop("Non-unique parent definition!")
  }
  if(!all(sapply(pp, substr, 1, 1) == "p"))
    stop("Please use the letter 'p' for parent!")
  if(any(is.na(as.integer(
    sapply(lchar, function(x)
      substring(x[2], 2)))))) {
    stop("Please use integer after letter 'p' for parent!")
  }

  ## left side order
  pplab <- paste0("p", 1:length(pp))
  opp <- pmatch(pplab, pp)
  if(any(is.na(opp))) {
    stop(paste("Missing definition for",
               paste(pplab[is.na(opp)],
                     collapse = ", ")))
  }
  fch <- fch[opp]
  lchar <- lchar[opp]
  pp <- pp[opp]
  m <- length(pp)
  P <- sort(pp)

  ## right side check, and collect terms
  terms.l <- vector("list", m)
  terms.i <- vector("list", m)
  terms.s <- vector("list", m)
  for(i in 1:m) {
    x <- gsub(" ", "", lchar[[i]][3])
    ##    print(c(x = x))
    if(substr(x, 1, 1) != "-")
      x <- paste0("+", x)
    ##  print(c(x = x))
    i0 <- gregexpr("-", x, fixed = TRUE)[[1]]
    i1 <- gregexpr("+", x, fixed = TRUE)[[1]]
    si <- integer(max(i0, i1))
    if(any(i0>0))
      si[i0] <- -1
    if(any(i1>0))
      si[i1] <- +1
    ##print(si)
    terms.s[[i]] <- si[si != 0]
    schi <- strsplit(x, "-", fixed = TRUE)[[1]]
    ##    print(schi)
    schi <- unlist(strsplit(schi, "+", fixed = TRUE))
    ##  print(schi)
    if(schi[1]=="") schi <- schi[-1]
    ch.l <- substr(schi, 1, 1)
    ##print(c(ch.l))
    if(!all(ch.l %in% c("p", "c")))
      stop("Invalid variable labeling in ~ ", lchar[[i]][3])
    jj.i <- as.integer(substring(schi, 2))
    ##print(jj.i)
    if(any(is.na(jj.i)) | any(jj.i<1))
      stop("Invalid variable numbering in ~ ", lchar[[i]][3])
    ipar <- ch.l == "p"
    if(any(ipar)) {
      if(any(jj.i[ipar] <= 1))
        stop("Parent id in ~ ", lchar[[i]][3], " must be >", 1)
      if(any(jj.i[ipar] > m))
        stop("Wrong parent id in ~ ", lchar[[i]][3])
      iord <- order(jj.i + ifelse(ipar, -max(jj.i+1), 0))
    } else {
      iord <- order(jj.i)
    }
    terms.s[[i]] <- terms.s[[i]][iord]
    terms.l[[i]] <- ch.l[iord]
    terms.i[[i]] <- jj.i[iord]
    sthi <- c(" - ", "", " + ")[2 + terms.s[[i]]]
    fch[i] <- paste0(
      "p", i, " ~",
      paste0(sthi, terms.l[[i]],
             terms.i[[i]], collapse = ""))
  }

  rterms <- paste0(
    unlist(terms.l),
    unlist(terms.i))
  urterms <- unique(rterms)
  if(length(rterms) > length(urterms)) {
    stop("Duplicated defintion for ",
         unique(rterms[duplicated(rterms)]))
  }

  ic <- unlist(terms.l) == "c"
  if(any(ic)) {
    n <- max(unlist(terms.i)[ic])
  } else{
    n <- 0L
  }

  trm <- matrix(0L, n+m, m)
  colnames(trm) <- paste0("p", 1:m)
  if(n>0) {
    rownames(trm) <- c(
      paste0("c", 1:n),
      paste0("p", 1:m)
    )
  } else {
    stop("There is no children variable!")
  }

  for(i in 1:m) {
    icc <- terms.l[[i]] == "c"
    if(any(icc)) {
      trm[terms.i[[i]][icc], i] <- terms.s[[i]][icc]
    }
    npc <- sum(!icc)
    if(npc>0) {
      trm[n + terms.i[[i]][!icc], i] <- terms.s[[i]][!icc]
    }
  }

  trm <- trm[-(n+1), , drop = FALSE]

  nr <- rowSums(trm != 0)
  if(any(nr==0)) {
    stop("Missing definition for ",
         rownames(trm)[nr==0])
  }

  class(fch) <- "treepcor"
  attr(fch, "children") <- n
  attr(fch, "parent") <- m
  attr(fch, "relationship") <- trm

  return(fch)

}
#' @describeIn treepcor
#' The `print` method for a `treepcor`
#' @param x treepcor
#' @export
print.treepcor <- function(x, ...) {
  cat("treepcor for",
      attr(x, "children"),
      "children and",
      attr(x, "parent"),
      "parent variables\n")
  for(i in 1:length(x)) {
    cat(x[[i]], "\n")
  }
}
#' @describeIn treepcor
#' The `summary` method for a `treepcor`
#' @param object treepcor
#' @export
summary.treepcor <- function(object, ...) {
  attr(object, "relationship")
}
#' @describeIn treepcor
#' The `dim` for a `treepcor`
#' @export
dim.treepcor <- function(x, ...) {
  trm <- attr(x, "relationship")
  m <- ncol(trm)
  c(children = nrow(trm) - m + 1, parent = m)
}
#' @describeIn treepcor
#' The `drop1` method for a `treepcor`
#' @param object treepcor
#' @importFrom INLAtools is.zero
#' @export
setMethod(
  "drop1",
  "treepcor",
  function(object) {
    stopifnot((m <- length(object))>1)
    trm0 <- attr(object, "relationship")
    stopifnot(ncol(trm0) == m)
    ilast <- which(rownames(trm0) == (colnames(trm0)[m]))
    iplast <- which(!is.zero(trm0[ilast, ]))
    trm0[, iplast] <- trm0[, iplast] + trm0[, m]
    trm <- trm0[-ilast, 1:(m-1), drop = FALSE]

    args <- lapply(1:(m-1), function(i) {
      j <- !is.zero(trm[, i])
      s <- ifelse(trm[j, i] < 0, "-", "+")
      if(s[1] == "+") s[1] <- ""
      e <- paste(
        colnames(trm)[i],
        "~",
        paste(s, rownames(trm)[j], collapse = "")
      )
      e
    }
    )
    do.call(
      "treepcor",
      args)
  }
)
#' @describeIn treepcor
#' Extract the edges of a `treepcor` to be used for plot
#' @param object treepcor
#' @param which not used (TO DO: )
#' @importFrom methods new
#' @importFrom graph edges
#' @export
setMethod(
  "edges",
  "treepcor",
  function(object, which, ...) {
    trm <- attr(object, "relationship")
    m <- ncol(trm)
    n <- nrow(trm)-m+1
    if(m>1) {
      trm <- trm[c(n+1:(m-1), 1:n), ]
      stopifnot(all(substr(rownames(trm)[1:(m-1)],1,1) == "p"))
    }
    stopifnot(all(substr(rownames(trm)[m:nrow(trm)],1,1) == "c"))
    stopifnot(all(substr(rownames(trm),1,1) %in% c("p", "c")))

    edgl <- vector("list", m + n)
    names(edgl) <- c(paste0("p", 1:m),
                     paste0("c", 1:n))
    for(i in 1:m) {
      w1 <- trm[, i] != 0
      edgl[[i]] <- list(
        n = sum(w1),
        edges = rownames(trm)[w1],
        weights = new("numeric", trm[w1, i])
      )
      edgl[[i]]$term <- edgl$edges
      edgl[[i]]$parent <- substr(edgl[[i]]$edges, 1, 1) == "p"
      edgl[[i]]$id <- new("integer", substring(edgl[[i]]$edges, 2))
      edgl[[i]]$sign <- ifelse(edgl[[i]]$weights<0, -1, 1)
    }
    return(edgl)
  }
)
#' @describeIn treepcor
#' The `plot` method for a `treepcor`
#' @param x treepcor object
#' @param y not used
#' @export
setMethod(
  "plot",
  "treepcor",
  function(x, y, ...) {
    edgl <- edges(x)
    nodes <- names(edgl)
    gr <- graph::graphNEL(
      nodes = nodes,
      edgeL = edgl,
      edgemode = 'directed')

    trm <- attr(x, "relationship")
    m <- ncol(trm)
    n <- nrow(trm)-m+1

    mc <- lapply(
      match.call(expand.dots = TRUE)[-1],
      eval)
    nargs <- names(mc)

    ppars <- list(
      color =  {
        if(any(nargs == "color"))
          mc$color[1:2]
        else
          c("red", "blue")
      },
      fillcolor = {
        if(any(nargs == "fillcolor"))
          mc$fillcolor[1:2]
        else
          c("lightsalmon", "lightblue")
      },
      shape = {
        if(any(nargs == "shape"))
          mc$shape[1:2]
        else
          c("box", "circle")
      },
      height = {
        if(any(nargs == "height"))
          mc$height[1:2]
        else
          c(0.5, 0.5)
      },
      width = {
        if(any(nargs == "width"))
          mc$width[1:2]
        else
          c(0.75, 0.75)
      },
      fontsize = {
        if(any(nargs == "fontsize"))
          mc$fontsize
        else
          c(14, 14)
      }
    )
    nattr <- lapply(
      ppars, rep, times = c(m, n))
    for(i in 1:length(nattr))
      names(nattr[[i]]) <- nodes

    ag <- Rgraphviz::agopen(gr, "", nodeAttrs = nattr)
    for(k in 1:length(ag@AgEdge)) {
      i <- pmatch(ag@AgEdge[[k]]@tail, names(edgl))
      j <- pmatch(ag@AgEdge[[k]]@head,
                  edgl[[i]]$edges)
      ag@AgEdge[[k]]@color <- c(
        "red", "black", "blue")[edgl[[i]]$weights[j]+2]
      if(any(nargs == "lwd"))
        ag@AgEdge[[k]]@lwd <- mc$lwd[1]
    }

    getMethod("plot", "Ragraph")(ag)
}
)
#' @describeIn treepcor
#' The `prec` for a `treepcor`
#' @param model treepcor
#' @param ... to be used to pass `theta` as a
#' numeric vector with the model parameters
#' @export
prec.treepcor <- function(model, ...) {
  d <- dim(model)
  trm <- attr(model, "relationship")
  edgl <- edges(model)
  q.el <- etreepcor2precision(edgl[1:d[2]])
  mc <- list(...)
  nargs <- names(mc)
  Q <- q.el$q
  if(any(nargs == "theta")) {
    stopifnot(length(mc$theta) ==
                max(q.el$i1th, q.el$i2th))
    nc <- q.el$nc
    Q[q.el$iq2th] <- Q[q.el$iq2th] +
      exp(-2 * mc$theta[q.el$i2th])
    Q[q.el$iq1th] <- -1.0 * q.el$sth *
      exp(-mc$theta[q.el$i1th])
  }
  rownames(Q) <- colnames(Q) <-
    names(edgl)[c(d[2] + 1:d[1], 1:d[2])]
  return(Q)
}
#' @describeIn treepcor
#' Internal function to extract elements to
#' build the precision from the `treepcor` edges.
#' @param d.el list of first n edges of a `treepcor`.
etreepcor2precision <- function(d.el) {
  stopifnot(all(substr(names(d.el), 1, 1) == "p"))
  stopifnot(length(d.el) == length(unique(names(d.el))))
  ip <- as.integer(substring(names(d.el), 2))
  stopifnot(length(ip) == length(unique(ip)))
  np <- length(ip)
  nc <- sum(sapply(d.el, function(x) sum(!x$parent)))
  p.nc <- sapply(d.el, function(x) x$n)
  dd <- c(rep(1, nc), p.nc)
  stopifnot((nc + np) == length(dd))
  q0 <- diag(x = dd, nrow = nc + np, ncol = nc + np)
  ij <- matrix(1:((nc+np)^2), nc+np, nc+np)
  iq1th <- integer(2 * (np - 1))
  sch <- iq1ch <- integer(2*nc)
  sth <- i1th <- integer(np-1)
  iq2th <- i2th <- integer(np)
  k0 <- 0
  k2 <- k1 <- 0
  for(i in 1:np) {
    i0 <- which(!d.el[[i]]$parent)
    nci <- length(i0)
    if(nci>0) {
      j <- d.el[[i]]$id[i0]
      sch[k0 + 1:nci] <- d.el[[i]]$sign[i0]
      iq1ch[k0 + 1:nci] <- ij[(col(ij) == (nc+i)) & (row(ij) %in% j)]
      k0 <- k0 + nci
      sch[k0 + 1:nci] <- d.el[[i]]$sign[i0]
      iq1ch[k0 + 1:nci] <- ij[(row(ij) == (nc+i)) & (col(ij) %in% j)]
      k0 <- k0 + nci
      q0[j, nc+i] <- -d.el[[i]]$sign[i0]
      q0[nc+i, j] <- -d.el[[i]]$sign[i0]
    }
    i2th[k1 + 1] <- i
    iq2th[k1 + 1] <- ij[(col(ij) == (nc+i)) & (row(ij) == (nc+i))]
    k1 <- k1 + 1
    i0 <- which(d.el[[i]]$parent)
    nj <- length(i0)
    if(nj>0) {
      j0 <- d.el[[i]]$id[i0]
      i1th[k2 + 1:nj] <- j0
      sth[k2 + 1:nj] <- d.el[[i]]$sign[i0] ## carry on the sign
      j <- nc + j0
      iq1th[k2 + 1:nj] <- ij[, nc+i][j]
      k2 <- k2 + nj
      i1th[k2 + 1:nj] <- j0
      sth[k2 + 1:nj] <- d.el[[i]]$sign[i0] ## carry on the sign
      iq1th[k2 + 1:nj] <- ij[nc+i, ][j]
      k2 <- k2 + nj
      q0[j, nc+i] <- -d.el[[i]]$sign[i0]
      q0[nc+i, j] <- -d.el[[i]]$sign[i0]
    }
  }
  stopifnot(k1 == np)
  stopifnot(k2 == (2*(np-1)))
  return(list(
    nc = as.integer(nc),
    np = as.integer(np),
    i2th = as.integer(i2th),
    iq2th = as.integer(iq2th),
    i1th = as.integer(i1th),
    iq1th = as.integer(iq1th),
    iq1ch = as.integer(iq1ch),
    sch = as.double(sch),
    sth = as.double(sth),
    q = q0
  ))
}
#' @describeIn treepcor
#' The `vcov` method for a `treepcor`
#' @importFrom stats cov2cor
#' @param object treepcor
#' @param ... used to pass `theta` as a numeric vector
#' with the model parameters
#' @export
setMethod(
  "vcov",
  "treepcor",
  function(object, ...) {
    mc <- list(...)
    nargs <- names(mc)
    nm <- dim(object)
    edgl <- edges(object)
    ij <- etreepcor2variance(edgl[1:nm[2]])
    np <- length(ij$iv)
    nc <- length(ij$iparent)
    stopifnot(all(c(nc, np) == nm))
    if(any(nargs == "theta")) {
      if(length(mc$theta) == sum(nm)) {
        sigmas <- exp(mc$theta[1:nm[1]])
        vi <- sapply(ij$iv, function(i)
          sum(exp(2 * mc$theta[nm[1]+i])))
      } else {
        sigmas <- rep(1, nm[1])
        vi <- sapply(ij$iv, function(i)
          sum(exp(2 * mc$theta[i])))
      }
    } else {
      sigmas <- rep(1, nm[1])
      vi <- sapply(ij$iv, function(i) length(i))
    }
    vv <- diag(nc) + vi[ij$itop]
    rownames(vv) <- colnames(vv) <- names(edgl)[np + 1:nc]
    c0 <- cov2cor(t(vv * ij$schildren) * ij$schildren)
    return(diag(sigmas) %*% c0 %*% diag(sigmas))
  }
)
#' @describeIn treepcor
#' Internal function to extract elements to
#' build the covariance matrix from a `treepcor`.
#' @param d.el list of the first n edges of a `treepcor`.
etreepcor2variance <- function(d.el) {
  np <- length(d.el)
  iv <- lapply(1:np, function(i) i)
  for(i in 1:np) {
    ip <- which(d.el[[i]]$parent)
    if(length(ip)>0) {
      jj <- d.el[[i]]$id[ip]
      for(j in jj) {
        iv[[j]] <- c(iv[[j]], iv[[i]])
      }
    }
  }
  NC <- sum(sapply(d.el, function(s)
    sum(!s$parent)))
  sch <- integer(NC)
  iP <- integer(NC)
  for(i in 1:np) {
    ic <- which(!d.el[[i]]$parent)
    if(length(ic)>0) {
      sch[d.el[[i]]$id[ic]] <-
        d.el[[i]]$sign[ic]
      for(j in 1:length(ic)) {
        iP[d.el[[i]]$id[ic[j]]] <- i
      }
    }
  }
  itop <- matrix(0L, NC, NC)
  for(i in 1:NC) {
    for(j in 1:NC) {
      itop[i, j] <- max(intersect(iv[[iP[i]]], iv[[iP[j]]]))
    }
  }
  stopifnot(all.equal(iP,diag(itop)))
  return(list(iparent = iP, iv = iv, itop = itop, schildren=sch))
}
#' @describeIn treepcor
#' The `cgeneric` method for `treepcor`, uses [cgeneric_treepcor()]
#' @export
cgeneric.treepcor <- function(model, ...) {
  args <- list(...)
  args$model <- model
  do.call(what = 'cgeneric_treepcor',
          args = args)
}
