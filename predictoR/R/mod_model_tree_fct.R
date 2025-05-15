path.rpart <- function (tree, nodes, pretty = 0, print.it = TRUE) 
{
  if (!inherits(tree, "rpart")) 
    stop("Not a legitimate \"rpart\" object")
  splits <- labels.rpart(tree, pretty = pretty)
  frame <- tree$frame
  n <- row.names(frame)
  node <- as.numeric(n)
  which <- descendants(node)
  path <- list()
  if (missing(nodes)) {
    xy <- rpartco(tree)
    while (length(i <- identify(xy, n = 1L, plot = FALSE)) > 
           0L) {
      path[[n[i]]] <- path.i <- splits[which[, i]]
      if (print.it) {
        cat("\n", "node number:", n[i], "\n")
        cat(paste("  ", path.i), sep = "\n")
      }
    }
  }
  else {
    if (length(nodes <- node.match(nodes, node)) == 0L) 
      return(invisible())
    for (i in nodes) {
      path[[n[i]]] <- path.i <- splits[which[, i]]
      if (print.it) {
        cat("\n", "node number:", n[i], "\n")
        cat(paste("  ", path.i), sep = "\n")
      }
    }
  }
  invisible(path)
}

#' Funcion rpart
#' @noRd
labels.rpart <- function (object, digits = 4, minlength = 1L, pretty, collapse = TRUE, 
          ...) 
{
  if (missing(minlength) && !missing(pretty)) {
    minlength <- if (is.null(pretty)) 
      1L
    else if (is.logical(pretty)) {
      if (pretty) 
        4L
      else 0L
    }
    else 0L
  }
  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L) 
    return("root")
  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow]
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
  irow <- index[c(whichrow, FALSE)]
  ncat <- object$splits[irow, 2L]
  lsplit <- rsplit <- character(length(irow))
  if (any(ncat < 2L)) {
    jrow <- irow[ncat < 2L]
    cutpoint <- formatg(object$splits[jrow, 4L], digits)
    temp1 <- (ifelse(ncat < 0, "< ", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "< "))[ncat < 2L]
    lsplit[ncat < 2L] <- paste0(temp1, cutpoint)
    rsplit[ncat < 2L] <- paste0(temp2, cutpoint)
  }
  if (any(ncat > 1L)) {
    xlevels <- attr(object, "xlevels")
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L]
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]
    if (minlength == 1L) {
      if (any(ncat > 52L)) 
        warning("more than 52 levels in a predicting factor, truncated for printout", 
                domain = NA)
      xlevels <- lapply(xlevels, function(z) c(letters, 
                                               LETTERS)[pmin(seq_along(z), 52L)])
    }
    else if (minlength > 1L) 
      xlevels <- lapply(xlevels, abbreviate, minlength, 
                        ...)
    for (i in seq_along(jrow)) {
      j <- jrow[i]
      splits <- object$csplit[crow[i], ]
      cl <- if (minlength == 1L) 
        ""
      else ","
      lsplit[j] <- paste((xlevels[[cindex[i]]])[splits == 
                                                  1L], collapse = cl)
      rsplit[j] <- paste((xlevels[[cindex[i]]])[splits == 
                                                  3L], collapse = cl)
    }
  }
  if (!collapse) {
    ltemp <- rtemp <- rep("<leaf>", n)
    ltemp[whichrow] <- lsplit
    rtemp[whichrow] <- rsplit
    return(cbind(ltemp, rtemp))
  }
  lsplit <- paste0(ifelse(ncat < 2L, "", "="), lsplit)
  rsplit <- paste0(ifelse(ncat < 2L, "", "="), rsplit)
  varname <- (as.character(vnames))
  node <- as.numeric(row.names(ff))
  parent <- match(node%/%2L, node[whichrow])
  odd <- (as.logical(node%%2L))
  labels <- character(n)
  labels[odd] <- paste0(varname[parent[odd]], rsplit[parent[odd]])
  labels[!odd] <- paste0(varname[parent[!odd]], lsplit[parent[!odd]])
  labels[1L] <- "root"
  labels
}



descendants<- function (nodes, include = TRUE) 
{
  n <- length(nodes)
  if (n == 1L) 
    return(matrix(TRUE, 1L, 1L))
  ind <- 1:n
  desc <- matrix(FALSE, n, n)
  if (include) 
    diag(desc) <- TRUE
  parents <- match((nodes%/%2L), nodes)
  lev <- floor(log(nodes, base = 2))
  desc[1L, 2L:n] <- TRUE
  for (i in max(lev):2L) {
    desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
    parents[lev == i] <- parents[parents[lev == i]]
    lev[lev == i] <- i - 1L
  }
  desc
}

formatg <- function (x, digits = getOption("digits"), format = paste0("%.", 
                                                           digits, "g")) 
{
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector")
  temp <- sprintf(format, x)
  if (is.matrix(x)) 
    matrix(temp, nrow = nrow(x))
  else temp
}


rpartco <- function (tree, parms) 
{
  frame <- tree$frame
  node <- as.numeric(row.names(frame))
  depth <- tree.depth(node)
  is.leaf <- (frame$var == "<leaf>")
  if (length(parms)) {
    uniform <- parms$uniform
    nspace <- parms$nspace
    minbranch <- parms$minbranch
  }
  else {
    uniform <- FALSE
    nspace <- -1
    minbranch <- 0.3
  }
  if (uniform) 
    y <- (1 + max(depth) - depth)/max(depth, 4L)
  else {
    y <- dev <- frame$dev
    temp <- split(seq(node), depth)
    parent <- match(node%/%2L, node)
    sibling <- match(ifelse(node%%2L, node - 1L, node + 1L), 
                     node)
    for (i in temp[-1L]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      y[i] <- y[parent[i]] - temp2
    }
    fudge <- minbranch * diff(range(y))/max(depth)
    for (i in temp[-1L]) {
      temp2 <- dev[parent[i]] - (dev[i] + dev[sibling[i]])
      haskids <- !(is.leaf[i] & is.leaf[sibling[i]])
      y[i] <- y[parent[i]] - ifelse(temp2 <= fudge & haskids, 
                                    fudge, temp2)
    }
    y <- y/(max(y))
  }
  x <- double(length(node))
  x[is.leaf] <- seq(sum(is.leaf))
  left.child <- match(node * 2L, node)
  right.child <- match(node * 2L + 1L, node)
  temp <- split(seq(node)[!is.leaf], depth[!is.leaf])
  for (i in rev(temp)) x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
  if (nspace < 0) 
    return(list(x = x, y = y))
  compress <- function(x, me, depth) {
    lson <- me + 1L
    if (is.leaf[lson]) 
      left <- list(left = x[lson], right = x[lson], depth = depth + 
                     1L, sons = lson)
    else {
      left <- compress(x, me + 1L, depth + 1L)
      x <- left$x
    }
    rson <- me + 1L + length(left$sons)
    if (is.leaf[rson]) 
      right <- list(left = x[rson], right = x[rson], depth = depth + 
                      1L, sons = rson)
    else {
      right <- compress(x, rson, depth + 1L)
      x <- right$x
    }
    maxd <- max(left$depth, right$depth) - depth
    mind <- min(left$depth, right$depth) - depth
    slide <- min(right$left[1L:mind] - left$right[1L:mind]) - 
      1L
    if (slide > 0) {
      x[right$sons] <- x[right$sons] - slide
      x[me] <- (x[right$sons[1L]] + x[left$sons[1L]])/2
    }
    else slide <- 0
    if (left$depth > right$depth) {
      templ <- left$left
      tempr <- left$right
      tempr[1L:mind] <- pmax(tempr[1L:mind], right$right - 
                               slide)
    }
    else {
      templ <- right$left - slide
      tempr <- right$right - slide
      templ[1L:mind] <- pmin(templ[1L:mind], left$left)
    }
    list(x = x, left = c(x[me] - nspace * (x[me] - x[lson]), 
                         templ), right = c(x[me] - nspace * (x[me] - x[rson]), 
                                           tempr), depth = maxd + depth, sons = c(me, left$sons, 
                                                                                  right$sons))
  }
  x <- compress(x, 1L, 1L)$x
  list(x = x, y = y)
}

node.match <- function (nodes, nodelist, leaves, print.it = TRUE) 
{
  node.index <- match(nodes, nodelist, 0L)
  bad <- nodes[node.index == 0L]
  if (length(bad) > 0 && print.it) 
    warning(gettextf("supplied nodes %s are not in this tree", 
                     paste(bad, collapse = ",")), domain = NA)
  good <- nodes[node.index > 0L]
  if (!missing(leaves) && any(leaves <- leaves[node.index])) {
    warning(gettextf("supplied nodes %s are leaves", paste(good[leaves], 
                                                           collapse = ",")), domain = NA)
    node.index[node.index > 0L][!leaves]
  }
  else node.index[node.index > 0L]
}

tree.depth <- function (nodes) 
{
  depth <- floor(log(nodes, base = 2) + 1e-07)
  depth - min(depth)
}
