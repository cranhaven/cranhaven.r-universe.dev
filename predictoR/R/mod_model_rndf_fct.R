#Funciones tomadas de los paquetes RATTLE Y RANDOMFOREST

rulesRandomForest <- function (model, n = 1, include.class = NULL, format = "", comment = "") 
{
  if (!inherits(model, "randomForest")) 
    stop(Rtxt("the model is not of the 'randomForest' class"))
  if (format == "VB") 
    comment = "'"
  trs      <- getTrees(model, n)
  tr.paths <- getRFPathNodesT(trs)
  tr.vars  <- attr(model$terms, "dataClasses")[-1]
  cat(sprintf("%sRandom Forest Model %d", comment, n), "\n\n")
  cat(paste(comment, "-------------------------------------------------------------\n", 
            sep = ""))
  if (format == "VB") 
    cat("IF FALSE THEN\n' This is a No Op to simplify the code\n\n")
  nrules <- 0
  for (i in seq_along(tr.paths)) {
    tr.path <- tr.paths[[i]]
    nodenum <- as.integer(names(tr.paths[i]))
    target <- levels(model$y)[trs[nodenum, "prediction"]]
    if (!is.null(include.class) && target %notinto% include.class) 
      (next)()
    cat(sprintf("%sTree %d Rule %d Node %d %s\n \n", comment, 
                n, i, nodenum, ifelse(is.null(target), "Regression (to do - extract predicted value)", 
                                      paste("Decision", target))))
    if (format == "VB") 
      cat("ELSE IF TRUE\n")
    nrules <- nrules + 1
    var.index <- trs[, 3][abs(tr.path)]
    var.names <- names(tr.vars)[var.index]
    var.values <- trs[, 4][abs(tr.path)]
    for (j in 1:(length(tr.path) - 1)) {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor" | 
          var.class == "ordered") {
        node.op <- "IN"
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binarys(var.values[j])
        bins <- c(bins, rep(0, length(var.levels) - length(bins)))
        if (tr.path[j] > 0) 
          node.value <- var.levels[bins == 1]
        else node.value <- var.levels[bins == 0]
        node.value <- sprintf("(\"%s\")", paste(node.value, 
                                                collapse = "\", \""))
      }
      else if (var.class == "integer" | var.class == "numeric") {
        if (tr.path[j] > 0) 
          node.op <- "<="
        else node.op <- ">"
        node.value <- var.values[j]
      }
      else stop(sprintf("Rattle E234: getRFRuleSet: class %s not supported.", 
                        var.class))
      if (format == "VB") 
        cat(sprintf("AND\n%s %s %s\n", var.names[j], 
                    node.op, node.value))
      else cat(sprintf("%d: %s %s %s\n", j, var.names[j], 
                       node.op, node.value))
    }
    if (format == "VB") 
      cat("THEN Count = Count + 1\n")
    cat("-----------------------------------------------------------------\n")
  }
  if (format == "VB") 
    cat("END IF\n\n")
  cat(sprintf("%sNumber of rules in Tree %d: %d\n\n", comment, 
              n, nrules))
}



getTrees <- function (rfobj, k = 1, labelVar = FALSE) 
{
  if (is.null(rfobj$forest)) {
    stop("No forest component in ", deparse(substitute(rfobj)))
  }
  if (k > rfobj$ntree) {
    stop("There are fewer than ", k, "trees in the forest")
  }
  if (rfobj$type == "regression") {
    tree <- cbind(rfobj$forest$leftDaughter[, k], rfobj$forest$rightDaughter[, 
                                                                             k], rfobj$forest$bestvar[, k], rfobj$forest$xbestsplit[, 
                                                                                                                                    k], rfobj$forest$nodestatus[, k], rfobj$forest$nodepred[, 
                                                                                                                                                                                            k])[1:rfobj$forest$ndbigtree[k], ]
  }
  else {
    tree <- cbind(rfobj$forest$treemap[, , k], rfobj$forest$bestvar[, 
                                                                    k], rfobj$forest$xbestsplit[, k], rfobj$forest$nodestatus[, 
                                                                                                                              k], rfobj$forest$nodepred[, k])[1:rfobj$forest$ndbigtree[k], 
                                                                                                                              ]
  }
  dimnames(tree) <- list(1:nrow(tree), c("left daughter", "right daughter", 
                                         "split var", "split point", "status", "prediction"))
  if (labelVar) {
    tree <- as.data.frame(tree)
    v <- tree[[3]]
    v[v == 0] <- NA
    tree[[3]] <- factor(rownames(rfobj$importance)[v])
    if (rfobj$type == "classification") {
      v <- tree[[6]]
      v[!v %in% 1:nlevels(rfobj$y)] <- NA
      tree[[6]] <- levels(rfobj$y)[v]
    }
  }
  tree
}
getRFPathNodesT <- function (tree, root = 1) 
{
  paths <- list()
  if (tree[root, "status"] == -1) {
    paths <- list(root)
    names(paths) <- root
  }
  else {
    lpaths <- getRFPathNodesT(tree, tree[root, "left daughter"])
    lpaths <- lapply(lpaths, append, root, 0)
    rpaths <- getRFPathNodesT(tree, tree[root, "right daughter"])
    rpaths <- lapply(rpaths, append, -root, 0)
    paths <- c(lpaths, rpaths)
  }
  return(paths)
}

sdecimal2binarys.small <- function (x) 
{
  if (x == 0) 
    return(0)
  if (x < 0) 
    stop(Rtxt("the input must be positive"))
  dec <- x
  n <- floor(log(x)/log(2))
  bin <- c(1)
  dec <- dec - 2^n
  while (n > 0) {
    if (dec >= 2^(n - 1)) {
      bin <- c(bin, 1)
      dec <- dec - 2^(n - 1)
    }
    else bin <- c(bin, 0)
    n <- n - 1
  }
  return(bin)
}
sdecimal2binarys <- function (x) 
{
  return(rev(sdecimal2binarys.small(x)))
}

`%notinto%` <- function (x, y) {
  !x %in% y
}
 

