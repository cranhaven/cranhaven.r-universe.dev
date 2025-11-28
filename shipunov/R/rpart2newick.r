Rpart2newick <- function(rpart.object) {
if (is.null(attr(rpart.object, "ylevels"))) stop("I need 'rpart' response as a factor")
fr <- rpart.object$frame
fr$var <- paste(fr$var)
if (nrow(fr) <= 1L) stop("Invalid tree, root only")
tree <- vector("list", 2)
.index <- function(node) {
## node is an integer in rpart's binary ordering system
## outputs a vector of ones and twos for dendrogram indexing
 column <- 1
 multiplier <- 1
 tmp <- node
 while (multiplier * 2 <= node) {
  tmp <- tmp - multiplier
  column <- column + 1
  multiplier <- multiplier * 2
 }
 out <- integer(column - 1)
 for(i in seq_along(out)) {
  multiplier <- multiplier / 2
  if (tmp <= multiplier) {
    out[i] <- 1
  } else {
    out[i] <- 2
    tmp <- tmp - multiplier
  }
 }
out
}
##
for (i in 2:nrow(fr)) {
 ind <- .index(as.numeric(rownames(fr)[i]))
 ind <- paste0("[[", paste0(ind, collapse = "]][["), "]]")
 if(fr$var[i] == "<leaf>") {
  subtree <- attr(rpart.object, "ylevels")[fr$yval[i]]
  } else {
  subtree <- vector("list", 2)
  }
 eval(parse(text = paste0("tree", ind, " <- subtree")))
}
## this is "list to Newick", based on how as.character() works with lists
nn <- gsub('\"| ', '', gsub('c\\(|list\\(', '(', paste0('(', paste(tree, collapse=','), ');')))
## add node labels:
lbls <- labels(rpart.object) # all labels
lpos <- which(unlist(strsplit(nn, "[(,]")) == "") # because single open parenthesis becomes ""
lbls <- lbls[lpos] # we need labels only for nodes (open parentheses), not terminals (text, possibly with comma or closing parentheses)
lbls[lbls == "root"] <- "" # remove root node label
.PPadd <- function(txt, labels){ # adds labels to closing matches of the each opening parenthesis
txts <- unlist(strsplit(txt, NULL))
cpp <- opp <- which(txts == "(")
txtn <- Recode4(txts, c("(", ")"), c(-1, 1), 0)
txtl <- length(txtn)
for (i in seq_along(opp)) {
 pos <- (opp[i] + which(cumsum(txtn[opp[i]:txtl]) == 0)[1]) - 1 # we need the first match
 txts[pos] <- paste0(txts[pos], labels[i])
 }
paste0(txts, collapse="")
}
.PPadd(nn, lbls)
}
