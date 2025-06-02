################    begin  block.fct   ##############################################

block.fct <-  function(...)
{
  #  Author: Joseph B. Lang
  #          Dept of Stat and Act Sci
  #          Univ of Iowa  (6/16/99, 3/30/04, last update: 5/1/09 )
  #
  #  Direct sum function.  Creates a block diagonal matrix.
  #
  n <- nargs()
  all.m <- list(...)
  rows <- 0
  cols <- 0
  for(i in 1:n) {
    rows <- rows + nrow(all.m[[i]])
    cols <- cols + ncol(all.m[[i]])
  }
  m <- matrix(0, rows, cols)
  r1 <- 1
  r2 <- 0
  c1 <- 1
  c2 <- 0
  dnames1 <- dnames2 <- c()
  for(i in 1:n) {
    mi <- all.m[[i]]
    r2 <- r2 + nrow(mi)
    c2 <- c2 + ncol(mi)
    m[r1:r2, c1:c2] <- mi
    r1 <- r2 + 1
    c1 <- c2 + 1
    if (is.null(rownames(mi))) {rownames(mi) <- paste(  sep="","[",i,".",1:nrow(mi),"]"  ) }
    if (is.null(colnames(mi))) {colnames(mi) <- paste(sep="","[",i,".",1:ncol(mi),"]") }
    dnames1 <- c(dnames1,dimnames(mi)[[1]])
    dnames2 <- c(dnames2,dimnames(mi)[[2]])
  }
  dimnames(m) <- list(dnames1,dnames2)
  m
}
##############  end  block.fct #########################################