#' data.frame.to.RSDA.inteval.table.j
#' @keywords internal
data.frame.to.RSDA.inteval.table.j <- function(data.df) {
  sym.obj.names <- row.names(data.df)
  col.names.sym <- colnames(data.df)
  dim.sym <- dim(data.df)
  var.dist <- dim.sym[2] / 2
  sym.var.types <- rep("$I", var.dist)
  sym.var.names <- col.names.sym[seq(from = 1, by = 2, length.out = var.dist)]
  sym.var.length <- rep(2, var.dist)
  sym.var.starts <- seq(from = 2, by = 3, length.out = var.dist)
  meta <- as.data.frame(matrix("$I", nrow = dim.sym[1], ncol = dim.sym[2] * 1.5))

  colnames.sym <- rep("$I", dim.sym[2] * 1.5)

  indx <- 1:var.dist

  for (j in indx) {
    pos.ini <- 2 + 3 * (j - 1)
    pos.fin <- pos.ini + 1
    pos.ini.data <- 1 + 2 * (j - 1)
    pos.fin.data <- pos.ini.data + 1
    pos <- pos.ini:pos.fin
    pos.data <- pos.ini.data:pos.fin.data
    colnames.sym[pos] <- sym.var.names[j]
    meta[, pos] <- data.df[, pos.data]
  }

  colnames(meta) <- colnames.sym
  row.names(meta) <- sym.obj.names

  data.sym <- list(
    N = dim.sym[1], M = var.dist, sym.obj.names = sym.obj.names, sym.var.names = sym.var.names,
    sym.var.types = sym.var.types, sym.var.length = sym.var.length, sym.var.starts = sym.var.starts,
    meta = meta, data = data.df
  )

  return(data.sym)
}
