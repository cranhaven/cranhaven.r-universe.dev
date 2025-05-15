#' sym.interval.pca.limits.new.j
#' @keywords internal
sym.interval.pca.limits.new.j <- function(sym.data, prin.comp, num.vertex) {
  num.vars <- sym.data$M
  num.ind <- sym.data$N

  res <- as.data.frame(prin.comp)

  nn <- sym.data$N
  sym.indiv <- rep("X", sum(num.vertex))

  start <- 1
  finish <- num.vertex[1]
  sym.indiv[start:finish] <- sym.data$sym.obj.names[1]

  for (i in 2:nn) {
    previous <- num.vertex[i - 1]
    start <- start + previous
    finish <- num.vertex[i] + finish
    sym.indiv[start:finish] <- sym.data$sym.obj.names[i]
  }

  res$symindiv <- sym.indiv
  var.type <- rep("$I", num.vars)
  variables <- colnames(prin.comp)
  sym.res <- classic.to.sym(x = res, concept = symindiv)
  return(sym.res)
}
