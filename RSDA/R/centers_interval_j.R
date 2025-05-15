#' process.histogram.variable
#' @keywords internal
centers.interval.j <- function(sym.data) {
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }

  if ((sym.data$sym.var.types[1] != "$I")) {
    stop("Variables have to be continuos or Interval")
  } else {
    nn <- sym.data$N
  }
  mm <- sym.data$M
  centers <- matrix(0, nn, mm)
  ratios <- matrix(0, nn, mm)
  centers <- as.data.frame(centers)
  ratios <- as.data.frame(ratios)
  rownames(centers) <- sym.data$sym.obj.names
  colnames(centers) <- sym.data$sym.var.names
  rownames(ratios) <- sym.data$sym.obj.names
  colnames(ratios) <- sym.data$sym.var.names
  for (i in 1:nn) {
    for (j in 1:mm) {
      sym.var.act <- sym.var(sym.data, j)
      min.val <- sym.var.act$var.data.vector[i, 1]
      max.val <- sym.var.act$var.data.vector[i, 2]
      centers[i, j] <- (min.val + max.val) / 2
      ratios[i, j] <- (-min.val + max.val) / 2
    }
  }
  return(list(centers = centers, ratios = ratios))
}
