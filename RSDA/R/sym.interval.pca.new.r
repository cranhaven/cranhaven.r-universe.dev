sym.interval.pca.new<-function (sym.data,
                                method = c("classic",
                                           "tops",
                                           "centers",
                                           "principal.curves",
                                           "optimized.distance",
                                           "optimized.variance",
                                           "fixed"),
                               fixed.matrix)
{
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }
  method <- match.arg(method)
  if (method == "classic") {
    if ((sym.data$sym.var.types[1] != "$C") && (sym.data$sym.var.types[1] !=
                                                "$I")) {
      stop("Variables have to be continuos or Interval")
    }
    if (sym.data$sym.var.types[1] == "$C") {
      res <- PCA(sym.data$data, scale.unit = TRUE, ncp = sym.data$M,
                 graph = FALSE)
    }
    else if (sym.data$sym.var.types[1] == "$I") {
      nn <- sym.data$N
      mm <- sym.data$M
      centers <- matrix(0, nn, mm)
      centers <- as.data.frame(centers)
      rownames(centers) <- sym.data$sym.obj.names
      colnames(centers) <- sym.data$sym.var.names
      for (i in 1:nn) for (j in 1:mm) centers[i, j] <- (sym.var(sym.data,
                                                                j)$var.data.vector[i, 1] + sym.var(sym.data,
                                                                                                   j)$var.data.vector[i, 2])/2
      res <- FactoMineR::PCA(centers, scale.unit = TRUE,
                             ncp = sym.data$M, graph = FALSE)
    }
    return(res)
  }
  if (method == "centers") {
    res<-centers.pca.j.new(sym.data)
    return(res)
  }
  if (method == "tops") {
    res <- vertex.pca.j(sym.data)
    return(res)
  }
  if (method == "principal.curves") {
    res <- sym.interval.pc(sym.data, "vertex", 150, FALSE,
                           TRUE, TRUE)
    return(res)
  }
  if (method == "optimized.distance") {
    res <- optim.pca.distance.j.new(sym.data)
    return(res)
  }
  if (method == "optimized.variance") {
    res <- optim.pca.variance.j.new(sym.data, num.dimension = 3)
    return(res)
  }
  if(method == "fixed"){
    res <-fixed.pca.j.new(sym.data, fixed.matrix)
    return(res)
  }
}
