#' Projections onto PCA
#'
#' @description Calculate the interval projection onto the principal components
#'
#' @param sym.data An interval matrix
#' @param matrix.stan A standardized matrix
#' @param min.stan A matrix of minimum values standardized for each interval
#' @param max.stan A matrix of maximum values standardized for each interval
#' @param svd An eigen vectors matrix
#' @param nn Number of concepts
#' @param mm Number of variables
#'
#' @return Concept Projections onto the principal components and correlation circle
#'

get.limits.PCA<-function(sym.data,matrix.stan,min.stan,max.stan,svd,nn,mm){
  sym.comp <- sym.data

  min.dim<-min(mm,ncol(svd$vectors))
  for (i in 1:nn) {
    posd <- 1
    for (j in 1:min.dim) {
      smin <- 0
      smax <- 0
      for (k in 1:mm) {
        if (svd$vectors[k, j] < 0) {
          smin <- smin + max.stan[i, k] * svd$vectors[k,j]
          smax <- smax + min.stan[i, k] * svd$vectors[k,j]
        }
        else{
          smin <- smin + min.stan[i, k] * svd$vectors[k,j]
          smax <- smax + max.stan[i, k] * svd$vectors[k,j]
        }
      }
      sym.comp$meta[i, sym.comp$sym.var.starts[j]] <- smin
      sym.comp$meta[i, sym.comp$sym.var.starts[j] + 1] <- smax
      sym.comp$data[i, posd] <- smin
      sym.comp$data[i, posd + 1] <- smax
      posd <- posd + 2
    }
  }
  pos <- 1
  for (j in 1:mm) {
    comp.name <- paste("C", j, sep = "")
    sym.comp$sym.var.names[j] <- comp.name
    comp.name <- paste("Min.C", j, sep = "")
    colnames(sym.comp$data)[pos] <- comp.name
    comp.name <- paste("Max.C", j, sep = "")
    colnames(sym.comp$data)[pos + 1] <- comp.name
    pos <- pos + 2
    comp.name <- paste("Min.C", j, sep = "")
    colnames(sym.comp$meta)[sym.comp$sym.var.starts[j]] <- comp.name
    comp.name <- paste("Max.C", j, sep = "")
    colnames(sym.comp$meta)[sym.comp$sym.var.starts[j] + 1] <- comp.name
  }

  if(nn > min.dim){
    svdV <- matrix(0, nn, nn)

    for (i in 1:nn) {
      for (j in 1:min.dim) {
        ss <- 0
        for (k in 1:mm) {
          ss <- ss + matrix.stan[i, k] * svd$vectors[k,j]
        }
        svdV[i, j] <- (1/sqrt(svd$values[j])) * ss
      }
    }
  }
  else{
    svdV <- matrix(0, min.dim, min.dim)

    for (i in 1:mm) {
      for (j in 1:mm) {
        ss <- 0
        for (k in 1:mm) {
          ss <- ss + matrix.stan[i, k] * svd$vectors[k,j]
        }
        svdV[i, j] <- (1/sqrt(svd$values[j])) * ss
      }
    }

  }


  IPrinCorre <- matrix(0, mm, 2 * mm)
  for (i in 1:mm) {
    pcol <- 1
    for (j in 1:min.dim) {
      smin <- 0
      smax <- 0
      for (k in 1:nn) {
        if (svdV[k, j] < 0) {
          smin <- smin + (1/sqrt(nn)) * max.stan[k,i] * svdV[k, j]
          smax <- smax + (1/sqrt(nn)) * min.stan[k,i] * svdV[k, j]
        }
        else{
          smin <- smin + (1/sqrt(nn)) * min.stan[k,i] * svdV[k, j]
          smax <- smax + (1/sqrt(nn)) * max.stan[k,i] * svdV[k, j]
        }
      }
      IPrinCorre[i, pcol] <- smin
      IPrinCorre[i, pcol + 1] <- smax
      pcol <- pcol + 2
    }
  }
  IPrinCorre <- as.data.frame(IPrinCorre)
  rownames(IPrinCorre) <- sym.data$sym.var.names
  class(sym.comp) <- "sym.data.table"
  return(list(Sym.Components = sym.comp, Sym.Prin.Correlations = IPrinCorre))
}
