#' Interval Principal Components Analysis.
#' @rdname sym.pca
#' @aliases sym.interval.pca
#' @author Oldemar Rodriguez Rojas
#' @description Cazes, Chouakria, Diday and Schektman (1997)
#' proposed the Centers and the Tops Methods to extend the well known principal
#' components analysis method to a particular kind of symbolic objects
#' characterized by multi--values variables of interval type.
#' @param sym.data Shoud be a symbolic data table
#' @param ... further arguments passed to or from other methods.
#' @return
#' Sym.Components: This a symbolic data table with the interval principal components. As
#' this is a symbolic data table we can apply over this table any other symbolic data
#' analysis method (symbolic propagation).
#'
#' Sym.Prin.Correlations: This is the interval correlations between the original interval
#' variables and the interval principal components, it can be use to plot the symbolic
#' circle of correlations.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#'
#' Cazes P., Chouakria A., Diday E. et Schektman Y. (1997).  Extension de l'analyse en
#' composantes principales a des donnees de type intervalle, Rev. Statistique Appliquee,
#' Vol. XLV Num. 3 pag. 5-24, France.
#'
#' Chouakria A. (1998)
#' Extension des methodes d'analysis factorialle a des
#' donnees de type intervalle, Ph.D. Thesis, Paris IX Dauphine University.
#'
#' Makosso-Kallyth S. and Diday E. (2012).  Adaptation of interval PCA to symbolic histogram
#' variables, Advances in Data Analysis and Classification July, Volume 6, Issue 2, pp 147-159.
#'
#' Rodriguez, O. (2000).
#' Classification et Modeles Lineaires en Analyse des Donnees Symboliques. Ph.D. Thesis,
#' Paris IX-Dauphine University.
#' @seealso sym.histogram.pca
#' @examples
#' \dontrun{
#' data(oils)
#' res <- sym.pca(oils, "centers")
#'
#' sym.scatterplot(res$Sym.Components[, 1], res$Sym.Components[, 1],
#'   labels = TRUE, col = "red", main = "PCA Oils Data"
#' )
#' sym.scatterplot3d(res$Sym.Components[, 1], res$Sym.Components[, 2],
#'   res$Sym.Components[, 3],
#'   color = "blue", main = "PCA Oils Data"
#' )
#' sym.scatterplot.ggplot(res$Sym.Components[, 1], res$Sym.Components[, 2],
#'   labels = TRUE
#' )
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' res <- sym.pca(oils, "classic")
#' plot(res, choix = "ind")
#' plot(res, choix = "var")
#'
#' data(lynne2)
#' res <- sym.pca(lynne2, "centers")
#'
#' sym.scatterplot(res$Sym.Components[, 1], res$Sym.Components[, 2],
#'   labels = TRUE, col = "red", main = "PCA Lynne Data"
#' )
#' sym.scatterplot3d(res$Sym.Components[, 1], res$Sym.Components[, 2],
#'   res$Sym.Components[, 3],
#'   color = "blue", main = "PCA Lynne Data"
#' )
#' sym.scatterplot.ggplot(res$Sym.Components[, 1], res$Sym.Components[, 2],
#'   labels = TRUE
#' )
#' sym.circle.plot(res$Sym.Prin.Correlations)
#'
#' data(StudentsGrades)
#' st <- StudentsGrades
#' s.pca <- sym.pca(st)
#' plot(s.pca, choix = "ind")
#' plot(s.pca, choix = "var")
#' }
#' @keywords PCA Intervals
#' @export
#'
#'
sym.pca <- function(sym.data, ...) {
  UseMethod("sym.pca")
}

#' @rdname sym.pca
#' @param method It is use so select the method, 'classic' execute a classical principal component
#' analysis over the centers of the intervals, 'tops' to use the vertices algorithm
#' and 'centers' to use the centers algorithm.
#' @param fixed.matrix Classic Matrix. It is use when the method chosen is "fixed".
#' @export
#' @importFrom  FactoMineR PCA
#' @importFrom tibble as_tibble rownames_to_column
sym.pca.symbolic_tbl <- function(sym.data, method = c(
                                   "classic", "tops",
                                   "centers", "principal.curves",
                                   "optimized.distance",
                                   "optimized.variance",
                                   "fixed"
                                 ), fixed.matrix = NULL
                                 , ...) {
  all_interval <- all(sapply(sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  sym.data <- to.v2(sym.data)
  method <- match.arg(method)
  if (method == "classic") {
    if ((sym.data$sym.var.types[1] != "$C") && (sym.data$sym.var.types[1] != "$I")) {
      stop("Variables have to be continuos or Interval")
    }
    if (sym.data$sym.var.types[1] == "$C") {
      res <- FactoMineR::PCA(sym.data$data, scale.unit = TRUE, ncp = sym.data$M, graph = FALSE)
    } else if (sym.data$sym.var.types[1] == "$I") {
      nn <- sym.data$N
      mm <- sym.data$M
      centers <- matrix(0, nn, mm)
      centers <- as.data.frame(centers)
      rownames(centers) <- sym.data$sym.obj.names
      colnames(centers) <- sym.data$sym.var.names
      for (i in 1:nn) {
        for (j in 1:mm) {
          centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[
            i,
            1
          ] + sym.var(sym.data, j)$var.data.vector[i, 2]) / 2
        }
      }
      res <- FactoMineR::PCA(centers, scale.unit = TRUE, ncp = sym.data$M, graph = FALSE)
    }
    return(res)
  }
  if (method == "centers") {
    nn <- sym.data$N
    mm <- sym.data$M
    centers <- matrix(0, nn, mm)
    centers.stan <- matrix(0, nn, mm)
    min.stan <- matrix(0, nn, mm)
    max.stan <- matrix(0, nn, mm)
    for (i in 1:nn) {
      for (j in 1:mm) {
        centers[i, j] <- (sym.var(sym.data, j)$var.data.vector[
          i,
          1
        ] + sym.var(sym.data, j)$var.data.vector[i, 2]) / 2
      }
    }
    # Standarized
    for (i in 1:nn) {
      for (j in 1:mm) {
        centers.stan[i, j] <- (centers[i, j] - mean(centers[
          ,
          j
        ])) / (sd(centers[, j]) * sqrt((nn - 1) / nn))
      }
    }
    # Min-Max
    for (i in 1:nn) {
      for (j in 1:mm) {
        min.stan[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 1] - mean(centers[
          ,
          j
        ])) / (sd(centers[, j]) * sqrt((nn - 1) / nn))
        max.stan[i, j] <- (sym.var(sym.data, j)$var.data.vector[i, 2] - mean(centers[
          ,
          j
        ])) / (sd(centers[, j]) * sqrt((nn - 1) / nn))
      }
    }
    # Correlation Centers Matrix
    R <- t(centers.stan) %*% centers.stan
    svd <- eigen(R)
    sym.comp <- sym.data
    # Interval Principal Components
    for (i in 1:nn) {
      posd <- 1
      for (j in 1:mm) {
        smin <- 0
        smax <- 0
        for (k in 1:mm) {
          if (svd$vectors[k, j] < 0) {
            smin <- smin + max.stan[i, k] * svd$vectors[k, j]
          } else {
            smin <- smin + min.stan[i, k] * svd$vectors[k, j]
          }
          if (svd$vectors[k, j] < 0) {
            smax <- smax + min.stan[i, k] * svd$vectors[k, j]
          } else {
            smax <- smax + max.stan[i, k] * svd$vectors[k, j]
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
    # Interval Principal Correlationsz
    svdV <- matrix(0, nrow = nn, ncol = mm)
    for (i in 1:nn) {
      for (j in 1:mm) {
        ss <- 0
        for (k in 1:mm) {
          ss <- ss + centers.stan[i, k] * svd$vectors[k, j]
        }
        svdV[i, j] <- (1 / sqrt(abs(svd$values[j]))) * ss
      }
    }
    IPrinCorre <- matrix(0, mm, 2 * mm)
    for (i in 1:mm) {
      pcol <- 1
      for (j in 1:mm) {
        smin <- 0
        smax <- 0
        for (k in 1:nn) {
          if (svdV[k, j] < 0) {
            smin <- smin + (1 / sqrt(nn)) * max.stan[k, i] * svdV[k, j]
          } else {
            smin <- smin + (1 / sqrt(nn)) * min.stan[k, i] * svdV[k, j]
          }
          if (svdV[k, j] < 0) {
            smax <- smax + (1 / sqrt(nn)) * min.stan[k, i] * svdV[k, j]
          } else {
            smax <- smax + (1 / sqrt(nn)) * max.stan[k, i] * svdV[k, j]
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
    sym.comp <- to.v3(sym.comp)
    IPrinCorre <- tibble::rownames_to_column(IPrinCorre, var = "varname")
    IPrinCorre <- tibble::as_tibble(IPrinCorre)
    out <- list(
      Sym.Components = sym.comp,
      Sym.Prin.Correlations = IPrinCorre
    )
    class(out) <- c("symbolic_pca", class(out))
    return(out)
  }
  if (method == "tops") {
    res <- vertex.pca.j(sym.data)
    class(res) <- c("symbolic_pca", class(res))
    return(res)
  }
  if (method == "principal.curves") {
    res <- sym.interval.pc(sym.data, "vertex", 150, FALSE, FALSE, TRUE)
    class(res) <- c("symbolic_pca_curves", class(res))
    return(res)
  }
  if (method == "optimized.distance") {
    res <- optim.pca.distance.j(sym.data)
    #res <- optim.pca.distance.j.new(sym.data)
    class(res) <- c("symbolic_pca_optimized", class(res))
    return(res)
  }
  if (method == "optimized.variance") {
    res <- optim.pca.variance.j(sym.data, num.dimension = 3)
    #res <- optim.pca.variance.j.new(sym.data, num.dimension = 3)
    class(res) <- c("symbolic_pca_optimized", class(res))
    return(res)
  }
  if(method == "fixed"){
    res <-fixed.pca.j.new(sym.data, fixed.matrix)
    return(res)
  }
  return(TRUE)
}

#' sym.scale.interval
#' @keywords internal
sym.scale.interval <- function(sym.data, mean.var, desv.var) {
  data <- sym.data$data
  M <- sym.data$M
  for (i in 1:M) {
    indx <- (2 * i - 1):(2 * i)
    data[, indx] <- (data[, indx] - mean.var[i]) / desv.var[i]
  }
  return(data)
}

#' sym.interval.vertex.pca.j
#' @keywords internal
sym.interval.vertex.pca.j <- function(data.sym) {
  vertex.sym <- vertex.interval.new.j(data.sym)
  data.vertex <- as.matrix(vertex.sym$vertex)
  dim.sym <- dim(data.vertex)
  indx.cols <- data.frame(i = 1:dim.sym[2])


  medias <- apply(indx.cols, 1, function(i) {
    mean(data.vertex[, i])
  })
  data.vertex.centrada <- t(t(data.vertex) - medias)

  desviaciones <- apply(indx.cols, 1, function(i) {
    sd(data.vertex[, i])
  })
  desviaciones <- desviaciones * sqrt((dim.sym[1] - 1) / dim.sym[1])

  data.vertex.centrada <- t(t(data.vertex.centrada) / desviaciones)

  matrix.data <- as.matrix(data.sym$data)
  matrix.data.centrada <- as.matrix(data.sym$data)
  m <- data.sym$M
  n <- data.sym$N

  indx <- data.frame(pos = 1:m)

  list.stand <- apply(indx, 1, function(i) {
    pos.ini <- 2 * (i - 1) + 1
    pos.fin <- 2 * i
    matrix.data.centrada[, pos.ini:pos.fin] <<- (matrix.data[, pos.ini:pos.fin] -
      medias[i]) / desviaciones[i]
  })

  cor.matrix <- t(data.vertex.centrada) %*% data.vertex.centrada / dim.sym[1]
  cor.matrix.eigen <- eigen(cor.matrix)
  vector.propios <- cor.matrix.eigen$vectors
  vector.propios.pos <- vector.propios

  vector.propios.pos <- apply(indx, 1, function(i) {
    apply(indx, 1, function(j) {
      if (vector.propios[j, i] > 0) {
        vector.propios[j, i]
      } else {
        0
      }
    })
  })


  vector.propios.neg <- vector.propios - vector.propios.pos

  indx.max <- seq(2, to = 2 * m, by = 2)
  indx.min <- seq(1, to = 2 * m, by = 2)

  max.neg <- matrix.data.centrada[, indx.max] %*% vector.propios.neg
  min.neg <- matrix.data.centrada[, indx.min] %*% vector.propios.neg

  max.pos <- matrix.data.centrada[, indx.max] %*% vector.propios.pos
  min.pos <- matrix.data.centrada[, indx.min] %*% vector.propios.pos

  maximos <- max.pos + min.neg
  minimos <- min.pos + max.neg

  names.sal <- paste0("Dim.", t(indx))
  sal.sym <- as.data.frame(matrix(rep(0, n * 2 * m), nrow = n))
  colnames(sal.sym)[indx.min] <- names.sal
  colnames(sal.sym)[indx.max] <- paste0(names.sal, ".1")

  sal.sym[, indx.max] <- maximos
  sal.sym[, indx.min] <- minimos

  row.names(sal.sym) <- data.sym$sym.obj.names

  return(list(
    Sym.Components = data.frame.to.RSDA.inteval.table.j(sal.sym), pos.coord.eigen = vector.propios.pos,
    neg.coord.eigen = vector.propios.neg, mean.vertex = medias, sd.vertex = desviaciones
  ))
}

#' pca.supplementary.vertex.fun.j
#' @keywords internal
pca.supplementary.vertex.fun.j <- function(x, N, M, sym.var.names, sym.data.vertex.matrix,
                                           tot.individuals) {
  M.x <- matrix(x, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")
  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix
  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }

  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)

  pca.min <- PCA(
    X = M.x, scale.unit = FALSE, ind.sup = (N + 1):tot.individuals,
    ncp = M, graph = FALSE
  )

  min.dist.pca <- pca.min$ind.sup$dist * pca.min$ind.sup$dist
  return(sum(min.dist.pca))
}

#' optim.pca.variance.j
#' @keywords internal
#' @importFrom nloptr lbfgs
optim.pca.variance.j <- function(sym.data, num.dimension) {
  N <- sym.data$N
  M <- sym.data$M
  num.dimen.aux <- num.dimension
  seq.min <- seq(from = 1, by = 2, length.out = M)
  seq.max <- seq(from = 2, by = 2, length.out = M)

  sym.var.names <- sym.data$sym.var.names
  sym.data.vertex <- vertex.interval.new.j(sym.data)
  sym.data.vertex.matrix <- sym.data.vertex$vertex
  dim.vertex <- dim(sym.data.vertex.matrix)[1]
  tot.individuals <- N + dim.vertex

  min.interval <- as.vector(as.matrix(sym.data$data[, seq.min]))
  max.interval <- as.vector(as.matrix(sym.data$data[, seq.max]))

  res.min <- nloptr::lbfgs(min.interval, pca.supplementary.vertex.lambda.fun.j,
    lower = min.interval,
    upper = max.interval, nl.info = FALSE, control = list(xtol_rel = 1e-08, maxeval = 20000),
    N = N, M = M, sym.var.names = sym.var.names, sym.data.vertex.matrix = sym.data.vertex.matrix,
    tot.individuals = tot.individuals, num.dimen.aux = num.dimen.aux
  )

  M.x <- matrix(res.min$par, nrow = N)
  colnames(M.x) <- sym.var.names
  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")
  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix

  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }
  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)
  pca.max <- PCA(
    X = M.x, scale.unit = FALSE, ind.sup = (N + 1):tot.individuals,
    ncp = M, graph = FALSE
  )

  pca.min.sym <- sym.interval.pca.limits.new.j(sym.data, pca.max$ind.sup$coord, sym.data.vertex$num.vertex)

  return(list(Sym.Components = pca.min.sym, pca.min = pca.max, res.max = res.min))
}

#' pca.supplementary.vertex.lambda.fun.j
#' @keywords internal
#' @export
pca.supplementary.vertex.lambda.fun.j <- function(x, M, N, sym.var.names, sym.data.vertex.matrix,
                                                  tot.individuals, num.dimen.aux) {
  M.x <- matrix(x, nrow = N)
  colnames(M.x) <- sym.var.names

  M.x <- scale(M.x)
  mean.var <- attr(M.x, "scaled:center")
  desv.var <- attr(M.x, "scaled:scale")

  sym.data.vertex.matrix.cent <- sym.data.vertex.matrix

  for (i in 1:M) {
    sym.data.vertex.matrix.cent[, i] <- (sym.data.vertex.matrix.cent[, i] - mean.var[i]) / desv.var[i]
  }

  M.x <- rbind(M.x, sym.data.vertex.matrix.cent)

  pca.max <- PCA(
    X = M.x, scale.unit = FALSE, ind.sup = (N + 1):tot.individuals,
    ncp = M, graph = FALSE
  )
  out <- list(pca.max = pca.max, out = -sum(pca.max$eig[(1:num.dimen.aux)]))

  return(-sum(pca.max$eig[(1:num.dimen.aux)]))
}

#' Plot symbolic PCA
#' @keywords internal
#' @export
plot.symbolic_pca <- function(x, choix = c("ind", "var"), axes = c(1,2), labels = TRUE, ...) {
  choix <- match.arg(choix)
  if (choix == "ind") {
    sym.scatterplot(
      x$Sym.Components[, axes[1]],
      x$Sym.Components[, axes[2]],
      labels = labels,
      ...
    )
  } else {
    sym.circle.plot(x$Sym.Prin.Correlations)
  }
}

#' @keywords internal
#' @export
plot.symbolic_pca_curves <- function(x, choix = c("ind", "var"), axes = c(1,2), labels = TRUE, ...) {
  choix <- match.arg(choix)
  if (choix == "ind") {
    sym.scatterplot(
      x$sym.prin.curve[,axes[1]],
      x$sym.prin.curve[,axes[2]],
      labels = labels,
      col = "red",
      ...
    )
  } else {
    sym.circle.plot(x$Sym.Prin.Correlations)
  }
}

#' @keywords internal
#' @export
plot.symbolic_pca_optimized <- function(x, choix = c("ind", "var"), axes = c(1,2),  labels = TRUE, ...) {
  choix <- match.arg(choix)
  if (choix == "ind") {
    sym.scatterplot(
      x$Sym.Components[,axes[1]],
      x$Sym.Components[,axes[2]],
      labels = labels,
      col = "red",
      ...
    )
  } else {
    plot(x$pca.min, choix = "var")
  }
}

