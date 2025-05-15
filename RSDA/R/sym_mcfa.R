#' sym.mcfa
#' @aliases sym.mcfa
#' @author Jorge Arce
#' @param sym.data A symbolic data table containing at least two set type variables.
#' @param pos.var Column numbers in the symbolic data table that contain the set type variables.
#' @description This function executes a Multiple Correspondence Factor Analysis for variables of set type.
#' @usage sym.mcfa(sym.data, pos.var)
#' @references  Arce J. and Rodriguez, O. (2018). Multiple Correspondence Analysis for Symbolic Multi–Valued Variables. On the Symbolic Data Analysis Workshop SDA 2018.
#'
#' Benzecri, J.P. (1973). L' Analyse des Données. Tomo 2: L'Analyse des Correspondances. Dunod, Paris.
#'
#' Castillo, W. and Rodriguez O. (1997). Algoritmo e implementacion del analisis factorial de correspondencias. Revista de Matematicas: Teoria y Aplicaciones, 24-31.
#'
#' Takagi I. and Yadosiha H. (2011). Correspondence Analysis for symbolic contingency tables base on interval algebra. Elsevier Procedia Computer Science, 6, 352-357.
#'
#' Rodriguez, O. (2007). Correspondence Analysis for Symbolic Multi--Valued Variables. CARME 2007 (Rotterdam, The Netherlands), http://www.carme-n.org/carme2007.
#'
#' @examples
#' data("ex_mcfa1")
#' sym.table <- classic.to.sym(ex_mcfa1,
#'   concept = suspect,
#'   hair = sym.set(hair),
#'   eyes = sym.set(eyes),
#'   region = sym.set(region)
#' )
#' sym.table
#' @export

sym.mcfa <- function(sym.data, pos.var) {
  sym.data <- to.v2(sym.data)
  sym.data <- calc.burt.sym(sym.data, pos.var)
  idn <- all(sym.data$sym.var.types == sym.data$sym.var.types[1])
  if (idn == FALSE) {
    stop("All variables have to be of the same type")
  }
  res <- cfa.totals(sym.data)
  Z <- cfa.MatrixZ(sym.data, res$TotalRows, res$TotalColumns)
  svd <- eigen(Z)
  # se aplica cambio enviado por jorge
  svd$values <- svd$values * svd$values

  MVPRealz <- cfa.CVPRealz(
    sym.data, res$TotalRows, res$TotalColumns,
    res$Total, svd$vectors
  )
  Mzz <- cfa.Czz(
    sym.data, res$TotalRows, res$TotalColumns,
    MVPRealz, svd$values
  )
  CMM <- cfa.minmax.new(
    sym.data, res$TotalRows, res$TotalRowsMin,
    res$TotalRowsMax, res$TotalColumns, res$TotalColumnsMin,
    res$TotalColumnsMax, res$Total, MVPRealz, Mzz
  )
  n.sym.objects <- sym.data$N
  n.sym.var <- sym.data$M - 1
  sym.var.types <- list()
  sym.var.length <- rep(0, n.sym.var)
  sym.var.names <- list()
  sym.var.starts <- rep(0, n.sym.var)
  posd <- 1
  posdd <- 2
  for (ss in 1:n.sym.var) {
    sym.var.types[ss] <- "$I"
    sym.var.length[ss] <- 2
    sym.var.names[ss] <- paste("C", ss, sep = "")
    sym.var.starts[ss] <- posdd
    posd <- posd + 2
    posdd <- posdd + 3
  }
  sym.obj.names <- sym.data$sym.obj.names
  data.matrix <- matrix(0, n.sym.objects, 2 * n.sym.var)
  meta.data <- matrix(0, n.sym.objects, 2 * n.sym.var)
  for (i in 1:n.sym.objects) {
    posd <- 1
    for (j in 1:n.sym.var) {
      meta.data[i, posd] <- CMM$Min[i, j]
      meta.data[i, posd + 1] <- CMM$Max[i, j]
      data.matrix[i, posd] <- CMM$Min[i, j]
      data.matrix[i, posd + 1] <- CMM$Max[i, j]
      posd <- posd + 2
    }
  }
  col.types <- matrix(0, n.sym.objects, 1)
  for (ss in 1:n.sym.objects) {
    col.types[ss] <- "$I"
  }
  qq <- matrix(0, n.sym.objects, 3 * n.sym.var)
  qq <- cbind(col.types, meta.data[, 1:2])
  posd <- 3
  for (ss in 2:n.sym.var) {
    qq <- cbind(qq, col.types)
    qq <- cbind(qq, meta.data[, posd])
    qq <- cbind(qq, meta.data[, posd + 1])
    posd <- posd + 2
  }
  meta.data <- qq
  tt <- list()
  ttt <- list()
  posd <- 1
  posdd <- 1
  for (ss in 1:n.sym.var) {
    tt[posd] <- paste("C", ss, sep = "")
    tt[posd + 1] <- paste("C", ss, sep = "")
    posd <- posd + 2
    ttt[posdd] <- "$I"
    ttt[posdd + 1] <- paste("C", ss, sep = "")
    ttt[posdd + 2] <- paste("C", ss, sep = "")
    posdd <- posdd + 3
  }
  meta.data <- as.data.frame(meta.data)
  data.matrix <- as.data.frame(data.matrix)
  rownames(data.matrix) <- sym.obj.names
  colnames(data.matrix) <- tt
  rownames(meta.data) <- sym.obj.names
  colnames(meta.data) <- ttt
  posdd <- 1
  for (ss in 1:n.sym.var) {
    meta.data[, posdd + 1] <- as.numeric(as.vector(meta.data[
      ,
      posdd + 1
    ]))
    meta.data[, posdd + 2] <- as.numeric(as.vector(meta.data[
      ,
      posdd + 2
    ]))
    posdd <- posdd + 3
  }
  sym.data <- list(
    N = n.sym.objects, M = n.sym.var, sym.obj.names = sym.obj.names,
    sym.var.names = unlist(sym.var.names), sym.var.types = unlist(sym.var.types),
    sym.var.length = sym.var.length, sym.var.starts = unlist(sym.var.starts),
    meta = meta.data, data = data.matrix
  )
  class(sym.data) <- "sym.data.table"
  sym.data <- to.v3(sym.data)
  return(sym.data)
}

#' Burt Matrix
#'
#' @param sym.data ddd
#' @param pos.var ddd
#'
#' @export
#'
calc.burt.sym <- function(sym.data, pos.var) {
  num.vars <- length(pos.var)
  dim.matrix <- sum(sym.data$sym.var.length[pos.var])
  burt.sym <- as.data.frame(matrix(rep(0, 2 * dim.matrix^2), nrow = dim.matrix))

  pos.col <- pos.row <- 0

  sym.var.length <- sym.data$sym.var.length[pos.var]
  sym.var.length.2 <- sym.var.length * 2

  cum.sym.var.length <- cumsum(sym.var.length)
  cum.sym.var.length.2 <- cumsum(sym.var.length.2)

  pos.col.ini <- 0
  pos.row.ini <- 0

  for (i in 1:(num.vars - 1))
  {
    var.act <- sym.data[, pos.var[i]]

    pos.col.ini <- pos.col.ini + 1
    pos.row.ini <- pos.row.ini + 1

    pos.row <- cum.sym.var.length[i]
    pos.col <- cum.sym.var.length.2[i]

    K <- calc.k(var.act, var.act)

    indx.col.i <- (pos.col.ini:pos.col)
    indx.row.i <- (pos.row.ini:pos.row)

    burt.sym[indx.row.i, indx.col.i] <- K$data
    colnames(burt.sym)[indx.col.i] <- colnames(K$data)
    row.names(burt.sym)[indx.row.i] <- row.names(K$data)

    for (j in (i + 1):num.vars)
    {
      pos.col.ini <- cum.sym.var.length.2[j - 1] + 1
      pos.col <- cum.sym.var.length.2[j]
      K <- calc.k(var.act, sym.data[, pos.var[j]])
      K.t <- transpose.sym(K)

      burt.sym[indx.row.i, (pos.col.ini:pos.col)] <- K$data
      colnames(burt.sym)[(pos.col.ini:pos.col)] <- colnames(K$data)


      burt.sym[(cum.sym.var.length[j - 1] + 1):(cum.sym.var.length[j]), indx.col.i] <- K.t$data
    }

    pos.row.ini <- pos.row
    pos.col.ini <- cum.sym.var.length.2[i]
  }

  i <- num.vars
  var.act <- sym.data[, pos.var[i]]

  pos.row <- cum.sym.var.length[i]
  pos.col <- cum.sym.var.length.2[i]

  K <- calc.k(var.act, var.act)
  indx.col.i <- ((cum.sym.var.length.2[i - 1] + 1):pos.col)
  indx.row.i <- ((cum.sym.var.length[i - 1] + 1):pos.row)

  burt.sym[indx.row.i, indx.col.i] <- K$data
  colnames(burt.sym)[indx.col.i] <- colnames(K$data)
  row.names(burt.sym)[indx.row.i] <- row.names(K$data)

  return(data.frame.to.RSDA.inteval.table(burt.sym))
}

#' cfa.totals
#' @keywords internal
cfa.totals <- function(sym.data) {
  Total <- 0
  TotalMin <- 0
  TotalMax <- 0
  N <- sym.data$N
  M <- sym.data$M
  TotalFilas <- rep(0, N)
  TotalColumnas <- rep(0, M)
  TotalFilasMin <- rep(0, N)
  TotalColumnasMin <- rep(0, M)
  TotalFilasMax <- rep(0, N)
  TotalColumnasMax <- rep(0, M)
  A <- matrix(0, N, M) # To centers
  CMin <- matrix(0, N, M)
  CMax <- matrix(0, N, M)
  for (i in 1:N) {
    for (j in 1:M) {
      CMin[i, j] <- sym.data[, j]$data[i, 1]
      CMax[i, j] <- sym.data[, j]$data[i, 2]
      A[i, j] <- (sym.data[, j]$data[i, 1] + sym.data[, j]$data[i, 2]) / 2
    }
  }
  # Centers Totals
  for (i in 1:N) {
    TotalFilas[i] <- sum(A[i, ])
  }
  for (j in 1:M) {
    TotalColumnas[j] <- sum(A[, j])
  }
  Total <- sum(TotalFilas)
  # To minimus
  for (i in 1:N) {
    TotalFilasMin[i] <- sum(CMin[i, ])
  }
  for (j in 1:M) {
    TotalColumnasMin[j] <- sum(CMin[, j])
  }
  TotalMin <- sum(TotalFilasMin)
  # To maximuns
  for (i in 1:N) {
    TotalFilasMax[i] <- sum(CMax[i, ])
  }
  for (j in 1:M) {
    TotalColumnasMax[j] <- sum(CMax[, j])
  }
  TotalMax <- sum(TotalFilasMax)
  return(list(
    Total = Total, TotalMin = TotalMin, TotalMax = TotalMax, TotalRows = TotalFilas,
    TotalColumns = TotalColumnas, TotalRowsMin = TotalFilasMin, TotalColumnsMin = TotalColumnasMin,
    TotalRowsMax = TotalFilasMax, TotalColumnsMax = TotalColumnasMax
  ))
}

#' cfa.MatrixZ
#' @keywords internal
cfa.MatrixZ <- function(sym.data, TFilas, TColumnas) {
  Fil <- sym.data$N
  Col <- sym.data$M
  a <- min(Fil, Col)
  TablaDatos <- matrix(0, Fil, Col) # To centers
  A <- matrix(0, a, a) # To Z
  for (i in 1:Fil) {
    for (j in 1:Col) {
      TablaDatos[i, j] <- (sym.data[, j]$data[i, 1] + sym.data[, j]$data[i, 2]) / 2
    }
  }
  if (a == Col) {
    for (j in 1:a) {
      for (l in j:a) {
        suma <- 0
        for (i in 1:Fil) {
          suma <- suma + ((TablaDatos[i, j]) * (TablaDatos[i, l])) / (TFilas[i])
        }
        suma <- suma * (1 / sqrt(abs((TColumnas[j]) * (TColumnas[l]))))
        A[j, l] <- suma
        if (i != j) {
          A[l, j] <- suma
        }
      }
    }
  } else {
    for (j in 1:a) {
      for (l in j:a) {
        suma <- 0
        for (i in 1:Col) {
          suma <- suma + ((TablaDatos[j, i]) * (TablaDatos[l, i])) / (TColumnas[i])
        }
        suma <- suma * (1 / sqrt(abs((TFilas[j]) * (TFilas[l]))))
        A[j, l] <- suma
        if (i != j) {
          A[l, j] <- suma
        }
      }
    }
  }
  return(A)
}

#' cfa.CVPRealz
#' @keywords internal
cfa.CVPRealz <- function(sym.data, TFilas, TColumnas, TT, z) {
  ## z = eigenvector of matrix Z TT = Total
  NFil <- sym.data$N
  MCol <- sym.data$M
  aMin <- min(NFil, MCol)
  TablaDatos <- matrix(0, NFil, MCol) # To centers
  VPRealz <- matrix(0, aMin, aMin) # To Z
  if (MCol <= NFil) {
    for (i in 1:aMin) {
      for (j in 1:aMin) {
        VPRealz[i, j] <- abs(TT) * (sqrt(abs(TT)) * sqrt(TColumnas[i])) * z[
          i,
          j
        ]
      }
    }
  } else {
    for (i in 1:aMin) {
      for (j in 1:aMin) {
        VPRealz[i, j] <- abs(TT) * (sqrt(abs(TT)) * sqrt(TFilas[i])) * z[
          i,
          j
        ]
      }
    }
  }
  return(VPRealz)
}

#' cfa.Czz
#' @keywords internal
cfa.Czz <- function(sym.data, TFilas, TColumnas, VPRealz, d) {
  NFil <- sym.data$N
  MCol <- sym.data$M
  aMin <- min(NFil, MCol)
  aMax <- max(NFil, MCol)
  X <- matrix(0, NFil, MCol) # To centers
  zz <- matrix(0, NFil, MCol) # To z
  for (i in 1:NFil) {
    for (j in 1:MCol) {
      X[i, j] <- (sym.data[, j]$data[i, 1] + sym.data[, j]$data[i, 2]) / 2
    }
  }
  suma <- 0
  if (MCol <= NFil) {
    for (i in 1:aMin) {
      for (l in 1:aMax) {
        suma <- 0
        for (s in 1:aMin) {
          suma <- suma + (X[l, s] / TColumnas[s]) * VPRealz[s, i]
        }
        suma <- (1 / sqrt(d[i])) * suma
        zz[l, i] <- suma
      }
    }
  } else {
    for (i in 1:aMin) {
      for (l in 1:aMax) {
        suma <- 0
        for (s in 1:aMin) {
          suma <- suma + (X[s, l] / TFilas[s]) * VPRealz[s, i]
        }
        suma <- (1 / sqrt(d[i])) * suma
        zz[l, i] <- suma
      }
    }
  }
  return(zz)
}

#' cfa.minmax.new
#' @keywords internal
cfa.minmax.new <- function(sym.data, TFilas, TFilasMin, TFilasMax, TColumnas,
                           TColumnasMin, TColumnasMax, Total, VP, VPzz) {
  n <- sym.data$N
  m <- sym.data$M
  aMin <- min(n, m)
  X <- matrix(0, n, m)
  XMin <- matrix(0, n, m)
  XMax <- matrix(0, n, m)
  A <- matrix(0, n + m, aMin - 1)
  Min <- matrix(0, n + m, aMin - 1)
  Max <- matrix(0, n + m, aMin - 1)
  for (i in 1:n) {
    for (j in 1:m) {
      XMin[i, j] <- sym.data[, j]$data[
        i,
        1
      ]
      XMax[i, j] <- sym.data[, j]$data[
        i,
        2
      ]
      X[i, j] <- (sym.data[, j]$data[
        i,
        1
      ] + sym.data[, j]$data[
        i,
        2
      ]) / 2
    }
  }
  for (j in 1:m) {
    for (k in 1:n) {
      X[k, j] <- X[k, j] / Total
      XMin[k, j] <- XMin[k, j] / Total
      XMax[k, j] <- XMax[k, j] / Total
    }
  }
  if (m <= n) {
    for (alfa in 2:aMin) {
      for (i in 1:n) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (j in 1:m) {
          suma <- suma + (X[i, j] * VP[j, alfa]) / TColumnas[j]
          if (VP[j, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VP[j, alfa]) / TColumnas[j]
            SumaC <- SumaC + (XMin[i, j] * VP[j, alfa]) / TColumnas[j]
          }
          if (VP[j, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VP[j, alfa]) / TColumnas[j]
            SumaD <- SumaD + (XMax[i, j] * VP[j, alfa]) / TColumnas[j]
          }
        }
        suma <- suma / TFilas[i]
        SumaA <- SumaA / TFilas[i]
        SumaB <- SumaB / TFilas[i]
        SumaC <- SumaC / TFilas[i]
        SumaD <- SumaD / TFilas[i]
        A[i, (alfa - 1)] <- suma
        Min[i, (alfa - 1)] <- (SumaA + SumaB)
        Max[i, (alfa - 1)] <- (SumaC + SumaD)
      }
    }
    for (alfa in 2:aMin) {
      for (j in 1:m) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (i in 1:n) {
          suma <- suma + (X[i, j] * VPzz[i, alfa]) / TFilas[i]
          if (VPzz[i, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VPzz[i, alfa]) / TFilas[i]
            SumaC <- SumaC + (XMin[i, j] * VPzz[i, alfa]) / TFilas[i]
          }
          if (VPzz[i, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VPzz[i, alfa]) / TFilas[i]
            SumaD <- SumaD + (XMax[i, j] * VPzz[i, alfa]) / TFilas[i]
          }
        }
        suma <- suma / TColumnas[j]
        SumaA <- SumaA / TColumnas[j]
        SumaB <- SumaB / TColumnas[j]
        SumaC <- SumaC / TColumnas[j]
        SumaD <- SumaD / TColumnas[j]
        A[(n + j), (alfa - 1)] <- suma
        Min[(n + j), (alfa - 1)] <- (SumaA + SumaB)
        Max[(n + j), (alfa - 1)] <- (SumaC + SumaD)
      }
    }
  }
  else {
    for (alfa in 2:aMin) {
      for (j in 1:m) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (i in 1:n) {
          suma <- suma + (X[i, j] * VP[i, alfa]) / TFilas[i]
          if (VP[i, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VP[i, alfa]) / TFilas[i]
            SumaC <- SumaC + (XMin[i, j] * VP[i, alfa]) / TFilas[i]
          }
          if (VP[i, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VP[i, alfa]) / TFilas[i]
            SumaD <- SumaD + (XMax[i, j] * VP[i, alfa]) / TFilas[i]
          }
        }
        suma <- suma / TColumnas[j]
        SumaA <- SumaA / TColumnas[j]
        SumaB <- SumaB / TColumnas[j]
        SumaC <- SumaC / TColumnas[j]
        SumaD <- SumaD / TColumnas[j]
        A[j, (alfa - 1)] <- suma
        Min[j, (alfa - 1)] <- (SumaA + SumaB)
        Max[j, (alfa - 1)] <- (SumaC + SumaD)
      }
    }
    for (alfa in 2:aMin) {
      for (i in 1:n) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (j in 1:m) {
          suma <- suma + (X[i, j] * VPzz[j, alfa]) / TColumnas[j]
          if (VPzz[j, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VPzz[j, alfa]) / TColumnas[j]
            SumaC <- SumaC + (XMin[i, j] * VPzz[j, alfa]) / TColumnas[j]
          }
          if (VPzz[j, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VPzz[j, alfa]) / TColumnas[j]
            SumaD <- SumaD + (XMax[i, j] * VPzz[j, alfa]) / TColumnas[j]
          }
        }
        suma <- suma / TFilas[i]
        SumaA <- SumaA / TFilas[i]
        SumaB <- SumaB / TFilas[i]
        SumaC <- SumaC / TFilas[i]
        SumaD <- SumaD / TFilas[i]
        A[(m + i), (alfa - 1)] <- suma
        Min[(m + i), (alfa - 1)] <- (SumaA + SumaB)
        Max[(m + i), (alfa - 1)] <- (SumaC + SumaD)
      }
    }
  }
  return(list(Centers = A[1:n, ], Min = Min[1:n, ], Max = Max[1:n, ]))
}

#' cfa.minmax
#' @keywords internal
cfa.minmax <- function(sym.data, TFilas, TFilasMin, TFilasMax, TColumnas, TColumnasMin,
                       TColumnasMax, Total, VP, VPzz) {
  n <- sym.data$N
  m <- sym.data$M
  aMin <- min(n, m)
  X <- matrix(0, n, m) # To centers
  XMin <- matrix(0, n, m)
  XMax <- matrix(0, n, m)
  A <- matrix(0, n + m, aMin - 1)
  Min <- matrix(0, n + m, aMin - 1)
  Max <- matrix(0, n + m, aMin - 1)
  for (i in 1:n) {
    for (j in 1:m) {
      XMin[i, j] <- sym.data[, j]$data[i, 1]
      XMax[i, j] <- sym.data[, j]$data[i, 2]
      X[i, j] <- (sym.data[, j]$data[i, 1] + sym.data[, j]$data[i, 2]) / 2
    }
  }
  for (j in 1:m) {
    for (k in 1:n) {
      X[k, j] <- X[k, j] / Total
      XMin[k, j] <- XMin[k, j] / Total
      XMax[k, j] <- XMax[k, j] / Total
    }
  }
  if (m <= n) {
    for (alfa in 2:aMin) {
      for (i in 1:n) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (j in 1:m) {
          suma <- suma + (X[i, j] * VP[j, alfa]) / TColumnas[j]
          if (VP[j, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VP[j, alfa]) / TColumnas[j]
            SumaC <- SumaC + (XMin[i, j] * VP[j, alfa]) / TColumnas[j]
          }
          if (VP[j, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VP[j, alfa]) / TColumnas[j]
            SumaD <- SumaD + (XMax[i, j] * VP[j, alfa]) / TColumnas[j]
          }
        }
        suma <- suma / TFilas[i]
        SumaA <- SumaA / TFilas[i]
        SumaB <- SumaB / TFilas[i]
        SumaC <- SumaC / TFilas[i]
        SumaD <- SumaD / TFilas[i]
        A[i, (alfa - 1)] <- suma
        Min[i, (alfa - 1)] <- (SumaA + SumaB)
        Max[i, (alfa - 1)] <- (SumaC + SumaD)
      }
    }
    for (alfa in 2:aMin) {
      for (j in 1:m) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (i in 1:n) {
          suma <- suma + (X[i, j] * VPzz[i, alfa]) / TFilas[i]
          if (VPzz[i, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VPzz[i, alfa]) / TFilas[i]
            SumaC <- SumaC + (XMin[i, j] * VPzz[i, alfa]) / TFilas[i]
          }
          if (VPzz[i, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VPzz[i, alfa]) / TFilas[i]
            SumaD <- SumaD + (XMax[i, j] * VPzz[i, alfa]) / TFilas[i]
          }
        }
        suma <- suma / TColumnas[j]
        SumaA <- SumaA / TColumnas[j]
        SumaB <- SumaB / TColumnas[j]
        SumaC <- SumaC / TColumnas[j]
        SumaD <- SumaD / TColumnas[j]
        A[(n + j), (alfa - 1)] <- suma
        Min[(n + j), (alfa - 1)] <- (SumaA + SumaB)
        Max[(n + j), (alfa - 1)] <- (SumaC + SumaD)
      }
    }
  } else {
    for (alfa in 2:aMin) {
      for (j in 1:m) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (i in 1:n) {
          suma <- suma + (X[i, j] * VP[i, alfa]) / TFilas[i]
          if (VP[i, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VP[i, alfa]) / TFilas[i]
            SumaC <- SumaC + (XMin[i, j] * VP[i, alfa]) / TFilas[i]
          }
          if (VP[i, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VP[i, alfa]) / TFilas[i]
            SumaD <- SumaD + (XMax[i, j] * VP[i, alfa]) / TFilas[i]
          }
        }
        suma <- suma / TColumnas[j]
        SumaA <- SumaA / TColumnas[j]
        SumaB <- SumaB / TColumnas[j]
        SumaC <- SumaC / TColumnas[j]
        SumaD <- SumaD / TColumnas[j]
        A[j, (alfa - 1)] <- suma
        Min[j, (alfa - 1)] <- (SumaA + SumaB)
        Max[j, (alfa - 1)] <- (SumaC + SumaD)
      }
    }
    for (alfa in 2:aMin) {
      for (i in 1:n) {
        suma <- 0
        SumaA <- 0
        SumaB <- 0
        SumaC <- 0
        SumaD <- 0
        for (j in 1:m) {
          suma <- suma + (X[i, j] * VPzz[j, alfa]) / TColumnas[j]
          if (VPzz[j, alfa] < 0) {
            SumaA <- SumaA + (XMax[i, j] * VPzz[j, alfa]) / TColumnas[j]
            SumaC <- SumaC + (XMin[i, j] * VPzz[j, alfa]) / TColumnas[j]
          }
          if (VPzz[j, alfa] > 0) {
            SumaB <- SumaB + (XMin[i, j] * VPzz[j, alfa]) / TColumnas[j]
            SumaD <- SumaD + (XMax[i, j] * VPzz[j, alfa]) / TColumnas[j]
          }
        }
        suma <- suma / TFilas[i]
        SumaA <- SumaA / TFilas[i]
        SumaB <- SumaB / TFilas[i]
        SumaC <- SumaC / TFilas[i]
        SumaD <- SumaD / TFilas[i]
        A[(m + i), (alfa - 1)] <- suma
        Min[(m + i), (alfa - 1)] <- (SumaA + SumaB)
        Max[(m + i), (alfa - 1)] <- (SumaC + SumaD)
      }
    }
  }
  return(list(Centers = A, Min = Min, Max = Max))
}

#' calc.k
#' @keywords internal
calc.k <- function(var.sym.X, var.sym.Y) {
  data.X.max <- var.sym.X$data
  data.Y.max <- var.sym.Y$data
  data.X.min <- calc.matrix.min(data.X.max)
  data.Y.min <- calc.matrix.min(data.Y.max)

  K.min <- t(as.matrix(data.X.min)) %*% as.matrix(data.Y.min)
  K.max <- t(as.matrix(data.X.max)) %*% as.matrix(data.Y.max)

  N <- dim(K.min)
  K <- as.data.frame(matrix(rep(0, 2 * N[1] * N[2]), nrow = N[1]))
  indx.min <- seq(from = 1, by = 2, length.out = N[2])
  indx.max <- seq(from = 2, by = 2, length.out = N[2])
  K[, indx.min] <- K.min
  K[, indx.max] <- K.max

  col.names.K <- colnames(K.min)
  row.names.K <- row.names(K.min)

  row.names(K) <- row.names.K
  colnames(K)[indx.min] <- col.names.K
  colnames(K)[indx.max] <- paste0(col.names.K, ".1")

  return(data.frame.to.RSDA.inteval.table(K))
}

#' calc.matrix.min
#' @keywords internal
calc.matrix.min <- function(data.max) {
  dim.data.max <- dim(data.max)
  data.min <- data.max
  zeros.rep <- rep(0, dim.data.max[2])
  indx.mult <- which(apply(data.max, 1, sum) > 1)
  data.min[indx.mult, ] <- zeros.rep
  return(data.min)
}

#' data.frame.to.RSDA.inteval.table
#' @keywords internal
data.frame.to.RSDA.inteval.table <- function(data.df) {
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
    N = dim.sym[1],
    M = var.dist,
    sym.obj.names = sym.obj.names,
    sym.var.names = sym.var.names,
    sym.var.types = sym.var.types,
    sym.var.length = sym.var.length,
    sym.var.starts = sym.var.starts,
    meta = meta,
    data = data.df
  )

  class(data.sym) <- "sym.data.table"
  return(data.sym)
}

transpose.sym <- function(sym.data) {
  M <- sym.data$M
  N <- sym.data$N
  sym.obj.names <- sym.data$sym.obj.names
  sym.var.names <- sym.data$sym.var.names
  data <- sym.data$data

  data.sal <- as.data.frame(matrix(rep(0, 2 * M * N), nrow = M))

  seq.min <- seq(from = 1, by = 2, length.out = N)
  seq.max <- seq(from = 2, by = 2, length.out = N)

  for (i in 1:M)
  {
    indx.i <- (2 * i - 1):(2 * i)
    for (j in 1:N)
    {
      ind.j <- (2 * j - 1):(2 * j)
      data.sal[i, ind.j] <- data[j, indx.i]
    }
  }

  row.names(data.sal) <- sym.var.names
  colnames(data.sal)[seq.min] <- sym.obj.names
  colnames(data.sal)[seq.max] <- paste0(sym.obj.names, ".1")
  return(data.frame.to.RSDA.inteval.table(data.sal))
}
