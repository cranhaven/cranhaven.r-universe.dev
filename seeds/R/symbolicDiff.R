symbolicDiff <- function(odeObj) {

  results <- list(costate = character(0), jacobian = matrix())

  formatEqs <- function(modelEq) {
    return(gsub("(d[x,y])\\[*([0-9]*)\\]*\\s*=", "", modelEq))
  }

  trimSpace <- function(x) gsub("\\s", "", x)

  simMaExpr <- function(x) apply(X = x, MARGIN = c(1, 2), FUN = Deriv::Simplify)


  costate <- function(odeObj) {
    modelEq <- formatEqs(odeObj@origEq)
    if (grepl("^y([0-9]*$)", modelEq[1])) {
      format = "y"
    } else {
      format = "x"
    }
    n <- m <- length(modelEq)
    Jf = matrix(list(), nrow = m, ncol = n)
    diffStates = paste0(rep(x = format, length(modelEq)), 1:length(modelEq))
    for (i in 1:m) {
      for (j in 1:n) {
        Jf[[i, j]] = Deriv::Deriv(modelEq[i], diffStates[j])
      }
    }
    costDeriveState <- costFuncDeriv(odeObj, format)
    costateEq <- formatDerivMatrix(Jf, costDeriveState)

    coStart <- costateStart(odeObj, format)


    formatedCostate <- vectorFormat(costateEq)

    return(list(formatedCostate, Jf, coStart[[1]], coStart[[2]]))
  }

  costateStart <- function(odeObj, format) {
    n <- length(odeObj@origEq)
    diffStates = paste0(rep(x = format, length(odeObj@origEq)), 1:length(odeObj@origEq))
    m <- length(odeObj@measureFunction)
    Jh <- matrix(list(), nrow = n, ncol = m)
    meaFunc <- unlist(strsplit(odeObj@measureFunction, split = "="))[seq(from = 2, to = 2 * m, by = 2)]
    for (i in 1:n) {
      for (j in 1:m) {
        Jh[[i, j]] = Deriv::Deriv(meaFunc[j], diffStates[i])
      }
    }
    # format for dynamic elastic net
    # create predErrorTerm


    Jh = matrix(unlist(Jh), nrow = n)
    multi <- strMatrixMulti(Jh, t(Jh))
    multi = vectorFormat(simMaExpr(multi))

    return(list(multi, Jh))

  }

  costFuncDeriv <- function(odeObj, format) {
    n <- length(odeObj@origEq)
    Jl <- rep("", n)
    diffStates = paste0(rep(x = format, length(odeObj@origEq)), 1:length(odeObj@origEq))
    if (!odeObj@dynamicElasticNet) {
      for (j in 1:n) {
        Jl[j] = Deriv::Deriv(odeObj@costFunction, diffStates[j])
      }
      ret <- Jl
    }
    else {
      m <- length(odeObj@measureFunction)
      Jh <- matrix(list(), nrow = n, ncol = m)
      meaFunc <- unlist(strsplit(odeObj@measureFunction, split = "="))[seq(from = 2, to = 2 * m, by = 2)]
      for (i in 1:n) {
        for (j in 1:m) {
          Jh[[i, j]] = Deriv::Deriv(meaFunc[j], diffStates[i])
        }
      }
      # format for dynamic elastic net
      # create predErrorTerm
      Jh = matrix(unlist(Jh), nrow = n)
      h <- matrix(paste(paste("q", 1:ncol(Jh), sep = ""), "*", paste("(y", 1:ncol(Jh), sep = ""), "-", paste("yhat", 1:ncol(Jh), ")", sep = ""), sep = ""), nrow = ncol(Jh))
      multi <- as.vector(strMatrixMulti(Jh, h))
      ret <- paste("2*(", multi, ")")
      ret = unlist(lapply(X = ret, FUN = Deriv::Simplify))

    }
    return(ret)
  }

  formatDerivMatrix <- function(derivMatrix, costDeriveState) {
    n <- ncol(derivMatrix)
    p <- character(length = n)
    lagrangePara <- paste0(rep(x = "-", n), rep(x = "p", n), 1:n, rep(x = "*", n))

    for (i in 1:n) {
      p[i] = paste0("dp", as.character(i), " = optW[", as.character(i), "]*(", paste(paste0(lagrangePara, rep(x = "(", n), unlist(derivMatrix[, i])), rep(x = ")", n), collapse = ""))
    }
    return(paste0(paste(p, costDeriveState, sep = " - "), ")"))
  }

  vectorFormat <- function(charVec) {
    formatedCharVec <- gsub(pattern = "(\\b[x,y,p,q])([0-9]*)(\\s)*", replacement = "\\1[\\2]", x = charVec)
    formatedCharVec = gsub(pattern = "\\[\\]", replacement = "", x = formatedCharVec)
    formatedCharVec = gsub(pattern = "(d+[p,x,y])\\[([0-9]*)\\]", replacement = "\\1\\2", x = formatedCharVec)
    if (sum(as.integer(grepl(pattern = "yhat[0-9]*", charVec))) > 0) {
      formatedCharVec <- gsub(pattern = "(yhat*)([0-9]*)", x = formatedCharVec, replacement = "\\1[\\2]")
    }
    return(formatedCharVec)
  }

  createHamilton <- function(odeEq) {
    n <- length(odeEq@origEq)
    EQ <- formatEqs(trimSpace(odeEq@origEq))
    addLambda <- paste(rep("p[", n), 1:n, rep("]*(", n), EQ, rep(x = ")", n), sep = "")
    concEq <- paste(addLambda, collapse = "+")
    if (odeEq@dynamicElasticNet) {
      m <- length(odeEq@measureFunction)
      costDynElaNet <- paste("(y", 1:m, "- yhat", 1:m, ")", sep = "")
      costDynElaNet = paste(costDynElaNet, collapse = "+")

      costHiddenInput <- paste("sum(abs(w[,", 1:n, "])+sum(w[,", 1:n, "]^2)", sep = "")
      costHiddenInput = paste(costHiddenInput, collapse = "+")

      combinedCost = paste(costDynElaNet, costHiddenInput, sep = "+")

      Hamilton <- vectorFormat(paste(concEq, combinedCost, sep = "+"))

    }
    else {
      Hamilton <- vectorFormat(paste(concEq, odeEq@costFunction, sep = "-"))
    }
    return(Hamilton)
  }

  strMatrixMulti <- function(M1, M2) {
    if (ncol(M1) != nrow(M2)) {
      stop('multiplication not possible. Dimensions dont match')
    }
    else {
      colM1 <- ncol(M1)
      colM2 <- ncol(M2)
      rowM1 <- nrow(M1)
      rowM2 <- nrow(M2)

      ret <- matrix(rep(0, rowM1 * colM2), ncol = colM2)
      m <- rowM1
      n <- colM2
      for (r in 1:m) {
        for (c in 1:n) {
          multi <- paste(M1[r,], M2[, c], sep = "*")
          ret[r, c] = paste(multi, collapse = "+")
        }
      }
    }
    return(ret)
  }

  if (identical(odeObj@measureFunction, character(0)) == FALSE) {
    temp <- costate(odeObj)
    results$costate <- temp[[1]]
    results$jacobian <- temp[[2]]
    results$JhT <- temp[[4]]
    results$origEq <- vectorFormat(odeObj@origEq)
    results$Hamilton <- createHamilton(odeObj)
  } else {
    results$costate <- odeObj@costateEq
    results$jacobian <- odeObj@jacobian
    results$JhT <- odeObj@JhT
    results$origEq <- vectorFormat(odeObj@origEq)
    results$Hamilton <- odeObj@hamiltonian
  }


  return(results)

}
