#' @importFrom earth earth
#' @importFrom stringr str_replace_all
#' @importFrom Ryacas0 yacas
#' @importFrom Ryacas0 Sym
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom stats cor
#' @importFrom AUC auc
#' @importFrom AUC roc

cmaRs.fit <- function(x = stop("no 'x' argument"),
                      y = stop("no 'y' argument"),
                      data = parent.frame(),
                      classification = classification,
                      threshold.class = 0.5,
                      degree = 1,
                      nk = 21,
                      Auto.linpreds = FALSE)
                      # this function prepares the data for cmars modeling,
                      # takes the BFs from MARS,
                      # constructs a CMARS models and
# then calculates the performance measures
{
  empt.vector <- c()
  for (i in 1:dim(data)[2])
  {
    empt.vector <- c(empt.vector, (all(data[, i] == y)))
  }
  response.name <- colnames(data)[empt.vector]
  assign(response.name, y)
  if (is.vector(x)) {
    empt.vector.x <- c()
    for (i in 1:dim(data)[2])
    {
      empt.vector.x <- c(empt.vector.x, (all(data[, i] == x)))
    }
    pred.names <- colnames(data)[empt.vector.x]
    assign(pred.names, x)
  } else {
    pred.names <- colnames(x)
    pred.names <- paste(pred.names, collapse = "+")
  }

  formula <- eval(stats::as.formula(paste(response.name,
    "~", pred.names,
    sep = ""
  )))
  p <- dim(x)[2] # number of independent variables
  n <- length(y) # number of observations

  for (i in 1:p)
  {
    istr <- as.character(i)
    d <- paste("x", istr, " = ", "x[", ",", istr, "]", sep = "")
    eval(parse(text = d))
  }

  for (i in 1:p)
  {
    assign(colnames(x)[i], x[[i]])
  }

  # Step 1: Forward MARS Algorithm
  # run MARS code

  if (classification == TRUE) {
    # constructs only the backward step of MARS model
    model_mars <- earth::earth(formula,
      degree = degree,
      nk = nk, glm = list(family = stats::binomial, maxit = 1000),
      pmethod = "forward",
      trace = 4, Auto.linpreds = Auto.linpreds, data
    )
  } else {
    # constructs only the backward step of MARS model
    model_mars <- earth::earth(formula,
      degree = degree, nk = nk,
      pmethod = "forward", trace = 4, Auto.linpreds = Auto.linpreds, data
    )
  }
  # return the model with the "intercept" as the first row
  bfs_forward_names <- rownames(model_mars$dirs)
  bfs_forward_names.orig <- bfs_forward_names
  v1 <- c()
  for (i in 1:p)
  {
    istr <- as.character(i)
    d <- paste("x", istr, sep = "")
    v1 <- c(v1, d)
  }
  k <- cbind(v1, colnames(x))

  for (i in 1:p)
  {
    istr <- as.character(i)
    bfs_forward_names <- stringr::str_replace_all(
      bfs_forward_names,
      as.character(colnames(x)[i]), as.character(paste("x", istr, sep = ""))
    )
  }

  # Step 2 : CMARS Algorithm
  # run cmaRs code
  sqrtz <- seq(0.1, 100, by = 0.1) # initialize sqrt of z


  # prepatation for taking derivatives
  # sort all independent variables
  for (i in 1:p)
  {
    istr <- as.character(i)
    d <- paste("<-sort", "(", "x", istr, ")", sep = "")
    e <- paste("xsort", istr, d, sep = "")
    eval(parse(text = e))
  }
  # derive variables used in D matrix
  xfirst_data <- c()
  for (i in 1:p)
  { # add new observations to beginning of sorted data
    istr <- as.character(i)
    f <- paste("xfirst", istr, "<- xsort", istr, "[1] - 0.1", sep = "")
    anew <- eval(parse(text = f))
    g <- paste("xfirst", istr, "<-", "c(", "xfirst", istr, ",",
      "xsort", istr, ")",
      sep = ""
    )
    bnew <- eval(parse(text = g))
    xfirst_data <- cbind(xfirst_data, bnew) # that will be used in DMS
  }
  colnames_vector <- c()
  for (i in 1:p)
  {
    istr <- as.character(i)
    colnames_vector <- c(
      colnames_vector,
      (text <- paste("xfirst", istr, sep = ""))
    )
  }
  colnames(xfirst_data) <- colnames_vector
  xfirst_data <- data.frame(xfirst_data, nrow <- n, ncol <- p)
  for (i in 1:p)
  { # add new observations to end of sorted data
    istr <- as.character(i)
    h <- paste("xplus", istr, "<- xfirst", istr, "[n+1] + 0.1", sep = "")
    anew <- eval(parse(text = h))
    k <- paste("xplus", istr, "<-", "c(", "xfirst", istr, ",",
      "xplus", istr, ")",
      sep = ""
    )
    bnew <- eval(parse(text = k))
  }
  # assign vector of BFs to variable bf.vector.
  bf.vector <- bfs_forward_names
  # replace "--" signs with "+"
  bf.vector1 <- stringr::str_replace_all(bf.vector, "--", "+")
  # assign vector of BFs to variable bf.vector.
  bf.vector <- bf.vector1
  bf.vector1 <- bf.vector
  # delete "h" in BFs.
  bf.vector1 <- stringr::str_replace_all(bf.vector, "h\\(", "\\(")
  bf.vector <- bf.vector1

  # delete reduntant information (Intercept) from vector of BF
  bf.vector <- bf.vector[2:length(bf.vector)]
  # replace "x" with "xfirst"
  k <- stringr::str_replace_all(bf.vector, "x", "xfirst")
  bf.vector <- k
  numBF <- length(bf.vector)
  max_int <- c()
  for (i in 1:numBF)
  { # determine the highest degree of interaction realized
    interaction_count <- 1
    for (j in 1:nchar(bf.vector[i]))
    {
      if (substr(bf.vector[i], j, j) == "*") {
        interaction_count <- interaction_count + 1
      }
    }
    max_int <- c(max_int, interaction_count)
  }
  maxint <- max(max_int)
  r1 <- c()
  r1 <- c()
  for (i in 1:numBF)
  { # indicate whether the related BF is an additive "m"
    # or interaction "i" BF and store the information into vector "int"
    for (j in 1:nchar(bf.vector[i])) {
      if (substr(bf.vector[i], j, j) == "*") {
        r <- "i"
        break
      } else {
        r <- "m"
      }
    }
    r1 <- c(r1, r)
  }
  int <- t(r1)
  L <- 0
  BasisFunctions <- c()
  bf.cmars <- c()
  # this part deletes the parantheses
  for (i in 1:numBF)
  {
    son <- stringr::str_replace_all(bf.vector, "\\)", "")
    son1 <- stringr::str_replace_all(son, "\\(", "")
    son2 <- stringr::str_replace_all(son1, "\\(", "")
    c <- son2
    c.new <- c
    c.new1 <- stringr::str_replace_all(c.new, "first", "")
    c.new2 <- paste("pmax(0,", c.new1, ")", sep = "")
    c.new3 <- stringr::str_replace_all(c.new2, "\\*", "\\)*pmax(0,")
    # obtain derivative for each BF
    r <- int[i]
    if (r == "m") {
      # write the BF in general format
      # for main effects such as max(0,x2-3)
      b1 <- c[i]
      f <- paste("pmax(0,", b1, ")", sep = "")
      ff <- f
    }
    if (r == "i") {
      nbf <- 1
      for (j in 1:nchar(c[i]))
      { # obtain number of interactions in ith BF
        if (substr(bf.vector[i], j, j) == "*") {
          nbf <- nbf + 1
        }
      }
      ind1 <- c()
      for (j in 1:nchar(c[i]))
      {
        if (substr(c[i], j, j) == "*") {
          ind1 <- c(ind1, j)
        }
      }
      ind1 <- c(ind1, nchar(c[i]))
      b1 <- substr(c[i], 1, ind1[1] - 1)
      f <- paste("pmax(0,", b1, ")", sep = "")
      b1 <- paste("(", b1, ")", sep = "")
      ff <- f
      for (j in 1:(length(ind1) - 1))
      {
        bb <- substr(c[i], (ind1[j] + 1), (ind1[j + 1] - 1))
        if (j == (length(ind1) - 1)) {
          el_last_elm <- substr(c[i], nchar(c[i]), nchar(c[i]))
          bb <- paste(bb, el_last_elm, sep = "")
        }
        for (k in 1:(nbf - 1)) {
          # write the BF in general format for interaction
          # such as max(0,x2-3)*max(0,8-x3)
          bb <- paste("(", bb, ")")
        }
        b1 <- paste(b1, "*", bb, sep = "")
        f <- paste(f, "*pmax(0,", bb, ")", sep = "")
        ff <- paste(ff, "*pmax(0,", bb, ")", sep = "")
      }
    }
    bf.cmars <- c.new3
    # construction of BF matrix as a
    # symbol to take symbolic derivative
    Ryacas0::yacas(Ryacas0::Sym(b1))
    # built-in function yacas
    # uploaded for symbolic derivatives
    BF <- Ryacas0::yacas("Simplify(%)")
    # reduce the expression to a simples form
    for (j in 1:numBF)
    {
      istr <- as.character(j)
      c <- paste("BF", istr, " <- bf.vector[j]", sep = "")
      eval(parse(text = c))
    }

    for (j in 1:(length(bf.vector)))
    {
      cf <- as.character(j)
      c <- paste("BFMS", cf, " <- ", f, sep = "")
      eval(parse(text = c))
    }
    Ryacas0::yacas(Ryacas0::Sym(f))
    BFMS <- Ryacas0::yacas("Simplify(%)")
    # BFMS: symbolic BF in terms of maximum function
    gg <- stringr::str_replace_all(ff, "first", "")
    BasisFunctions <- cbind(BasisFunctions, eval(parse(text = gg)))
    # taking derivatives
    # DM: derivative matrix of a BF
    DM <- matrix(0, nrow <- p, ncol <- p)
    # VARM: variable names whose derivative exists in DM
    VARM <- matrix(1, nrow <- p, ncol <- p)
    elm.dms <- Ryacas0::Sym(0)
    DMS <- matrix(elm.dms, nrow <- p, ncol <- p)
    # symbolic derivative matrix (DM)
    elm.varms <- Ryacas0::Sym(1)
    VARMS <- matrix(elm.varms, nrow <- p, ncol <- p) # symbolic version of VARM
    for (j in 1:p)
    {
      if (maxint == 2) {
        DMS <- (cmaRs.derivative_one(BF, DMS, j, b1))$DMS
        DMS <- cmaRs.identical_function(DMS, j, j, 1, xfirst_data, n)
        DMS <- cmaRs.identical_function(DMS, j, j, -1, xfirst_data, n)
      } else {
        DMS <- (cmaRs.derivative_one(BF, DMS, j, b1))$DMS
        numeric1 <- eval(parse(text = DMS[j, j]))
      }
      if (DMS[j, j] != "0") {
        VARMS[j, j] <- paste("xplus", as.character(j), sep = "")
      }
      # two-interaction case
      if (j == p) break
      if (maxint == 2) {
        for (k in (j + 1):p)
        {
          DMS <- (cmaRs.derivative_more_than_one(BF, DMS, j, k))$DMS
          DMS <- cmaRs.identical_function(DMS, j, k, 1, xfirst_data, n)
          DMS <- cmaRs.identical_function(DMS, j, k, -1, xfirst_data, n)
        }
      } else {
        # more than two-interaction case
        if (j == p) break
        for (k in (j + 1):p)
        {
          DMS <- (cmaRs.derivative_more_than_one(BF, DMS, j, k))$DMS
        }
      }
    }
    DSSUM <- matrix(0, nrow <- n + 1, ncol <- 1)
    CC <- DMS
    for (j in 1:p)
    {
      for (k in j:p)
      {
        if (!(identical(eval(parse(text = DMS[j, k])), 0))) {
          DMS <- cmaRs.identical_function(DMS, j, k, 1, xfirst_data, n)
          DMS <- cmaRs.identical_function(DMS, j, k, -1, xfirst_data, n)
          DMS <- cmaRs.identical_function(DMS, j, k, 0, xfirst_data, n)
          DS <- DMS[j, k]
          D <- eval(parse(text = DS))
          BFM <- eval(parse(text = ff))
          D_orig <- D
          for (l in 1:(n + 1))
          {
            if (BFM[l] == 0) {
              D[l] <- 0
            } else {
              D[l] <- D_orig[l]
            }
          }
          DSSUM <- DSSUM + (D^(2.0))
        }
      }
    }
    # construction of L matrix
    prod <- matrix(1, nrow <- n + 1, ncol <- 1)
    for (j in 1:p)
    {
      for (k in j:p)
      {
        if (VARMS[j, k] != "1") {
          VS <- VARMS[j, k]
          V <- eval(parse(text = VS))
          DV <- diff(V) # take the difference [(x2-x1),(x3-x2),...,(x_N+1-x_N)]
          prod <- prod * DV
        }
      }
    }
    l <- sqrt(sum(DSSUM * prod))
    L <- c(L, l)
  }
  L <- diag(L)
  # problem preparation of conic quadratic programming problems' constraints
  BasisFunctions <- cbind(matrix(1, nrow <- n, ncol <- 1), BasisFunctions)
  M1 <- cbind(matrix(0, nrow <- n, ncol <- 1), BasisFunctions)
  M2 <- Matrix(diag(n) > 0, sparse = TRUE)
  M3 <- c()
  if (numBF == 1) {
    M3 <- -1
  } else {
    for (i in 1:numBF)
    {
      M3 <- c(M3, -1)
    }
    M3 <- diag(M3)
  }
  M4 <- matrix(0, nrow <- n, ncol <- numBF)
  M5 <- cbind(matrix(0, nrow <- numBF, ncol <- 2), L[
    2:(numBF + 1),
    2:(numBF + 1)
  ], matrix(0, nrow <- numBF, ncol <- n))
  V1 <- matrix(0, nrow <- (n + numBF), ncol <- 1)
  A <- cbind(M1, as.matrix(M2), M4)
  B <- cbind(M5, M3)
  C1 <- rbind(A, B)
  D1 <- cbind(C1, V1)
  alpha <- c()
  T1 <- c()
  z <- c()
  t1 <- c()
  kokm <- t(sqrtz)
  if (is.vector(sqrtz) == TRUE) # vector
    {
      s <- length(sqrtz)
    } else {
    s <- 1
  }
  auc.value <- c()
  # call Rmosek to obtain parameter estimates
  for (i in 1:s)
  {
    optimization_all <- cmaRs.mosek_optimization(
      i, numBF,
      sqrtz, T1, l, alpha, z, L, t1, BasisFunctions, n, D1, y
    )
    Theta <- optimization_all$Theta
    m <- optimization_all$m
    t1 <- optimization_all$t1
    z <- optimization_all$z
    BasisFunctions <- optimization_all$BasisFunctions
    if (classification == TRUE) {
      auc.value <- c(auc.value, AUC::auc(AUC::roc(
        as.numeric(optimization_all$m),
        factor(eval(parse(text = response.name)),
          levels = c("0", "1"), labels = c("0", "1")
        )
      )))
    }
  }

  if (classification == FALSE) {
    all_z <- optimization_all$z
    all_t1 <- optimization_all$t1
    min.RSS <- min(all_z)
    index.RSS <- which(all_z == min.RSS)
    sqrtz.final <- sqrtz[index.RSS]
    final_opt <- cmaRs.mosek_optimization(
      1, numBF, sqrtz.final,
      T1, l, alpha, sqrtz.final, L, t1, BasisFunctions, n, D1, y
    )
  } else {
    all_z <- optimization_all$z
    all_t1 <- optimization_all$t1
    max.AUC <- max(auc.value)
    index.AUC <- which(auc.value == max.AUC)
    sqrtz.final <- sqrtz[min(index.AUC)]
    final_opt <- cmaRs.mosek_optimization(
      1, numBF, sqrtz.final,
      T1, l, alpha, sqrtz.final, L, t1, BasisFunctions, n, D1, y
    )
  }

  # output
  R2 <- NULL
  r <- NULL
  RSS <- NULL
  MCR <- NULL
  AUC <- NULL
  PCC <- NULL
  precision <- NULL
  recall <- NULL
  specificity <- NULL
  SSR <- NULL
  SST <- NULL

  yhat <- final_opt$m
  final_Theta <- final_opt$Theta
  final_RSS <- final_opt$z[2]
  BasisFunctions <- final_opt$BasisFunctions

  if (classification == TRUE) {
    threshold <- threshold.class
    fitted.binary <- c()
    for (i in 1:n)
    {
      if (yhat[i] < threshold) {
        fitted.binary <- c(fitted.binary, 0)
      } else {
        fitted.binary <- c(fitted.binary, 1)
      }
    }
    # notation
    # True Positive (TP) <- a
    # False Negative (FN) <- b
    # False Positive (FP) <- c
    # True Negative (TN) <- d
    # confusion matrix
    a.mtrx <- 0
    b.mtrx <- 0
    c.mtrx <- 0
    d.mtrx <- 0
    for (i in 1:n)
    {
      if ((y[i] == 0) && (y[i] == fitted.binary[i])) {
        a.mtrx <- a.mtrx + 1
      }
      if ((y[i] == 0) && (y[i] != fitted.binary[i])) {
        b.mtrx <- b.mtrx + 1
      }
      if ((y[i] == 1) && (y[i] == fitted.binary[i])) {
        d.mtrx <- d.mtrx + 1
      }
      if ((y[i] == 1) && (y[i] != fitted.binary[i])) {
        c.mtrx <- c.mtrx + 1
      }
    }
    N <- a.mtrx + b.mtrx + c.mtrx + d.mtrx
    conf.mtrx <- matrix(0, nrow <- 2, ncol <- 2)
    conf.mtrx[1, 1] <- a.mtrx
    conf.mtrx[1, 2] <- b.mtrx
    conf.mtrx[2, 1] <- c.mtrx
    conf.mtrx[2, 2] <- d.mtrx
    AUC <- max.AUC
    # misclassification error rate
    MCR <- (b.mtrx + c.mtrx) / N
    # percentage of correct classified
    PCC <- 1 - MCR
    # precision
    precision <- a.mtrx / (a.mtrx + c.mtrx)
    # recall
    recall <- a.mtrx / (a.mtrx + b.mtrx)
    # specificity
    specificity <- d.mtrx / (d.mtrx + c.mtrx)
  } else {
    # performance measures for prediction
    avg.y <- mean(y)
    SSR <- sum((yhat - avg.y)^2.0)
    SST <- sum((y - avg.y)^2.0)
    R2 <- SSR / SST
    num <- (1 - R2) * (n - 1)
    den <- (n - numBF - 1)
    AdjR2 <- 1 - (num / den)
    r <- stats::cor(y, yhat)
  }

  bfs_forward_names <- bfs_forward_names.orig[2:length(bfs_forward_names.orig)]
  # assign vector of BFs to variable bf.vector.
  bf.vector <- bfs_forward_names
  # replace "--" signs with "+"
  bf.vector1 <- stringr::str_replace_all(bf.vector, "--", "+")
  # assign vector of BFs to variable bf.vector.
  bf.vector <- bf.vector1
  bf.vector1 <- bf.vector
  # delete "h" in BFs.
  bf.vector1 <- stringr::str_replace_all(bf.vector, "h\\(", "\\(")
  bf.vector <- bf.vector1
  bf.vector <- stringr::str_replace_all(bf.vector, "\\(", "\\pmax(0,")
  bf.vector <- stringr::str_replace_all(bf.vector, "\\)\\)", "\\)")
  bf.vector <- stringr::str_replace_all(bf.vector, "0,\\(", "0,")
  bf.cmars <- bf.vector


  coefficients <- final_Theta
  Theta.original <- coefficients
  Theta.cmars <- matrix(c(round(coefficients, digits = 4)), ncol <- 1)
  out1 <- c()
  for (i in 1:(numBF + 1))
  {
    if (sign(Theta.cmars[i]) == +1) {
      out1 <- c(out1, paste("+", Theta.cmars[i], sep = ""))
    } else {
      out1 <- c(out1, Theta.cmars[i])
    }
  }

  # returned fields match cmaRs's fields
  retval <- list(
    coefficients = final_Theta,
    formula = formula,
    fitted.values = as.vector(yhat),
    y = y,
    final.sqrtz = sqrtz.final,
    predictors = colnames(x),
    residuals = y - yhat,
    number.of.BF = numBF,
    bf.cmars = bf.cmars,
    L = L,
    DMS = DMS,
    VARMS = VARMS,
    response.name = response.name,
    R2 = R2,
    r = r,
    RSS = SST - SSR,
    MCR = MCR,
    PCC = PCC,
    AUC = AUC,
    precision = precision,
    recall = recall,
    specificity = specificity,
    classification = classification
  )

  class(retval) <- "cmaRs"
  retval
  return(retval)
}
