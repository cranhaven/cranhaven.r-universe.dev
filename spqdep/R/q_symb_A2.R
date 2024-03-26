q_symb_A2 <- function(data, ms, symb) {
  if (inherits(data, "rsplit")) {
      fxdf <- rsample::analysis(data) # Data Frame
      fx <- as.factor(fxdf[, 1])
  } else fx <- data
  if (!is.factor(fx)) {
    fx <- as.factor(fx)
    warning("variable is not factor")
  }

  R <- nrow(ms)  # R: number of symbolized locations
  m <- ncol(ms)  # m: size of m-surrounding
  N <- length(fx)  # N: number of observations
  k <- nlevels(fx) # k: number of classes
  pk <- summary(fx) / length(fx)
  n <- nrow(symb$p_symb)  # n: number of standard permutation symbols
  # Collapse the symbols to characters to then use as factors

  PSymb <- paste0(symb$p_symb[, 1])
  for (i in 2:m) {
    PSymb <- paste(PSymb, symb$p_symb[, i])
  }
  PSymb <- as.factor(PSymb)

  # Standard symbols in equivalents by combination-totals
  p_symbi <- matrix(rep(0, n * k), ncol = k)
  for (i in 1:n) {
    for (j in 1:k) {
      p_symbi[i, j] = sum(symb$p_symb[i, ] == j)
    }
  }
  # Collapse the symbols to characters to then use as factors
  PSymbi <- paste0(p_symbi[, 1])
  for (i in 2:k) {
    PSymbi <- paste(PSymbi, p_symbi[, i])
  }
  PSymbi <- as.factor(PSymbi)
  nc_symb <- nlevels(PSymbi) # nc_symb: number of reduced combination symbols
  CSymb <- as.factor(levels(PSymbi))
  # Evaluate the probabilities of each symbol under the null, standard
  # permutation symbols.

  qp_symb <- rep(0, n)
  for (i in 1:n) {
    qp_symb[i] <- 1
    for (j in 1:m) {
      qp_symb[i] <- qp_symb[i] * pk[symb$p_symb[i, j]]  # frequency of symbols, is the joint probability of classes in symbols
    }
  }

  # Expected frequency of standard permutation symbols under the null
  sfp_0 <- R * qp_symb  # The probability of the symbol under the null times the number of symbolized locations
  # Symbolize locations
  Zp <- matrix(as.numeric(fx[ms]), ncol = m)
  Zpf <- as.character(Zp[, 1])
  for (i in 2:m) {
    Zpf <- paste(Zpf, as.character(Zp[, i]))
  }
  Zpf <- as.factor(Zpf)
  efp_symb <- summary(Zpf)
  nZpf <- length(efp_symb)

  # Check levels in PSymb missing in Zpf
  if (nZpf < n) {
    kk <- rep(0,n)
    kk[(levels(PSymb) %in% levels(Zpf))] <- efp_symb
    names(kk) <- levels(PSymb)
    efp_symb <- kk
    # indxmislevZpf <- !(levels(PSymb) %in% levels(Zpf))
    # mislevZpf <- levels(PSymb)[indxmislevZpf]
    # efp_symb <- c(efp_symb, rep(0, length(mislevZpf)))
    # names(efp_symb)[(nZpf+1):(nZpf+length(mislevZpf))] <- mislevZpf
  }

  # Evaluate the probabilities of each symbol under the null, combination
  # symbols.
  qc_symb <- rep(0, nc_symb)
  for (i in 1:nc_symb) {
    qc_symb[i] = sum((PSymbi == CSymb[i]) * qp_symb)  #frequency of combination symbols
  }

  names(qc_symb) <- CSymb
  # Expected frequency of standard permutation symbols under the null
  sfc_0 <- R * qc_symb  #The probability of the symbol under the null times the number of symbolized locations
  # Symbolize locations
  Zc <- matrix(rep(0, R * k), ncol = k)
  for (i in 1:R) {
    for (j in 1:k) {
      Zc[i, j] <- sum(Zp[i, ] == j)
    }
  }

  Zcf <- as.character(Zc[, 1])
  for (i in 2:k) {
    Zcf <- paste(Zcf, as.character(Zc[, i]))
  }
  Zcf <- as.factor(Zcf)
  efc_symb <- summary(Zcf)
  nZcf <- length(efc_symb)
  # Check levels in CSymb missing in Zcf

  if (nZcf < nc_symb) {
    kk <- rep(0,nc_symb)
    kk[(levels(CSymb) %in% levels(Zcf))] <- efc_symb
    names(kk) <- levels(CSymb)
    efc_symb <- kk
    # indxmislevZcf <- !(levels(CSymb) %in% levels(Zcf))
    # mislevZcf <- levels(CSymb)[indxmislevZcf]
    # efc_symb <- c(efc_symb, rep(0, length(mislevZcf)))
    # names(efc_symb)[(nZcf+1):(nZcf+length(mislevZcf))] <- mislevZcf
  }
  # Calculate statistics With standard permutations symbols
  lnp <- log(efp_symb/R)
  lnp[lnp==-Inf] <- 0
  names(lnp) <- PSymb
  hmp <- -sum((efp_symb/R) * lnp)  #Empirical
  hmp_0 <- sum((efp_symb/R) * log(qp_symb))  #Expected under the null
  qp <- -2 * R * sum(hmp + hmp_0)  #Likelihood ratio statistic for permutations symbols
  # With combinations-totals symbols
  lnc <- rep(0, nc_symb)
  for (i in 1:nc_symb) {
    if (efc_symb[i] != 0) {
      lnc[i] <- log(efc_symb[i]/R)
    }
  }
  names(lnc) <- CSymb

  hmc <- -sum((efc_symb/R) * lnc)  #Empirical
  hmc_0 <- sum((efc_symb/R) * log(qc_symb))  #Expected under the null
  qc <- -2 * R * sum(hmc + hmc_0)  # Likelihood ratio statistic for combinations-totals symbols

  results <- list(qp, qc, efp_symb, efc_symb,
                  qp_symb, qc_symb,
                  PSymb, CSymb)
  names(results)[1] <- "qp"
  names(results)[2] <- "qc"
  names(results)[3] <- "efp_symb"
  names(results)[4] <- "efc_symb"
  names(results)[5] <- "qp_symb"
  names(results)[6] <- "qc_symb"
  names(results)[7] <- "PSymb"
  names(results)[8] <- "CSymb"
  return(results)
}
