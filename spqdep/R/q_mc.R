q_mc <- function(fx, x, m, nsim = 999,
                    seedinit = 123,
                    distance = "Euclidean") {
  Y <- fx
  if (length(Y) != dim(x)[1])
    stop("The length and Y does not match the dimension of the coordinates")
  k <- nlevels(Y)
  N <- length(Y)
  mdtfull <- st_distance(st_as_sf(x),
                             which = distance)
  # full distance matrix
  ms <- mdtms <- matrix(0, nrow = nrow(mdtfull),
                        ncol = m)
  ms[, 1] <- 1:N
  rownames(mdtms) <- ms[, 1]
  colnames(mdtms) <- NULL
  for (i in 1:N) {
    mdti <- mdtfull[i, ]
    mdtms[i, 1] <- mdti[i]
    # maximum distance ith row
    max_dt_mdti <- mdti[which.max(mdti)]
    mdti[i] <- mdti[i] + max_dt_mdti
    # distance with the same point is always zero...
    for (j in 2:m) {
      indx_mdti <- which.min(mdti)
      ms[i, j] <- indx_mdti
      mdtms[i, j] <- mdti[indx_mdti]
      mdti[indx_mdti] <- mdti[indx_mdti] + max_dt_mdti
    }
  }
  symb <- cr_symb(k, m)
  Q0 <- q_symb_A2(Y, ms, symb)
  set.seed(seedinit)
  mcsamp <- rsample::bootstraps(as.data.frame(as.factor(Y)),
                                  times = nsim)
  Qfull_mc <- purrr::map(mcsamp$splits,
                           q_symb_A2, ms, symb)
  Qfull_stat <- unlist(Qfull_mc)
  Qpmc <- Qfull_stat[names(Qfull_stat) == "qp"]
  Qcmc <- Qfull_stat[names(Qfull_stat) == "qc"]
  mefp_symb <- matrix(0, nrow = nrow(symb$p_symb),
                      ncol = nsim)
  mefc_symb <- matrix(0, nrow = nrow(symb$c_symb),
                      ncol = nsim)
  rownames(mefp_symb) <- names(Q0$efp_symb)
  rownames(mefc_symb) <- names(Q0$efc_symb)
  colnames(mefp_symb) <- paste("sim", 1:nsim, sep = "")
  colnames(mefc_symb) <- paste("sim", 1:nsim, sep = "")
  for (i in 1:nsim) {
    mefp_symb[,i] <- Qfull_mc[[i]]$efp_symb
    mefc_symb[,i] <- Qfull_mc[[i]]$efc_symb
  }
  pvaluemc_p <- sum(Qpmc > Q0$qp) / (nsim + 1)
  pvaluemc_c <- sum(Qcmc > Q0$qc) / (nsim + 1)
  results <- list(Q0$qp, pvaluemc_p,
                  Q0$qc, pvaluemc_c,
                  Q0$qp_symb, Q0$qc_symb,
                  Q0$PSymb, Q0$CSymb,
                  Q0$efp_symb, Q0$efc_symb,
                  Qpmc, Qcmc, mefp_symb, mefc_symb,
                  ms, mdtms, symb, distance)
  names(results) <- c("qp", "pvaluemc_qp",
                      "qc", "pvaluemc_qc",
                      "qp_symb", "qc_symb",
                      "PSymb", "CSymb",
                      "efp_symb", "efc_symb",
                      "qpmc", "qcmc",
                      "efp_symb_mc", "efc_symb_mc",
                      "ms", "mdtms", "symb", "distance")
  return(results)
}
