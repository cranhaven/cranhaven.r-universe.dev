# Internal function calling the individual test statistics for the tests 
# implemented in the package. The test statistics are listed below. 
# The function is called by the internal function .gofCopulapb which performs
# the parametric bootstrap 
.Tstats <- function(x, Tstat, copula, ...) {
  if (is.element(Tstat, eval(formals(gofTstat)$method))) {
    res.f <- gofTstat(u = x, method = Tstat, copula = copula)
    return(res.f)
  } else if (Tstat == "KS") {
    res.f <- .KS(x = x, copula = copula)
    return(res.f)
  } else if (Tstat == "Rn") {
    res.f <- .Rn(x = x, copula = copula, dims = copula@dimension)
    return(res.f)
  } else if (Tstat == "Tn") {
    add.parameters <- list(...)$add.parameters
    res.f <- .Tn(x = x, copula = copula, B = add.parameters[[1]], 
                 m = add.parameters[[2]], dims = copula@dimension, 
                 estim.method = add.parameters[[3]])
    return(res.f)
  } else if (Tstat == "Kernel") {
    add.parameters <- list(...)$add.parameters
    res.f <- .Kernel(x = x, copula = copula, dims = copula@dimension, 
                     n = nrow(x), nodes.Integration = add.parameters[[1]], 
                     MJ = add.parameters[[2]], delta.J = add.parameters[[3]])
    return(res.f)
  } else if (Tstat == "SnK") {
    cop.compare <- list(...)$cop.compare
    res.f <- .SnK(x = x, cop = cop.compare)
    return(res.f)
  } else if (Tstat == "TnK") {
    cop.compare <- list(...)$cop.compare
    res.f <- .TnK(x = x, cop = cop.compare)
    return(res.f)
  } else if (Tstat == "White") {
    add.parameters <- list(...)$add.parameters
    BiCopGofTest(u1 = x[, 1], u2 = x[, 2], family = add.parameters[[1]], 
                 par = if (inherits(copula, "tCopula")) {
      copula@parameters[-length(copula@parameters)]
    } else {
      copula@parameters
    }, par2 = if (inherits(copula, "tCopula")) {
      copula@parameters[length(copula@parameters)]
    } else {
      0
    }, method = "white", B = 0)$statistic
  } else if (!is.null(Tstat)) {
    res.f <- doCall(Tstat, x = x, copula = copula)
    return(res.f)
  }
}

# Internal function for the Kolmogorov Smirnov test statistic
.KS <- function(x, copula) {
  n <- dim(x)[1]
  dims <- dim(x)[2]
  Cn <- F.n(x, x)
  Csample.n <- pCopula(x, copula)
  KS <- sqrt(n) * max(abs(Cn - Csample.n))
  return(KS)
}

# Internal function for the Cramer von Mises test statistic with Kendall 
# transformation
.SnK <- function(x, cop) {
  n <- dim(x)[1]
  dims <- dim(x)[2]
  Cn <- F.n(x, x)
  Kn <- F.n(as.matrix(seq(1, n) / n), as.matrix(Cn))
  tcop <- seq(1, n) / n
  Csample.n <- cop
  Scvm1 <- sum(Kn[-n]^2 * (F.n(as.matrix(tcop[-1]), 
                               as.matrix(Csample.n)) - 
                             F.n(as.matrix(tcop[-n]), as.matrix(Csample.n)))) 
  Scvm2 <- sum(Kn[-n] * (F.n(as.matrix(tcop[-1]), 
                             as.matrix(Csample.n))^2 - 
                           F.n(as.matrix(tcop[-n]), as.matrix(Csample.n))^2))
  SnK <- n / 3 + n * Scvm1 - n * Scvm2
  return(SnK)
}

# Internal function for the Kolmogorov Smirnov test statistic with Kendall 
# transformation
.TnK <- function(x, cop) {
  n <- dim(x)[1]
  dims <- dim(x)[2]
  Cn <- F.n(x, x)
  Kn <- F.n(as.matrix(seq(1, n) / n), as.matrix(Cn))
  Csample.n <- cop
  Ksample <- F.n(as.matrix(seq(1, n) / n), as.matrix(Csample.n))
  TnK <- sqrt(n) * max(abs(c(Kn[-n] - Ksample[-n], Kn[-n] - Ksample[-1])))
  return(TnK)
}


# Internal function for the PIOSRn test statistic 
.Rn <- function(x, copula, dims) {
  switch(dims - 1,
    {
      switch(class(copula),
        claytonCopula = {
          Rn.ac.c <- -sum(.V.clay.12(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.S.clay.12(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.c)
        },
        gumbelCopula = {
          Rn.ac.g <- -sum(.V.gumb.12(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.S.gumb.12(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.g)
        },
        frankCopula = {
          Rn.ac.f <- -sum(.V.fran.12(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.S.fran.12(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.f)
        },
        tCopula = {
          psn.sample <- qt(x, df = tail(copula@parameters, n = 1))
          Rn.t <- -sum(apply(psn.sample, 1, FUN = .V.t, 
                             rho = copula@parameters[-length(copula@parameters)], 
                             nu = tail(copula@parameters, n = 1))^2) / 
            sum(apply(psn.sample, 1, FUN = .S.t, 
                      rho = copula@parameters[-length(copula@parameters)], 
                      nu = tail(copula@parameters, n = 1))) - 1
          return(Rn.t)
        },
        normalCopula = {
          sig <- matrix(c(1, copula@parameters, copula@parameters, 1), 
                        ncol = 2, byrow = TRUE)
          sig.inv <- rbind(c(sig[1, 1], -sig[1, 2]), c(-sig[2, 1], sig[2, 2])) / 
            det(sig)
          psn.sample <- qnorm(x)
          Rn.g <- -sum(apply(psn.sample, 1, FUN = .V.ga, 
                             sig.inv = sig.inv, dims = dims)[2, ]^2) / 
            sum(apply(psn.sample, 1, FUN = .S.ga.12, sig = sig)) - 1
          return(Rn.g)
        },
        joeCopula = {
          Rn.ac.j <- -sum(.joe.12.V(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.joe.12.S(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.j)
        },
        amhCopula = {
          Rn.ac.amh <- -sum(.amh.12.V(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.amh.12.S(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.amh)
        },
        galambosCopula = {
          Rn.ac.gal <- -sum(.galambos.12.V(copula@parameters, x[, 1], 
                                           x[, 2])^2) / 
            sum(.galambos.12.S(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.gal)
        },
        fgmCopula = {
          Rn.ac.fgm <- -sum(.fgm.12.V(copula@parameters, x[, 1], x[, 2])^2) / 
            sum(.fgm.12.S(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.fgm)
        },
        plackettCopula = {
          Rn.ac.pla <- -sum(.plackett.12.V(copula@parameters, x[, 1], 
                                           x[, 2])^2) / 
            sum(.plackett.12.S(copula@parameters, x[, 1], x[, 2])) - 1
          return(Rn.ac.pla)
        },
        stop("This copula is not implemented for this test.")
      )
    },
    {
      switch(class(copula),
        claytonCopula = {
          Rn.ac.c <- -sum(.clay.V.123(copula@parameters, x)^2) / 
            sum(.clay.S.123(copula@parameters, x)) - 1
          return(Rn.ac.c)
        },
        gumbelCopula = {
          Rn.ac.g <- -sum(.gumb.V.123(copula@parameters, x)^2) / 
            sum(.gumb.S.123(copula@parameters, x)) - 1
          return(Rn.ac.g)
        },
        frankCopula = {
          Rn.ac.f <- -sum(.fran.V.123(copula@parameters, x)^2) / 
            sum(.fran.S.123(copula@parameters, x)) - 1
          return(Rn.ac.f)
        },
        tCopula = {
stop(
"The t-Copula is not implemented for dimension 3 and the test gofPIOSRn."
)
        },
        normalCopula = {
          sig <- getSigma(copula)
          sig.inv <- solve(sig)
          sig.inv <- (sig.inv + t(sig.inv)) / 2
          psn.sample <- qnorm(x)
          V <- t(apply(psn.sample, 1, FUN = .V.ga, 
                       sig.inv = sig.inv, dims = dims)[c(2, 3, 6), ])
          V <- matrix(colSums(cbind(V[, 1]^2, V[, 1] * V[, 2], V[, 1] * V[, 3], 
                                    V[, 1] * V[, 2], V[, 2]^2, V[, 2] * V[, 3], 
                                    V[, 1] * V[, 3], V[, 2] * V[, 3], 
                                    V[, 3]^2)), 
                      ncol = dims)
          S1212 <- apply(psn.sample, 1, FUN = .S.ga.12.12, sig = sig)
          S1213 <- apply(psn.sample, 1, FUN = .S.ga.12.13, sig = sig)
          S1223 <- apply(psn.sample, 1, FUN = .S.ga.12.23, sig = sig)
          S1313 <- apply(psn.sample, 1, FUN = .S.ga.13.13, sig = sig)
          S1323 <- apply(psn.sample, 1, FUN = .S.ga.13.23, sig = sig)
          S2323 <- apply(psn.sample, 1, FUN = .S.ga.23.23, sig = sig)
          S <- matrix(colSums(cbind(S1212, S1213, S1223, S1213, S1313, S1323, 
                                    S1223, S1323, S2323)), ncol = dims)
          Rn.g <- sum(diag(-solve(S) %*% V)) - 3
          return(Rn.g)
        },
        joeCopula = {
          Rn.ac.j <- -sum(.joe.123.V(copula@parameters, x)^2) / 
            sum(.joe.123.S(copula@parameters, x)) - 1
          return(Rn.ac.j)
        },
        amhCopula = {
          Rn.ac.amh <- -sum(.amh.123.V(copula@parameters, x)^2) / 
            sum(.amh.123.S(copula@parameters, x)) - 1
          return(Rn.ac.amh)
        },
        galambosCopula = {
stop(
"The galambos Copula is not implemented for dimension 3 and the test 
gofPIOSRn."
)
        },
        fgmCopula = {
stop(
"The fgm Copula is not implemented for dimension 3 and the test gofPIOSRn."
)
        },
        plackettCopula = {
stop(
"The plackett Copula is not implemented for dimension 3 and the test 
gofPIOSRn."
)
        },
        stop("This copula is not implemented for this test.")
      )
    },
    stop("This dimension is not implemented for this test.")
  )
}

# Internal function for the PIOSTn test statistic 
.Tn <- function(x, copula, B, m, dims, estim.method) {
  switch(dims - 1,
    {
      switch(class(copula),
        claytonCopula = {
          l.ac.c <- log(.clay.12.density(copula@parameters, x))
          l.ac.c.b <- 0
            for (b in seq_len(B)) {
              l.ac.c.b <- c(l.ac.c.b, 
                            .clay.12.density(fitCopula(
                              claytonCopula(dim = dims), 
                              data = x[-(((b - 1) * m + 1):(b * m)), ], 
                              method = estim.method, 
                              estimate.variance = FALSE)@estimate, 
                              x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.c.b <- l.ac.c.b[-1]
          stat <- sum(l.ac.c - log(l.ac.c.b)) - 1
          return(stat)
        },
        gumbelCopula = {
          l.ac.g <- log(.gumb.12.density(copula@parameters, x))
          l.ac.g.b <- 0
            for (b in seq_len(B)) {
              l.ac.g.b <- c(l.ac.g.b, .gumb.12.density(fitCopula(
                gumbelCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.g.b <- l.ac.g.b[-1]
          stat <- sum(l.ac.g - log(l.ac.g.b)) - 1
          return(stat)
        },
        frankCopula = {
          l.ac.f <- log(.fran.12.density(copula@parameters, x))
          l.ac.f.b <- 0
            for (b in seq_len(B)) {
              l.ac.f.b <- c(l.ac.f.b, .fran.12.density(fitCopula(
                frankCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.f.b <- l.ac.f.b[-1]
          stat <- sum(l.ac.f - log(l.ac.f.b)) - 1
          return(stat)
        },
        tCopula = {
          psn.sample <- qt(x, df = tail(copula@parameters, n = 1))
          l.t <- log(.t.12.dens(copula@parameters[-length(copula@parameters)], 
                                psn.sample, 
                                nu = tail(copula@parameters, n = 1)))
          l.t.b <- 0
            for (b in seq_len(B)) {
              l.t.b <- c(l.t.b, .t.12.dens(fitCopula(
                tCopula(dim = dims, df = tail(copula@parameters, n = 1), 
                        df.fixed = TRUE), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                psn.sample[((b - 1) * m + 1):(b * m), ], 
                nu = tail(copula@parameters, n = 1)))
            }
          l.t.b <- l.t.b[-1]
          stat <- sum(l.t - log(l.t.b)) - 1
          return(stat)
        },
        normalCopula = {
          psn.sample <- qnorm(x)
          l.ac.ga.b <- as.vector(sapply(1:B, FUN = .opt.ga, 
                                        psn.sample = psn.sample, m = m, 
                                        dims = dims))
          stat <- sum(log(dCopula(x, normalCopula(copula@parameters, dims))) - 
                        log(l.ac.ga.b)) - 1
          return(stat)
        },
        joeCopula = {
          l.ac.j <- log(.joe.12.density(copula@parameters, x))
          l.ac.j.b <- 0
            for (b in seq_len(B)) {
              l.ac.j.b <- c(l.ac.j.b, .joe.12.density(fitCopula(
                joeCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.j.b <- l.ac.j.b[-1]
          stat <- sum(l.ac.j - log(l.ac.j.b)) - 1
          return(stat)
        },
        amhCopula = {
          l.ac.amh <- log(.amh.12.density(copula@parameters, x))
          l.ac.amh.b <- 0
            for (b in seq_len(B)) {
              l.ac.amh.b <- c(l.ac.amh.b, 
                              .amh.12.density(fitCopula(
                                amhCopula(dim = dims), 
                                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                                method = estim.method, 
                                estimate.variance = FALSE)@estimate, 
                                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.amh.b <- l.ac.amh.b[-1]
          stat <- sum(l.ac.amh - log(l.ac.amh.b)) - 1
          return(stat)
        },
        galambosCopula = {
          l.ac.gal <- log(.galambos.12.density(copula@parameters, x))
          l.ac.gal.b <- 0
            for (b in seq_len(B)) {
              l.ac.gal.b <- c(l.ac.gal.b, .galambos.12.density(fitCopula(
                galambosCopula(),
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.gal.b <- l.ac.gal.b[-1]
          stat <- sum(l.ac.gal - log(l.ac.gal.b)) - 1
          return(stat)
        },
        fgmCopula = {
          l.ac.fgm <- log(.fgm.12.density(copula@parameters, x))
          l.ac.fgm.b <- 0
            for (b in seq_len(B)) {
              l.ac.fgm.b <- c(l.ac.fgm.b, .fgm.12.density(fitCopula(
                fgmCopula(),
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.fgm.b <- l.ac.fgm.b[-1]
          stat <- sum(l.ac.fgm - log(l.ac.fgm.b)) - 1
          return(stat)
        },
        plackettCopula = {
          l.ac.pla <- log(.plackett.12.density(copula@parameters, x))
          l.ac.pla.b <- 0
            for (b in seq_len(B)) {
              l.ac.pla.b <- c(l.ac.pla.b, .plackett.12.density(fitCopula(
                plackettCopula(),
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.pla.b <- l.ac.pla.b[-1]
          stat <- sum(l.ac.pla - log(l.ac.pla.b)) - 1
          return(stat)
        },
        stop("This copula is not implemented for this test.")
      )
    },
    {
      switch(class(copula),
        claytonCopula = {
          l.ac.c <- log(.clay.123.density(copula@parameters, x))
          l.ac.c.b <- 0
            for (b in seq_len(B)) {
              l.ac.c.b <- c(l.ac.c.b, .clay.123.density(fitCopula(
                claytonCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.c.b <- l.ac.c.b[-1]
          stat <- sum(l.ac.c - log(l.ac.c.b)) - 1
          return(stat)
        },
        gumbelCopula = {
          l.ac.g <- log(.gumb.123.density(copula@parameters, x))
          l.ac.g.b <- 0
            for (b in seq_len(B)) {
              l.ac.g.b <- c(l.ac.g.b, .gumb.123.density(fitCopula(
                gumbelCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.g.b <- l.ac.g.b[-1]
          stat <- sum(l.ac.g - log(l.ac.g.b)) - 1
          return(stat)
        },
        frankCopula = {
          l.ac.f <- log(.fran.123.density(copula@parameters, x))
          l.ac.f.b <- 0
            for (b in seq_len(B)) {
              l.ac.f.b <- c(l.ac.f.b, 
                            .fran.123.density(fitCopula(
                              frankCopula(dim = dims), 
                              data = x[-(((b - 1) * m + 1):(b * m)), ], 
                              method = estim.method, 
                              estimate.variance = FALSE)@estimate, 
                              x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.f.b <- l.ac.f.b[-1]
          stat <- sum(l.ac.f - log(l.ac.f.b)) - 1
          return(stat)
        },
        tCopula = {
stop(
"The t-Copula is not implemented for dimension 3 and the test gofPIOSTn."
)
        },
        normalCopula = {
          psn.sample <- qnorm(x)
          l.ac.ga.b <- as.vector(sapply(seq_len(B), FUN = .opt.ga, 
                                        psn.sample = psn.sample, m = m, 
                                        dims = dims))
          stat <- sum(log(dCopula(x, 
                                  normalCopula(copula@parameters, dims, 
                                               dispstr = copula@dispstr))) - 
                        log(l.ac.ga.b)) - 3
          return(stat)
        },
        joeCopula = {
          l.ac.j <- log(.joe.123.density(copula@parameters, x))
          l.ac.j.b <- 0
            for (b in seq_len(B)) {
              l.ac.j.b <- c(l.ac.j.b, .joe.123.density(fitCopula(
                joeCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.j.b <- l.ac.j.b[-1]
          stat <- sum(l.ac.j - log(l.ac.j.b)) - 1
          return(stat)
        },
        amhCopula = {
          l.ac.amh <- log(.amh.123.density(copula@parameters, x))
          l.ac.amh.b <- 0
            for (b in seq_len(B)) {
              l.ac.amh.b <- c(l.ac.amh.b, .amh.123.density(fitCopula(
                amhCopula(dim = dims), 
                data = x[-(((b - 1) * m + 1):(b * m)), ], 
                method = estim.method, 
                estimate.variance = FALSE)@estimate, 
                x[((b - 1) * m + 1):(b * m), ]))
            }
          l.ac.amh.b <- l.ac.amh.b[-1]
          stat <- sum(l.ac.amh - log(l.ac.amh.b)) - 1
          return(stat)
        },
        galambosCopula = {
stop(
"The galambos Copula is not implemented for dimension 3 and the test 
gofPIOSTn."
)
        },
        fgmCopula = {
stop(
"The fgm Copula is not implemented for dimension 3 and the test gofPIOSTn."
)
        },
        plackettCopula = {
stop(
"The plackett Copula is not implemented for dimension 3 and the test 
gofPIOSTn."
)
        },
        stop("This copula is not implemented for this test.")
      )
    },
    stop("This dimension is not implemented for this test.")
  )
}

# Internal function for the Kernel test statistic 
.Kernel <- function(x, copula, dims, n, nodes.Integration, MJ, delta.J) {
  switch(class(copula),
    claytonCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, claytonCopula(copula@parameters, dim = dims))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    gumbelCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, gumbelCopula(copula@parameters, dim = dims))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    frankCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, frankCopula(copula@parameters, dim = dims))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_f <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_f <- c(Jn_f, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_f)
      return(stat)
    },
    tCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, 
                            tCopula(
                              copula@parameters[-length(copula@parameters)], 
                                    df = tail(copula@parameters, n = 1), 
                                    dim = dims, dispstr = copula@dispstr))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    normalCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, normalCopula(copula@parameters, dim = dims, 
                                             dispstr = copula@dispstr))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    joeCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, joeCopula(copula@parameters, dim = dims))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    amhCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, amhCopula(copula@parameters, dim = dims))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    galambosCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, galambosCopula(copula@parameters))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    fgmCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, fgmCopula(copula@parameters))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    plackettCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, plackettCopula(copula@parameters))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    tevCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, 
                            tevCopula(
                              copula@parameters[-length(copula@parameters)], 
                                    df = tail(copula@parameters, n = 1)))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    tawnCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, tawnCopula(copula@parameters))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    huslerReissCopula = {
      Int_Grid <- createIntegrationGrid("GQU", dimension = dims, 
                                        k = nodes.Integration)
      bootsample <- rCopula(MJ, huslerReissCopula(copula@parameters))
      h <- as.vector((diag(2.6073 * n^(-1 / 6) * chol(cov(x))) * delta.J))
      Jn_c <- c()
      for (i in seq_len(dim(Int_Grid$nodes)[1])) {
        Jn_c <- c(Jn_c, Int_Grid$weights[i] * 
                    .integrand(Int_Grid$nodes[i, ], x, 
                               Lbootsample = bootsample, h))
      }
      stat <- sum(Jn_c)
      return(stat)
    },
    stop("This copula is not implemented for this test.")
  )
}
