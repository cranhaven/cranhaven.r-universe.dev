.gofCopulapb <- function(copula, x, M, method, estim.method, processes, 
                         param.est, df.est, dispstr, param.margins, margins, 
                         seed.active, lower, upper, flip, ...) {
  # required to avoid error on global variable definition
  cnt <- NULL

  if (!is.null(seed.active)) {
    set.seed(seed.active[1], kind = "default")
  }
  bs.ac.c <- list()

  if (is.element(method, c("SnB", "SnC", "AnChisq", "AnGamma"))) {
    if (inherits(class(copula), "amhCopula")) {
      if (getAcop(copula)@paraConstr(copula@parameters,dim(copula)) == FALSE) {
stop(
"The Rosenblatt transformation cannot be computed for these parameters. Please 
consider using a different test or try different parameters for the copula."
)
      }  
    }
    
    # Rosenblatt transformation if needed 
    if (inherits(class(copula), "galambosCopula")) {
      x = .rosenblatt.galambos(u = x, theta = copula@parameters)
    } else if (inherits(class(copula), "fgmCopula")) {
      x = .rosenblatt.fgm(u = x, theta = copula@parameters)
    } else if (inherits(class(copula), "plackettCopula")) {
      x = .rosenblatt.plackett(u = x, theta = copula@parameters)
    } else {
      x <- do.call(cCopula, c(list(x, copula = copula)))
    }
  }
  
  if (is.element(method, c("ArchmSnB", "ArchmSnC", "ArchmAnChisq", 
                           "ArchmAnGamma"))) {
    if (inherits(class(copula), "amhCopula")) {
      if (inherits(class(try(F.n(as.matrix(pCopula(x, copula)), 
                        as.matrix(pCopula(x, copula))), 
                    silent = TRUE)), "try-error")) {
stop(
"The Rosenblatt transformation for Archimedean Copula cannot be computed for 
these parameters. Please consider using a different test or try different 
parameters for the copula."
)
      }  
    }
    x <- .ArchmRtrans(copula = copula, x = x)
  }
  
  # Test statistic for dataset depending on test and copula choice
  ac.c <- if (method == "Sn" || method == "KS") {
    if (inherits(copula, c("tCopula", "tevCopula"))) {
      copula@parameters[length(copula@parameters)] <- as.integer(ceiling(
        copula@parameters[length(copula@parameters)]))
    }
    .Tstats(x, Tstat = method, copula = copula)
  } else if (method == "Tn") {
    if (inherits(copula, c("tCopula", "tevCopula"))) {
      copula@parameters[2] <- min(copula@parameters[2], 60)
    }
    add.parameters <- list(...)$add.parameters
    .Tstats(x, Tstat = method, copula = copula, add.parameters = add.parameters)
  } else if (method == "Kernel" || method == "White") {
    add.parameters <- list(...)$add.parameters
    .Tstats(x, Tstat = method, copula = copula, add.parameters = add.parameters)
  } else if (is.element(method, c("ArchmSnB", "ArchmSnC", "ArchmAnChisq", 
                                  "ArchmAnGamma"))) {
    method.temp = sub("Archm", "", method)
    .Tstats(x, Tstat = method.temp, copula = copula)
  } else {
    cop.compare <- rCopula(10000, copula)
    cop.compare.n <- F.n(cop.compare, cop.compare)
    .Tstats(x, Tstat = method, copula = copula, cop.compare = cop.compare.n)
  }

  # Parallelization if specified by the user 
  if (processes > 1) {
    cl <- makeCluster(processes)
    clusterEvalQ(cl, library(copula))
    clusterEvalQ(cl, library(foreach))
    clusterEvalQ(cl, library(progress))
    clusterEvalQ(cl, library(gofCopula))
    if (method == "White") {
      clusterEvalQ(cl, "BiCopDeriv")
      clusterEvalQ(cl, "BiCopDeriv2")
      clusterEvalQ(cl, "BiCopPDF")
      clusterEvalQ(cl, "ginv")
    }
    if (!is.element(method, c("SnB", "SnC", "AnChisq", "AnGamma", "Sn", "KS", 
                              "Tn", "Kernel", "White", "SnK", "TnK", "Rn", 
                              "ArchmSnB", "ArchmSnC", "ArchmAnChisq", 
                              "ArchmAnGamma"))) {
      clusterExport(cl, paste(method))
    }
    registerDoSNOW(cl)
  } else {
    registerDoSEQ()
  }

  # Progress bar
  pb <- progress_bar$new(total = M, 
                         format = ":dummy [:bar] :percent | time left: :eta", 
                         force = TRUE, show_after = 0)
  progBar_dummy <- rep("Progress:", M)
  progBar <- function(n) {
    pb$tick(tokens = list(dummy = progBar_dummy[n]))
  }
  opts <- list(progress = progBar)

  bs.ac.c <- foreach(cnt = seq_len(M), .options.snow = opts) %dopar% {
    progBar(cnt)
    seed.counter <- 0
    repeat {
      if (!is.null(seed.active)) {
        new.seed <- seed.active[cnt + 1] + (M + 1) * seed.counter
        while (is.element(new.seed, seed.active)) {
          seed.counter <- seed.counter + 1
          new.seed <- seed.active[cnt + 1] + (M + 1) * seed.counter
        }
        seed.active <- unique(c(seed.active, new.seed))
        set.seed(new.seed, kind = "default")
      }
      
      # data simulation for bootstrap
      xsim <- rCopula(nrow(x), copula)
      # margins entry has to be NULL, since the data are in [0,1] already
      copula.sim <- try(.margins.param.est(
        copula = as.character(substr(class(copula), 1, 
                                     nchar(class(copula)) - 6)), 
        margins = NULL, x = xsim,
        param = if (!inherits(copula, c("tCopula", "tevCopula"))) {
          copula@parameters
        } else {
          copula@parameters[-length(copula@parameters)]
        },
        param.est = TRUE,
        df = if (!inherits(copula, c("tCopula", "tevCopula"))) {
          4
        } else {
          copula@parameters[length(copula@parameters)]
        },
        df.est = df.est,
        dispstr = dispstr,
        lower = lower,
        upper = upper, 
        flip = flip
      )[[1]], silent = TRUE)

      if (!inherits(copula.sim, "try-error")) {
        break
      }
      seed.counter <- seed.counter + 1
    }
    
    # Rosenblatt transformation of simulated data if needed 
    if (is.element(method, c("SnB", "SnC", "AnChisq", "AnGamma"))) {
      if (inherits(class(copula.sim), "galambosCopula")) {
        xsim = .rosenblatt.galambos(u = xsim, theta = copula.sim@parameters)
      } else if (inherits(class(copula), "fgmCopula")) {
        xsim = .rosenblatt.fgm(u = xsim, theta = copula.sim@parameters)
      } else if (inherits(class(copula), "plackettCopula")) {
        xsim = .rosenblatt.plackett(u = xsim, theta = copula.sim@parameters)
      } else {
        xsim <- do.call(cCopula, c(list(xsim, copula = copula.sim)))
      }
    }
    if (is.element(method, c("ArchmSnB", "ArchmSnC", "ArchmAnChisq", 
                             "ArchmAnGamma"))) {
      xsim <- .ArchmRtrans(copula = copula.sim, x = xsim)
    }
    
    # Test statistic for simulated data depending on test and copula choice
    if (method == "Sn" || method == "KS") {
      if (inherits(copula.sim, c("tCopula", "tevCopula"))) {
        copula.sim@parameters[length(copula.sim@parameters)] <- as.integer(
          ceiling(copula.sim@parameters[length(copula.sim@parameters)]))
      }
      .Tstats(xsim, Tstat = method, copula = copula.sim)
    } else if (method == "Tn") {
      if (inherits(copula, c("tCopula", "tevCopula"))) {
        copula.sim@parameters[2] <- min(copula.sim@parameters[2], 60)
      }
      .Tstats(xsim, Tstat = method, copula = copula.sim, 
              add.parameters = add.parameters)
    } else if (method == "Kernel" || method == "White") {
      .Tstats(xsim, Tstat = method, copula = copula.sim, 
              add.parameters = add.parameters)
    } else if (is.element(method, c("ArchmSnB", "ArchmSnC", "ArchmAnChisq", 
                                    "ArchmAnGamma"))) {
      method.temp = sub("Archm", "", method)
      .Tstats(xsim, Tstat = method.temp, copula = copula.sim)
    } else {
      .Tstats(xsim, Tstat = method, copula = copula.sim, 
              cop.compare = cop.compare.n)
    }
  }

  bs.ac.c <- unlist(bs.ac.c)

  if (processes > 1) {
    stopCluster(cl)
  }

  ac.c <- as.numeric(ac.c)
  bs.ac.c <- as.numeric(bs.ac.c)
  test <- sum(abs(bs.ac.c) >= abs(ac.c)) / M

  switch(method,
    SnB = {
      matrix_names <- "RosenblattSnB"
    },
    SnC = {
      matrix_names <- "RosenblattSnC"
    },
    AnChisq = {
      matrix_names <- "RosenblattChisq"
    },
    AnGamma = {
      matrix_names <- "RosenblattGamma"
    },
    Sn = {
      matrix_names <- "CvM"
    },
    KS = {
      matrix_names <- "KS"
    },
    Rn = {
      matrix_names <- "PIOSRn"
    },
    Tn = {
      matrix_names <- "PIOSTn"
    },
    Kernel = {
      matrix_names <- "Kernel"
    },
    SnK = {
      matrix_names <- "KendallCvM"
    },
    TnK = {
      matrix_names <- "KendallKS"
    },
    White = {
      matrix_names <- "White"
    },
    ArchmSnB = {
      matrix_names <- "ArchmSnB"
    },
    ArchmSnC = {
      matrix_names <- "ArchmSnC"
    },
    ArchmAnGamma = {
      matrix_names <- "ArchmGamma"
    },
    ArchmAnChisq = {
      matrix_names <- "ArchmChisq"
    },
    {
      matrix_names <- method
    }
  )
  
  # Assigning of gofCOP structure
  res <- structure(
    class = "gofCOP",
    list(
      list(
        method = sprintf(
          "Parametric bootstrap goodness-of-fit test with %s test and %s copula",
          matrix_names, paste0(as.character(substr(class(copula), 1, 
                                                   nchar(class(copula)) - 6)), 
                               flip)
        ),
        copula = paste0(as.character(substr(class(copula), 1, 
                                            nchar(class(copula)) - 6)),flip),
        margins = margins,
        param.margins = param.margins,
        theta = if (!inherits(copula, c("tCopula", "tevCopula"))) {
          copula@parameters
        } else {
          copula@parameters[-length(copula@parameters)]
        },
        df = if (!inherits(copula, c("tCopula", "tevCopula"))) {
          NULL
        } else {
          copula@parameters[length(copula@parameters)]
        },
        res.tests = matrix(c(test, ac.c),
          ncol = 2,
          dimnames = list(matrix_names, c("p.value", "test statistic"))
        )
      )
    )
  )
  names(res) <- paste0(substr(class(copula), 1, nchar(class(copula)) - 6), flip)
  return(res)
}
