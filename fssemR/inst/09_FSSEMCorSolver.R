library(fssemR)

TestCor4FSSEM = function(N = c(100, 100),
                         Ng = 30,
                         Nk = Ng * 3, Ns = 15 / Ng,
                         sigma2 = 0.01,
                         nrep = 20, dag = TRUE) {
  seed = as.numeric(Sys.time())
  set.seed(seed)
  Dfcor = NULL
  for (r in seq(0, 1, length.out = 5)) {
    data = randomFSSEMdata4Cor(
      n = N,
      p = Ng,
      k = Nk,
      sparse = Ns,
      df = 0.2,
      sigma2 = sigma2,
      u = 5,
      type = "DG",
      nhub = 1,
      dag = dag,
      r = r
    )
    gamma = cv.multiRegression(
      data$Data$X,
      data$Data$Y,
      data$Data$Sk,
      ngamma = 50,
      nfold = 5,
      N,
      Ng,
      Nk
    )
    fit   = multiRegression(data$Data$X,
                            data$Data$Y,
                            data$Data$Sk,
                            gamma,
                            N,
                            Ng,
                            Nk,
                            trans = FALSE)
    Xs    = data$Data$X
    Ys    = data$Data$Y
    Sk    = data$Data$Sk
    
    fitm <-
      opt.multiFSSEMiPALM(
        Xs = Xs,
        Ys = Ys,
        Bs = fit$Bs,
        Fs = fit$Fs,
        Sk = Sk,
        sigma2 = fit$sigma2,
        nlambda = 20,
        nrho = 20,
        p = Ng,
        q = Nk,
        wt = T
      )
    
    fitc0 <- multiFSSEMiPALM(Xs = Xs, Ys = Ys, Bs = fit$Bs, Fs = fit$Fs, Sk = Sk,
                            sigma2 = fit$sigma2, lambda = fitm$lambda, rho = fitm$rho,
                            Wl = inverseB(fit$Bs), Wf = flinvB(fit$Bs),
                            p = Ng, maxit = 1000, threshold = 1e-6, sparse = T, verbose = T, trans = T, strict = T)
    
    TPR = (TPR(fitc0$Bs[[1]], data$Vars$B[[1]], 1e-2) + TPR(fitc0$Bs[[2]], data$Vars$B[[2]], 1e-2)) / 2
    FDR = (FDR(fitc0$Bs[[1]], data$Vars$B[[1]], 1e-2) + FDR(fitc0$Bs[[2]], data$Vars$B[[2]], 1e-2)) / 2
    TPR2 = TPR(fitc0$Bs[[1]] - fitc0$Bs[[2]], data$Vars$B[[1]] - data$Vars$B[[2]], 1e-2)
    FDR2 = FDR(fitc0$Bs[[1]] - fitc0$Bs[[2]], data$Vars$B[[1]] - data$Vars$B[[2]], 1e-2)
    Dfcor = rbind(Dfcor, c(r, TPR, FDR, TPR2, FDR2))
  }
  Dfcor = as.data.frame(Dfcor)
  colnames(Dfcor) = c("Corr", "TPR", "FDR", "TPR2", "FDR2")
  rownames(Dfcor) = NULL
  for (n in 1:(nrep-1)) {
    for (r in seq(0, 1, length.out = 5)) {
      data = randomFSSEMdata4Cor(
        n = N,
        p = Ng,
        k = Nk,
        sparse = Ns,
        df = 0.2,
        sigma2 = sigma2,
        u = 5,
        type = "DG",
        nhub = 1,
        dag = dag,
        r = r
      )
      gamma = cv.multiRegression(
        data$Data$X,
        data$Data$Y,
        data$Data$Sk,
        ngamma = 50,
        nfold = 5,
        N,
        Ng,
        Nk
      )
      fit   = multiRegression(data$Data$X,
                              data$Data$Y,
                              data$Data$Sk,
                              gamma,
                              N,
                              Ng,
                              Nk,
                              trans = FALSE)
      Xs    = data$Data$X
      Ys    = data$Data$Y
      Sk    = data$Data$Sk
      
      fitm <-
        opt.multiFSSEMiPALM(
          Xs = Xs,
          Ys = Ys,
          Bs = fit$Bs,
          Fs = fit$Fs,
          Sk = Sk,
          sigma2 = fit$sigma2,
          nlambda = 20,
          nrho = 20,
          p = Ng,
          q = Nk,
          wt = T
        )
      
      fitc0 <- multiFSSEMiPALM(Xs = Xs, Ys = Ys, Bs = fit$Bs, Fs = fit$Fs, Sk = Sk,
                            sigma2 = fit$sigma2, lambda = fitm$lambda, rho = fitm$rho,
                            Wl = inverseB(fit$Bs), Wf = flinvB(fit$Bs),
                            p = Ng, maxit = 1000, threshold = 1e-6, sparse = T, verbose = T, trans = T, strict = T)
      
      TPR = (TPR(fitc0$Bs[[1]], data$Vars$B[[1]], 1e-2) + TPR(fitc0$Bs[[2]], data$Vars$B[[2]], 1e-2)) / 2
      FDR = (FDR(fitc0$Bs[[1]], data$Vars$B[[1]], 1e-2) + FDR(fitc0$Bs[[2]], data$Vars$B[[2]], 1e-2)) / 2
      TPR2 = TPR(fitc0$Bs[[1]] - fitc0$Bs[[2]], data$Vars$B[[1]] - data$Vars$B[[2]], 1e-2)
      FDR2 = FDR(fitc0$Bs[[1]] - fitc0$Bs[[2]], data$Vars$B[[1]] - data$Vars$B[[2]], 1e-2)
      Dfcor[Dfcor$Corr == r, ] =  Dfcor[Dfcor$Corr == r, ] + c(0, TPR, FDR, TPR2, FDR2)
    }
  }
  Dfcor[, c(2, 3, 4, 5)] = Dfcor[, c(2, 3, 4, 5)] / nrep
  Dfcor
}

DfcorDAG30_0.01 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.01, nrep = 30, dag = TRUE)
saveRDS(DfcorDAG30_0.01, file = "DfcorDAG30_0.01.rds")
DfcorDCG30_0.01 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.01, nrep = 30, dag = FALSE)
saveRDS(DfcorDCG30_0.01, file = "DfcorDCG30_0.01.rds")
DfcorDAG30_0.10 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.10, nrep = 30, dag = TRUE)
saveRDS(DfcorDAG30_0.10, file = "DfcorDAG30_0.10.rds")
DfcorDCG30_0.10 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.10, nrep = 30, dag = FALSE)
saveRDS(DfcorDCG30_0.10, file = "DfcorDCG30_0.10.rds")
DfcorDAG30_0.25 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.25, nrep = 30, dag = TRUE)
saveRDS(DfcorDAG30_0.25, file = "DfcorDAG30_0.25.rds")
DfcorDCG30_0.25 = TestCor4FSSEM(Ng = 30, Nk = 30 * 3, Ns = 30/30, sigma2 = 0.25, nrep = 30, dag = FALSE)
saveRDS(DfcorDCG30_0.25, file = "DfcorDCG30_0.25.rds")

PlotData = list(DfcorDAG30_0.01 = DfcorDAG30_0.01, DfcorDCG30_0.01 = DfcorDCG30_0.01, 
                DfcorDAG30_0.10 = DfcorDAG30_0.10, DfcorDCG30_0.10 = DfcorDCG30_0.10,
                DfcorDAG30_0.25 = DfcorDAG30_0.25, DfcorDCG30_0.25 = DfcorDCG30_0.25)

saveRDS(PlotData, "/media/xinchou/Storage/FSSEMBioinfor/PlotCorrData4FSSEM.rds")
