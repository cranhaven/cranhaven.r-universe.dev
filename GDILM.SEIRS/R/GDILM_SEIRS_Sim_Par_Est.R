#' GDILM SEIRS for a Simulation Study
#'
#' This function conducts a simulation study for the Geographically Dependent Individual Level Model (GDILM) of infectious disease transmission, incorporating reinfection dynamics within the Susceptible-Exposed-Infectious-Recovered-Susceptible (SEIRS) framework, using a user-defined grid size. It applies a likelihood based Monte Carlo Expectation Conditional Maximization (MCECM) algorithm to estimate model parameters and compute the AIC.
#' @param GridDim1 First dimension of the grid
#' @param GridDim2 Second dimension of the grid
#' @param NPostPerGrid Number of postal codes per grid cell
#' @param MaxTimePand Last time point of the pandemic
#' @param tau0 Initial value for spatial precision
#' @param lambda0 Initial value for spatial dependence
#' @param alphaS0 Initial value for the susceptibility intercept
#' @param delta0 Initial value for the spatial decay parameter
#' @param alphaT0 Initial value for the infectivity intercept
#' @param PopMin Minimum population per postal code
#' @param PopMax Maximum population per postal code
#' @param InfFraction Fraction of each grid cell's population to be infected
#' @param ReInfFraction Fraction of each grid cell's population to be reinfected
#' @param InfPrd Infectious period that can be obtained either from the literature or by fitting an SEIRS model to the data
#' @param IncPrd Incubation period that can be obtained either from the literature or by fitting an SEIRS model to the data
#' @param NIterMC Number of MCMC iterations
#' @param NIterMCECM Number of MCECM iterations
#' @return
#'
#'   `alphaS` Estimate of alpha S
#'
#'   `BetaCovInf` Estimate of beta vector for the individual level infection covariate
#'
#'   `BetaCovSus` Estimate of beta vector for the areal susceptibility to first infection covariate
#'
#'   `BetaCovSusReInf` Estimate of beta vector for the areal susceptibility to reinfection covariate
#'
#'   `alphaT` Estimate of alpha T
#'
#'   `delta` Estimate of delta
#'
#'   `tau1` Estimate of tau
#'
#'   `lambda1` Estimate of lambda
#'
#'   `AIC` AIC of the fitted GDILM SEIRS
#'
#' @export
#' @import MASS
#' @import mvtnorm
#' @import ngspatial
#' @import stats
#' @examples
#' \donttest{
#' GDILM_SEIRS_Sim_Par_Est(5,5,10,30,0.7, 0.5, 1, 2.5, 0,40, 50,0.3,0.6, 5, 5, 10, 3)
#' }
#'
GDILM_SEIRS_Sim_Par_Est=function(GridDim1,GridDim2,NPostPerGrid,MaxTimePand,tau0, lambda0, alphaS0, delta0, alphaT0,PopMin, PopMax,InfFraction,ReInfFraction, InfPrd, IncPrd, NIterMC, NIterMCECM){
  if(lambda0>1) stop("The spatial dependence parameter should be restricted to a range between 0 and 1.")
  if(lambda0==0) stop("Absence of spatial dependence: This model is designed for scenarios where spatial dependence is present.")
  if(delta0<0) stop("The spatial decay parameter must be greater than zero.")
  if(NIterMC<2) stop("The number of iterations must exceed 2.")
  if(InfPrd<0) stop("The infectious period must be greater than zero.")
  if(IncPrd<0) stop("The incubation period must be greater than zero.")
  if(InfFraction>1) stop("Fraction of each grid cell's population to be infected must be be restricted to a range between 0 and 1.")
  if(InfFraction<0) stop("Fraction of each grid cell's population to be infected must be be restricted to a range between 0 and 1.")
  if(ReInfFraction>1) stop("Fraction of each grid cell's population to be reinfected must be be restricted to a range between 0 and 1.")
  if(ReInfFraction<0) stop("Fraction of each grid cell's population to be reinfected must be be restricted to a range between 0 and 1.")
  NTotalGrid     <- GridDim1^2
  NAllPostPerGrid <- rep(NPostPerGrid, NTotalGrid)
  NTotalpost     <- sum(NAllPostPerGrid)
  generate_grid_data <- function(NPostPerGrid, GridDim1, GridDim2) {
    x_coords <- unlist(lapply(1:GridDim1, function(col) {
      matrix(runif(NPostPerGrid * GridDim2, GridDim1 * (col-1), GridDim1 * col), nrow = NPostPerGrid)
    }))
    y_coords <- unlist(lapply(1:GridDim2, function(row) {
      matrix(runif(NPostPerGrid * GridDim1, GridDim2 * (row-1), GridDim2 * row), nrow = NPostPerGrid)
    }))
    data.frame(xcor = x_coords, ycor = y_coords)
  }

  data <- generate_grid_data(NPostPerGrid, GridDim1, GridDim2)
  Lat  <- data$xcor
  Long <- data$ycor
  D  <- (-1)*adjacency.matrix(GridDim1, GridDim1)
  diag(D) <- colSums(adjacency.matrix(GridDim1, GridDim1))
  NLableGrid <- unlist(lapply(1:NTotalGrid, function(i) rep(i, NAllPostPerGrid[i])))
  NewLabelGrid <- sapply(1:NTotalGrid, function(g) rep(D[, g], NAllPostPerGrid))
  Dist <- as.matrix(dist(cbind(Lat, Long))) * 50
  Pop      <- sample(PopMin:PopMax, NTotalpost, replace = TRUE)
  NInf     <- pmax(round(Pop * InfFraction), 1)
  NReInf   <- pmax(round(Pop * ReInfFraction), 1)
  Sigma0 <- solve(tau0^2 * (lambda0 * D + (1-lambda0) * diag(NTotalGrid)))
  phi    <- mvrnorm(1, rep(0, NTotalGrid), Sigma0, tol = 1e-6)
  CovSus       <- cbind(rnorm(NTotalGrid,1,1), runif(NTotalGrid,1,2))
  CovSusReInf  <- cbind(rnorm(NTotalGrid,2,0.1), runif(NTotalGrid,2,3))
  CovInf       <- cbind(rnorm(NTotalpost,0,1), runif(NTotalpost,0,1))
  DimCovSus       <- ncol(CovSus)
  DimCovSusReInf  <- ncol(CovSusReInf)
  DimCovInf       <- ncol(CovInf)
  BetaCovSus0      <- rep(2, DimCovSus)
  BetaCovSusReInf0 <- rep(1, DimCovSusReInf)
  BetaCovInf0      <- rep(1, DimCovInf)
  ExpoTime  <- unlist(lapply(1:NTotalGrid, function(g) {
    vec <- rep(0, NAllPostPerGrid[g])
    vec[sapply(1:NTotalGrid, function(g) sample(NAllPostPerGrid[g], 1))[g]] <- 1
    vec
  }))

  ExpoTimeReInf <- unlist(lapply(1:NTotalGrid, function(g) {
    vec <- rep(0, NAllPostPerGrid[g])
    vec[sapply(1:NTotalGrid, function(g) sample(NAllPostPerGrid[g], 1))[g]] <- 1
    vec
  }))
  InfPeriod <- rep(InfPrd, NTotalpost)
  IncPeriod <- rep(IncPrd, NTotalpost)
  InfTime    <- ifelse(ExpoTime > 0, ExpoTime + IncPeriod[1], 0)
  ReInfTime  <- ifelse(ExpoTimeReInf > 0, ExpoTimeReInf + IncPeriod[1], 0)
  for(t in 1:MaxTimePand) {
    not_infected <- which(InfTime == 0)
    for(i in not_infected) {
      GridIndic <- NLableGrid[i]
      infectious <- which(NewLabelGrid[, GridIndic] != 0 &
                            InfTime <= t &
                            (InfTime + InfPeriod) >= t &
                            InfTime != 0)

      if(length(infectious) > 0) {
        dx <- sum(NInf[infectious] * exp(alphaT0 + CovInf[infectious, ] %*% BetaCovInf0) *
                    Dist[i, infectious]^(-delta0), na.rm = TRUE)
        P <- 1 - exp(-Pop[i] * exp(alphaS0 + CovSus[GridIndic, ] %*% BetaCovSus0 +
                                     CovSusReInf[GridIndic, ] %*% BetaCovSusReInf0 +
                                     phi[GridIndic]) * dx)
        if(runif(1) < P) InfTime[i] <- t + 1
      }
    }
  }
  ExpoTime <- InfTime - IncPeriod[1]
  for(t in 1:MaxTimePand) {
    not_infected <- which(ReInfTime == 0)
    for(i in not_infected) {
      GridIndic <- NLableGrid[i]
      infectious <- which(NewLabelGrid[, GridIndic] != 0 &
                            ReInfTime <= t &
                            (ReInfTime + InfPeriod) >= t &
                            ReInfTime != 0)
      if(length(infectious) > 0) {
        dx <- sum(NReInf[infectious] * exp(alphaT0 + CovInf[infectious, ] %*% BetaCovInf0) *
                    Dist[i, infectious]^(-delta0), na.rm = TRUE)
        P <- 1 - exp(-Pop[i] * exp(alphaS0 + CovSus[GridIndic, ] %*% BetaCovSus0 +
                                     CovSusReInf[GridIndic, ] %*% BetaCovSusReInf0 +
                                     phi[GridIndic]) * dx)
        if(runif(1) < P) ReInfTime[i] <- t + 1
      }
    }
  }
  ExpoTimeReInf <- ReInfTime - IncPeriod[1]
  is_exposed <- function(ExpoTime, IncPeriod, t, i) {
    ExpoTime[i] <= t &
      (ExpoTime[i] + IncPeriod[i]) > t &
      ExpoTime[i] != 0
  }
  replace_nonfinite <- function(x, value = 0) {
    x[!is.finite(x)] <- value
    return(x)
  }
  F1 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                   BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F11 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        F11[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta)
      }
    }
    F11<- replace_nonfinite(F11)
    return(sum(F11) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F2 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                        BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F22 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && ReInfTime[j] <= t && (ReInfTime[j] + InfPeriod[j]) >= t && ReInfTime[j] != 0) {
        F22[j] <- NReInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta)
      }
    }
    F22<- replace_nonfinite(F22)
    return(sum(F22) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F3 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                   BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F33 <- array(0, c(DimCovInf, 1, NTotalpost))
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        F33[,,j] <- NInf[j] * CovInf[j,] * as.numeric(exp(alphaT + CovInf[j,] %*% BetaCovInf)) * Dist[i, j]^(-delta)
      }
    }
    F33<- replace_nonfinite(F33)
    return(apply(F33, c(1,2), sum, na.rm = TRUE) *
             as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F4 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F44 <- array(0, c(DimCovInf, DimCovInf, NTotalpost))
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        F44[,,j] <- NInf[j] * CovInf[j,] %*% t(CovInf[j,]) * as.numeric(exp(alphaT + CovInf[j,] %*% BetaCovInf)) * Dist[i, j]^(-delta)
      }
    }
    F44<- replace_nonfinite(F44)
    return(apply(F44, c(1,2), sum, na.rm = TRUE) *
             as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F5 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F55 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        F55[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta) * log(Dist[i, j])
      }
    }
    F55<- replace_nonfinite(F55)
    return(sum(F55) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F6 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F66 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        F66[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta) * (log(Dist[i, j]))^2
      }
    }
    F66<- replace_nonfinite(F66)
    return(sum(F66) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  F7 <- function(phi, alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    F8 <- array(0, c(NTotalpost, MaxTimePand, NTotalGrid))
    for (i in 1:NTotalpost) {
      for (t in 1:MaxTimePand) {
        for (GridIndic in 1:NTotalGrid) {
          if (NLableGrid[i] == GridIndic) {

            dx_fun <- function() {
              dx <- rep(0, NTotalpost)
              idx <- which(NewLabelGrid[, GridIndic] != 0 & InfTime <= t & (InfTime + InfPeriod) >= t & InfTime != 0)
              dx[idx] <- NInf[idx] * exp(alphaT + CovInf[idx, ] %*% BetaCovInf) * Dist[i, idx]^(-delta)
              sum(dx[is.finite(dx)])
            }
            if (ExpoTime[i] > t | ExpoTime[i] == 0) {
              dx <- dx_fun()

              F8[i, t, GridIndic] <- 1 - (1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus + phi[GridIndic] + CovSusReInf[GridIndic, ] %*% BetaCovSusReInf) * dx))
            }
            if (is_exposed(ExpoTime, IncPeriod, t, i)) {
              dx <- dx_fun()
              F8[i, t, GridIndic] <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus + phi[GridIndic] + CovSusReInf[GridIndic, ] %*% BetaCovSusReInf) * dx)
            }
          }
        }
      }
    }
    F9 <- sapply(1:NTotalGrid, function(GridIndic) {
      F10 <-
        prod(sapply(1:NTotalpost, function(i) round(prod(F8[i, , GridIndic][F8[i, , GridIndic] > 0]), 10))[which(sapply(1:NTotalpost, function(i) round(prod(F8[i, , GridIndic][F8[i, , GridIndic] > 0]), 10)) != 1)][sapply(1:NTotalpost, function(i) round(prod(F8[i, , GridIndic][F8[i, , GridIndic] > 0]), 10))[which(sapply(1:NTotalpost, function(i) round(prod(F8[i, , GridIndic][F8[i, , GridIndic] > 0]), 10)) != 1)] > 0])
    })
    return(F9)
  }
  alphaS=alphaS0
  delta=delta0
  tau1=tau0
  lambda1=lambda0
  BetaCovInf=BetaCovInf0
  BetaCovSus=BetaCovSus0
  BetaCovSusReInf=BetaCovSusReInf0
  alphaT=alphaT0
  Rnd <- matrix(0, NIterMC + 1, NTotalGrid)
  Sigma1 <- solve(tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid)))
  Rnd[1, ] <- mvrnorm(1, rep(0, NTotalGrid), Sigma1, tol = 1e-6)
  estfun=function(NLableGrid,Dist,alphaS,delta,lambda1,tau1,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT){
    for (L in 2:NIterMC) {
      phi <- mvrnorm(1, rep(0, NTotalGrid), Sigma1, tol = 1e-6)
      Q1 <- min(1, prod(F7(phi, alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) /
                              F7(Rnd[L - 1, ], alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)))
      Rnd[L, ] <- if (runif(1) < Q1) phi else Rnd[L - 1, ]
    }
    Av1 <- function(Rnd, GridIndic) mean(exp(Rnd[1:NIterMC, GridIndic]))
    calc_prob <- function(L, i, GridIndic, Rnd, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
      idx <- which(NewLabelGrid[, GridIndic] != 0 & InfTime <= t & (InfTime + InfPeriod) >= t & InfTime != 0)
      dx <- sum(NInf[idx] * exp(alphaT + CovInf[idx, ] %*% BetaCovInf) * Dist[i, idx]^(-delta), na.rm = TRUE)
      P1 <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus +
                                      CovSusReInf[GridIndic, ] %*% BetaCovSusReInf + Rnd[L, GridIndic]) * dx)
      return(P1)
    }
    Av2 <- function(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L) {
      P11 <- calc_prob(L, i, GridIndic, Rnd, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
      if (P11 == 0) return(0)
      (1 - P11) / P11 * exp(Rnd[L, GridIndic])
    }
    Av3 <- function(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L) {
      P11 <- calc_prob(L, i, GridIndic, Rnd, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
      if (P11 == 0) return(0)
      (1 - P11) / P11^2 * exp(2 * Rnd[L, GridIndic])
    }
    S1 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          -Pop[i] * as.numeric(F1(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                                    BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                                 Av1(Rnd, GridIndic))
        } else 0
      }), na.rm = TRUE)
    }), na.rm = TRUE)

    S2 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (is_exposed(ExpoTime, IncPeriod, t, i)) {
          SA4 <- sapply(1:NIterMC, function(L) {
            Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                  BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
          })
          Pop[i] * as.numeric(F1(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                                   BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                                mean(SA4, na.rm = TRUE))
        } else 0
      }), na.rm = TRUE)
    }), na.rm = TRUE)
    S3 <- S1 + S2
    S4=rep(0,MaxTimePand)
    for(t in 1:MaxTimePand){
      S44=rep(0,NTotalpost)
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if (NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              S444=c()
              for(L in 1:NIterMC){
                S444[L]=Av3(NLableGrid,Rnd,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              S44[i]=-Pop[i]^2*as.numeric((F1(NLableGrid,Dist,alphaS,delta,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT))^2*mean(S444))
            }
          }
        }
      }
      S4[t]=sum(S44)
    }
    S5=sum(S4,na.rm=T)
    S6=S3+S5
    EstAlphaS=alphaS-S3/S6
    S7 <- array(0, c(DimCovSus, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          S7[,,t] <- S7[,,t] - Pop[i] * CovSus[GridIndic, , drop = FALSE] *
            as.numeric(F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                         Av1(Rnd, GridIndic))
        }
      }
    }
    S8 <- apply(S7, c(1,2), sum)
    S9 <- array(0, c(DimCovSus, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          S99 <- sapply(1:NIterMC, function(L)
            Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                  i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))
          S9[,,t] <- S9[,,t] + Pop[i] * CovSus[GridIndic, , drop = FALSE] *
            as.numeric(F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                         mean(S99))
        }
      }
    }
    S10 <- apply(S9, c(1,2), sum, na.rm = TRUE)
    S11 <- S8 + S10
    S12 <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    S13 <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    S14  <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    CovMatrices <- lapply(1:NTotalGrid, function(g) {
      cov_vec <- matrix(CovSus[g, ], ncol = 1)
      cov_vec %*% t(cov_vec)
    })
    for (t in 1:MaxTimePand) {
      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          contrib <- -Pop[i] * as.numeric(
            F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                 BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
              Av1(Rnd, GridIndic)
          )
          S12[,,t] <- S12[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }
      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (is_exposed(ExpoTime, IncPeriod, t, i)) {
          S15 <- numeric(NIterMC)
          for (L in 1:NIterMC) {
            S15[L] <- Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                             i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
          }
          contrib <- Pop[i] * as.numeric(
            F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                 BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
              mean(S15)
          )
          S13[,,t] <- S13[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }
      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (is_exposed(ExpoTime, IncPeriod, t, i)) {
          S16 <- numeric(NIterMC)
          for (L in 1:NIterMC) {
            S16[L] <- Av3(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                             i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
          }
          contrib <- -Pop[i]^2 * as.numeric(
            (F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                  BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)^2) * mean(S16)
          )
          S14[,,t] <- S14[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }
    }
    S17 <- apply(S12, c(1,2), sum)
    S18 <- apply(S13, c(1,2), sum, na.rm = TRUE)
    S19  <- apply(S14, c(1,2), sum, na.rm = TRUE)
    S20  <- S17 + S18 + S19 + 0.01
    epsilon <- 1e-3
    if (det(S20) < epsilon) {
      S20 <- S20 + diag(epsilon, nrow(S20))
    }
    EstBetaCovSus <- BetaCovSus - solve(S20) %*% S11
    S21 <- array(0, c(DimCovSusReInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      S22 <- array(0, c(DimCovSusReInf, 1, NTotalpost))
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          contrib <- -Pop[i] * CovSusReInf[GridIndic,] *
            as.numeric(F2(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                 BetaCovInf, EstBetaCovSus, BetaCovSusReInf, alphaT) *
                         Av1(Rnd, GridIndic))
          S22[,,i] <- contrib
        }
      }
      S21[,,t] <- apply(S22, c(1,2), sum)
    }
    S23 <- apply(S21, c(1,2), sum, na.rm = TRUE)
    S24 <- array(0, c(DimCovSusReInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      S25 <- array(0, c(DimCovSusReInf, 1, NTotalpost))
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          S26 <- sapply(1:NIterMC, function(L) Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                                                      i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))
          contrib <- Pop[i] * CovSusReInf[GridIndic,] *
            as.numeric(F2(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                 BetaCovInf, EstBetaCovSus, BetaCovSusReInf, alphaT) *
                         mean(S26))
          S25[,,i] <- contrib
        }
      }
      S24[,,t] <- apply(S25, c(1,2), sum, na.rm = TRUE)
    }
    S27 <- apply(S24, c(1,2), sum, na.rm = TRUE)
    S28 <- S23 + S27
    S29 <- array(0, c(DimCovSusReInf, DimCovSusReInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      S30 <- array(0, c(DimCovSusReInf, DimCovSusReInf, NTotalpost))
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          contrib <- -Pop[i] * CovSusReInf[GridIndic,] %*% t(CovSusReInf[GridIndic,]) *
            as.numeric(F2(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                 BetaCovInf, EstBetaCovSus, BetaCovSusReInf, alphaT) *
                         Av1(Rnd, GridIndic))
          S30[,,i] <- contrib
        }
      }
      S29[,,t] <- apply(S30, c(1,2), sum)
    }
    S31 <- apply(S29, c(1,2), sum, na.rm = TRUE)
    S32 <- array(0, c(DimCovSusReInf, DimCovSusReInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      S33 <- array(0, c(DimCovSusReInf, DimCovSusReInf, NTotalpost))
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          S34 <- sapply(1:NIterMC, function(L) Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                                                       i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))
          contrib <- Pop[i] * CovSusReInf[GridIndic,] %*% t(CovSusReInf[GridIndic,]) *
            as.numeric(F2(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                 BetaCovInf, EstBetaCovSus, BetaCovSusReInf, alphaT) *
                         mean(S34))
          S33[,,i] <- contrib
        }
      }
      S32[,,t] <- apply(S33, c(1,2), sum, na.rm = TRUE)
    }
    S35 <- apply(S32, c(1,2), sum, na.rm = TRUE)
    S36 <- array(0, c(DimCovSusReInf, DimCovSusReInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      S37 <- array(0, c(DimCovSusReInf, DimCovSusReInf, NTotalpost))
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          S38 <- sapply(1:NIterMC, function(L) Av3(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                                                       i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))
          contrib <- -Pop[i]^2 * CovSusReInf[GridIndic,] %*% t(CovSusReInf[GridIndic,]) *
            as.numeric((F2(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                  BetaCovInf, EstBetaCovSus, BetaCovSusReInf, alphaT))^2 * mean(S38))
          S37[,,i] <- contrib
        }
      }
      S36[,,t] <- apply(S37, c(1,2), sum, na.rm = TRUE)
    }
    S39 <- apply(S36, c(1,2), sum, na.rm = TRUE)
    S40 <- S31 + S35 + S39 + 0.01
    epsilon <- 1e-3
    if(det(S40) < epsilon){
      S40 <- S40 + diag(epsilon, nrow(S40))
    }
    EstBetaCovSusReInf <- BetaCovSusReInf - solve(S40) %*% S28
    contrib_time <- function(i, t) {
      GridIndic <- NLableGrid[i]
      if (ExpoTime[i] > t | ExpoTime[i] == 0) {
        return(-Pop[i] * as.numeric(
          F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
               BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT) *
            Av1(Rnd, GridIndic)
        ))
      } else if (is_exposed(ExpoTime, IncPeriod, t, i)) {
        IA <- sapply(1:NIterMC, function(L)
          Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
        )
        return(Pop[i] * as.numeric(
          F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
               BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT) * mean(IA)
        ))
      } else return(0)
    }
    T1 <- sapply(1:MaxTimePand, function(t)
      sum(sapply(1:NTotalpost, contrib_time, t = t), na.rm = TRUE)
    )
    T2 <- sum(T1, na.rm = TRUE)
    T3 <- T2 + sum(T1, na.rm = TRUE)
    T4 <- rep(0, MaxTimePand)
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          T4[t] <- T4[t] - Pop[i]^2 * as.numeric(
            (F1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                  BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT))^2 *
              mean(sapply(1:NIterMC, function(L) Av3(NLableGrid, Rnd, Dist, alphaS, delta,
                                                       lambda1, i, GridIndic, t,
                                                       BetaCovInf, BetaCovSus, BetaCovSusReInf,
                                                       alphaT, L)))
          )
        }
      }
    }
    T5 <- sum(T4, na.rm = TRUE)
    T6 <- T3 + T5
    EstAlphaT <- alphaT - T3 / T6
    T7 <- array(0, c(DimCovInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          T7[,,t] <- T7[,,t] - Pop[i] * F3(NLableGrid, Dist, EstAlphaS, delta,
                                             i, GridIndic, t, BetaCovInf,
                                             EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            Av1(Rnd, GridIndic)
        }
      }
    }
    T8 <- apply(T7, c(1,2), sum, na.rm = TRUE)
    T9 <- array(0, c(DimCovInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          T9[,,t] <- T9[,,t] + Pop[i] * F3(NLableGrid, Dist, EstAlphaS, delta,
                                             i, GridIndic, t, BetaCovInf,
                                             EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            mean(sapply(1:NIterMC, function(L)
              Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    T10 <- apply(T9, c(1,2), sum, na.rm = TRUE)
    T11 <- T8 + T10
    T12 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          T12[,,t] <- T12[,,t] - Pop[i] * F4(NLableGrid, Dist, EstAlphaS, delta,
                                                   i, GridIndic, t, BetaCovInf,
                                                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            as.numeric(Av1(Rnd, GridIndic))
        }
      }
    }
    T13 <- apply(T12, c(1,2), sum, na.rm = TRUE)
    T14 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          T14[,,t] <- T14[,,t] + Pop[i] * F4(NLableGrid, Dist, EstAlphaS, delta,
                                                   i, GridIndic, t, BetaCovInf,
                                                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            mean(sapply(1:NIterMC, function(L)
              Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    T15 <- apply(T14, c(1,2), sum, na.rm = TRUE)
    T16 <- T13 + T15
    T17 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          T17[,,t] <- T17[,,t] - Pop[i]^2 *
            F3(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t, BetaCovInf,
                 EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) %*%
            t(F3(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t, BetaCovInf,
                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT)) *
            mean(sapply(1:NIterMC, function(L)
              Av3(NLableGrid, Rnd, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    T18 <- apply(T17, c(1,2), sum, na.rm = TRUE)
    T19 <- T16 + T18
    EstBetaCovInf <- BetaCovInf - solve(T19) %*% T11
    T20 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          Pop[i] * as.numeric(
            F5(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              Av1(Rnd, GridIndic)
          )
        } else 0
      }), na.rm = TRUE)
    }))
    T21 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (is_exposed(ExpoTime, IncPeriod, t, i)) {
          -Pop[i] * as.numeric(
            F5(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              mean(sapply(1:NIterMC, function(L)
                Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic,
                      t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
              ))
          )
        } else 0
      }))
    }))
    T22 <- T20 + T21
    T23 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          -Pop[i] * as.numeric(
            F6(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              Av1(Rnd, GridIndic)
          )
        } else 0
      }))
    }))
    T24 <- rep(0, MaxTimePand)
    for(t in 1:MaxTimePand){
      T24[t] <- sum(sapply(1:NTotalpost, function(i){
        sum(sapply(1:NTotalGrid, function(GridIndic){
          if(NLableGrid[i] == GridIndic && ExpoTime[i] <= t && (ExpoTime[i] + IncPeriod[i]) > t && ExpoTime[i] != 0){
            Pop[i]*as.numeric(F6(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                       EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
                                mean(sapply(1:NIterMC, function(L)
                                  Av2(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                                        BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)))) -
              Pop[i]^2*as.numeric((F5(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                            EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT))^2 *
                                    mean(sapply(1:NIterMC, function(L)
                                      Av3(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))))
          } else 0
        }))
      }))
    }
    T25 <- sum(T24, na.rm = TRUE)
    T26 <- T23 + T25
    Estdelta <- delta - T22 / T26
    LGL1 <- function(par) {
      lambda1 <- par[1]
      tau1 <- par[2]
      Sigma <- tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid))
      -mean(sapply(1:NIterMC, function(L) dmvnorm(Rnd[L,], rep(0, NTotalGrid), sigma = Sigma, log = TRUE)))
    }
    EstU1 <- optim(c(lambda1, tau1), fn = LGL1)$par
    EstGammau <- EstU1[1]
    HatSigmmaU <- EstU1[2]
    result=list(Rnd=Rnd,BetaCovInf=EstBetaCovInf,BetaCovSus=EstBetaCovSus,BetaCovSusReInf=EstBetaCovSusReInf,Uhat=EstU1,alphaS=EstAlphaS,alphaT=EstAlphaT,delta=Estdelta,tau1=HatSigmmaU,lambda1=EstGammau)
    result
  }
  O1=numeric()
  LGL1 <- function(NLableGrid, rndeft, Dist, alphaS, delta, lambda1, tau1,
                     BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    O2 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        sum(sapply(1:NTotalGrid, function(GridIndic) {
          if (NLableGrid[i] == GridIndic && (ExpoTime[i] > t || ExpoTime[i] == 0)) {
            d1 <- mean(exp(rndeft[, GridIndic]))
            -Pop[i] * F1(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                           BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) * d1
          } else 0
        }))
      }))
    }))
    O3 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        sum(sapply(1:NTotalGrid, function(GridIndic) {
          if (NLableGrid[i] == GridIndic &&
              ExpoTime[i] <= t && (ExpoTime[i] + IncPeriod[i]) > t && ExpoTime[i] != 0) {
            mean(sapply(1:NIterMC, function(L) {
              dx <- sum(sapply(1:NTotalpost, function(j) {
                if (NewLabelGrid[j, GridIndic] != 0 &&
                    InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
                  NInf[j] * exp(alphaT + CovInf[j, ] %*% BetaCovInf) * Dist[i, j]^(-delta)
                } else 0
              }))
              P11 <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus +
                                               CovSusReInf[GridIndic, ] %*% BetaCovSusReInf +
                                               rndeft[L, GridIndic]) * dx)
              ifelse(P11 == 0, 0, log(P11))
            }))
          } else 0
        }))
      }))
    }))
    O4 <- mean(sapply(1:NIterMC, function(L) {
      dmvnorm(rndeft[L, ], rep(0, NTotalGrid),
              sigma = tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid)),
              log = TRUE)
    }))
    O4 + O3 + O2
  }
  est0=estfun(NLableGrid,Dist,alphaS0,delta0,lambda0,tau0,BetaCovInf0,BetaCovSus0,BetaCovSusReInf0,alphaT0)
  alphaS=est0$alphaS
  delta=est0$delta
  lambda1=est0$lambda1
  tau1=est0$tau1
  BetaCovInf=est0$BetaCovInf
  BetaCovSus=est0$BetaCovSus
  BetaCovSusReInf=est0$BetaCovSusReInf
  alphaT=est0$alphaT
  Uhat=est0$Uhat
  rndeft=est0$Rnd
  AIC <- numeric()
  mes <- numeric()
  tolerance <- 0.1
  for (crtr in 1:NIterMCECM) {
    est <- estfun(NLableGrid, Dist, alphaS, delta, lambda1, tau1,
                  BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
    list2env(est, envir = environment())
    O1[crtr] <- LGL1(NLableGrid, Rnd, Dist, alphaS, delta, lambda1, tau1,
                     BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
    AIC[crtr] <- -2 * O1[crtr] + 11
    current_params <- c(alphaS, BetaCovInf, BetaCovSus, BetaCovSusReInf,
                        alphaT, delta, tau1, lambda1)
    mes <- if(crtr > 1) sqrt(sum((current_params - prev_params)^2)) else Inf
    prev_params <- current_params
    if(mes < tolerance) {
      message("MCECM Converged at iteration ", crtr, " with parameter change: ", mes)
      break
    }
    if(crtr %% 10 == 0) message("Iteration ", crtr, ": Parameter change = ", mes)
  }
  out1 <- list(
    alphaS = alphaS,
    BetaCovInf = BetaCovInf,
    BetaCovSus = BetaCovSus,
    BetaCovSusReInf = BetaCovSusReInf,
    alphaT = alphaT,
    delta = delta,
    tau1 = tau1,
    lambda1 = lambda1,
    AIC = AIC[crtr]
  )
  out1
}
