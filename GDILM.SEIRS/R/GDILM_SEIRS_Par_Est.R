#' GDILM SEIRS for Real Data
#'
#' This function applies the Geographically Dependent Individual Level Model (GDILM) for infectious disease transmission, incorporating reinfection dynamics within the Susceptible-Exposed-Infectious-Recovered-Susceptible (SEIRS) framework, to real data. It employs a likelihood based Monte Carlo Expectation Conditional Maximization (MCECM) algorithm for parameter estimation and AIC calculation. This function requires two dataframes, named `data` and `adjacency_matrix`, along with the necessary parameters. Detailed information on the structure of these two datasets is provided in the package.
#' @param data Dataset. The dataset should exactly match the `data` file in the data folder, including all the columns with the same names.
#' @param adjacency_matrix Adjacency matrix representing the regions in the study area (0 if no connection between regions)
#' @param DimCovInf Dimensions of the individual infectivity covariate
#' @param DimCovSus Dimensions of the area-level susceptibility to initial infection covariate
#' @param DimCovSusReInf Dimensions of the area-level susceptibility to reinfection covariate
#' @param tau0 Initial value for spatial precision
#' @param lambda0 Initial value for spatial dependence
#' @param alphaS0 Initial value for the susceptibility intercept
#' @param delta0 Initial value for the spatial decay parameter
#' @param alphaT0 Initial value for the infectivity intercept
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
#'
#' @examples
#' \donttest{
#' data(data)
#' data(adjacency_matrix)
#' GDILM_SEIRS_Par_Est(data,adjacency_matrix,2,2,2,0.5, 0.5, 1, 2, 1, 1, 1, 20, 2)
#' }
#'
GDILM_SEIRS_Par_Est=function(data,adjacency_matrix,DimCovInf,DimCovSus,DimCovSusReInf,tau0, lambda0, alphaS0, delta0, alphaT0,InfPrd, IncPrd, NIterMC, NIterMCECM){
  if(lambda0>1) stop("The spatial dependence parameter should be restricted to a range between 0 and 1.")
  if(lambda0==0) stop("Absence of spatial dependence: This model is designed for scenarios where spatial dependence is present.")
  if(delta0<0) stop("The spatial decay parameter must be greater than zero.")
  if(NIterMC<2) stop("The number of iterations must exceed 2.")
  if(InfPrd<0) stop("The infectious period must be greater than zero.")
  if(IncPrd<0) stop("The incubation period must be greater than zero.")
  if(DimCovInf<0) stop("Dimensions of the individual infectivity covariate must be greater than zero.")
  if(DimCovSus<0) stop("Dimensions of the area-level susceptibility to initial infection covariate must be greater than zero.")
  if(DimCovSusReInf<0) stop("Dimensions of the area-level susceptibility to reinfection covariate must be greater than zero.")
  NTotalpost=nrow(data)
  NTotalGrid=length(unique(data$Label_NC_shape))
  NAllPostPerGrid <- table(data$Label_NC_shape)
  MaxTimePand=max(ceiling(data[,8]))+15
  Lat=data[,1]
  Long=data[,2]
  D=-1*(adjacency_matrix)
  diag(D)=colSums(adjacency_matrix)
  NLableGrid=as.numeric(as.vector(data[,9]))
  NewLabelGrid=matrix(0,NTotalpost,NTotalGrid)
  for(RHAD in 1:NTotalGrid){
    for(i in 1:NTotalpost){
      if(D[NLableGrid[i],RHAD]!=0){
        NewLabelGrid[i,RHAD]=-1
      }
    }
  }
  Dist=matrix(0,NTotalpost,NTotalpost)
  for(i in 1:NTotalpost){
    for(j in 1:NTotalpost){
      Dist[i,j]=sqrt((Lat[i]-Lat[j])^2+(Long[i]-Long[j])^2)
    }
  }
  Dist=Dist*50
  Pop=data[,3]
  NInf=data[,4]
  NReInf=data[data$status==1,4]
  Sigma0=solve(tau0^2*(lambda0*D+(1-lambda0)*diag(NTotalGrid)))
  rnd=mvrnorm(1, rep(0,NTotalGrid), Sigma0, tol = 1e-6)
  CovInf=as.matrix(data[,c(6,7)])
  dimnames(   CovInf) <- NULL
  CovSus=as.matrix(data[, c(10, 11)])
  dimnames( CovSus) <- NULL
  CovSusReInf=as.matrix(data[, c(10, 11)])
  dimnames(CovSusReInf) <- NULL
  BetaCovInf0=rep(1,DimCovInf)
  BetaCovSus0=rep(1,DimCovSus)
  BetaCovSusReInf0=rep(1,DimCovSusReInf)
  InfPeriod=rep(InfPrd,NTotalpost)
  IncPeriod=rep(IncPrd,NTotalpost)
  ExpoTime=ceiling(data[,8])
  ExpoTimeReInf=ceiling(data[data$status==1,8])
  InfTime=ExpoTime+IncPeriod[1]
  ReInfTime= ExpoTimeReInf+IncPeriod[1]
  is_exposed <- function(ExpoTime, IncPeriod, t, i) {
    ExpoTime[i] <= t &
      (ExpoTime[i] + IncPeriod[i]) > t &
      ExpoTime[i] != 0
  }
  replace_nonfinite <- function(x, value = 0) {
    x[!is.finite(x)] <- value
    return(x)
  }
  FN1 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                   BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN11 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        FN11[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta)
      }
    }
    FN11<- replace_nonfinite(FN11)
    return(sum(FN11) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  FN2=function(NLableGrid,Dist,alphaS,delta,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT){
    FN22=rep(0,NTotalpost)
    for(j in 1:NTotalpost){
      if(data[j, 12] == 0){
        if (NewLabelGrid[j,GridIndic]!=0){
          if(ReInfTime[j]<=t & (ReInfTime[j]+InfPeriod[j])>=t & ReInfTime[j]!=0){
            FN22[j]=NReInf[j]*exp(alphaT+CovInf[j,]%*%BetaCovInf)*Dist[i,j]^(-delta)
          }
        }
      }
      FN22=replace(FN22,is.infinite(FN22),0)
      FN222=sum(FN22)
      FN2222=exp(alphaS+CovSus[GridIndic,]%*%BetaCovSus+CovSusReInf[GridIndic,]%*%BetaCovSusReInf)*FN222
      return(FN2222)
    }
  }
  FN3 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                   BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN33 <- array(0, c(DimCovInf, 1, NTotalpost))
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        FN33[,,j] <- NInf[j] * CovInf[j,] * as.numeric(exp(alphaT + CovInf[j,] %*% BetaCovInf)) * Dist[i, j]^(-delta)
      }
    }
    FN33<- replace_nonfinite(FN33)
    return(apply(FN33, c(1,2), sum, na.rm = TRUE) *
             as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  FN4 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN5 <- array(0, c(DimCovInf, DimCovInf, NTotalpost))
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        FN5[,,j] <- NInf[j] * CovInf[j,] %*% t(CovInf[j,]) * as.numeric(exp(alphaT + CovInf[j,] %*% BetaCovInf)) * Dist[i, j]^(-delta)
      }
    }
    FN5<- replace_nonfinite(FN5)
    return(apply(FN5, c(1,2), sum, na.rm = TRUE) *
             as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  FN6 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN66 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        FN66[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta) * log(Dist[i, j])
      }
    }
    FN66<- replace_nonfinite(FN66)
    return(sum(FN66) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }

  FN7 <- function(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                       BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN77 <- rep(0, NTotalpost)
    for (j in 1:NTotalpost) {
      if (NewLabelGrid[j, GridIndic] != 0 && InfTime[j] <= t && (InfTime[j] + InfPeriod[j]) >= t && InfTime[j] != 0) {
        FN77[j] <- NInf[j] * exp(alphaT + CovInf[j,] %*% BetaCovInf) * Dist[i, j]^(-delta) * (log(Dist[i, j]))^2
      }
    }
    FN77<- replace_nonfinite(FN77)
    return(sum(FN77) * as.numeric(exp(alphaS + CovSus[GridIndic,] %*% BetaCovSus + CovSusReInf[GridIndic,] %*% BetaCovSusReInf)))
  }
  FN8 <- function(rnd, alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FN9 <- array(0, c(NTotalpost, MaxTimePand, NTotalGrid))
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
              FN9[i, t, GridIndic] <- 1 - (1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus + rnd[GridIndic] + CovSusReInf[GridIndic, ] %*% BetaCovSusReInf) * dx))
            }
            if (ExpoTime[i] <= t & (ExpoTime[i] + IncPeriod[i]) > t & ExpoTime[i] != 0) {
              dx <- dx_fun()
              FN9[i, t, GridIndic] <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus + rnd[GridIndic] + CovSusReInf[GridIndic, ] %*% BetaCovSusReInf) * dx)
            }
          }
        }
      }
    }
    FN10 <- sapply(1:NTotalGrid, function(GridIndic) {
      FN11 <-
        prod(sapply(1:NTotalpost, function(i) round(prod(FN9[i, , GridIndic][FN9[i, , GridIndic] > 0]), 10))[which(sapply(1:NTotalpost, function(i) round(prod(FN9[i, , GridIndic][FN9[i, , GridIndic] > 0]), 10)) != 1)][sapply(1:NTotalpost, function(i) round(prod(FN9[i, , GridIndic][FN9[i, , GridIndic] > 0]), 10))[which(sapply(1:NTotalpost, function(i) round(prod(FN9[i, , GridIndic][FN9[i, , GridIndic] > 0]), 10)) != 1)] > 0])
    })
    return(FN10)
  }
  alphaS=alphaS0
  delta=delta0
  tau1=tau0
  lambda1=lambda0
  BetaCovInf=BetaCovInf0
  BetaCovSus=BetaCovSus0
  BetaCovSusReInf=BetaCovSusReInf0
  alphaT=alphaT0
  rndmef <- matrix(0, NIterMC + 1, NTotalGrid)
  Sigma1 <- solve(tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid)))
  rndmef[1, ] <- mvrnorm(1, rep(0,NTotalGrid), Sigma1, tol = 1e-6)
  estfun=function(NLableGrid,Dist,alphaS,delta,lambda1,tau1,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT){
    for (L in 2:NIterMC) {
      rnd <- mvrnorm(1, rep(0,NTotalGrid), Sigma1, tol = 1e-6)
      FN20 <- min(1, prod(FN8(rnd, alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) /
                              FN8(rndmef[L - 1, ], alphaS, delta, lambda1, tau1, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)))
      rndmef[L, ] <- if (runif(1) < FN20) rnd else rndmef[L - 1, ]
    }
    AV1 <- function(rndmef, GridIndic) mean(exp(rndmef[1:NIterMC, GridIndic]))
    calc_prob <- function(L, i, GridIndic, rndmef, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
      idx <- which(NewLabelGrid[, GridIndic] != 0 & InfTime <= t & (InfTime + InfPeriod) >= t & InfTime != 0)
      dx <- sum(NInf[idx] * exp(alphaT + CovInf[idx, ] %*% BetaCovInf) * Dist[i, idx]^(-delta), na.rm = TRUE)
      PRB1 <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus +
                                      CovSusReInf[GridIndic, ] %*% BetaCovSusReInf + rndmef[L, GridIndic]) * dx)
      return(PRB1)
    }
    AV2 <- function(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L) {
      PROB <- calc_prob(L, i, GridIndic, rndmef, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
      if (PROB == 0) return(0)
      (1 - PROB) / PROB * exp(rndmef[L, GridIndic])
    }
    AV3 <- function(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L) {
      PROB <- calc_prob(L, i, GridIndic, rndmef, alphaS, delta, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
      if (PROB == 0) return(0)
      (1 - PROB) / PROB^2 * exp(2 * rndmef[L, GridIndic])
    }
    FQ1 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          -Pop[i] * as.numeric(FN1(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                                    BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                                 AV1(rndmef, GridIndic))
        } else 0
      }), na.rm = TRUE)
    }), na.rm = TRUE)
    FQ2=rep(0,MaxTimePand)
    for(t in 1:MaxTimePand){
      FQ3=rep(0,NTotalpost)
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if(NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              FQ4=c()
              for(L in 1:NIterMC){
                FQ4[L]=AV2(NLableGrid,rndmef,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              FQ3[i]=Pop[i]*as.numeric(FN1(NLableGrid,Dist,alphaS,delta,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT)*mean(FQ4))
            }
          }
        }
      }
      FQ2[t]=sum(FQ3)
    }
    FQ5=sum(FQ2,na.rm=T)
    FQ6=FQ1+FQ5
    FQ7=rep(0,MaxTimePand)
    for(t in 1:MaxTimePand){
      FQ8=rep(0,NTotalpost)
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if (NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              FQ9=c()
              for(L in 1:NIterMC){
                FQ9[L]=AV3(NLableGrid,rndmef,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              FQ8[i]=-Pop[i]^2*as.numeric((FN1(NLableGrid,Dist,alphaS,delta,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT))^2*mean(FQ9))
            }
          }
        }
      }
      FQ7[t]=sum(FQ8)
    }
    FQ10=sum(FQ7,na.rm=T)
    FQ11=FQ6+FQ10
    EstAlphaS=alphaS-FQ6/FQ11
    FQ12 <- array(0, c(DimCovSus, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          FQ12[,,t] <- FQ12[,,t] - Pop[i] * CovSus[GridIndic, , drop = FALSE] *
            as.numeric(FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                         AV1(rndmef, GridIndic))
        }
      }
    }
    FQ13 <- apply(FQ12, c(1,2), sum)
    FQ14 <- array(0, c(DimCovSus, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] <= t & (ExpoTime[i] + IncPeriod[i]) > t & ExpoTime[i] != 0){
          FQ15 <- sapply(1:NIterMC, function(L)
            AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                  i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))
          FQ14[,,t] <- FQ14[,,t] + Pop[i] * CovSus[GridIndic, , drop = FALSE] *
            as.numeric(FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
                         mean(FQ15))
        }
      }
    }
    FQ16 <- apply(FQ14, c(1,2), sum, na.rm = TRUE)
    FQ17 <- FQ13 + FQ16
    FQ18 <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    FQ19 <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    FQ20  <- array(0, c(DimCovSus, DimCovSus, MaxTimePand))
    CovMatrices <- lapply(1:NTotalGrid, function(g) {
      cov_vec <- matrix(CovSus[g, ], ncol = 1)
      cov_vec %*% t(cov_vec)
    })

    for (t in 1:MaxTimePand) {
      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          contrib <- -Pop[i] * as.numeric(
            FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                 BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
              AV1(rndmef, GridIndic)
          )
          FQ18[,,t] <- FQ18[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }
      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] <= t & (ExpoTime[i] + IncPeriod[i]) > t & ExpoTime[i] != 0) {
          FQ21 <- numeric(NIterMC)
          for (L in 1:NIterMC) {
            FQ21[L] <- AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                             i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
          }
          contrib <- Pop[i] * as.numeric(
            FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                 BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) *
              mean(FQ21)
          )
          FQ19[,,t] <- FQ19[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }

      for (i in 1:NTotalpost) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] <= t & (ExpoTime[i] + IncPeriod[i]) > t & ExpoTime[i] != 0) {
          FQ22 <- numeric(NIterMC)
          for (L in 1:NIterMC) {
            FQ22[L] <- AV3(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                             i, GridIndic, t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
          }
          contrib <- -Pop[i]^2 * as.numeric(
            (FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                  BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)^2) * mean(FQ22)
          )
          FQ20[,,t] <- FQ20[,,t] + CovMatrices[[GridIndic]] * contrib
        }
      }
    }
    FQ23 <- apply(FQ18, c(1,2), sum)
    FQ24 <- apply(FQ19, c(1,2), sum, na.rm = TRUE)
    FQ25  <- apply(FQ20, c(1,2), sum, na.rm = TRUE)
    FQ26  <- FQ23 + FQ24 + FQ25 + 0.01
    epsilon <- 1e-3
    if (det(FQ26) < epsilon) {
      FQ26 <- FQ26 + diag(epsilon, nrow(FQ26))
    }
    EstBetaCovSus <- BetaCovSus - solve(FQ26) %*% FQ17
    J1=array(0,c(DimCovSusReInf,1,MaxTimePand))
    for(t in 1:MaxTimePand){
      J2=array(0,c(DimCovSusReInf,1,NTotalpost))
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if (NLableGrid[i]==GridIndic){
            if(ExpoTime[i]>t|ExpoTime[i]==0){
              J2[,,i]=-Pop[i]*CovSusReInf[GridIndic,]*as.numeric(FN2(NLableGrid,Dist,EstAlphaS,delta,i,GridIndic,t,BetaCovInf,EstBetaCovSus,BetaCovSusReInf,alphaT)*AV1(rndmef,GridIndic))
            }
          }
        }
      }
      J1[,,t]=apply(J2,c(1,2),sum)
    }
    J3=apply(J1,c(1,2),sum)
    J4=array(0,c(DimCovSusReInf,1,MaxTimePand))
    for(t in 1:MaxTimePand){
      J5=array(0,c(DimCovSusReInf,1,NTotalpost))
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if(NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              YA4r=c()
              for(L in 1:NIterMC){
                YA4r[L]=AV2(NLableGrid,rndmef,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              J5[,,i]=Pop[i]*CovSusReInf[GridIndic,]*as.numeric(FN2(NLableGrid,Dist,EstAlphaS,delta,i,GridIndic,t,BetaCovInf,EstBetaCovSus,BetaCovSusReInf,alphaT)*mean(YA4r))
            }
          }
        }
      }
      J4[,,t]=apply(J5,c(1,2),sum,na.rm=T)
    }
    J6=apply(J4,c(1,2),sum)
    J7=J3+J6
    J8=array(0,c(DimCovSusReInf,DimCovSusReInf,MaxTimePand))
    for(t in 1:MaxTimePand){
      J9=array(0,c(DimCovSusReInf,DimCovSusReInf,NTotalpost))
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if (NLableGrid[i]==GridIndic){
            if(ExpoTime[i]>t|ExpoTime[i]==0){
              J9[,,i]=-Pop[i]*CovSusReInf[GridIndic,]%*%t(CovSusReInf[GridIndic,])*as.numeric(FN2(NLableGrid,Dist,EstAlphaS,delta,i,GridIndic,t,BetaCovInf,EstBetaCovSus,BetaCovSusReInf,alphaT)*AV1(rndmef,GridIndic))
            }
          }
        }
      }
      J8[,,t]=apply(J9,c(1,2),sum)
    }
    J10=apply(J8,c(1,2),sum)
    J11=array(0,c(DimCovSusReInf,DimCovSusReInf,MaxTimePand))
    for(t in 1:MaxTimePand){
      J12=array(0,c(DimCovSusReInf,DimCovSusReInf,NTotalpost))
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if(NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              J13=c()
              for(L in 1:NIterMC){
                J13[L]=AV2(NLableGrid,rndmef,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              J12[,,i]=Pop[i]*CovSusReInf[GridIndic,]%*%t(CovSusReInf[GridIndic,])*as.numeric(FN2(NLableGrid,Dist,EstAlphaS,delta,i,GridIndic,t,BetaCovInf,EstBetaCovSus,BetaCovSusReInf,alphaT)*mean(J13))
            }
          }
        }
      }
      J11[,,t]=apply(J12,c(1,2),sum)
    }
    J14=apply(J11,c(1,2),sum,na.rm=T)
    J15=array(0,c(DimCovSusReInf,DimCovSusReInf,MaxTimePand))
    for(t in 1:MaxTimePand){
      J16=array(0,c(DimCovSusReInf,DimCovSusReInf,NTotalpost))
      for(i in 1:NTotalpost){
        for(GridIndic in 1:NTotalGrid){
          if (NLableGrid[i]==GridIndic){
            if(is_exposed(ExpoTime, IncPeriod, t, i)){
              J17=c()
              for(L in 1:NIterMC){
                J17[L]=AV3(NLableGrid,rndmef,Dist,alphaS,delta,lambda1,i,GridIndic,t,BetaCovInf,BetaCovSus,BetaCovSusReInf,alphaT,L)
              }
              J16[,,i]=-Pop[i]^2*CovSusReInf[GridIndic,]%*%t(CovSusReInf[GridIndic,])*as.numeric((FN2(NLableGrid,Dist,EstAlphaS,delta,i,GridIndic,t,BetaCovInf,EstBetaCovSus,BetaCovSusReInf,alphaT))^2*mean(J17))
            }
          }
        }
      }
      J15[,,t]=apply(J16,c(1,2),sum,na.rm=T)
    }
    J18=apply(J15,c(1,2),sum)
    J19=J10+J14+J18
    epsilon <- 1e-3
    if (det(J19) < epsilon) {
      J19 <- J19 + diag(epsilon, nrow(J19))
    }
    EstBetaCovSusReInf=BetaCovSusReInf-solve(J19)%*%J7
    contrib_time <- function(i, t) {
      GridIndic <- NLableGrid[i]
      if (ExpoTime[i] > t | ExpoTime[i] == 0) {
        return(-Pop[i] * as.numeric(
          FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
               BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT) *
            AV1(rndmef, GridIndic)
        ))
      } else if (is_exposed(ExpoTime, IncPeriod, t, i)) {
        IA <- sapply(1:NIterMC, function(L)
          AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
        )
        return(Pop[i] * as.numeric(
          FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
               BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT) * mean(IA)
        ))
      } else return(0)
    }
    J20 <- sapply(1:MaxTimePand, function(t)
      sum(sapply(1:NTotalpost, contrib_time, t = t), na.rm = TRUE)
    )
    J21 <- sum(J20, na.rm = TRUE)
    J22 <- J21 + sum(J20, na.rm = TRUE)  # Actually same as J21 + TInfA3
    J23 <- rep(0, MaxTimePand)
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          J23[t] <- J23[t] - Pop[i]^2 * as.numeric(
            (FN1(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                  BetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, alphaT))^2 *
              mean(sapply(1:NIterMC, function(L) AV3(NLableGrid, rndmef, Dist, alphaS, delta,
                                                       lambda1, i, GridIndic, t,
                                                       BetaCovInf, BetaCovSus, BetaCovSusReInf,
                                                       alphaT, L)))
          )
        }
      }
    }
    J24 <- sum(J23, na.rm = TRUE)
    J25 <- J22 + J24
    EstAlphaT <- alphaT - J22 / J25
    KL1 <- array(0, c(DimCovInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          KL1[,,t] <- KL1[,,t] - Pop[i] * FN3(NLableGrid, Dist, EstAlphaS, delta,
                                             i, GridIndic, t, BetaCovInf,
                                             EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            AV1(rndmef, GridIndic)
        }
      }
    }
    KL2 <- apply(KL1, c(1,2), sum, na.rm = TRUE)
    KL3 <- array(0, c(DimCovInf, 1, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          KL3[,,t] <- KL3[,,t] + Pop[i] * FN3(NLableGrid, Dist, EstAlphaS, delta,
                                             i, GridIndic, t, BetaCovInf,
                                             EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            mean(sapply(1:NIterMC, function(L)
              AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    KL4 <- apply(KL3, c(1,2), sum, na.rm = TRUE)
    KL5 <- KL2 + KL4
    KL6 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(ExpoTime[i] > t | ExpoTime[i] == 0){
          KL6[,,t] <- KL6[,,t] - Pop[i] * FN4(NLableGrid, Dist, EstAlphaS, delta,
                                                   i, GridIndic, t, BetaCovInf,
                                                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            as.numeric(AV1(rndmef, GridIndic))
        }
      }
    }
    KL7 <- apply(KL6, c(1,2), sum, na.rm = TRUE)
    KL8 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          KL8[,,t] <- KL8[,,t] + Pop[i] * FN4(NLableGrid, Dist, EstAlphaS, delta,
                                                   i, GridIndic, t, BetaCovInf,
                                                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
            mean(sapply(1:NIterMC, function(L)
              AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    KL9 <- apply(KL8, c(1,2), sum, na.rm = TRUE)
    KL10 <- KL7 + KL9
    KL11 <- array(0, c(DimCovInf, DimCovInf, MaxTimePand))
    for(t in 1:MaxTimePand){
      for(i in 1:NTotalpost){
        GridIndic <- NLableGrid[i]
        if(is_exposed(ExpoTime, IncPeriod, t, i)){
          KL11[,,t] <- KL11[,,t] - Pop[i]^2 *
            FN3(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t, BetaCovInf,
                 EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) %*%
            t(FN3(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t, BetaCovInf,
                   EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT)) *
            mean(sapply(1:NIterMC, function(L)
              AV3(NLableGrid, rndmef, Dist, alphaS, delta, lambda1,
                    i, GridIndic, t, BetaCovInf, BetaCovSus,
                    BetaCovSusReInf, alphaT, L)
            ))
        }
      }
    }
    KL12 <- apply(KL11, c(1,2), sum, na.rm = TRUE)
    KL13 <- KL10 + KL12
    EstBetaCovInf <- BetaCovInf - solve(KL13) %*% KL5
    KL15 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          Pop[i] * as.numeric(
            FN6(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              AV1(rndmef, GridIndic)
          )
        } else 0
      }), na.rm = TRUE)
    }))
    KL16 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (is_exposed(ExpoTime, IncPeriod, t, i)) {
          -Pop[i] * as.numeric(
            FN6(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              mean(sapply(1:NIterMC, function(L)
                AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic,
                      t, BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)
              ))
          )
        } else 0
      }))
    }))
    KL17 <- KL15 + KL16
    KL18 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        GridIndic <- NLableGrid[i]
        if (ExpoTime[i] > t | ExpoTime[i] == 0) {
          -Pop[i] * as.numeric(
            FN7(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                     EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
              AV1(rndmef, GridIndic)
          )
        } else 0
      }))
    }))
    KL19 <- rep(0, MaxTimePand)
    for(t in 1:MaxTimePand){
      KL19[t] <- sum(sapply(1:NTotalpost, function(i){
        sum(sapply(1:NTotalGrid, function(GridIndic){
          if(NLableGrid[i] == GridIndic && ExpoTime[i] <= t && (ExpoTime[i] + IncPeriod[i]) > t && ExpoTime[i] != 0){
            Pop[i]*as.numeric(FN7(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                       EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT) *
                                mean(sapply(1:NIterMC, function(L)
                                  AV2(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                                        BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L)))) -
              Pop[i]^2*as.numeric((FN6(NLableGrid, Dist, EstAlphaS, delta, i, GridIndic, t,
                                            EstBetaCovInf, EstBetaCovSus, EstBetaCovSusReInf, EstAlphaT))^2 *
                                    mean(sapply(1:NIterMC, function(L)
                                      AV3(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, i, GridIndic, t,
                                            BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT, L))))
          } else 0
        }))
      }))
    }
    KL20 <- sum(KL19, na.rm = TRUE)
    KL21 <- KL18 + KL20
    Estdelta <- delta - KL17 / KL21
    LGLK1 <- function(par) {
      lambda1 <- par[1]
      tau1 <- par[2]
      Sigma <- tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid))
      -mean(sapply(1:NIterMC, function(L) dmvnorm(rndmef[L,], rep(0, NTotalGrid), sigma = Sigma, log = TRUE)))
    }
    EstU1 <- optim(c(lambda1, tau1), fn = LGLK1)$par
    EstGammau <- EstU1[1]
    HatSigmmaU <- EstU1[2]
    result=list(rndmef=rndmef,BetaCovInf=EstBetaCovInf,BetaCovSus=EstBetaCovSus,BetaCovSusReInf=EstBetaCovSusReInf,Uhat=EstU1,alphaS=EstAlphaS,alphaT=EstAlphaT,delta=Estdelta,tau1=HatSigmmaU,lambda1=EstGammau)
    result
  }
  LA=numeric()
  LLKH <- function(NLableGrid, rndmefEst, Dist, alphaS, delta, lambda1, tau1,
                     BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) {
    FNC1 <- sum(sapply(1:MaxTimePand, function(t) {
      sum(sapply(1:NTotalpost, function(i) {
        sum(sapply(1:NTotalGrid, function(GridIndic) {
          if (NLableGrid[i] == GridIndic && (ExpoTime[i] > t || ExpoTime[i] == 0)) {
            d1 <- mean(exp(rndmefEst[, GridIndic]))
            -Pop[i] * FN1(NLableGrid, Dist, alphaS, delta, i, GridIndic, t,
                           BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT) * d1
          } else 0
        }))
      }))
    }))
    FNC2 <- sum(sapply(1:MaxTimePand, function(t) {
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
              PROB <- 1 - exp(-Pop[i] * exp(alphaS + CovSus[GridIndic, ] %*% BetaCovSus +
                                               CovSusReInf[GridIndic, ] %*% BetaCovSusReInf +
                                               rndmefEst[L, GridIndic]) * dx)
              ifelse(PROB == 0, 0, log(PROB))
            }))
          } else 0
        }))
      }))
    }))

    FNC3 <- mean(sapply(1:NIterMC, function(L) {
      dmvnorm(rndmefEst[L, ], rep(0, NTotalGrid),
              sigma = tau1^2 * (lambda1 * D + (1 - lambda1) * diag(NTotalGrid)),
              log = TRUE)
    }))
    FNC3 + FNC2 + FNC1
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
  rndmefEst=est0$rndmef
  rndmefEst
  AIC <- numeric()
  mes <- numeric()
  tolerance <- 0.1
  for (NMB in 1:NIterMCECM) {
    est <- estfun(NLableGrid, Dist, alphaS, delta, lambda1, tau1,
                  BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
    list2env(est, envir = environment())
    LA[NMB] <- LLKH(NLableGrid, rndmef, Dist, alphaS, delta, lambda1, tau1,
                     BetaCovInf, BetaCovSus, BetaCovSusReInf, alphaT)
    AIC[NMB] <- -2 * LA[NMB] + 11
    current_params <- c(alphaS, BetaCovInf, BetaCovSus, BetaCovSusReInf,
                        alphaT, delta, tau1, lambda1)
    mes <- if(NMB > 1) sqrt(sum((current_params - prev_params)^2)) else Inf
    prev_params <- current_params
    if(mes < tolerance) {
      message("MCECM Converged at iteration ", NMB, " with parameter change: ", mes)
      break
    }
    if(NMB %% 10 == 0) message("Iteration ", NMB, ": Parameter change = ", mes)
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
    AIC = AIC[NMB]
  )
  out1
}
