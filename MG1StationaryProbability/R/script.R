beta <- 0.55

#' Service continuous density distribution
#'
#' @param t time value
#'
#' @return density function value (double) given \code{t}
#'
#' @export
#'
b <- function(t) {
  a<- 0
  if(t < 0 || t > 2*beta) {
    return (a)
  }

  if (t > 0 && t <= beta) {
    a <-t/beta^2
  }
  if(t > beta && t <= 2*beta) {
    a <- (2*beta - t)/beta^2
  }
  return (a)
}

#' Service distribution function
#'
#' @param t time value
#'
#' @return service function value (double) given \code{t}
#'
#' @export
#'
serviceDistribution <- function(t) {
  a<-0
  if(t<0) {
    return (a)
  }
  if(t>=2*beta) {
    return(1)
  }
  if (t > 0 && t <= beta) {
    a <-0.5*beta^-2*t^2
  }
  if(t > beta && t <= 2*beta) {
    a <- beta^(-2)*(2*beta*t - 0.5*t^2) -1
  }
  return (a)

}

#' The stationary probabilities of the environment state 0
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#'
#' @return stationary probability of the environment state 0 (double)
#'
#' @export
#'
#' @examples p0()
p0 <- function(m = c(0.2, 0.3)) {
  m[2]/sum(m)
}
#' The stationary probabilities of the environment state 1
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#'
#' @return stationary probability of the environment state 1 (double)
#'
#' @export
#'
p1 <- function(m = c(0.2, 0.3)) {
  m[1]/sum(m)
}

#' The mean intensity of the arrived flow
#'
#' @param lambda Poisson flow intensity vector
#'
#' @return mean intensity value (double) of the arrived flow
#'
#' @export
#'
flowIntensityMean <- function(lambda = c(1, 2)) {
  sum(p0() * lambda[1], p1() * lambda[2])
}


#' Load coefficient
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return load coefficient value (double) of the arriving flow
#'
#' @export
#'
#' @examples loadCoefficient(m = c(0.2, 0.3), lambda = c(1,2))
loadCoefficient <- function(m, lambda){
  (sum(m[2]*lambda[1], m[1]*lambda[2])*beta)/sum(m)
}

#' Helper "not i" function
#'
#' @param i MC i-th state
#' @export
#'
#' @return 2 if i = 0 and 1 if i = 1
#'
not_i <- function(i = 0) {
  if(i == 0) {
    return (i+2)
  }
  else {
    return (i)
  }
}

#' The density of the sojourn time in state i with probability that
# the final state equals j
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param t time value
#' @param dt time increment
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param mMax max number of addends in sums
#'
#' @return double
#' @export
#'
#' @examples densityOfSojournTimeAtState_i(1, 0, 10, 1, m=c(1, 2), mMax=5)
densityOfSojournTimeAtState_i <- function(i, j, t, dt, m=c(0.2, 0.3), mMax=14) {
  R <- 0
  R_list <- sapply(dt, function(tprim) {
    if(i != j ) {
      sapply(0:mMax, function(k) {
        R1 <- m[i+1]*(1/factorial(k))*(tprim*m[i+1])^k*exp(-(tprim*m[i+1]))
        R2 <- 1/factorial(k)*((t-tprim)*m[j+1])^k * exp(-(t-tprim)*m[j+1])
        R1*R2
      })
    }
    else {
      sapply(0:mMax, function(k) {
        R1 <- 1/factorial(k+1)*(tprim*m[i+1])^(k+1)*exp(-(tprim*m[i+1]))
        R2 <- m[not_i(i)]*
          (1/factorial(k)*((t-tprim)*m[not_i(i)])^k*exp(-((t-tprim)*m[not_i((i))])))
        R1*R2
      })
    }
  })
  R <- sum(R_list)
  return(R)
}


#' Probability of the final state
#'
#' @importFrom stats integrate
#' @param i MC i-th state
#' @param j MC j-th state
#' @param t time value
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#'
#' @return double
#' @export
#'
#' @examples finalStateProbability(0, 1, 10)
finalStateProbability <- function(i, j, t, m=c(0.2, 0.3)) {
  f <- function(subdivisions) {
    densityOfSojournTimeAtState_i(i, j, t, dt=subdivisions)
  }

  prF <- integrate(Vectorize(f), 0, t)

  if(i==j) {
    prF <- prF$value + exp(-m[i+1]*t)
  }

  return (prF)
}

#' Mean sojourn time in the initial state i jointly with final probability of state j
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param t time value
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#'
#' @return double
#' @export
#'
#' @examples meanSojournTimeWithFSP(1, 0, 3)
meanSojournTimeWithFSP <- function(i, j, t, m=c(0.2, 0.3)) {
  fmean <-function(subdivisions) {
    subdivisions*densityOfSojournTimeAtState_i(i=i, j=j,t=t, dt=subdivisions)
  }
  R <- integrate(Vectorize(fmean), 0, t)$value
  if(i==j) {
    R <- R + t*exp(-m[i+1]*t)
  }

  return (R)
}

#' Mean sojourn time in the initial state i (without final probability of state j)
#'
#' @param i MC i-th state
#' @param t time value
#'
#' @return double
#' @export
#'
#' @examples meanSoujournTime(0, 10)
meanSoujournTime <- function(i, t) {
  ETD0 <- meanSojournTimeWithFSP(i,0,t)
  ETD1 <- meanSojournTimeWithFSP(i, 1, t)
  ETD <- sum(ETD0, ETD1)
  return (ETD)
}

#' Probability of n arrival during time t jointly with final state j if initial state is i
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param n number of arrivals
#' @param t upper integration limit
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return double
#' @export
#'
#' @examples probabilityOfNArrival(1, 0, 10, 3, m=c(0.5, 0.3), lambda=c(2, 1))
probabilityOfNArrival <- function(i, j, n, t, m=c(0.2, 0.3), lambda = c(1, 2)) {
  a <- 0
  if(i!=j) {
    k <- function(subdivisions) {
      densityOfSojournTimeAtState_i(i, j, t, dt=subdivisions, m)* 1/factorial(n) *(
        (lambda[i+1]*subdivisions + ((t-subdivisions) * lambda[j+1]))^n *
          (exp(-(lambda[i+1]*subdivisions + (t-subdivisions)* lambda[j+1])))
      )
    }

    a <- integrate(Vectorize(k), 0, t)$value

  } else {
    v <- not_i(i)
    g <- function(subdivisions) {
      densityOfSojournTimeAtState_i(i, j, t, dt=subdivisions, m)*1/factorial(n) * (
        (lambda[i+1]*subdivisions + ((t-subdivisions) * lambda[v]))^n *
          (exp(-(lambda[i+1]*subdivisions + (t-subdivisions)* lambda[v])))
      )
    }

    b <- integrate(Vectorize(g), 0, t)$value

    a<- b + exp(-t*m[i+1])*(1/factorial(n))*(lambda[i+1]*t)^n * exp(-(lambda[i+1]*t))

  }

  return(a)
}

#' Probability of n arrival during time t (without joint probability of j)
#'
#' @param i MC i-th state
#' @param n number of arrivals
#' @param t time value
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return double
#' @export
#'
#' @examples probabilityOfNArrivalW(1, 2, 3)
probabilityOfNArrivalW <- function(i, n, t, m = c(0.2, 0.3), lambda = c(1,2)) {
  p <- probabilityOfNArrival(i, 1-i, n, t, m=m, lambda=lambda)
  t <- probabilityOfNArrival(i, i, n, t, m=m, lambda=lambda)

  return (sum(p, t))
}

#' Expectation of number of arriving claims depending on i and j
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param t time value
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return double
#' @export
#'
#' @examples EN(1, 1, 2)
EN <- function(i, j, t, m=c(0.2, 0.3), lambda=c(1,2)) {
  R <- 0
  v <- not_i(i)
  if(i != j) {
    k <- function(subdivisions) {
      densityOfSojournTimeAtState_i(i, j, t, dt=subdivisions, m = c(0.2, 0.3)) * (
        (lambda[i+1]*subdivisions + ((t-subdivisions) * lambda[j+1]))
      )
    }
    R <- integrate(Vectorize(k), 0, t)$value
  }
  else {
    k <- function(subdivisions) {
      densityOfSojournTimeAtState_i(i, j, t, dt=subdivisions) * (
        (lambda[i+1]*subdivisions + ((t-subdivisions) * lambda[v]))
      )
    }
    R <- integrate(Vectorize(k), 0, t)$value
    R <- R + lambda[i+1]*t*exp(-t*m[i+1])
  }

  return (R)
}

#' Expectation of number of arriving claims
#'
#' @param i MC i-th state
#' @param t time value
#'
#' @return double
#' @export
#'
#' @examples ENU(1, 3)
ENU <- function(i, t) {
  EN1 <- EN(i, 0, t)
  EN2 <- EN(i, 1, t)

  return (sum(EN1, EN2))
}

#' Density of empty time for initial state i jointly with probability of final state j
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param t time value
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return double
#' @export
#'
#' @examples h(1, 1, 2, m = c(2.5, 0.2))
h <- function(i, j, t, m = c(0.2, 0.3), lambda=c(1,2)) {
  return (probabilityOfNArrival(i, j, 0, t, m=m, lambda=lambda)*lambda[j+1])
}

#' Mean idle time if initial state i
#'
#' @param i MC i-th state
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#'
#' @return double
#' @export
#'
#' @examples MET(1)
MET <- function(i, m = c(0.2, 0.3), lambda=c(1,2), tmax=12) {
  base <- function(t) {

    return (t*(h(i,0,t, m, lambda)+ h(i, 1,t, m, lambda)))
  }

  int <- integrate(Vectorize(base), 0, tmax)
  return (int$value)
}

#' Mean empty time sojourn time in the initial state i during the empty period
#'
#' @param i MC i-th state
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#'
#' @return double
#' @export
#'
#' @examples MST(1)
MST <- function(i, m = c(0.2, 0.3), lambda=c(1,2), tmax=12) {
  base <- function(t) {

    return (probabilityOfNArrival(i,i,0,t, m = m, lambda=lambda))
  }

  int <- integrate(Vectorize(base), 0, tmax)
  return (int$value)

}

#' Probability to have state j in the ending of the idle period, if initially we have state i
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#'
#' @return double
#' @export
#'
#' @examples PrTr(1, 0)
PrTr <- function(i,j, m = c(0.2, 0.3), lambda=c(1,2), tmax=12) {
  base <- function(t) {
    return (h(i,j, t, m=m, lambda=lambda))
  }

  int <- integrate(Vectorize(base), 0, tmax)

  return (int$value)
}

#' Stationary probabilities for continuous time environment's state
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#'
#' @return double
#' @export
#'
#' @examples pi()
pi <- function(m=c(0.2, 0.3)) {
  R_list <- numeric(length(2))
  for(i in 0:length(m)-1) {
    R_list[i+1] <- m[not_i(i)]/(m[1]+m[2])
  }
  return (R_list)
}

#' Probability matrix calculation.
#' Rows represent arriving probabilities at state i
#' and columns represent the same for state j
#'
#' @param i MC i-th state
#' @param j MC j-th state
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#' @param nmax limitation for number of arriving claims
#'
#' @return matrix with \code{nmax} rows and columns
#' @export
probabilitiesMatrix <- function(i, j, m = c(0.2, 0.3), lambda=c(1,2), tmax=12, nmax=5) {
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    cores <- 2L
  } else {
    cores <- parallel::detectCores()
  }
  cl <- parallel::makeCluster(cores)
  parallel::clusterExport(cl, "PrTr")
  parallel::clusterExport(cl, "h")
  parallel::clusterExport(cl, "b")
  parallel::clusterExport(cl, "beta")
  parallel::clusterExport(cl, "probabilityOfNArrival")
  parallel::clusterExport(cl, "not_i")
  parallel::clusterExport(cl, "densityOfSojournTimeAtState_i")


  R_matrix <- matrix(0, nmax, nmax) # pre-allocate matrix

  base0 <- function(k,t) {
    vapply(t, function(x) probabilityOfNArrival(0, j, k-1, x, m = m, lambda = lambda)*b(x), numeric(1))
  }
  base1 <- function(k,t) {
    vapply(t, function(x) probabilityOfNArrival(1, j, k-1, x,  m=m, lambda = lambda)*b(x), numeric(1))
  }

  int0 <- function(a){
    parallel::parLapply(a, function(t) {
      integrate(function(x) Vectorize(base0)(t = x, k=t), 0, tmax)$value}, cl=cl)
  }

  int1 <-function(a){
    parallel::parLapply(a, function(t) {
      integrate(function(x) Vectorize(base1)(t = x, k=t), 0, tmax)$value}, cl=cl)
  }

  base <- function(k,t) {
    vapply(t, function(x)  probabilityOfNArrival(i, j, k, x, m=m, lambda=lambda) * b(x), numeric(1))
  }

  int <- function(a) {
    parallel::parLapply(a, function(t) integrate(function(x) Vectorize(base)(t = x, k=t-1), 0, tmax)$value, cl=cl)
  }

  p0 <- PrTr(i, 0, m=m, lambda=lambda, tmax=tmax)
  listResult0 <- int0(1:nmax)
  listResult1 <- int1(1:nmax)
  listResult <- int(1:nmax)

  R_matrix[1,] <- p0 * unlist(listResult0) + (1 - p0) * unlist(listResult1)

  rowValues <- unlist(listResult)
  R_matrix[2,] <- rowValues

  for (n in 3:nmax) {
    rowValues <- c(0, rowValues[-length(rowValues)])
    R_matrix[n,] <- rowValues

  }
  parallel::stopCluster(cl)

  return(R_matrix)
}

#' Resulting probabilities matrix calculation
#'
#' @importFrom foreach %dopar%
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#' @param nmax limitation for number of arriving claims
#'
#' @return matrix with \code{2*nmax} rows and columns
#' @export
resultingMatrix <- function(m=c(0.2, 0.3), lambda=c(1,2), tmax=12, nmax=5) {
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    cores <- 2L
  } else {
    cores <- parallel::detectCores()
  }
  cl1 <- parallel::makeCluster(cores)
  parallel::clusterExport(cl1, "PrTr")
  parallel::clusterExport(cl1, "h")
  parallel::clusterExport(cl1, "b")
  parallel::clusterExport(cl1, "beta")
  parallel::clusterExport(cl1, "probabilityOfNArrival")
  parallel::clusterExport(cl1, "not_i")
  parallel::clusterExport(cl1, "densityOfSojournTimeAtState_i")
  parallel::clusterExport(cl1, "probabilitiesMatrix")
  doParallel::registerDoParallel(cl1)

  # create a list of input arguments for probabilities matrix
  input_list <- list(
    list(0, 0),
    list(0, 1),
    list(1, 0),
    list(1, 1)
  )

  input <- list()

  output_list <- foreach::foreach(input=input_list) %dopar% {
    probabilitiesMatrix(input[[1]],input[[2]], m=m, lambda = lambda, tmax=tmax, nmax=nmax)
  }

  M00 <- output_list[[1]]
  M01 <- output_list[[2]]
  M10 <- output_list[[3]]
  M11 <- output_list[[4]]

  R_matrix <- matrix(0, 2*nmax, 2*nmax)

  for(n in 1:nmax) {
    for(m in 1:nmax) {
      R_matrix[2*n-1,2*m-1] <- M00[n,m]
      R_matrix[2*n-1,2*m] <- M01[n,m]
      R_matrix[2*n, 2*m-1] <- M10[n,m]
      R_matrix[2*n, 2*m] <-M11[n,m]

    }
  }
  parallel::stopCluster(cl1)

  return(R_matrix)

}


#' Stationary probability function
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#' @param nmax limitation for number of arriving claims
#'
#' @export
#'
#' @return MC stationary probability vector
#'
stationaryProbabilities <- function(m=c(0.2, 0.3), lambda=c(1,2), tmax=12, nmax=5) {
  M <- resultingMatrix(m=m, lambda = lambda, tmax=tmax, nmax=nmax)
  VCor <- M %*% rep(1, ncol(M))
  MM <- diag(c(1/VCor)) %*% M
  eig <- eigen(t(MM))
  V <- eig$values
  R <- eig$vectors[,1]
  Corr <- t(R) %*% rep(1, (length(R)))
  Res <- as.vector(1/Corr) * R
  return(Res)
}


#' Stationary probability caching function
#' @importFrom memoise memoise
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @param tmax upper integration limit
#' @param nmax limitation for number of arriving claims
#'
#' @return stationary probability vector cached
#'
stationaryProbabilities_cached <- memoise::memoise(stationaryProbabilities)

#' Mean time of empty period given the stationary probability
#'
#' @return complex
#' @export
#'
meanTimeOfEmptyPeriod <- function() {
  f1 <- MET(0)* stationaryProbabilities_cached()[1]/sum(stationaryProbabilities_cached()[1],stationaryProbabilities_cached()[2])
  f2 <- MET(1)* stationaryProbabilities_cached()[2]/sum(stationaryProbabilities_cached()[1],stationaryProbabilities_cached()[2])
  return(sum(f1, f2))
}

#' Mean time of empty period in fixed state i
#'
#' @param i MC i-th state
#'
#' @return complex
#' @export
#'
meanTimeEmptyFixed <- function(i) {
  sum <-sum(stationaryProbabilities_cached()[i+1], stationaryProbabilities_cached()[not_i(i)])
  f1 <- stationaryProbabilities_cached()[i+1]/sum
  f2<- stationaryProbabilities_cached()[not_i(i)]/sum
  f3<- MST(i)*f1 + (MET(1-i)-MST(1-i))*f2

  return(f3)

}

#' Stationary probabilities of the empty states in continuous time model
#'
#' @param i MC i-th state
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#'
#' @return complex
#' @export
#'
stationaryProbabilitiesOfEmptyStates <- function(i, m = c(0.2, 0.3), lambda = c(1, 2)) {
  ETED0 <- meanTimeEmptyFixed(0)
  ETED1 <- meanTimeEmptyFixed(1)

  return((1-loadCoefficient(m, lambda))*meanTimeEmptyFixed(i)/sum(ETED0, ETED1))
}

#' Mean time of busy period multiplied by load coefficient
#'
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' @return complex
#' @export
#'
meanTimeOfBusyPeriodEW <- function(m = c(0.2, 0.3), lambda = c(1, 2)) {
  loadCoefficient(m, lambda)*meanTimeOfEmptyPeriod()/(1-loadCoefficient(m, lambda))
}

#' Mean time of busy period
#' @param m distribution parameters vector of sojourn times
#' in alternating environment states
#' @param lambda Poisson flow intensity vector
#' description
#'
#' @return complex
#' @export
#'
meanTimeOfBusyPeriodETW <- function(m = c(0.2, 0.3), lambda = c(1, 2)) {
  meanTimeOfEmptyPeriod()/(1-loadCoefficient(m, lambda))
}


