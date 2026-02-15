#' General Markovian arrival process
#'
#' A point process dominated by a continuous-time Markov chain.
#' 
MAPClass <- R6::R6Class(
  "MAPClass",
  private = list(
    param.alpha = NULL,
    param.D0 = NULL,
    param.D1 = NULL,
    param.xi = NULL,
    param.df = 0,
    matclass = "dgeMatrix",
    
    make.matrixD0 = function() {
      as(as.matrix(private$param.D0), private$matclass)
    },
    make.matrixD1 = function() {
      as(as.matrix(private$param.D1), private$matclass)
    }      
  ),
  public = list(
    #' @description 
    #' Get alpha
    #' @return A vector of alpha
    alpha = function() private$param.alpha,
    
    #' @description 
    #' Get D0
    #' @return A matrix of D0
    D0 = function() private$param.D0,
    
    #' @description 
    #' Get D1
    #' @return A matrix of D1
    D1 = function() private$param.D1,

    #' @description 
    #' Get xi
    #' @return A vector of xi
    xi = function() private$param.xi,
    
    #' @description
    #' Create a MAP
    #' @param alpha A vector of initial probability
    #' @param D0 An infinitesimal generator
    #' @param D1 An infinitesimal generator
    #' @param xi An exit rate vector
    #' @return An instance of MAP
    initialize = function(alpha, D0, D1, xi) {
      # check
      if (!(length(alpha) == length(xi) &&
            length(alpha) == dim(D0)[1] &&
            length(alpha) == dim(D0)[2] &&
            length(alpha) == dim(D1)[1] &&
            length(alpha) == dim(D1)[2])) {
        stop(sprintf("Error: vector and matrix size are wrong"))
      }
      private$param.alpha <- alpha / sum(alpha)
      diag(D0) <- 0
      diag(D0) <- -(apply(D0, 1, sum) + apply(D1, 1, sum))
      private$param.D0 <- as(D0, private$matclass)
      private$param.D1 <- as(D1, private$matclass)
      private$param.xi <- xi

      # set df
      zero <- 1.0e-8
      private$param.df <- (sum(private$param.alpha > zero) - 1) +
        sum(private$param.D0 > zero) +
        sum(private$param.D1 > zero)
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      map(alpha=as.vector(self$alpha()),
          D0=as.matrix(self$D0()),
          D1=as.matrix(self$D1()))
    },
    
    #' @description 
    #' The number of phases
    #' @return The number of phases
    size = function() {
      length(self$alpha())
    },
    
    #' @description 
    #' Degrees of freedom
    #' @return The degrees of freedom
    df = function() {
      private$param.df
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      cat(gettextf("Size : %d\n", self$size()))
      cat("Initial : ", self$alpha(), "\n")
      cat("Infinitesimal generator D0: \n")
      print(self$D0())
      cat("Infinitesimal generator D1: \n")
      print(self$D1())
    },
    
    #' @description 
    #' Marginal moments
    #' @param k An integer of degree
    #' @param ... Others
    #' @return A vector of moments
    mmoment = function(k, ...) {
      size <- self$size()
      D0 <- as.matrix(self$D0())
      D1 <- as.matrix(self$D1())
      piv <- as.vector(ctmc.st(D0+D1) %*% D1)
      piv <- piv / sum(piv)
      tmp <- piv
      tmp2 <- 1.0
      res <- numeric(0)
      for (i in 1:k) {
        tmp <- solve(a=-t(D0), b=tmp)
        tmp2 <- tmp2 * i
        res <- c(res, tmp2 * sum(tmp))
      }
      res
    },
    
    #' @description 
    #' Joint moments
    #' @param lag An integer of lag
    #' @param ... Others
    #' @return A matrix of moments
    jmoment = function(lag, ...) {
      size <- self$size()
      D0 <- as.matrix(self$D0())
      D1 <- as.matrix(self$D1())
      piv <- as.vector(ctmc.st(D0+D1) %*% D1)
      piv <- piv / sum(piv)
      P <- diag(size)
      tmp <- solve(-D0) %*% D1
      for (i in 1:lag) {
        P <- tmp %*% P
      }
      vone <- rep(1, size)
      
      fmat <- matrix(0, size, size)
      fmat[,1] <- piv
      for (i in 2:size) {
        fmat[,i] <- solve(a=-t(D0), b=fmat[,i-1])
      }
      
      res <- matrix(0, size, size)
      tmp <- vone
      for (j in 1:size) {
        for (i in 1:size) {
          res[i,j] <- gamma(i) * gamma(j) * as.vector(fmat[,i] %*% P %*% tmp)
        }
        tmp <- solve(a=-D0, b=tmp)
      }
      res
    },

    #' @description 
    #' k-lag correlation
    #' @param ... Others
    #' @return A vector for k-lag correlation
    acf = function(...) {
      size <- self$size()
      D0 <- as.matrix(self$D0())
      D1 <- as.matrix(self$D1())
      piv <- as.vector(ctmc.st(D0+D1) %*% D1)
      piv <- piv / sum(piv)
      P <- solve(-D0) %*% D1
      vone <- rep(1, size)
      
      piv <- solve(a=-t(D0), b=piv)
      vone <- solve(a=-D0, b=vone)
      
      mres <- self$mmoment(2)
      result <- c()
      tmp <- P
      for (k in 1:size) {
        result <- c(result, (as.vector(piv %*% tmp %*% vone) - mres[1]^2) / (mres[2] - mres[1]^2))
        tmp <- tmp %*% P
      }
      result
    },
    
    #' @description 
    #' Run EM
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    emfit = function(data, options, ...) {
      con <- list(...)
      nmsC <- names(options)
      options[(namc <- names(con))] <- con
      
      alpha <- self$alpha()
      D0 <- self$D0()
      D1 <- self$D1()
      xi <- self$xi()
      P0 <- private$make.matrixD0()
      P1 <- private$make.matrixD1()
      H0 <- private$make.matrixD0()
      H1 <- private$make.matrixD1()
      en0 <- private$make.matrixD0()
      en1 <- private$make.matrixD1()
      switch(class(data),
             "map.time" = emfit_mapgen_group(alpha, xi, D0, D1, data, options,
                                              P0, P1, H0, H1, en0, en1),
             "map.group" = emfit_mapgen_group(alpha, xi, D0, D1, data, options,
                                              P0, P1, H0, H1, en0, en1)
      )
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param ... Others
    init = function(data, ...) {
      m <- map.param(data, self, ...)
      self$initialize(m$alpha(), m$D0(), m$D1(), m$xi())
    }
  )
)

#' Create MAP
#'
#' Create an instance of MAP
#'
#' @param size An integer for the number of phases
#' @param alpha A vector of initial probability
#' @param D0 An infinitesimal generator without arrivals
#' @param D1 An infinitesimal generator with arrivals
#' @return An instance of MAP
#'
#' @note
#' This function can omit several patterns of arguments. For example, `map(5)`
#' omit the arguments `alpha`, `D0` `D1` and `xi`. In this case, the default values are
#' assigned to them.
#'
#' @examples
#' ## create a map (full matrix) with 5 phases
#' (param1 <- map(5))
#'
#' ## create a map with specific parameters
#' (param2 <- map(alpha=c(1,0,0),
#'               D0=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               D1=rbind(c(2,0,0),c(0,2,0),c(0,0,0))))
#'
#' @export

map <- function(size, alpha, D0, D1) {
  if (missing(size)) {
    if (missing(alpha) || missing(D0) || missing(D1)) {
      stop("alpha, D0 and D1 are needed.")
    } else {
      size <- length(alpha)
    }
  } else {
    if (!missing(alpha) && !missing(D0) && !missing(D1)) {
      # warning("size is ignored.")
      size <- length(alpha)
    } else {
      if (missing(alpha)) {
        # warning("alpha is set by default")
        alpha <- rep(1.0/size, size)
      }
      if (missing(D1)) {
        # warning("D1 is set by default")
        D1 <- matrix(1.0, size, size)
      }
      if (missing(D0)) {
        # warning("D0 is set by default")
        D0 <- matrix(1.0, size, size)
      }
    }
  }
  MAPClass$new(alpha=alpha, D0=D0, D1=D1, xi=rep(1, size))
}

#' Create an MMPP
#'
#' Create an instance of MMPP (Markov-Modulated Poisson Process)
#'
#' @param size An integer for the number of phases
#' @return An instance of MMPP
#'
#' @note
#' MMPP is a MAP whose D1 is given by a diagonal matrix.
#'
#' @examples
#' ## create an MMPP with 5 phases
#' (param1 <- mmpp(5))
#'
#' @export

mmpp <- function(size) {
  alpha <- rep(1.0/size, size)
  D1 <- diag(1.0, size)
  D0 <- matrix(1.0, size, size)
  map(alpha=alpha, D0=D0, D1=D1)
}


#' Marginal moments of MAP
#'
#' Compute up to k-th marginal moments for a given MAP
#'
#' @param k An integer for the moments to be computed
#' @param map An instance of MAP
#' @param ... Others
#' @return A vector of moments
#' @examples
#' ## create an MAP with specific parameters
#' (param1 <- map(alpha=c(1,0,0),
#'                D0=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-4)),
#'                D1=rbind(c(1,1,0),c(1,0,1),c(2,0,1))))
#'
#' ## create an ER-HMM with specific parameters
#' (param2 <- erhmm(shape=c(2,3), alpha=c(0.3,0.7),
#'                  rate=c(1.0,10.0),
#'                  P=rbind(c(0.3, 0.7), c(0.1, 0.9))))
#'
#' map.mmoment(k=3, map=param1)
#' map.mmoment(k=3, map=param2)
#'
#' @export

map.mmoment <- function(k, map, ...) {
  map$mmoment(k, ...)
}

#' Joint moments of MAP
#'
#' Compute joint moments for a given MAP
#'
#' @param lag An integer for lag
#' @param map An instance of MAP
#' @param ... Others
#' @return A vector of moments
#' @examples
#' ## create an MAP with specific parameters
#' (param1 <- map(alpha=c(1,0,0),
#'                D0=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-4)),
#'                D1=rbind(c(1,1,0),c(1,0,1),c(2,0,1))))
#'
#' ## create an ER-HMM with specific parameters
#' (param2 <- erhmm(shape=c(2,3), alpha=c(0.3,0.7),
#'                  rate=c(1.0,10.0),
#'                  P=rbind(c(0.3, 0.7), c(0.1, 0.9))))
#'
#' map.jmoment(lag=1, map=param1)
#' map.jmoment(lag=1, map=param2)
#'
#' @export

map.jmoment <- function(lag, map, ...) {
  map$jmoment(lag, ...)
}

#' k-lag correlation of MAP
#'
#' Compute k-lag correlation
#'
#' @param map An instance of MAP
#' @param ... Others
#' @return A vector of k-lag correlation
#' @examples
#' ## create an MAP with specific parameters
#' (param1 <- map(alpha=c(1,0,0),
#'                D0=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-4)),
#'                D1=rbind(c(1,1,0),c(1,0,1),c(2,0,1))))
#'
#' ## create an ER-HMM with specific parameters
#' (param2 <- erhmm(shape=c(2,3), alpha=c(0.3,0.7),
#'                  rate=c(1.0,10.0),
#'                  P=rbind(c(0.3, 0.7), c(0.1, 0.9))))
#'
#' map.acf(map=param1)
#' map.acf(map=param2)
#'
#' @export

map.acf <- function(map, ...) {
  map$acf(...)
}

#' Generate MAP using the information on data
#'
#' Generate MAP randomly and adjust parameters to fit its first moment to
#' the first moment of data.
#'
#' @param data A dataframe
#' @param skel An instance of skeleton of MAP
#' @param ... Others
#' @return An instance of MAP
#'
#' @export

map.param <- function(data, skel, ...) {
  size <- skel$size()
  alpha <- skel$alpha()
  D0 <- skel$D0()
  D1 <- skel$D1()

  maxt <- max(data$intervals)
  maxg <- max(data$counts + data$instants)
  x <- cbind(data$intervals, (data$counts + data$instants))
  v <- cbind(data$intervals / maxt, (data$counts + data$instants) / maxg)
  result <- kmeans(v, size)
  
  diagelem <- sapply(1:size, function(k) {
    (sum(x[result$cluster == k,2]) + 1) /
      sum(x[result$cluster == k,1])})
  
  alpha <- alpha * runif(size)
  alpha <- alpha / sum(alpha)
  
  diag(D0) <- 0
  D0 <- D0 * matrix(runif(size*size), size, size)
  D1 <- D1 * matrix(runif(size*size), size, size)
  d <- diagelem / (apply(D0, 1, sum) + apply(D1, 1, sum))
  D0 <- D0 * d
  D1 <- D1 * d
  diag(D0) <- -diagelem
  map(alpha=alpha, D0=D0, D1=D1)
}
