#' ErlangHMM for MAP
#'
#' A special case of MAP.
#' 
ERHMMClass <- R6::R6Class(
  "ERHMMClass",
  private = list(
    param.alpha = NULL,
    param.shape = NULL,
    param.rate = NULL,
    param.P = NULL,
    param.xi = NULL,
    matclass = "dgeMatrix",
    
    make.matrix = function() {
      as(as.matrix(private$param.P), private$matclass)
    }
  ),
  public = list(
    #' @description 
    #' Get alpha
    #' @return A vector of alpha
    alpha = function() private$param.alpha,
    
    #' @description 
    #' Get shape
    #' @return A vector of shapes
    shape = function() private$param.shape,
    
    #' @description 
    #' Get rate
    #' @return A vector of rates
    rate = function() private$param.rate,
    
    #' @description 
    #' Get P
    #' @return A matrix of P
    P = function() private$param.P,

    #' @description 
    #' Get exit rates
    #' @return A vector of exit rates
    xi = function() private$param.xi,

    #' @description
    #' Create an ERHMM
    #' @param alpha A vector of initial probability
    #' @param shape A vector of shape parameters
    #' @param rate A vector of rate parameters
    #' @param P A matrix of transition probabilities
    #' @param xi An exit rate vector
    #' @return An instance of ERHMM
    initialize = function(alpha, shape, rate, P, xi) {
      # check
      if (!(length(alpha) == length(shape)
            && length(alpha) == length(rate)
            && length(alpha) == dim(P)[1]
            && length(alpha) == dim(P)[2]
            && length(alpha) == length(xi))) {
        stop(sprintf("Error: vector and matrix size are wrong"))
      }
      private$param.alpha <- alpha
      private$param.shape <- shape
      private$param.rate <- rate
      private$param.P <- as(P, private$matclass)
      private$param.xi <- xi
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      erhmm(shape=as.vector(self$shape()),
            alpha=as.vector(self$alpha()),
            rate = as.vector(self$rate()),
            P = as.matrix(self$P()))
    },
    
    #' @description 
    #' The number of components
    #' @return The number of components
    size = function() {
      length(self$alpha())
    },
    
    #' @description 
    #' Degrees of freedom
    #' @return The degrees of freedom
    df = function() {
      self$size() * self$size() - self$size() + 2*self$size() - 1
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      cat(gettextf("Size : %d\n", self$size()))
      cat("Shape   : ", self$shape(), "\n")
      cat("Initial : ", self$alpha(), "\n")
      cat("Rate    : ", self$rate(), "\n")
      cat("Transition probability : \n")
      print(self$P())
    },

    #' @description 
    #' Marginal moments
    #' @param k An integer of degree
    #' @param ... Others
    #' @return A vector of moments
    mmoment = function(k, ...) {
      m <- as.map(self)
      m$mmoment(k, ...)
    },

    #' @description 
    #' Joint moments
    #' @param lag An integer of lag
    #' @param ... Others
    #' @return A matrix of moments
    jmoment = function(lag, ...) {
      m <- as.map(self)
      m$jmoment(lag, ...)
    },
    
    #' @description 
    #' k-lag correlation
    #' @param ... Others
    #' @return A vector for k-lag correlation
    acf = function(...) {
      m <- as.map(self)
      m$acf(...)
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
      shape <- self$shape()
      rate <- self$rate()
      xi <- self$xi()
      P <- self$P()
      H <- private$make.matrix()
      switch(class(data),
             "map.time" = emfit_erhmm_time(alpha, xi, rate, shape, P, data, options, H),
             stop("Error: ERHMM can handle time point data only.")
      )
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param ... Others
    init = function(data, ...) {
      m <- erhmm.param(data, self, ...)
      self$initialize(m$alpha(), m$shape(), m$rate(), m$P(), m$xi())
    }
  )
)

#' ErlangHMM for MAP with fixed phases
#'
#' A special case of MAP.
#' 
AERHMMClass <- R6::R6Class(
  "AERHMMClass",
  private = list(
    param.size = NULL,
    param.erhmm = NULL
  ),
  public = list(
    #' @description 
    #' Get alpha
    #' @return A vector of alpha
    alpha = function() private$param.erhmm$alpha(),
    
    #' @description 
    #' Get shape
    #' @return A vector of shapes
    shape = function() private$param.erhmm$shape(),
    
    #' @description 
    #' Get rate
    #' @return A vector of rates
    rate = function() private$param.erhmm$rate(),
    
    #' @description 
    #' Get P
    #' @return A matrix of P
    P = function() private$param.erhmm$P(),
    
    #' @description 
    #' Get exit rates
    #' @return A vector of exit rates
    xi = function() private$param.erhmm$xi(),
    
    #' @description
    #' Create an AERHMM
    #' @param size An integer of the number of phases
    #' @param erhmm An instance of ERHMM
    #' @return An instance of AERHMM
    initialize = function(size, erhmm) {
      if (size != sum(erhmm$shape())) {
        stop(sprintf("Error: size and erhmm are not matched."))
      }
      private$param.size <- size
      private$param.erhmm <- erhmm
    },

    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      AERHMMClass$new(private$param.size, private$param.erhmm$copy())
    },
    
    #' @description 
    #' The number of components
    #' @return The number of components
    size = function() {
      private$param.erhmm$size()
    },
    
    #' @description 
    #' Degrees of freedom
    #' @return The degrees of freedom
    df = function() {
      self$size() * self$size() - self$size() + 3*self$size() - 2
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      private$param.erhmm$print(...)
    },
    
    #' @description 
    #' Marginal moments
    #' @param k An integer of degree
    #' @param ... Others
    #' @return A vector of moments
    mmoment = function(k, ...) {
      private$param.erhmm$mmoment(k, ...)
    },
    
    #' @description 
    #' Joint moments
    #' @param lag An integer of lag
    #' @param ... Others
    #' @return A matrix of moments
    jmoment = function(lag, ...) {
      private$param.erhmm$jmoment(lag, ...)
    },
    
    #' @description 
    #' k-lag correlation
    #' @param ... Others
    #' @return A vector for k-lag correlation
    acf = function(...) {
      private$param.erhmm$acf(...)
    },
    
    #' @description 
    #' Run EM
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    emfit = function(data, options, ...) {
      method <- options$shape.method
      phsize <- private$param.size
      lbound <- options$lbound
      ubound <- ifelse(is.na(options$ubound), phsize, options$ubound)
      verbose <- options$shape.verbose
      result <- switch(
        method,
        "all" = {
          maxllf <- -Inf
          maxmodel <- NULL
          maxresult <- NULL
          allshape <- shape.all(phsize, lbound, ubound)
          for (s in allshape) {
            m <- erhmm(shape=s)
            m$init(data)
            result <- m$emfit(data, options, ...)
            if (verbose)
              cat("shape: ", s, gettextf(" llf=%.2f\n", result$llf))
            if (is.finite(result$llf)) {
              if (result$llf > maxllf) {
                maxllf <- result$llf
                maxresult <- result
                maxmodel <- m
              }
            }
          }
          private$param.erhmm <- maxmodel
          maxresult
        },
        "increment" = {
          maxllf <- -Inf
          maxmodel <- NULL
          maxresult <- NULL
          shape <- rep(1, lbound)
          repeat {
            shapelist1 <- shape.increment(shape, ubound, phsize)
            shapelist2 <- shape.decrement(shape, lbound)
            shapelist <- c(shapelist1, shapelist2)
            shape <- NULL
            for (s in shapelist) {
              m <- erhmm(shape=s)
              m$init(data)
              result <- m$emfit(data, options, ...)
              if (verbose)
                cat("shape: ", s, gettextf(" llf=%.2f\n", result$llf))
              if (is.finite(result$llf)) {
                if (result$llf > maxllf) {
                  maxllf <- result$llf
                  maxresult <- result
                  shape <- s
                  maxmodel <- m
                }
              }
            }
            if (is.null(shape)) {
              ##      warning(message="break")
              break
            }
          }
          private$param.erhmm <- maxmodel
          maxresult
        }
      )
      # print(result)
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    init = function(data, ...) {
      private$param.erhmm$init(data, ...)
    }
  )
)

#' Create ERHMM
#' 
#' Create an instance of ERHMM
#' 
#' @param size An integer of the number of phases
#' @param shape A vector of shape parameters
#' @param alpha A vector of initial probability (alpha)
#' @param rate A vector of rate parameters
#' @param P A matrix of transition probabilities
#' @return An instance of ERHMM
#' 
#' @note 
#' If shape is given, shape is used even though size is set.
#'
#' @export

erhmm <- function(size, shape, alpha = rep(1/length(shape),length(shape)),
                    rate = rep(1,length(shape)), P = matrix(1/length(shape), length(shape), length(shape))) {
  if (missing(shape)) {
    if (missing(size)) {
      stop("Either size or shape is needed.")
    } else {
      # create an erhmm
      AERHMMClass$new(size, erhmm(shape=c(size)))
    }
  } else {
    if (missing(size)) {
      size <- length(shape)
    }
    ERHMMClass$new(alpha=alpha, shape=shape, rate=rate, P=P, xi=rep(1, size))
  }
}

#' Convert from ERHMM to MAP
#' 
#' Convert from ERHMM to the general MAP
#' 
#' @param x An instance of ERHMM
#' @return An instance of MAP
#' 
#' @examples 
#' ## create a hyper Erlang with specific parameters
#' (param <- erhmm(shape=c(2,3), alpha=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## convert to a general PH
#' as.map(param)
#'
#' @export

as.map <- function(x) {
  size <- x$size()
  mixrate <- x$alpha()
  shape <- x$shape()
  rate <- x$rate()
  P <- x$P()

  phsize <- sum(shape)
  index <- cumsum(shape)
  sindex <- c(1, index + 1)[1:size]
  eindex <- index
  alpha <- numeric(phsize)
  alpha[sindex] <- mixrate

  v <- numeric(0)
  i <- numeric(0)
  j <- numeric(0)
  for (k in 1:size) {
    i <- c(i, sindex[k]:eindex[k])
    j <- c(j, sindex[k]:eindex[k])
    v <- c(v, rep(-rate[k], shape[k]))
  }
  for (k in 1:size) {
    if (shape[k] != 1) {
      i <- c(i, sindex[k]:(eindex[k]-1))
      j <- c(j, (sindex[k]+1):eindex[k])
      v <- c(v, rep(rate[k], shape[k]-1))
    }
  }
  D0 <- sparseMatrix(dims=c(phsize,phsize), i=i, j=j, x=v)

  v <- numeric(0)
  i <- numeric(0)
  j <- numeric(0)
  for (k1 in 1:size) {
    for (k2 in 1:size) {
      i <- c(i, eindex[k1])
      j <- c(j, sindex[k2])
      v <- c(v, rate[k1] * P[k1,k2])
    }
  }
  D1 <- sparseMatrix(dims=c(phsize,phsize), i=i, j=j, x=v)
  
  map(alpha=alpha, D0=D0, D1=D1)
}

#' Determine ERHMM parameters
#' 
#' Determine ERHMM parameters with k-means.
#' 
#' @param data A dataframe
#' @param skel An instance of ERHMM used as a skeleton
#' @param ... Others
#' @return An instance of ERHMM

erhmm.param <- function(data, skel, ...) {
  size <- skel$size()
  shape <- skel$shape()
  tmp <- numeric(size)
  rate <- numeric(size)
  
  if (size >= 2) {
    x <- data$intervals
    result <- kmeans(x, size)
    for (k in 1:size) {
      m <- base::mean(x[result$cluster == k])
      s2 <- var(x[result$cluster == k])
      tmp[k] <- round(m^2 / s2)
      rate[k] <- 1.0 / m
    }
    rate <- rate[rank(tmp)] * shape
  } else {
    m <- mean(data)
    rate[1] <- shape[1] / m
  }
  alpha <- skel$alpha() * runif(size)
  alpha <- alpha / sum(alpha)
  P <- as.matrix(skel$P()) * matrix(runif(size*size), size, size)
  P <- P / apply(P, 1, sum)
  erhmm(shape=shape, alpha=alpha, rate=rate, P=P)
}

