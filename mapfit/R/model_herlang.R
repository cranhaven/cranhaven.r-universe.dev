#' Hyper-Erlang distribution
#'
#' A mixture of Erlang distributions. A subclass of PH distributions.
#' 
HErlangClass <- R6::R6Class(
  "HErlangClass",
  private = list(
    param.mixrate = NULL,
    param.shape = NULL,
    param.rate = NULL
  ),
  public = list(
    #' @description 
    #' Get mixrate
    #' @return A vector of mixrate
    mixrate = function() private$param.mixrate,
    
    #' @description 
    #' Get shape
    #' @return A vector of shapes
    shape = function() private$param.shape,
    
    #' @description 
    #' Get rate
    #' @return A vector of rates
    rate = function() private$param.rate,
    
    #' @description
    #' Create a hyper-Erlang distribution
    #' @param mixrate A vector of initial probability
    #' @param shape A vector of shape parameters
    #' @param rate A vector of rate parameters
    #' @return An instance of HErlang
    initialize = function(mixrate, shape, rate) {
      # check
      if (!(length(mixrate) == length(shape) && length(mixrate) == length(rate))) {
        stop(sprintf("Error: vector and matrix size are wrong"))
      }
      private$param.mixrate <- mixrate
      private$param.shape <- shape
      private$param.rate <- rate
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      herlang(shape=as.vector(self$shape()),
              mixrate=as.vector(self$mixrate()),
              rate=as.vector(self$rate()))
    },
    
    #' @description 
    #' The number of components
    #' @return The number of components
    size = function() {
      length(self$mixrate())
    },
    
    #' @description 
    #' Degrees of freedom
    #' @return The degrees of freedom
    df = function() {
      2*self$size() - 1
    },
    
    #' @description 
    #' Moments of HErlang
    #' @param k A value to indicate the degrees of moments. k-th moment
    #' @param ... Others
    #' @return A vector of moments from 1st to k-th moments
    moment = function(k, ...) {
      sapply(1:k, function(v) sum(
        self$mixrate() * apply(
          cbind(self$shape(), self$rate()), 1, 
          function(x) exp(lgamma(x[1]+v) - lgamma(x[1]) - v*log(x[2])))))
    },

    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      cat(gettextf("Size : %d\n", self$size()))
      cat("Shape   : ", self$shape(), "\n")
      cat("Initial : ", self$mixrate(), "\n")
      cat("Rate    : ", self$rate(), "\n")
    },
    
    #' @description 
    #' PDF
    #' @param x A vector of points
    #' @param ... Others
    #' @return A vector of densities.
    pdf = function(x, ...) {
      sapply(x, function(t) sum(apply(cbind(self$mixrate(), self$shape(), self$rate()), 1,
            function(param) param[1] * dgamma(x=t, shape=param[2], rate=param[3]))))
    },
    
    #' @description 
    #' CDF
    #' @param q A vector of points
    #' @param ... Others
    #' @return A vector of probabilities
    cdf = function(q, ...) {
      sapply(q, function(x) sum(apply(cbind(self$mixrate(), self$shape(), self$rate()), 1,
        function(param) param[1] * pgamma(q=x, shape=param[2], rate=param[3], lower.tail=TRUE))))
    },
    
    #' @description 
    #' Complementary CDF
    #' @param q A vector of points
    #' @param ... Others
    #' @return A vector of probabilities
    ccdf = function(q, ...) {
      sapply(q, function(x) sum(apply(cbind(self$mixrate(), self$shape(), self$rate()), 1,
        function(param) param[1] * pgamma(q=x, shape=param[2], rate=param[3], lower.tail=FALSE))))
    },
    
    #' @description 
    #' Make a sample
    #' @param ... Others
    #' @return A sample of HErlang
    sample = function(...) {
      i <- which(c(rmultinom(n=1, size=1, prob=self$mixrate())) == 1)
      rgamma(n=1, shape=self$shape()[i], rate=self$rate()[i])
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
      
      mixrate <- self$mixrate()
      shape <- self$shape()
      rate <- self$rate()
      switch(class(data),
             "phase.time" = emfit_herlang_wtime(mixrate, shape, rate, data, options),
             "phase.group" = emfit_herlang_group(mixrate, shape, rate, data, options)
      )
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    init = function(data, ...) {
      ph <- herlang.param(data, self$shape(), ...)
      self$initialize(ph$mixrate(), ph$shape(), ph$rate())
    }
  )
)

#' Hyper-Erlang distribution with a fixed phase
#'
#' A mixture of Erlang distributions. A subclass of PH distributions.
#' 
AHerlangClass <- R6::R6Class(
  "AHerlangClass",
  private = list(
    param.size = NULL,
    param.herlang = NULL
  ),
  public = list(
    #' @description 
    #' Get mixrate
    #' @return A vector of mixrate
    mixrate = function() private$param.herlang$mixrate(),
    
    #' @description 
    #' Get shape
    #' @return A vector of shapes
    shape = function() private$param.herlang$shape(),
    
    #' @description 
    #' Get rate
    #' @return A vector of rates
    rate = function() private$param.herlang$rate(),
    
    #' @description
    #' Create a hyper-Erlang distribution with fixed phases
    #' @param size An integer of the number of phases
    #' @param herlang An instance of HErlang
    #' @return An instance of AHerlang
    initialize = function(size, herlang) {
      if (size != sum(herlang$shape())) {
        stop(sprintf("Error: size and herlang are not matched."))
      }
      private$param.size <- size
      private$param.herlang <- herlang
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      AHerlangClass$new(private$param.size, private$param.herlang$copy())
    },
    
    #' @description 
    #' The number of components
    #' @return The number of components
    size = function() {
      private$param.herlang$size()
    },
    
    #' @description 
    #' Degrees of freedom
    #' @return The degrees of freedom
    df = function() {
      3*self$size() - 2
    },

    #' @description 
    #' Moments of HErlang
    #' @param k A value to indicate the degrees of moments. k-th moment
    #' @param ... Others
    #' @return A vector of moments from 1st to k-th moments
    moment = function(k, ...) {
      private$param.herlang$moment(k, ...)
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      private$param.herlang$print(...)
    },
    
    #' @description 
    #' PDF
    #' @param x A vector of points
    #' @param ... Others
    #' @return A vector of densities.
    pdf = function(x, ...) {
      private$param.herlang$pdf(x, ...)
    },
    
    #' @description 
    #' CDF
    #' @param q A vector of points
    #' @param ... Others
    #' @return A vector of probabilities
    cdf = function(q, ...) {
      private$param.herlang$cdf(q, ...)
    },
    
    #' @description 
    #' Complementary CDF
    #' @param q A vector of points
    #' @param ... Others
    #' @return A vector of probabilities
    ccdf = function(q, ...) {
      private$param.herlang$ccdf(q, ...)
    },
    
    #' @description 
    #' Make a sample
    #' @param ... Others
    #' @return A sample of HErlang
    sample = function(...) {
      private$param.herlang$sample(...)
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
            m <- herlang(shape=s)
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
          private$param.herlang <- maxmodel
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
              m <- herlang(shape=s)
              m$init(data)
              result <- m$emfit(data, options)
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
          private$param.herlang <- maxmodel
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
      private$param.herlang$init(data, ...)
    }
  )
)

#' Create HErlang distribution
#' 
#' Create an instance of Hyper-Erlang distribution
#' 
#' @param size An integer of the number of phases
#' @param shape A vector of shape parameters
#' @param mixrate A vector of initial probability (mixrate)
#' @param rate A vector of rate parameters
#' @return An instance of HErlang
#' 
#' @note 
#' If shape is given, shape is used even though size is set.
#' 
#' @examples
#' ## create a hyper Erlang consisting of two Erlang
#' ## with shape parameters 2 and 3.
#' (param1 <- herlang(shape=c(2,3)))
#' 
#' ## create a hyper Erlang with specific parameters
#' (param2 <- herlang(shape=c(2,3), mixrate=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## convert to a general PH
#' as.gph(param2)
#' 
#' ## p.d.f. for 0, 0.1, ..., 1
#' (dphase(x=seq(0, 1, 0.1), ph=param2))
#' 
#' ## c.d.f. for 0, 0.1, ..., 1
#' (pphase(q=seq(0, 1, 0.1), ph=param2))
#' 
#' ## generate 10 samples
#' (rphase(n=10, ph=param2))
#'
#' @export

herlang <- function(size, shape, mixrate = rep(1/length(shape),length(shape)),
                    rate = rep(1,length(shape))) {
  if (missing(shape)) {
    if (missing(size)) {
      stop("Either size or shape is needed.")
    } else {
      # create a herlang
      AHerlangClass$new(size, herlang(shape=c(size)))
    }
  } else {
    HErlangClass$new(mixrate=mixrate, shape=shape, rate=rate)
  }
}

#' Convert from HErlang to GPH
#' 
#' Convert from hyper-Erlang distribution to the general PH distribution
#' 
#' @param h An instance of HErlang
#' @return An instance of GPH
#' 
#' @examples 
#' #' ## create a hyper Erlang with specific parameters
#' (param <- herlang(shape=c(2,3), mixrate=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## convert to a general PH
#' as.gph(param)
#'
#' @export

as.gph <- function(h) {
  size <- h$size()
  mixrate <- h$mixrate()
  shape <- h$shape()
  rate <- h$rate()

  phsize <- sum(shape)
  index <- cumsum(shape)
  sindex <- c(1, index + 1)[1:size]
  eindex <- index
  alpha <- numeric(phsize)
  xi <- numeric(phsize)
  alpha[sindex] <- mixrate
  xi[eindex] <- rate

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
  Q <- sparseMatrix(dims=c(phsize,phsize), i=i, j=j, x=v)
  ph(alpha=alpha, Q=Q, xi=xi)
}

#' Determine hyper-Erlang parameters
#' 
#' Determine the hyper-Erlang parameters with k-means.
#' 
#' @param data A dataframe
#' @param shape A vector of shape parameters
#' @param ... Others
#' @return An instance of HErlang
#' @examples 
#' ## Create data
#' wsample <- rweibull(10, shape=2)
#' (dat <- data.frame.phase.time(x=wsample))
#' 
#' ## Generate PH that is fitted to dat
#' (model <- herlang.param(data=dat, shape=c(1,2,3)))
#' 
#' @export

herlang.param <- function(data, shape, ...) {
  size <- length(shape)
  tmp <- numeric(size)
  rate <- numeric(size)
  
  if (size >= 2) {
    x <- switch(
      class(data),
      "phase.time" = cumsum(data$time),
      "phase.group" = {
        t <- 0
        v <- c()
        for (i in 1:length(data$intervals)) {
          t <- t + data$intervals[i]
          v <- c(v, rep(t, ifelse(is.finite(data$counts[i]), data$counts[i], 0) + data$instants[i]))
        }
        v
      }
      )
    result <- kmeans(x, size)
    for (k in 1:size) {
      m <- base::mean(x[result$cluster == k])
      s2 <- var(x[result$cluster == k])
      tmp[k] <- round(m^2 / s2)
      rate[k] <- 1.0 / m
      #      rate[k] <- shape[k] / m
    }
    rate <- rate[rank(tmp)] * shape
  } else {
    m <- mean(data)
    rate[1] <- shape[1] / m
    mixrate <- c(1)
  }
  ##  mixrate <- runif(size)
  mixrate <- rep(1/size, size)
  herlang(shape=shape, mixrate=mixrate/sum(mixrate), rate=rate)
}

