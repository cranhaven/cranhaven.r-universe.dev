#' Canonical phase-type distribution
#'
#' A continuous distribution dominated by a continuous-time Markov chain.
#' A random time is given by an absorbing time. In the CF1 (canonical form 1),
#' the infinitesimal generator is given by a bi-diagonal matrix, and whose order
#' is determined by the ascending order.
#' 
CF1Class <- R6::R6Class(
  "CF1Class",
  inherit = GPHClass,
  private = list(
    param.rate = NULL
  ),
  public = list(
    #' @description 
    #' Get rate
    #' @return An instance of rate
    rate = function() private$param.rate,

    #' @description
    #' Create a CF1
    #' @param alpha A vector of initial probability
    #' @param rate A vector of rates
    #' @return An instance of CF1
    initialize = function(alpha, rate) {
      phase_cf1_sort(alpha, rate)
      private$param.rate <- rate
      size <- length(alpha)
      xi <- numeric(size)
      xi[size] <- rate[size]
      if (size >= 2) {
        i <- c(1:size, 1:(size-1))
        j <- c(1:size, 2:size)
        x <- c(-rate, rate[1:(size-1)])
        Q <- sparseMatrix(i=i, j=j, x=x)
      } else {
        Q <- matrix(-rate[1],1,1)
      }
      super$initialize(alpha, Q, xi)
    },

    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      cf1(alpha=as.vector(self$alpha()),
          rate=as.vector(self$rate()))
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      cat(gettextf("Size : %d\n", self$size()))
      cat("Initial : ", super$alpha(), "\n")
      cat("Rate    : ", self$rate(), "\n")
    },
    
    #' @description 
    #' Generate a sample of CF1
    #' @param ... Others
    #' @return A sample of CF1
    sample = function(...) {
      i <- which(c(rmultinom(n=1, size=1, prob=super$alpha())) == 1)
      sum(sapply(self$rate()[i:self$size()], function(r) rexp(1, rate=r)))
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

      alpha <- super$alpha()
      rate <- self$rate()
      Q <- super$Q()
      xi <- super$xi()
      P <- super$make.matrix()
      H <- super$make.matrix()
      switch(class(data),
        "phase.time" = emfit_cf1_wtime(alpha, rate, data, options, Q, P, H),
        "phase.group" = emfit_cf1_group(alpha, rate, data, options, Q, P, H)
      )
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    init = function(data, options, ...) {
      ph <- cf1.param(data, self$size(), options, ...)
      self$initialize(ph$alpha(), ph$rate())
    }
  )
)

#' Create CF1
#' 
#' Create an instance of CF1.
#' 
#' @param size An integer of the number of phases
#' @param alpha A vector of initial probabilities
#' @param rate A vector of rates
#' @return An instance of CF1.
#' @examples
#' ## create a CF1 with 5 phases
#' (param1 <- cf1(5))
#' 
#' ## create a CF1 with 5 phases
#' (param1 <- cf1(size=5))
#' 
#' ## create a CF1 with specific parameters
#' (param2 <- cf1(alpha=c(1,0,0), rate=c(1.0,2.0,3.0)))
#' 
#' @export

cf1 <- function(size, alpha, rate) {
  if (missing(size)) {
    if (missing(alpha) || missing(rate)) {
      stop("alpha and rate are needed.")
    } else {
      size <- length(alpha)
    }
  } else {
    if (!missing(alpha) && !missing(rate)) {
      warning("size is ignored.")
      size <- length(alpha)
    } else {
      if (!missing(alpha) || !missing(rate)) {
        warning("alpha and rate are ignored.")
      }
      alpha <- rep(1.0/size, size)
      rate <- rep(1.0, size)      
    }
  }
  CF1Class$new(alpha, rate)
}

#' Determine CF1 parameters
#' 
#' Determine CF1 parameters based on the power rule.
#' 
#' @param size An integer of the number of phases
#' @param mean A value of mean of data
#' @param s A value of fraction of minimum and maximum rates
#' @return A list of alpha and rate

cf1.param.power <- function(size, mean, s) {
  alpha <- rep(1.0/size, size)
  rate <- numeric(size)
  
  p <- exp(1.0/(size-1) * log(s))
  total <- 1.0
  tmp <- 1.0
  for (i in 2:size) {
    tmp <- tmp * i / ((i-1) * p)
    total <- total + tmp
  }
  base <- total / (size * mean)
  tmp <- base
  for (i in 1:size) {
    rate[i] <- tmp
    tmp <- tmp * p
  }
  cf1(alpha=alpha, rate=rate)
}

#' Determine CF1 parameters
#' 
#' Determine CF1 parameters based on the linear rule.
#' 
#' @param size An integer of the number of phases
#' @param mean A value of mean of data
#' @param s A value of fraction of minimum and maximum rates
#' @return A list of alpha and rate

cf1.param.linear <- function(size, mean, s) {
  alpha <- rep(1.0/size, size)
  rate <- numeric(size)
  
  al <- (s-1)/(size-1)
  total <- 1.0
  for (i in 2:size) {
    total <- total + i / (al * (i-1) + 1)
  }
  base <- total / (size * mean)
  for (i in 1:size) {
    tmp <- base * (al * (i-1) + 1)
    rate[i] <- tmp
  }
  cf1(alpha=alpha, rate=rate)
}

#' Create CF1 with data information
#' 
#' Crate CF1 with the first moment of a given data. This function calls cf1.param.linear
#' and cf1.param.power to determine CF1. After execute 5 EM steps, the model with the smallest
#' LLF is selected.
#' 
#' @param data A dataframe
#' @param size An integer for the number of phases
#' @param options A list of options for EM steps
#' @param ... Others. This can provide additional options for EM steps.
#' @examples
#' ## Generate group data
#' dat <- data.frame.phase.group(c(1,2,0,4), seq(0,10,length.out=5))
#' 
#' ## Create an instance of CF1
#' p <- cf1.param(data=dat, size=5)
#' 
#' @export

cf1.param <- function(data, size, options, ...) {
  if (missing(options)) {
    options <- emoptions()
  }
  con <- list(...)
  nmsC <- names(options)
  options[(namc <- names(con))] <- con

  diff.init <- options$cf1.diff.init
  scale.init <- options$cf1.scale.init
  verbose <- options$cf1.verbose
  maxiter.init <- options$cf1.maxiter
  
  if (verbose) cat("Initializing CF1 ...\n")
  m <- mean(data)
  maxllf <- -Inf
  for (x in scale.init) {
    for (s in diff.init) {
      ph <- cf1.param.linear(size, m * x, s)
      phres <- try(ph$emfit(data, options, maxiter=maxiter.init), silent = TRUE)
      if (!is(phres, "try-error")) {
        if (is.finite(phres$llf)) {
          if (maxllf < phres$llf) {
            maxllf <- phres$llf
            maxph <- ph
            if (verbose) cat("o")
          }
          else {
            if (verbose) cat("x")
          }
        }
        else {
          if (verbose) cat("-")
        }
      }
      else {
        if (verbose) cat("-")
      }
    }
    if (verbose) cat("\n")
    
    for (s in diff.init) {
      ph <- cf1.param.power(size, m * x, s)
      phres <- try(ph$emfit(data, options, maxiter = maxiter.init), silent = TRUE)
      if (!is(phres, "try-error")) {
        if (is.finite(phres$llf)) {
          if (maxllf < phres$llf) {
            maxllf <- phres$llf
            maxph <- ph
            if (verbose) cat("o")
          }
          else {
            if (verbose) cat("x")
          }
        }
        else {
          if (verbose) cat("-")
        }
      }
      else {
        if (verbose) cat("-")
      }
    }
    if (verbose) cat("\n")
  }
  maxph
}


