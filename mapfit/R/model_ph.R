#' General phase-type distribution
#'
#' A continuous distribution dominated by a continuous-time Markov chain.
#' A random time is given by an absorbing time.
#' 
GPHClass <- R6::R6Class(
  "GPHClass",
  private = list(
    param.alpha = NULL,
    param.Q = NULL,
    param.xi = NULL,
    param.df = 0,
    matclass = "dgCMatrix",

    make.matrix = function() {
      as(as.matrix(private$param.Q), private$matclass)
    }      
  ),
  public = list(
    #' @description 
    #' Get alpha
    #' @return A vector of alpha
    alpha = function() private$param.alpha,

    #' @description 
    #' Get Q
    #' @return A matrix of Q
    Q = function() private$param.Q,

    #' @description 
    #' Get xi
    #' @return A vector of xi
    xi = function() private$param.xi,

    #' @description
    #' Create a GPH
    #' @param alpha A vector of initial probability
    #' @param Q An infinitesimal generator
    #' @param xi An exit rate vector
    #' @return An instance of GPH
    initialize = function(alpha, Q, xi) {
      # check
      if (!(length(alpha) == length(xi) && length(alpha) == dim(Q)[1] && length(alpha) == dim(Q)[2])) {
        stop(sprintf("Error: vector and matrix size are wrong"))
      }
      private$param.alpha <- alpha / sum(alpha)
      diag(Q) <- 0
      diag(Q) <- -(apply(Q, 1, sum) + xi)
      private$param.Q <- as(Q, private$matclass)
      private$param.xi <- xi
      
      # set df
      zero <- 1.0e-8
      private$param.df <- (sum(private$param.alpha > zero) - 1) +
        sum(private$param.Q > zero) +
        sum(private$param.xi > zero)
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      ph(alpha=as.vector(self$alpha()),
         Q=as.matrix(self$Q()),
         xi=as.vector(self$xi()))
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
    #' Moments of GPH
    #' @param k A value to indicate the degrees of moments. k-th moment
    #' @param ... Others
    #' @return A vector of moments from 1st to k-th moments
    moment = function(k, ...) {
      tmp <- self$alpha()
      A <- t(-as.matrix(self$Q()))
      tmp2 <- 1.0
      res <- numeric(0)
      for (i in 1:k) {
        tmp <- solve(a=A, b=tmp)
        tmp2 <- tmp2 * i
        res <- c(res, tmp2 * sum(tmp))
      }
      res
    },
    
    #' @description 
    #' Print
    #' @param ... Others
    print = function(...) {
      cat(gettextf("Size : %d\n", self$size()))
      cat("Initial : ", self$alpha(), "\n")
      cat("Exit    : ", self$xi(), "\n")
      cat("Infinitesimal generator : \n")
      print(self$Q())
    },
    
    #' @description 
    #' PDF
    #' @param x A vector of points
    #' @param poisson.eps A value of tolerance error for uniformization
    #' @param ufactor A value of uniformization factor
    #' @param ... Others
    #' @return A vector of densities.
    #' @note
    #' This function provides the values of p.d.f. for PH distribution with
    #' the uniformization technique.
    pdf = function(x, poisson.eps = 1.0e-8, ufactor = 1.01, ...) {
      alpha <- self$alpha()
      Q <- self$Q()
      xi <- self$xi()
      P <- private$make.matrix()
      inv <- order(order(x))
      sx <- sort(x)
      dt <- c(sx[1], diff(sx))
      y <- phase_dist_pdf(dt, max(dt), alpha, Q, xi, poisson.eps, ufactor, P)
      y[inv]
    },
    
    #' @description 
    #' CDF
    #' @param x A vector of points
    #' @param poisson.eps A value of tolerance error for uniformization
    #' @param ufactor A value of uniformization factor
    #' @param ... Others
    #' @return A vector of probabilities
    #' @note
    #' This function provides the values of c.d.f. for PH distribution with
    #' the uniformization technique.
    cdf = function(x, poisson.eps = 1.0e-8, ufactor = 1.01, ...) {
      alpha <- self$alpha()
      Q <- self$Q()
      P <- private$make.matrix()
      inv <- order(order(x))
      sx <- sort(x)
      dt <- c(sx[1], diff(sx))
      y <- phase_dist_ccdf(dt, max(dt), alpha, Q, poisson.eps, ufactor, P)
      1 - y[inv]
    },

    #' @description 
    #' Complementary CDF
    #' @param x A vector of points
    #' @param poisson.eps A value of tolerance error for uniformization
    #' @param ufactor A value of uniformization factor
    #' @param ... Others
    #' @return A vector of probabilities
    #' @note
    #' This function provides the values of complementary c.d.f. for
    #' PH distribution with the uniformization technique.
    ccdf = function(x, poisson.eps = 1.0e-8, ufactor = 1.01, ...) {
      alpha <- self$alpha()
      Q <- self$Q()
      P <- private$make.matrix()
      inv <- order(order(x))
      sx <- sort(x)
      dt <- c(sx[1], diff(sx))
      y <- phase_dist_ccdf(dt, max(dt), alpha, Q, poisson.eps, ufactor, P)
      y[inv]
    },
    
    #' @description 
    #' Make a sample
    #' @param ... Others
    #' @return A sample of GPH
    sample = function(...) {
      size <- self$size()
      alpha <- self$alpha()
      Q <- self$Q()
      xi <- self$xi()
      s <- which(as.vector(rmultinom(n=1, size=1, prob=c(alpha,0)))==1)
      t <- 0
      while (s != size+1) {
        x <- c(Q[s,], xi[s])
        r <- -x[s]
        p <- x / r
        p[s] <- p[s] + 1
        t <- t + rexp(n=1, rate=r)
        s <- which(as.vector(rmultinom(n=1, size=1, prob=p))==1)
      }
      t
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
      Q <- self$Q()
      xi <- self$xi()
      P <- private$make.matrix()
      H <- private$make.matrix()
      switch(class(data),
             "phase.time" = emfit_gph_wtime(alpha, Q, xi, data, options, P, H),
             "phase.group" = emfit_gph_group(alpha, Q, xi, data, options, P, H)
      )
    },
    
    #' @description 
    #' Initialize with data
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    init = function(data, ...) {
      ph <- gph.param(data, self)
      self$initialize(ph$alpha(), ph$Q(), ph$xi())
    }
  )
)

#' Create GPH distribution
#' 
#' Create an instance of GPH
#' 
#' @param size An integer for the number of phases
#' @param alpha A vector of initial probability
#' @param Q An infinitesimal generator
#' @param xi An exit rate vector
#' @return An instance of GPH
#' 
#' @note
#' This function can omit several patterns of arguments. For example, `ph(5)`
#' omit the arguments `alpha`, `Q` and `xi`. In this case, the default values are
#' assigned to them.
#' 
#' @examples
#' ## create a PH (full matrix) with 5 phases
#' (param1 <- ph(5))
#'
#' ## create a PH (full matrix) with 5 phases
#' (param1 <- ph(size=5))
#'
#' ## create a PH with specific parameters
#' (param2 <- ph(alpha=c(1,0,0),
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               xi=c(2,2,0)))
#'
#' @export

ph <- function(size, alpha, Q, xi) {
  if (missing(size)) {
    if (missing(alpha) || missing(Q) || missing(xi)) {
      stop("alpha, Q and xi are needed.")
    } else {
      size <- length(alpha)
    }
  } else {
    if (!missing(alpha) && !missing(Q) && !missing(xi)) {
      warning("size is ignored.")
      size <- length(alpha)
    } else {
      if (!missing(alpha) || !missing(Q) || !missing(xi)) {
        warning("alpha, Q and xi are ignored.")
      }
      alpha <- rep(1.0/size, size)
      Q <- matrix(1.0, size, size)
      diag(Q) <- rep(-size, size)
      xi <- rep(1.0, size)
    }
  }
  if (missing(xi)) {
    xi = -apply(Q, 1, sum)
  }
  GPHClass$new(alpha=alpha, Q=Q, xi=xi)
}

#' Create a Coxian PH distribution
#' 
#' Create an instance of coxian PH distribution.
#' 
#' @param size An integer for the number of phases
#' @return An instance of coxian PH distribution
#' 
#' @note
#' Coxian PH distribution is the PH distribution whose infinitesimal
#' generator is given by a upper bi-diagonal matrix. This is also called
#' canonical form 3.
#' 
#' @examples
#' ## create a Coxian PH with 5 phases
#' (param1 <- ph.coxian(5))
#'
#' @export

ph.coxian <- function(size) {
  if (size <= 1) {
    ph(size)
  } else {
    alpha <- c(1, rep(0,size-1))
    xi <- rep(1, size)
    Q <- matrix(0, size, size)
    for (i in 1:(size-1)) {
      Q[i,i] <- -2
      Q[i,i+1] <- 1
    }
    Q[size,size] <- -1
    ph(alpha=alpha, Q=Q, xi=xi)
  }
}

#' Create a bi-diagonal PH distribution
#' 
#' Create an instance of bi-diagonal PH distribution.
#' 
#' @param size An integer for the number of phases
#' @return An instance of bi-diagonal PH distribution
#' 
#' @note
#' Bi-diagonal PH distribution is the PH distribution whose infinitesimal
#' generator is given by a upper bi-diagonal matrix. This is similar to
#' canonical form 1. But there is no restriction on the order for diagonal
#' elements. 
#' 
#' @examples
#' ## create a bidiagonal PH with 5 phases
#' (param1 <- ph.bidiag(5))
#'
#' @export

ph.bidiag <- function(size) {
  if (size <= 1) {
    ph(size)
  } else {
    alpha <- rep(1/size,size)
    xi <- rep(0, size)
    Q <- matrix(0, size, size)
    for (i in 1:(size-1)) {
      Q[i,i] <- -1
      Q[i,i+1] <- 1
    }
    Q[size,size] <- -1
    xi[size] <- 1
    ph(alpha=alpha, Q=Q, xi=xi)
  }
}

#' Create a tri-diagonal PH distribution
#' 
#' Create an instance of tri-diagonal PH distribution.
#' 
#' @param size An integer for the number of phases
#' @return An instance of tri-diagonal PH distribution
#' 
#' @note
#' Tri-diagonal PH distribution is the PH distribution whose infinitesimal
#' generator is given by a tri-diagonal matrix (band matrix).
#' 
#' @examples
#' ## create a tridiagonal PH with 5 phases
#' (param1 <- ph.tridiag(5))
#'
#' @export

ph.tridiag <- function(size) {
  if (size <= 2) {
    ph(size)
  } else {
    alpha <- rep(1/size,size)
    xi <- rep(0, size)
    Q <- matrix(0, size, size)
    Q[1,1] <- -1
    Q[1,2] <- 1
    for (i in 2:(size-1)) {
      Q[i,i] <- -2
      Q[i,i-1] <- 1
      Q[i,i+1] <- 1
    }
    Q[size,size-1] <- 1
    Q[size,size] <- -2
    xi[size] <- 1
    ph(alpha=alpha, Q=Q, xi=xi)
  }
}

#' Probability density function of PH distribution
#' 
#' Compute the probability density function (p.d.f.) for a given PH distribution
#' 
#' @param x A numeric vector of quantiles.
#' @param ph An instance of PH distribution.
#' @param log logical; if TRUE, densities y are returned as log(y)
#' @param ... Others.
#' @return A vector of densities.
#' @examples 
#' ## create a PH with specific parameters
#' (phdist <- ph(alpha=c(1,0,0),
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               xi=c(2,2,0)))
#' 
#' ## p.d.f. for 0, 0.1, ..., 1
#' dphase(x=seq(0, 1, 0.1), ph=phdist)
#' 
#' @export

dphase <- function(x, ph, log = FALSE, ...) {
  y <- ph$pdf(x, ...)
  if (log) {
    log(y)
  } else {
    y
  }
}

#' Distribution function of PH distribution
#' 
#' Compute the cumulative distribution function (c.d.f.) for a given PH distribution
#' 
#' @param q A numeric vector of quantiles.
#' @param ph An instance of PH distribution.
#' @param lower.tail logical; if TRUE, probabilities are P(X <= x), otherwise P(X > x).
#' @param log.p logical; if TRUE, probabilities p are returned as log(p).
#' @param ... Others
#' @return A vector of densities
#' @examples 
#' ## create a PH with specific parameters
#' (phdist <- ph(alpha=c(1,0,0),
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               xi=c(2,2,0)))
#' 
#' ## c.d.f. for 0, 0.1, ..., 1
#' pphase(q=seq(0, 1, 0.1), ph=phdist)
#' 
#' @export

pphase <- function(q, ph, lower.tail = TRUE, log.p = FALSE, ...) {
  if (lower.tail) {
    y <- ph$cdf(q, ...)
    if (log.p) {
      log(y)
    } else {
      y
    }
  } else {
    y <- ph$ccdf(q, ...)
    if (log.p) {
      log(y)
    } else {
      y
    }
  }
}

#' Sampling of PH distributions
#' 
#' Generate a sample from a given PH distribution.
#' 
#' @param n An integer of the number of samples.
#' @param ph An instance of PH distribution.
#' @param ... Others
#' @return A vector of samples.
#' @examples 
#' ## create a PH with specific parameters
#' (phdist <- ph(alpha=c(1,0,0),
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               xi=c(2,2,0)))
#' 
#' ## generate 10 samples
#' rphase(n=10, ph=phdist)
#' 
#' @export

rphase <- function(n, ph, ...) {
  sapply(1:n, function(i) ph$sample(...))
}

#' Moments of PH distribution
#' 
#' Generate moments up to k-th moments for a given PH distribution.
#' 
#' @param k An integer for the moments to be computed
#' @param ph An instance of PH distribution
#' @param ... Others
#' @return A vector of moments
#' @examples
#' ## create a PH with specific parameters
#' (param1 <- ph(alpha=c(1,0,0), 
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)), 
#'               xi=c(2,2,0)))
#'
#' ## create a CF1 with specific parameters
#' (param2 <- cf1(alpha=c(1,0,0), rate=c(1.0,2.0,3.0)))
#' 
#' ## create a hyper Erlang with specific parameters
#' (param3 <- herlang(shape=c(2,3), mixrate=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## up to 5 moments 
#' ph.moment(5, param1)
#' ph.moment(5, param2)
#' ph.moment(5, param3)
#' 
#' @export

ph.moment <- function(k, ph, ...) {
  ph$moment(k, ...)
}

#' Mean of PH distribution
#' 
#' Compute the mean of a given PH distribution.
#' 
#' @param ph An instance of PH distribution
#' @param ... Others
#' @return A value of mean
#' @examples
#' ## create a PH with specific parameters
#' (param1 <- ph(alpha=c(1,0,0), 
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)), 
#'               xi=c(2,2,0)))
#'
#' ## create a CF1 with specific parameters
#' (param2 <- cf1(alpha=c(1,0,0), rate=c(1.0,2.0,3.0)))
#' 
#' ## create a hyper Erlang with specific parameters
#' (param3 <- herlang(shape=c(2,3), mixrate=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## mean
#' ph.mean(param1)
#' ph.mean(param2)
#' ph.mean(param3)
#' 
#' @export

ph.mean <- function(ph, ...) {
  ph$moment(1, ...)
}

#' Variance of PH distribution
#' 
#' Compute the variance of a given PH distribution.
#' 
#' @param ph An instance of PH distribution
#' @param ... Others
#' @return A value of variance
#' @examples
#' ## create a PH with specific parameters
#' (param1 <- ph(alpha=c(1,0,0), 
#'               Q=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)), 
#'               xi=c(2,2,0)))
#'
#' ## create a CF1 with specific parameters
#' (param2 <- cf1(alpha=c(1,0,0), rate=c(1.0,2.0,3.0)))
#' 
#' ## create a hyper Erlang with specific parameters
#' (param3 <- herlang(shape=c(2,3), mixrate=c(0.3,0.7), rate=c(1.0,10.0)))
#' 
#' ## variance
#' ph.var(param1)
#' ph.var(param2)
#' ph.var(param3)
#' 
#' @export

ph.var <- function(ph, ...) {
  x <- ph$moment(2, ...)
  x[2] - x[1]^2
}


#' Generate GPH using the information on data
#' 
#' Generate GPH randomly and adjust parameters to fit its first moment to
#' the first moment of data.
#' 
#' @param data A dataframe
#' @param skel An instance of skeleton of GPH.
#' @param ... Others
#' @return An instance of GPH
#' @examples 
#' ## Create data
#' wsample <- rweibull(10, shape=2)
#' (dat <- data.frame.phase.time(x=wsample))
#' 
#' ## Generate PH that is fitted to dat
#' (model <- gph.param(data=dat, skel=ph(5)))
#' 
#' @export

gph.param <- function(data, skel, ...) {
  size <- skel$size()
  alpha <- skel$alpha()
  Q <- skel$Q()
  xi <- skel$xi()
  
  alpha <- alpha * runif(size)
  alpha <- alpha / sum(alpha)
  Q <- as.matrix(Q) * matrix(runif(size*size), size, size)
  xi <- xi * runif(size)
  diag(Q) <- 0
  diag(Q) <- -(apply(Q, 1, sum) + xi)
  scale <- sum(solve(a=-t(Q), b=alpha)) / mean(data)
  Q <- Q * scale
  xi <- xi * scale
  ph(alpha=alpha, Q=Q, xi=xi)
}

