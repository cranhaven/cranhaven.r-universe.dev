# Filename: mosallocStepwiseFirst.R
#
# Date: 07.07.2025
# Author: Felix Willems
# Contact: mail.willemsf+MOSAlloc@gmail.com
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
# Licensing: GPL-3.0-or-later
#
# Please report any bugs or unexpected behavior to
# mail.willemsf+MOSAlloc@gmail.com 
# (mail[DOT]willemsf+MOSAlloc[AT]gmail[DOT]com)
#
#---------------------------------------------------------------------------
#
#' @title Multiobjective sample allocation for constraint multivariate and
#' multidomain optimal allocation in survey sampling (a stepwise optimality
#' procedure is processed first to force Pareto optimality of the solution)
#'
#' @description Computes solutions to standard sample allocation problems
#' under various precision and cost restrictions. The input data is
#' transformed and parsed to the Embedded COnic Solver (ECOS) from the
#' 'ECOSolveR' package. Multiple survey purposes are optimized
#' simultaneously through a stepwise weighted Chebyshev minimization which
#' forces Pareto optimality of solutions (cf. \code{mosalloc()}).
#'
#' @param D (type: matrix)
#' The objective matrix. A matrix of either precision or cost units.
#' @param d (type: vector)
#' The objective vector. A vector of either fixed precision components
#' (e.g. finite population corrections) or fixed costs.
#' @param A (type: matrix)
#' A matrix of precision units for precision constraints.
#' @param a (type: vector)
#' The right-hand side vector of the precision constraints.
#' @param C (type: matrix)
#' A matrix of cost coefficients for cost constraints
#' @param c (type: vector)
#' The right-hand side vector of the cost constraints.
#' @param l (type: vector)
#' A vector of lower box constraints.
#' @param u (type: vector)
#' A vector of upper box constraints.
#' @param opts (type: list)
#' The options used by the algorithms:
#' \cr \code{$sense} (type: character) Sense of optimization
#' (default = \code{"max_precision"}, alternative \code{"min_cost"}).
#' \cr \code{$init_w} (type numeric or matrix) Preference weightings
#' (default = 1; The weight for first objective component must be 1).
#' \cr \code{$mc_cores} (type: integer) The number of cores for parallelizing
#' multiple input weightings stacked rowwise (default = 1L).
#' \cr \code{max_iters} (type: integer) The maximum number of iterations
#' (default = 100L).
#'
#' @references
#' See:
#'
#' Willems, F. (2025). A Framework for Multiobjective and Uncertain Resource
#' Allocation Problems in Survey Sampling based on Conic Optimization
#' (Doctoral dissertation). Trier University.
#' \doi{10.25353/ubtr-9200-484c-5c89}.
#'
#' @return The function \code{mosallocStepwiseFirst()} returns a list
#' containing the following components:
#' @returns \code{$w} The initial preference weighting \code{opts$init_w}.
#' @returns \code{$n} The vector of optimal sample sizes.
#' @returns \code{$J} The optimal objective vector.
#' @returns \code{$Objective} The objective value with respect to decision
#' functional f. \code{NULL} if \code{opts$f = NULL}.
#' @returns \code{$Utopian} Always \code{NULL} (consistency to \code{mosalloc()}
#' output). \code{NULL} if \code{opts$f = NULL}.
#' @returns \code{$Normal} The vector normal to the Pareto frontier at
#' \code{$J}.
#' @returns \code{$dfJ} Always \code{NULL} (consistency to \code{mosalloc()}
#' output).
#' @returns \code{$Sensitivity} The dual variables of the objectives and
#' constraints.
#' @returns \code{$Qbounds} The Quality bounds of the Lorentz cones.
#' @returns \code{$Dbounds} The weighted objective constraints ($w * $J).
#' @returns \code{$Scalepar} An internal scaling parameter.
#' @returns \code{$Ecosolver} A list of ECOSolveR returns including:
#' \cr \code{...$Ecoinfostring} The info string of
#' \code{ECOSolveR::ECOS_csolve()}.
#' \cr \code{...$Ecoredcodes} The redcodes of \code{ECOSolveR::ECOS_csolve()}.
#' \cr \code{...$Ecosummary} Problem summary of \code{ECOSolveR::ECOS_csolve()}.
#' @returns \code{$Timing} Run time info.
#' @returns \code{$Iteration} Always \code{NULL} (consistency to
#' \code{mosalloc()} output).
#'
#' @examples
#' # Artificial population of 50 568 business establishments and 5 business
#' # sectors (data from Valliant, R., Dever, J. A., & Kreuter, F. (2013).
#' # Practical tools for designing and weighting survey samples. Springer.
#' # https://doi.org/10.1007/978-1-4614-6449-5, Example 5.2 pages 133-9)
#'
#' # See also https://umd.app.box.com/s/9yvvibu4nz4q6rlw98ac/file/297813512360
#' # file: Code 5.3 constrOptim.example.R
#'
#' Nh <- c(6221, 11738, 4333, 22809, 5467) # stratum sizes
#' ch <- c(120, 80, 80, 90, 150) # stratum-specific cost of surveying
#'
#' # Revenues
#' mh.rev <- c(85, 11, 23, 17, 126) # mean revenue
#' Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0) # standard deviation revenue
#'
#' # Employees
#' mh.emp <- c(511, 21, 70, 32, 157) # mean number of employees
#' Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00) # std. dev. employees
#'
#' # Proportion of estabs claiming research credit
#' ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)
#'
#' # Proportion of estabs with offshore affiliates
#' ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)
#'
#' budget <- 300000 # overall available budget
#' n.min  <- 100 # minimum stratum-specific sample size
#'
#' #----------------------------------------------------------------------------
#' # Problem: Minimization of the maximum relative variation of estimates for
#' # the total revenue, the number of employee, the number of businesses claimed
#' # research credit and the number of businesses with offshore affiliates
#' # subject to cost restrictions
#'
#' l <- rep(n.min, 5) # minimum sample size ber stratum
#' u <- Nh            # maximum sample size per stratum
#' C <- rbind(ch, ch * c(-1, -1, -1, 0, 0))
#' c <- c(budget, - 0.5 * budget)
#' A <- NULL # no precision constraint
#' a <- NULL # no precision constraint
#'
#' # Variance components for multidimensional objective
#' D <- rbind(Sh.rev**2 * Nh**2/sum(Nh * mh.rev)**2,
#'            Sh.emp**2 * Nh**2/sum(Nh * mh.emp)**2,
#'            ph.rsch * (1 - ph.rsch) * Nh**3/(Nh - 1)/sum(Nh * ph.rsch)**2,
#'            ph.offsh * (1 - ph.offsh) * Nh**3/(Nh - 1)/sum(Nh * ph.offsh)**2)
#'
#' d <- as.vector(D %*% (1 / Nh)) # finite population correction
#'
#' opts = list(sense = "max_precision",
#'             init_w = 1,
#'             mc_cores = 1L,
#'             max_iters = 100L)
#'
#' res1 <- mosallocStepwiseFirst(D = D, d = d, C = C, c = c, l = l, u = u,
#'                               opts = opts)
#' w <- res1$J[1] / res1$J
#' w # [1] 1.000000 3.879692 2.653655 1.000000
#'
#' opts = list(sense = "max_precision",
#'             init_w = w,
#'             mc_cores = 1L,
#'             max_iters = 100L)
#'
#' res2 <- mosallocStepwiseFirst(D = D, d = d, C = C, c = c, l = l, u = u,
#'                               opts = opts)
#' res2$w # [1] 1.000000 3.879692 2.653655 1.000000
#'
#' # Compare to function mosalloc (without stepwise procedure)
#' opts = list(sense = "max_precision",
#'             f = NULL, df = NULL, Hf = NULL,
#'             init_w = w,
#'             mc_cores = 1L, pm_tol = 1e-05,
#'             max_iters = 100L, print_pm = FALSE)
#' res3 <- mosalloc(D = D, d = d, C = C, c = c, l = l, u = u, opts = opts)
#'
#' # Compare objectives
#' rbind(res1$J, res2$J, res3$J)
#' #            [,1]         [,2]         [,3]       [,4]
#' #[1,] 0.00170589 0.0004396972 0.0006428453 0.00170589
#' #[2,] 0.00170589 0.0004396971 0.0006428420 0.00170589
#' #[3,] 0.00170589 0.0004396971 0.0006428440 0.00170589
#'
#' # Compare optimal sample sizes
#' rbind(res1$n, res2$n, res3$n)
#' #          [,1]     [,2]     [,3]     [,4]     [,5]
#' # [1,] 958.0510 290.7446 147.1789 602.8856 638.2686
#' # [2,] 958.0455 290.7447 147.1871 602.8847 638.2692
#' # [3,] 958.0488 290.7446 147.1822 602.8853 638.2688
#'
#' @export
mosallocStepwiseFirst <- function(D, d, A = NULL, a = NULL, C = NULL, c = NULL,
                                   l = 2, u = NULL,
                                   opts = list(sense = "max_precision",
                                               init_w = 1,
                                               mc_cores = 1L,
                                               max_iters = 100L)) {
  time_t0 <- proc.time()

  # check input
  #-----------------------------------------------------------------------------

  # check if package parallel is available; if not, never try parallelization
  if (nchar(system.file(package = "parallel")) == 0) {
    opts$mc_cores <- 1L
  }

  # get sense of optimization (precision maximization or cost minimization)
  if (opts$sense == "max_precision") {
    ksense <- ncol(D)
    if (is.null(C) | is.null(c)) {
      stop("No cost constraint specified!")
    }
  } else if (opts$sense == "min_cost") {
    stop("Function not implemente for cost objectives!")
  } else {
    stop("No or invalid 'sense'!")
  }

  # check Objective matrix and vector
  if (!is.matrix(D)) {
    stop("D is not a matrix!")
  } else {
    # get number of strata H and number of variates Q
    H <- ncol(D)
    Q <- nrow(D)
    if (!(all(rowSums(D) > 0) & all(colSums(D) > 0))) {
      stop("D is not a valid input matrix!")
    }
  }

  if (!is.vector(d)) {
    stop("d is not a vector!")
  }else{
    if (length(d) != Q) {
      stop("Dimension of D and d do not match!")
    }
    if (any(d < 0)) {
      stop("d is negative!")
    }
  }

  # precision constraints
  if (is.null(A)) {
    A <- matrix(NA, nrow = 0, ncol = H)
    a <- c()
  } else {
    if (!is.matrix(A)) {
      stop("A is not a matrix!")
    } else {
      if (ncol(A) != H) {
        stop("Worng dimensin of A!")
      }
      if (any(A < 0)) {
        stop("Negative accuracies. This is not valid!")
      }
      if (!is.vector(a)) {
        stop("a is not a vector!")
      } else {
        if(nrow(A) != length(a)){
          stop("Dimension of A and a do not match!")
        }
        if (any(a <= 0)) {
          stop("a is not problem compatible (nonpositivity)!")
        }
      }
    }
  }

  # cost constraints
  if (is.null(C)) {
    C <- matrix(NA, nrow = 0, ncol = H)
    c <- c()
  } else {
    if (!is.matrix(C)) {
      stop("C is not a matrix!")
    } else {
      if (ncol(C) != H) {
        stop("Worng dimensin of C!")
      }
      if (!is.vector(c)) {
        stop("c is not a vector!")
      } else {
        if(nrow(C) != length(c)){
          stop("Dimension of C and c do not match!")
        }
      }
    }
  }

  # box constraints must be specified to ensure existence of solutions
  if (is.null(u)) {
    stop("No upper box-constraints defined!")
  } else {
    if (!is.vector(u)) {
      stop("u is not a vector!")
    }
    if (length(u) != H) {
      stop("Wrong dimension of u!")
    }
  }

  if (!is.vector(l)) {
    stop("l is not a vector!")
  } else {
    if(length(l) == 1) {
      l <- as.vector(rep(l, H))
    } else {
      if (length(l) != H) {
        stop("Wrong dimension of l!")
      }
    }
    if (any(l < 1)) {
      stop("Invalid problem structure! l has to be greater or equal to 1!")
    }
    if (any(u < l)) {
      stop("Invalid problem structure! u has to be greater or equal to l!")
    }
  }

  # check weights
  if (is.vector(opts$init_w)) {
    if (length(opts$init_w) == 1) {
      w <- matrix(rep(1, Q), 1, Q)
    } else {
      if (length(opts$init_w) != Q) {
        stop("Wrongnumber of weigths!")
      }
      if (opts$init_w[1] != 1) {
        stop("First weight component is not 1!")
      }
      w <- matrix(opts$init_w, 1, Q)
    }
  } else if (is.matrix(opts$init_w)) {
    if (ncol(opts$init_w) != Q) {
      stop("Wrongnumber of weigths!")
    }
    if (any(opts$init_w[, 1] != 1)) {
      stop("First weight component is not 1!")
    }
    w <- opts$init_w
  } else {
    stop("Weights are not correctly specified!")
  }
  if (any(w <= 0)) {
    stop("Nonpositive weights detected!")
  }

  # scale input data and built problem matrix
  #-----------------------------------------------------------------------------
  if (opts$sense == "max_precision") {
    if (any(as.vector(D %*% (1 / u)) - d < 0) & any(
      rowSums(D / rep(u, each=dim(D)[1])) - d < 0)) {
        stop("d is not an utopian vector! d is too large.")
    }

    ecos_control <- ECOSolveR::ecos.control(
      maxit = as.integer(max(floor(log(ncol(D))**1.5) + 1, 35,
                             opts$max_iters / 10)),
      FEASTOL <- 1e-10,
      ABSTOL <- 1e-8,
      RELTOL <- 1e-8
    )

    kD <- nrow(D) # No. of objectives
    kA <- nrow(A) # No. of precision constraints
    kC <- nrow(C) # No. of cost constraints excluding box constraints

    # standardize input data
    Dsc <- D / min(D %*% (1 / u))
    dsc <- d / min(D %*% (1 / u))

    if (all(d == 0)) {
      d0 <- d
      dsc <- rep(1, kD)
    } else {
      Dsc <- Dsc / dsc
      d0 <- rep(1, kD)
    }

    Asc <- A / a
    Csc <- C / c * sign(c)

    # presolve via Neyman-Tschuprow allocation to estimate range of sample sizes
    sDsc    <- sqrt(Dsc)
    sCsc    <- sqrt(abs(Csc))
    neyman  <- colSums(sDsc)/colSums(sCsc) / sum(colSums(sDsc) * colSums(sCsc))
    scale_n <- pmax(l + 0.1, pmin(neyman, u - 0.5))
    ny      <- sum(neyman) / sum(scale_n)

    scale_nexp <- scale_n * ny / exp(mean(log((scale_n))))

    # scale problem data
    scaleV   <- rbind(Dsc, Asc)
    zscale   <- exp(mean(log(scaleV[scaleV != 0])))
    scaleC   <- rbind(Csc)
    nscale   <- exp(mean(log(abs(scaleC[scaleC != 0]))))
    scale_nz <- exp((log(nscale) + log(zscale)) / 2) / zscale
    scalepar <- scale_nz / scale_nexp

    Dsc <- Dsc * rep(scalepar, each = dim(Dsc)[1])
    Asc <- Asc * rep(scalepar, each = dim(Asc)[1])
    Csc <- Csc / rep(scalepar, each = dim(Csc)[1])

    tsc <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (1 / scale_n / scalepar) - d0))) * tsc

  } else {

    ecos_control <- ECOSolveR::ecos.control(
      maxit = as.integer(max(floor(log(ncol(D))**1.5) + 1, 35,
                           opts$max_iters / 10)),
      FEASTOL <- 1e-10,
      ABSTOL <- 1e-8,
      RELTOL <- 1e-8)

    kD <- nrow(D) # No. of objectives
    kA <- nrow(A) # No. of precision constraints
    kC <- nrow(C) # No. of cost constraints excluding box constraints

    # standardize input data
    Dsc <- D / min(D %*% l)
    dsc <- d / min(D %*% l)

    if (all(d == 0)) {
      d0 <- -d
      dsc <- rep(1, kD)
    } else {
      Dsc <- Dsc / dsc
      d0 <- rep(-1, kD)
    }

    Asc <- A / a
    if (is.null(c)) {
      Csc <- C / c
    } else {
      Csc <- C / c * sign(c)
    }

    # presolve via Neyman-Tschuprow allocation to estimate range of sample sizes
    sDsc    <- sqrt(abs(Dsc))
    sAsc    <- sqrt(Asc)
    neyman  <- colSums(sAsc)/colSums(sDsc) / sum(colSums(sAsc) * colSums(sDsc))
    scale_n <- pmax(l + 0.1, pmin(neyman, u - 0.5))
    ny      <- sum(neyman) / sum(scale_n)

    scale_nexp <- scale_n * ny / exp(mean(log((scale_n))))

    # scale problem data
    scaleV   <- Asc
    zscale   <- exp(mean(log(scaleV[scaleV != 0])))
    scaleC   <- rbind(Dsc, Csc)
    nscale   <- exp(mean(log(abs(scaleC[scaleC != 0]))))
    scale_nz <- exp((log(nscale) + log(zscale)) / 2) / zscale
    scalepar <- scale_nz / scale_nexp

    Dsc <- Dsc / rep(scalepar, each = dim(Dsc)[1])
    Asc <- Asc * rep(scalepar, each = dim(Asc)[1])
    Csc <- Csc / rep(scalepar, each = dim(Csc)[1])

    tsc <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (scale_n / scalepar) - d0))) * tsc
  }

  # construct sparse data matrix for large-scale problems
  A_idx <- list(getColRowVal(Dsc, ksense, 0),
                list(rep(H * 2 + 1, kD), 1:kD,
                     -scale_t / as.vector(dsc)),
                getColRowVal(Asc, H, kD),
                getColRowVal(Csc, 0, kD + kA),
                list(1:H, 1:H + kC + kD + kA, -1 / l / scalepar),
                list(1:H, 1:H + H + kC + kD + kA, 1 / u / scalepar),
                list(rep(1:H, rep(2, H)),
                     (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA, rep(-1, 2 * H)),
                list(rep(1:H + H, rep(2, H)),
                     (1:(3*H))[-(1:H * 3)] + H * 2 + kC + kD + kA,
                     rep(c(-1, 1), H)))

  A_init <- Matrix::sparseMatrix(i = unlist(lapply(A_idx, function(X) X[[2]])),
                                 j = unlist(lapply(A_idx, function(X) X[[1]])),
                                 x = unlist(lapply(A_idx, function(X) X[[3]])),
                                 dims = c(H * 5 + kC + kD + kA, 2 * H + 1))

  # construct right hand-side
  b_init <- matrix(0, 1, kA + kC + kD + 5 * H)
  b_init[1:(kD + kA + kC + 2 * H)] <- sign(c(d0, a, c, -l, u))
  b_init[seq(1, 3 * H, 3) + kA + kC + 2 * H + kD + 2] <- 2

  # presolve, scale test and feasibility check
  sol <- ECOSolveR::ECOS_csolve(
    c = c(matrix(0, 1, 2 * H), 1),
    G = A_init,
    h = b_init,
    dims = list(l = nrow(A_init) - 3 * H, q = matrix(3, H, 1), e = 0),
    A = NULL,
    b = numeric(0),
    bool_vars = integer(0),
    int_vars = integer(0),
    control = ecos_control
  )
  # check primal feasibility
  if (sol$retcodes[1] == 1) {
    warning("Allocation problem is infeasible!")
  }
  # check quality bounds of Lorentz cones
  if (any(sol$x[1:H] * sol$x[1:H + H] - 1 > 1e-6)) {
    # rescale problem according to the solution of the presolve
    ecos_control$MAXIT <- as.integer(opts$max_iters)
    ecos_control$FEASTOL <- 1e-10
    ecos_control$ABSTOL <- 1e-9
    ecos_control$RELTOL <- 1e-9
    scaleadd <- 10**mean(log(sol$x[1:H], 10))

    Dsc <- Dsc / rep(scaleadd, each = dim(Dsc)[1])
    Asc <- Asc / rep(scaleadd, each = dim(Asc)[1])
    Csc <- Csc * rep(scaleadd, each = dim(Csc)[1])
    scalepar <- scalepar / scaleadd
    tsc     <-  exp(mean(abs(log(1 / dsc))))
    scale_t <- exp(mean(log(Dsc %*% (scale_n / scalepar) - d0))) * tsc / 10

    # construct sparse data matrix
    A_idx <- list(getColRowVal(Dsc, ksense, 0),
                  list(rep(H * 2 + 1, kD), 1:kD,
                       -scale_t / as.vector(dsc)),
                  getColRowVal(Asc, H, kD),
                  getColRowVal(Csc, 0, kD + kA),
                  list(1:H, 1:H + kC + kD + kA, -1 / l / scalepar),
                  list(1:H, 1:H + H + kC + kD + kA, 1 / u / scalepar),
                  list(rep(1:H, rep(2, H)),
                       (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA,
                       rep(-1, 2*H)),
                  list(rep(1:H + H, rep(2, H)),
                       (1:(3*H))[-(1:H*3)] + H*2 + kC + kD + kA,
                       rep(c(-1, 1), H)))

    A_init <- Matrix::sparseMatrix(i = unlist(lapply(A_idx,
                                                     function(X) X[[2]])),
                                   j = unlist(lapply(A_idx,
                                                     function(X) X[[1]])),
                                   x = unlist(lapply(A_idx,
                                                     function(X) X[[3]])),
                                   dims = c(H * 5 + kC + kD + kA, 2 * H + 1))
  }

  # Calculate stepwise solution for given weight matrix w
  #-----------------------------------------------------------------------------
  time_t1 <- proc.time()

  # Solve worst-case problem 
  wstd <- exp(mean(log(1 / w[1, ])))
  w2a <- wstd * w[1, ]
  scalet <- as.vector(w2a) * as.vector(dsc)
  A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(scalet)
  w2wcsol <- wcSolquick(A_init, b_init, Q, H, ecos_control)

  # Get corresponding optimal weight
  if (sum(A_init[1:kD, 1]) == 0) {
    Jopt <- D %*% matrix(scalepar / w2wcsol) - d
  } else {
    Jopt <- D %*% matrix(w2wcsol / scalepar) - d
  }
  wopt <- as.vector(Jopt[1] / Jopt)

  sol <- wECOSCsolve(w = wopt, dsc = dsc, D = D, d = d, a = a, c = c,
                     l = l, u = u, scalepar = scalepar, scale_t = scale_t,
                     A_init = A_init, b_init = b_init,
                     ecos_control = ecos_control,
                     t0 = time_t0, t1 = time_t1)
  sol$w <- w[1, ]

  if (nrow(w) > 1) {
    if(opts$mc_cores == 1) {
      SOL <- lapply(2:nrow(w), function(i) {
        wstd <- exp(mean(log(1 / w[i, ])))
        w2a <- wstd * w[i, ]
        scalet <- as.vector(w2a) * as.vector(dsc)
        A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(scalet)
        w2wcsol <- wcSolquick(A_init, b_init, Q, H, ecos_control)

        # get corresponding optimal weight
        if (sum(A_init[1:kD, 1]) == 0) {
          Jopt <- D %*% matrix(scalepar / w2wcsol) - d
        } else {
          Jopt <- D %*% matrix(w2wcsol / scalepar) - d
        }
        wopt <- as.vector(Jopt[1] / Jopt)

        # compute solution
        sol <- wECOSCsolve(w = wopt, dsc = dsc, D = D, d = d, a = a, c = c,
                           l = l, u = u, scalepar = scalepar, scale_t = scale_t,
                           A_init = A_init, b_init = b_init,
                           ecos_control = ecos_control,
                           t0 = time_t0, t1 = time_t1)
        sol$w <- w[i, ]
        return(sol)
      })
    } else{
      SOL <- parallel::mclapply(2:nrow(w), function(i) {
        wstd <- exp(mean(log(1 / w[i, ])))
        w2a <- wstd * w[i, ]
        scalet <- as.vector(w2a) * as.vector(dsc)
        A_init[1:kD, 2 * H + 1] <- -1 * scale_t / as.vector(scalet)
        w2wcsol <- wcSolquick(A_init, b_init, Q, H, ecos_control)

        # get corresponding optimal weight
        if (sum(A_init[1:kD, 1]) == 0) {
          Jopt <- D %*% matrix(scalepar / w2wcsol) - d
        } else {
          Jopt <- D %*% matrix(w2wcsol / scalepar) - d
        }
        wopt <- as.vector(Jopt[1] / Jopt)

        # compute solution
        sol <- wECOSCsolve(w = wopt, dsc = dsc, D = D, d = d, a = a, c = c,
                           l = l, u = u, scalepar = scalepar, scale_t = scale_t,
                           A_init = A_init, b_init = b_init,
                           ecos_control = ecos_control,
                           t0 = time_t0, t1 = time_t1)
        sol$w <- w[i, ]
        return(sol)
      }, mc.cores = opts$mc_cores)
    }
    SOL <- append(SOL, list(sol), after = 0)
    return(SOL)
  } else {
    return(sol)
  }
}