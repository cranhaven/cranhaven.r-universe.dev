#' Select control units that optimize covariate balance
#'
#' Select control units within strata that optimize covariate balance.
#' Uses randomized rounding of a linear program or a mixed
#' integer linear program.
#'
#' @inheritParams stand
#' @param st a stratum vector with the \code{i}th entry equal to the
#'   stratum of unit \code{i}. This should have the same order of units and length
#'   as \code{z}.
#' @param X a matrix or data frame containing constraints in the columns. The number
#'   of rows should equal the length of \code{z}. Balance is achieved when a constraint
#'   sums to 0, such that numbers closer to 0 are better. When a constraint
#'   does not apply to a particular unit, the entry should be \code{NA}.
#'   This should typically be generated using \code{\link{generate_constraints}()}.
#' @param treated_star which treatment value should be considered the treated units
#' for the supplemental comparison. This
#' must be one of the values of \code{z}.
#'  If multiple supplemental comparisons are desired, this should be a vector with one entry per supplemental
#'   comparison.
#' @param ratio a numeric or vector specifying the desired ratio of controls to `treated` in
#'   each stratum. If there is one control group and all treated units should be included,
#'   this can be a numeric. Otherwise, this should be
#'   a vector with one entry per treatment group, in the same order as the levels of
#'   \code{z}, including the treated level. If \code{NULL}, \code{q_s} should be specified.
#' @param q_s a named vector or matrix indicating how many units are to be selected from each stratum.
#'   If there is one control group and all treated units are desired, this can be a vector; otherwise,
#'   this should have one row per treatment group, where the order of the rows matches the order of
#'   the levels of \code{z}, including the treated level.
#'   If \code{NULL}, \code{ratio} should be specified. If both are specified, \code{q_s} will take priority.
#'   Typically, if the desired ratio is not feasible for every stratum, \code{q_s} should be generated
#'   using \code{\link{generate_qs}()}.
#' @param q_star_s a named vector or matrix,
#'   indicating how many supplemental units are to be selected from each stratum.
#'   The matrix should have one row per treatment group, where the order of the rows matches the order of
#'   the levels of \code{z}, including the treated level.
#'   If multiple supplemental comparisons are desired, this should be a list with one entry per supplemental
#'   comparison.
#' @param weight_star a numeric stating how much to prioritize balance between the supplemental units as
#' compared to balance between the main units.
#'   If multiple supplemental comparisons are desired, this should be a vector with one entry per supplemental
#'   comparison.
#' @param importances a vector with length equal to the number of constraints or columns
#'   in \code{X}. This can be generated using \code{\link{generate_constraints}()} and each nonnegative value
#'   denotes how much to prioritize each constraint, with the default being 1
#'   for all constraints.
#' @param integer a logical stating whether to use a mixed integer programming solver
#'   instead of randomized rounding. Default is \code{FALSE}.
#' @param solver a character stating which solver to use to run the linear program.
#'   Options are "Rglpk" (default) or "gurobi". You must have the 'gurobi' package
#'   installed to use the "gurobi" option. If available, this is the recommended solver.
#' @param seed the seed to use when doing the randomized rounding of the linear program.
#'   This will allow results to be reproduced if desired. The default is \code{NULL},
#'   which will choose a random seed to use and return.
#' @param runs the number of times to run randomized rounding of the linear solution.
#'   The objective values of all runs will be reported, but the detailed results
#'   will only be reported for the run with the lowest objective value. The default is 1.
#' @param time_limit numeric stating maximum amount of seconds for which the
#'   program is allowed to run before aborting. Default is \code{Inf} for no time limit.
#' @param correct_sizes boolean stating whether the desired sample sizes should
#'  be exactly correct (if \code{correct_sizes = TRUE}) or only need to be correct
#'  in expectation. For multiple comparisons, sample sizes may only be
#'  correct in expectation.
#' @param low_memory boolean stating whether some outputs should not be included
#'  due to the scale of the problem being too large compared to memory space.
#'  If \code{TRUE}, \code{eps} and \code{eps_star} will not be reported. Imbalances
#'  can be computed post hoc using the \code{\link{check_balance}()} instead.
#' @param threads The maximum number of threads that should be used. This is only
#'  applicable if \code{solver = 'gurobi'}.
#'
#' @return List containing:
#' \describe{
#'   \item{\code{objective}}{objective value of the randomized rounding or mixed integer
#'     linear program solution.}
#'   \item{\code{objective_wo_importances}}{objective value of the randomized rounding or mixed integer
#'     linear program solution not weighted by the variable importances.}
#'   \item{\code{eps}}{the amount of imbalance obtained in each constraint from the linear program.
#'   The row names specify the covariate, the population of interest, and, if there are
#'   more than two comparison groups, which groups are being compared.}
#'   \item{\code{eps_star}}{same as \code{eps} but for the supplemental units instead of the units
#'   in the main comparison. If there are multiple supplemental comparisons, this is a list.
#'   If there are none, this is \code{NULL}.}
#'   \item{\code{importances}}{the importance of each on the balance constraints.}
#'   \item{\code{weight_star}}{the importance of balancing in the supplemental comparison
#'   relative to the main comparison. If there are multiple supplemental comparisons,
#'   this is a vector. If there are none, this is \code{NULL}.}
#'   \item{\code{selected}}{whether each unit was selected for the main comparison.}
#'   \item{\code{selected_star}}{whether each unit was selected for the supplement.
#'   If there are multiple supplemental comparisons, this is a list.
#'   If there are none, this is \code{NULL}.}
#'   \item{\code{pr}}{the linear program weight assigned to each unit for the main comparison.}
#'   \item{\code{pr_star}}{the linear program weight assigned to each unit for the supplement.
#'   If there are multiple supplemental comparisons, this is a list.
#'   If there are none, this is \code{NULL}.}
#'   \item{\code{rrdetails}}{A list containing:
#'   \describe{
#'   \item{\code{seed}}{the seed used before commencing the random sampling.}
#'   \item{\code{run_objectives}}{the objective values for each run of randomized rounding.}
#'   \item{\code{run_objectives_wo_importances}}{the objective values for each run of randomized rounding,
#'   not scaled by constraint importances.}
#'   }}
#'   \item{\code{lpdetails}}{the full return of the function \code{\link[Rglpk]{Rglpk_solve_LP}()}
#'     or \code{gurobi()} plus information about the epsilons and objective values
#'     for the linear program solution.}
#' }
#'
#' @importFrom stats complete.cases
#' @export
#'
#' @examples
#'
#' data('nh0506')
#'
#' # Create strata
#' age_cat <- cut(nh0506$age,
#'                breaks = c(19, 39, 50, 85),
#'                labels = c('< 40 years', '40 - 50 years', '> 50 years'))
#' strata <- age_cat : nh0506$sex
#'
#' # Balance age, race, education, poverty ratio, and bmi both across and within the levels of strata
#' constraints <- generate_constraints(
#'                  balance_formulas = list(age + race + education + povertyr + bmi ~ 1 + strata),
#'                  z = nh0506$z,
#'                  data = nh0506)
#'
#' # Choose one control for every treated unit in each stratum,
#' # balancing the covariates as described by the constraints
#' results <- optimize_controls(z = nh0506$z,
#'                              X = constraints$X,
#'                              st = strata,
#'                              importances = constraints$importances,
#'                              ratio = 1)
#'
#' # If you want to use a ratio that's not feasible,
#' # you can supply a vector of the desired number of controls per stratum, q_s,
#' # typically generated by creating a distance matrix between strata and using
#' # generate_qs():
#'
#' \dontrun{
#' age_dist <- matrix(data = c(0, 1, 2, 1, 0, 1, 2, 1, 0),
#'                    nrow = 3,
#'                    byrow = TRUE,
#'                    dimnames = list(levels(age_cat), levels(age_cat)))
#'
#' sex_dist <- matrix(data = c(0, 1, 1, 0),
#'                    nrow = 2,
#'                    dimnames = list(levels(nh0506$sex), levels(nh0506$sex)))
#'
#' strata_dist <- create_dist_matrix(age_dist, sex_dist)
#'
#' qs <- generate_qs(z = nh0506$z,
#'                   st = strata,
#'                   ratio = 2.5,
#'                   max_ratio = 2.6,
#'                   max_extra_s = 0,
#'                   strata_dist = strata_dist)
#'
#' results <- optimize_controls(z = nh0506$z,
#'                              X = constraints$X,
#'                              st = strata,
#'                              importances = constraints$importances,
#'                              q_s = qs)
#'
#' }
#'
#' # We can also have multiple treatment and control groups,
#' # as well as multiple simultaneous comparisons:
#'
#' \dontrun{
#' data('nh0506_3groups')
#' strata2 <- cut(nh0506_3groups$age, breaks = c(19, 39, 50, 85),
#'               labels = c('< 40 years', '40 - 50 years', '> 50 years'))
#' constraints2 <- generate_constraints(
#'   balance_formulas = list(age + race + education + povertyr + bmi + sex ~ 1 + strata2),
#'   z = nh0506_3groups$z,
#'   data = nh0506_3groups,
#'   treated = 'daily smoker')
#' q_star_s <- matrix(c(rep(table(nh0506_3groups$z, strata2)['some smoking', ] -
#'                            table(nh0506_3groups$z, strata2)['daily smoker', ], 2),
#'                      rep(0, 3)), byrow = TRUE, nrow = 3,
#'                    dimnames = list(levels(nh0506_3groups$z), levels(strata2)))
#'
#' results <- optimize_controls(z = nh0506_3groups$z,
#'                              X = constraints2$X,
#'                              importances = constraints2$importances,
#'                              st = strata2,
#'                              ratio = 1,
#'                              treated = 'daily smoker',
#'                              treated_star = 'some smoking',
#'                              q_star_s = q_star_s,
#'                              correct_sizes = FALSE)
#'
#'}


optimize_controls <- function(z, X, st, importances = NULL, treated = 1,
                              ratio = NULL, q_s = NULL, treated_star = NULL,
                              q_star_s = NULL, weight_star = 1,
                              integer = FALSE, solver = "Rglpk",
                              seed = NULL, runs = 1,
                              time_limit = Inf, threads = 1, correct_sizes = TRUE,
                              low_memory = FALSE) {

  # Make sure inputs are good
  verify_inputs(X = X, importances = importances, ratio = ratio, q_s = q_s,
                st = st, z = z, treated = treated, integer = integer, solver = solver)
  multi_comp <- !is.null(q_star_s) | !is.null(treated_star)
  z <- factor(z)
  group <- levels(z)
  k <- length(group)
  kc2 <- choose(k, 2)

  # Look at strata counts
  n_s <- table(z, st)
  stratios <- n_s / n_s[group == treated, ]
  st_vals <- as.character(colnames(n_s))  # stratum values
  S <- length(st_vals)  # number of strata

  # Prepare sample size matrix for main comparison
  q_s <- process_qs(ratio = ratio, q_s = q_s, n_s = n_s, treated = treated,
                    k = k, group = group, st_vals = st_vals, stratios = stratios)

  # Make sure inputs are good for supplemental comparisons
  if (multi_comp) {
    verify_multi_comp_inputs(q_s = q_s, q_star_s = q_star_s,
                             n_s = n_s, treated = treated, treated_star = treated_star,
                             weight_star = weight_star,
                             group = group, correct_sizes = correct_sizes)
    correct_sizes <- FALSE
    n_comp <- length(treated_star) + 1
    if (is.null(q_star_s)) {
      q_star_s <- list()
    } else if (!is.list(q_star_s)) {
      q_star_s <- list(q_star_s)
    }
    Q_s <- q_s
    for (comp in 1:(n_comp - 1)) {
      if(length(q_star_s) < comp) {
        q_temp <- NULL
      } else {
        q_temp <- q_star_s[[comp]]
      }
      q_star_s[[comp]] <- process_qs(q_s = q_temp, ratio = NULL, n_s = n_s,
                 treated = treated_star[comp], k = k, group = group, st_vals = st_vals)
      Q_s <- Q_s + q_star_s[[comp]]
    }
    if (any(Q_s[, colnames(n_s)] > n_s)) {
      stop("The total number of units desired across comparisons for at least one stratum
            is greater than the number of units available in the stratum.
            Please lower `q_s` or `q_star_s` accordingly.",
           call. = FALSE)
    }

  } else {
    n_comp <- 1
  }

  # Combine the first and the supplemental comparisons into a single list of comparisons
  if (!is.null(treated)) {
    weight_comp <- 1
  }
  if (!is.null(q_s)) {
    q_s <- list(q_s)
  }
  if (!is.null(q_star_s)) {
    q_s <- append(q_s, q_star_s)
    treated <- c(treated, treated_star)
    weight_comp <- c(weight_comp, weight_star)
  }

  # Prepare importances
  if (is.null(importances)) {
    importances <- setNames(rep(1, ncol(X)), colnames(X))
  } else {
    importances <- importances[colnames(X)]

  }

  nvars_per_group <- dim(X)[2]
  nvars <- nvars_per_group * kc2
  N <- length(z)

  # Run linear program to choose control units
  lp_results <- balance_LP(z = z, X = X, importances = importances,
                           st = st, st_vals = st_vals, S = S,
                           q_s = q_s, weight_comp = weight_comp,
                           N = N, integer = integer, solver = solver,
                           time_limit = time_limit, threads = threads)
  if (is.null(lp_results)) {
    return(NULL)
  } else {
    Q <- lapply(q_s, rowSums)

    if (!low_memory) {
      # Epsilons for the linear program solution
      lp_results$lpdetails$eps <- list()
      for (comp in 1:n_comp) {
        lp_results$lpdetails$eps[[comp]] <- lp_results$o$solution[(n_comp * N + 2 * (comp - 1) * nvars + 1):(n_comp * N + (2 * comp * nvars))]
        lp_results$lpdetails$eps[[comp]] <- matrix(lp_results$lpdetails$eps[[comp]], nvars, 2)
      }
      if (!is.null(colnames(X))) {
        if (k == 2) {
          for (comp in 1:n_comp) {
            rownames(lp_results$lpdetails$eps[[comp]]) <- colnames(X)
          }
        } else {
          for (comp in 1:n_comp) {
            rownames(lp_results$lpdetails$eps[[comp]]) <- 1:nvars
          }
          pairs <- combn(group, 2)
          for (pair_num in 1:kc2) {
            group1 <- pairs[1, pair_num]
            group2 <- pairs[2, pair_num]
            for (comp in 1:n_comp) {
              rownames(lp_results$lpdetails$eps[[comp]])[((pair_num - 1) * nvars_per_group + 1):(pair_num * nvars_per_group)] <-
                paste0(colnames(X), "_", group1, ":", group2)
            }
          }
        }
      }

      for (comp in 1:n_comp) {
      colnames(lp_results$lpdetails$eps[[comp]]) <- c("positive", "negative")
      }

      if (n_comp == 2) {
        lp_results$lpdetails$eps_star <- lp_results$lpdetails$eps[[2]]
      }
      if(n_comp > 2) {
        lp_results$lpdetails$eps_star <- lp_results$lpdetails$eps[2:n_comp]
      }
      lp_results$lpdetails$eps <- lp_results$lpdetails$eps[[1]]
    }

    # Record objectives
    lp_results$lpdetails$objective <- lp_results$o$optimum
    # Obj without importances for LP
    # sum of epsilons for all comparisons
    lp_results$lpdetails$objective_wo_importances <- sum(lp_results$o$solution[(n_comp * N + 1):(n_comp * N + (2 * n_comp * nvars))])

    best_objective <- Inf
    run_objectives <- rep(NA, runs)
    run_objectives_wo_importances <- rep(NA, runs)

    if (is.null(seed)) {
      seed <- sample(1:1000000, 1)
    }
    set.seed(seed, kind = "Mersenne-Twister")


    balance_matrices <- create_balance_matrices(X = X, z = z, N = N, nvars = nvars_per_group,
                                                kc2 = kc2, q_s = q_s, return = "X")

    eps <- NULL
    for (run in 1:runs) {
      # Run randomized rounding
      if (correct_sizes) {
        rr_results_temp <- randomized_rounding(o = lp_results$o, N = N, st = st,
                                               st_vals = st_vals, S = S, z = z)
      } else {
        rr_results_temp <- randomized_rounding_expectation(o = lp_results$o, N = N,
                                                           n_comp = n_comp)
      }

      # Calculate and format results

      if (!low_memory) {
        # Epsilons for the randomized rounding (or integer) solution
        eps_zero <- matrix(0, nvars, 2)
        eps_temp <- list()
        for (comp in 1:n_comp) {
          eps_temp[[comp]] <- eps_zero
        }

        for (i in 1:nvars) {
          for (comp in 1:n_comp) {
            row <- as.vector(balance_matrices$x_blk[(comp - 1) * nvars + i, ])
            imbalance <- sum(row * rr_results_temp$select)
            if (imbalance < 0) {
              eps_temp[[comp]][i, 1] <- abs(imbalance)
            } else if (imbalance > 0) {
              eps_temp[[comp]][i, 2] <- imbalance
            }
          }
        }

        if (k == 2) {
          if (!is.null(colnames(X))) {
            for (comp in 1:n_comp) {
              rownames(eps_temp[[comp]]) <- colnames(X)
            }
          }
        } else {
          if (!is.null(colnames(X))) {
            for (comp in 1:n_comp) {
              rownames(eps_temp[[comp]]) <- 1:nvars
            }
            pairs <- combn(unique(z), 2)
            for (pair_num in 1:kc2) {
              group1 <- pairs[1, pair_num]
              group2 <- pairs[2, pair_num]
              for (comp in 1:n_comp) {
                rownames(eps_temp[[comp]])[((pair_num - 1) * nvars_per_group + 1):(pair_num * nvars_per_group)] <- paste0(colnames(X), "_", group1, ":", group2)
              }
            }
          }
        }

        for (comp in 1:n_comp) {
          colnames(eps_temp[[comp]]) <- c("positive", "negative")
        }

      }

      # Objective value for the randomized rounding (or integer) solution
      run_objectives[run] <- 0
      run_objectives_wo_importances[run] <- 0
      for (comp in 1:n_comp) {
        run_objectives[run] <- run_objectives[run] +
          sum(abs(importances * weight_comp[comp] *
                    (matrix(balance_matrices$x_blk[((comp - 1) * nvars + 1):(comp * nvars), ],
                            nrow = nvars,
                            ncol = n_comp * N)
                     %*% rr_results_temp$select)))
        run_objectives_wo_importances[run] <- run_objectives_wo_importances[run] +
          sum(abs(matrix(balance_matrices$x_blk[((comp - 1) * nvars + 1):(comp * nvars), ],
                         nrow = nvars,
                         ncol = n_comp * N)
                  %*% rr_results_temp$select))
      }

      if (run_objectives[run] < best_objective) {
        if (!low_memory) {
          eps <- eps_temp
        }
        objective_wo_importances <- run_objectives_wo_importances[run]
        rr_results <- rr_results_temp
        best_objective <- run_objectives[run]
      }

    }

    selected_star <- NULL
    pr_star <- NULL
    eps_star <- NULL
    weight_star <- NULL
    if (n_comp > 1) {
      selected_star <- rr_results$select[(N+1):(2*N)]
      pr_star <- rr_results$pr[(N+1):(2*N)]
      eps_star <- eps[[2]]
      weight_star <- weight_comp[2]
      if (n_comp > 2) {
        selected_star <- list(selected_star)
        pr_star <- list(pr_star)
        for (comp in 3:n_comp) {
          selected_star <- append(selected_star, list(rr_results$select[((comp - 1) * N+1):(comp*N)]))
          pr_star <- append(pr_star, list(rr_results$pr[((comp - 1) * N+1):(comp*N)]))
        }
        eps_star <- eps[2:n_comp]
        weight_star = weight_comp[2:n_comp]
      }
    }

    return(list(objective = best_objective,
                objective_wo_importances = objective_wo_importances,
                eps = eps[[1]],
                eps_star = eps_star,
                importances = importances,
                weight_star = weight_star,
                selected = rr_results$select[1:N],
                selected_star = selected_star,
                pr = rr_results$pr[1:N],
                pr_star = pr_star,
                rrdetails = list(
                  seed = seed,
                  run_objectives = run_objectives,
                  run_objectives_wo_importances = run_objectives_wo_importances),
                lpdetails = lp_results$lpdetails))
  }
}
