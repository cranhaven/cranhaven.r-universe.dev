#' Simulate an Adaptive Trial With Bernoulli Distributed Outcomes
#' @description Simulates a response-adaptive randomized experiment with Bernoulli
#' distributed outcomes. At each period, observed outcomes are used to update assignment
#' probabilities according to the specified `algorithm`. `algorithm = "static"` is the non-adaptive
#' uniform baseline, where probabilities of being assigned to one treatment is the same as any other.
#' @param n A positive integer. Total number of units to simulate.
#' @param t Total number of assignment periods. Positive integer. Default is `t = n` for pure sequential (one unit per period) assignment.
#' The sizes of each period will be equal as `n %/% t`,
#' except for the last period which will be `n %/% t + n %% t`, when `period_sizes = NULL`.
#' @param p The true probabilities of success for each treatment arm. Specified as a matrix,
#' where `rownames(p)` are the treatment arm names. If there is a control condition, specify its
#' rowname as `"Control"`. `colnames(p)` are the cluster or block labels, e.g.
#'       `matrix(c(0.5, 0.3, 0.5, 0.6), nrow = 2, ncol = 2, dimnames(list(c("Control", "T1"), c("B1", "B2"))))`.
#'       Probabilities are accessed as `p[treatment, block]`.
#' With blocks and clusters utilize the clusters for the columns because clusters are fully nested in blocks.
#' For no clusters or blocks simply use a matrix with 1 column.
#' @param dt Logical. If `TRUE` returns a [data.table::data.table()]; otherwise returns a [tibble::tibble()]. Default `FALSE`.
#' @param blocks A named numeric vector of block membership probabilities (must sum to 1), where `names(blocks)`
#' are the block labels. Units are assigned to blocks via [randomizr::complete_ra()]. Pass `NULL` (default) for no blocking.
#' @param clusters Cluster membership probabilities. Can be:
#' \describe{
#' \item{Numeric vector}{A named vector where `names(clusters)` are the cluster labels e. g. `cC1 = 0.4, C2 = 0.6)`.
#' Used when there is not blocking.}
#' \item{Named list of vectors}{A named list where `names(clusters)` are block labels, and each element is a named vector
#' of per-block cluster proportions, e.g.
#' `list(B1 = c(C1 = 0.4, C2=0.6), B2 = c(C3 = 0.2, C4 = 0.8))`
#' Clusters are accessed as `clusters[[block]][cluster]`. Inside each block, cluster proportions must sum to 1, and the same cluster cannot appear in multiple blocks.}
#' }
#' Units are assigned to clusters via [randomizr::complete_ra()]. Pass `NULL` (default) for no clustering.
#' @param assignment_dates An optional `Date` vector of dates representing when units are assigned.
#' If shorter than `n` it is recycled and sorted. If `NULL` (default) no assignment dates are recorded.
#' @param time_model An optional function with signature:
#'
#' `function(n, conditions, successes, current_period, blocks = NULL, clusters = NULL, ...)`
#'
#' It returns a vector of [lubridate::period] objects which will then be added to `assignment_dates` to produce `success_date`. Used to simulate delayed feedback mechanism
#' during the trial, so outcomes are imperfectly observed. Only used when`assignment_dates` is also supplied. Dates can be generated even when `delayed_feedback == FALSE`,
#' but they will not be used. Default `NULL`. Other optional arguments Cannot share names with arguments in [furrr::furrr_options()].
#' @param period_sizes Numeric vector of `length(t)`, with the specific number of units to be assigned in each period. Used when it is required to assign different numbers of units
#' to treatment across the periods of the trial.
#' @param ... Additional named arguments forwarded to `time_model` and [furrr::furrr_options()].
#' @inheritParams mab_from_rct
#'
#' @returns Depends on ` r` value if ` r = 1`, an S3 `single_param_mab` class object, and if ` r > 1`, an
#' S3 `muti_param_mab`, with the following:
#' \itemize{
#' \item `new_data`: `tibble` or `data.table` containing the new treatment assignments and outcomes under the simulation.
#' If ` r >1` and `keep_data = TRUE`, the tables from each trial are nested inside.
#' \item `bandits`: A list with 3 elements:
#' \itemize{
#' \item `statistic`: Thompson Sampling or UCB1 statistics computed for each treatment at each period of each trial.
#' \item `assignment_prob`: Assignment probabilities for each treatment at each period of each trial.
#' \item `assignment_quant`: Assignment quantities for each treatment in each trial.
#' }
#' \item `means`:  A `tibble` or `data.table` containing point estimates, and standard errors for
#' the AW-AIPW, IPW, and OLS estimators for each treatment in each trial.
#' \item `f_stats`: A named numeric vector of F statistics from IPW and OLS regressions. When ` r >
#' 1`, it is a data.frame/data.table of F statistics with columns corresponding to IPW and OLS.
#' \item `contrasts`: A `tibble` or `data.table` containing point estimates, and standard errors for
#' the estimated linear contrasts of treatment arm estimates for each trial. Only when `contrasts`
#' is not `NULL`.
#' \item `models`: List containing `lm_robust` objects from IPW and OLS regressions, only stored
#' when `keep_models = TRUE` or ` r = 1` and clusters are provided.
#' \item `config`: Configuration list of 3 elements:
#' \itemize{
#' \item `args`: List of arguments passed to [simulate_mab()].
#' \item `call`: The original call to [simulate_mab()].
#' \item `parallel`: The [furrr::furrr_options()] object used for parallelization.
#' }
#' }
#' @details
#'
#' ## Blocking and Clustering
#'
#' When blocking and/or clustering are specified, these assignments will be randomly pregenerated before the start of the adaptive sequential assignment. These arguments allow simulating a trial
#' when there may be heterogeneous outcomes across a treatment block or treatment cluster, so different assignment probabilities can be provided for the same treatment, depending on the block and/or cluster
#' of a unit.
#'
#' Clusters should be contained inside each assignment wave (a warning is thrown if this is not the
#' case), so it is possible to have 2 observations in the same cluster assigned to different
#' treatments if they were assigned in different waves. This is assumed because without it the
#' adaptive probabilities will not be impacting assignments. For example if someone in cluster 1 is
#' assigned in period 1, then all other members are forced to have the same treatment, even if they
#' are assigned in period 5, 10, 20 etc.
#'
#' ## Implementation
#'
#' At each period, either the Thompson sampling probabilities or UCB1 values are calculated based on
#' the outcomes from the number of `prior_periods` specified weighted by `discount_rate`. New
#' treatments are then assigned randomly using the Thompson sampling probabilities via the
#' \href{https://cran.r-project.org/package=randomizr}{randomizr} package, or as the treatment with
#' the highest UCB1 values, while implementing the specific treatment blocking and control
#' augmentation specified.
#'
#' After assigning treatments, observations will have their outcomes generated via a Bernoulli draw
#' associated to the probability in the `p` matrix corresponding to their treatment and
#' block/cluster. If `delayed_feedback = TRUE`, dates of success will be generated via the provided `time_model()`
#' function. When the next period starts, the success dates are checked against the maximum/latest
#' `assignment_date` for the period, and if any success occurs after that, it is treated as a
#' failure for the purpose of the bandit decision algorithms.
#'
#' ## Inference
#'
#' At the end of the simulation the results are aggregated together to calculate the Adaptively
#' Weighted Augmented Inverse Probability Estimator (Hadad et al. 2021) using the mean and variance
#' formulas provided, under the constant allocation rate adaptive schema. These estimators are
#' unbiased and asymptotically normal under the adaptive conditions and their differences are also
#' unbiased asymptotically normal estimators for treatment effects.
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et al. (2021)}. Asymptotic
#' validity, hinges on sub-optimal arms continually being assigned, if arms have low to 0
#' probability of being assigned, the central limit theorem proved no longer applies. Thus it is
#' recommended to use `random_assign_prop` and `control_augment` to ensure all arms non-zero probabilities
#' of assignment over the whole trial.
#'
#' Under
#' clustering the unit of observation becomes the cluster, the sample size the number of clusters.
#' Individual estimates are aggregated in each period by cluster before being used to compute the
#' final AW-AIPW estimate and variance (CR0 style). The variance is adjusted by the Stata CR1
#' adjustment, (\eqn{\frac{G}{G-1} * \frac{N-1}{N-k}}) where k is
#' the number of treatments, and G is the number of clusters. Degrees of freedom of `G-1` are also provided,
#' for use of the more conservative t-distribution, though inference is still only valid asymptotically.
#'

#' Inverse Probability Weighted (IPW) estimates are also provided using [estimatr::lm_robust()].
#' \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)}.
#' In clustered cases CR2 standard errors are used, and CR1 (Stata) used if CR2 computation fails.
#' HC2 standard errors are used in non-clustered cases. In high sample sizes for the arms chosen,
#' standard t-tests of the estimates and their contrasts can be asymptotically valid. F-statistics
#' are provided for joint tests provided in [joint_test()].
#'
#' AW-AIPW and IPW are unbiased, with AW-AIPW having lower variances generally, while standard unweighted
#' OLS estimates will be biased with spuriously low variance but are provided for comparisons.
#'
#' ## ` r > 1`
#' Multiple simulations allow researchers to gauge the variance of the procedure and produce
#' bootstrap estimates of variance of the procedure under the passed parameters. For each simulation
#' new data is drawn according to the passed population parameters. This differs from [mab_from_rct()]
#' where resimulations occurs on the same fixed dataset.
#'
#' Further details about the adaptive procedure can be found in [mab_from_rct()]
#'
#'
#' ## Performance Concerns
#'
#' This procedure has the potential to be computationally expensive and time-consuming. Performance
#' depends on the relative size of each period, `t`, `n`, and ` r`.
#' This function has separate support for `data.frame`s and `data.table`s, selected by the `dt`
#' argument. This flag defines two separate tracks where either combination of `dplyr`, `tidyr` and base `R` to shape data, and run
#' the simulation or only `data.table` code operations are used.
#'
#' In general, smaller batches run faster under base `R`, while larger ones could benefit from the
#' performance and memory efficiencies provided by `data.table`. However, we've observed larger
#' sizes can cause numerical instability with some calculations in the Thompson sampling
#' procedure. Internal safeguards exist to prevent this, but the best way to preempt any issues is
#' to set `prior_periods` to a low number.
#'
#' ## Parallel Processing
#'
#' The function provides support for parallel processing via the
#' \href{https://cran.r-project.org/package=future}{future} and
#' \href{https://cran.r-project.org/package=furrr}{furrr} packages. When conducting a large number
#' of simulations, parallelization can improve performance if sufficient system resources are
#' available. Parallel processing must be explicitly set by the user, through `future::plan()`.
#' Windows users should set the plan to "multisession", while Linux and MacOS users can use
#' "multicore" or "multisession". Users running in a High Performance Computing environment (HPC),
#' are encouraged to use
#' \href{https://cran.r-project.org/package=future.batchtools}{future.batchtools}, for their
#' respective HPC scheduler. Note that parallel processing is not guaranteed to work on all systems,
#' and may require additional setup or debugging effort from the user. For any issues, users are
#' encouraged to consult the documentation of the above packages.
#'
#' @seealso [mab_from_rct()]
#'
#'
#' @examples
#' n <- 100
#' t <- 10
#'
#' p <- matrix(c(0.2, 0.5, 0.45, 0.5), ncol =1, dimnames = list(paste0("T", 1:4)))
#'
#' set.seed(543)
#' simulate_mab(n, t, p = p, random_assign_prop = 0.1,
#' algorithm =  "ucb1", discount_rate  = 0.5)
#'
#' simulate_mab(n, t, p = p, random_assign_prop = 0.1,
#' period_sizes = c(37, rep(7, 9)), algorithm = "thompson")
#'
#'
#' simulate_mab(n, t, p = matrix(c(0.1, 0.5, 0.3, 0.2, 0.3, 0.3),
#' dimnames = list(c("T1", "T2"), c("B1", "B2", "B3")),
#' ncol = 3, nrow = 2), blocks = c("B1" =0.3, "B2" = 0.5, "B3" = 0.2),
#' algorithm = "thompson")
#'
#' @references
#'
#'
#' Agrawal, Shipra, and Navin Goyal. 2012. "Analysis of Thompson Sampling for the Multi-Armed Bandit
#' Problem." \emph{Proceedings of the 25th Annual Conference on Learning Theory}, June 16,
#' 39.1-39.26. \url{https://proceedings.mlr.press/v23/agrawal12.html}.
#'
#' Asyuraa, F. C., S. Abdullah, and T. E. Sutanto. 2021. "Empirical Evaluation on Discounted
#'  Thompson Sampling for Multi-Armed Bandit Problem with Piecewise-Stationary Bernoulli Arms."
#'  Journal of Physics: Conference Series 1722 (1): 012096. \doi{10.1088/1742-6596/1722/1/012096}
#'
#' Auer, Peter, Nicolò Cesa-Bianchi, and Paul Fischer. 2002. "Finite-Time Analysis of the Multiarmed
#' Bandit Problem." \emph{Machine Learning} 47 (2): 235–56. \doi{10.1023/A:1013689704352}.
#'
#' Bengtsson, Henrik. 2025. "Future: Unified Parallel and Distributed Processing in R for Everyone."
#' \url{https://cran.r-project.org/package=future}.
#'
#' Bengtsson, Henrik. 2025. "Future.Batchtools: A Future API for Parallel and Distributed Processing
#' Using ‘Batchtools.’" \url{https://cran.r-project.org/package=future.batchtools}.
#'
#' Garivier, Aurélien, and Eric Moulines. 2008. "On Upper-Confidence Bound Policies for
#'  Non-Stationary Bandit Problems." arXiv:0805.3415. Preprint, arXiv, May 22.
#'  \doi{10.48550/arXiv.0805.3415}
#'
#' Hadad, Vitor, David A. Hirshberg, Ruohan Zhan, Stefan Wager, and Susan Athey. 2021. "Confidence
#' Intervals for Policy Evaluation in Adaptive Experiments." \emph{Proceedings of the National
#' Academy of Sciences of the United States of America} 118 (15): e2014602118.
#' \doi{10.1073/pnas.2014602118}.
#'
#' Kuleshov, Volodymyr, and Doina Precup. 2014. "Algorithms for Multi-Armed Bandit Problems."
#' \emph{arXiv}. \doi{10.48550/arXiv.1402.6028}.
#'
#' Loecher, Thomas Lotze and Markus. 2022. "Bandit: Functions for Simple a/B Split Test and
#' Multi-Armed Bandit Analysis." \url{https://cran.r-project.org/package=bandit}.
#'
#' Offer‐Westort, Molly, Alexander Coppock, and Donald P. Green. 2021. "Adaptive Experimental
#' Design: Prospects and Applications in Political Science." \emph{American Journal of Political
#' Science} 65 (4): 826–44. \doi{10.1111/ajps.12597}.
#'
#' Slivkins, Aleksandrs. 2024. "Introduction to Multi-Armed Bandits." \emph{arXiv}.
#' \doi{10.48550/arXiv.1904.07272}.
#'
#' Vaughan, Davis, Matt Dancho, and RStudio. 2022. "Furrr: Apply Mapping Functions in Parallel Using
#' Futures." \url{https://cran.r-project.org/package=furrr}.
#'
#'
#' @export

simulate_mab <- function(
  n,
  t = n,
  p,
  algorithm = c("thompson", "ucb1"),
  blocks = NULL,
  clusters = NULL,
  control_augment = 0,
  random_assign_prop = 0,
  delayed_feedback = FALSE,
  assignment_dates = NULL,
  time_model = NULL,
  period_sizes = NULL,
  prior_periods = NULL,
  discount_rate = 1,
  dt = FALSE,
  ndraws = 5000,
  r = 1,
  keep_data = FALSE,
  keep_models = FALSE,
  contrasts = NULL,
  check_args = TRUE,
  verbose = FALSE,
  ...
) {
  cl <- match.call()
  algorithm <- rlang::arg_match(algorithm)
  if (!is.null(contrasts)) {
    contrasts <- rlang::arg_match(
      contrasts,
      c("control", "best", "both", "all")
    )
    if (
      !"control" %in% tolower(rownames(p)) &&
        contrasts %in% c("control", "both")
    ) {
      contrasts <- "best"
      rlang::warn(
        "No control condition provided; `contrasts` set to \"best\". Name a row \"control\" in `p` to use \"control\" or \"both\"."
      )
    }
  }

  if (check_args) {
    check_mab_sim(
      n = n,
      t = t,
      p = p,
      blocks = blocks,
      clusters = clusters,
      control_augment = control_augment,
      random_assign_prop = random_assign_prop,
      assignment_dates = assignment_dates,
      delayed_feedback = delayed_feedback,
      time_model = time_model,
      period_sizes = period_sizes,
      prior_periods = prior_periods,
      discount_rate = discount_rate,
      dt = dt,
      ndraws = ndraws,
      r = r,
      keep_data = keep_data,
      keep_models = keep_models,
      verbose = verbose
    )
  }
  if (!is.null(blocks)) {
    names(blocks) <- tolower(names(blocks))
  }
  if (!is.null(clusters)) {
    names(clusters) <- tolower(names(clusters))
  }

  args <- mget(methods::formalArgs(simulate_mab))

  other_args <- split_args(..., time_model = time_model)

  setup <- setup_mab_sim(
    n = n,
    t = t,
    p = p,
    blocks = blocks,
    clusters = clusters,
    assignment_dates = assignment_dates,
    time_model = time_model,
    period_sizes = period_sizes
  )
  args <- utils::modifyList(
    args,
    list(
      col_names = setup$col_names,
      equal_probs = setup$equal_probs,
      period_idxs = setup$period_idxs,
      conditions = setup$conditions,
      simulate_dates = setup$simulate_dates,
      p = setup$p
    )
  )

  if (!"control" %in% names(setup$conditions) && control_augment > 0) {
    rlang::abort(c(
      "a Control group must be specified when `control_augment` > 0",
      "x" = sprintf(
        "Treatment conditions specified: %s",
        paste(setup$conditions, sep = ", ")
      ),
      "x" = paste0("Control Augment: ", control_augment)
    ))
  }

  furrr_opt <- do.call(
    furrr::furrr_options,
    c(list(seed = TRUE), other_args$furrr_args)
  )
  run_single <- purrr::partial(
    run_mab_single,
    sim_type = "param",
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    prior_periods = prior_periods,
    delayed_feedback = delayed_feedback,
    discount_rate = discount_rate,
    conditions = setup[["conditions"]],
    blocking = setup[["blocking"]],
    clustering = setup[["clustering"]],
    col_names = setup[["col_names"]],
    ndraws = ndraws,
    keep_data = keep_data,
    keep_models = keep_models,
    contrasts = contrasts,
    verbose = verbose,
    r = r,
    time_model = time_model,
    time_model_args = other_args[["time_model_args"]],
    p = setup[["p"]],
    n = n,
    dt = dt,
    blocks = blocks,
    clusters = clusters,
    equal_probs = setup[["equal_probs"]],
    assignment_dates = setup[["assignment_dates"]],
    simulate_dates = setup[["simulate_dates"]],
    period_idxs = setup[["period_idxs"]]
  )
  verbose_log(verbose, "Starting Simulations")
  if (r == 1) {
    results <- run_single()
  } else if (r > 1) {
    mabs <- furrr::future_map(
      seq_len(r),
      \(.) {
        run_single()
      },
      .options = furrr_opt,
      .progress = verbose
    )
    verbose_log(verbose, "Collating Results")
    results <- condense_results(
      dt = dt || (r * t > 100000),
      keep_data = keep_data,
      keep_models = keep_models,
      mabs = mabs
    )
  }

  results$args <- args
  results$cl <- cl
  results$furrr <- furrr_opt
  results$args$time_model_args <- other_args$time_model$args
  return(construct_mab(results, type = "param", multi = r > 1))
}

#' Set Up MAB Simulation
#' @name setup_mab_sim
#' @description
#' Performs all one-time set-up required for [simulate_mab()] as opposed to
#' [prep_sim_data()] which needs to be re-run each period.
#' @inheritParams simulate_mab
#' @returns A named list containing:
#' \itemize{
#'   \item `period_idxs`: A list of 2 integer vectors of period boundary indices.
#'   \item `assignment_dates`: Vector of assignment dates based on provided dates.
#'   \item `blocking`: Logical; `TRUE` if `blocks` is non-null.
#'   \item `clustering`: Logical; `TRUE` if `clusters` is non-null.
#'   \item `simulate_dates`: Logical; `TRUE` if both `time_model` is a function
#'   and `assignment_dates` is non-null.
#'   \item `p`: The success probability matrix with lowercase and sorted rownames
#'   with rows reordered to match `conditions`.
#'   \item `conditions`: A named character vector of arm labels sorted
#'   alphabetically, with names `"control"` or `"treatment"` as appropriate.
#'   \item `equal_probs`: A named numeric vector of equal assignment
#'   probabilities `1 / K` for each of the `K` arms.
#'   \item `col_names`: A fixed named list of output column name strings.
#' }
#' @family param
#' @keywords internal
setup_mab_sim <- function(
  n,
  t,
  p,
  blocks,
  clusters,
  assignment_dates,
  time_model,
  period_sizes
) {
  period_idxs <- generate_period_idx(n = n, t = t, period_sizes = period_sizes)
  assignment_dates <- generate_assignment_dates(
    n = n,
    assignment_dates = assignment_dates
  )

  blocking <- !is.null(blocks)
  clustering <- !is.null(clusters)
  simulate_dates <- is.function(time_model) && !is.null(assignment_dates)
  dimnames(p) <- lapply(dimnames(p), tolower)
  conditions <- sort(rownames(p))
  names(conditions) <- ifelse(conditions == "control", "control", "treatment")
  p <- p[conditions, , drop = FALSE]
  equal_probs <- stats::setNames(rep(1 / nrow(p), nrow(p)), conditions)

  col_names <- list(
    cluster_col = "cluster",
    assignment_date_col = "assignment_date",
    success_date_col = "success_date"
  )

  list(
    period_idxs = period_idxs,
    assignment_dates = assignment_dates,
    blocking = blocking,
    clustering = clustering,
    simulate_dates = simulate_dates,
    p = p,
    conditions = conditions,
    equal_probs = equal_probs,
    col_names = col_names
  )
}


#' Split Function Arguments
#' @name split_args
#' @inheritParams simulate_mab
#' @description
#' Uses [methods::formalArgs()] to match arguments provided to `...` of [simulate_mab()] to [furrr::furrr_options()] and the user specified `time_model`
#' @returns A named list with 2 elements, `furr_args` and `time_model_args` each a list of the respective arguments to
#' [furrr::furrr_options()] and the user specified `time_model`
#' @keywords internal
#' @family param

split_args <- function(time_model = NULL, ...) {
  all_args <- rlang::dots_list(..., .named = TRUE)
  furrr_args <- all_args[
    names(all_args) %in% methods::formalArgs(furrr::furrr_options)
  ]
  time_model_args <- if (!is.null(time_model)) {
    all_args[names(all_args) %in% methods::formalArgs(time_model)]
  } else {
    NULL
  }
  return(list(
    furrr_args = furrr_args,
    time_model_args = time_model_args
  ))
}
