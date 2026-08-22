#' @title
#' Simulate a Multi-Arm-Bandit Trial from an Existing Randomized Controlled Trial, With Bernoulli Distributed Outcomes.
#' @name mab_from_rct
#' @description Simulates a response-adaptive, Multi-Arm-Bandit (MAB) trial using experimental data
#' from an original randomized controlled trial (RCT), and adaptive inference strategies
#' described in \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et al. (2021)}
#' and \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12597}{Offer-Westort et al. (2021)} to robustly estimate
#' treatment effects. See the details to learn more.
#'
#' @param formula A `formula` object specifying outcome variable, treatment indicator, treatment
#' blocking and treatment clustering for optional blocked and/or clustered randomized designs. The treatment variable should always be the first variable
#' following `~`
#' (Additional covariates to be added in later updates). Clustering and blocking variables
#' should be included in specific `block()` or `cluster()` groups in the formula, e.g. `outcome ~
#' treatment + block(x1, x2, x3) + cluster(x4)`.
#'
#' @param data A `data.frame`, `data.table`, or any object which inherits from `data.frame`, containing input data from the trial. This should be the results
#' of a traditional Randomized Controlled Trial (RCT).
#'
#' @param algorithm A character string specifying the MAB algorithm to use. Options are
#' `"thompson"`, `"ucb1"`, or "static", ignoring case. Algorithm
#' defines the adaptive assignment process. For more details on these specific algorithms see
#' \href{https://www.jstor.org/stable/2332286}{Thompson 1933};
#' \href{https://link.springer.com/article/10.1023/A:1013689704352}{Auer et al. 2002};
#' \href{https://proceedings.mlr.press/v23/agrawal12.html}{Agrawal and Goyal 2012};
#' \href{https://arxiv.org/abs/1402.6028}{Kuleshov and Precup 2014} and
#' \href{https://arxiv.org/abs/1904.07272}{Slivkins 2024}.
#'
#' @param control_augment Minimum proportion of each treatment assignment wave guaranteed to receive the treatment labeled as `"Control"`. Ranges from 0 to 1,
#' and the default is 0. Adjustment is always made after the adjustment from random_assign_prop.
#'
#' @param control_condition Value of the control condition. Only necessary when `control_augment` is greater than 0. Internally this value
#' is coerced to a string, so it should be passed as a string, or a type that can easily be converted to a string.
#
#' @param random_assign_prop Proportion of each treatment wave assigned via static, equal
#' probabilities of assignment. Adaptive probabilities are updated by `p * (1 - random_assign_prop) + random_assign_prop * 1/k`, where k is the number of treatment arms.
#'
#' @param period_method A character string; one of `"date"`, `"batch"`, or `"individual"`, to define
#' the assignment into treatment waves. When using `"batch"` or `"individual"`, ensure your dataset
#' is pre-arranged in the proper order observations should be considered so that groups are assigned
#' correctly. For "date", observations will be considered in chronological order. `"individual"`
#' assignment can be computationally intensive for larger datasets.
#'
#' @param time_unit A character string specifying the unit of time for assigning periods when
#' `period_method ="date"`. Acceptable values are `"day", "week",` or `"month"`. `"month"` does not
#' require an additional column with the months of each observation, but it can accept a separate
#' `month_col`. If `month_col` is specified, the periods follow the calendar months strictly, and
#' when it is not specified months are simply used as the time interval. For example if a dataset
#' has dates starting on July 26th, under month based assignment and a specified `month_col` the
#' dates July 26th and August 3rd would be in different periods, but if the `month_col` was not
#' specified, they would be in the same period because the dates are less than one month apart.
#'
#' @param period_length A positive integer; represents the length of each treatment period. If
#' `period_method` is "date", this length refers the number of units specified in `time_unit`.
#' (i.e., if `"day"`, 10 would be 10 days). If `period_method` = `"batch"`, this refers to the
#' number of units in each batch.
#'
#' @param prior_periods A positive integer; number of previous periods to use in the treatment
#' assignment model. Default is `NULL`, where all prior periods are considered. See below for
#' details.
#'
#' @param discount_rate Rate for discounting observations from earlier periods when updating
#' assignment probabilities. A value between 0 and 1, where outcomes from `k` periods ago are
#' weighted by `discount_rate^k`. Default is 1 for no discounting.
#'
#' @param date_col Bare column in `data`; contains original date of event/trial. Only necessary when
#' assigning by "Date". Must be of type `Date`.
#'
#' @param month_col Bare column in `data`; contains
#' month of treatment. Only necessary when `time_unit = "month"`, and when periods should be
#' determined directly by the calendar months instead of month based time periods. This column can
#' be a string/factor variable with the month names or numeric with the month number. It can easily
#' be created from your `date_col` via `lubridate::month(data[[date_col]])` or
#' `format(data[[date_col]], "%m")`.
#'
#' @param delayed_feedback Logical; if `FALSE`, assumes instantaneous feedback for outcomes, as soon
#' as a treatment is assigned, the outcome is realized and known. If `TRUE`, delayed feedback is
#' assumed, so as soon as treatment is assigned, a potential outcome is realized, but it is not
#' known to the simulation, until a certain date. When re-computing the adaptive assignment
#' probabilities, outcomes that have not been observed on the date of
#' assignment are treated as failures.
#'
#' @param success_date_col Bare column in `data`; contains original dates each success occurred.
#' Only necessary when `delayed_feedback = TRUE`. Must be of type `Date`, not a character string.
#'
#' @param assignment_date_col Bare column in `data`; contains original dates treatments were
#' assigned to observations. Only necessary when `delayed_feedback = TRUE`. Used to simulate
#' imperfect observation of outcomes in the simulation. Must be of type `Date`, not a character
#' string.
#'
#' @param whole_experiment Logical; if `TRUE`, uses all past experimental data for imputing
#' outcomes. If `FALSE`, uses only data available up to the current period. In large datasets or
#' with a high number of periods, setting this to `FALSE` can be more computationally intensive,
#' though not a significant contributor to total run time. Default is `FALSE`.
#'
#' @param ndraws Number of draws used to approximate Thompson sampling probabilities. Used only when
#' direct calculation fails or overflows. Default is 5000 but can be raised or lowered depending on
#' performance and accuracy concerns.
#'
#' @param verbose Logical; whether or not to print intermediate messages. Default is `FALSE`.
#' @param check_args Logical; whether or not to validate passed arguments. Default is `TRUE` and recommended
#' not to be changed.
#'
#' @param r Positive integer; number of replications (under different random seed). Replications of
#' the MAB procedure on a fixed dataset provides important diagnostic information on the
#' stochasticity/variance of the re-simulation method. Replications can be conducted in parallel, by
#' setting an appropriate [future::plan()]. See details below.
#'
#' @param keep_data Logical; Whether or not to keep the final data from each trial. Recommended
#' `FALSE`. When` r = 1` the final data is always kept and reported.
#'
#' @param keep_models Logical; Whether or not to keep the final IPW and OLS models from each trial. Recommended
#' `FALSE`. When` r = 1` models are always kept and reported. Required to be `TRUE` to compute
#' arbitrary pairwise contrasts. Only utilized under clustering.
#'
#' @param contrasts Character string specifying which pairwise contrasts to
#'   precompute after each replication. One of `"control"` (each arm vs. control arm),
#' `"best"` (each arm vs. the MAB-selected best arm),
#'  `"both"`, or `"all"` (all `choose(k, 2)` pairwise comparisons, expensive
#'  for large `k`). All contrasts are tested under the two-sided null of no difference.
#' Defaults to `NULL`, arbitrary contrasts can be computed after if `keep_models == TRUE`
#'
#'
#' @param ... Additional named arguments passed to [furrr::furrr_options()]
#'
#' @returns Depends on ` r` value if ` r = 1`, an S3 `single_rct_mab` class object, and if ` r > 1`, an
#' S3 `muti_rct_mab`, with the following:
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
#' the AIPW, IPW, and OLS estimators for each treatment in each trial.
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
#'
#' @details
#'
#' ## Clustering
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
#' After assigning treatments, observations with new treatments have their outcomes imputed using
#' success rates from the original randomized trial. These rates are estimated as grouped means
#' within each treatment arm. If blocking is specified, rates are estimated within each combination
#' of treatment arm and block.
#'
#' If `delayed_feedback = TRUE`, new dates of success will be imputed using the means of those dates
#' in the period, grouped by treatment block if necessary. Observations for which their treatment
#' changed, but their outcome was success in the original and simulation, do not have their date
#' changed. When the next period starts, the success dates are checked against the maximum/latest
#' `assignment_date` for the period, and if any success occurs after that, it is treated as a
#' failure for the purpose of the bandit decision algorithms.
#'
#' ## Inference
#'
#' At the end of the simulation the results are aggregated together to calculate the Adaptively
#' Weighted Augmented Inverse Probability Estimator (Hadad et al. 2021) using the mean and variance
#' formulas provided, under the constant allocation rate adaptive schema. These estimators are
#' unbiased and asymptotically normal under the adaptive conditions and their differences are also
#' unbiased asymptotically normal estimators for treatment effects. See
#' \href{https://www.pnas.org/doi/pdf/10.1073/pnas.2014602118}{Hadad et al. (2021)}. Asymptotic
#' validity hinges on sub-optimal arms continually being assigned, if arms have low to 0
#' probability of being assigned, the central limit theorem proved no longer applies. Thus it is
#' recommended to use `random_assign_prop` and `control_augment` to ensure all arms have non-zero probabilities
#' of assignment over the whole trial.
#'
#'
#' Under
#' clustering the unit of observation becomes the cluster, the sample size becomes the number of clusters.
#' Individual estimates are aggregated in each period by cluster before being used to compute the
#' final AIPW estimate and variance (CR0 style). The variance is adjusted by the Stata CR1
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
#' AIPW and IPW are unbiased, with AIPW having lower variances generally, while standard unweighted
#' OLS estimates will be biased with spuriously low variance, but are provided for comparisons.
#'
#'
#' ## Performance Concerns
#'
#' This procedure has the potential to be computationally expensive and time-consuming. Performance
#' depends on the relative size of each period, number of periods, the overall size of the dataset,
#' and number of replications.
#' This function has separate support for `data.frame`s and `data.table`s. If a `data.frame` is
#' passed, the function uses a combination of `dplyr`, `tidyr` and base `R` to shape data, and run
#' the simulation. However, if a `data.table` is passed the function exclusively uses the
#' `data.table` code for all the same operations.
#'
#' In general, smaller batches run faster under base `R`, while larger ones could benefit from the
#' performance and memory efficiencies provided by `data.table`. However, we've observed larger
#' datasets can cause numerical instability with some calculations in the Thompson sampling
#' procedure. Internal safeguards exist to prevent this, but the best way to preempt any issues is
#' to set `prior_periods` to a low number.
#'
#' ## ` r > 1`
#' Multiple simulations allows researchers to gauge the variance of the simulation procedure itself,
#' by repeating it several times under different random states, using the same fixed data.
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
#' @references
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
#' @seealso \href{https://furrr.futureverse.org}{furrr},
#' \href{https://future.futureverse.org}{future}, [joint_test()], [simulate_mab()]
#'
#' @examples
#' data(tanf)
#' set.seed(454)
#'
#' mab_from_rct(success ~ condition, data = tanf, algorithm = "thompson",
#' period_method = "batch", period_length = 500, delayed_feedback = TRUE,
#' assignment_date_col = appt_date, success_date_col = date_of_recert)
#'
#' mab_from_rct(success ~ condition, data = tanf, algorithm = "ucb1",
#' period_method = "date", time_unit = "day", date_col = appt_date,
#'  period_length = 60, r = 2, discount_rate = 0.8)
#'
#' mab_from_rct(success ~ condition + block(service_center), data = tanf, algorithm = "thompson",
#' period_method = "batch", period_length = 500, control_condition = "no_letter",
#' control_augment = 0.2, prior_periods = 1)
#'
#' @export
mab_from_rct <- function(
  formula,
  data,
  algorithm = c("thompson", "ucb1", "static"),
  random_assign_prop = 0,
  control_augment = 0,
  control_condition = NULL,
  period_method = c("batch", "date", "individual"),
  time_unit = NULL,
  period_length,
  prior_periods = NULL,
  discount_rate = 1,
  date_col = NULL,
  month_col = NULL,
  delayed_feedback = FALSE,
  assignment_date_col = NULL,
  success_date_col = NULL,
  whole_experiment = FALSE,
  ndraws = 5000,
  r = 1,
  verbose = FALSE,
  check_args = TRUE,
  keep_data = FALSE,
  keep_models = FALSE,
  contrasts = NULL,
  ...
) {
  cl <- match.call()
  algorithm <- rlang::arg_match(algorithm)
  period_method <- rlang::arg_match(period_method)
  if (!is.null(time_unit)) {
    time_unit <- rlang::arg_match(time_unit, values = c("day", "week", "month"))
  }
  if (!is.null(contrasts)) {
    contrasts <- rlang::arg_match(
      contrasts,
      c("control", "best", "both", "all")
    )
    if (is.null(control_condition) && contrasts %in% c("control", "both")) {
      contrasts <- "best"
      rlang::warn(
        "No control condition provided; `contrasts` set to \"best\". Supply `control_condition` to use \"control\" or \"both\"."
      )
    }
  }

  col_names <- c(
    formula_parse(formula),
    date_col = deparse(substitute(date_col)),
    month_col = deparse(substitute(month_col)),
    assignment_date_col = deparse(substitute(assignment_date_col)),
    success_date_col = deparse(substitute(success_date_col))
  )
  col_names <- col_names[!vapply(col_names, \(x) all(x == "NULL"), logical(1))]

  args <- mget(setdiff(methods::formalArgs(mab_from_rct), names(col_names)))

  blocking <- !is.null(col_names[["block_cols"]])
  clustering <- !is.null(col_names[["cluster_col"]])

  prepped <- prep_rct_data(
    data = data,
    control_augment = control_augment,
    control_condition = control_condition,
    random_assign_prop = random_assign_prop,
    period_method = period_method,
    time_unit = time_unit,
    period_length = period_length,
    prior_periods = prior_periods,
    discount_rate = discount_rate,
    delayed_feedback = delayed_feedback,
    whole_experiment = whole_experiment,
    blocking = blocking,
    clustering = clustering,
    col_names = col_names,
    check_args = check_args,
    verbose = verbose,
    ndraws = ndraws,
    r = r,
    keep_data = keep_data,
    keep_models = keep_models
  )

  args <- utils::modifyList(
    args,
    list(
      col_names = col_names,
      blocks = col_names$block_cols,
      clusters = col_names$cluster_col,
      period_idxs = prepped$period_idxs,
      conditions = prepped$conditions
    )
  )
  furrr_opt <- do.call(
    furrr::furrr_options,
    c(list(seed = TRUE), rlang::dots_list(..., .named = TRUE))
  )
  run_single <- purrr::partial(
    run_mab_single,
    sim_type = "resim",
    algorithm = algorithm,
    control_augment = control_augment,
    random_assign_prop = random_assign_prop,
    prior_periods = prior_periods,
    delayed_feedback = delayed_feedback,
    whole_experiment = whole_experiment,
    discount_rate = discount_rate,
    conditions = prepped[["conditions"]],
    blocking = blocking,
    clustering = clustering,
    col_names = col_names,
    ndraws = ndraws,
    keep_data = keep_data,
    keep_models = keep_models,
    contrasts = contrasts,
    verbose = verbose,
    r = r,
    period_idxs = prepped[["period_idxs"]],
    imputation_information = prepped[["imputation_information"]],
    data = prepped[["data"]]
  )

  if (r == 1) {
    results <- run_single()
  } else {
    mabs <- furrr::future_map(
      seq_len(r),
      \(.) run_single(),
      .options = furrr_opt,
      .progress = verbose
    )
    verbose_log(verbose, "Collating Results")
    results <- condense_results(
      dt = ((data.table::is.data.table(data)) ||
        r * length(prepped$period_starts) > 100000),
      keep_data = keep_data,
      keep_models = keep_models,
      mabs = mabs
    )
  }
  results$args <- args

  results$furrr <- furrr_opt
  results$cl <- cl
  return(
    construct_mab(results, type = "rct", multi = r > 1)
  )
}
