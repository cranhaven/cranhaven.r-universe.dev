#' @docType class
#' @title iglm Objects (R6 Class)
#' @description
#' The `iglm.object` class encapsulates all components required to define,
#' estimate, and simulate from a generalized linear model under interference. This includes
#' the model formula, coefficients, the underlying network and attribute data
#' (via a `iglm.data` object), sampler controls, estimation controls, and storage
#' for results.
#' @references
#' Fritz, C., Schweinberger, M. , Bhadra S., and D. R. Hunter (2025). A Regression Framework for Studying Relationships among Attributes under Network Interference. Journal of the American Statistical Association, to appear.
#'
#' Stewart, J. R. and M. Schweinberger (2025). Pseudo-Likelihood-Based M-Estimation of Random Graphs with Dependent Edges and Parameter Vectors of Increasing Dimension. Annals of Statistics, to appear.
#'
#' Schweinberger, M. and M. S. Handcock (2015). Local dependence in random graph models: characterization, properties, and statistical inference. Journal of the Royal Statistical Society, Series B (Statistical Methodology), 7, 647-676.
#'
#' @importFrom R6 R6Class
#' @importFrom methods as
#' @importFrom stats runif
#' @importFrom stats as.formula pnorm terms quantile update printCoefmat na.omit sd
#' @importFrom utils modifyList
#' @export
iglm.object.generator <- R6::R6Class("iglm.object",
  private = list(
    .formula = NULL,
    .name = NULL,
    .iglm.data = NULL,
    .coef = NULL,
    .coef_degrees = NULL,
    .coef_degrees_internal = NULL,
    .sampler = NULL,
    .control = NULL,
    .preprocess = NULL,
    .time_estimation = NULL,
    .sufficient_statistics = NULL,
    .results = list(),
    #' @description
    #' Internal method to calculate the observed count statistics based on the
    #' model formula and the data in the `iglm.data` object. Populates the
    #' `private$.sufficient_statistics` field.
    .calc_sufficient_statistics = function() {
      counts <- as.vector(xyz_count_global(
        z_network = private$.iglm.data$z_network,
        x_attribute = private$.iglm.data$x_attribute,
        y_attribute = private$.iglm.data$y_attribute,
        family_x = private$.iglm.data$family_x,
        family_y = private$.iglm.data$family_y,
        attr_x_scale = private$.iglm.data$scale_x,
        attr_y_scale = private$.iglm.data$scale_y,
        neighborhood = private$.iglm.data$neighborhood,
        overlap = private$.iglm.data$overlap,
        directed = private$.iglm.data$directed,
        terms = private$.preprocess$term_names,
        data_list = private$.preprocess$data_list,
        type_list = private$.preprocess$type_list,
        n_units = private$.iglm.data$n_units
      ))
      names(counts) <- private$.preprocess$coef_names
      private$.sufficient_statistics <- counts
    },
    #' @description
    #' Internal validation method. Checks the consistency and validity of
    #' all components of the `iglm.object`. Stops with an error if any
    #' check fails.
    .validate = function() {
      if (!is.character(private$.name) || length(private$.name) != 1) {
        stop("`name` must be a single character string.", call. = FALSE)
      }

      if (is.null(private$.formula) || !inherits(private$.formula, "formula")) {
        stop("Invalid formula in iglm.object.", call. = FALSE)
      }
      if (is.null(private$.coef) || !is.numeric(private$.coef)) {
        stop("Invalid coef in iglm.object.", call. = FALSE)
      }
      if (length(private$.coef) != length(private$.preprocess$coef_names)) {
        stop("Length of coef does not match number of terms in formula.", call. = FALSE)
      }
      if (!inherits(private$.sampler, "sampler.iglm")) {
        stop("Invalid sampler in iglm.object.", call. = FALSE)
      }

      if (!is.null(private$.coef_degrees)) {
        if (!is.numeric(private$.coef_degrees)) {
          stop("Invalid coef_degrees in iglm.object.", call. = FALSE)
        }
        expected_length <- private$.iglm.data$n_units +
          private$.iglm.data$directed * private$.iglm.data$n_units
        if (length(private$.coef_degrees) != expected_length) {
          stop("Length of coef_degrees does not match number of units in data object.", call. = FALSE)
        }
        if (length(private$.coef_degrees_internal) != expected_length) {
          stop("Length of coef_degrees does not match number of units in data object.", call. = FALSE)
        }
      }
      if (!is.null(private$.results)) {
        if (!inherits(private$.results, "results")) {
          stop("Invalid results object in iglm.object.", call. = FALSE)
        }
      }
      if (!inherits(private$.iglm.data, "iglm.data")) {
        stop("Invalid iglm.data object in iglm.object.", call. = FALSE)
      }
      if (!inherits(private$.control, "control.iglm")) {
        stop("Invalid control object in iglm.object.", call. = FALSE)
      }

      if (is.null(private$.sufficient_statistics)) {
        stop("Sufficient statistics have not been computed yet.", call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Creates a new `iglm.object`. This involves parsing the formula,
    #' linking the data object, initializing coefficients, setting up sampler
    #' and control objects, calculating initial statistics, and validating.
    #' @param formula A model `formula` object. The left-hand side should be the
    #'   name of a \code{\link{iglm.data}} object available in the calling environment.
    #'   See \code{\link{iglm-terms}} for details on specifying the right-hand side terms.
    #' @param coef A numeric vector of initial coefficients for the terms in
    #'   the formula (excluding degree coefficeints). If `NULL`, coefficients are
    #'   initialized to zero.
    #' @param name An optional character string specifying a name for the model,
    #'   would be used in plots and model assessment.
    #' @param coef_degrees An optional numeric vector of initial degree
    #'   coefficients. Should be `NULL` if the formula does
    #'   not include degree-correcting terms.
    #' @param sampler A \code{\link{sampler.iglm}} object specifying the MCMC sampler
    #'   settings. If `NULL`, default settings are used.
    #' @param control A \code{\link{control.iglm}} object specifying estimation control
    #'   parameters. If `NULL`, default settings are used.
    #' @param file (character or `NULL`) If provided, loads the sampler state from
    #'  the specified .rds file instead of initializing from parameters.
    #' @param ... Additional arguments. Unrecognized arguments will trigger informative errors.
    #' @return A new `iglm.object`.
    initialize = function(formula = NULL, coef = NULL,
                          coef_degrees = NULL,
                          sampler = NULL,
                          control = NULL,
                          name = NULL,
                          file = NULL,
                          ...) {
      check_glm_arguments(list(...))
      # browser()
      if (!is.null(file)) {
        if (missing(file) || !is.character(file) || length(file) != 1) {
          stop("A valid 'file' (character string) must be provided.", call. = FALSE)
        }
        data_loaded <- readRDS(file)
        required_fields <- c(
          "formula", "preprocess", "iglm.data", "coef",
          "coef_degrees", "time_estimation",
          "sufficient_statistics", "results",
          "control", "name", "sampler"
        )
        if (!is.list(data_loaded) || !all(required_fields %in% names(data_loaded))) {
          stop("File does not contain a valid iglm state.", call. = FALSE)
        }
        private$.formula <- data_loaded$formula
        private$.preprocess <- data_loaded$preprocess
        private$.name <- data_loaded$name
        private$.iglm.data <- iglm.data(
          x_attribute = data_loaded$iglm.data$x_attribute,
          y_attribute = data_loaded$iglm.data$y_attribute,
          z_network = data_loaded$iglm.data$z_network,
          n_units = if (!is.null(data_loaded$iglm.data$n_units)) data_loaded$iglm.data$n_units else data_loaded$iglm.data$n_actor,
          family_x = data_loaded$iglm.data$family_x,
          family_y = data_loaded$iglm.data$family_y,
          scale_x = data_loaded$iglm.data$scale_x,
          scale_y = data_loaded$iglm.data$scale_y,
          neighborhood = data_loaded$iglm.data$neighborhood,
          directed = data_loaded$iglm.data$directed
        )
        private$.coef <- data_loaded$coef
        private$.coef_degrees <- data_loaded$coef_degrees
        private$.coef_degrees_internal <- private$.coef_degrees
        private$.time_estimation <- data_loaded$time_estimation
        private$.sufficient_statistics <- data_loaded$sufficient_statistics

        private$.results <- results(
          size_coef = length(private$.coef),
          size_coef_degrees = length(private$.coef_degrees) *
            private$.preprocess$includes_degrees
        )
        private$.results$update(
          coefficients_path = data_loaded$results$coefficients_path,
          var = data_loaded$results$var,
          fisher_degrees = data_loaded$results$fisher_degrees,
          fisher_nondegrees = data_loaded$results$fisher_nondegrees,
          score_degrees = data_loaded$results$score_degrees,
          score_nondegrees = data_loaded$results$score_nondegrees,
          llh = data_loaded$results$llh,
          estimated = data_loaded$results$estimated
        )
        if (length(data_loaded$results$samples) > 0) {
          private$.results$update(
            stats = data_loaded$results$stats,
            samples = data_loaded$results$samples
          )
        }
        if (length(data_loaded$results$model_assessment) > 0) {
          private$.results$set_model_assessment(
            res =
              data_loaded$results$model_assessment
          )
        }
        private$.control <- data_loaded$control
        private$.sampler <- data_loaded$sampler
      } else {
        if (!inherits(formula, "formula")) {
          stop("`formula` must be a formula object.")
        }
        private$.formula <- formula
        if (is.null(name)) {
          private$.name <- "Current Model"
        } else {
          private$.name <- name
        }
        private$.preprocess <- formula_preprocess(formula)
        private$.iglm.data <- private$.preprocess$data_object
        private$.preprocess$data_object <- NULL

        if (is.null(coef)) {
          private$.coef <- rep(0, length(private$.preprocess$coef_names))
        } else {
          if (!is.numeric(coef) || any(!is.finite(coef))) {
            stop("`coef` must contain finite numeric values (no NA, NaN, or Inf).", call. = FALSE)
          }
          private$.coef <- coef
        }
        if (is.null(coef_degrees)) {
          private$.coef_degrees <- rep(
            0,
            private$.iglm.data$n_units +
              private$.iglm.data$directed * private$.iglm.data$n_units
          )
        } else {
          if (private$.preprocess$includes_degrees == FALSE) {
            stop("The formula does not include degrees terms, so `coef_degrees` should be NULL.")
          }
          if (!is.numeric(coef_degrees) || any(!is.finite(coef_degrees))) {
            stop("`coef_degrees` must contain finite numeric values (no NA, NaN, or Inf).", call. = FALSE)
          }
          private$.coef_degrees <- coef_degrees
        }
        private$.coef_degrees_internal <- private$.coef_degrees
        private$.results <- results(
          size_coef = length(private$.coef),
          size_coef_degrees = length(private$.coef_degrees) *
            private$.preprocess$includes_degrees
        )

        if (is.null(control)) {
          private$.control <- control.iglm()
        } else {
          private$.control <- control
        }
        # --- Handle sampler ---
        if (is.null(sampler)) {
          sampler.x.obj <- sampler.net.attr(
            n_proposals = self$iglm.data$n_units * 10
          )
          sampler.y.obj <- sampler.net.attr(
            n_proposals = self$iglm.data$n_units * 10
          )
          sampler.z.obj <- sampler.net.attr(
            n_proposals = nrow(self$iglm.data$overlap) * 10
          )
          private$.sampler <- sampler.iglm(
            n_simulation = 100,
            n_burn_in = 1,
            init_empty = FALSE,
            sampler_x = sampler.x.obj,
            sampler_y = sampler.y.obj,
            sampler_z = sampler.z.obj
          )
        } else {
          if (!inherits(sampler, "sampler.iglm")) {
            stop("`sampler` must be a 'sampler.iglm' object created with sampler.iglm().")
          }
          private$.sampler <- sampler
        }
        private$.calc_sufficient_statistics()
      }
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Check if this iglm object is equivalent to another iglm object by comparing
    #' their defining features, data, and parameters.
    #' @param other Another object to compare against.
    #' @param tol Tolerance for numeric comparisons (default is 1e-5).
    #' @param check_results (logical) If `TRUE`, also requires the estimation results
    #'   and MCMC samples to match exactly. Default is `FALSE` (only compares model
    #'   specification, input data, and initial coefficients).
    #' @return `TRUE` if the objects are equivalent, otherwise `FALSE`.
    is_equivalent = function(other, tol = 1e-5, check_results = FALSE) {
      if (!inherits(other, "iglm.object")) {
        return(FALSE)
      }
      f1 <- paste(trimws(deparse(private$.formula)), collapse = " ")
      f2 <- paste(trimws(deparse(other$formula)), collapse = " ")
      if (f1 != f2) {
        return(FALSE)
      }

      if (!isTRUE(all.equal(private$.coef, other$coef, tolerance = tol))) {
        return(FALSE)
      }
      if (!isTRUE(all.equal(private$.coef_degrees, other$coef_degrees, tolerance = tol))) {
        return(FALSE)
      }

      data_self <- private$.iglm.data$gather()
      data_other <- other$iglm.data$gather()
      if (!isTRUE(all.equal(data_self, data_other, tolerance = tol))) {
        return(FALSE)
      }
      if (!isTRUE(all.equal(private$.control, other$control, tolerance = tol))) {
        return(FALSE)
      }
      if (check_results) {
        res_self <- private$.results$gather()
        res_other <- other$results$gather()
        if (!isTRUE(all.equal(res_self, res_other, tolerance = tol))) {
          return(FALSE)
        }
      }
      return(TRUE)
    },
    #' @description
    #' Performs model assessment by calculating specified network statistics
    #' on the observed network and comparing their distribution to the
    #' distribution obtained from simulated networks based on the current
    #' model parameters. Requires simulations to have been run first (via
    #'  \code{iglm.object$simulate} or \code{iglm.object_generator$estimate}).
    #'
    #' @param formula A formula specifying the network statistics to assess
    #'   (e.g., `~ degree_distribution() + geodesic_distances_distribution()`).
    #'   The terms should correspond to methods available in the \code{\link{iglm.data}}
    #'   object that end with `distributions`.
    #'   If the term mcmc_diagnostics is included, MCMC diagnostics will also be computed.
    #' @param plot (logical) If `TRUE`, generates plots comparing observed and simulated statistics. Default is `TRUE`.
    #' @return An object of class `iglm_model_assessment` containing the
    #'   observed statistics and the distribution of simulated statistics.
    #'   The result is also stored internally.
    assess = function(formula, plot = TRUE) {
      if (!private$.results$estimated) {
        stop("Model has not been estimated yet, assessing the fit thus makes little sense.", call. = FALSE)
      }

      if (length(private$.results$samples) == 0) {
        self$simulate()
      }
      term_labels <- attr(terms(formula), "term.labels")
      if ("mcmc_diagnostics" %in% term_labels) {
        term_labels <- term_labels[term_labels != "mcmc_diagnostics"]
        formula <- update(formula, . ~ . - mcmc_diagnostics)
        sufficient_statistics <- private$.sufficient_statistics
        include_mcmc <- TRUE
      } else {
        include_mcmc <- FALSE
        sufficient_statistics <- NULL
      }


      names_tmp <- attr(terms(formula), "term.labels")
      names_tmp <- gsub("\"", "", names_tmp)
      names_tmp <- gsub("\\(", "_", names_tmp)
      names_tmp <- gsub("\\)", "", names_tmp)
      names_tmp <- gsub("=", "_", names_tmp)
      names_tmp <- gsub(" ", "", names_tmp)
      names_tmp <- gsub("_+$", "", names_tmp)

      is_valid_dist <- function(term) {
        grepl("(_distribution$|_dist$|^distribution_|^dist_|_distribution_|_dist_)", term) && !grepl("distances$", term)
      }

      valid_mask <- vapply(names_tmp, is_valid_dist, logical(1))
      if (any(!valid_mask)) {
        raw_labels <- attr(terms(formula), "term.labels")
        bad_terms <- raw_labels[!valid_mask]
        warning(paste0("Unrecognized terms deleted: ", paste(bad_terms, collapse = ", ")))
        formula <- update(formula, as.formula(paste(". ~ . -", paste(bad_terms, collapse = " - "))))
        names_tmp <- names_tmp[valid_mask]
      }


      observed <- eval_change(formula = formula, object = private$.iglm.data)
      names(observed) <- names_tmp
      base_name <- unlist(lapply(rhs_terms_as_list(update(formula, a ~ .)), function(x) {
        x$base_name
      }))

      ranges_tmp <- lapply(seq_along(names_tmp), function(idx) {
        bn <- base_name[idx]
        if (bn %in% c("y_distribution", "y_dist") && private$.iglm.data$family_y == "normal") {
          all_y <- c(private$.iglm.data$y_attribute, unlist(lapply(private$.results$samples, function(s) s$y_attribute)))
          return(range(all_y, na.rm = TRUE))
        }
        if (bn %in% c("x_distribution", "x_dist") && private$.iglm.data$family_x == "normal") {
          all_x <- c(private$.iglm.data$x_attribute, unlist(lapply(private$.results$samples, function(s) s$x_attribute)))
          return(range(all_x, na.rm = TRUE))
        }
        x <- observed[[idx]]
        if (is.numeric(x)) {
          return(range(as.numeric(names(x))[is.finite(as.numeric(names(x)))]))
        } else if (is.list(x)) {
          return(range(as.numeric(unlist(lapply(x, names)))))
        } else {
          return(NULL)
        }
      })
      names(ranges_tmp) <- rep("value_range", length(names_tmp))

      # For normal distribution terms, re-evaluate observed with the common value_range
      for (idx in seq_along(names_tmp)) {
        bn <- base_name[idx]
        if (bn %in% c("y_distribution", "y_dist") && private$.iglm.data$family_y == "normal") {
          observed[[idx]] <- private$.iglm.data$y_distribution(value_range = ranges_tmp[[idx]], plot = FALSE)
        } else if (bn %in% c("x_distribution", "x_dist") && private$.iglm.data$family_x == "normal") {
          observed[[idx]] <- private$.iglm.data$x_distribution(value_range = ranges_tmp[[idx]], plot = FALSE)
        }
      }

      simulated <- lapply(private$.results$samples,
        function(object, info_tmp, names_tmp) {
          res <- eval_change(formula = formula, object = object, additional_args = ranges_tmp)
          names(res) <- names_tmp
          return(res)
        },
        names_tmp = names_tmp, info_tmp = info_tmp
      )


      res <- list(
        observed = observed,
        simulated = simulated,
        base_name = base_name,
        names = names_tmp,
        include_mcmc = include_mcmc,
        name = private$.name,
        sufficient_statistics = sufficient_statistics
      )
      class(res) <- "iglm_model_assessment"
      private$.results$set_model_assessment(res)
      if (plot) {
        private$.results$plot(model_assessment = TRUE)
      }
      invisible(res)
    },
    #' @description
    #' Print a summary of the `iglm.object`. If estimation results are
    #' available, they are printed in a standard coefficient table format.
    #' @param digits (integer) Number of digits for rounding numeric output.
    #' @param rows If a numeric vector with values between 1 and 5 is provided,
    #'              only the corresponding columns are printed (1: Estimate, 2: S.E., 3: t-value, 4: Pr(>|t|), 5: Global Count of Sufficient Statistic). Default is `c(1, 2)` to show only estimates and standard errors.
    #' @param signif.stars (logical) If `TRUE`, prints significance stars for the coefficients. Default is `getOption("show.signif.stars")`.
    #' @param eps.Pvalue (numeric) Tolerance for small p-values. Default is `0.0001`.
    #' @param print.formula (logical) If `TRUE` (default), prints the model formula.
    #' @param print.fitinfo (logical) If `TRUE` (default), prints information about the estimation results.
    #' @param print.coefmat (logical) If `TRUE` (default), prints the coefficient table.
    #' @param print.call (logical) If `TRUE` (default), prints the call that generated the object.
    #' @param canonical_names (logical) If `TRUE`, prints canonical term names without label substitution. Default is `FALSE`.
    #' @param ... Additional arguments passed to \code{\link{printCoefmat}}.
    print = function(digits = 3,
                     rows = c(1, 2),
                     signif.stars = getOption("show.signif.stars"),
                     eps.Pvalue = 0.0001,
                     print.formula = TRUE,
                     print.fitinfo = TRUE,
                     print.coefmat = TRUE,
                     print.call = TRUE,
                     canonical_names = FALSE, ...) {
      # Validation
      if (length(digits) != 1 || !is.numeric(digits) || digits < 0) {
        stop("`digits` must be a single non-negative integer.", call. = FALSE)
      }

      # --- Prepare outputs for dynamic width calculation ---
      call_out <- if (print.call && !is.null(private$.results$call)) {
        paste0("Call:\n", paste(deparse(private$.results$call), sep = "\n", collapse = "\n"), "\n\n")
      }

      formula_out <- if (print.formula) {
        paste0("Formula:\n", paste(deparse(private$.formula), sep = "\n", collapse = "\n"), "\n\n")
      }

      results_header <- NULL
      coefmat_out <- NULL
      table_width <- 50 # Default fallback

      # --- Results Table Logic ---
      if (nrow(private$.results$coefficients_path) > 0) {
        if (print.fitinfo) {
          results_header <- "Results: \n\n"
        }
        names <- if (canonical_names) {
          private$.preprocess$coef_names
        } else {
          format_term_names(private$.preprocess$coef_names, private$.iglm.data, canonical_names = FALSE)
        }
        est <- as.numeric(private$.coef)
        stderr <- as.numeric(sqrt(diag(private$.results$var)))
        tvalue <- est / stderr
        pvalue <- 2 * pnorm(-abs(tvalue))

        coef_table <- matrix(c(est, stderr, tvalue, pvalue), ncol = 4)
        colnames(coef_table) <- c("Estimate", "S.E.", "t-value", "Pr(>|t|)")
        if (5 %in% rows) {
          coef_table <- cbind(coef_table, "Suff. Statistic" = as.numeric(private$.sufficient_statistics))
        }
        rownames(coef_table) <- names

        cols_to_print <- intersect(rows, 1:ncol(coef_table))
        has_p <- 4 %in% cols_to_print
        cs_ind <- which(cols_to_print %in% 1:2)
        tst_ind <- which(cols_to_print == 3)

        if (print.coefmat) {
          # Capture output to determine width
          coef_table_print <- coef_table[, cols_to_print, drop = FALSE]

          coefmat_out <- capture.output({
            printCoefmat(coef_table_print,
              digits = digits,
              has.Pvalue = has_p,
              P.values = has_p,
              cs.ind = if (length(cs_ind) > 0) cs_ind else NULL,
              tst.ind = if (length(tst_ind) > 0) tst_ind else NULL,
              dig.tst = digits,
              signif.stars = signif.stars,
              na.print = "NA",
              eps.Pvalue = eps.Pvalue,
              ...
            )
          })
          if (length(coefmat_out) > 0) {
            # Find the widest line (typically the header or data rows)
            table_width <- max(nchar(coefmat_out))
          }
        }
      }

      # --- Final Output ---
      cat("iglm object\n")
      cat(strrep("-", table_width), "\n", sep = "")

      if (nrow(private$.results$coefficients_path) > 0) {
        if (!is.null(call_out)) cat(call_out)
        if (!is.null(formula_out)) cat(formula_out)
        if (!is.null(results_header)) cat(results_header)
        if (!is.null(coefmat_out)) cat(coefmat_out, sep = "\n")

        cat(paste(
          "\nTime for estimation: ",
          format(private$.time_estimation, digits = digits),
          "\n",
          sep = ""
        ))

        if (private$.preprocess$includes_degrees) {
          cat("\nDegree Parameters:\n")
          format_summary <- function(x, d) {
            s <- summary(x)
            return(format(s, digits = d))
          }
          if (private$.iglm.data$directed) {
            cat("  Outdegrees:\n")
            print(format_summary(private$.coef_degrees[1:private$.iglm.data$n_units], digits), quote = FALSE)
            cat("\n  Indegrees:\n")
            print(format_summary(private$.coef_degrees[(private$.iglm.data$n_units + 1):(2 * private$.iglm.data$n_units)], digits), quote = FALSE)
          } else {
            print(format_summary(as.vector(private$.coef_degrees), digits), quote = FALSE)
          }
        }
        invisible(coef_table)
      } else {
        cat("\n")
        cat("Observed Sufficient Statistics:\n")
        stats_to_print <- private$.sufficient_statistics
        names(stats_to_print) <- if (canonical_names) {
          private$.preprocess$coef_names
        } else {
          format_term_names(private$.preprocess$coef_names, private$.iglm.data, canonical_names = FALSE)
        }
        print(format(stats_to_print, digits = digits), quote = FALSE)
        invisible(NULL)
      }
    },
    #' @description
    #' Plot the estimation results, including coefficient convergence
    #' paths and model assessment diagnostics if available.
    #' @param stats (logical) If `TRUE`, plot the observed vs. simulated
    #'  statistics from model assessment. Default is `FALSE`.
    #' @param trace (logical) If `TRUE`, plot the coefficient convergence
    #'  paths. Default is `FALSE`.
    #' @param model_assessment (logical) If `TRUE`, plot diagnostics from the
    #'  model assessment (if already carried out). Default is `FALSE`.
    #' @param ... If the plot of the model_assessment is wanted, additional fits with identical model_assessment terms are currently identified from this argument.
    #'    The names of the arguments are shown as the legend in the model assessment plots.
    plot = function(stats = FALSE, trace = FALSE, model_assessment = FALSE, ...) {
      private$.results$plot(stats = stats, trace = trace, model_assessment = model_assessment, ...)
    },
    #' @description
    #' Gathers all components of the \code{\link{iglm.object}} into a single list for
    #' easy saving or inspection.
    #' @return A list containing all key components of the \code{\link{iglm.object}}.
    #'   This includes the formula, coefficients, sampler, control settings,
    #'   preprocessing info, time taken for estimation, count statistics,
    #'   results, and the underlying \code{\link{iglm.data}} data object.
    gather = function() {
      list(
        formula = private$.formula,
        coef = private$.coef,
        coef_degrees = private$.coef_degrees,
        sampler = private$.sampler,
        control = private$.control,
        preprocess = private$.preprocess,
        time_estimation = private$.time_estimation,
        sufficient_statistics = private$.sufficient_statistics,
        results = private$.results$gather(),
        name = private$.name,
        iglm.data = private$.iglm.data$gather()
      )
    },
    #' @description
    #' Set the name of the \code{\link{iglm.object}}.
    #' @param name (character) The name to assign to the object.
    #' @return The name of the object as a character string.
    set_name = function(name) {
      if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a single character string.", call. = FALSE)
      }
      private$.name <- name
    },
    #' @description
    #' Set control parameters for model estimation.
    #' @param control A \code{\link{control.iglm}} object specifying new
    #'  control settings.
    #' @return Invisibly returns `NULL`.
    set_control = function(control) {
      if (!inherits(control, "control.iglm")) {
        stop("`control` must be a 'control.iglm' object created with control.iglm().")
      }
      private$.control <- control
    },
    #' @description
    #' Save the \code{\link{iglm.object}} to a file in RDS format.
    #' @param file (character) File path to save the object to (has to be a RDS object).
    #' @return Invisibly returns `NULL`.
    save = function(file = NULL) {
      if (missing(file) || !is.character(file) || length(file) != 1) {
        stop("A valid 'file' (character string) must be provided.", call. = FALSE)
      }
      extension <- tools::file_ext(file)
      if (tolower(extension) != "rds") {
        stop("File extension must be .rds", call. = FALSE)
      }
      data_to_save <- self$gather()
      saveRDS(data_to_save, file = file)
      message(paste("Object state saved to:", file))
    },
    #' @description
    #' Estimate the model parameters using the specified control settings.
    #' Stores the results internally and updates the coefficient fields.
    #' @return If no preprocessing should be returned (as per control settings),
    #'   this function returns a list containing detailed estimation results,
    #'   invisibly. Includes final coefficients, variance-covariance matrix,
    #'   convergence path, Fisher information, score vector, log-likelihood,
    #'   and any simulations performed during estimation.
    #'   Else, the function returns a list of the desired preprocessed data
    #'   (as a data.frame) and needed time.
    estimate = function() {
      # private$.coef_degrees[is.na(private$.coef_degrees)] <- -50
      # browser()
      now <- Sys.time()

      if (private$.iglm.data$fix_z && private$.preprocess$includes_degrees) {
        warning("fix_z = TRUE is incompatible with models including degree parameters.
                                                   Setting includes_degrees = FALSE.")
        private$.preprocess$includes_degrees <- FALSE
      }

      info <- estimate_xyz(
        formula = private$.formula,
        preprocessed = private$.preprocess,
        control = private$.control,
        sampler = private$.sampler,
        beg_coef = private$.coef,
        nonoverlap_random = !private$.iglm.data$fix_z_alocal,
        beg_coef_degrees = private$.coef_degrees_internal,
        data_object = private$.iglm.data,
        start = nrow(private$.results$coefficients_path)
      )
      # private$.preprocess$includes_degrees

      private$.time_estimation <- Sys.time() - now
      if (private$.control$estimate_model) {
        if (private$.preprocess$includes_degrees) {
          coefficients_degrees_internal <- info$coefficients_degrees
          tmp <- private$.iglm.data$degree()
          if (private$.iglm.data$directed) {
            coefficients_degrees_internal[which(tmp$in_degree_seq == 0) + private$.iglm.data$n_units] <- NA
            coefficients_degrees_internal[which(tmp$out_degree_seq == 0)] <- NA
          } else {
            coefficients_degrees_internal[which(tmp$degree_seq == 0)] <- NA
          }
        }

        if (is.null(info$var)) {
          if (private$.preprocess$includes_degrees) {
            info$var <- solve(info$fisher_nondegrees)
          } else {
            info$var <- solve(info$fisher)
          }
        }
        if (length(info$where_wrong) > 0) {
          private$.sufficient_statistics <- private$.sufficient_statistics[-(info$where_wrong + 1)]
          if (private$.preprocess$includes_degrees) {
            private$.results$resize(
              size_coef = length(info$coefficients_nondegrees),
              size_coef_degrees =
                (private$.iglm.data$n_units + private$.iglm.data$n_units * private$.iglm.data$directed) *
                  private$.preprocess$includes_degrees
            )
          } else {
            private$.results$resize(
              size_coef = length(info$coefficients),
              size_coef_degrees =
                (private$.iglm.data$n_units + private$.iglm.data$n_units * private$.iglm.data$directed) *
                  private$.preprocess$includes_degrees
            )
          }
          private$.formula <- update_formula_remove_terms(
            private$.formula,
            private$.preprocess$coef_names[info$where_wrong + 1]
          )
          private$.preprocess$coef_names <- private$.preprocess$coef_names[-(info$where_wrong + 1)]
        }

        if (length(info$simulations) > 0) {
          tmp <- lapply(
            seq_along(info$simulations),
            function(x) {
              iglm.data(
                x_attribute = info$simulations[[x]]$x_attribute,
                y_attribute = info$simulations[[x]]$y_attribute,
                z_network = info$simulations[[x]]$z_network,
                n_units = private$.iglm.data$n_units,
                return_neighborhood = FALSE,
                directed = private$.iglm.data$directed,
                family_x = private$.iglm.data$family_x,
                family_y = private$.iglm.data$family_y,
                scale_x = private$.iglm.data$scale_x,
                scale_y = private$.iglm.data$scale_y,
                fix_x = private$.iglm.data$fix_x,
                fix_z = private$.iglm.data$fix_z,
                fix_z_alocal = private$.iglm.data$fix_z_alocal,
                label_x = private$.iglm.data$label_x,
                label_y = private$.iglm.data$label_y,
                label_z = private$.iglm.data$label_z
              )
            }
          )
          class(tmp) <- "iglm.data.list"
          attr(tmp, "neighborhood") <- iglm.data.neighborhood(private$.iglm.data$neighborhood)
          info$simulations <- tmp
          colnames(info$stats) <- private$.preprocess$coef_names
        }

        # Remove samples because they would not anymore be consistent with the currect estimates
        private$.results$remove_samples()
        # Update the internal results object
        if (private$.preprocess$includes_degrees) {
          private$.results$update(
            samples = info$simulations,
            var = info$var,
            coefficients_path = info$coefficients_path,
            fisher_degrees = info$fisher_degrees,
            fisher_nondegrees = info$fisher_nondegrees,
            score_degrees = info$score_degrees,
            score_nondegrees = info$score_nondegrees,
            llh = info$llh,
            stats = info$stats,
            estimated = TRUE
          )
        } else {
          private$.results$update(
            samples = info$simulations,
            var = info$var,
            coefficients_path = info$coefficients_path,
            fisher_nondegrees = info$fisher,
            score_nondegrees = info$score,
            llh = info$llh,
            stats = info$stats,
            estimated = TRUE
          )
        }

        if (private$.preprocess$includes_degrees) {
          private$.coef <- info$coefficients_nondegrees
          private$.coef_degrees <- coefficients_degrees_internal
          private$.coef_degrees_internal <- info$coefficients_degrees
        } else {
          private$.coef <- info$coefficients
        }
        private$.validate()

        if (private$.control$display_progress) {
          self$print(print.formula = FALSE, print.call = FALSE)
        }
      } else {
        if (private$.control$display_progress) {
          cat("\nEstimation skipped as per control settings.\n")
        }
      }
      if (!is.null(info$preprocess)) {
        if (private$.control$display_progress) {
          cat("\nPreprocessing is returned from estimation.\n")
        }
        return(info$preprocess)
      }

      invisible(info)
    },
    #' @description
    #' Provides a summary of the estimation results. Requires the model to have been estimated first.
    #' @param digits (integer) Number of digits for rounding numeric output. Default is 2.
    #' @param canonical_names (logical) If `TRUE`, print canonical term names without label substitution. Default is `FALSE`.
    #' @param ... Additional arguments passed to \code{\link{printCoefmat}}.
    #' @return Prints the summary to the console and invisibly returns the coefficient table (or \code{NULL} if the model has not been estimated).
    summary = function(digits = 2, canonical_names = FALSE, ...) {
      self$print(digits = digits, rows = c(1, 2, 3, 4), print.formula = FALSE, canonical_names = canonical_names, ...)
    },
    #' @description
    #' Simulate networks from the fitted model or a specified model. Stores
    #' the simulations and/or summary statistics internally. The simulation
    #' is carried out using the internal MCMC sampler described in \code{\link{simulate_iglm}}.
    #' @param only_stats (logical) If `TRUE`, only calculate and store summary
    #'   statistics for each simulation, discarding the network object itself.
    #'   Default is `FALSE`.
    #' @param display_progress (logical) If `TRUE` (default), display a
    #'   progress bar during simulation.
    #' @param offset_nonoverlap (numeric) Offset to apply for non-overlapping
    #'   dyads during simulation (if applicable to the sampler). This option
    #'   is useful if the sparsity of edges of units with non-overlapping
    #'   neighborhoods is known. Default is 0.
    #' @return A list containing the simulated networks (`samples`, as a
    #'   `iglm.data.list` if `only_stats = FALSE`) and/or their summary
    #'   statistics (`stats`), invisibly.
    #' @importFrom grDevices col2rgb rgb
    simulate = function(only_stats = FALSE, display_progress = TRUE,
                        offset_nonoverlap = 0) {
      # debugonce(simulate_iglm)
      # browser()
      if (length(self$results$samples) > 0) {
        basis <- self$results$samples[[length(self$results$samples)]]
      } else {
        basis <- self$iglm.data
      }

      info <- simulate_iglm(
        formula = private$.formula, coef = private$.coef,
        coef_degrees = private$.coef_degrees,
        sampler = private$.sampler,
        only_stats = only_stats,
        display_progress = display_progress,
        offset_nonoverlap = offset_nonoverlap,
        cluster = private$.control$cluster,
        basis = basis
      )

      private$.results$update(
        samples = info$samples,
        stats = info$stats,
        estimated = length(private$.coef) > 0
      )
      private$.validate()
      invisible(private$.results$simulation)
    },
    #' @description
    #' Calculates predicted values for the nodal covariates (\code{x}), the outcome variable (\code{y}),
    #' and the network structure (\code{z}). The function supports two prediction modes:
    #' \emph{marginal} (based on Monte Carlo integration over simulated samples) and
    #' \emph{conditional} (based on the analytical linear predictor and point estimates).
    #'
    #' @param variant A character string specifying the type of prediction to generate.
    #'   Must be one of:
    #'   \itemize{
    #'     \item \code{"marginal"}: Computes predictions by aggregating over the MCMC samples stored
    #'     in the internal results. If samples do not exist, \code{self$simulate()} is triggered automatically.
    #'     \item \code{"conditional"}: Computes predictions using the systematic component of the
    #'     Generalized Linear Model (GLM). It calculates the linear predictor \eqn{\eta = X\beta}
    #'     (plus offset and degrees terms for the network) and applies the inverse link function
    #'     \eqn{\mu = g^{-1}(\eta)}.
    #'   }
    #'   Defaults to \code{c("conditional", "marginal")}.
    #' @param type A character vector indicating which components to predict. Options are:
    #'   \itemize{
    #'     \item \code{"x"}: Nodal covariates.
    #'     \item \code{"y"}: Nodal outcome variable.
    #'     \item \code{"z"}: Dyadic network structure (interaction probabilities).
    #'   }
    #'   Defaults to \code{c("x", "y", "z")}.
    #'
    #' @details
    #' \strong{Marginal Predictions:}
    #' When \code{variant = "marginal"}, the function approximates the expected value via Monte Carlo integration:
    #' \deqn{\hat{\mu} = \frac{1}{S} \sum_{s=1}^{S} h^{(s)}}
    #' where \eqn{h^{(s)}} are the realized values from the \eqn{s}-th simulation sample (being either attribute x, y or the connections z).
    #' For the network \code{z}, this results in a marginal edge probability matrix averaged over all sampled networks.
    #'
    #' \strong{Conditional Predictions:}
    #' When \code{variant = "conditional"}, the function calculates the theoretical mean \eqn{\mu} based on the
    #' estimated coefficients \eqn{\hat{\theta}}:
    #' \itemize{
    #'   \item For \strong{Binomial} families: \eqn{\mu = (1 + \exp(-\eta))^{-1}} (Logistic).
    #'   \item For \strong{Poisson} families: \eqn{\mu = \exp(\eta)} (Exponential).
    #'   \item For \strong{Normal} families: \eqn{\mu = \eta} (Identity).
    #' }
    #' For the network component \code{z}, the linear predictor includes dyadic covariates,
    #' degrees effects (sender/receiver variances), and structural offsets.
    #'
    #' @return A list containing the requested predictions:
    #' \describe{
    #'   \item{\code{x}, \code{y}}{A matrix or data frame where the first column is the unit ID and subsequent
    #'   columns represent the predicted mean values.}
    #'   \item{\code{z}}{A data frame containing the edgelist with columns: \code{sender}, \code{receiver},
    #'   and \code{prediction} (probability or intensity).}
    #' }
    #' The results are also invisibly stored in the internal state \code{private$.results}.
    predict = function(variant = c("conditional", "marginal"), type = c("x", "y", "z")) {
      if (!private$.results$estimated) {
        stop("Model has not been estimated yet, prediction thus makes little sense.", call. = FALSE)
      }
      res <- list()
      if (length(variant) > 1) {
        stop("Please provide only one variant: 'conditional' or 'marginal'.", call. = FALSE)
      }
      if (variant == "marginal") {
        if (is.null(private$.results$samples)) {
          self$simulate()
        }
        if ("x" %in% type) {
          res$x <- data.frame(cbind(
            1:private$.iglm.data$n_units, private$.iglm.data$x_attribute,
            rowMeans(vapply(private$.results$samples, function(x) x$x_attribute, numeric(private$.iglm.data$n_units)))
          ))
          names(res$x) <- c("unit", "target", "prediction")
        }
        if ("y" %in% type) {
          res$y <- data.frame(cbind(
            1:private$.iglm.data$n_units, private$.iglm.data$y_attribute,
            rowMeans(vapply(private$.results$samples, function(x) x$y_attribute, numeric(private$.iglm.data$n_units)))
          ))
          names(res$y) <- c("unit", "target", "prediction")
        }
        if ("z" %in% type) {
          matrices_list <- lapply(private$.results$samples, function(x) {
            sparseMatrix(x$z_network[, 1], x$z_network[, 2],
              symmetric = !x$directed,
              dims = c(x$n_units, x$n_units)
            )
          })
          res_z <- Reduce("+", matrices_list) / length(matrices_list)
          res_z <- as.matrix(res_z)
          rownames(res_z) <- colnames(res_z) <- paste0(1:private$.iglm.data$n_units)
          network_obs <- matrix(0, nrow = private$.iglm.data$n_units, ncol = private$.iglm.data$n_units)
          network_obs[private$.iglm.data$z_network] <- 1
          res$z <- data.frame(
            sender = rownames(res_z)[row(res_z)],
            receiver = colnames(res_z)[row(res_z)],
            target = as.vector(network_obs),
            prediction = as.vector(res_z)
          )
        }
      } else if (variant == "conditional") {
        # browser()
        control_old <- private$.control
        private$.control$estimate_model <- FALSE
        private$.control$display_progress <- FALSE
        private$.control$return_x <-
          private$.control$return_y <-
          private$.control$return_z <- FALSE
        if ("x" %in% type) {
          private$.control$return_x <- TRUE
          info <- self$estimate()
          lp <- info$res_x[, -c(1, 2)] %*% private$.coef
          if (private$.iglm.data$family_x == "binomial") {
            mu <- 1 / (1 + exp(-lp))
          } else if (private$.iglm.data$family_x == "normal") {
            mu <- lp
          } else if (private$.iglm.data$family_x == "poisson") {
            mu <- exp(lp)
          }
          private$.control$return_x <- FALSE
          res$x <- data.frame(cbind(
            info$res_x[, c(2, 1)],
            mu
          ))
          names(res$x) <- c("unit", "target", "prediction")
        }
        if ("y" %in% type) {
          private$.control$return_y <- TRUE
          info <- self$estimate()
          lp <- info$res_y[, -c(1, 2)] %*% private$.coef
          if (private$.iglm.data$family_y == "binomial") {
            mu <- 1 / (1 + exp(-lp))
          } else if (private$.iglm.data$family_y == "normal") {
            mu <- lp
          } else if (private$.iglm.data$family_y == "poisson") {
            mu <- exp(lp)
          }
          res$y <- data.frame(cbind(
            info$res_y[, c(2, 1)],
            mu
          ))
          names(res$y) <- c("unit", "target", "prediction")

          private$.control$return_y <- FALSE
        }
        if ("z" %in% type) {
          private$.control$return_z <- TRUE
          # debugonce(estimate_xyz)
          info <- self$estimate()
          # private$.control$offset_nonoverlap[]
          lp <- info$res_z[, -c(1, 2, 3, 4)] %*% private$.coef + private$.coef_degrees[info$res_z[, 3]] +
            private$.coef_degrees[info$res_z[, 3] + private$.iglm.data$n_units * private$.iglm.data$directed] +
            private$.control$offset_nonoverlap * (1 - info$res_z[, 4])
          mu <- 1 / (1 + exp(-lp))
          res$z <- data.frame(cbind(
            info$res_z[, c(2, 3, 1)],
            mu
          ))
          names(res$z)[4] <- c("prediction")
        }
      }
      class(res) <- "iglm.prediction"
      private$.results$set_prediction(res)
      invisible(res)
    },
    #' @description
    #' Manually set the model coefficients to new values.
    #' This is useful for sensitivity analyses or
    #' applying the model to different scenarios.
    #' @param coef A numeric vector of new coefficient values for the non-degrees terms.
    #' @param coef_degrees A numeric vector of new coefficient values for the degrees terms,
    #'   if applicable. Must be provided if the model includes degrees effects.
    #' @return The \code{\link{iglm.object}} itself, invisibly.
    set_coefficients = function(coef, coef_degrees = NULL) {
      if (length(coef) != length(private$.coef)) {
        stop(paste0(
          "Length of `coef` (", length(coef),
          ") does not match the number of model coefficients (",
          length(private$.coef), ")."
        ), call. = FALSE)
      }
      private$.coef <- coef
      if (private$.preprocess$includes_degrees) {
        if (is.null(coef_degrees)) {
          stop("Model includes degrees effects; `coef_degrees` must be provided.", call. = FALSE)
        }
        if (length(coef_degrees) != length(private$.coef_degrees)) {
          stop(paste0(
            "Length of `coef_degrees` (", length(coef_degrees),
            ") does not match the number of degrees coefficients (",
            length(private$.coef_degrees), ")."
          ), call. = FALSE)
        }
        private$.coef_degrees <- coef_degrees
        private$.coef_degrees_internal <- coef_degrees
      }
      private$.validate()
      invisible(self)
    },
    #' @description
    #' Retrieve the simulated networks stored in the object.
    #' Requires \code{simulate} or \code{estimate} to have been run first.
    #' @return A list of \code{\link{iglm.data}} objects representing
    #'   the simulated networks, invisibly. Returns an error if no samples
    #'   are available.
    get_samples = function() {
      if (is.null(private$.results$samples)) {
        stop("No samples available. Please run `simulate()` first.", call. = FALSE)
      } else {
        invisible(private$.results$samples)
      }
    },
    #' @description
    #' Replace the internal MCMC sampler with a new one.
    #' This is useful for changing the sampling scheme without
    #' redefining the entire model.
    #' @param sampler A \code{\link{sampler.iglm}} object.
    #'  @return The \code{\link{iglm.object}} itself, invisibly.
    set_sampler = function(sampler) {
      if (!inherits(sampler, "sampler.iglm")) {
        stop("`sampler` must be a 'sampler.iglm' object created with sampler.iglm().")
      }
      private$.sampler <- sampler
      if (private$.control$display_progress) {
        cat("Sampler has been set successfully.\n")
      }
      invisible(self)
    },
    #' @description
    #' Replace the internal `iglm.data` data object with a new one. This is
    #' useful for applying a fitted model to new observed data. Recalculates
    #' count statistics and re-validates the object.
    #' @param x A \code{\link{iglm.data}} object containing the new observed data.
    #' @return The \code{\link{iglm.object}} itself, invisibly.
    set_target = function(x) {
      if (!"iglm.data" %in% class(x)) {
        stop("The target object must be of class 'iglm.data'.", call. = FALSE)
      } else {
        private$.iglm.data <- x
        # Reset the results object
        private$.results <- results(
          size_coef = length(private$.coef),
          size_coef_degrees = length(private$.coef_degrees) *
            private$.preprocess$includes_degrees
        )
      }
      if (private$.control$display_progress) {
        cat("Target iglm.data object has been set successfully.\n")
      }
      private$.calc_sufficient_statistics()
      private$.validate()
    }
  ),
  active = list(
    #' @field formula (`formula`) Read-only. The model formula specifying terms and data object.
    formula = function(value) {
      if (missing(value)) private$.formula else stop("`formula` is read-only.", call. = FALSE)
    },

    #' @field coef (`numeric`) Read-only. The current vector of non-degrees coefficient estimates or initial values.
    coef = function(value) {
      if (missing(value)) private$.coef else stop("`coef` is read-only.", call. = FALSE)
    },

    #' @field coef_degrees (`numeric` or `NULL`) Read-only. The current vector of degrees coefficient estimates or initial values, or `NULL` if not applicable.
    coef_degrees = function(value) {
      if (missing(value)) private$.coef_degrees else stop("`coef_degrees` is read-only.", call. = FALSE)
    },

    #' @field results (`results`) Read-only. The \code{\link{results}} R6 object containing all estimation and simulation outputs.
    results = function(value) {
      if (missing(value)) private$.results else stop("`results` is read-only.", call. = FALSE)
    },

    #' @field iglm.data (`iglm.data`) The associated \code{\link{iglm.data}} R6 object containing the network and attribute data.
    iglm.data = function(value) {
      if (missing(value)) {
        private$.iglm.data
      } else {
        self$set_target(value)
      }
    },

    #' @field control (`control.iglm`) The \code{\link{control.iglm}} object specifying estimation parameters.
    control = function(value) {
      if (missing(value)) private$.control else self$set_control(value)
    },

    #' @field sampler (`sampler.iglm`) The  \code{\link{sampler.iglm}} object specifying MCMC sampling parameters.
    sampler = function(value) {
      if (missing(value)) private$.sampler else self$set_sampler(value)
    },

    #' @field name (`character`) The name of the model.
    name = function(value) {
      if (missing(value)) private$.name else if (is.character(value)) self$name <- value else stop("`name` must be a character string.", call. = FALSE)
    },

    #' @field sufficient_statistics (`numeric`) Read-only. A named vector of the observed network statistics corresponding to the model terms, calculated on the current `iglm.data` data.
    sufficient_statistics = function(value) {
      if (missing(value)) private$.sufficient_statistics else stop("`sufficient_statistics` is read-only.", call. = FALSE)
    }
  )
)

#' @title Construct an iglm Model Specification Object
#' @description
#' \code{R} package \code{iglm} implements generalized linear models (GLMs)
#' for studying relationships among attributes in connected populations,
#' where responses of connected units can be dependent.
#' It extends GLMs for independent responses to dependent responses and can
#' be used for studying spillover in connected populations and other network-mediated phenomena.
#' It is based on a joint probability model for dependent
#' responses (\eqn{Y}), connections \eqn{(Z)}, and predictors \eqn{(X)}.
#' Some of these terms can be fixed depending on the application (see the \code{fix_x}, \code{fix_z}, and \code{fix_z_alocal} arguments in \code{\link{iglm.object}}).
#'
#' @section Model Formulation:
#'
#' The joint probability density is specified as
#' \deqn{f_{\theta}(y,z,x) \propto \Big[\prod_{i=1}^{N} a_x(x_i)\, a_y(y_i) \exp(\theta_g^T \mathbf{g}_i(x_i^*, y_i^*)) \Big] \times
#' \Big[\prod_{i \ne j} a_z(z_{i,j}) \exp(\theta_h^T \mathbf{h}_{i,j}(x_i^*,x_j^*, y_i^*, y_j^*, z)) \Big],}
#' which is defined by two distinct sets of user-specified features:
#' \itemize{
#'   \item \strong{\eqn{\mathbf{g}_i(x_i^*, y_i^*)= (g_i(x_i^*, y_i^*))}}: A vector of unit-level functions (or "g-terms")
#'     that describe the relationship between an individual unit \eqn{i}'s
#'     predictors (\eqn{x_i}) and their own response (\eqn{y_i}).
#'   \item \strong{\eqn{\mathbf{h}_{i,j}(x_i^*,x_j^*, y_i^*, y_j^*, z)= (h_{i,j}(x_i^*,x_j^*, y_i^*, y_j^*, z))}}: A vector of pair-level functions (or "h-terms")
#'     that specify how the connections (\eqn{z}) and responses (\eqn{y_i, y_j})
#'     of a pair of units \eqn{\{i,j\}} depend on each other and the wider
#'     network structure.
#' }
#'
#' This separation allows the model to simultaneously capture individual-level
#' behavior (via \eqn{g_i}) and dyadic, network-based dependencies (via \eqn{h_{i,j}}),
#' including local dependence limited to overlapping neighborhoods.
#' This help page documents the various statistics available in 'iglm',
#' corresponding to the \eqn{g_i} (attribute-level) and \eqn{h_{i,j}} (pair-level)
#' components of the joint model. See \code{\link{iglm-terms}} for details on specifying
#' all model terms via the formula interface.
#'
#'
#' @return An object of class \code{\link{iglm.object}}.
#'
#'
#' @param formula A model `formula` object. The left-hand side should be the
#'   name of a `iglm.data` object available in the calling environment.
#'   See \code{\link{iglm-terms}} for details on specifying the right-hand side terms.
#' @param coef Optional numeric vector of initial coefficients for the structural
#'   (non-degrees) terms in `formula`. If `NULL`, coefficients are
#'   initialized to zero. Length must match the number of terms.
#' @param coef_degrees Optional numeric vector specifying the initial degrees
#'   coefficients. Required if `formula` includes degrees terms, otherwise
#'   should be `NULL`. Length must match `n_units` (for undirected) or
#'   `2 * n_units` (for directed).
#' @param sampler An object of class \code{\link{sampler.iglm}}, controlling the MCMC sampling scheme. If `NULL`,
#'   default sampler settings will be used.
#' @param control An object of class \code{\link{control.iglm}}, specifying parameters for the estimation algorithm.
#'   If `NULL`, default control settings will be used.
#' @param name Optional character string specifying a name for the model.
#' @param file Optional character string specifying a file path to load a
#'  previously saved  \code{\link{iglm.object}} from disk (in RDS format). If provided,
#'  other arguments are ignored and the object is loaded from the file.
#' @param ... Additional arguments. Unrecognized arguments, particularly those from
#'   standard \code{glm} (such as \code{data} and \code{family}), trigger informative errors explaining differences in \code{iglm}.
#' @aliases iglm.object
#' @examples
#' # Example usage:
#' # Create a iglm.data data object (example)
#' n_units <- 50
#' neighborhood <- matrix(1, nrow = n_units, ncol = n_units)
#' xyz_obj <- iglm.data(
#'   neighborhood = neighborhood, directed = FALSE,
#'   family_x = "binomial", family_y = "binomial"
#' )
#' # Define ground truth coefficients
#' gt_coef <- c("edges_local" = 3, "attribute_y" = -1, "attribute_x" = -1)
#' gt_coef_pop <- rnorm(n = n_units, -2, 1)
#' # Define MCMC sampler
#' sampler_new <- sampler.iglm(
#'   n_burn_in = 100, n_simulation = 10,
#'   sampler_x = sampler.net.attr(n_proposals = n_units * 10),
#'   sampler_y = sampler.net.attr(n_proposals = n_units * 10),
#'   sampler_z = sampler.net.attr(n_proposals = sum(neighborhood > 0) * 10),
#'   init_empty = FALSE
#' )
#' # Create iglm model specification
#' model_tmp_new <- iglm(
#'   formula = xyz_obj ~ edges(mode = "local") +
#'     attribute_y + attribute_x + degrees,
#'   coef = gt_coef,
#'   coef_degrees = gt_coef_pop,
#'   sampler = sampler_new,
#'   control = control.iglm(
#'     accelerated = FALSE,
#'     max_it = 200, display_progress = FALSE
#'   )
#' )
#' # Simulate from the model
#' model_tmp_new$simulate()
#' model_tmp_new$set_target(model_tmp_new$get_samples()[[1]])
#'
#' # Estimate model parameters
#' model_tmp_new$estimate()
#'
#' # Model Assessment
#' model_tmp_new$assess(formula = ~degree_distribution)
#' model_tmp_new$results$plot(model_assessment = TRUE)
#'
#' @references
#'
#' Fritz, C., Schweinberger, M., Bhadra, S., and D.R. Hunter (2025). A Regression Framework for Studying Relationships among Attributes under Network Interference. Journal of the American Statistical Association, to appear.
#'
#' Schweinberger, M. and M.S. Handcock (2015). Local Dependence in Random Graph Models: Characterization, Properties, and Statistical Inference. Journal of the Royal Statistical Society, Series B (Statistical Methodology), 7, 647-676.
#'
#' Schweinberger, M. and J.R. Stewart (2020). Concentration and Consistency Results for Canonical and Curved Exponential-Family Models of Random Graphs. The Annals of Statistics, 48, 374-396.
#'
#' Stewart, J.R. and M. Schweinberger (2025). Pseudo-Likelihood-Based M-Estimation of Random Graphs with Dependent Edges and Parameter Vectors of Increasing Dimension. The Annals of Statistics, to appear.
#'
#' @export
iglm <- function(formula = NULL, coef = NULL, coef_degrees = NULL, sampler = NULL,
                 control = NULL, name = NULL, file = NULL, ...) {
  check_glm_arguments(list(...))
  iglm.object.generator$new(
    formula = formula,
    coef = coef,
    coef_degrees = coef_degrees,
    sampler = sampler,
    control = control,
    name = name,
    file = file,
    ...
  )
}
