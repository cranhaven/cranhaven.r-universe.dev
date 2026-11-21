.astra_variance_metadata <- function (scope, method = NA_character_, primary = FALSE, available = TRUE) 
{
    lens <- max(length(scope), length(method), length(primary), length(available))
    scope <- rep_len(as.character(scope), lens)
    method <- rep_len(method, lens)
    lookup <- c(lin_ying_visit = "lin_ying_marker", liang_visit = "liang_marker", jmvl_g_visit = "jmvl_g_marker", 
        iirr_balancing_visit = "iirr_balancing_marker")
    hit <- as.character(method) %in% names(lookup)
    method[hit] <- unname(lookup[as.character(method[hit])])
    primary <- rep_len(primary, lens)
    available <- rep_len(available, lens)
    one <- function(s, id, pr, av) {
        if (!isTRUE(av)) 
            return(c(variance_type = "unavailable", variance_label = "variance (not available)"))
        if (is.na(s) || !nzchar(s)) 
            s <- "reported covariance; see method documentation"
        ss <- tolower(s)
        plugin <- grepl("plug.in|conditional.*(nuisance|stage|rate|baseline|frailty.moment|reconstruct|completed|history|smoother)|conditional observation|observation_conditional|outcome_conditional|outcome_legacy|legacy outcome", 
            ss) || (isTRUE(pr) && id %in% c("lmm", "oa_lmm", "va_lmm", "mi_lmm", 
            "mi_liang", "li_lmm", "li_liang", "li_iirr")) || (id %in% 
            "mi_liang" && grepl("within|pooled|imputation.*outcome", ss))
        if (grepl("bootstrap", ss)) {
            ty <- "patient_bootstrap"
            label <- "variance (bootstrap)"
        }
        else if (grepl("rubin|pooled", ss) && (isTRUE(pr) || !grepl("rubin_between|between.imputation covariance|between.imputation component", 
            ss))) {
            ty <- if (plugin) 
                "rubin_mi_plugin_within"
            else "rubin_mi"
            label <- if (plugin) 
                "variance (Rubin MI); within-imputation variance (plug-in)"
            else "variance (Rubin MI)"
        }
        else if (grepl("between.imputation|rubin_between", ss)) {
            ty <- "rubin_between"
            label <- "variance (Rubin between-imputation)"
        }
        else if (grepl("branch|active.face|boundary", ss) && grepl("stacked", ss)) {
            ty <- "stacked_branch_conditional"
            label <- "variance (stacked; conditional on selected branch)"
        }
        else if (grepl("gamma_conditional|rate_partial_likelihood_model_based|gamma_model_based|alpha_model_based", 
            ss)) {
            ty <- "model_based"
            label <- "variance (model-based)"
        }
        else if (plugin) {
            ty <- "plug_in_conditional"
            label <- "variance (plug-in)"
        }
        else if (grepl("stacked|breslow.profiled|dyadic", ss)) {
            ty <- "stacked_sandwich"
            label <- "variance (stacked sandwich)"
        }
        else if (grepl("hc3", ss)) {
            ty <- "hc3"
            label <- "variance (HC3)"
        }
        else if (grepl("model.based|observed.*information|marginal.likelihood|nlme|apvar", ss)) {
            ty <- "model_based"
            label <- "variance (model-based)"
        }
        else if (grepl("sandwich|robust", ss)) {
            ty <- "subject_sandwich"
            label <- "variance (subject sandwich)"
        }
        else {
            ty <- "reported"
            label <- "variance (reported)"
        }
        c(variance_type = ty, variance_label = if (grepl("^variance", ss) && plugin && !grepl("rubin", ss)) s else paste0(label, 
            ": ", s))
    }
    z <- mapply(one, scope, method, primary, available, SIMPLIFY = FALSE)
    as.data.frame(do.call(rbind, z), stringsAsFactors = FALSE)
} 
.engine_call_args <- function (fun, data, id, time, outcome, obs_indicator, visit_covs, cimehr_visit_covs, obs_fixed_covs, long_covs, 
    outcome_random_covs, obs_random_covs, focal, seed, entry_col, censor_col, extra, outcome_random_structure = NULL, 
    outcome_random_intercept = TRUE, outcome_fixed_structure = NULL) 
{
    fml <- names(formals(fun))
    candidates <- list(data = data, d = data, id = id, id_col = id, time = time, time_col = time, outcome = outcome, 
        y_col = outcome, obs_indicator = obs_indicator, r_col = obs_indicator, entry_col = entry_col, censor_col = censor_col, 
        visit_covs = visit_covs, covars_visit_XV = visit_covs, cimehr_visit_covs = cimehr_visit_covs, obs_fixed_covs = obs_fixed_covs, 
        long_covs = long_covs, outcome_covs = long_covs, covars_outcome_fixed_XY = long_covs, random_covs = outcome_random_covs, 
        outcome_random_covs = outcome_random_covs, outcome_random_structure = outcome_random_structure, outcome_fixed_structure = outcome_fixed_structure, 
        outcome_random_intercept = outcome_random_intercept, obs_random_covs = obs_random_covs, covars_outcome_random_link_ZY = outcome_random_covs, 
        covars_obs_fixed_XO = obs_fixed_covs, covars_obs_random_link_ZO = obs_random_covs, focal = focal, seed = seed, 
        pair_seed = seed, bootstrap_seed = seed, return_full = TRUE)
    args <- candidates[intersect(names(candidates), fml)]
    if (length(extra)) {
        if (is.null(names(extra)) || any(!nzchar(names(extra)))) {
            stop("Every method-specific control must be named.", call. = FALSE)
        }
        unknown <- setdiff(names(extra), fml)
        if (!"..." %in% fml && length(unknown)) {
            stop("Unknown method-specific control argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
        }
        if ("..." %in% fml) 
            args <- c(args, extra)
        else args <- c(args, extra)
    }
    args[!duplicated(names(args), fromLast = TRUE)]
} 
.normalize_fit <- function (ans, method, elapsed, warnings) 
{
    if (is.numeric(ans) && length(ans) == 1L) {
        ans <- list(method_key = method, method = .cimehr_method_registry$label[match(method, .cimehr_method_registry$method)], estimand = NA_character_, coefficients = c(estimate = ans), 
            vcov = NULL, se = NULL, vcov_type = "not returned", fit = NULL, diagnostics = list(ok = is.finite(ans)), 
            assumptions = character(0))
    }
    if (!is.list(ans)) 
        stop("Method did not return a list or scalar estimate.", call. = FALSE)
    ans$method_key <- as.character(ans$method_key %||% method)
    ans$method <- as.character(ans$method %||% .cimehr_method_registry$label[match(method, .cimehr_method_registry$method)])
    ans$coefficients <- ans$coefficients %||% numeric(0)
    if (is.data.frame(ans$coefficients)) {
        tab <- ans$coefficients
        if (!all(c("term", "estimate") %in% names(tab))) {
            stop(sprintf("Method '%s' returned a coefficient table without `term`/`estimate` columns.", method), 
                call. = FALSE)
        }
        ans$coefficient_table <- tab
        ans$coefficients <- stats::setNames(as.numeric(tab$estimate), as.character(tab$term))
        if ((is.null(ans$se) || !length(ans$se)) && "se" %in% names(tab)) {
            ans$se <- stats::setNames(as.numeric(tab$se), as.character(tab$term))
        }
        if ((is.null(ans$df) || !length(ans$df)) && "df" %in% names(tab)) {
            ans$df <- stats::setNames(as.numeric(tab$df), as.character(tab$term))
        }
    }
    if (!is.numeric(ans$coefficients) || is.list(ans$coefficients)) {
        stop(sprintf("Method '%s' returned non-numeric `coefficients` (class: %s).", method, paste(class(ans$coefficients), 
            collapse = "/")), call. = FALSE)
    }
    ans$diagnostics <- ans$diagnostics %||% list()
    if ((is.null(ans$df) || !length(ans$df)) && length(ans$diagnostics$rubin_df %||% numeric())) {
        ans$df <- ans$diagnostics$rubin_df
    }
    if (is.null(ans$diagnostics$regular) && !is.null(ans$diagnostics$covariance_regular)) 
        ans$diagnostics$regular <- isTRUE(ans$diagnostics$covariance_regular)
    vm <- .astra_variance_metadata(ans$vcov_type %||% "", method, primary = TRUE)
    ans$variance_type <- vm$variance_type
    ans$variance_label <- vm$variance_label
    ans$diagnostics$elapsed_seconds <- elapsed
    ans$diagnostics$warnings <- unique(c(ans$diagnostics$warnings %||% character(0), warnings))
    ans$diagnostics$package_version <- as.character(utils::packageVersion("CIMEHR"))
    ans$diagnostics$object_bytes <- if (method %in% .curve_refit_methods()) 
        NA_real_
    else as.numeric(object.size(ans))
    ans
} 
