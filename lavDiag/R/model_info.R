#' Extract compact, robust meta-information about a lavaan fit
#'
#' @description
#' Lightweight helper that queries a fitted \pkg{lavaan} object
#' for commonly needed model metadata (grouping, variables, estimator,
#' parameterization, categorical/multilevel flags, etc.). All lookups are
#' wrapped in `tryCatch()`, so the function returns informative `NA`/`NULL`
#' values instead of failing when particular slots are unavailable.
#'
#' @details
#' The function relies on stable \code{\link[lavaan]{lavInspect}} /
#' \code{\link[lavaan]{lavNames}} queries with minimal post-processing:
#' \itemize{
#'   \item Grouping: number of groups, labels, grouping variable (if any), and per-group sample sizes.
#'   \item Variables: observed (ov) and latent (lv) in model order; observed split into
#'         ordinal vs. continuous using \code{lavNames(type = "ov.ord")}.
#'   \item Estimator and parameterization: taken from \code{lavInspect(fit, "options")}.
#'   \item Multilevel summary: coarse flags derived from \code{"nlevels"} / \code{"cluster"}
#'         and related quantities (clusters, average cluster size).
#' }
#' All fields are safe to access: if a query is not applicable (e.g., single-group
#' model has no \code{"group"}), the corresponding element is set to \code{NA},
#' \code{NULL}, or a sensible default.
#'
#' @param fit A fitted \code{lavaan} model object.
#'
#' @return A named \code{list} with the following elements:
#' \describe{
#'   \item{\code{converged}}{Logical or \code{NA}; model convergence flag.}
#'   \item{\code{has_meanstructure}}{Logical or \code{NA}; whether a mean structure was estimated.}
#'   \item{\code{estimator}}{Character or \code{NA}; e.g., \code{"ML"}, \code{"WLSMV"}, \code{"Bayes"}.}
#'   \item{\code{parameterization}}{Character or \code{NA}; e.g., \code{"delta"}, \code{"theta"}.}
#'
#'   \item{\code{is_single_group}}{Logical; \code{TRUE} for single-group models.}
#'   \item{\code{n_groups}}{Integer or \code{NA}; number of groups.}
#'   \item{\code{group_var}}{Character or \code{NULL}; name of the grouping variable.}
#'   \item{\code{group_labels}}{Character vector or \code{NULL}; group labels in model order.}
#'   \item{\code{n_obs}}{Per-group sample sizes (vector/list) or \code{NULL}.}
#'
#'   \item{\code{observed_variables}}{Character; observed indicators (ov) in model order.}
#'   \item{\code{latent_variables}}{Character; latent variables (lv) in model order.}
#'   \item{\code{ov_ordinal}}{Character; subset of ordinal observed variables.}
#'   \item{\code{ov_continuous}}{Character; observed variables not in \code{ov_ordinal}.}
#'
#'   \item{\code{is_categorical}}{Logical or \code{NA}; lavaan-level categorical flag.}
#'
#'   \item{\code{is_multilevel}}{Logical; \code{TRUE} if a multilevel structure is detected.}
#'   \item{\code{n_levels}}{Integer or \code{NA}; number of levels.}
#'   \item{\code{cluster_var}}{Character vector or \code{NULL}; clustering variable(s), if any.}
#'   \item{\code{n_clusters}}{Integer or \code{NA}; number of clusters (if available).}
#'   \item{\code{average_cluster_size}}{Numeric or \code{NA}; average cluster size (if available).}
#' }
#'
#' @seealso
#' \code{\link{prepare}}, \code{\link{augment}}, \code{\link{item_data}};
#' \code{\link[lavaan]{lavInspect}}, \code{\link[lavaan]{lavNames}}
#'
#' @examples
#' HS.model <- 'visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9'
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939,
#'                    group = 'school')
#' model_info(fit)
#'
#' @export

model_info <- function(fit) {
  # -- Defensive check ---------------------------------------------------------
  .assert_lavaan_fit(fit)  # requires lavaan-compatible object

  # -- Groups ------------------------------------------------------------------
  n_groups        <- tryCatch(lavaan::lavInspect(fit, "ngroups"),
                              error = function(e) NA_integer_)
  is_single_group <- isTRUE(n_groups == 1L)

  # -- Basic meta --------------------------------------------------------------
  converged        <- tryCatch(lavaan::lavInspect(fit, "converged"),
                               error = function(e) NA)
  has_meanstructure<- tryCatch(lavaan::lavInspect(fit, "meanstructure"),
                               error = function(e) NA)

  # Pull lavaan options once; extract estimator & parameterization from there
  opts <- tryCatch(lavaan::lavInspect(fit, "options"), error = function(e) NULL)

  # Estimator (e.g., "ML", "WLSMV", "Bayes"); NA if missing
  estimator <- if (!is.null(opts) && !is.null(opts$estimator)) opts$estimator else NA_character_

  # Parameterization ("delta" default if missing)
  parameterization <- if (!is.null(opts) && !is.null(opts$parameterization))
    opts$parameterization else NA_character_

  # -- Categorical flag (lavaan-level) ----------------------------------------
  is_categorical <- tryCatch(lavaan::lavInspect(fit, "categorical"),
                             error = function(e) NA)

  # -- Variables ---------------------------------------------------------------
  observed_variables <- tryCatch(lavaan::lavNames(fit, type = "ov"),
                                 error = function(e) character())
  latent_variables   <- tryCatch(lavaan::lavNames(fit, type = "lv"),
                                 error = function(e) character())

  # Separate ordinal vs. continuous observed variables
  ov_ordinal    <- tryCatch(lavaan::lavNames(fit, type = "ov.ord"),
                            error = function(e) character())
  ov_continuous <- setdiff(observed_variables, ov_ordinal)

  # -- Group meta --------------------------------------------------------------
  group_labels <- tryCatch(lavaan::lavInspect(fit, "group.label"),
                           error = function(e) NULL)
  n_obs        <- tryCatch(lavaan::lavInspect(fit, "nobs"),
                           error = function(e) NULL)
  group_var    <- tryCatch(lavaan::lavInspect(fit, "group"),
                           error = function(e) NULL)

  # -- Multilevel detection ----------------------------------------------------
  n_levels         <- tryCatch(lavaan::lavInspect(fit, "nlevels"),
                               error = function(e) NA_integer_)
  cluster_var      <- tryCatch(lavaan::lavInspect(fit, "cluster"),
                               error = function(e) NULL)
  n_clusters       <- tryCatch(lavaan::lavInspect(fit, "nclusters"),
                               error = function(e) NA_integer_)
  avg_cluster_size <- tryCatch(lavaan::lavInspect(fit, "average.cluster.size"),
                               error = function(e) NA_real_)

  is_multilevel <- isTRUE(!is.na(n_levels) && n_levels >= 2L) ||
    (!is.null(cluster_var) && length(cluster_var) > 0L)

  list(
    # --- high-level status ---
    converged          = converged,
    has_meanstructure  = has_meanstructure,
    estimator          = estimator,
    parameterization   = parameterization,

    # --- grouping ---
    is_single_group    = is_single_group,
    n_groups           = n_groups,
    group_var          = group_var,
    group_labels       = group_labels,
    n_obs              = n_obs,

    # --- variables ---
    observed_variables = observed_variables,
    latent_variables   = latent_variables,
    ov_ordinal         = ov_ordinal,
    ov_continuous      = ov_continuous,

    # --- categorical flag ---
    is_categorical     = is_categorical,

    # --- multilevel summary ---
    is_multilevel        = is_multilevel,
    n_levels             = n_levels,
    cluster_var          = cluster_var,
    n_clusters           = n_clusters,
    average_cluster_size = avg_cluster_size
  )
}
