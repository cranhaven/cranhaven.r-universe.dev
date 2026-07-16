#' Build a pass-rate target table
#'
#' @param overall Optional scalar overall target rate.
#' @param groups Named list. Each element is a named numeric vector containing
#'   target rates for one grouping variable.
#' @param interactions Named list of cross-classification (interaction) targets.
#'   Each element name is a colon-joined set of grouping variables
#'   (e.g. `"sex:residence"`) and each element is a named numeric vector whose
#'   names are the matching colon-joined level combinations
#'   (e.g. `c("M:Urban" = 0.7)`).
#' @param overall_priority Positive priority for the overall target.
#' @param group_priority Either one positive scalar or a named positive vector
#'   indexed by group-variable name.
#' @param interaction_priority Either one positive scalar or a named positive
#'   vector indexed by interaction key.
#' @param means,totals Optional data frames of mean/total targets for a numeric
#'   variable, each with columns `variable`, `level`, `value_var` and `target`
#'   (plus optional `priority`). When supplied, the result gains `statistic`,
#'   `value_var` and `value` columns; proportion rows are tagged
#'   `statistic = "proportion"`.
#' @param proportions Optional data frame of proportion targets for a value of a
#'   categorical variable, with columns `variable`, `level`, `value_var`,
#'   `value` and `target` (plus optional `priority`).
#'
#' @return A data frame suitable for `calibrate_pass_rates()`.
#' @examples
#' make_rate_targets(overall = 0.70, groups = list(sex = c(M = 0.72, F = 0.68)))
#' @export
make_rate_targets <- function(
    overall = NULL,
    groups = list(),
    interactions = list(),
    overall_priority = 5,
    group_priority = 1,
    interaction_priority = 1,
    means = NULL,
    totals = NULL,
    proportions = NULL
) {
  if (!is.list(groups) ||
      (length(groups) > 0L && (is.null(names(groups)) || any(names(groups) == "")))) {
    stop("groups must be a named list.", call. = FALSE)
  }

  out <- data.frame(
    variable = character(),
    level = character(),
    target_rate = numeric(),
    priority = numeric(),
    stringsAsFactors = FALSE
  )

  if (!is.null(overall)) {
    if (!is.numeric(overall) || length(overall) != 1L ||
        !is.finite(overall) || overall < 0 || overall > 1) {
      stop("overall must be NULL or one number between 0 and 1.", call. = FALSE)
    }
    if (!is.numeric(overall_priority) || length(overall_priority) != 1L ||
        !is.finite(overall_priority) || overall_priority <= 0) {
      stop("overall_priority must be one finite positive number.", call. = FALSE)
    }
    out <- rbind(out, data.frame(
      variable = ".overall",
      level = ".all",
      target_rate = overall,
      priority = overall_priority,
      stringsAsFactors = FALSE
    ))
  }

  if (length(groups) > 0L) {
    if (length(group_priority) == 1L) {
      group_priority <- stats::setNames(rep(group_priority, length(groups)), names(groups))
    }
    if (is.null(names(group_priority))) {
      stop("group_priority must be a scalar or a named vector.", call. = FALSE)
    }
  }

  for (v in names(groups)) {
    rates <- groups[[v]]
    if (!is.numeric(rates) || is.null(names(rates)) || any(names(rates) == "")) {
      stop("Each groups element must be a named numeric vector: ", v,
           call. = FALSE)
    }
    if (any(!is.finite(rates)) || any(rates < 0 | rates > 1)) {
      stop("All rates in groups[['", v, "']] must be between 0 and 1.",
           call. = FALSE)
    }
    priority <- unname(group_priority[v])
    if (length(priority) != 1L || is.na(priority) || priority <= 0) {
      stop("Missing or invalid group_priority for variable: ", v, call. = FALSE)
    }
    out <- rbind(out, data.frame(
      variable = v,
      level = names(rates),
      target_rate = unname(rates),
      priority = priority,
      stringsAsFactors = FALSE
    ))
  }

  if (!is.list(interactions) ||
      (length(interactions) > 0L &&
       (is.null(names(interactions)) || any(names(interactions) == "")))) {
    stop("interactions must be a named list.", call. = FALSE)
  }
  if (length(interactions) > 0L) {
    if (length(interaction_priority) == 1L) {
      interaction_priority <- stats::setNames(
        rep(interaction_priority, length(interactions)), names(interactions))
    }
    if (is.null(names(interaction_priority))) {
      stop("interaction_priority must be a scalar or a named vector.",
           call. = FALSE)
    }
  }

  for (v in names(interactions)) {
    rates <- interactions[[v]]
    if (!is.numeric(rates) || is.null(names(rates)) || any(names(rates) == "")) {
      stop("Each interactions element must be a named numeric vector: ", v,
           call. = FALSE)
    }
    if (any(!is.finite(rates)) || any(rates < 0 | rates > 1)) {
      stop("All rates in interactions[['", v, "']] must be between 0 and 1.",
           call. = FALSE)
    }
    priority <- unname(interaction_priority[v])
    if (length(priority) != 1L || is.na(priority) || priority <= 0) {
      stop("Missing or invalid interaction_priority for: ", v, call. = FALSE)
    }
    out <- rbind(out, data.frame(
      variable = v,
      level = names(rates),
      target_rate = unname(rates),
      priority = priority,
      stringsAsFactors = FALSE
    ))
  }

  # Optional mean/total/proportion value targets. Their presence switches the
  # table to the extended schema with statistic, value_var and value columns.
  if (!is.null(means) || !is.null(totals) || !is.null(proportions)) {
    out$statistic <- rep("proportion", nrow(out))
    out$value_var <- rep(NA_character_, nrow(out))
    out$value <- rep(NA_character_, nrow(out))
    add_value_targets <- function(out, df, stat) {
      if (is.null(df)) return(out)
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      needed <- c("variable", "level", "value_var", "target")
      if (stat == "proportion") needed <- c(needed, "value")
      if (!all(needed %in% names(df))) {
        stop(stat, " must have columns: ", paste(needed, collapse = ", "),
             call. = FALSE)
      }
      if (any(!is.finite(as.numeric(df$target)))) {
        stop("All ", stat, " target values must be finite.", call. = FALSE)
      }
      prio <- if ("priority" %in% names(df)) as.numeric(df$priority) else 1
      rbind(out, data.frame(
        variable = as.character(df$variable),
        level = as.character(df$level),
        target_rate = as.numeric(df$target),
        priority = prio,
        statistic = stat,
        value_var = as.character(df$value_var),
        value = if (stat == "proportion") as.character(df$value) else NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    out <- add_value_targets(out, means, "mean")
    out <- add_value_targets(out, totals, "total")
    out <- add_value_targets(out, proportions, "proportion")
  }

  rownames(out) <- NULL
  out
}
