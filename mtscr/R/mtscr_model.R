#' Create MTS model
#'
#' Create MTS model for creativity analysis.
#'
#' @inheritParams mtscr_prepare
#' @param top Integer or vector of integers (see examples), number of top answers
#'     to include in the model. Default is 1, i.e. only the top answer.
#' @param prepared Logical, is the data already prepared with `mtscr_prepare()`?
#'
#' @return The return value depends on length of the `top` argument. If `top` is a single
#'     integer, a `glmmTMB` model is returned. If `top` is a vector of integers, a list
#'     of `glmmTMB` models is returned, with names corresponding to the `top` values,
#'     e.g. `top1`, `top2`, etc.
#'
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#'
#' mtscr_creativity <- mtscr_creativity |>
#'   dplyr::slice_sample(n = 300) # for performance, ignore
#'
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN) |>
#'   summary()
#'
#' # three models for top 1, 2, and 3 answers
#' mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1:3) |>
#'   mtscr_model_summary()
#'
#' # you can prepare data first
#' data <- mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN)
#' mtscr_model(data, id, item, SemDis_MEAN, prepared = TRUE)
#'
#' # extract effects for creativity score by hand
#' model <- mtscr_model(mtscr_creativity, id, item, SemDis_MEAN, top = 1)
#' creativity_score <- glmmTMB::ranef(model)$cond$id[, 1]
mtscr_model <- function(df, id_column, item_column = NULL, score_column, top = 1, prepared = FALSE, ties_method = c("random", "average"), normalise = TRUE, self_ranking = NULL) {
  id_column <- rlang::ensym(id_column)
  item_column_quo <- rlang::enquo(item_column)
  if (!rlang::quo_is_null(item_column_quo)) {
    item_column <- rlang::ensym(item_column)
  } else {
    item_column <- item_column_quo
  }
  score_column <- rlang::ensym(score_column)
  ties_method <- rlang::arg_match(ties_method)
  self_ranking_quo <- rlang::enquo(self_ranking)
  if (!rlang::quo_is_null(self_ranking_quo)) {
    self_ranking <- rlang::ensym(self_ranking)
  } else {
    self_ranking <- self_ranking_quo
  }

  # check if all .ordering_X columns exist
  present_ordering_columns <- purrr::map(
    as.list(top),
    \(x) {
      paste0(".ordering_top", x)
    }
  )
  if (prepared && !any(present_ordering_columns %in% names(df))) {
    cli::cli_warn(
      c(
        "Couldn't find all {.var .ordering_top} columns.",
        "i" = "The dataframe was prepared again."
      )
    )
    prepared <- FALSE
  }

  # determine if normalise = TRUE when prepared = TRUE
  if (prepared) {
    if (rlang::has_name(df, ".z_score")) {
      normalise <- TRUE
    } else {
      normalise <- FALSE
    }
  }

  # prepare
  if (!prepared) {
    df <- mtscr_prepare(df, !!id_column, !!item_column, !!score_column, top = top, minimal = TRUE, ties_method = ties_method, normalise = normalise, self_ranking = !!self_ranking)
  }

  # implicit conversion to factors
  df <- df |>
    dplyr::mutate(
      !!id_column := factor(!!id_column)
    )

  if (!rlang::quo_is_null(item_column_quo)) {
    df <- df |>
      dplyr::mutate(
        !!item_column := factor(!!item_column)
      )
  }

  if (!rlang::quo_is_null(item_column_quo)) {
    n_items <- length(unique(df[[rlang::as_label(item_column)]])) # number of unique items
  } else {
    n_items <- 1
  }

  # count ordering columns
  ordering_columns <- df |>
    dplyr::select(dplyr::starts_with(".ordering_top")) |>
    names()

  # create formulas
  # formula example: .z_score ~ -1 + item + item:.ordering_topX + (.ordering_topX | id)
  # formula if normalise = FALSE: SemDis_MEAN ~ -1 + item + item:.ordering_topX + (.ordering_topX | id)
  formulas <- purrr::map_vec(
    ordering_columns,
    \(x) {
      if (normalise) {
        formula <- ".z_score"
      } else {
        formula <- rlang::as_label(score_column)
      }
      formula <- paste0(formula, " ~ -1 + ")
      if (n_items != 1) { # item effect only when more than 1 item
        formula <- paste0(
          formula,
          rlang::as_name(item_column),
          " + ",
          rlang::as_name(item_column),
          ":"
        )
      }
      formula <- paste0(
        formula,
        x,
        " + (",
        x,
        " | ",
        rlang::as_name(id_column),
        ")"
      )

      formula |>
        stats::as.formula() |>
        c() # convert to vector
    }
  )

  # models
  models <- purrr::map(formulas, function(formula) {
    glmmTMB::glmmTMB(
      formula,
      data = df,
      family = stats::gaussian()
    )
  })

  if (length(ordering_columns) == 1) {
    return(models[[1]])
  } else {
    names(models) <- paste0("top", stringr::str_remove(ordering_columns, "\\.ordering_top"))
    return(models)
  }
}
