#' Prepare database for MTS
#'
#' Prepare database for MTS analysis.
#'
#' @param df Data frame in long format.
#' @param id_column Name of the column containing participants' id.
#' @param item_column Optional, name of the column containing distinct trials
#'     (e.g. names of items in AUT).
#' @param score_column Name of the column containing divergent thinking scores
#'     (e.g. semantic distance).
#' @param top Integer or vector of integers (see examples), number of top answers
#'     to prepare indicators for. Default is 1, i.e. only the top answer.
#' @param minimal Logical, append columns to df (`FALSE`) or return only `id`, `item`,
#'     and the new columns (`TRUE`).
#' @param ties_method Character string specifying how ties are treated when
#'     ordering. Can be `"average"` (better for continuous scores like semantic
#'     distance) or `"random"` (default, better for ratings). See [rank()] for details.
#' @param normalise Logical, should the creativity score be normalised? Default is `TRUE` and
#'    it's recommended to leave it as such.
#' @param self_ranking Name of the column containing answers' self-ranking.
#'     Provide if model should be based on top answers self-chosen by the participant.
#'     Every item should have its own ranks. The top answers should have a value of 1,
#'     and the other answers should have a value of 0. In that case, the `top` argument
#'     doesn't change anything and should be left as `top = 1`. `ties_method` is not used if `self_ranking`
#'     was provided. See [mtscr_self_rank] for example.
#'
#' @return The input data frame with additional columns:
#'     \describe{
#'         \item{`.z_score`}{Numerical, z-score of the creativity score}
#'         \item{`.ordering`}{Numerical, ranking of the answer relative to participant and item}
#'         \item{`.ordering_topX`}{Numerical, 0 for *X* top answers, otherwise value of `.ordering`}
#'     }
#'     Number of `.ordering_topX` columns depends on the `top` argument. If `minimal = TRUE`,
#'     only the new columns and the item and id columns are returned. The values are
#'     relative to the participant AND item, so the values for different
#'     participants scored for different tasks (e.g. uses for "brick" and "can") are distinct.
#' @export
#'
#' @examples
#' data("mtscr_creativity", package = "mtscr")
#' # Indicators for top 1 and top 2 answers
#' mtscr_prepare(mtscr_creativity, id, item, SemDis_MEAN, top = 1:2, minimal = TRUE)
mtscr_prepare <- function(df, id_column, item_column = NULL, score_column, top = 1, minimal = FALSE, ties_method = c("random", "average"), normalise = TRUE, self_ranking = NULL) {
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

  # check if df is a data frame
  if (!is.data.frame(df)) {
    cli::cli_abort(
      c(
        "{.arg df} must be a data frame.",
        "x" = "{.var {rlang::expr_text(substitute(df))}} is {.obj_type_friendly {df}}"
      )
    )
  }

  # check if columns exist
  if (!rlang::has_name(df, rlang::as_name(id_column))) {
    cli::cli_abort(
      c(
        "All columns must exist in the data.",
        "x" = "Column {.var {id_column}} does not exist.",
        "i" = "Check the spelling."
      )
    )
  }
  if (!rlang::quo_is_null(item_column_quo)) {
    if (!rlang::has_name(df, rlang::as_name(item_column))) {
      cli::cli_abort(
        c(
          "All columns must exist in the data.",
          "x" = "Column {.var {item_column}} does not exist.",
          "i" = "Check the spelling."
        )
      )
    }
  }
  if (!rlang::has_name(df, rlang::as_name(score_column))) {
    cli::cli_abort(
      c(
        "All columns must exist in the data.",
        "x" = "Column {.var {score_column}} does not exist.",
        "i" = "Check the spelling."
      )
    )
  }

  # check if score_column is numeric
  if (!is.numeric(df[[rlang::as_name(score_column)]])) {
    cli::cli_abort(
      c(
        "{.var score_column} must be numeric.",
        "x" = "{.var {rlang::expr_text(substitute(score_column))}} is {.cls {class(df[[rlang::as_name(score_column)]])}}"
      )
    )
  }

  # check if minimal is logical
  if (!is.logical(minimal)) {
    cli::cli_abort(
      c(
        "{.arg minimal} must be logical.",
        "x" = "{.var {rlang::expr_text(substitute(minimal))}} is {.obj_type_friendly {minimal}}"
      )
    )
  }

  # check if top is numeric
  if (!is.numeric(top)) {
    cli::cli_abort(
      c(
        "{.arg top} must be an integer or a vector of integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} is {.cls {class(top)}}"
      )
    )
  }

  # check if top is an integer or a vector of integers
  if (!any(top == as.integer(top))) {
    cli::cli_abort(
      c(
        "{.arg top} must be an integer or a vector of integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} is not an integer."
      )
    )
  }

  # check if top contains only positive values
  if (any(top < 1)) {
    cli::cli_abort(
      c(
        "{.arg top} must be a positive integer or a vector of positive integers.",
        "x" = "{.var {rlang::expr_text(substitute(top))}} contains non-positive integers."
      )
    )
  }

  if (dplyr::is.grouped_df(df)) {
    cli::cli_inform(
      c(
        "Data must not be grouped.",
        "i" = "It has been ungrouped."
      )
    )
    df <- dplyr::ungroup(df)
  }

  # check if normalise is logical
  if (!is.logical(normalise)) {
    cli::cli_abort(
      c(
        "{.arg normalise} must be logical.",
        "x" = "{.var {rlang::expr_text(substitute(normalise))}} is {.obj_type_friendly {normalise}}"
      )
    )
  }

  # check if self_ranking contains only positive values
  if (!rlang::quo_is_null(self_ranking_quo) && any(df[[rlang::as_name(self_ranking)]] < 0)) {
    cli::cli_abort(
      c(
        "{.var self_ranking} must contain only positive values.",
        "i" = "Check if the best answers have rank 1."
      )
    )
  }

  # Remove NA scores if present
  if (any(is.na(df[[rlang::as_name(score_column)]]))) {
    cli::cli_inform(
      c(
        "Removed NA values from {.var {rlang::expr_text(substitute(score_column))}}"
      )
    )
    df <- dplyr::filter(df, !is.na(!!score_column))
  }

  if (normalise) {
    df <- df |>
      dplyr::mutate(df,
        .z_score = as.vector(scale({{ score_column }}))
      )
  }

  if (!rlang::quo_is_null(item_column_quo)) {
    df <- df |>
      dplyr::group_by({{ id_column }}, {{ item_column }})
  } else {
    df <- df |>
      dplyr::group_by({{ id_column }})
  }

  df <- df |>
    dplyr::arrange({{ id_column }}, {{ item_column }}, {{ score_column }})

  # if (normalise) {
  #   base_cols <- df |>
  #     dplyr::mutate(
  #       .ordering = rank(
  #         -.data$.z_score, # minus for descending order
  #         ties.method = ties_method
  #       ) - 1 # -1 to start with 0
  #     )
  # } else {
  base_cols <- df |>
    dplyr::mutate(
      .ordering = rank(
        -{{ score_column }}, # minus for descending order
        ties.method = ties_method
      ) - 1 # -1 to start with 0
    )
  # }



  if (!rlang::quo_is_null(self_ranking_quo)) {
    base_cols <- base_cols |>
      dplyr::mutate(
        .self_ranked = ({{ self_ranking }} - 1) |>
          abs(),
        .ordering = dplyr::case_when(
          .self_ranked == 0 ~ 0,
          .default = .data$.ordering + 1
        )
      ) |>
      dplyr::select(-".self_ranked")
  }

  if (any(top > max(base_cols[[".ordering"]]))) {
    if (all(base_cols[[".ordering"]] == 0)) {
      top <- 1
      cli::cli_warn(
        "No variance in scores."
      )
    } else {
      top <- top[top <= max(base_cols[[".ordering"]])]
    }
    if (length(top) == 0) {
      cli::cli_abort(
        "{.var top} must not be greater than biggest rank."
      )
    } else {
      cli::cli_warn(
        c(
          "{.var top} must not be greater than biggest rank.",
          "i" = "Deleted inadequate values."
        )
      )
    }
  }

  top <- as.list(top)

  df <- purrr::map(top, \(x) {
    base_cols |>
      dplyr::mutate(
        !!glue(".ordering_top{x}") := dplyr::case_when(
          .ordering < x ~ 0,
          .default = .data$.ordering
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::starts_with(".ordering_top"))
  })

  df <- dplyr::bind_cols(base_cols, df) |>
    dplyr::ungroup()


  if (minimal && normalise) {
    df <- df |>
      dplyr::select({{ id_column }}, {{ item_column }}, ".z_score", dplyr::starts_with(".ordering"))
  } else if (minimal) {
    df <- df |>
      dplyr::select({{ id_column }}, {{ item_column }}, {{ score_column }}, dplyr::starts_with(".ordering"))
  }

  return(df)
}
