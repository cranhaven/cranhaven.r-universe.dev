#' summary_interact
#'
#' @param model    A model from \code{\link[rms]{lrm}}
#' @param ref      A continuous variable for which we are interested in the
#'   estimation of the OR for the various level of interaction with a discrete
#'   variable interacting with it
#' @param discrete The discrete interacting variable
#' @param ref_min  Denominator continuous level for the Odds Ratio
#'   (i.e., the reference level), if NULL (the default)
#' @param ref_max  Numerator continuous level for the Odds Ratio
#'   (i.e., the target level)
#' @param level A character vector of levels to show. Default (NULL) means to
#' show all the possible levels for the discrete variable
#' @param digits number of significant digits to print. Default is 3
#'
#' Note: the \code{\link[rms]{datadist}} has to be defined for the data used in
#'  the model
#' @param ... for possible future development
#' @param p do you want also the P-value (default = FALSE)
#'
#' @return A data frame
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#'   library(rms)
#'   options(datadist = "dd")
#'
#'   data("transplant", package = "survival")
#'
#'   transplant <- transplant[transplant[["event"]] != "censored", ] %>%
#'    droplevels()
#'   dd <- datadist(transplant)
#'
#'   lrm_mod <- lrm(event ~ rcs(age, 3) * (sex + abo) + rcs(year, 3),
#'     data = transplant
#'   )
#'
#'   lrm_mod
#'   summary(lrm_mod)
#'   summary_interact(lrm_mod, age, sex)
#'   summary_interact(lrm_mod, age, sex, ref_min = 60, ref_max = 80)
#'   summary_interact(lrm_mod, age, sex,
#'     ref_min = 60, ref_max = 80, digits = 5L
#'   )
#'
#'   summary_interact(lrm_mod, age, abo)
#'   summary_interact(lrm_mod, age, abo, level = c("A", "AB"))
#'   summary_interact(lrm_mod, age, abo, level = c("A", "AB"), p = TRUE)
#' }
summary_interact <- function(model, ref, discrete,
                             ref_min = NULL, ref_max = NULL,
                             level = NULL,
                             ...,
                             digits = 3L,
                             p = FALSE) {
  if (!inherits(model, "lrm")) {
    ui_stop("model has to inherits to lrm class")
  }
  if (is.null(getOption("datadist"))) ui_stop("datadist non defined")

  discrete <- rlang::enquo(discrete)
  discrete_name <- rlang::quo_name(discrete)

  ref <- rlang::enquo(ref)
  ref_name <- rlang::quo_name(ref)

  dd <- getOption("datadist") %>%
    as.name() %>%
    eval()

  if (!ref_name %in% names(dd[["limits"]])) {
    ui_stop("ref isn't in datadist")
  }
  if (!discrete_name %in% names(dd[["limits"]])) {
    ui_stop("discrete isn't in datadist")
  }


  if (is.null(ref_min)) {
    ref_min <- dd[["limits"]][[ref_name]][[1L]]
  }
  if (is.null(ref_max)) {
    ref_max <- dd[["limits"]][[ref_name]][[3L]]
  }
  if (is.null(level)) {
    level <- dd[["values"]][[discrete_name]]
  }

  suppressMessages({
    res <- purrr::map_df(.x = level, ~ {
      interact <- .x
      eval(parse(text = paste0(
        "summary(model, ",
          discrete_name, " = interact, ",
          ref_name, " = c(ref_min, ref_max)",
        ")"
      ))) %>%
        tibble::as_tibble(
          rownames = ".rownames",
          .name_repair = "universal"
        ) %>%
        dplyr::mutate(.rownames = dplyr::lag(.data[[".rownames"]])) %>%
        dplyr::filter(Type == 2L) %>%
        dplyr::select(-"Type", -"S.E.") %>%
        dplyr::filter(.rownames == rlang::quo_name(ref)) %>%
        dplyr::mutate(
          Low = ifelse(is.na(Diff.), NA, Low),
          High = ifelse(is.na(Diff.), NA, High),
          Diff. = ifelse(!is.na(Diff.), Diff.,
            stringr::str_extract(dplyr::all_of(".rownames"), " - .*$") %>%
              stringr::str_replace(" - ", "")
          ),
          Effect = as.numeric(Effect),
          Lower.0.95 = as.numeric(Lower.0.95),
          Upper.0.95 = as.numeric(Upper.0.95),
          .rownames = stringr::str_replace(.data[[".rownames"]], " -+.*$", "")
        ) %>%
        dplyr::mutate(
          .rownames = paste0(.data[[".rownames"]], " - ", interact)
        ) %>%
        dplyr::rename(
          `&nbsp;` = dplyr::all_of(".rownames"),
          `Odds Ratio` = dplyr::all_of("Effect"),
          `Lower 95% CI` = dplyr::all_of("Lower.0.95"),
          `Upper 95% CI` = dplyr::all_of("Upper.0.95")
        )
    })
  })
  if (p) {
    res[["P-value"]] <- purrr::pmap_dbl(
      list(
        or = res[["Odds Ratio"]],
        low = res[["Lower 95% CI"]],
        high = res[["Upper 95% CI"]]
      ),
      function(or, low, high) {
        ci2p(or, low, high, log_transform = TRUE)
      }
    )
  }
  res %>%
    dplyr::mutate_if(is.double, round, digits = digits)
}
