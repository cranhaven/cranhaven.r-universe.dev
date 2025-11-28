#' tidy_summary
#'
#' Converts a \code{summary()} object produced by \code{Hmisc} or
#' by \code{rms} packages to a tidy data frame ready to be
#' `{pander}`ed (e.g. printed on a word document after
#' \code{knit}ting the source (with `{knitr}`).
#'
#' @note The output is supposed to be used as input to
#'       [pander][pander::pander], and contains few otherwise messy
#'       characters included for an optimal (pander) formatting.
#'
#' @param x an object used to select a method, output of some summary
#'          by \code{Hmisc}.
#' @param ... further arguments passed to or from other methods
#' @param digits number of significant digits to print. Default is 3
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
tidy_summary <- function(x, ..., digits = 3L) {
  UseMethod("tidy_summary", x)
}




#' @describeIn tidy_summary Tidies a summary reverse output from the
#'             \code{\link[Hmisc]{summary.formula}} called with
#'             \code{method = "reverse"}.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @note to see the options you can pass to \code{...} for a custom
#' print, see the print section in \code{\link[Hmisc]{summary.formula}}.
#'
#' @export
#' @examples
#' \donttest{
#'   library(Hmisc)
#'   my_summary <- summary(Species ~ ., data = iris, method = "reverse")
#'   tidy_summary(my_summary)
#' }
tidy_summary.summary.formula.reverse <- function(x, ..., digits = 3L) {

  invisible(utils::capture.output({
    printed <- print(x, ...)
  }))

  colnames(printed) <- printed[1L, ]
  printed <- dplyr::as_tibble(printed, rownames = "&nbsp;") %>%
    dplyr::mutate(
      `&nbsp;` =  stringr::str_replace_all(
        .data[["&nbsp;"]],
        " ",
        "&nbsp;"
      )
    )

  res <- printed[-1L, ]

  class(res) <- c("tidy_summary", class(res))
  res %>%
    dplyr::mutate_if(is.double, round, digits = digits)
}



#' @describeIn tidy_summary Convert the output of the
#'             \code{\link[rms]{summary.rms}} into a data frame,
#'             reporting only the Hazard Ratio with the .95 CI and the
#'             incremental step (for continuous variables) reference
#'             (for categorical variables) for which the Hazard is
#'             referred to (i.e. without \eqn{\beta}s, Low, High, S.E.
#'             and Type).
#'
#' @param digits number of significant digits to use (default 3L).
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#'   library(rms)
#'   options(datadist = "dd")
#'   n <- 1000L
#'   set.seed(731L)
#'   age <- 50L + 12L * rnorm(n)
#'   sex <- factor(sample(c("Male", "Female"), n,
#'     rep = TRUE,
#'     prob = c(.6, .4)
#'   ))
#'   cens <- 15L * runif(n)
#'   h <- .02 * exp(.04 * (age - 50L) + .8 * (sex == "Female"))
#'   dt <- -log(runif(n)) / h
#'   e <- ifelse(dt <= cens, 1L, 0L)
#'   dt <- pmin(dt, cens)
#'
#'   dd <- datadist(age, sex)
#'
#'   S <- survival::Surv(dt, e)
#'   f <- rms::cph(S ~ age + sex)
#'
#'
#'   my_summary <- summary(f)
#'   tidy_summary(my_summary)
#' }
tidy_summary.summary.rms <- function(x, ..., digits = 3L) {
  res <- as.data.frame(x) %>%
    tibble::as_tibble(rownames = ".rownames") %>%
    dplyr::mutate(.rownames = dplyr::lag(.data[[".rownames"]])) %>%
    dplyr::filter(.data[["Type"]] == 2L)

  res <- res[!names(res) %in% c("Low", "High", "S.E.", "Type")] %>%
    dplyr::mutate(
      Diff. = ifelse(!is.na(.data[["Diff."]]), .data[["Diff."]],
        stringr::str_extract(.data[[".rownames"]], "\\.\\.\\..*$") %>%
          stringr::str_replace("\\.\\.\\.", "") %>%
          stringr::str_replace("\\.", ":")
      ),
      .rownames = stringr::str_replace(.data[[".rownames"]], "\\.\\.\\..*$", "")
    ) %>%
    dplyr::rename(
      `&nbsp;` = dplyr::all_of(".rownames"),
      `HR` = dplyr::all_of("Effect"),
      `Lower 95% CI` = dplyr::all_of("Lower 0.95"),
      `Upper 95% CI` = dplyr::all_of("Upper 0.95")
    )
}
