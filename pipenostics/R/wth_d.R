#' @title
#'  Derive the wall thickness depending on the outside diameter of pipe
#'
#' @family utils
#'
#' @description
#' Use \href{https://docs.cntd.ru/document/1200174717}{GOST 30732}
#' specifications to derive the value of the pipe wall thickness if only its
#' diameter is known for the pipe.
#'
#' @details
#' Utility should be used only in cases where the actual value
#' of the pipe wall thickness cannot be determined by any
#' other means. In many cases internal diameter may be used instead of outside
#' one without significant loss in precision. The wall thickness value is
#' derived only for the diameters mentioned in
#' \href{https://docs.cntd.ru/document/902148459}{Minenergo Order 325}.
#'
#' Unfortunately, the inverse function cannot be constructed in any reliable way
#' due to significant ambiguity.
#'
#' @param x
#'  outside diameter of pipe, [\emph{mm}]. Type: \code{\link{assert_double}}.
#'
#' @references
#'   \href{https://docs.cntd.ru/document/1200174717}{GOST 30732}.
#'   \emph{Steel pipes and shaped products with foamed polyurethane thermal
#'   insulation in protective sheath. Specifications.}
#'
#' @examples
#'  library(pipenostics)
#'
#'  # Guess pipe widths for some frequently met diameters
#'  wth_d(as.double(c(57, 76, 89)))
#'
#'  # [1]  3  7 11  # [mm]
#'
#' @export
wth_d <- function(x) {
  GOST <- data.frame(
    d = c(
      25,  32,    38,  45,  57,  76,  89,
      108,  114,  133, 159, 219, 273,
      325,  377,  426, 530, 630, 720, 820, 920,
      1020, 1220, 1420
    ),  # pipe diameter, [mm]
    wth = c(
      2.5, 3, 3, 3, 3, 3, 4, 4, 4, 4,
      4.5, 6, 7, 7, 7, 7, 7, 8, 8, 9,
      10  , 11, 11, 12
    )   # pipe thickness, [mm]
  )
  n <- nrow(GOST)

  checkmate::assert_double(
    x,
    lower       = min(GOST[["d"]]),
    upper       = max(GOST[["d"]]),
    any.missing = FALSE,
    min.len = 1
  )
  get_index <- stats::approxfun(
    x      = GOST[["d"]],
    y      = seq.int(n),
    yleft  = 1,
    yright = n,
    ties   = "ordered"
  )
  GOST[round(get_index(x)), "wth"]
}
