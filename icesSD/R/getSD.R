#' Get Stock List Data
#'
#' Get stock list data such as stock code, expert group, assessment type, etc.
#'
#' @param stock the stock code, e.g. cod.27.47d20.
#' @param year the active year of the stock list, e.g. 2016, or NULL to get the
#' most recent year available.
#'
#' @return A data frame.
#'
#' \code{\link{icesSD-package}} gives an overview of the package.
#'
#' @examples
#' \donttest{
#' sddata <- getSD()
#' cod <- getSD(stock = "cod.27.47d20")
#' cod22 <- getSD(stock = "cod.27.47d20", year = 2022)
#' y2022 <- getSD(year = 2022)
#'}
#' @export
getSD <- function(stock = NULL, year = NULL) {

  args <- list()
  if (!is.null(stock) && is.null(year)) {
    args <- list(`$filter` = paste0("StockKeyLabel eq '", stock, "'"))
  } else
  if (is.null(stock) && !is.null(year)) {
    args <- list(`$filter` = paste0("ActiveYear eq ", year))
  } else
  if (!is.null(stock) && !is.null(year)) {
    args <-
      list(`$filter` = paste0("StockKeyLabel eq '", stock, "' and ", "ActiveYear eq ", year))
  }

  url <-
    do.call(
      sd_api,
      c(list(service = "odata4/StockListDWs4"), args)
    )

  out <- sd_get(url)

  out$value
}
