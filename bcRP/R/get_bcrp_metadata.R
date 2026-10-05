#' Access metadata for all available datasets provided by the Peruvian Central Bank
#' @export
#' @examples
#' # No arguments required, simply call the function.
#' get_bcrp_metadata()
#'
#' @return A data frame with one row per code available for request.
#' @author Julio Collazos
get_bcrp_metadata <- function() {
  url <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/metadata"
  tryCatch(
    expr = {
      temp <- tempfile(fileext = ".csv")
      httr2::req_perform(
        httr2::request(url),
        path = temp
      )
      suppressMessages(
        readr::read_delim(
          temp,
          locale = readr::locale(encoding = "latin1"),
          delim = ";",
          show_col_types = FALSE
        )
      )
    },
    error = function(cnd) {
      stop(
        "Server response: ",
        conditionMessage(cnd),
        "\nConsider visiting the following url: https://estadisticas.bcrp.gob.pe/estadisticas/series/ayuda/metadatos to check whether the page is currently down.",
        call. = FALSE
      )
    },
    finally = unlink(temp)
  )
}
