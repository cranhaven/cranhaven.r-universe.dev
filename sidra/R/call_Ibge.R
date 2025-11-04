#' @title Gracefully handle API calls with internet checking
#' @description A wrapper for API calls that catches connection errors and
#'   issues a warning instead of stopping execution.
#' @param expr The expression to evaluate (e.g., an httr::GET() call).
#' @param api_url The base URL of the API being called, for use in the warning message.
#' @return The result of the expression, or NULL invisibly if a connection
#'   error is caught.
#' @noRd
call_ibge <- \(expr, api_url = "https://servicodados.ibge.gov.br") {
  tryCatch(
    expr,
    error = \(e) {
      # Check if the error message indicates a connection/timeout issue
      if (grepl("Timeout was reached|Could not connect|Connection timed out|Could not resolve host|could not be reached", e$message)) {
        warning(
          "The API at ", api_url, " could not be reached.\n",
          "Please check your internet connection and access to IBGE ibge.gov.br or try again later.",
          call. = FALSE # Suppresses the 'In addition: ...' part of the warning
        )
        return(invisible(NULL))
      } else {
        # For any other type of error, show the original error and return NULL also
        warning(e$message)
        return(invisible(NULL))
      }
    }
  )
}
