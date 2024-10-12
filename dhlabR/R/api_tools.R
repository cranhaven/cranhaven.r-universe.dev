library(httr)

api_call_wrapper <- function(api_url, ..., method = "POST") {
  result <- tryCatch({
    if (method == "POST") {
      response <- POST(api_url, ...)
    } else if (method == "GET") {
      response <- GET(api_url, ...)
    } else {
      stop("Invalid HTTP method: ", method)
      return(NULL)
    }

    if (status_code(response) >= 200 && status_code(response) < 300) {
      return(response)
    } else {
      stop("API request failed with status code: ", status_code(response))
      return(NULL)
    }
  }, error = function(e) {
    warning("Failed to fetch data from API: ", e$message)
    return(NULL)
  })

  return(result)
}
