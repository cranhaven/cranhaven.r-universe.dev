#' Retrieve Data from UniProt API
#'
#' This function sends a GET request to the UniProt REST API and retrieves
#' data based on the provided UniProt accession number.
#'
#' @param accession A character string representing the UniProt accession number.
#' @return A list containing the retrieved data in JSON format, or NULL if the request fails.
#' @import httr jsonlite cli
#' @examples
#' # Example usage
#' result <- retrieve_uniprot_data("O88737")
#' print(result)
#' @export
retrieve_uniprot_data <- function(accession) {
  # Base URL for UniProt API
  base_url <- paste0("https://rest.uniprot.org/uniprotkb/", accession, ".json")

  # Send a GET request to the API
  cli::cli_alert_info("Sending request to UniProt for accession: {accession}")
  response <- httr::GET(base_url)

  # Check if the request was successful
  if (response$status_code == 200) {
    cli::cli_alert_success("Successfully retrieved data for {accession}")
    # Parse the response to JSON
    content_data <- httr::content(response, as = "text", encoding = "UTF-8")
    json_data <- jsonlite::fromJSON(content_data)
    return(json_data)
  } else {
    cli::cli_alert_danger("Failed to retrieve data for {accession}. HTTP status: {response$status_code}")
    return(NULL)
  }
}
