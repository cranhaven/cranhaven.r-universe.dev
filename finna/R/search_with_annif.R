#' Search Finna and Enrich Records with Top Annif Subject Suggestion
#'
#' This function searches the Finna API for records matching a query and enriches
#' each record with the top subject suggestion from the Annif API.
#'
#' @param query A character string representing the search term for Finna.
#' @param finna_limit An integer specifying the maximum number of Finna records to retrieve. Default is 10.
#' @param annif_project_id The project identifier for Annif (e.g., "yso-en"). Default is "yso-en".
#' @param annif_limit An optional parameter to specify the maximum number of results to return from Annif. Default is 10.
#' @param annif_threshold An optional parameter to specify the minimum score threshold for Annif results. Default is 0.
#' @param annif_language An optional parameter to specify the language of subject labels from Annif. Default is "en".
#' @return A tibble of Finna records, each enriched with the top Annif subject suggestion.
#'
#' @importFrom dplyr mutate rowwise ungroup
#' @importFrom httr POST status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' enriched_records <- search_finna_with_annif("Sibelius", finna_limit = 5)
#' }
#' @export
search_finna_with_annif <- function(query, finna_limit = 10, annif_project_id = "yso-fi", annif_limit = 10, annif_threshold = 0, annif_language = "fi") {

  # Define the suggest_subjects function
  suggest_subjects <- function(project_id, text, limit = 10, threshold = 0, language = "en") {
    base_url <- paste0("https://api.annif.org/v1/projects/", project_id, "/suggest")
    body <- list(
      text = text,
      limit = limit,
      threshold = threshold,
      language = language
    )
    response <- httr::POST(base_url, body = body, encode = "form", httr::accept("application/json"))
    if (httr::status_code(response) != 200) {
      stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
    }
    json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    suggestions_df <- tibble::as_tibble(json_data$results) %>%
      dplyr::select(label, uri, score)
    return(suggestions_df)
  }

  # Search for records in Finna
  finna_records <- finna::search_finna(query, limit = finna_limit)

  # Check if 'Title' and 'Subjects' columns exist
  if (!all(c("Title", "Subjects") %in% colnames(finna_records))) {
    stop("The Finna records do not contain the required 'Title' and 'Subjects' columns.")
  }

  # Enrich each record with the top Annif subject suggestion
  enriched_records <- finna_records %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      annif_suggestions = list(
        suggest_subjects(
          project_id = annif_project_id,
          text = paste(
            ifelse(is.character(.data$Title), .data$Title, ""),
            ifelse(is.character(.data$Subjects), .data$Subjects, ""),
            sep = " "
          ),
          limit = annif_limit,
          threshold = annif_threshold,
          language = annif_language
        )
      ),
      top_annif_subject = annif_suggestions %>%
        dplyr::slice_max(order_by = score, n = 1, with_ties = FALSE) %>%
        dplyr::select(label, uri, score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-annif_suggestions) %>%
    tidyr::unnest(cols = c(top_annif_subject))

  return(enriched_records)
}
