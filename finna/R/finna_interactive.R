#' @title Interactive Finna Search and Data Download
#'
#' @description
#' Provides an interactive interface to search, select, and download datasets from Finna API.
#'
#' @seealso [search_finna()], [fetch_finna()], [finna_cite()]
#' @return A dataframe containing the selected dataset or downloaded data.
#'
#' @importFrom utils menu
#' @importFrom dplyr select arrange desc
#' @importFrom utils URLdecode
#' @importFrom glue glue
#' @export
finna_interactive <- function() {
  collection_options <- c(
    "Finnan oma collections (All content) - ''",
    "Dublin Core collections (All content) - 'oai_dc'",
    "Mostly library catalogs - 'marc21'",
    "Archival material (old version) - 'oai_ead'",
    "Archival material (new version) - 'oai_ead3'",
    "Material of the National Audiovisual Institute - 'oai_forward'",
    "Museums - 'record_format:lido'",
    "Repositories, thesis, library material - 'oai_qdc'"
  )

  collection_ids <- c("", "oai_dc", "marc21", "oai_ead", "oai_ead3",
                      "oai_forward", "record_format:lido", "oai_qdc")

  collection_index <- menu(collection_options, title = "Select a Collection")
  if (collection_index == 0) {
    message("No collection selected. Exiting.")
    return(invisible())
  }

  selected_collection <- collection_ids[collection_index]
  message(glue("You selected: {collection_options[collection_index]}"))

  query <- selected_collection

  results <- tryCatch({
    fetch_finna(query = query, lng = selected_collection)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)
  })

  if (is.null(results) || nrow(results) == 0) {
    message(glue("No data found for the selected collection: {query}"))
    return(invisible())
  }

  print(glue("Total results found: {nrow(results)}"))
  formatted_results <- results %>%
    dplyr::select(.data$value, .data$translated, .data$count, .data$href) %>%
    dplyr::arrange(desc(.data$count))
  print(formatted_results)

  dataset_titles <- results$translated
  dataset_index <- menu(c(dataset_titles, "No"), title = "Select a dataset of interest:")
  if (dataset_index == length(dataset_titles) + 1) {
    message("No dataset selected. Exiting.")
    return(invisible())
  }

  selected_data <- results[dataset_index, ]
  confirm_choice <- menu(c("Yes", "No"), title = "Is this the correct dataset?")
  if (confirm_choice == 2) {
    message("Dataset not confirmed. Exiting.")
    return(invisible())
  }

  print_citation <- menu(c("Yes", "No"), title = "Print dataset citation?")
  if (print_citation == 1) {
    finna_cite(result = results, index = dataset_index)
  }

  download_selection <- menu(c("Yes", "No"), title = "Download the dataset?")
  if (download_selection == 1) {
    dataset_href <- selected_data$href
    params <- parse_url_params(dataset_href)
    finna_data <- tryCatch({
      search_finna(
        query = params$lookfor,
        type = params$type,
        filters = params$filter[]
      )
    }, error = function(e) {
      message("Error downloading data: ", e$message)
      return(NULL)
    })

    if (!is.null(finna_data) && nrow(finna_data) > 0) {
      message("Data downloaded successfully.")
      return(finna_data)
    } else {
      message("No data found for the given parameters.")
      return(invisible())
    }
  }

  message("Interactive session completed.")
  return(invisible())
}

# Helper to parse URL parameters
parse_url_params <- function(url) {
  url_params <- sub(".*\\?", "", url)
  param_list <- strsplit(url_params, "&")[[1]]
  params <- list()
  for (param in param_list) {
    key_value <- strsplit(param, "=")[[1]]
    params[[key_value[1]]] <- URLdecode(key_value[2])
  }
  return(params)
}

