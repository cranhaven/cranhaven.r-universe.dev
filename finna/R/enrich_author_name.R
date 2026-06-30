#' @title Enrich Author Name from 'Finna' API and Save Results
#'
#' @description
#' This function reads a CSV file from a URL containing Melinda IDs and author names.
#' If the author name is missing (NA), it searches the 'Finna' API for the corresponding Melinda ID
#' to retrieve and update the author name. The updated data is saved in a CSV file.
#'
#' @param url A character string specifying the URL of the CSV file with Melinda IDs and author names.
#' @param output_file A character string specifying the output CSV file name.
#' @return A tibble with updated author names. The file is saved to a temporary directory using \code{tempdir()}.
#'
#' @importFrom readr read_csv write_csv cols col_character
#' @importFrom dplyr mutate if_else
#' @importFrom purrr map_chr
#' @export
#' @examples
#' \dontrun{
#' enrich_author_name(url = "https://example/na_author_rows.csv",
#'                    output_file = "updated_na_author_rows.csv")
#' }
enrich_author_name <- function(url, output_file = "updated_na_author_rows.csv") {

  # Internal function to get author name by Melinda ID with retry for rate limiting
  get_author_name_from_finna <- function(melinda_id) {
    attempt <- 1
    max_attempts <- 5
    wait_time <- 60  # Wait time in seconds after rate limit is hit

    while (attempt <= max_attempts) {
      results <- tryCatch({
        search_finna(
          query = melinda_id,
          type = "AllFields",
          fields = c("authors"),
          limit = 1
        )
      }, error = function(e) NULL)

      if (!is.null(results) && nrow(results) > 0) {
        authors <- results$Author[1]
        return(authors)
      } else {
        return(NA)
      }
    }
    return(NA)
  }

  # Step 1: Read CSV with Melinda IDs and check for 'NA' in author_name column
  data <- readr::read_csv(url, col_types = readr::cols(
    melinda_id = readr::col_character(),
    author_name = readr::col_character()
  ))

  # Step 2: Update `author_name` if it is 'NA' by fetching from 'Finna'
  data <- data %>%
    dplyr::mutate(
      updated_author_name = dplyr::if_else(
        is.na(author_name) | author_name == "NA",
        purrr::map_chr(melinda_id, ~ {
          Sys.sleep(1)  # Delay between requests to avoid rate limit
          author <- get_author_name_from_finna(.x)
          message("Melinda ID: ", .x, " - Retrieved Author: ", author)
          author
        }),
        author_name
      )
    )

  # Step 3: Save the updated data as CSV in a temporary directory
  output_csv_path <- file.path(tempdir(), output_file)
  readr::write_csv(data, output_csv_path)
  message("CSV file with updated author names saved to ", output_csv_path)

  return(data)  # Return the updated tibble
}
