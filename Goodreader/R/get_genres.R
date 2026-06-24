#' Get Genres for Books from Goodreads
#'
#' This function reads book IDs from a file, fetches the corresponding Goodreads pages,
#' and extracts the genres for each book.
#'
#' @param file_path A character string specifying the path to the file containing book IDs.
#' @return A named list where each element corresponds to a book ID and contains
#'         a character vector of genres for that book.
#' @export
#' @importFrom rvest read_html html_node html_nodes html_text
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the function
#' genres <- get_genres(temp_file)
#'
#' # Display the results
#' print(genres)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }

get_genres <- function(file_path) {
  # Read book IDs from the file
  read_book_ids <- function(file_path) {
    book_ids <- readLines(file_path)
    return(book_ids)
  }

  # Fetch HTML content for a given book ID
  fetch_book_soup <- function(book_id) {
    url <- paste0("https://www.goodreads.com/book/show/", book_id)
    soup <- rvest::read_html(url)
    return(soup)
  }

  # Extract genres for each book ID
  results <- lapply(read_book_ids(file_path), function(book_id) {
    soup <- fetch_book_soup(book_id)
    genres_div <- soup %>% rvest::html_node("[data-testid='genresList']")
    if (!is.null(genres_div)) {
      genres <- genres_div %>%
        rvest::html_nodes("a.Button--tag-inline") %>%
        rvest::html_text(trim = TRUE)
      return(genres)
    } else {
      return(character(0))
    }
  })

  # Name the results list with corresponding book IDs
  names(results) <- read_book_ids(file_path)
  return(results)
}
