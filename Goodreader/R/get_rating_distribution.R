#' Get Rating Distribution from Goodreads
#'
#' This function takes a file path containing Goodreads book IDs and retrieves
#' the rating distribution for each book.
#'
#' @param file_path A character string specifying the path to the file containing Goodreads book IDs.
#' @return A named list where each element contains the rating distribution for a book.
#' @export
#' @importFrom rvest read_html html_nodes html_attr html_node html_text
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the function
#' rating_distributions <- get_rating_distribution(temp_file)
#' print(rating_distributions)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
get_rating_distribution <- function(file_path) {
  read_book_ids <- function(file_path) {
    book_ids <- readLines(file_path)
    return(book_ids)
  }

  fetch_book_soup <- function(book_id) {
    url <- paste0("https://www.goodreads.com/book/show/", book_id)
    soup <- rvest::read_html(url)
    return(soup)
  }

  book_ids <- read_book_ids(file_path)
  results <- lapply(book_ids, function(book_id) {
    soup <- fetch_book_soup(book_id)
    rating_numbers <- list()
    rating_bars <- rvest::html_nodes(soup, "div.RatingsHistogram__bar")
    for (rating_bar in rating_bars) {
      rating <- rvest::html_attr(rating_bar, "aria-label") %>%
        stringr::str_extract("^[0-9]")
      num_ratings <- rvest::html_node(rating_bar, "div.RatingsHistogram__labelTotal") %>%
        rvest::html_text() %>%
        stringr::str_extract("^[0-9,]+") %>%
        gsub(",", "", x = .)
      rating_numbers[[rating]] <- num_ratings
    }
    return(rating_numbers)
  })
  names(results) <- book_ids
  return(results)
}
