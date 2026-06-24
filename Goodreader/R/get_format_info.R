#' Get Format Information from Goodreads
#'
#' This function takes a file path containing Goodreads book IDs and retrieves
#' the format information for each book.
#'
#' @param file_path A character string specifying the path to the file containing Goodreads book IDs.
#' @return A named list where each element contains the format information for a book.
#' @export
#' @importFrom rvest read_html html_nodes html_node html_text
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the function
#' format_info <- get_format_info(temp_file)
#' print(format_info)
#'
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
get_format_info <- function(file_path) {
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
    a <- rvest::html_nodes(soup, "div.FeaturedDetails")
    format_info_list <- rvest::html_node(a, "p[data-testid='pagesFormat']") %>%
      rvest::html_text()
    return(format_info_list)
  })
  names(results) <- book_ids
  return(results)
}
