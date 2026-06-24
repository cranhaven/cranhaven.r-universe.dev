#' Get Number of Pages from Goodreads
#'
#' This function takes a file path containing Goodreads book IDs and retrieves
#' the number of pages for each book.
#'
#' @param file_path A character string specifying the path to the file containing Goodreads book IDs.
#' @return A named list where each element contains the number of pages for a book.
#' @export
#' @importFrom rvest read_html html_nodes html_node html_text
#' @importFrom stringr str_split
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#' # Run the function
#' num_pages <- get_num_pages(temp_file)
#' print(num_pages)
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
get_num_pages <- function(file_path) {
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
    featured_details <- rvest::html_nodes(soup, "div.FeaturedDetails")
    number_of_pages_list <- vector("list", length(featured_details))
    for (i in seq_along(featured_details)) {
      format_info <- rvest::html_node(featured_details[i], "p[data-testid='pagesFormat']")
      if (!is.null(format_info)) {
        format_text <- rvest::html_text(format_info)
        parts <- stringr::str_split(format_text, ", ")[[1]]
        if (length(parts) == 2) {
          number_of_pages <- gsub("[^0-9]", "", parts[1])
          number_of_pages_list[[i]] <- number_of_pages
        } else if (length(parts) == 1 && grepl("[0-9]", parts[1])) {
          number_of_pages_list[[i]] <- parts[1]
        } else {
          number_of_pages_list[[i]] <- NA
        }
      } else {
        number_of_pages_list[[i]] <- NA
      }
    }
    return(unlist(number_of_pages_list))
  })
  names(results) <- book_ids
  return(results)
}
