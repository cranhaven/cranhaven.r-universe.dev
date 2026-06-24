#' Search Goodreads
#'
#' This function searches books on Goodreads.
#'
#' @param search_term A search term string.
#' @param search_in Where to search (e.g., "title", "author").
#' @param num_books Number of books to return.
#' @param sort_by How to sort the results (e.g., "ratings", "published_year").
#' @return A data frame of search results.
#' @importFrom rvest read_html html_nodes html_node html_text html_attr
#' @importFrom dplyr arrange desc filter bind_rows
#' @importFrom stringr str_extract str_remove str_replace_all str_detect regex
#' @importFrom utils URLencode head
#' @export
#' @examples
#' search_goodreads("parenting", search_in = "title", num_books = 2)

search_goodreads <- function(search_term, search_in = c("title", "author"),
                             num_books = 10, sort_by = "ratings") {
  # Base URL for Goodreads search
  base_url <- "https://www.goodreads.com/search?q="

  # URL-encode the search term
  search_term_encoded <- utils::URLencode(search_term)

  # Create the search URL based on user input
  search_url <- if (search_in == "title") {
    paste0(base_url, search_term_encoded, "&search_type=books&search%5Bfield%5D=title")
  } else {
    paste0(base_url, search_term_encoded, "&search_type=books&search%5Bfield%5D=author")
  }

  # Function to scrape a single page of Goodreads search results
  scrape_page <- function(url) {
    page <- rvest::read_html(url)
    on.exit(close(url(url)))  # Close the connection when the function exits

    # Extracting book details
    book_nodes <- rvest::html_nodes(page, ".tableList tr")

    titles <- rvest::html_text(rvest::html_node(book_nodes, ".bookTitle span"))
    authors <- rvest::html_text(rvest::html_node(book_nodes, ".authorName span"))
    ids <- stringr::str_extract(rvest::html_attr(rvest::html_node(book_nodes, ".bookTitle"), "href"), "\\d+")
    urls <- paste0("https://www.goodreads.com", rvest::html_attr(rvest::html_node(book_nodes, ".bookTitle"), "href"))

    if (sort_by == "ratings") {
      ratings <- rvest::html_text(rvest::html_node(book_nodes, ".minirating"))
      ratings <- stringr::str_extract(ratings, "([0-9,]+) rating")
      ratings <- stringr::str_remove(ratings, " rating")
      ratings <- as.numeric(stringr::str_replace_all(ratings, ",", ""))
    } else if (sort_by == "published_year") {
      published_years <- rvest::html_text(rvest::html_node(book_nodes, ".greyText.smallText"))
      published_years <- as.numeric(stringr::str_extract(published_years, "\\d{4}"))
    }

    # Combine into a data frame
    if (sort_by == "ratings") {
      data.frame(
        title = titles,
        author = authors,
        book_id = ids,
        url = urls,
        ratings = ratings,
        stringsAsFactors = FALSE
      )
    } else if (sort_by == "published_year") {
      data.frame(
        title = titles,
        author = authors,
        book_id = ids,
        url = urls,
        published_year = published_years,
        stringsAsFactors = FALSE
      )
    }
  }

  results <- data.frame()
  page_num <- 1
  while (nrow(results) < num_books) {
    if (page_num == 1) {
      current_url <- search_url
    } else {
      current_url <- paste0(search_url, "&page=", page_num)
    }

    # Scrape the current page
    new_results <- scrape_page(current_url)

    # Filter results to include only exact matches based on the search criteria
    if (search_in == "title") {
      new_results <- dplyr::filter(new_results, stringr::str_detect(new_results$title, stringr::regex(paste0("\\b", search_term, "\\b"), ignore_case = TRUE)))
    } else if (search_in == "author") {
      new_results <- dplyr::filter(new_results, stringr::str_detect(new_results$author, stringr::regex(paste0("\\b", search_term, "\\b"), ignore_case = TRUE)))
    }

    results <- dplyr::bind_rows(results, new_results)
    page_num <- page_num + 1
  }

  # Sort the results based on user choice
  if (sort_by == "ratings") {
    results <- dplyr::arrange(results, dplyr::desc(results$ratings))
  } else if (sort_by == "published_year") {
    results <- dplyr::arrange(results, dplyr::desc(results$published_year))
  }

  # Limit the results to the number of books specified by the user
  results <- utils::head(results, num_books)

  return(results)
}
