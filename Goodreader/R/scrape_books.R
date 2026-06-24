#' Scrape book details from Goodreads
#'
#' This function scrapes details of books using their IDs from Goodreads.
#'
#' @param book_ids_path Path to a text file containing book IDs.
#' @param use_parallel Logical indicating whether to scrape in parallel (default is FALSE).
#' @param num_cores Number of CPU cores to use for parallel scraping (default is 4).
#' @return A data frame containing scraped book details.
#' @importFrom httr GET
#' @importFrom rvest read_html html_node html_nodes html_text html_attr
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom parallel makeCluster clusterExport clusterEvalQ parLapply stopCluster
#' @importFrom rlang .data
#' @export
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#'
#' # Run the function (with a small delay to avoid overwhelming the server)
#' result <- scrape_books(temp_file, use_parallel = FALSE)
#' print(head(result))
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }

scrape_books <- function(book_ids_path, use_parallel = FALSE, num_cores = 4) {
  book_ids <- readLines(book_ids_path)

  scrape_book <- function(book_id) {
    url <- paste0('https://www.goodreads.com/book/show/', book_id)
    response <- httr::GET(url)
    soup <- rvest::read_html(response)
    Sys.sleep(2)

    get_genres <- function(soup) {
      genres_div <- soup %>% rvest::html_node("[data-testid='genresList']")
      if (!is.null(genres_div)) {
        genres <- genres_div %>%
          rvest::html_nodes("a.Button--tag-inline") %>%
          rvest::html_text(trim = TRUE)
        return(genres)
      } else {
        return(character(0))
      }
    }

    get_publication_info <- function(soup) {
      soup %>%
        rvest::html_nodes("div.FeaturedDetails") %>%
        rvest::html_node("p[data-testid='publicationInfo']") %>%
        rvest::html_text()
    }

    get_num_pages <- function(soup) {
      featured_details <- soup %>% rvest::html_nodes("div.FeaturedDetails")
      sapply(featured_details, function(detail) {
        format_info <- detail %>% rvest::html_node("p[data-testid='pagesFormat']")
        if (!is.null(format_info)) {
          format_text <- format_info %>% rvest::html_text()
          parts <- strsplit(format_text, ", ")[[1]]
          if (length(parts) == 2) {
            return(gsub("[^0-9]", "", parts[1]))
          } else if (length(parts) == 1 && grepl("[0-9]", parts[1])) {
            return(parts[1])
          }
        }
        return(NA)
      })
    }

    get_format_info <- function(soup) {
      soup %>%
        rvest::html_nodes("div.FeaturedDetails") %>%
        rvest::html_node("p[data-testid='pagesFormat']") %>%
        rvest::html_text()
    }

    get_rating_distribution <- function(soup) {
      rating_bars <- soup %>% rvest::html_nodes("div.RatingsHistogram__bar")
      sapply(rating_bars, function(bar) {
        rating <- bar %>%
          rvest::html_attr("aria-label") %>%
          stringr::str_extract("^[0-9]")
        num_ratings <- bar %>%
          rvest::html_node("div.RatingsHistogram__labelTotal") %>%
          rvest::html_text() %>%
          stringr::str_extract("^[0-9,]+") %>%
          gsub(",", "", x = .)
        stats::setNames(num_ratings, rating)
      })
    }

    book_details <- function(soup) {
      tryCatch({
        soup %>%
          rvest::html_node("div.DetailsLayoutRightParagraph") %>%
          rvest::html_text()
      }, error = function(e) {
        return(" ")
      })
    }

    contributor_info <- function(soup) {
      soup %>% rvest::html_node("a.ContributorLink")
    }

    list(
      book_id = book_id,
      book_title = soup %>%
        rvest::html_node("h1[data-testid='bookTitle']") %>%
        rvest::html_text() %>%
        gsub("\n", " ", x = .) %>%
        trimws(),
      book_details = book_details(soup),
      format = get_format_info(soup),
      publication_info = get_publication_info(soup),
      authorlink = contributor_info(soup) %>% rvest::html_attr("href"),
      author = contributor_info(soup) %>%
        rvest::html_node("span.ContributorLink__name") %>%
        rvest::html_text(trim = TRUE),
      num_pages = get_num_pages(soup),
      genres = paste(get_genres(soup), collapse = ", "),
      num_ratings = soup %>%
        rvest::html_node("span[data-testid='ratingsCount']") %>%
        rvest::html_text() %>%
        gsub("[^0-9]", "", x = .),
      num_reviews = soup %>%
        rvest::html_node("span[data-testid='reviewsCount']") %>%
        rvest::html_text() %>%
        gsub("[^0-9]", "", x = .),
      average_rating = soup %>%
        rvest::html_node("div.RatingStatistics__rating") %>%
        rvest::html_text(trim = TRUE),
      rating_distribution = toString(get_rating_distribution(soup))
    )
  }

  scrape_with_error_handling <- function(book_id) {
    tryCatch(
      scrape_book(book_id),
      error = function(e) {
        message(paste("Error scraping book ID:", book_id))
        message(e)
        return(NULL)
      }
    )
  }

  if (use_parallel) {
    cl <- parallel::makeCluster(num_cores)
    parallel::clusterExport(cl, c("scrape_book", "scrape_with_error_handling"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(httr)
      library(rvest)
      library(dplyr)
      library(magrittr)
      library(stringr)
      library(stats)
    })
    book_data <- parallel::parLapply(cl, book_ids, scrape_with_error_handling)
    parallel::stopCluster(cl)
  } else {
    book_data <- lapply(book_ids, scrape_with_error_handling)
  }

  book_data <- book_data[!sapply(book_data, is.null)]
  dplyr::bind_rows(book_data)
}
