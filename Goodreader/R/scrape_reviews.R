#' Scrape book reviews from Goodreads
#'
#' This function scrapes book reviews from Goodreads based on provided book IDs.
#'
#' @param book_ids_path A character string specifying the path to a file containing book IDs.
#' @param num_reviews An integer specifying the number of reviews to scrape per book. Default is 30.
#' @param use_parallel A logical value indicating whether to use parallel processing. Default is FALSE.
#' @param num_cores An integer specifying the number of cores to use for parallel processing. Default is 4.
#'
#' @return A data frame containing scraped review information.
#'
#' @import rvest dplyr stringr purrr parallel
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' # Create a temporary file with sample book IDs
#' temp_file <- tempfile(fileext = ".txt")
#' writeLines(c("1420", "2767052", "10210"), temp_file)
#' # Run the function (with a small number of reviews to keep the example quick)
#' reviews <- scrape_reviews(temp_file, num_reviews = 5, use_parallel = FALSE)
#' print(head(reviews))
#' # Clean up: remove the temporary file
#' file.remove(temp_file)
#' }
scrape_reviews <- function(book_ids_path, num_reviews = 30, use_parallel = FALSE, num_cores = 4) {
  start_time <- Sys.time()
  script_name <- "scrape_goodreads_reviews"

  # Utility functions
  get_user_id <- function(article) {
    avatar <- article %>% rvest::html_node('.ReviewerProfile__avatar a')
    user_id_link <- rvest::html_attr(avatar, 'href')
    stringr::str_extract(user_id_link, "\\d+")
  }

  get_rating_and_date_user <- function(article) {
    rating_date_user <- article %>% rvest::html_node('.ReviewCard__row')
    date_element <- rating_date_user %>% rvest::html_node('.Text__body3 a')
    review_date <- rvest::html_text(date_element, trim = TRUE)

    rating_element <- rating_date_user %>% rvest::html_node('.RatingStars__small')
    aria_label <- rvest::html_attr(rating_element, 'aria-label')
    rating <- ifelse(!is.null(aria_label), stringr::str_extract(aria_label, "\\d+"), NA)

    list(review_date = review_date, rating = rating)
  }

  get_reviewers_info <- function(review_articles, book_id) {
    purrr::map(review_articles, function(article) {
      review_content <- article %>% rvest::html_node('.ReviewText__content') %>% rvest::html_text(trim = TRUE)

      rating_date <- get_rating_and_date_user(article)
      list(
        book_id = book_id,
        reviewer_id = tryCatch(get_user_id(article), error = function(e) NA),
        reviewer_name = tryCatch(article %>% rvest::html_node('.ReviewerProfile__name') %>% rvest::html_text(trim = TRUE), error = function(e) NA),
        review_content = review_content,
        reviewer_followers = tryCatch(article %>% rvest::html_nodes('.ReviewerProfile__meta span') %>% `[`(2) %>% rvest::html_text(trim = TRUE) %>% stringr::str_extract("\\d+") %>% as.numeric(), error = function(e) NA),
        reviewer_total_reviews = tryCatch(article %>% rvest::html_nodes('.ReviewerProfile__meta span') %>% `[`(1) %>% rvest::html_text(trim = TRUE) %>% stringr::str_extract("\\d+") %>% as.numeric(), error = function(e) NA),
        review_date = rating_date$review_date,
        review_rating = as.numeric(rating_date$rating)
      )
    })
  }

  scrape_reviews_for_book <- function(book_id, num_reviews) {
    result_list <- vector("list", num_reviews)
    tryCatch({
      url <- paste0("https://www.goodreads.com/book/show/", book_id)
      session <- rvest::session(url)
      Sys.sleep(1)

      reviews_collected <- 0
      more_reviews_available <- TRUE
      page_number <- 1

      while (more_reviews_available && reviews_collected < num_reviews) {
        page <- session$response %>% rvest::read_html()
        review_articles <- page %>% rvest::html_nodes('article.ReviewCard')
        results <- get_reviewers_info(review_articles, book_id)

        if (length(results) > 0) {
          new_reviews <- min(length(results), num_reviews - reviews_collected)
          result_list[(reviews_collected + 1):(reviews_collected + new_reviews)] <- results[1:new_reviews]
          reviews_collected <- reviews_collected + new_reviews
        }

        if (reviews_collected >= num_reviews) {
          break
        }

        more_button <- page %>% rvest::html_node('button.Button.Button--secondary[data-testid="pagination-button-next"]')
        if (!is.na(more_button)) {
          onclick_attr <- rvest::html_attr(more_button, "onclick")
          if (!is.null(onclick_attr)) {
            more_url <- stringr::str_extract(onclick_attr, "https://[^']+")
            if (!is.null(more_url)) {
              session <- rvest::session_jump_to(session, more_url)
              Sys.sleep(1)
              page_number <- page_number + 1
            } else {
              more_reviews_available <- FALSE
            }
          } else {
            more_reviews_available <- FALSE
          }
        } else {
          more_reviews_available <- FALSE
        }
      }

    }, error = function(e) {
      message(sprintf("Error processing book ID %s: %s", book_id, e$message))
    })

    if (length(result_list) == 0) {
      result_list <- list(list(
        book_id = book_id,
        reviewer_id = NA,
        reviewer_name = NA,
        review_content = NA,
        reviewer_followers = NA,
        reviewer_total_reviews = NA,
        review_date = NA,
        review_rating = NA
      ))
    }

    result_list
  }

  # Read book IDs
  book_ids <- readLines(book_ids_path)
  message(sprintf("Total book IDs to process: %d", length(book_ids)))

  if (use_parallel) {
    cl <- parallel::makeCluster(num_cores)

    # Export the entire environment of the scrape_reviews function
    environment_to_export <- environment()
    parallel::clusterExport(cl, varlist = ls(envir = environment_to_export), envir = environment_to_export)

    parallel::clusterEvalQ(cl, {
      library(rvest)
      library(dplyr)
      library(stringr)
      library(purrr)
    })

    all_reviews <- parallel::parLapply(cl, book_ids, function(book_id) {
      scrape_reviews_for_book(book_id, num_reviews)
    })

    parallel::stopCluster(cl)
  } else {
    all_reviews <- lapply(book_ids, function(book_id) scrape_reviews_for_book(book_id, num_reviews))
  }

  all_reviews_df <- dplyr::bind_rows(all_reviews)

  message(sprintf("%s %s: Completed! All book reviews extracted", Sys.time(), script_name))
  message(sprintf("Scraping run time = %s", Sys.time() - start_time))
  message(sprintf("Total books processed: %d", length(unique(all_reviews_df$book_id))))

  all_reviews_df
}
