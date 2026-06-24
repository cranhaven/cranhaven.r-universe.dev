## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, 
  message = FALSE,
  out.width='\\textwidth', fig.height = 4, fig.width = 5, fig.align='center'
)


## ----eval = FALSE-------------------------------------------------------------
#  install.packages("Goodreader")

## -----------------------------------------------------------------------------
library(Goodreader)

## ----eval = FALSE-------------------------------------------------------------
#  parent_df <- search_goodreads(search_term = "parenting", search_in = "title", num_books = 10, sort_by = "ratings")

## ----eval = FALSE-------------------------------------------------------------
#  summary(parent_df)
#  ##   title              author            book_id
#  ## Length:10          Length:10          Length:10
#  ## Class :character   Class :character   Class :character
#  ## Mode  :character   Mode  :character   Mode  :character
#  
#  ##     url               ratings
#  ## Length:10          Min.   : 8427
#  ## Class :character   1st Qu.:11744
#  ## Mode  :character   Median :13662
#  ##                    Mean   :19757
#  ##                    3rd Qu.:13784
#  ##                    Max.   :69591

## ----eval = FALSE-------------------------------------------------------------
#  search_goodreads(search_term = "J.K. Rowling", search_in = "author", num_books = 5, sort_by = "ratings")

## ----eval = FALSE-------------------------------------------------------------
#  search_goodreads(search_term = "J.K. Rowling", search_in = "author", num_books = 5, sort_by = "published_year")

## ----eval = FALSE-------------------------------------------------------------
#  get_book_ids(input_data = parent_df, file_name = "parent_books.txt") #the book IDs are now stored in a text file named “parent_books”

## ----eval = FALSE-------------------------------------------------------------
#  parent_bookinfo <- scrape_books(book_ids_path = "parent_books.txt", use_parallel = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  parent_bookreviews <- scrape_reviews(book_ids_path = "parent_books.txt", num_reviews = 10, use_parallel = FALSE) #users can also turn on parallel process to speed up the process

## ----eval = FALSE-------------------------------------------------------------
#  sentiment_results <- analyze_sentiment(parent_bookreviews, lexicon = "afinn")

## ----eval = FALSE-------------------------------------------------------------
#  ave_sentiment <- average_book_sentiment(sentiment_results)
#  summary(ave_sentiment)
#  ##    book_id          avg_sentiment
#  ##  Length:10          Min.   : 4.40
#  ##  Class :character   1st Qu.: 7.25
#  ##  Mode  :character   Median :12.86
#  ##                     Mean   :12.95
#  ##                     3rd Qu.:14.65
#  ##                     Max.   :27.30

## ----eval=FALSE---------------------------------------------------------------
#  sentiment_histogram(sentiment_results)

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/sentiment_hist.png')

## ----eval=FALSE---------------------------------------------------------------
#  sentiment_trend(sentiment_results, time_period = "year")

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/sentiment_trend.png')

## ----eval = FALSE-------------------------------------------------------------
#  reviews_topic <- model_topics(parent_bookreviews, num_topics = 3, num_terms = 10, english_only = TRUE)
#  ## Topic 1:
#  ## parent, children, need, one, way, good, get, work, dont, give
#  ##
#  ## Topic 2:
#  ## parent, child, book, emot, feel, help, also, can, children, use
#  ##
#  ## Topic 3:
#  ## book, just, kid, think, read, like, time, say, realli, much

## ----eval=FALSE---------------------------------------------------------------
#  plot_topic_terms(reviews_topic)

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/topic_terms.png')

## ----eval=FALSE---------------------------------------------------------------
#  gen_topic_clouds(reviews_topic)

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/Topic1.png')

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/Topic2.png')

## ----echo=FALSE, out.width='400px'--------------------------------------------
knitr::include_graphics('../man/figures/Topic3.png')

## ----echo=FALSE---------------------------------------------------------------
library(dplyr)
data.frame(Function = c("get_book_ids()", "get_book_summary()", "get_author_info()", "get_genres()", "get_published_time()", "get_num_pages()", "get_format_info()", "get_rating_distribution()"), 
           Output = c("Text file",  "List", "List", "List", "List", "List", "List", "List"),    
           Description = c("Retrieve the book IDs from the input data and save to a text file ", "Retrieve the summary for each book", "Retrieve the author information for each book", "Extract the genres for each book", "Retrieve the published time for each book", "Retrieve the number of pages for each book", "Retrieve the format information for each book", "Retrieve the rating distribution for each book")) %>%
  knitr::kable() 

