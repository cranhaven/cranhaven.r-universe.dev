## ----results=FALSE, message=FALSE---------------------------------------------
library(curl)
library(XML)

## -----------------------------------------------------------------------------
extract_links <- function(data) {
  (data$content
    |> rawToChar()
    |> XML::getHTMLLinks(externalOnly=TRUE)
    |> vapply(FUN.VALUE="",
              \(link) tryCatch(XML::getRelativeURL(link, data$url, addBase=TRUE),
                               error=\(e) NA_character_))
    |> na.omit()
    |> as.character())
}

## -----------------------------------------------------------------------------
spider_site <- function(
                        start_pages, #starting URLs
                        regexp,    # Linked URLs must match this to be included
                        limit=500) { # maximum number of pages to collect

  #all encountered pages will be collected in this hash table
  pages <- new.env()
  # keep track of how many URLS seen and stop after a limit
  seen <- 0

  # inner helper function:
  is_new_page <- function(url) {
    is_new <- (seen < limit) && !exists(url, pages) && grepl(regexp, url)
    if (is_new) {
      #mark this page as "in progress" and increment counter
      pages[[url]] <<- list()
      seen <<- seen + 1
    }
    is_new
  }

  # define inner recursive function to visit a page
  visit_page <- function(url) {
    cat("visiting ", url, "\n")

    # Fetch the page
    start_time <- Sys.time()
    data <- curl_fetch_memory(url)
    end_time <- Sys.time()

    # extract the links and store our page in the hash table.
    links <- extract_links(data) |> unique()
    pages[[url]] <- data.frame(
      url = url, start = start_time, end = end_time, links = I(list(links)))

    #recursively follow new links, if within the site filter
    ( links
      |> Filter(f=is_new_page)
      |> lapply(visit_page))

    invisible(NULL)
  }

  # Kick off by visiting each page in the starting set.
  start_pages |> lapply(visit_page)

  #Return our hash table as a data frame with "links" as a list-column.
  pages |> as.list() |> do.call(what=rbind)
}


## ----eval=FALSE---------------------------------------------------------------
#  spidered <- spider_site("https://mysite.example/webapp", "mysite\\.example/")
#  saveRDS(spidered, file="spidered.rds")

## ----include=FALSE, eval=FALSE------------------------------------------------
#  spidered <- spider_site("https://rdrr.io/cran/async/", "rdrr\\.io/cran/async/")
#  saveRDS(spidered, file="spidered.rds")

## -----------------------------------------------------------------------------
spidered <- readRDS("spidered.rds")

## ---- fig.width=7-------------------------------------------------------------
library(ggplot2)
time_plot <- function(results) (
  ggplot(results)
  + aes(xmin=start-min(start), xmax=end-min(start), y=rank(start))
  + scale_x_continuous("Elapsed(s)", breaks=0:16, minor_breaks=seq(0, 16, by=0.5))
  + scale_y_reverse("Page #")
  + geom_errorbarh()
)
time_plot(spidered)

## ----include=FALSE------------------------------------------------------------
total.time <- as.numeric(with(spidered, max(end) - min(start)))
curl.time <- as.numeric(sum(with(spidered, end-start)))

## -----------------------------------------------------------------------------
library(later)
library(promises)

# global variable
curl_is_active <- FALSE

curl_fetch_async <- function(url) {
  # Promise constructor provides two callback functions
  # which fit right in curl_fetch_multi's arguments
  pr <- promise(function(resolve, reject) {
    curl_fetch_multi(url, done=resolve, fail=reject)
    # since we've just told curl to do something new, let it start
    multi_run(timeout = 0, poll = TRUE)
  })

  # And then start checking it periodically
  if (!curl_is_active) {
    curl_is_active <<- TRUE
    later(poll_curl)
  }
  pr
}

## -----------------------------------------------------------------------------
poll_curl <- function() {
  if (length(multi_list()) == 0) {
    curl_is_active <<- FALSE
  } else {
    multi_run(timeout = 0.001, poll = TRUE)
    later(poll_curl)
  }
}

## -----------------------------------------------------------------------------
library(async)
spider_site_async <- async(function(
                        start_pages, #starting URLs
                        regexp,    # Linked URLs must match this to be included
                        limit=500) { # maximum number of pages to collect

  #all encountered pages will be collected in this hash table
  pages <- new.env()
  # keep track of how many URLS seen and stop after a limit
  seen <- 0

  # inner helper function:
  is_new_page <- function(url) {
    is_new <- (seen < limit) && !exists(url, pages) && grepl(regexp, url)
    if (is_new) {
      #mark this page as "in progress" and increment counter
      pages[[url]] <<- list()
      seen <<- seen + 1
    }
    is_new
  }

  # define inner recursive function to visit a page
  visit_page <- async(function(url) {
    cat("visiting (async) ", url, "\n")

    # Fetch the page
    start_time <- Sys.time()
    data <- curl_fetch_async(url) |> await()
    end_time <- Sys.time()

    # extract the links and store our page in the hash table.
    links <- extract_links(data) |> unique()
    pages[[url]] <- data.frame(
      url = url, start = start_time, end = end_time, links = I(list(links)))

    #recursively follow new links, if within the site filter
    ( links
      |> Filter(f=is_new_page)
      |> lapply(visit_page)
      |> promise_all(.list=_)
      |> await())

    invisible(NULL)
  })

  # Kick off by visiting each page in the starting set.
  start_pages |> lapply(visit_page) |> promise_all(.list=_) |> await()

  #Return our hash table as a data frame with "links" as a list-column.
  pages |> as.list() |> do.call(what=rbind)
})

## ----eval=FALSE---------------------------------------------------------------
#  spidered <- spider_site("https://mysite.example/webapp", "mysite\\.example/")
#  spidered_async |> then(\(x) x |> saveRDS("spidered_async.rds"))

## ----eval=FALSE, include=FALSE------------------------------------------------
#  spidered_async <- spider_site_async("https://rdrr.io/cran/async/", "rdrr\\.io/cran/async/")
#  spidered_async |> then(\(x) x |> saveRDS("spidered_async.rds"))

## -----------------------------------------------------------------------------
spidered_async_data <- readRDS("spidered_async.rds")

## ----results=FALSE, message=FALSE---------------------------------------------
library(dplyr)

## ----fig.width=7--------------------------------------------------------------
spidered <- (spidered
  |> mutate(method="sync", order=rank(start),
            end=end-min(start), start=start-min(start)))
spidered_async_data <- (spidered_async_data
  |> mutate(method="async", order=rank(start),
             end=end-min(start), start=start-min(start)))

(rbind(spidered, spidered_async_data)
  |> time_plot()
  + aes(y=order, color=method))

