## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  eval = TRUE
)
# write to a temporary jar
options(cookie_dir = tempdir())
is_available <- function(p) requireNamespace(p, quietly = TRUE)
curl_available <- is_available("curl")
httr_available <- is_available("httr")
httr2_available <- is_available("httr2")

## ----setup--------------------------------------------------------------------
library(cookiemonster)

## ----eval=FALSE---------------------------------------------------------------
# file.copy(
#   from = system.file("extdata", "cookies.txt", package = "cookiemonster"),
#   to = "."
# )

## ----eval=FALSE---------------------------------------------------------------
# readLines("cookies.txt") |>
#   cat(sep = "\n")

## ----echo=FALSE---------------------------------------------------------------
system.file("extdata", "cookies.txt", package = "cookiemonster") |> 
  readLines() |> 
  cat(sep = "\n")

## ----eval=FALSE---------------------------------------------------------------
# add_cookies(cookiefile = "cookies.txt")

## ----echo=FALSE---------------------------------------------------------------
add_cookies(
  cookiefile = system.file("extdata", "cookies.txt", package = "cookiemonster"), 
  confirm = TRUE
)

## ----eval=FALSE---------------------------------------------------------------
# cookies <- get_cookies_from_browser(browser = "Firefox")
# store_cookies(cookies)

## ----eval=FALSE---------------------------------------------------------------
# sess <- rvest::read_html_live("https://vu.nl")

## ----eval=FALSE---------------------------------------------------------------
# add_cookies(session = sess)
# #> ✔ Cookies for vu.nl put in the jar!

## ----echo=FALSE---------------------------------------------------------------
options(cookie_dir = NULL)

## -----------------------------------------------------------------------------
default_jar()

## -----------------------------------------------------------------------------
options(cookie_dir = tempdir())
default_jar()

## -----------------------------------------------------------------------------
options(cookie_dir = NULL)
default_jar()

## ----echo=FALSE---------------------------------------------------------------
options(cookie_dir = tempdir())

## -----------------------------------------------------------------------------
get_cookies("hb.cran.dev")

## ----eval=httr2_available-----------------------------------------------------
library(httr2)
resp <- request("https://hb.cran.dev/cookies/set") |> # start a request
  req_options(cookie = get_cookies("hb.cran.dev", as = "string")) |> # add cookies to be sent with it
  req_perform() # perform the request

resp |> 
  resp_body_json()

## ----eval=httr2_available-----------------------------------------------------
resp <- request("https://hb.cran.dev/cookies/set") |> # start a request
  req_cookiemonster_set() |> # add cookies to be sent with it
  req_perform() # perform the request

resp |> 
  resp_body_json()

## ----eval=httr_available------------------------------------------------------
library(httr)
GET("https://hb.cran.dev/cookies/set", set_cookies(get_cookies("hb.cran.dev", as = "vector")))

## ----eval=curl_available------------------------------------------------------
library(curl)
h <- new_handle()
handle_setopt(h, cookie = get_cookies("hb.cran.dev", as = "string"))
resp <- curl_fetch_memory("https://hb.cran.dev/cookies/set", handle = h)
jsonlite::fromJSON(rawToChar(resp$content))

## ----eval=curl_available------------------------------------------------------
h2 <- new_handle()
resp <- curl_fetch_memory("https://hb.cran.dev/cookies/set?new_cookies=moo", handle = h2)
handle_cookies(h2)

## ----eval=curl_available------------------------------------------------------
new_cookies <- handle_cookies(h2)
store_cookies(new_cookies)
get_cookies("hb.cran.dev")

