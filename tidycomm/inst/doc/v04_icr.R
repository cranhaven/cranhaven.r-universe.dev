## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message = FALSE, warning = FALSE, include = FALSE-----------------
library(tidycomm)

## -----------------------------------------------------------------------------
fbposts

## -----------------------------------------------------------------------------
fbposts %>% 
  test_icr(post_id, coder_id, pop_elite, pop_people, pop_othering)

## -----------------------------------------------------------------------------
fbposts %>% 
  test_icr(post_id, coder_id)

## -----------------------------------------------------------------------------
fbposts %>% 
  test_icr(post_id, coder_id, fleiss_kappa = TRUE, agreement = FALSE)

## -----------------------------------------------------------------------------
fbposts %>% 
  test_icr(post_id, coder_id, levels = c(n_pictures = "ordinal"))

## -----------------------------------------------------------------------------
# Introduce some missing values
fbposts$type[1] <- NA
fbposts$type[2] <- NA
fbposts$pop_elite[5] <- NA

fbposts %>% 
  test_icr(post_id, coder_id)

## -----------------------------------------------------------------------------
fbposts %>% 
  test_icr(post_id, coder_id, na.omit = TRUE)

## -----------------------------------------------------------------------------
data <- tibble::tibble(
  unit = c(1, 1, 2, 2, 3, 3),
  coder = c('a', 'b', 'a', 'c', 'b', 'c'),
  code = c(1, 0, 1, 1, 0, 0)
)

data

## ----error=TRUE---------------------------------------------------------------
data %>%
  test_icr(unit, coder, code, na.omit = TRUE)

## -----------------------------------------------------------------------------
data %>% 
  dplyr::group_by(unit) %>% 
  dplyr::mutate(coding = 1:dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  test_icr(unit, coding, code)

