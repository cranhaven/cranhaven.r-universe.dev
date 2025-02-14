## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----setup, message = FALSE---------------------------------------------------
library(tidyr)
library(dplyr)
library(repurrrsive)
library(unnest)
options(unnest.return.type = "tibble")

## -----------------------------------------------------------------------------
repos <- tibble(repo = gh_repos)
repos <- unnest_longer(repos, repo)
hoist(repos, repo,
      login = c("owner", "login"),
      name = "name",
      homepage = "homepage",
      watchers = "watchers_count") %>%
  select(-repo)

## -----------------------------------------------------------------------------
spec <- s(stack = TRUE,
          s(stack = TRUE,
            s("name"),
            s("homepage"),
            s("watchers_count", as = "watchers"),
            s("owner",
              s("login"))))
unnest(gh_repos, spec)

## -----------------------------------------------------------------------------
spec <- s(stack = TRUE,
          s(stack = TRUE,
            s("name"),
            s("homepage"),
            s("watchers_count", as = "watchers"),
            s("owner")))
unnest(gh_repos, spec) %>% tibble()

## -----------------------------------------------------------------------------
tibble(char = got_chars) %>%
  unnest_wider(char) %>%
  select(name, books, tvSeries) %>%
  unnest_longer(books) %>%
  unnest_longer(tvSeries)

unnest(got_chars,
       s(stack = T,
         s("name"),
         s("books,tvSeries/", stack = T)))

## -----------------------------------------------------------------------------
tibble(char = got_chars) %>%
  hoist(char, name = "name", title = "titles") %>%
  select(-char) %>%
  unnest_longer(title)

unnest(got_chars,
       s(stack = T,
         s("name"),
         s("titles/", stack = T)))

## -----------------------------------------------------------------------------
str(discog[[3]])

## -----------------------------------------------------------------------------
tibble(disc = discog) %>%
  unnest_wider(disc) %>%
  hoist(basic_information, artist = "artists") %>%
  select(disc_id = id, artist) %>%
  unnest_longer(artist) %>%
  unnest_wider(artist)

tibble(disc = discog) %>%
  unnest_wider(disc) %>%
  hoist(basic_information, format = "formats") %>%
  select(disc_id = id, format) %>%
  unnest_longer(format) %>%
  unnest_wider(format) %>%
  unnest_longer(descriptions)

