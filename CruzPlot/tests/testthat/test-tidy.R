# test that various tidyverse functions work as expected
library(dplyr)

test_that("str_trim works as expected", {
  d <- " jlkshdf  lksd     "
  expect_identical(stringr::str_trim(d, side = "both"), "jlkshdf  lksd")
})


test_that("dplyr::summarise has .groups argument", {
  x <- data.frame(a1 = c("a", "a", "a", "b", "b", "b"),
                  a2 = c(1, 2, 1, 2, 1, 2)) %>%
    mutate(b = 1:6)

  x.out <- x %>%
    group_by(a1, a2) %>%
    summarise(b_sum = sum(b), .groups = "drop") %>%
    as.data.frame()

  df.out <- data.frame(a1 = c("a", "a", "b", "b"), a2 = c(1, 2, 1, 2),
                       b_sum = as.integer(c(4, 2, 5, 10)))
  expect_equal(x.out, df.out)
})


test_that("tidy flow", {
  df1 <- data.frame(birth_year = c(33, 112), tst = c("a", "b"), stringsAsFactors = FALSE)
  df2 <- data.frame(homeworld = c("Tatooine", "New York"), earth = c(FALSE, TRUE),
                    stringsAsFactors = FALSE)
  tst1 <- starwars %>%
    mutate(hi = "a") %>%
    left_join(df1, by = "birth_year") %>%
    full_join(df2, by = "homeworld") %>%
    arrange(name) %>%
    filter(between(height, 0, 50)) %>%
    select(!!names(starwars)) %>%
    slice(0)

  expect_equal(tst1, starwars[0, ])
})
