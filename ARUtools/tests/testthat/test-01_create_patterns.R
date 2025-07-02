test_that("create_pattern_XXXX()", {
  expect_silent(d <- create_pattern_date())
  expect_silent(t <- create_pattern_time())
  expect_silent(s <- create_pattern_dt_sep())

  p <- paste0(d, s, t)

  # Different separators
  test <- c(
    "sdfs20210605T230000lkajsdlfj89", "aru2021_06_05T23_00_00_site2",
    "2021-06-05T23:00:00", "20210605T2300_siteX"
  )
  expect_equal(
    stringr::str_extract(test, p),
    c(
      "20210605T230000", "2021_06_05T23_00_00",
      "2021-06-05T23:00:00", NA_character_
    )
  ) # No seconds
  expect_silent(stringr::str_extract(test, p) |> lubridate::ymd_hms())

  # Different order
  expect_silent(d <- create_pattern_date(order = "mdy", yr_digits = 2))
  p <- paste0(d, s, t)

  test <- c(
    "sdfs061206T230000lkajsdlfj89", "aru06_05_10T23_00_00_site2",
    "06-05-21T23:00:00"
  )
  expect_equal(
    stringr::str_extract(test, p),
    c("061206T230000", "06_05_10T23_00_00", "06-05-21T23:00:00")
  )
  expect_silent(stringr::str_extract(test, p) |> lubridate::mdy_hms())

  ## With lookarounds
  expect_silent(d <- create_pattern_date(look_ahead = "T", look_behind = 'sdfs'))
  expect_silent(t <- create_pattern_time(look_behind = "T", look_ahead = "lkajs"))
  expect_silent(s <- create_pattern_dt_sep(look_ahead = '\\d', look_behind = '\\d'))

  p <- paste0(d, s, t)

  test_look <-  "sdfs20210605T230000lkajsdlfj89"

  expect_equal(
    stringr::str_extract(test_look, p),
    "20210605T230000"
  )

  ## Bad look around
  expect_silent(d <- create_pattern_date(look_ahead = "T", look_behind = 'bad'))
  expect_equal(
    stringr::str_extract(test_look, d),
    NA_character_
  )

  # Check maybe in create_pattern_time
  # Note this requires a look_behind when excluded from full usage
  expect_silent(t <- create_pattern_time(seconds = 'maybe', look_behind = "T"))
  test_s <- c(
    "sdfs061206T230000lkajsdlfj89", "aru06_05_10T23_00_00_site2",
    "06-05-21T23:00:00",  "sdfs061206T2300lkajsdlfj89",
    "aru06_05_10T23_00_site2",
    "06-05-21T23:00"
  )
  expect_equal(
  stringr::str_extract(test_s, t),
  c("230000", "23_00_00", "23:00:00","2300", "23_00",
    "23:00")
  )

  expect_equal(
  paste0(create_pattern_sep("T", optional = F), "?"),
  create_pattern_sep("T"))

  expect_equal(create_pattern_sep(""), "")

  expect_error(test_pattern(1),
               "`test` must be text")

  expect_error(test_pattern("test_text", 1),
               "`pattern` must be text")

  expect_equal(test_pattern("123directory8383", "\\d{1}[a-z]+\\d{2}"),
               stringr::str_extract("123directory8383", "\\d{1}[a-z]+\\d{2}"))


})

test_that("create_pattern_site_id",{
  # Checks
  expect_error(create_pattern_site_id(prefix = 1),
               "`prefix` must be text")
  expect_error(create_pattern_site_id(p_digits = "string"),
               "`p_digits` must be numeric")
  expect_error(create_pattern_site_id(sep = 42),
               "`sep` must be text")
  expect_error(create_pattern_site_id(suffix = 1),
               "`suffix` must be text")
  expect_error(create_pattern_site_id(s_digits = "string"),
               "`s_digits` must be numeric")

  expect_equal(create_pattern_site_id(prefix = "",
                                      p_digits = 0, sep = "T",
                                      s_digits = 1),
               "(T)((\\d{1}))")

  pp <- create_pattern_site_id(prefix = "",
                               p_digits = 1:4, s_digits = 0)
  expect_true(
    all(purrr::map_lgl(1:4,
        ~stringr::str_detect(pp,as.character(.x)) ) ) )

})

test_that("create_pattern_aru_id",{
  # Checks
  expect_error(create_pattern_aru_id(arus = 1),
               "`arus` must be text")
  expect_error(create_pattern_aru_id(n_digits = "string"),
               "`n_digits` must be numeric")
  expect_error(create_pattern_aru_id(sep = 42),
               "`sep` must be text")
  expect_error(create_pattern_aru_id(suffix = 1),
               "`suffix` must be text")
  expect_error(create_pattern_aru_id(prefix = 1),
               "`prefix` must be text")
  sep_ <- "this_is_a_separator"
  expect_true(stringr::str_detect(
    create_pattern_aru_id(sep= sep_),
    sep_))

  # expect_equal(create_pattern_aru_id(prefix = "",#sep_ expect_equal(create_pattern_aru_id(prefix = "",
  #                                     p_digits = 0, sep = "T",
  #                                     s_digits = 1),
  #              "(T)((\\d{1}))")
  #
  pp <- create_pattern_aru_id(prefix = "",
                               n_digits = c(1,4))
  expect_true(stringr::str_detect(pp, "\\{1,4\\}"))

  pp <- create_pattern_aru_id(prefix = "",
                              n_digits = 486)
  expect_true(stringr::str_detect(pp, "486"))


  a <- paste0(c("BARLT", "S\\d(A|U)", "SM\\d", "SMM", "SMA"), collapse = "|")
  expect_true(stringr::str_detect(create_pattern_aru_id(), a))

  pre_p <- c("prefix", "here")
  pre_p_c <- paste0(pre_p, collapse="|")

  expect_true(stringr::str_detect(create_pattern_aru_id(prefix = pre_p),
                                  pre_p_c))

  expect_true(stringr::str_detect(create_pattern_aru_id(suffix = pre_p),
                                  pre_p_c))

  expect_equal(
    paste0(pre_p_c, stringr::str_remove_all(a, "\\(|\\)"), sep_, "\\d{1,4}", pre_p_c),
    create_pattern_aru_id(arus = a, n_digits = c(1,4),sep = sep_,prefix = pre_p, suffix = pre_p
                          ) |>
      stringr::str_remove_all("\\(|\\)")
  )

})



test_that("create_lookaround()",
          {
            # Test output is as expected for different positions and wording
            expect_equal(
              paste("(?<!","\\d+)" , create_pattern_date(),   sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'before', negate = T)
            )
            expect_equal(
              paste("(?<!","\\d+)" , create_pattern_date(),   sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'BEFORE', negate = T)
            )
            expect_equal(
              paste("(?<!","\\d+)" , create_pattern_date(),   sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'Behind', negate = T)
            )
            expect_equal(
              paste("(?<!","\\d+)" , create_pattern_date(),   sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'behind', negate = T)
            )

            expect_equal(
              paste("(?<=","\\d+)" , create_pattern_date(),   sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'before', negate = F)
            )





            expect_equal(
              paste(create_pattern_date(), "(?!","\\d+)" ,  sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'ahead', negate = T)
            )
            expect_equal(
              paste(create_pattern_date(), "(?!","\\d+)" ,  sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'AHEAD', negate = T)
            )
            expect_equal(
              paste(create_pattern_date(), "(?!","\\d+)" ,  sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'after', negate = T)
            )


            expect_equal(
              paste(create_pattern_date(), "(?=","\\d+)" ,  sep = ""),
              create_lookaround(create_pattern_date(), "\\d+", position = 'ahead', negate = F)
            )

            # Test pipe
            piped_pattern <- "\\d+" |>
              create_lookaround("this_is_the_look_behind_",
                                position = 'behind') |>

              create_lookaround("_this_is_look_ahead",
                                position = 'ahead')
            expect_equal(piped_pattern,
                         "(?<=this_is_the_look_behind_)\\d+(?=_this_is_look_ahead)")


            # Test it works, with pipe

            expect_true(
              stringr::str_detect(string =
                "this_is_the_look_behind_156456_this_is_look_ahead",
                pattern = piped_pattern
              )
            )

            # Checks for non-text
            expect_error(
              create_lookaround(create_pattern_date(), 1, position = 'before'),
              "`lookaround_pattern` must be text"
            )
            expect_error(
              create_lookaround(1, "lookaround", position = 'after'),
              "`pattern` must be text"
            )



          }
)


test_that("create_pattern_tz_offset",{
  # Checks
  expect_error(create_pattern_tz_offset(direction_from_UTC = "+"),
               "`direction_from_UTC` must be among")
  expect_error(create_pattern_tz_offset(direction_from_UTC = 4),
               "`direction_from_UTC` must be text")
  expect_error(create_pattern_tz_offset(n_digits_hrs =  "string"),
               "`n_digits_hrs` must be numeric")
  expect_error(create_pattern_tz_offset(n_digits_hrs =  "string"),
               "`n_digits_hrs` must be numeric")

  expect_true(stringr::str_detect(
    create_pattern_tz_offset(n_digits_min = 14),
    "14\\}$"))
  expect_true(stringr::str_detect(
    create_pattern_tz_offset(n_digits_min = 81),
    "81\\}"))
  expect_true(stringr::str_detect(
    create_pattern_tz_offset(direction_from_UTC = "West"),
    "\\-"))
  expect_true(stringr::str_detect(
    create_pattern_tz_offset(direction_from_UTC = "East"),
    "\\+"))
  expect_true(stringr::str_detect(
    create_pattern_tz_offset(direction_from_UTC = "Both"),
    "\\[\\+\\,\\-\\]"))

})
