skip_on_cran()
skip_if_not_installed("dplyr")
require(dplyr)

context("dplyr methods")

# if user has independently downloaded data it will be read from there,
# otherwise it will be downloaded to the temporary session directory
if(dir.exists("C:/EdSurveyData/TIMSS/2019")) {
  path <- "C:/EdSurveyData"
}else if(dir.exists("~/EdSurveyData/TIMSS/2019")) {
  path <- "~/EdSurveyData"
}else {
  path <- tempdir()
  downloadTIMSS(root=path, year=2019, verbose = FALSE)
}

fin8.19 <- readTIMSS(path=file.path(path, "TIMSS", "2019"),
                         countries = "fin", gradeLvl = 8, verbose=FALSE)

suppressMessages(attach(fin8.19))

test_that("distinct",{
  fin8.19 %>%
    distinct(idstud) %>%
    nrow() %>%
    expect_equal(4874)

})

test_that("select",{
  fin8.19 %>%
    select(ends_with("_f")) %>%
    ncol() %>%
    expect_equal(397)

})

test_that("select a subscale",{
  fin8.19 %>%
    select(mmat,ssci) %>%
    ncol() %>%
    expect_equal(10)
  
})

test_that("mutate",{
  fin8.19_m <- fin8.19 %>%
               mutate(books_in_home = case_when(
                 bsbg04 %in% c("NONE OR VERY FEW (0-10 BOOKS)",
                               "ENOUGH TO FILL ONE SHELF (11-25 BOOKS)") ~ "<= 25",
                 bsbg04 %in% c("ENOUGH TO FILL ONE BOOKCASE (26-100 BOOKS)") ~ "26-100",
                 bsbg04 %in% c("ENOUGH TO FILL TWO BOOKCASES (101-200 BOOKS)",
                               "ENOUGH TO FILL THREE OR MORE BOOKCASES (MORE THAN 200)") ~ "> 100",
                 bsbg04 %in% c("OMITTED OR INVALID",NA) ~ "Unknown"
               ))
  expect_equal(unique(fin8.19_m$books_in_home),c("<= 25","26-100","> 100","Unknown"))

})

test_that("group_by and summarise",{
  fin8.19_s <- fin8.19 %>%
    group_by(itsex) %>%
    summarise(avg_math = mean(bsmmat01),
              avg_sci = mean(bsssci01))

  expect_equal(round(fin8.19_s$avg_math,4), c(511.2766, 507.1680))
  expect_equal(round(fin8.19_s$avg_sci,4), c(554.0858, 534.8465))

})

test_that("filter",{
  fin8.19 %>%
    filter(itsex == "FEMALE") %>%
    nrow() %>%
    expect_equal(2366)
})


suppressMessages(detach(fin8.19))

file.remove(temp)

