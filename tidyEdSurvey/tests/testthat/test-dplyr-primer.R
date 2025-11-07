skip_if_not_installed("dplyr")
require(dplyr)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

suppressMessages(attach(sdf))

test_that("distinct",{
  sdf %>%
    distinct(scrpsu) %>%
    nrow() %>%
    expect_equal(680)
  
})

test_that("select",{
  sdf %>%
    select(matches("m[0-9]{6}")) %>%
    ncol() %>%
    expect_equal(147)
  
})

test_that("select a subscale",{
  sdf %>%
    select(algebra,geometry) %>%
    ncol() %>%
    expect_equal(10)
})

test_that("mutate",{
  sdf_m <- sdf %>%
    mutate(parent_hs_grad = case_when(
      pared %in% c("Did not finish H.S.") ~ "No",
      pared %in% c("Graduated college","Graduated H.S.","Some ed after H.S.") ~ "Yes",
      pared %in% c("Multiple","Omitted","I Don't Know") ~ "Unknown"
    ))
  expect_equal(sort(unique(sdf_m$parent_hs_grad)),c("No","Unknown","Yes"))
  
})

test_that("group_by and summarise",{
  sdf_s <- sdf %>%
    group_by(dsex) %>%
    summarise(avg_math = mean(mrpcm1))
  
  expect_equal(round(sdf_s$avg_math,4), c(276.2437,275.4750))
  
})

test_that("filter",{
  sdf %>%
    filter(dsex == "Female") %>%
    nrow() %>%
    expect_equal(8429)
})
