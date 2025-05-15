test_that("`viralx_mars()` works", {
  local_edition(3)
  library(dplyr)
  library(rsample)
  library(Formula)
  library(plotmo)
  library(plotrix)
  library(TeachingDemos)

  cd_2019 <- c(824, 169, 342, 423, 441, 507, 559,
               173, 764, 780, 244, 527, 417, 800,
               602, 494, 345, 780, 780, 527, 556,
               559, 238, 288, 244, 353, 169, 556,
               824, 169, 342, 423, 441, 507, 559)
  vl_2019 <- c(40, 11388, 38961, 40, 75, 4095, 103,
               11388, 46, 103, 11388, 40, 0, 11388,
               0,   4095,   40,  93,  49,  49,  49,
               4095,  6837, 38961, 38961, 0, 0, 93,
               40, 11388, 38961, 40, 75, 4095, 103)
  cd_2021 <- c(992, 275, 331, 454, 479, 553,  496,
               230, 605, 432, 170, 670, 238,  238,
               634, 422, 429, 513, 327, 465,  479,
               661, 382, 364, 109, 398, 209, 1960,
               992, 275, 331, 454, 479, 553,  496)
  vl_2021 <- c(80, 1690,  5113,  71,  289,  3063,  0,
               262,  0,  15089,  13016, 1513, 60, 60,
               49248, 159308, 56, 0, 516675, 49, 237,
               84,  292,  414, 26176,  62,  126,  93,
               80, 1690, 5113,    71, 289, 3063,   0)
  cd_2022 <- c(700, 127, 127, 547, 547, 547, 777,
               149, 628, 614, 253, 918, 326, 326,
               574, 361, 253, 726, 659, 596, 427,
               447, 326, 253, 248, 326, 260, 918,
               700, 127, 127, 547, 547, 547, 777)
  vl_2022 <- c(0,   0,   53250,   0,   40,   1901, 0,
               955,    0,    0,    0,   0,   40,   0,
               49248, 159308, 56, 0, 516675, 49, 237,
               0,    23601,   0,   40,   0,   0,   0,
               0,    0,     0,     0,    0,    0,  0)
  x <- cbind(cd_2019, vl_2019, cd_2021, vl_2021, cd_2022, vl_2022) |>
    as.data.frame()
  set.seed(123)
  hi_data <- rsample::initial_split(x)
  set.seed(123)
  hiv_data <- hi_data |>
    rsample::training()
  nt <- 3
  pd <- 1
  pru <- "none"
  vip_featured <- c("cd_2022")
  vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
  set.seed(123)
  vi_train <- rsample::initial_split(x)
  set.seed(123)
  vip_train <- vi_train |>
    rsample::training() |>
    dplyr::select(rsample::all_of(vip_features))
  vip_new <- vip_train[1,]
  expect_snapshot(print(viralx_mars(vip_featured, hiv_data, nt, pd, pru, vip_train, vip_new)))
})
