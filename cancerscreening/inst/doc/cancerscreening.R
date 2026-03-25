## ----setup, include = FALSE---------------------------------------------------
auth_success <- tryCatch(
  khisr:::khis_cred_docs(),
  khis_cred_internal_error = function(e) e
)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE,
  purl = khisr::khis_has_cred(),
  eval = khisr::khis_has_cred()
)

## ----eval = !khisr::khis_has_cred(), echo = FALSE, comment = NA---------------
#  cancerscreening:::cancerscreening_bullets(c(
#    "Code chunks will not be evaluated, because:",
#    strsplit(auth_success$message, split = "\n")[[1]]
#  ))
#  khisr::khis_cred_clear()

## ----include = FALSE----------------------------------------------------------
library(cancerscreening)

## ----eval = khisr::khis_has_cred()--------------------------------------------
# Get data for those screening for cervical cancer
cervical_screened <- get_cervical_screened('2022-01-01', end_date = '2022-06-30')
cervical_screened

# Get data for those screening for colorectal cancer using FOBT
colorectal_screened <- get_colorectal_fobt('2022-01-01', end_date = '2022-06-30')
colorectal_screened

# Get data for those screening for breast cancer using mammogram
breast_screened <- get_breast_mammogram('2022-01-01', end_date = '2022-06-30')
breast_screened

## ----eval = khisr::khis_has_cred()--------------------------------------------
# Get the cervical screening target population for 2022
cervical_target_population <- get_cervical_target_population(2022)
cervical_target_population

# Get the colorectal cancer screening target population for 20223 by county
colorectal_target_population <- get_colorectal_target_population(2023, level = 'county')
colorectal_target_population

# Get the population of women 15-49 year for the year 2024
wra_pop <- get_filtered_population(year = 2024, min_age = 15, max_age = 49, pop_sex = 'female')
wra_pop

