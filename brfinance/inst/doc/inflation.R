## ----include = FALSE----------------------------------------------------------
# Definir mirror do CRAN primeiro
options(repos = c(CRAN = "https://cloud.r-project.org/"))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----inflation-usage, eval=FALSE----------------------------------------------
# devtools::install_github("efram2/brfinance")
# library(brfinance)
# 
# # Complete data (default)
# inflation <- get_inflation_rate()
# 
# # Specific period in English
# inflation_eng <- get_inflation_rate(
#   start_date = "2020-01-01",
#   end_date = "2024-12-01",
#   language = "eng"
# )
# 
# # Portuguese version
# inflation_pt <- get_inflation_rate(
#   start_date = "2020-01-01",
#   language = "pt"
# )

