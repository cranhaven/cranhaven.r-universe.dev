## ----include = FALSE----------------------------------------------------------
# Definir mirror do CRAN primeiro
options(repos = c(CRAN = "https://cloud.r-project.org/"))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----language-example, eval=FALSE---------------------------------------------
# # English (default)
# data_eng <- get_selic_rate(2020, 2024, language = "eng")
# 
# # Portuguese
# data_pt <- get_selic_rate(2020, 2024, language = "pt")

## ----install, eval=FALSE------------------------------------------------------
# # From CRAN
# install.packages("brfinance")
# 
# # Development version
# devtools::install_github("efram2/brfinance")
# 
# library(brfinance)

