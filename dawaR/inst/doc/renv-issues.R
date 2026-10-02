## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, include=FALSE------------------------------------------------
# renv::purge("sf")
# renv::remove("sf")
# renv::purge("dawaR")
# renv::remove("dawaR")
# 
# reprex::reprex({
#   options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
#   renv::install("sf")
#   renv::install("aleksanderbl29/dawaR")
#   dawaR::get_map_data("regioner")
# })

## ----eval=FALSE---------------------------------------------------------------
# options(repos = c(CRAN = "https://cran.rstudio.com/"))
# renv::remove("sf")
# renv::purge("sf")
# renv::install("sf")
# renv::install("aleksanderbl29/dawaR")
# renv::snapshot()
# renv::clean() # for good measure
# renv::rebuild()

