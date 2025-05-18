## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 5, tibble.print_max = 5)

## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)

## ----include=FALSE------------------------------------------------------------
suppressWarnings(library(SticsRFiles))

## ----echo=FALSE---------------------------------------------------------------
stics_version <- get_stics_versions_compat()$latest_version

## -----------------------------------------------------------------------------
example_txt_dir <-
  get_examples_path(file_type = "txt", stics_version = stics_version)

## -----------------------------------------------------------------------------
get_stics_versions_compat()$versions_list

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "patm")

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "atm")

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "a")$soil

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "interrang")

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "stlevamf",
              variety = c("Pactol", "Cecilia", "clarica"))

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "stlevamf",
              variety = c(1, 2, 5))

## ----eval=FALSE---------------------------------------------------------------
#  get_climate_txt(workspace = example_txt_dir)

## ----echo=FALSE---------------------------------------------------------------
library(dplyr)
get_climate_txt(workspace = example_txt_dir) %>%
  rmarkdown::paged_table()

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "patm")

## -----------------------------------------------------------------------------
set_param_txt(workspace = example_txt_dir, param = "patm", value = 900)

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "patm")

## ----include=FALSE------------------------------------------------------------
# resetting the value:
set_param_txt(workspace = example_txt_dir, param = "patm", value = 1000)

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "densinitial")

## -----------------------------------------------------------------------------
set_param_txt(workspace = example_txt_dir,
              param = "densinitial",
              plant_id = 1,
              layer = c(1, 4),
              value = c(0.5, 0.1))

## -----------------------------------------------------------------------------
get_param_txt(workspace = example_txt_dir, param = "densinitial")

## -----------------------------------------------------------------------------
gen_varmod(workspace = example_txt_dir, var = c("lai(n)", "masec(n)"))

## -----------------------------------------------------------------------------
get_varmod(example_txt_dir)

## -----------------------------------------------------------------------------
gen_varmod(workspace = example_txt_dir, var = c("hauteur"), append = TRUE)
get_varmod(example_txt_dir)

## -----------------------------------------------------------------------------
get_var_info("lai")

