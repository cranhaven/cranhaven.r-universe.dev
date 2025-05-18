## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)

## ----include=FALSE------------------------------------------------------------
suppressWarnings(library(SticsRFiles))
# just in case for unzipping examples files in tempdir
get_examples_path(c("xml", "csv"))

## ----include=FALSE------------------------------------------------------------
example_data <- SticsRFiles::download_data(out_dir = tempdir(),
                                           example_dirs = "study_case_1",
                                           "V10.0")

## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)
#  example_data <- SticsRFiles::download_data(example_dirs = "study_case_1",
#                                             "V10.0")

## -----------------------------------------------------------------------------
workspace <- file.path(example_data, "XmlFiles")
plant_file <- file.path(workspace, "plant", "maisopti_plt.xml")

## -----------------------------------------------------------------------------
SticsRFiles::get_var_info("lai")

## -----------------------------------------------------------------------------
SticsRFiles::get_var_info(keyword = "lai")

## -----------------------------------------------------------------------------
get_param_info(param = "lai")

## -----------------------------------------------------------------------------
get_param_info(keyword = "plant")

## -----------------------------------------------------------------------------
dlaimax <- get_param_xml(plant_file, "dlaimax")
dlaimax

## -----------------------------------------------------------------------------
values <- get_param_xml(plant_file, select = "formalisme",
                        select_value = "radiation interception")
unlist(values) # For pretty-printing

## -----------------------------------------------------------------------------
set_param_xml(plant_file, "dlaimax", unlist(dlaimax) * 1.3, overwrite = TRUE)

## -----------------------------------------------------------------------------
dlaimax <- get_param_xml(plant_file, "dlaimax")
dlaimax

## -----------------------------------------------------------------------------
obs_df <- data.frame(usm_name = "Test", ian = 2021, mo = 3:10, jo = 1,
                     `masec(n)` = 0.1 * 3:10)

## ----eval=FALSE---------------------------------------------------------------
#  gen_obs(df = obs_df, out_dir = "/path/to/dest/dir")

## -----------------------------------------------------------------------------
obs <- get_obs(workspace)

## ----eval=FALSE---------------------------------------------------------------
#  sim <- get_sim(workspace)
#  #> Warning in get_file_(workspace = x, usm_name = usm_name, usms_filepath =
#  #> usms_path, : Not any sim file detected in
#  #> workspace/tmp/RtmpjkDYAq/data-master/
#  #> study_case_1/V10.0/XmlFiles

