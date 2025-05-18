## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)


## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)

## ----include=FALSE------------------------------------------------------------
suppressWarnings(library(SticsRFiles))

## ----echo=FALSE---------------------------------------------------------------
stics_version <- get_stics_versions_compat()$latest_version

## ----include = FALSE, eval=FALSE----------------------------------------------
#  example_data <- SticsRFiles::download_data(out_dir = tempdir(),
#                                            example_dirs = "study_case_1", "V9.2")
#  workspace <- file.path(example_data, "XmlFiles")
#  out_dir <- file.path(tempdir(), "XmlFiles_V10.2.0")
#  if (!dir.exists(out_dir)) dir.create(out_dir)
#  javastics <-
#    "/home/plecharpent/tmp/TEST_UPDATE_XML_V9_V10/JavaSTICS-1.41-stics-9.2"
#  upgrade_workspace_xml(workspace = workspace, javastics = javastics,
#                        out_dir = out_dir, overwrite = TRUE)

## ----eval = FALSE, results='markup'-------------------------------------------
#  workspace <- "/path/to/workspace/dir/V9.2"
#  out_dir <- "/path/to/out/dir/V10.2.0"
#  javastics <- "/path/to/JavaSTICS-1.41-stics-9.2"
#  upgrade_workspace_xml(workspace = workspace, javastics = javastics,
#                        out_dir = out_dir)

## ----include = FALSE, eval=FALSE----------------------------------------------
#  usms_path <- file.path(example_data, "XmlFiles", "usms.xml")
#  out_dir <- file.path(tempdir(), "XmlFiles_V10.2.0")
#  param_gen_path <- file.path(javastics, "config", "param_gen.xml")
#  upgrade_usms_xml(file = usms_path, param_gen_file = param_gen_path,
#                   out_dir = out_dir, overwrite = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  usms_path <- "/path/to/workspace/dir/V9.2/usms.xml"
#  out_dir <- "/path/to/workspace/dir/V10.2.0"
#  param_gen_path <- "/path/to/JavaSTICS-1.41-stics-9.2/config/param_gen.xml"
#  upgrade_usms_xml(file = usms_path, param_gen_file = param_gen_path,
#                   out_dir = out_dir)

