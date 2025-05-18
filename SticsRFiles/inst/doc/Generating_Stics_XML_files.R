## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)
#  library(dplyr)

## ----include=FALSE------------------------------------------------------------
suppressWarnings(library(SticsRFiles))

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  workspace_path <- tempdir(),
  stics_version <- SticsRFiles::get_stics_versions_compat()$latest_version
)

## ----eval=FALSE---------------------------------------------------------------
#  # Getting the file in the current directory
#  usm_xl_file <- download_usm_xl()
#  
#  # Getting the file in a specic directory
#  xl_dir <- "/path/to/xl/dir" # or something like C:/path/to/xl/dir" for Windows
#  
#  usm_xl_file <- download_usm_xl(out_dir = xl_dir)
#  
#  #> [1] "inputs_stics_example.xlsx  has been copied in directory
#  #> "/path/to/xl/dir
#  

## ----eval=TRUE, echo = FALSE, results='hide'----------------------------------
usm_xl_file <- download_usm_xl(file = "inputs_stics_example.xlsx",
                               out_dir = workspace_path)

## ----eval=FALSE---------------------------------------------------------------
#  # Getting the file in the current directory
#  usm_csv_file <- download_usm_csv(file = "inputs_stics_example_USMs.csv")
#  
#  # Getting the file in a specific directory
#  csv_dir <- "/path/to/csv/dir" # or something like C:/path/to/xl/dir" for Windows
#  
#  usm_csv_file <- download_usm_csv(file = "inputs_stics_example_USMs.csv",
#                                   out_dir = csv_dir)
#  
#  #> [1] inputs_stics_example_USMs.csv  has been copied in directory
#  #> "/path/to/csv/dir

## ----eval=TRUE, echo = FALSE, results='hide'----------------------------------
usm_csv_file <- download_usm_csv(file = "inputs_stics_example_USMs.csv",
                                 out_dir = workspace_path)

## ----echo=FALSE, eval=TRUE, fig.align='center'--------------------------------
l <- SticsRFiles:::get_params_dict()
df <- data.frame(keyword = names(l), realname = unlist(l, use.names = FALSE),
                 stringsAsFactors = FALSE)
rmarkdown::paged_table(df)

## ----eval = TRUE, echo = FALSE------------------------------------------------
library(readxl)
xl_param <- read_params_table(usm_xl_file, sheet = "USMs")
knitr::kable(xl_param)

## ----read_xl_file_usms, eval = FALSE------------------------------------------
#  # Reading the Excel file
#  usms_param <- read_params_table(usm_xl_file, sheet_name = "USMs")
#  # Or
#  # Reading the CSV file
#  usms_param <- read_params_table(usm_csv_file)

## ----eval_read_xl_file_usms, eval = TRUE, echo = FALSE------------------------
library(readxl)

# Reading the Excel file
xl_param <- read_params_table(usm_xl_file, sheet_name = "USMs")

xl_param <- read_params_table(usm_xl_file, sheet_name = "USMs")

rmarkdown::paged_table(xl_param)

## ----gen_usms_file, eval = FALSE----------------------------------------------
#  
#  # Output file path
#  out_file <- "/path/to/file/usms.xml"
#  # or something like C:/path/to/file/usms.xml" for Windows
#  
#  # Generating a new usms.xml file, for all xl_param lines
#  gen_usms_xml(file = out_file, param_df = usms_param)

## ----eval_gen_usms_file, eval = TRUE, echo = FALSE----------------------------
out_file <- file.path(workspace_path, "usms.xml")

# Generating a new usms.xml file, for all xl_param lines
gen_usms_xml(file = out_file, param_df = xl_param)

## ----show_usms_file, eval = TRUE, echo = FALSE, class.output="xml"------------
l <- readLines(con = out_file)
cat(paste(c(l[1:24], "...", l[length(l) - 1]), collapse = "\n"))

## ----read_xl_file_sols, eval = FALSE------------------------------------------
#  # Reading the Excel file
#  soils_param <- read_params_table(usm_xl_file, sheet_name = "Soils")

## ----eval_read_xl_file_sols, eval = TRUE, echo = FALSE------------------------
library(readxl)

# Reading the Excel file
soils_param <- read_params_table(usm_xl_file, sheet_name = "Soils")

rmarkdown::paged_table(soils_param)

## ----gen_sols_file, eval = FALSE----------------------------------------------
#  
#  # Output file path
#  out_file <- "/path/to/file/sols.xml"
#  # or something like C:/path/to/file/usms.xml" for Windows
#  
#  # Generating a new sols.xml file, for all xl_param lines
#  gen_sols_xml(file = out_file, param_df = soils_param)

## ----eval_gen_sols_file, eval = TRUE, echo = FALSE----------------------------
out_file <- file.path(workspace_path, "sols.xml")

# Generating a new sols.xml file, for all xl_param lines
gen_sols_xml(file = out_file, param_df = soils_param)

## ----show_sols_file, eval = TRUE, echo = FALSE, class.output="xml"------------
l <- readLines(con = out_file)
cat(paste(c(l[1:30], "...", l[110], "...", l[length(l) - 1]), collapse = "\n"))

## ----read_xl_file_tec, eval = FALSE-------------------------------------------
#  # Reading the Excel file
#  tec_param <- read_params_table(usm_xl_file, sheet_name = "Tec")

## ----eval_read_xl_file_tec, eval = TRUE, echo = FALSE-------------------------
library(readxl)

# Reading the Excel file
tec_param <- read_params_table(usm_xl_file, sheet_name = "Tec")

rmarkdown::paged_table(tec_param)

## ----gen_tec_files, eval = FALSE----------------------------------------------
#  # *_tec.xml files, one for each xl_param line
#  gen_tec_xml(param_df = tec_param, out_dir = workspace_path)

## ----eval_gen_tec_files, eval = TRUE, echo = FALSE----------------------------

# *_tec.xml files, one for each xl_param line
gen_tec_xml(param_df = tec_param, out_dir = workspace_path)

## ----show_tec_file, eval = TRUE, echo = FALSE, class.output="xml"-------------
l <- readLines(con = file.path(workspace_path,
                              "BIN_CAN_05_SEC_220-0-0_34K_CANPC05T3_Q_tec.xml"))
cat(paste(c(l[1:30], "...", l[length(l) - 1]), collapse = "\n"))

## ----read_xl_file_ini, eval = FALSE-------------------------------------------
#  # Reading the Excel file
#  ini_param <- read_params_table(usm_xl_file, sheet_name = "Ini")

## ----eval_read_xl_file_ini, eval = TRUE, echo = FALSE-------------------------
library(readxl)

# Reading the Excel file
ini_param <- read_params_table(usm_xl_file, sheet_name = "Ini")

rmarkdown::paged_table(ini_param)

## ----gen_ini_files, eval = FALSE----------------------------------------------
#  # *_ini.xml files, one for each xl_param line
#  gen_ini_xml(param_df = ini_param, out_dir = workspace_path)

## ----eval_gen_ini_files, eval = TRUE, echo = FALSE----------------------------

# *_ini.xml files, one for each xl_param line
gen_ini_xml(param_df = ini_param, out_dir = workspace_path)

## ----show_ini_file, eval = TRUE, echo = FALSE, class.output="xml"-------------
l <- readLines(con = file.path(workspace_path, "USM_2017_T1_ini.xml"))
cat(paste(c(l[1:19], "...", l[length(l) - 1]), collapse = "\n"))

## ----read_xl_file_sta, eval = FALSE-------------------------------------------
#  # Reading the Excel file
#  sta_param <- read_params_table(usm_xl_file, sheet_name = "Station")

## ----eval_read_xl_file_sta, eval = TRUE, echo = FALSE-------------------------
library(readxl)

# Reading the Excel file
sta_param <- read_params_table(usm_xl_file, sheet_name = "Station")

rmarkdown::paged_table(sta_param)

## ----gen_sta_files, eval = FALSE----------------------------------------------
#  # *_sta.xml files, one for each xl_param line
#  gen_sta_xml(param_df = sta_param, out_dir = workspace_path)

## ----eval_gen_sta_files, eval = TRUE, echo = FALSE----------------------------

# *_sta.xml files, one for each xl_param line
gen_sta_xml(param_df = sta_param, out_dir = workspace_path)

## ----show_sta_file, eval = TRUE, echo = FALSE, class.output="xml"-------------
l <- readLines(con = file.path(workspace_path, "climatex_sta.xml"))
cat(paste(c(l[1:41], "...", l[length(l) - 1]), collapse = "\n"))

## ----gen_gen_param, eval = FALSE----------------------------------------------
#  # with the latest version
#  gen_general_param_xml(out_dir = workspace_path)
#  
#  # or with a specific version
#  gen_general_param_xml(out_dir = workspace_path, stics_version = "V10.2.0")
#  

