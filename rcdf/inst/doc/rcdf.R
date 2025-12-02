## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf(
#   data,
#   path,
#   pub_key,
#   ...,
#   metadata = list(),
#   ignore_duplicates = TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# library(rcdf)
# # Sample data (list of data frames)
# data <- rcdf_list()
# data$table1 = data.frame(x = 1:10, y = letters[1:10])
# data$table2 = data.frame(a = rnorm(10), b = rnorm(10))
# 
# # You can generate a new RSA key pair using the following function:
# key <- generate_rsa_keys(
#   path = "path/to/rsa-keys",
#   password = "1234" # this is optional
# )
# 
# # Write the data to an RCDF file
# write_rcdf(data = data, path = "path/to/rcdf_file.rcdf", pub_key = key)

## ----eval=FALSE---------------------------------------------------------------
# read_rcdf(
#   path,
#   decryption_key,
#   ...,
#   password = NULL,
#   metadata = list(),
#   ignore_duplicates = TRUE,
#   recursive = FALSE,
#   return_meta = FALSE
# )

## ----eval=FALSE---------------------------------------------------------------
# # Using sample RCDF data
# dir <- system.file("extdata", package = "rcdf")
# 
# # mtcars.rcdf is a sample RCDF file included in the package for demonstration purposes only.
# rcdf_path <- file.path(dir, 'mtcars.rcdf')
# 
# # This is also a sample key built into the package.
# private_key <- file.path(dir, 'sample-private-key.pem')
# 
# rcdf_data <- read_rcdf(path = rcdf_path, decryption_key = private_key)
# rcdf_data
# 

## ----eval=FALSE---------------------------------------------------------------
# # Using encrypted/password protected private key
# rcdf_path_pw <- file.path(dir, 'mtcars-pw.rcdf')
# private_key_pw <- file.path(dir, 'sample-private-key-pw.pem')
# pw <- '1234'
# 
# rcdf_data_with_pw <- read_rcdf(
#   path = rcdf_path_pw,
#   decryption_key = private_key_pw,
#   password = pw
# )
# rcdf_data_with_pw
# 

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_csv(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_csv(data = rcdf_data, path = "path/to/output", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_tsv(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_tsv(data = rcdf_data, path = "path/to/output", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_json(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_json(data = rcdf_data, path = "path/to/output", pretty = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_parquet(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_parquet(data = rcdf_data, path = "path/to/output")

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_xlsx(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_excel(data = rcdf_data, path = "path/to/output.xlsx", sheetName = "Sheet1")

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_dta(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_dta(data = rcdf_data, path = "path/to/output")

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_sav(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_sav(data = rcdf_data, path = "path/to/output")

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_sqlite(data, path, ..., parent_dir = NULL)

## ----eval=FALSE---------------------------------------------------------------
# write_rcdf_sqlite(data = rcdf_data, path = "path/to/output")

## ----eval=F-------------------------------------------------------------------
# write_rcdf_as(data, path, formats, ...)

