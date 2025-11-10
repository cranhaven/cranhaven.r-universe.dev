## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(parquetize)

## ----iris-memory-example------------------------------------------------------
table_to_parquet(
  path_to_file = system.file("examples", "iris.sas7bdat", package = "haven"),
  path_to_parquet = tempfile(),
  max_memory = 5 / 1024,
  encoding = "utf-8"
)

## ----real-memory-example, eval=FALSE------------------------------------------
#    table_to_parquet(
#    path_to_file = "myhugefile.sas7bdat",
#    path_to_parquet = tempdir(),
#    max_memory = 5000,
#    encoding = "utf-8"
#  )

## ----iris-example, eval=FALSE-------------------------------------------------
#  table_to_parquet(
#    path_to_file = system.file("examples", "iris.sas7bdat", package = "haven"),
#    path_to_parquet = tempfile(),
#    max_rows = 50,
#    encoding = "utf-8"
#  )

## ----real-example, eval=FALSE-------------------------------------------------
#  table_to_parquet(
#    path_to_file = "myhugefile.sas7bdat",
#    path_to_parquet = tempdir(),
#    max_rows = 2000000,
#    encoding = "utf-8"
#  )

## ----rbind_parquet-example, eval=FALSE----------------------------------------
#  rbind_parquet(
#    folder = tempfile(),
#    output_name = "myhugefile",
#    delete_initial_files = FALSE
#  )

