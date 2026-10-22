## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(querychat)
# library(palmerpenguins)
# 
# qc <- querychat(mtcars)
# qc$app()  # Launch the app

## -----------------------------------------------------------------------------
# library(DBI)
# library(duckdb)
# library(querychat)
# 
# # Connect to a DuckDB database file
# con <- dbConnect(duckdb::duckdb(), dbdir = "my_database.duckdb")
# 
# qc <- querychat(con, "my_table")
# qc$app()  # Launch the app
# 
# # Don't forget to disconnect when done
# # dbDisconnect(con)

## -----------------------------------------------------------------------------
# library(DBI)
# library(RSQLite)
# library(querychat)
# 
# # Connect to a SQLite database file
# con <- dbConnect(RSQLite::SQLite(), "my_database.db")
# 
# qc <- querychat(con, "my_table")
# qc$app()  # Launch the app
# 
# # Don't forget to disconnect when done
# # dbDisconnect(con)

## -----------------------------------------------------------------------------
# library(DBI)
# library(RPostgres)
# library(querychat)
# 
# # Connect to PostgreSQL
# con <- dbConnect(
#   RPostgres::Postgres(),
#   host = "localhost",
#   port = 5432,
#   dbname = "mydatabase",
#   user = "myuser",
#   password = "mypassword"
# )
# 
# qc <- querychat(con, "my_table")
# qc$app()  # Launch the app
# 
# # Don't forget to disconnect when done
# # dbDisconnect(con)

## -----------------------------------------------------------------------------
# library(DBI)
# library(RMariaDB)
# library(querychat)
# 
# # Connect to MySQL
# con <- dbConnect(
#   RMariaDB::MariaDB(),
#   host = "localhost",
#   port = 3306,
#   dbname = "mydatabase",
#   user = "myuser",
#   password = "mypassword"
# )
# 
# qc <- querychat(con, "my_table")
# qc$app()  # Launch the app
# 
# # Don't forget to disconnect when done
# # dbDisconnect(con)

## -----------------------------------------------------------------------------
# library(DBI)
# library(duckdb)
# 
# con <- dbConnect(duckdb::duckdb(), dbdir = "my_database.duckdb")
# 
# # Write a data frame to the database
# dbWriteTable(con, "penguins", penguins)
# 
# # Or from CSV
# duckdb::duckdb_read_csv(con, "my_table", "path/to/your/file.csv")

## -----------------------------------------------------------------------------
# library(pins)
# library(querychat)
# 
# board <- board_connect()
# 
# qc <- querychat(board, "my_pin")
# qc$app()

## -----------------------------------------------------------------------------
# ps <- PinSource$new(board, "user.name/my_pin", table_name = "my_data")
# qc <- querychat(ps)

## -----------------------------------------------------------------------------
# library(pins)
# library(dplyr)
# library(duckdb)
# library(querychat)
# 
# board <- board_connect()
# paths <- pin_download(board, "my_pin")
# 
# con <- dbConnect(duckdb::duckdb())
# DBI::dbExecute(
#   con,
#   sprintf("CREATE VIEW my_pin AS SELECT * FROM read_parquet('%s')", paths[1])
# )
# 
# qc <- querychat(tbl(con, "my_pin"))
# qc$app()

