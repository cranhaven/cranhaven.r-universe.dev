## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidywikidatar)

## -----------------------------------------------------------------------------
tw_enable_cache()

## ----eval=FALSE---------------------------------------------------------------
# tw_set_cache_folder(path = fs::path(
#   fs::path_home_r(),
#   "R",
#   "tw_data"
# ))
# tw_create_cache_folder()

## ----eval = FALSE-------------------------------------------------------------
# tw_enable_cache(SQLite = FALSE)
# tw_set_cache_db(
#   driver = "MySQL",
#   host = "localhost",
#   port = 3306,
#   database = "tidywikidatar",
#   user = "secret_username",
#   pwd = "secret_password"
# )
# 
# 
# # for testing, consider running a local database e.g. with:
# # docker run --name tidywikidatar_db -p 3306:3306 -e MYSQL_ROOT_PASSWORD=secret_root_password -e MYSQL_USER=secret_username -e MYSQL_PASSWORD=secret_password -e MYSQL_DATABASE=tidywikidatar mysql:latest

## -----------------------------------------------------------------------------
tw_get_cache_table_name(type = "item", language = "en")

## ----eval=FALSE---------------------------------------------------------------
# db <- tw_connect_to_cache()
# 
# 
# tables_v <- DBI::dbListTables(conn = db)
# 
# # for search cache tables
# purrr::walk(
#   .x = tables_v[stringr::str_starts(string = tables_v, "tw_search_item")],
#   .f = function(x) {
#     tw_index_cache_search(table_name = x)
#   }
# )
# 
# # for item cache tables
# purrr::walk(
#   .x = tables_v[stringr::str_starts(string = tables_v, "tw_item")],
#   .f = function(x) {
#     tw_index_cache_item(table_name = x)
#   }
# )

