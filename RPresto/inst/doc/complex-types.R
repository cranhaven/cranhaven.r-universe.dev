## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = RPresto::presto_has_default()
)

## ----setup--------------------------------------------------------------------
library(RPresto)

## ----version------------------------------------------------------------------
packageVersion("RPresto")

## ----PrestoConnection---------------------------------------------------------
con <- DBI::dbConnect(
  drv = RPresto::Presto(),
  host = "http://localhost",
  port = 8080,
  user = Sys.getenv("USER"),
  catalog = "memory",
  schema = "default"
)

## ----test---------------------------------------------------------------------
DBI::dbGetQuery(con, "SELECT 1+1 AS res")

## ----primitive-arrays-table---------------------------------------------------
RPresto:::create_primitive_arrays_table(
  con, table_name = "presto_primitive_arrays", verbose = FALSE
)

## ----check-primitive_arrays-table---------------------------------------------
DBI::dbExistsTable(con, "presto_primitive_arrays")

## ----primitive-arrays-table-fields--------------------------------------------
DBI::dbListFields(con, "presto_primitive_arrays")

## ----array-of-primitive-types-------------------------------------------------
(
  df.array_of_primitive_types <- dbGetQuery(
    con,
    "SELECT * FROM presto_primitive_arrays",
    bigint = "integer64"
  )
)

## ----array-of-primitive-type-check--------------------------------------------
tibble::enframe(purrr::map_chr(df.array_of_primitive_types, ~class(.[[1]])[1]))

## ----array-of-primitive-name-check--------------------------------------------
purrr::every(df.array_of_primitive_types, ~is.null(names(.[[1]])))

## ----array-cardinality--------------------------------------------------------
tibble::enframe(purrr::map_int(df.array_of_primitive_types, ~length(.[[1]])))

## ----primitive-maps-table-----------------------------------------------------
RPresto:::create_primitive_maps_table(
  con, table_name = "presto_primitive_maps", verbose = FALSE
)

## ----check-primitive_maps-table-----------------------------------------------
DBI::dbExistsTable(con, "presto_primitive_maps")

## ----primitive-maps-table-fields----------------------------------------------
DBI::dbListFields(con, "presto_primitive_maps")

## ----map-of-primitive-types---------------------------------------------------
(
  df.map_of_primitive_types <- dbGetQuery(
    con,
    "SELECT * FROM presto_primitive_maps",
    bigint = "integer64"
  )
)

## ----map-of-primitive-type-check----------------------------------------------
tibble::enframe(purrr::map_chr(df.map_of_primitive_types, ~class(.[[1]])[1]))

## ----map-of-primitive-name-check----------------------------------------------
purrr::none(df.map_of_primitive_types, ~is.null(names(.[[1]])))

## ----array-of-maps-table------------------------------------------------------
RPresto:::create_array_of_maps_table(
  con, table_name = "presto_array_of_maps", verbose = FALSE
)

## ----check-array-of-maps-table------------------------------------------------
DBI::dbExistsTable(con, "presto_array_of_maps")

## ----array-of-maps-table-fields-----------------------------------------------
DBI::dbListFields(con, "presto_array_of_maps")

## ----array-of-maps------------------------------------------------------------
(
  df.array_of_maps <- dbGetQuery(
    con,
    "SELECT * FROM presto_array_of_maps",
    bigint = "integer64"
  )
)

## ----array-of-map-check-------------------------------------------------------
tibble::enframe(purrr::map_chr(df.array_of_maps, ~class(.[[1]][[1]])[1]))

## ----primitive-rows-table-----------------------------------------------------
RPresto:::create_primitive_rows_table(
  con, table_name = "presto_primitive_rows", verbose = FALSE
)

## ----check-primitive_rows-table-----------------------------------------------
DBI::dbExistsTable(con, "presto_primitive_rows")

## ----primitive-rows-table-fields----------------------------------------------
DBI::dbListFields(con, "presto_primitive_rows")

## ----row-of-primitive---------------------------------------------------------
(
  df.row_of_primitive <- dbGetQuery(
    con,
    "SELECT row_primitive_types FROM presto_primitive_rows",
    bigint = "integer64"
  )
)

## ----row-of-primitive-check---------------------------------------------------
tibble::enframe(
  purrr::map_chr(df.row_of_primitive$row_primitive_types[[1]], ~class(.)[1])
)

## ----arrays-of-rows-table-----------------------------------------------------
RPresto:::create_array_of_rows_table(
  con, table_name = "presto_array_of_rows", verbose = FALSE
)

## ----check-array-of-rows-table------------------------------------------------
DBI::dbExistsTable(con, "presto_array_of_rows")

## ----array-of-rows-table-fields-----------------------------------------------
DBI::dbListFields(con, "presto_array_of_rows")

## ----array-of-rows------------------------------------------------------------
(
  df.array_of_rows <- dbGetQuery(
    con,
    "SELECT array_of_rows FROM presto_array_of_rows",
    bigint = "integer64"
  )
)

## ----array-of-row-check-------------------------------------------------------
tibble::enframe(
  purrr::map_chr(df.array_of_rows$array_of_rows[[1]], ~class(.)[1])
)

## ----close-db-----------------------------------------------------------------
DBI::dbDisconnect(con)

