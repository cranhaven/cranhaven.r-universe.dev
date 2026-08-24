## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = RPresto::presto_has_default()
)
options(pillar.max_dec_width=20)

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

## ----primitive-types-table----------------------------------------------------
RPresto:::create_primitive_types_table(
  con, table_name = "presto_primitive_types", verbose = FALSE
)

## ----check-primitive_types-table----------------------------------------------
DBI::dbExistsTable(con, "presto_primitive_types")

## ----primitive-types-table-fields---------------------------------------------
DBI::dbListFields(con, "presto_primitive_types")

## ----boolean-type-------------------------------------------------------------
(
  df.boolean <- dbGetQuery(con, "SELECT boolean FROM presto_primitive_types")
)

## ----boolean-check------------------------------------------------------------
purrr::map_chr(df.boolean$boolean, class)

## ----non-bigint-type----------------------------------------------------------
(
  df.non_bigint_int <- dbGetQuery(
    con, "SELECT tinyint, smallint, integer FROM presto_primitive_types"
  )
)

## ----non-bigint-check---------------------------------------------------------
purrr::map_chr(df.non_bigint_int, class)

## ----non-bigint-exception, error=TRUE-----------------------------------------
try({
dbGetQuery(con, "SELECT CAST('-2147483648' AS INTEGER) AS non_bigint_exception")
})

## ----integer64-limits---------------------------------------------------------
bit64::lim.integer64()

## ----bigint-character---------------------------------------------------------
dbGetQuery(
  con, "SELECT bigint FROM presto_primitive_types", bigint = "character"
)

## ----bigint-integer64---------------------------------------------------------
dbGetQuery(
  con, "SELECT bigint FROM presto_primitive_types", bigint = "integer64"
)

## ----bigint-numeric-----------------------------------------------------------
dbGetQuery(
  con, "SELECT bigint FROM presto_primitive_types", bigint = "numeric"
)

## ----bigint-out-of-range------------------------------------------------------
dbGetQuery(
  con,
  "
  SELECT SIGN(bigint) * (ABS(bigint) + 1) AS bigint_precision_loss
  FROM presto_primitive_types
  ",
  bigint = "numeric"
)

## ----floating-point-type------------------------------------------------------
(
  df.floating_point <- dbGetQuery(
    con,
    "SELECT real, double FROM presto_primitive_types"
  )
)

## ----floating-point-check-----------------------------------------------------
purrr::map_chr(df.floating_point, class)

## ----fixed-precision-type-----------------------------------------------------
(
  df.fixed_precision <- dbGetQuery(
    con,
    "SELECT decimal FROM presto_primitive_types"
  )
)

## ----character-types----------------------------------------------------------
(
  df.characters <- dbGetQuery(
    con,
    "SELECT varchar, char FROM presto_primitive_types"
  )
)

## ----character-check----------------------------------------------------------
purrr::map_chr(df.characters, class)

## ----bytes-type---------------------------------------------------------------
(
  df.bytes <- dbGetQuery(
    con,
    "SELECT varbinary FROM presto_primitive_types"
  )
)

## ----bytes-check--------------------------------------------------------------
purrr::map_chr(df.bytes$varbinary, class)

## ----bytes-to-char------------------------------------------------------------
dplyr::mutate(df.bytes, string = purrr::map_chr(varbinary, rawToChar))

## ----date-type----------------------------------------------------------------
(
  df.date <- dbGetQuery(
    con,
    "SELECT date FROM presto_primitive_types"
  )
)

## ----date-check---------------------------------------------------------------
purrr::map_chr(df.date, class)

## ----posixct-value------------------------------------------------------------
foo <- lubridate::ymd_hms("2000-01-01 01:02:03", tz = "America/New_York")
mode(foo)
as.integer(foo)

## ----session-tz---------------------------------------------------------------
con@session.timezone

## ----posixct-type-------------------------------------------------------------
(
  df.posixct <- dbGetQuery(
    con,
    "SELECT timestamp, timestamp_with_tz FROM presto_primitive_types"
  )
)

## ----posixct-check------------------------------------------------------------
purrr::map(df.posixct, class)

## ----posixct-tz---------------------------------------------------------------
purrr::map_chr(df.posixct$timestamp, lubridate::tz)
purrr::map_chr(df.posixct$timestamp_with_tz, lubridate::tz)

## ----duration-type------------------------------------------------------------
(
  df.duration <- dbGetQuery(
    con,
    "
    SELECT
      interval_year_to_month,
      interval_day_to_second
    FROM presto_primitive_types
    "
  )
)

## ----close-db-----------------------------------------------------------------
DBI::dbDisconnect(con)

