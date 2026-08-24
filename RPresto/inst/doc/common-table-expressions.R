## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = RPresto::presto_has_default()
)
options(pillar.max_dec_width=20)

## ----setup, message=FALSE-----------------------------------------------------
library(RPresto)
library(DBI)
library(dplyr)
library(dbplyr)

## ----version------------------------------------------------------------------
packageVersion("RPresto")

## ----PrestoConnection---------------------------------------------------------
con <- DBI::dbConnect(
  drv = RPresto::Presto(),
  host = "http://localhost",
  port = 8080,
  user = Sys.getenv("USER"),
  catalog = "memory",
  schema = "default",
  # Define a testing CTE using dummy VALUES
  ctes = list(
    "dummy_values" =
      "SELECT * FROM (VALUES (1, 'a'), (2, 'b'), (3, 'c') ) AS t (id, name)"
  )
)

## ----exists_dummy_values------------------------------------------------------
db_has_table(con, "dummy_values")

## ----read_dummy_values--------------------------------------------------------
dbReadTable(con, "dummy_values")

## ----getquery_dummy_values----------------------------------------------------
dbGetQuery(con, "SELECT id * 2 AS id_2, name FROM dummy_values")

## ----setup_dplyr--------------------------------------------------------------
# We first copy mtcars to Presto and create a remote table on it
tbl.mtcars <- copy_to(con, mtcars, "test_mtcars", overwrite = TRUE)
tbl.mtcars %>% colnames()

## ----dplyr_transformation-----------------------------------------------------
tbl.mtcars.transform <- tbl.mtcars %>%
  mutate(hp2 = pow(hp, 2)) %>%
  group_by(cyl) %>%
  mutate(mean_mpg_by_cyl = mean(mpg, na.rm = TRUE))

## ----dplyr_showquery----------------------------------------------------------
tbl.mtcars.transform %>% show_query()

## ----unionall-----------------------------------------------------------------
tbl.mtcars.union <- union(
  filter(tbl.mtcars.transform, cyl == 4L),
  filter(tbl.mtcars.transform, cyl == 8L),
  all = TRUE
)
tbl.mtcars.union %>% show_query()

## ----compute_cte--------------------------------------------------------------
tbl.mtcars.transform <- tbl.mtcars.transform %>%
  compute(name = "mtcars_transform", cte = TRUE)
tbl.mtcars.transform %>% show_query()

## ----unionall_cte-------------------------------------------------------------
tbl.mtcars.union <- union(
  filter(tbl.mtcars.transform, cyl == 4L),
  filter(tbl.mtcars.transform, cyl == 8L),
  all = TRUE
)
tbl.mtcars.union %>% show_query()

## ----compute_cte_2------------------------------------------------------------
tbl.mtcars.union <- tbl.mtcars.union %>%
  compute(name = "mtcars_union", cte = TRUE)
tbl.mtcars.union %>% show_query()

## ----teardown_dplyr, echo=FALSE-----------------------------------------------
# Clean up
dbRemoveTable(con, "test_mtcars")

