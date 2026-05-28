## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  library(rsat)
#  set_credentials("rsat.package","UpnaSSG.2021")

## ----search_review------------------------------------------------------------
#  ip <- st_sf(st_as_sfc(st_bbox(c(
#    xmin = -9.755859,
#    xmax =  4.746094,
#    ymin = 35.91557,
#    ymax = 44.02201
#  ), crs = 4326)))
#  toi <- seq(as.Date("2021-01-10"),as.Date("2021-01-15"),1)

## -----------------------------------------------------------------------------
#  db.path <- file.path(tempdir(),"database")
#  ds.path <- file.path(tempdir(),"datasets")
#  dir.create(db.path)
#  dir.create(ds.path)

## -----------------------------------------------------------------------------
#  filomena <- new_rtoi(name = "filomena",
#                       region = ip,
#                       db_path = db.path,
#                       rtoi_path = ds.path)

## -----------------------------------------------------------------------------
#  rsat_search(region = filomena, product = c("mod09ga"), dates = toi)

## ----download_rtoi------------------------------------------------------------
#  rsat_download(filomena)

## ----download_database--------------------------------------------------------
#  list.files(get_database(filomena), recursive = TRUE)

## ----download_records---------------------------------------------------------
#  rsat_download(records(filomena), out.dir = get_database(filomena))

