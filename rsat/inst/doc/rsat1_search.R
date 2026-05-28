## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----credentials_individual---------------------------------------------------
#  library(rsat)
#  set_credentials("rsat.package","UpnaSSG.2021", "scihub")
#  set_credentials("rsat.package","UpnaSSG.2021", "earthdata")

## ----credentials_print--------------------------------------------------------
#  print_credentials()

## ----credentials_simultaneous-------------------------------------------------
#  set_credentials("rsat.package","UpnaSSG.2021")

## ----records_search-----------------------------------------------------------
#  data("ex.navarre")
#  toi <- as.Date("2021-01-01") + 0:15
#  rcd <- rsat_search(region = ex.navarre,
#                     product = c("mod09ga"),
#                     dates = toi)

## ----records_search_multiple--------------------------------------------------
#  rcd <- rsat_search(region = ex.navarre,
#                     product = c("mod09ga", "SY_2_SYN___"),
#                     dates = toi)
#  class(rcd)

## ----records_slots------------------------------------------------------------
#  unique(sat_name(rcd))
#  names(rcd)[1]
#  unique(dates(rcd))

## ----records_filter_basic-----------------------------------------------------
#  mod.rcd <- subset(rcd, "sat", "Modis")
#  sn3.rcd <- subset(rcd, "sat", "Sentinel-3")
#  length(mod.rcd)
#  length(sn3.rcd)

## ----records_filter_advanced--------------------------------------------------
#  mod.mtch <- mod.rcd[dates(mod.rcd) %in% dates(sn3.rcd)]
#  sn3.mtch <- sn3.rcd[dates(sn3.rcd) %in% dates(mod.rcd)]
#  rcd <- c(mod.mtch, sn3.mtch)

## ----records_dataframe--------------------------------------------------------
#  rcd.df <- as.data.frame(rcd)
#  dim(rcd.df)
#  names(rcd.df)
#  # rcd <- as.records(rcd.df)
#  # class(rcd)

## -----------------------------------------------------------------------------
#  prview <- plot(rcd[1:12])
#  prview
#  rcd <- rcd[-9]

## -----------------------------------------------------------------------------
#  plot(rcd[1:6],
#       region = ex.navarre,
#       tm.polygon.region.border.col = "red",
#       tm.polygon.region.alpha = 0,
#       compass.rm = T,
#       scale.bar.rm = T)

## ----roi_toi------------------------------------------------------------------
#  ip <- st_sf(st_as_sfc(st_bbox(c(
#    xmin = -9.755859,
#    xmax =  4.746094,
#    ymin = 35.91557,
#    ymax = 44.02201
#  ), crs = 4326)))
#  
#  toi <- seq(as.Date("2021-01-10"),as.Date("2021-01-15"),1)

## ----database_dataset---------------------------------------------------------
#  db.path <- file.path(tempdir(),"database")
#  ds.path <- file.path(tempdir(),"datasets")
#  dir.create(db.path)
#  dir.create(ds.path)

## ----rtoi_search--------------------------------------------------------------
#  filomena <- new_rtoi(name = "filomena",
#                       region = ip,
#                       db_path = db.path,
#                       rtoi_path = ds.path)

## ----rtoi_file----------------------------------------------------------------
#  rtoi.files <- list.files(file.path(ds.path, "filomena"), full.name = TRUE)
#  rtoi.files

## ----rtoi_read----------------------------------------------------------------
#  filomena <- read_rtoi(file.path(ds.path, "filomena"))

## ----rtoi_print---------------------------------------------------------------
#  print(filomena)

## ----rtoi_search_2------------------------------------------------------------
#  toi <- as.Date("2021-01-10") + 0:5
#  rsat_search(region = filomena,
#              product = c("mod09ga", "SY_2_SYN___"),
#              dates = toi)

## ----rtoi_r_update------------------------------------------------------------
#  print(filomena)

## ----rtoi_file_update---------------------------------------------------------
#  file.info(rtoi.files[1])$ctime # creation time
#  file.info(rtoi.files[1])$mtime # modification time

## ----rtoi_records-------------------------------------------------------------
#  rcds <- records(filomena)
#  class(rcds)

## ----rtoi_preview-------------------------------------------------------------
#  plot(filomena,
#       "preview",
#       product = "mod09ga",
#       dates = "2021-01-11")

## ----rtoi_calendar------------------------------------------------------------
#  plot(filomena, "dates")

## ----rtoi_lightening----------------------------------------------------------
#  rcd <- records(filomena)
#  records(filomena) <- subset(rcd, "product", "mod09ga")

