## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(photon)

## ----eval=FALSE---------------------------------------------------------------
# opts <- cmd_options(
#   e = "PBF_URL=https://download.geofabrik.de/australia-oceania/samoa-latest.osm.pbf",
#   e = "NOMINATIM_PASSWORD=mypassword",
#   e = "FREEZE=true",
#   p = "8080:8080",
#   p = "5432:5432",
#   name = "nominatim",
#   "mediagis/nominatim:4.4",
#   use_double_hyphens = TRUE
# )
# 
# # Note: on Windows, make sure you have Docker Desktop running!
# nominatim <- process$new("docker", c("run", opts))
# 
# # Wait until Nominatim is ready
# ready <- FALSE
# while (!ready) {
#   Sys.sleep(5)
#   logs <- run("docker", c("logs", "nominatim"))
#   ready <- any(grepl("ready to accept requests", logs))
# }
# 
# run(
#   "docker",
#   c(
#     "exec", "--user", "postgres", "nominatim", "psql", "-d", "nominatim", "-c",
#     "ALTER USER nominatim WITH ENCRYPTED PASSWORD 'mypassword'"
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# library(RPostgres)
# db <- dbConnect(Postgres(), password = "MNdtC2*pP#aMbe", user = "nominatim")
# dbGetInfo(db)
# #> $dbname
# #> [1] "nominatim"
# #>
# #> $host
# #> [1] "localhost"
# #>
# #> $port
# #> [1] "5432"
# #>
# #> $username
# #> [1] "nominatim"
# #>
# #> $protocol.version
# #> [1] 3
# #>
# #> $server.version
# #> [1] 140013
# #>
# #> $db.version
# #> [1] 140013
# #>
# #> $pid
# #> [1] 604
# 
# dbDisconnect(db)

## ----eval=FALSE---------------------------------------------------------------
# dir <- file.path(tempdir(), "photon")
# photon <- new_photon(dir, overwrite = TRUE)
# #> ℹ java version "22" 2024-03-19
# #> ℹ Java(TM) SE Runtime Environment (build 22+36-2370)
# #> ℹ Java HotSpot(TM) 64-Bit Server VM (build 22+36-2370, mixed mode, sharing)
# #> ✔ Successfully downloaded photon 1.0.0. [8.2s]
# #> ℹ No search index downloaded! Download one or import from a Nominatim database.
# #> • Version: 1.0.0
# 
# photon$import(host = "localhost", password = "MNdtC2*pP#aMbe")

## ----eval=FALSE---------------------------------------------------------------
# photon$start()
# #> 2024-10-24 23:26:46,360 [main] WARN  org.elasticsearch.node.Node - version [5.6.16-SNAPSHOT] is a pre-release version of Elasticsearch and is not suitable for production
# #> ✔ Photon is now running. [11.1s]

## ----eval=FALSE---------------------------------------------------------------
# geocode("Apia", limit = 3)
# #> Simple feature collection with 3 features and 13 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -171.7631 ymin: -13.83613 xmax: -171.7512 ymax: -13.82611
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 3 × 14
# #>     idx osm_type     osm_id country osm_key city        street     countrycode osm_value name  state type  extent
# #>   <int> <chr>         <int> <chr>   <chr>   <chr>       <chr>      <chr>       <chr>     <chr> <chr> <chr> <list>
# #> 1     1 W        1322127938 Samoa   place   NA          NA         WS          city      Apia  Tuam… city  <dbl>
# #> 2     1 W         723300892 Samoa   landuse Matautu Tai NA         WS          harbour   Apia… Tuam… other <dbl>
# #> 3     1 W         666117780 Samoa   tourism Levili      Levili St… WS          attracti… Apia… Tuam… house <dbl>
# #> # ℹ 1 more variable: geometry <POINT [°]>

## ----eval=FALSE---------------------------------------------------------------
# photon$remove_data()
# photon$download_data("Andorra", json = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# photon$import(json = TRUE)

