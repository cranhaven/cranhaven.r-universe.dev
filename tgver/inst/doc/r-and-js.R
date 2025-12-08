## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data-prep----------------------------------------------------------------
library(igraph)
library(edgebundle)
library(ggplot2)
library(ggraph)

# this environment variable would return true if run on
# github actions therefore, we can stop running TGVE if so.
# See GitHub docs: https://docs.github.com/en/actions/learn-github-actions/environment-variables
is.actions = Sys.getenv("GITHUB_ACTIONS") != ""

g = us_flights
xy = cbind(V(g)$longitude, V(g)$latitude)
verts = data.frame(x = V(g)$longitude, y = V(g)$latitude)

states = map_data("state")

pbundle = edge_bundle_path(g, xy, max_distortion = 12, weight_fac = 2, segments = 50)
# caching the heavy processing
# saveRDS(pbundle, "vignettes/pbundle.Rds")
# pbundle = readRDS("pbundle.Rds")

# edge list
el = get.edgelist(g)
colnames(el) = c("from", "to")

# get names of airports in verts
verts.with.names = data.frame(x = V(g)$longitude, y = V(g)$latitude, V(g)$name)
colnames(verts.with.names) = c("x", "y", "name")
# intermediate
from = verts.with.names[match(el[,"from"], verts.with.names$name), c("x", "y")]
to = verts.with.names[match(el[,"to"], verts.with.names$name), c("x", "y")]
# create matrix for sf
m = cbind(from, to)
library(sf)
sfc = lapply(1:nrow(m), function(x) st_linestring(matrix(unlist(m[x,]), ncol = 2, byrow = TRUE)))
sfc = st_sfc(sfc, crs = 4326)

## ----plot-no-bundling, out.width='90%'----------------------------------------
plot(sfc)

## ----ggplot-no-bundle, out.width='90%'----------------------------------------
ggplot() +
  geom_path(data = pbundle, aes(x, y, group = group),
            col = "orange", size = 0.05) +
  geom_path(data = pbundle, aes(x, y, group = group),
            col = "white", size = 0.005) +
  labs(title = "Edge-Path Bundling") +
  ggraph::theme_graph(background = "black") +
  theme(plot.title = element_text(color = "white"))


## ----tgve1, out.width='100%'--------------------------------------------------
library(tgver)
p = getwd() # for reproducibilty this needs to be a persistent location
tp = file.path(p, "tgve")
unlink(tp, recursive = TRUE)
# some random line width
# df = data.frame(lw=runif(length(sfc), min=1, max=5))
run_tgve = function(layerName="line") {
  setup(p)
  sf = st_as_sf(sfc)
  p = explore_sf(sf, static = TRUE, path = tp)
  knitr::include_url(file.path(p, "index.html"))
}

img_or_warning = function(img.url) {
  if(!curl::has_internet()) {
    warning("Rmd was rendered with no connection!")
  } else {
    paste0("<img src='", img.url, "' />")
  }
}
if(!is.actions) {
  run_tgve()
} else {
  img_or_warning("https://user-images.githubusercontent.com/408568/144712831-7d2aec72-0af4-4ca9-b2e0-2316d7533753.png")
}

## ----tgve2, out.width='100%'--------------------------------------------------
# convert the pbundle into sf
sfc = lapply(1:length(unique(pbundle$group)), function(x) st_linestring(matrix(unlist(pbundle[pbundle$group == x, 1:2]), ncol = 2)))
sfc = st_sfc(sfc, crs = 4326)
# plot(sfc)
p = file.path(p, "edge-tgve")
tp = file.path(p, "tgve")
unlink(tp, recursive = TRUE)

if(!is.actions) {
  run_tgve(layerName = "path")
} else {
  img_or_warning("https://user-images.githubusercontent.com/408568/144713110-245cf94e-826d-4525-bc48-ac7c97220c71.png")
}

## ---- eval=FALSE--------------------------------------------------------------
#  unlink(file.path(getwd(), "tgve"), recursive = TRUE)
#  unlink(file.path(getwd(), "edge-tgve"), recursive = TRUE)

## ---- data-prep2, eval=FALSE--------------------------------------------------
#  dir = file.path(tempdir(), "cdrc")
#  dir.create(dir)
#  
#  # download the csv data which is behind authorization from cdrc
#  # https://data.cdrc.ac.uk/system/files/c1_english_imd_2019_rebased_for_london.csv
#  # via https://data.cdrc.ac.uk/dataset/index-multiple-deprivation-imd
#  # renamed data.csv
#  csv = read.csv(file.path(dir, "data.csv"))
#  
#  # download the geography (a .geojson file for our purposes using sf)
#  # from here https://github.com/gausie/LSOA-2011-GeoJSON
#  library(sf)
#  geo = st_read("https://raw.githubusercontent.com/gausie/LSOA-2011-GeoJSON/master/lsoa.geojson")
#  
#  # filter it to those in the CSV file
#  m = match(csv$ls11cd, geo$LSOA11CD)
#  # notice the different column names for the same geography codes
#  geo = geo[m, ]
#  # rename column as TGVE package is young
#  colnames(geo)[1] = "ls11cd" # same as column one in CSV
#  
#  # make sure both files are in the ~/Downloads/cdrc directory
#  # named lsoa.geojson
#  st_write(geo, file.path(dir, "lsoa.geojson"))
#  
#  # Finally, let TGVE serve it!
#  tgver::explore_dir(dir)
#  

## ---- out.width='100%'--------------------------------------------------------
knitr::include_url(
  paste0("https://tgve.github.io/app/?",
         "defaultURL=https://raw.githubusercontent.com",
         "/tgve/example-data/main/casualties_100.geojson&layerName=heatmap")
)

## ---- eval=FALSE--------------------------------------------------------------
#  # same URL as the instance above
#  knitr::include_url(
#    paste0(
#      # use the 1data1 parameter
#      "https://tgve.github.io/app?data=",
#      gj # see above
#    )
#  )

