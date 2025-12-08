## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("devtools")

## ----install_gh, eval=FALSE---------------------------------------------------
#  devtools::install_github("tgve/tgver")

## ----version------------------------------------------------------------------
tgver::version

## ----tgve, eval=FALSE---------------------------------------------------------
#  tgver::tgve_server()

## ----embed, out.width="100%"--------------------------------------------------
# open a static tgve
# tgver::tgve()
#
# or start a tgve instance before embedding it
# tgver::tgve_server()
# knitr::include_url("http://127.0.0.1:8000")
# or use the public one
knitr::include_url("https://tgve.github.io/app/")

## ----build, eval=FALSE--------------------------------------------------------
#  # setup a local instance
#  # tempdir() will disappear inside an Rmd
#  p = "~/Downloads"
#  tgver::setup(p)
#  # now the instance is at
#  tp = file.path(p, "tgve")
#  # we just need to pass the data/csv URL
#  # using `defaultURL` API
#  url = tgver::get_url(file.path(tp, "index.html"), defaultURL = 'https://raw.githubusercontent.com/tgve/example-data/main/utlas.geojson',
#                     column = "long",
#                     hideChartGenerator="true",
#                     # In future R should assemble the json
#                     viewport="{zoom:5.5,pitch:0,bearing:0}")
#  # now we have the app's main url
#  knitr::include_url(url)
#  # unlink(tp, recursive = TRUE)

## ----buildoffline, echo=FALSE, out.width="100%"-------------------------------
if(!curl::has_internet()) {
  warning("Rmd was rendered with no connection!")
} else {
  knitr::include_graphics("https://user-images.githubusercontent.com/408568/142702067-003e98d8-a0a0-4e23-85ad-b875434da518.png")
}

## ---- out.width="100%"--------------------------------------------------------
# tgve = "https://tgve.github.io/app/?"
# defaultURL = "https://raw.githubusercontent.com/tgve/example-data/main/casualties_100.geojson"
# url = paste0(tgve, "defaultURL=", defaultURL)
# knitr::include_url(url)
# or simply
knitr::include_url("https://tgve.github.io/app/?defaultURL=https://raw.githubusercontent.com/tgve/example-data/main/casualties_100.geojson&layerName=heatmap")

