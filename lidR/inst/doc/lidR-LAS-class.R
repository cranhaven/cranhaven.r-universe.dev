## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::knit_hooks$set(time_it = local({
  now <- NULL
  function(before, options) {
    if (before) {
      # record the current time before each chunk
      now <<- Sys.time()
    } else {
      # calculate the time difference after a chunk
      res <- difftime(Sys.time(), now)
      # return a character string to show the time
      #if (res > 0.1)
      #paste("<br/>========================<br/>Time for this code chunk ", options$label, " to run:", round(res,2), "<br/>========================<br/>")
    }
  }
}))
knitr::opts_chunk$set(time_it = TRUE)
#rgl::setupKnitr()
options(rmarkdown.html_vignette.check_title = FALSE)
library(lidR)

## -----------------------------------------------------------------------------
LASfile <- system.file("extdata", "example.laz", package="rlas")
las <- readLAS(LASfile)
print(las)

## -----------------------------------------------------------------------------
print(header(las))

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# las@header@PHB[["Global Encoding"]][["WKT"]] = TRUE
# 
# st_crs(las) <- 26917
# 
# # Header has been updated but users do not need to take care of that
# las@header@VLR[["WKT OGC CS"]][["WKT OGC COORDINATE SYSTEM"]]

## -----------------------------------------------------------------------------
las$Classification <- 0L

## -----------------------------------------------------------------------------
las@data$R <- 0

## ----echo = FALSE-------------------------------------------------------------
las@data$R <- NULL

## -----------------------------------------------------------------------------
las  <- add_attribute(las, 1:30, "ID")
las2 <- filter_poi(las, ID > 15)

## -----------------------------------------------------------------------------
las  <- add_lasattribute(las, 1:30, "ID", "An ID for each point")

## -----------------------------------------------------------------------------
las_check(las)

## ----eval = FALSE, echo = FALSE, rgl=TRUE, dev='png'--------------------------
# LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
# las = readLAS(LASfile)
# m = structure(c(0.921, -0.146, 0.362, 0, 0.386, 0.482, -0.787, 0,
# -0.06, 0.864, 0.5, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
# plot(las)
# rgl::view3d(fov = 50, userMatrix = m)

