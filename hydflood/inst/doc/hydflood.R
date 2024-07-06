## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align="center",
    fig.width = 7,
    fig.height = 4, 
    root.dir = "vignettes"
)

base_hyd1d <- "https://hyd1d.bafg.de/"

## ----captions, echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, include = FALSE----
# set english locale to produce english plot labels
Sys.setlocale(category = "LC_ALL", locale = "en_US.utf8")

# standard library path for the installed local packages
options("hyd1d.datadir" = tempdir())
library(hyd1d)
options("hydflood.datadir" = tempdir())
library(hydflood)
library(stringr)

# Determine the output format of the document
outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (outputFormat == "html") {
    is_html <- TRUE
} else {
    is_html <- FALSE
}

# Figure and Table Caption Numbering, for HTML do it manually
capTabNo <- 1
capFigNo <- 1

# Function to add the Table Number
capTab <- function(x){
    if(outputFormat == 'html'){
        x <- paste0("**Tab. ", capTabNo, "**: ", x)
        capTabNo <<- capTabNo + 1
    } else if (outputFormat == 'latex'){
        y <- str_replace_all(x, '(^.*)(\\[.*\\])(\\(.*\\))(.*$)', 
                             '\\1\\\\href{\\3}{\\2}\\4')
        y <- gsub("{(", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("{[", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub(")}", "}", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("]}", "}", y, fixed = TRUE, useBytes = TRUE)
        x <- gsub("_", "\\_", y, fixed = TRUE, useBytes = TRUE)
    }
    return(x)
}

# Function to add the Figure Number
capFig <- function(x){
    if(outputFormat == 'html'){
        x <- paste0("**Fig. ", capFigNo, "**: ", x)
        capFigNo <<- capFigNo + 1
    } else if (outputFormat == 'latex'){
        y <- str_replace_all(x, '(^.*)(\\[.*\\])(\\(.*\\))(.*$)', 
                             '\\1\\\\href{\\3}{\\2}\\4')
        y <- gsub("{(", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("{[", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub(")}", "}", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("]}", "}", y, fixed = TRUE, useBytes = TRUE)
        x <- gsub("_", "\\_", y, fixed = TRUE, useBytes = TRUE)
    }
    return(x)
}

href <- function(x, y) {
    if (outputFormat == 'html') {
        x <- paste0("[", x, "](", y, ")")
    } else if (outputFormat == 'latex') {
        x <- paste0("\\href{", y, "}{", x, "}")
    }
    return(x)
}

bf <- function(x) {
    if (outputFormat == 'html') {
        x <- paste0("**", x, "**")
    } else if (outputFormat == 'latex') {
        x <- paste0("\\textbf{", x, "}")
    }
    return(x)
}

## ----install-cran, eval = FALSE-----------------------------------------------
#  install.packages("hydflood")

## ----install-git, eval = FALSE------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github("bafg-bund/hydflood")

## ----library-print, eval = FALSE, echo = TRUE---------------------------------
#  options("hydflood.datadir" = tempdir())
#  library(hydflood)

## ----figure-dem, fig.show = 'asis', fig.cap = capFig("Digital elevation model of the waterway (DEM-W, in German: Digitales Geländemodell des Wasserlaufs, DGM-W) with 1 m spatial resolution at the River Elbe near Rosslau and Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, fig.show = 'asis', out.width = "95%", fig.pos="H", fig.align = "center"----
knitr::include_graphics('./screenshot_hydflood_dem.png')

## ----figure-cs, fig.show = 'asis', fig.cap = capFig("Cross sections produced to gather input data for SOBEK models used in [FLYS](https://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html) at the River Elbe near Rosslau and Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, fig.show = 'asis', out.width = "95%", fig.pos="H", fig.align = "center"----
knitr::include_graphics('./screenshot_hydflood_crosssections.png')

## ----figure-csa, fig.show = 'asis', fig.cap = capFig("Cross section areas derived from the cross sections illustrated in Fig. 2 at the River Elbe near Rosslau and Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, fig.show = 'asis', out.width = "95%", fig.pos="H", fig.align = "center"----
knitr::include_graphics('./screenshot_hydflood_crosssectionareas.png')

## ----figure-csacs, fig.show = 'asis', fig.cap = capFig("Cross section areas illustrated in Fig. 3 overlaid by the corresponding cross sections illustrated in Fig. 2 at the River Elbe near Rosslau and Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, fig.show = 'asis', out.width = "95%", fig.pos="H", fig.align = "center"----
knitr::include_graphics('./screenshot_hydflood_crosssectionareas_cs.png')

## ----init, eval = FALSE, error = FALSE, warning = FALSE, message = FALSE------
#  # import the raster data and create a raster stack
#  x <- hydSpatRaster(filename_dem = "data-raw/raster.dem.tif",
#                     filename_csa = "data-raw/raster.csa.tif")

## ----seq, eval = FALSE, error = FALSE, warning = FALSE, message = FALSE-------
#  seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")

## ----usage, eval = FALSE, error = FALSE, warning = FALSE, message = FALSE-----
#  # compute a flood duration
#  fd <- flood3(x = x, seq = seq)
#  
#  # and plot it
#  plot(fd)

## ----link-flood3daily, eval = is_html, echo = FALSE, results = 'asis'---------
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/flood3daily/" target="_blank">https://shiny.bafg.de/flood3daily/</a></p>')

## ----figure1, echo = FALSE, fig.cap = capFig(paste0("Screenshot of the ", href("flood3daily-ShinyApp", "https://shiny.bafg.de/flood3daily/"), " with the modelled flood extent computed for 2016-12-21 at the River Elbe between Rosslau and Dessau, Germany.")), fig.show = 'asis', out.width = "100%"----
knitr::include_graphics('screenshot_flood3daily.png')

## ----link-waterlevelpegelonline, eval = is_html, echo = FALSE, results = 'asis'----
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/flood3wms/" target="_blank">https://shiny.bafg.de/flood3wms/</a></p>')

## ----figure2, echo = FALSE, fig.cap = capFig(paste0("Screenshot of the ", href("flood3wms-ShinyApp", "https://shiny.bafg.de/flood3wms/"), " with the annual flood duration of 2016 at the River Elbe between Rosslau and Dessau, Germany.")), fig.show = 'asis', out.width = "100%"----
knitr::include_graphics('screenshot_flood3wms.png')

