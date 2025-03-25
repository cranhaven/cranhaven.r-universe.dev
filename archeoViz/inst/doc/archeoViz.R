## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # set the working directory on your shiny server:
#  setwd(dir = "/some/path/")
#  # download the package:
#  download.file(
#    url = "https://github.com/sebastien-plutniak/archeoviz/archive/master.zip",
#    destfile = "archeoviz.zip")
#  # unzip it:
#  unzip(zipfile = "archeoviz.zip")

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(objects.df = NULL,   # data.frame with data about the objects
#            refits.df = NULL,    # optional data.frame for refitting data
#            timeline.df = NULL,  # optional data.frame for the excavation timeline
#            title = NULL,        # title of the site / data set
#            home.text = NULL,    # HTML content to display on the home page
#            lang = "en"          # interface language ("de": German, "en": English, "fr": French, "it": Italian, "pt": Portuguese, "es": Spanish)
#            set.theme = "cosmo") # graphic theme for the Shiny interface

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz()

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(objects.df = NULL,  # data.frame with data about the objects
#            refits.df = NULL,   # data.frame for refitting objects
#            timeline.df = NULL) # optional data.frame for the excavation timeline

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(objects.df=NULL, refits.df=NULL, timeline.df=NULL,
#            title=NULL, home.text=NULL, lang="en", set.theme="cosmo",
#            square.size = 100, unit = "cm", rotation = 0,
#            grid.orientation = NULL, background.map = NULL,
#            reverse.axis.values = NULL, reverse.square.names = NULL,
#            add.x.square.labels = NULL, add.y.square.labels = NULL,
#            class.variable = NULL, class.values = NULL,
#            default.group = "by.layer", location.mode = NULL,
#            map.z.val = NULL, map.density = "no", map.refits = NULL,
#            plot3d.ratio = 1, plot3d.hulls = FALSE, hulls.class.values = NULL,
#            plot3d.surfaces = NULL, plot3d.refits = NULL, point.size = 2,
#            sectionX.x.val = NULL, sectionX.y.val = NULL, sectionX.refits = NULL,
#            sectionY.x.val = NULL, sectionY.y.val = NULL, sectionY.refits = NULL,
#            camera.center = c(0, 0, 0), camera.eye = c(1.25, 1.25, 1.25),
#            run.plots = FALSE, html.export = TRUE, table.export = TRUE
#            )

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(square.size = 100, unit = "cm", rotation = 0,
#            grid.orientation = NULL, background.map = NULL,
#            reverse.axis.values = NULL, reverse.square.names = NULL,
#            add.x.square.labels = NULL, add.y.square.labels = NULL
#            )

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(class.variable = NULL, class.values = NULL,
#            default.group = "by.layer", location.mode = NULL,
#            map.z.val = NULL, map.density = "no", map.refits = NULL,
#            plot3d.hulls = NULL, plot3d.surfaces = NULL, plot3d.refits = NULL,
#            sectionX.x.val = NULL, sectionX.y.val = NULL, sectionX.refits = NULL,
#            sectionY.x.val = NULL, sectionY.y.val = NULL, sectionY.refits = NULL,
#            camera.center = NULL, camera.eye = NULL
#            )

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(run.plots = FALSE)

