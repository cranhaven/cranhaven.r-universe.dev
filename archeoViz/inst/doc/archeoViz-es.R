## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # configure el directorio de trabajo en su servidor Shiny:
#  setwd(dir = "/some/path/")
#  # descargar el package:
#  download.file(url = "https://github.com/sebastien-plutniak/archeoviz/archive/master.zip",
#                destfile = "archeoviz.zip")
#  # unzip:
#  unzip(zipfile = "archeoviz.zip")

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(objects.df = NULL,   # data.frame con datos sobre los objetos
#            refits.df = NULL,    # data.frame opcional para reinstalar datos
#            timeline.df = NULL,  # data.frame opcional para la cronología de excavación
#            default.group =NULL, # méthode de groupement des données,
#                                 # par couche ("by.layer") ou "by.variable"
#            title = NULL,        # titulo del sitio / data set
#            home.text = NULL,    # Contenido HTML para mostrar en la página de inicio
#            lang = "fr"          # lenguaje de interfaz ("en": Inglés, "fr": Francés, "it": Italiano, "pt": Portugués, "es": Español)
#            set.theme = "cosmo") # tema gráfico para la interfaz Shiny

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz()

## ----eval=FALSE---------------------------------------------------------------
#  archeoViz(objects.df = NULL,  # data.frame con datos sobre los objetos
#            refits.df = NULL,   # data.frame para reacondicionar objetos
#            timeline.df = NULL) # data.frame opcional para la cronología de excavación

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

