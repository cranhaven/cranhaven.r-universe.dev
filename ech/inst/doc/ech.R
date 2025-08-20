## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
	eval = FALSE,
	message = FALSE,
	warning = FALSE,
	include = FALSE,
  dpi = 400
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  # install.packages('devtools')
#  # si no tiene instalado devtools
#  
#  devtools::install_github("calcita/ech")
#  library(ech)

## ----eval = FALSE-------------------------------------------------------------
#  # Cargo la libreria
#  library(ech)
#  
#  # Cargo la base
#  ech19 <- get_microdata(year = "2019", # Año/s a descargar (2011-2019)
#                folder = tempdir(), # Carpeta para descarga
#                toR = FALSE) # No lo guarda en formato RData

## ----eval = FALSE-------------------------------------------------------------
#  # Organizamos nombres de variables
#  # para el caso de 2019 no es necesario porque el diccionario base es el 2017 y las variables se llaman igual
#  #ech19 <- organize_names(ech19, 2019)
#  ech19 <- ech19 %>% dplyr::rename(upm = upm_fic)

## ----eval = FALSE-------------------------------------------------------------
#  # Calculamos variables de empleo
#  
#  ech19 <- employment(data = ech19)
#  

## ----eval = FALSE-------------------------------------------------------------
#  # Organizamos nombres de variables
#  
#  ech19 <- income_constant_prices(data = ech19, # Data.frame de una ech
#                                  base_month = 6, # Mes base
#                                  base_year = 2019, # Año base
#                                  index = "IPC", # Indice
#                                  level = "G") # Puede ser General ("G") o Regional ("R")

## -----------------------------------------------------------------------------
#  # Genero una estimación:
#  
#  pobre_x_dpto <- get_estimation_mean(data = ech19, # Indico el data.frame
#                             variable = "pobre06", # La variable a estimar
#                             by.x = "nomdpto", # La variable de agrupación
#                             by.y = NULL, # Se permite otra variable de agrupación
#                             domain = NULL, # Se podría indicar un dominio
#                             level = "h", # Defino que lo haga a nivel de hogar
#                             name = "Pobreza")
#  

## -----------------------------------------------------------------------------
#  # Solo me quedo con la estimación de pobre
#  
#  pobre_x_dpto <- pobre_x_dpto %>% filter(pobre06 == "Pobre")
#  
#  # Agrego geometrías
#  
#  pobre_x_dpto_geo <- add_geom(data = pobre_x_dpto, # Los datos en una unidad geográfica de entre las opciones
#                              unit = "Departamentos", # Unidad de agregación de los datos
#                              variable = "nomdpto") # Variable correspondiente a los códigos a la unidad
#  
#  

## -----------------------------------------------------------------------------
#  # Hago un mapa
#  
#  plot_geouy(x = pobre_x_dpto_geo, col = "Pobreza", l = "%")
#  

## ----echo=FALSE, fig.align="center", message=FALSE, warning=FALSE-------------
#  knitr::include_graphics("man/figures/pobre_readme.png")

