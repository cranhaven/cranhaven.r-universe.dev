## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
	eval = FALSE,
	message = FALSE,
	warning = FALSE,
	include = FALSE
)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  install.packages('geouy')

## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE------------------
#  # install.packages('devtools') si no tiene instalado devtools
#  devtools::install_github("RichDeto/geouy")

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
#  library(geouy)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  depor <- geouy::load_geouy("Instituciones deportivas")

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  nuevas <- data.frame(cbind(dpto = c("Montevideo", "Salto"),
#                             loc = c("Montevideo", "Salto"),
#                             dir = c("Cebollati esq. Magallanes",
#                                     "15 de noviembre 1310")),
#                       stringsAsFactors = F)
#  nuevas_geo <- geocode_ide_uy(nuevas)

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  depor_dep <- geouy::which_uy(depor, "Departamentos")

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  dep <- geouy::load_geouy("Departamentos")

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  dep$"instituciones_deportivas" <- lengths(sf::st_intersects(dep, depor))
#  
#  plot_geouy(dep, "instituciones_deportivas")

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
#  # devtools::install_github("RichDeto/geouy", ref = 'master');
#  library(geouy)
#  
#  # Simulamos una población
#  pop <- data.frame(x = sample(560000:585000,500),
#                    y = sample(6136000:6160000,500))
#  
#  # La categorizamos por la variable preconstruida (Pero obviamente se pueden hacer otras agregaciones)
#  pop_loc <- which_uy(pop, "Localidades pg") %>%
#    dplyr::left_join(loc_agr_ine, by = c("cod_Localidades pg" = "codloc"))
#  
#  # Confiando en que los datos están todos en territorio uruguayo, asignamos "Rural" a los NA
#  loc[is.na(pop_loc$cat_loc_agr), "cat_loc_agr"] <- "Rural"
#  
#  # y finalmente podemos hacer una tabla por estas regiones, por ejemplo:
#  tabla_regiones <- loc %>% dplyr::group_by(cat_loc_agr) %>%
#          summarise(Casos = n())

