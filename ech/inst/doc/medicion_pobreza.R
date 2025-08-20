## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 400
)

options(rmarkdown.html_vignette.check_title = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  library(ech)
#  # cargamos la base de ECH y estandarizamos los nombres de variables
#  df <- get_microdata(year = 2019, folder = tempdir())
#  #df <- organize_names(df, year = 2019)
#  
#  # calculamos las variables de pobreza monteria
#  df <- poverty(data = df)
#  
#  # Calculamos las variables de NBI
#  df <- enrolled_school(df)
#  df <- years_of_schooling(df)
#  df <- unsatisfied_basic_needs(data = df)
#  
#  # calculamos la variable metodo integrado
#  df <- integrated_poverty_measure(data = df)

## ---- eval = FALSE------------------------------------------------------------
#  # Distribución de hogares según cantidad de NBI que presenta
#  nbi_hogares <- get_estimation_mean(df, variable = "UBN", level = "h", ids = "upm", estrato = "estrato")
#  
#  # Distribución de personas según cantidad de NBI que presenta el hogar
#  nbi_personas <- get_estimation_mean(df, variable = "UBN", level = "i", ids = "upm", estrato = "estrato")
#  
#  # Hogares según situación de pobreza
#  pobres_hogares <- get_estimation_mean(df, variable = "poor", level = "h", ids = "upm", estrato = "estrato")
#  
#  # Personas según situación de pobreza
#  pobres_personas <- get_estimation_mean(df, variable = "poor", level = "i", ids = "upm", estrato = "estrato")
#  
#  # Hogares según situación de pobreza
#  ipm_hogares <- get_estimation_mean(df, variable = "integrated_poverty_measure", level = "h", ids = "upm", estrato = "estrato")
#  

## ---- eval = FALSE------------------------------------------------------------
#  nbi_hogares

## ---- eval = FALSE------------------------------------------------------------
#  nbi_personas

## ---- eval = FALSE------------------------------------------------------------
#  pobres_hogares

## ---- eval = FALSE------------------------------------------------------------
#  pobres_personas

## ---- eval = FALSE------------------------------------------------------------
#  ipm_hogares

## ---- eval = FALSE------------------------------------------------------------
#  df <- poverty(data = df)
#  
#  # Hogares según situación de pobreza
#  pobres_hogares_region <- get_estimation_mean(df, variable = "poor", by.x = "region_3",level = "h", ids = "upm", estrato = "estrato")
#  pobres_hogares_region %>% filter(poor == "Pobre")
#  
#  # Personas según situación de pobreza
#  pobres_personas_region <- get_estimation_mean(df, variable = "poor", by.x = "region_3", level = "i", ids = "upm", estrato = "estrato")
#  pobres_personas_region %>% filter(poor == "Pobre")

