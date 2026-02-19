## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE, eval=FALSE, results='hold', warning=FALSE, include=TRUE----
#  library(pannotator)
#  
#  options(shiny.port = httpuv::randomPort(), shiny.launch.browser = .rs.invokeShinyWindowExternal)
#  
#  run_app()

## ----echo=TRUE, eval=FALSE, results='hold', warning=FALSE, include=TRUE-------
#  
#  library(dplyr)
#  library(mapview)
#  library(RColorBrewer)
#  library(sf)
#  
#  
#  df_annotation <- readRDS("C:/user_1_annotations.rds") # read in the .rds file
#  df_annotation <- st_as_sf(df_annotation, wkt = "geometry",crs = 4326) #define
#  #the geometry
#  
#  df_annotation$dd2 <- as.numeric(df_annotation$dd2) # dd2 = -999 where crowns
#  # have not been assessed for health (NA); range = 0 for no live leaves (entirely
#  # dead) to 100 for entire crown healthy with green leaves
#  
#  df_annotation <- subset(df_annotation, dd2 > -1 ) # select only records where
#  # Allocasuarina crowns have been assessed for health; that is, excluding NA records
#  
#  
#  mapviewOptions(basemaps = c("Esri.WorldImagery"),
#                 vector.palette = colorRampPalette(c("red","orange", "yellow", "green")),
#                 layers.control.pos = "topright")
#  
#  
#  mapview(df_annotation, zcol = "dd2 ", na.rm = TRUE)
#  

## ----echo=TRUE, eval=FALSE, results='hold', warning=FALSE, include=TRUE-------
#  #read in the species data file#
#  
#  species_data <- read_csv("Calibration_species.csv", show_col_types = FALSE)
#  
#  # confirm that there are 79 plots of species data
#  cat("The number of rows in the dataframe is", nrow(species_data), "\n")
#  
#  
#  # now determine the relationship between plot-level species counts in the field survey versus camera survey counts. abline adds a linear model to the plot   #
#  
#  plot(species_data$No_Field_species, species_data$No_Camera_species,
#       main = "Plot-level species richness",
#       xlab = "No. of species (Field Survey)",
#       ylab = "No. of species (Camera Survey)",
#       pch = 16,  # Use filled circles as data points
#       col = "black",  # Set point color
#       ylim = c(0, 8),  # Set y-axis limits
#       xlim = c(0, 8))  # Set x-axis limits
#  abline(lm(No_Camera_species ~ No_Field_species, data = species_data), col = "red")
#  
#  # view the linear model statistics #
#  model <- lm(No_Camera_species ~ No_Field_species, data = species_data)
#  
#  model_summary <- summary(model)
#  print(model_summary)
#  

