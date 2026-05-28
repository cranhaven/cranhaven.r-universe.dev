## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(rGhanaCensus)
library(sf)
library(tmap)
library(dplyr)
library(magrittr)

## ----load_geometry_data, echo=TRUE--------------------------------------------
data("Ghana_2021_school_attendance_geometry", package = "rGhanaCensus")

## ----convert_to_sf_data_frame-------------------------------------------------
Ghana_edu_sf<- sf::st_as_sf(Ghana_2021_school_attendance_geometry)

## ----Example1-----------------------------------------------------------------

#Use tmap to create interactive map
tmap_mode("plot")

Ghana_edu_sf %>%
      dplyr::filter(Locality=="Urban") %>%
      tm_shape()+
      tm_polygons(id="Region", col="Region",palette="YlOrRd",
                  title="Percentage of School drop-outs")+
      tm_text(text="Percent_Dropped_out_of_School", size=0.7)+
      tm_facets(by="Gender")

## ----Example2-----------------------------------------------------------------

#Load geometry data
data("Ghana_2021_school_attendance_geometry", package = "rGhanaCensus")

#Convert to sf data frame
Ghana_edu_sf<- sf::st_as_sf(Ghana_2021_school_attendance_geometry)


## ----plot_population_densities_of_school_drop_outs----------------------------

tmap_mode("plot")

Ghana_edu_sf %>%
      dplyr::filter(Locality=="Urban") %>%
      tm_shape()+
      tm_polygons(id="Region",col="Dropped_out_of_School", palette = "RdPu",
                  style="kmeans", convert2density = TRUE,
                  title="Population density of School drop-outs")+ 
      tm_text(text="Region", size=0.7)+
      tm_facets(by="Gender")

