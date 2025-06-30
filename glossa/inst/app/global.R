#=========================================================#
# Load packages ----
#=========================================================#
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(waiter)

library(ggplot2)
library(sparkline)
library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(mcp)
library(DT)
library(svglite)
library(dbarts)
library(leaflet)

library(glossa)

#=========================================================#
# GLOSSA options ----
#=========================================================#
sf::sf_use_s2(FALSE)
