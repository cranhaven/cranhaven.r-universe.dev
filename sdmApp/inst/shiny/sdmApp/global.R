library(shiny)
library(grid)
library(sdmApp)
library(rhandsontable)
library(haven)
library(shinyBS)
library(data.table)
library(sf) # classes and functions for vector data
library(raster)# classes and functions for raster data
library(ggplot2)
library(dismo)
library(DT)
library(readxl)
library(shinyFiles)
library(shinydashboard)
library(SSDM)
library(automap)
library(ggcorrplot)
library(blockCV)
#library(tidyverse)
#library(ggpubr)
library(cowplot)
#library(CENFA)
library(randomForest)
library(kernlab)
library(dplyr)
library(sp)
library(rJava)
library(stringr)
if (!getShinyOption("sdmAppInvoked", FALSE)) {### Beginning required code for deployment
  .startdir <- .guitheme <- .guijsfile <- NULL
  # maxRequestSize <- 50
  # options(shiny.maxRequestSize=ceiling(maxRequestSize)*1024^2)

  shinyOptions(.startdir = getwd())

  theme="IHSN"
  shinyOptions(.guitheme = "ihsn-root.css")
  shinyOptions(.guijsfile = "js/ihsn-style.js")
}## End of deployment code
# required that 'dQuote()' works nicely when
# outputting R-Code
options(useFancyQuotes=FALSE)
#obj=sdm is an object
obj <- reactiveValues()
obj$code_read_and_modify <- c()
#$code_setup <- c()
obj$code_model <- c()
obj$code <- c(
  paste("# created using sdmApp", packageVersion("sdmApp")),
  "library(sdmApp)", "",
  "obj <- NULL")

obj$cur_selection_results <- "btn_results_1"

Specdata<-reactive({
  dsf<-load.occ$select
  dsf<-dsf %>% dplyr::rename(lon=load.occ$lon,lat=load.occ$lat)
  dsf[,1]<-as.numeric(unlist(dsf[,1]))
  dsf[,2]<-as.numeric(unlist(dsf[,2]))
  dsf[,3]<-as.numeric(unlist(dsf[,3]))
  dsf
})

Specdata_Presence<-reactive({
  dsf<-Specdata()
  dsf<-dsf[dsf[,ncol(dsf)] == 1,]
  sp::coordinates(dsf) <-~lon+lat
  sp::proj4string(dsf) <-raster::crs(data$Env)
  dsf
})

glc<-reactive({
  CENFA::GLcenfa(x = data$Env)
})

mod.enfa<-reactive({
  pr<-Specdata_Presence()
  pr@data$load.occ$spec_select<-as.numeric(pr@data$load.occ$spec_select)
  CENFA::enfa(x = data$Env, s.dat = pr, field = load.occ$spec_select)
})

enfa_plot<-reactive({
  glc <- glc()

  mod.enfa <- mod.enfa()
  load.occ$enfa_plot<-CENFA::scatter(x = mod.enfa, y = glc,n=nlayers(data$Env),p=1)
  load.occ$enfa_plot
})

#################
Z<-reactive({
  CENFA::parScale(data$Env)
})


# Efficient calculation of covariance matrices for Raster* objects
mat<-reactive({
  CENFA::parCov(Z())
})

pa_data<-reactive({
  sf::st_as_sf(Specdata(), coords = c("lon","lat"), crs = crs(data$Env))

})
Cor<-reactive({
  Corr<-raster::extract(data$Env, pa_data(), df = TRUE)
  Corr<-Corr[,-1]
  Corr
})

p.mat <-reactive({
  p_mat<-ggcorrplot::cor_pmat(Cor())
  p_mat
})

marg_spec<-reactive({
  mod.enfa <- mod.enfa()
  data.frame(mod.enfa@co)
})

# sac<-reactive({
#   a = try(withProgress(message = 'Spatial Autorange',
#                        blockCV::spatialAutoRange(rasterLayer = data$Env,
#                                         doParallel = T,
#                                         plotVariograms = TRUE,
#                                         showPlots = FALSE)))
#   a
# })

range<-reactive({
  sac<-load.occ$sac
  round(sac$range,0)
})


tableRange<-reactive({
  sac<-load.occ$sac
  sac$rangeTable
})

data <- reactiveValues(Env = stack(), Occ = data.frame(), dir = getwd(), ESDM = NULL, esdms = list(), Stack = NULL)
load.var <- reactiveValues(factors = c(), formats = c(), norm = TRUE,  vars = list())
working.directory <- system.file("extdata", package = "sdmApp")

load.occ <- reactiveValues()
model <- reactiveValues()
