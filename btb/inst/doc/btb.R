## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 5,fig.align = 'center'
  
  
)

## ----setup , eval = FALSE-----------------------------------------------------
#  install.packages("btb")

## ---- eval = F----------------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("InseeFr/btb")

## ----seepoints , eval = T-----------------------------------------------------
library(btb)
data(dfPrix_SP95_2016)
head(dfPrix_SP95_2016)

## ----cartopoints--------------------------------------------------------------
library(sf)
sfPrix_SP95_2016 <- st_as_sf(dfPrix_SP95_2016,coords = c("x","y"), crs=2154)
plot(sfPrix_SP95_2016$geometry)

## ---- addcentro---------------------------------------------------------------
dfPrix_SP95_2016 <- btb_add_centroids(dfPrix_SP95_2016, 
                                      iCellSize = 20000,
                                      names_coords = c("x","y"))
head(dfPrix_SP95_2016)

## ----checkgrid, warning=FALSE-------------------------------------------------
library(dplyr)
centro_values <- dfPrix_SP95_2016 %>%
  group_by(x_centro, y_centro) %>%
  summarise(pricemean=mean(SP95, rm.na = TRUE))

## ----checkgrid2---------------------------------------------------------------
grid_values <- btb_ptsToGrid(centro_values, sEPSG = 2154,
                             iCellSize = 20000, 
                             names_centro = c("x_centro","y_centro"))
nrow(grid_values)
head(grid_values)


## ----seegrid------------------------------------------------------------------
library(mapsf)

mapsf::mf_map(x = grid_values,
       type = "choro",
       var="pricemean",
       breaks = "quantile",
       nbreaks = 5,
       lwd=1,
       leg_val_rnd = 1)

## ----smooth_density-----------------------------------------------------------

pts_density <- dfPrix_SP95_2016[,c("x","y")]
# Create dummy
pts_density$stations_density <- 1L
head(pts_density)

# Smoothing
smooth_density <- btb_smooth(
  pts = pts_density,
  sEPSG = 2154,
  iBandwidth = 100000,
  iCellSize = 5000)

head(smooth_density)

# Map
mapsf::mf_map(x = smooth_density,
       type = "choro",
       var="stations_density",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)

## ----smooth_mean_price--------------------------------------------------------
# Prepare your data
pts_meanprice <- dfPrix_SP95_2016[,c("x","y","SP95")]
pts_meanprice$stations_density <- 1L
head(pts_meanprice)

# Smooth both prices and station density
smooth_density <- btb_smooth(
  pts = pts_meanprice,
  sEPSG = 2154,
  iBandwidth = 100000,
  iCellSize = 5000)

head(smooth_density)

# Calculate the smoothed mean (from smoothed nominator and denominator)
smooth_density <- smooth_density %>% mutate(meanprice=SP95/stations_density)
mapsf::mf_map(x = smooth_density,
       type = "choro",
       var="meanprice",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)

## ----include=F----------------------------------------------------------------
Cstack_info()

## ----quantile_smooth----------------------------------------------------------

pts_quantiles <- dfPrix_SP95_2016[,c("x","y","SP95")]
head(pts_quantiles)

smooth_quantiles <- btb_smooth(pts = pts_quantiles, 
                               sEPSG = 2154, iBandwidth = 100000,
                               iCellSize = 5000,vQuantiles = c(0.5,0.9))

head(smooth_quantiles)

# Median smoothing : 
mapsf::mf_map(x = smooth_quantiles,
       type = "choro",
       var="SP95_05",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)
# Smooth the 9th decile :
mapsf::mf_map(x = smooth_quantiles,
       type = "choro",
       var="SP95_09",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)


## ----ratesmooth---------------------------------------------------------------

# Load data
data("reunion")
head(reunion)

# Optional : transform as sf points
sfreunion <- sf::st_as_sf(reunion,coords= c("x","y"), crs = 3727)
plot(sfreunion$geometry)

# btb_smooth with an automatic grid
smooth_reunion <- btb_smooth(sfreunion,iCellSize = 500,iBandwidth = 5000)

# Calculate the ratio
smooth_reunion <- smooth_reunion %>% mutate(prop_poors = 100 * phouhold / houhold)

# map
mapsf::mf_map(x = smooth_reunion,
       type = "choro",
       var="prop_poors",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)

## ----neighboors---------------------------------------------------------------
smooth_reunion <- btb_smooth(sfreunion,iCellSize = 500,iBandwidth = 5000, iNeighbor = 0)
smooth_reunion <- smooth_reunion %>% mutate(prop_poors = 100 * phouhold / houhold)

mapsf::mf_map(x = smooth_reunion,
       type = "choro",
       var="prop_poors",
       breaks = "quantile",
       nbreaks = 5,
       border = NA,
       leg_val_rnd = 1)

## ----inspire------------------------------------------------------------------
smooth_reunion <- btb_smooth(sfreunion,iCellSize = 500,
                             iBandwidth = 2000, iNeighbor = 0, inspire = TRUE)
smooth_reunion <- smooth_reunion %>% mutate(prop_poors = 100 * phouhold / houhold)
head(smooth_reunion)


## ----export, eval=FALSE-------------------------------------------------------
#  sf::write_sf("MY/REPOSITORY/myfile.gpkg")

