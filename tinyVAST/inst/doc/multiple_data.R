## ----include = FALSE----------------------------------------------------------
has_lattice = requireNamespace("lattice", quietly = TRUE)
has_pdp = requireNamespace("pdp", quietly = TRUE)
EVAL <- has_lattice && has_pdp
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = EVAL,
  purl = EVAL
)
start_time = Sys.time()
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/multiple_data.Rmd"); rmarkdown::render( "vignettes/multiple_data.Rmd", rmarkdown::pdf_document())

## ----setup, echo=TRUE, warning=FALSE, message=FALSE---------------------------
library(tinyVAST)
library(sf)
library(fmesher)
library(ggplot2)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Load data
data( red_snapper ) 
data( red_snapper_shapefile ) 

# Plot data extent
plot( x = red_snapper$Lon,
        y = red_snapper$Lat,
        col = rainbow(3)[red_snapper$Data_type] )
plot( red_snapper_shapefile, col=rgb(0,0,0,0.2), add=TRUE )
legend( "bottomleft", bty="n", 
        legend=levels(red_snapper$Data_type), 
        fill = rainbow(3) )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Define link and distribution for each data type
Family = list(
   "Encounter" = binomial(link="cloglog"),
   "Count" = poisson(link="log"),
   "Biomass_KG" = tweedie(link="log")
)

# Relevel gear factor so Biomass_KG is base level
red_snapper$Data_type = relevel( red_snapper$Data_type,
                                 ref = "Biomass_KG" )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Define mesh
mesh = fm_mesh_2d( red_snapper[,c('Lon','Lat')], cutoff = 0.5 )

# define formula with a catchability covariate for gear
formula = Response_variable ~ Data_type + factor(Year) + offset(log(AreaSwept_km2))

# make variable column
red_snapper$var = "logdens"

# fit using tinyVAST
fit = tinyVAST( data = red_snapper, 
                formula = formula,
                space_term = "logdens <-> logdens, sd_space",
                spacetime_term = "logdens <-> logdens, 0, sd_spacetime",
                space_columns = c("Lon",'Lat'),
                spatial_domain = mesh,
                time_column = "Year",
                distribution_column = "Data_type",
                family = Family,
                variable_column = "var" )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# make extrapolation-grid
sf_grid = st_make_grid( red_snapper_shapefile, cellsize=c(0.2,0.2) )
sf_grid = st_intersection( sf_grid, red_snapper_shapefile )
sf_grid = st_make_valid( sf_grid )

# Extract coordinates for grid
grid_coords = st_coordinates( st_centroid(sf_grid) )
areas_km2 = st_area( sf_grid ) / 1e6

# Calcualte log-density for each year and grid-cell
index = plot_grid = NULL
for( year in sort(unique(red_snapper$Year)) ){
  # compile predictive data frame
  newdata = data.frame( "Lat" = grid_coords[,'Y'], 
                        "Lon" = grid_coords[,'X'],
                        "Year" = year,
                        "Data_type" = "Biomass_KG",
                        "AreaSwept_km2" = mean(red_snapper$AreaSwept_km2),
                        "var" = "logdens" )
  
  # predict
  log_dens = predict( fit, 
                      newdata = newdata, 
                      what = "p_g")
  log_dens = ifelse( log_dens < max(log_dens-log(100)), NA, log_dens )

  # Compile densities
  plot_grid = cbind( plot_grid, log_dens )

  # Estimate and compile total
  total = integrate_output( fit, 
                            newdata = newdata,
                            area = areas_km2 )
  index = cbind( index, total[c('Est. (bias.correct)','Std. Error')] )
  
}
colnames(plot_grid) = colnames(index) = sort(unique(red_snapper$Year))

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
# Convert to sf and plot
plot_grid = st_sf( sf_grid, 
                   plot_grid )
plot( plot_grid,
      border = NA )

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6-----------
ggplot( data.frame("Year"=colnames(index),"Est"=index[1,],"SE"=index[2,]) ) + 
  geom_point( aes(x=Year, y=Est)) + 
  geom_errorbar( aes(x=Year, ymin=Est-SE, ymax=Est+SE) )

## ----include = FALSE, warning=FALSE, message=FALSE----------------------------
run_time = Sys.time() - start_time

