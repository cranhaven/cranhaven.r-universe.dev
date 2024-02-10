#' Environmental descriptors for future A1B scenario for 2200 (Kerguelen Plateau)
#'
#' @description
#' RasterStack of 10 environmental descriptors modelled by IPCC (scenario A1B, 4th report, 2007) for 2187 to 2196 (described as 2200), on the extent of the Kerguelen Plateau (63/81W; -46/-56S)
#'
#' @usage
#' data('predictors2200AIB')
#'
#' @format
#' RasterStack of 10 environmental descriptors. Grid: nrow= 100, ncol= 179, ncells= 17900 pixels. Spatial resolution: 0.1. Spatial extent: 63/81W; -46/-56S. \cr Crs : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0. Origin=0. \cr See Guillaumot et al.(2016) for more information
#'
#' \itemize{
#'   \item \strong{depth} \cr Bathymetric grid around the Kerguelen Plateau \cr
#'   Unit=meter. Reference=Guillaumot et al. (2016), derived from Smith & Sandwell (1997) \cr \url{https://topex.ucsd.edu/WWW_html/mar_topo.html}
#'
#'   \item \strong{seasurface_salinity_mean_2200_A1B} \cr Mean sea surface salinity over 2187 to 2196, A1B scenario \cr Unit= PSS. Reference= BIO ORACLE (Tyberghein et al. 2012) \cr \url{https://www.bio-oracle.org/}
#'
#'   \item \strong{seasurface_temperature_mean_2200_A1B} \cr Mean sea surface temperature over 2187-2196, A1B scenario \cr Unit=Celsius degrees. Reference= BIO ORACLE (Tyberghein et al. 2012) \cr \url{https://www.bio-oracle.org/}
#'
#'   \item \strong{seasurface_temperature_amplitude_2200_A1B} \cr Amplitude between mean summer and mean winter sea surface temperature. Absolute value interpolated over 2187-2196, scenario A1B \cr Unit=Celsius degrees. Reference= BIO ORACLE (Tyberghein et al. 2012) \cr \url{https://www.bio-oracle.org/}
#'
#'   \item \strong{chlorophyla_summer_mean_2002_2009} \cr Surface chlorophyll a concentration. Summer mean over 2002-2009 \cr Unit=mg/m3. Reference=MODIS AQUA (NASA) 2010 \cr \url{https://oceandata.sci.gsfc.nasa.gov/}
#'
#'   \item \strong{geomorphology} \cr Geomorphologic features \cr Unit= 27 categories. Reference= ATLAS ETOPO2 2014 (Douglass et al. 2014)
#'
#'   \item \strong{sediments} \cr Sediment features \cr Unit= 14 categories. Reference= McCoy (1991), updated by Griffiths 2014 (unpublished)
#'
#'   \item \strong{slope} \cr Bathymetric slope \cr Unitless. Reference= Smith & Sandwell (1997)
#'
#'   \item \strong{seafloor_oxygen_mean_1955_2012} \cr Mean seafloor oxygen concentration over 1955-2012 \cr Unit=mL/L. Reference= Guillaumot et al. (2016), derived from World Ocean Circulation Experiment 2013 sea surface oxygen concentration layers \cr \url{https://www.nodc.noaa.gov/OC5/woa13/woa13data.html}
#'
#'   \item \strong{roughness} \cr Rugosity index (difference between minimal and maximal depth values of the 8 neighbour-pixels) \cr Unit= meters. Reference=Guillaumot et al.(2016), derived from bathymetric layer.
#'   }

#'@references
#'Douglass LL, Turner J, Grantham HS, Kaiser S, Constable A, Nicoll R, Raymond B, Post A, Brandt A, Beaver D (2014) A hierarchical classification of benthic biodiversity and assessment of protected areas in the Southern Ocean. PloS one 9(7): e100551. doi: 10.1371/journal.pone.0100551.
#'
#'Guillaumot, C., Martin , A., Fabri-Ruiz, S., Eleaume, M. and Saucede, T. (2016) Environmental parameters (1955-2012) for echinoids distribution modelling on the Kerguelen Plateau. Australian Antarctic Data Centre - doi:10.4225/15/578ED5A08050F
#'
#'Jueterbock A, Tyberghein L, Verbruggen H, Coyer JA, Olsen JL, Hoarau G (2013) Climate change impact on seaweed meadow distribution in the North Atlantic rocky intertidal. Ecology and Evolution 3(5): 1356-1373. doi: 10.1002/ece3.541.
#'
#'McCoy FW (1991) Southern Ocean sediments: circum-Antarctic to 30S. Marine Geological and Geophysical Atlas of the circum-Antarctic to 30S. (ed. by D.E. Hayes). Antarctic Research Series.
#'
#'Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F, De Clerck O (2012) Bio ORACLE: a global environmental dataset for marine species distribution modelling. Global Ecology and Biogeography 21(2): 272-28. doi: 10.1111/j.1466-8238.2011.00656.x.
#'
#'Smith W, Sandwell D (1997) Global seafloor topography from satellite altimetry and ship depth soundings. Science 277(5334): 1957-1962. doi: 10.1126/science.277.5334.1956.

#'@examples
#'data('predictors2200AIB')
#'raster :: plot(predictors2200AIB)
#'
"predictors2200AIB"



