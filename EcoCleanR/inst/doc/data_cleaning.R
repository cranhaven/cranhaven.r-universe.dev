## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # eval = FALSE,
  fig.width = 8,
  fig.height = 6,
  out.width = "70%"
)

## ----setup--------------------------------------------------------------------
# package loading
library(EcoCleanR)
library(dplyr)

## -----------------------------------------------------------------------------
# provide example species name
species_name <- "Mexacanthina lugubris"

## -----------------------------------------------------------------------------
ec_geographic_map(ecodata,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)

## -----------------------------------------------------------------------------
comparison <- ec_worms_synonym(species_name,
  ecodata,
  scientificName = "scientificName"
)
print(comparison)
# compare the columns to know if any taxa found that is not a synonym in WoRMS data base, filter bad taxa from ecodata using dplyr::filter()

## -----------------------------------------------------------------------------
ecodata_cl <- ec_filter_by_uncertainty(ecodata,
  uncertainty_col = "coordinateUncertaintyInMeters",
  percentile = 0.95,
  ask = FALSE,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)
str(ecodata_cl[, 1:3])
### plot the map
ec_geographic_map(ecodata_cl,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)

## -----------------------------------------------------------------------------
ecodata_cl$flag_precision <- ec_flag_precision(ecodata_cl,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)

# filter the flag - flag_cordinate_precision
ecodata_cl <- ecodata_cl %>%
  filter(flag_precision != 1)
str(ecodata_cl[1:3])

## ----heavy-processing-0, eval = FALSE-----------------------------------------
# # This is a heavy processing step, won’t execute during vignette building.
# direction <- "east"
# buffer <- 25000
# ocean <- "pacific"
# ecodata_cl$flag_non_region <- ec_flag_non_region(direction,
#   ocean,
#   buffer,
#   ecodata_cl,
#   latitude = "decimalLatitude",
#   longitude = "decimalLongitude"
# )
# str(ecodata_cl[, 1:3])
# # filter flagged records
# ecodata_cl <- ecodata_cl %>%
#   filter(flag_non_region != 1)
# ### map view to see accepted records
# ec_geographic_map(ecodata_cl,
#   latitude = "decimalLatitude",
#   longitude = "decimalLongitude"
# )

## ----heavy-processing-1, eval = FALSE-----------------------------------------
# # This is a heavy processing step, won’t execute during vignette building.
# # get the unique combination of coordiantes
# ecodata_unique <- ecodata_cl[, c("decimalLatitude", "decimalLongitude")]
# ecodata_unique <- base::unique(ecodata_unique)
# # It is recommended to check what layers available in sdm_predictors and correct name.
# # available_layers <- list_layers() # returns something like c("BO_sstmean", "BO_sstmax", ...)
# # provide layers as input to env_layers variable
# env_layers <- c("BO_sstmean", "BO_sstmin", "BO_sstmax")
# 
# ### extraction env layers
# ecodata_unique <- ec_extract_env_layers(ecodata_unique,
#   env_layers = env_layers,
#   latitude = "decimalLatitude",
#   longitude = "decimalLongitude"
# )
# # A warning message if layers are in saved in cache.
# 
# ### impute env var values those were missing after extraction
# ecodata_unique <- ec_impute_env_values(
#   ecodata_unique,
#   latitude = "decimalLatitude",
#   longitude = "decimalLongitude",
#   radius_km = 10,
#   iter = 3
# )
# 
# ### omit the coordinate which couldn't get any env values after imputation
# ecodata_unique <- na.omit(ecodata_unique)

## ----heavy-processing-2, eval = FALSE-----------------------------------------
# # This is a heavy processing step, won’t execute during vignette building.
# # Instead of executing it here, we will use a pre-saved cleaned file.
# ecodata_unique$flag_outliers <- ec_flag_outlier(ecodata_unique,
#   latitude = "decimalLatitude",
#   longitude = "decimalLongitude",
#   env_layers,
#   itr = 50,
#   k = 3,
#   geo_quantile = 0.99,
#   maha_quantile = 0.99
# )$outlier
# 
# ### these unique combinations of coordiantes, environmental variables and outliers will be mergeed to main ecodata_cl file
# ecodata_cl <- ecodata_cl %>%
#   left_join(ecodata_unique[, c("decimalLatitude", "decimalLongitude", "flag_outliers", env_layers)],
#     by = c("decimalLatitude", "decimalLongitude")
#   )

## -----------------------------------------------------------------------------
# pre-saved file ecodata_with_outliers instead of using ecodata_cl
### map view to see records with outlier probability
ec_geographic_map_w_flag(ecodata_with_outliers,
  flag_column = "outliers",
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)

## -----------------------------------------------------------------------------
### mapview to visualize accepted data
ec_geographic_map(ecodata_cleaned,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude"
)

## -----------------------------------------------------------------------------
env_layers <- c("BO_sstmean", "BO_sstmax", "BO_sstmin")
data("ecodata_cleaned")
summary_table <- ec_var_summary(ecodata_cleaned,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude",
  env_layers
)
head(summary_table)

ec_plot_var_range(ecodata_with_outliers,
  summary_df = summary_table,
  latitude = "decimalLatitude",
  longitude = "decimalLongitude",
  env_layers = env_layers
)

