rLakeHabitat
------------

This package offers bathymetric interpolation using Inverse Distance Weighted and Ordinary Kriging via the gstat and terra packages. Other functions focus on quantifying physical aquatic habitats (e.g., littoral, epliminion, metalimnion, hypolimnion) from interpolated DEMs. Functions were designed to calculate these metrics across water levels for use in reservoirs but can be applied to any DEM and will provide values for fixed conditions. Parameters like Secchi disk depth or estimated photic zone, thermocline depth, and water level fluctuation depth are included in most functions.

**To install and load 'rLakeHabitat' in R, run the following (requires the devtools package):**

`devtools::install_gitlab("tristanblechinger/rlakehabitat")`

`require(rLakeHabitat)`
