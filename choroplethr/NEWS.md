# choroplethr 5.0.1
- Removed dependency on the non-CRAN package rnatrualearthhires
- Added support for zip code level maps
- Fixed bugs and issues related to depreciation of ggplot2::aes_str(). 

# choroplethr 5.0.0
Many changes have been made to this version of the package, some of which are not backwards compatible with previous versions. If you need everything to work as before, please install an older version of choroplethr (instructions available on https://github.com/eastnile/choroplethr).

-   Columns no longer need to be named "value" and "region"! Instead, specify *value.name* and *geoid.name* as the variable to be plotted and the region names, respectively.
-   There are now multiple ways to identify each region in your map, as specified by *geoid.type*. For example, abbreviations, proper names, and FIPS code are now accepted for U.S. state level data. If geoid.type is not given, the package will try to automatically guess the type of geographic identifier being used.
-   Regions in your map can now be labeled using the *ggrepel* package, which intelligently places labels and prevents them from intersecting.
-   Many more customization options are now available for your map, including custom colors, styles, and map projections.
-   Ended support for the choropleth_wdi(); to map data from the WDI (world development indicators) package, please use country_choropleth().
-   county_choropleth() now supports an older map based on county definitions in 2015, as well as the most recent one based on 2024.
-   Added a `NEWS.md` file to track changes to the package.
