# from the globalVariables documentation

if(getRversion() >= "2.15.1") {

utils::globalVariables(

# air_stripper
c("Removal", "Influent", "Effluent", "Diff", "..dcol", "Average Mass", "Molecular Formula", "Substance Name",

# concr_mix_lightweight_strength and concr_mix_normal_strength
"..water_nonair_column", "..water_air_column", "..water_avg_air_column", "..exp_search", "V1",

# secprop
"nOC", "nIC", "x", "y",

# engr_survey_batch, engr_survey_reverse, polygon_area, project_midpoint
"Y", "X", "Easting", "Northing",

# lat_long2utm
"Latitude",

# algor
"i"))

}
