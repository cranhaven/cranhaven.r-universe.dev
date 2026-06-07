#' Download a map of requested zip codes
#' 
#' The map returned is exactly the same map which zip_choropleth() uses. It is
#' downloaded using the "zctas" function in the tigris package, and then it is
#' modified for use with choroplethr.
#'
#' @param zipcodes The zipcodes for which you want to download map data.
#' @param map_year The year of the map you want to download. Only years after 
#' 2013 are available.
#' @param drop_geometry Drop geometry data?
#' @importFrom tigris zctas
#' @importFrom sf st_transform st_drop_geometry
#' @export
get_zip_map = function(zipcodes, map_year, drop_geometry = TRUE) {
  if (F) {
    zipcodes = c("02817", "02831")
    map_year = 2020
  }
  zip.map.df = zctas(starts_with = zipcodes, cb = TRUE, year = map_year, progress_bar = FALSE)
  names_needed = c(grep('ZCTA', names(zip.map.df), value = TRUE), 'geometry')
  zip.map.df = zip.map.df[, names_needed]
  rownames(zip.map.df) = NULL
  names(zip.map.df) = c('zip_code', 'geometry')
  zip.map.df = st_transform(zip.map.df, 4326)
  if (drop_geometry) {
    zip.map.df = as.data.frame(sf::st_drop_geometry(zip.map.df))
  }
  return(zip.map.df)
}

#' Create a choropleth map using zip code level data
#' 
#' To link zip codes to counties, states, or CBSA (Census Based Statistical
#' Areas), see choroplethr::zip_lookup.
#' 
#' @inheritParams common_args
#' @param df A dataframe containing zip code level data.
#' @param geoid.name The variable that contains the zip code for each
#'   observation, written as a five digit character (string) vector.
#' @param map_year Uses zip code definitions for that particular year, as
#'   reported by the tigris package. Only years 2013 and later are supported.
#' @param zoom An optional vector of zip codes to zoom in on, written as five
#'   digit characters.
#' @examples
#' \donttest{
#' # Plot zip code level data for Rhode Island:
#'   df_zip = choroplethr::df_ri_zip_demographics
#'   zip_choropleth(df = df_zip, geoid.name = 'region', 
#'                  value.name = 'population')
#' # Label each zip code on the map:
#'   zip_choropleth(df = df_zip, geoid.name = 'region', 
#'                  value.name = 'population', label = 'zip_code')
#' }
#' @export
#' 
zip_choropleth = function(df, geoid.name = 'region', value.name = 'value', map_year = 2020,
                            num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL, nbreaks = 5,
                            zoom = NULL, projection = 'cartesian',
                            border_color = 'grey15', border_thickness = 0.2,
                            background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, whitespace = TRUE,
                            label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white',
                            ggrepel_options = NULL,
                            legend = NULL, legend_position = 'right', title = NULL, return = 'plot') {
  
  # Check zip codes have the correct format
  if (!inherits(df[[geoid.name]], 'character') | !all(nchar(df[[geoid.name]]) == 5)) {
    stop('Zip codes must be given as a five-digit character vector')
  }
  
  if (map_year < 2013) {
    stop('Only years 2013 or later are supported.')
  }
  zips_requested = unique(df[[geoid.name]])
  zip_map = get_zip_map(zipcodes = zips_requested, map_year = map_year, drop_geometry = FALSE)
  c = Choropleth$new(ref.regions = sf::st_drop_geometry(zip_map), 
                     ref.regions.name = 'output of get_zip_map()',
                     map.df = zip_map, 
                     geoid.all = c('zip_code'),
                     user.df = df, geoid.name = geoid.name, geoid.type = 'zip_code', 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  if (!is.null(zoom)) {
    c$set_zoom(zoom)
  }
  
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  
  ggproj = c$get_projection(projection = projection, limits_lat = NULL, limits_lon = NULL,
                            reproject = FALSE, ignore_latlon = TRUE, whitespace = whitespace)
  
  if (return == 'sf') {
    return(c$choropleth.df)
  }
  
  plot = c$render(ggscale = ggscale, projection = ggproj, occlude_latlon_limits = FALSE,
                  border_color = border_color, border_thickness = border_thickness,
                  background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                  label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                  ggrepel_options = ggrepel_options,
                  legend = legend, legend_position = legend_position, title = title, addl_gglayer = NULL)
  return(plot)
  
}
