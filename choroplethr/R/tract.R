#' Download a map of all census tracts in a given state
#' 
#' The map returned is exactly the same map which tract_choropleth uses. It is
#' downloaded using the "tracts" function in the tigris package, and then it is
#' modified for use with choroplethr.
#'
#' @param state_name The name of the state, given by proper name, abbreviation,
#'   for FIPS code.
#' @param map_year The year of the map you want to download.
#' @param drop_geometry Drop geometry data?
#' @importFrom tigris tracts
#' @importFrom sf st_transform st_drop_geometry
#' @export
get_tract_map = function(state_name, map_year, drop_geometry = TRUE) {
  tract.map.df = tracts(state = state_name, cb = TRUE, year = map_year, progress_bar = FALSE)
  tract.map.df$tractid.numeric = as.numeric(tract.map.df$GEOID)
  tract.map.df$county.fips.numeric = as.numeric(paste0(tract.map.df$STATEFP, tract.map.df$COUNTYFP))
  tract.map.df = st_transform(tract.map.df, 4326)
  if (drop_geometry) {
    tract.map.df = as.data.frame(sf::st_drop_geometry(tract.map.df))
  }
  return(tract.map.df)
}

#' Create a choropleth map using census tract level data for a given state.
#' 
#' @inheritParams common_args
#' @param df A dataframe containing census tract level data for a given state.
#' @param state_name The state in question, given by either proper name,
#'   abbreviation, or FIPS code.
#' @param geoid.name The variable that identifies each tract.
#' @param geoid.type How the variable given by geoid.name specifies each tract;
#'   the allowed \cr geoid.type are given by the columns "GEOID", or
#'   "tractid.numeric" variable obtained from get_tract_map(). If "auto", the
#'   function will try to automatically determine geoid.type.
#' @param map_year Uses tract definitions for that particular year, as reported
#'   by the tigris package. Only years 2013 and later are supported.
#' @param tract_zoom An optional vector of tracts to zoom in on, written in the
#'   same manner as geoid.name.
#' @param county_zoom An optional vector of countries to zoom in on, written as
#'   they appear in the "county.fips.numeric" column of the object returned from
#'   get_tract_map().
#' @examples
#' \donttest{
#' # Plot tract level data from New York state:
#' df_ny_tract_demographics = choroplethr::df_ny_tract_demographics
#' tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
#'                  geoid.name = 'region', value.name = 'population')
#'
#' # Zoom in on the five counties that comprise New York City: 
#' tract_choropleth(df = df_ny_tract_demographics, state_name = 'NY', 
#'                  geoid.name = 'region', value.name = 'population',
#'                  county_zoom = c(36005, 36047, 36061, 36081, 36085))
#' }
#' @seealso \url{https://www.census.gov/data/academy/data-gems/2018/tract.html}
#'   for more information on Census Tracts
#' @export
#' 
tract_choropleth = function(df, state_name, geoid.name = 'region', geoid.type = 'auto', value.name = 'value', map_year = 2020,
                            num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL, nbreaks = 5,
                            tract_zoom = NULL, county_zoom = NULL, projection = 'cartesian',
                            border_color = 'grey15', border_thickness = 0.2,
                            background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, whitespace = TRUE,
                            label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white',
                            ggrepel_options = NULL,
                            legend = NULL, legend_position = 'right', title = NULL, return = 'plot') {
  
  if (map_year < 2013) {
    stop('Only years 2013 or later are supported.')
  }
  
  tract_map = get_tract_map(state_name, map_year = map_year, drop_geometry = FALSE)
  c = Choropleth$new(ref.regions = sf::st_drop_geometry(tract_map), 
                     ref.regions.name = 'output of get_tract_map()',
                     map.df = tract_map[, c('tractid.numeric', 'geometry')], 
                     geoid.all = c('GEOID', 'tractid.numeric'),
                     user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  if (!is.null(county_zoom)) {
    if (!all(county_zoom %in% tract_map$county.fips.numeric)) {
      stop('County_zoom must match the names of counties as they appear in the "county.fips.numeric" column of the object returned from "get_tract_map()".')
    }
    tracts_in_county = tract_map[tract_map$county.fips.numeric %in% county_zoom, ][[c$geoid.type]]
    if(!is.null(tract_zoom)) {
      c$set_zoom(intersect(tract_zoom, tracts_in_county))
    } else {
      c$set_zoom(tracts_in_county)
    }
  } else {
    c$set_zoom(tract_zoom)
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
