#' Create a choropleth map using U.S. county level data:
#'
#' Counties must be identified by FIPS code; see 
#' choroplethr::county.regions.2015 \cr
#' or choroplethr::county.regions.2024 for an
#' object that can help you coerce your county names into this format.
#'
#' @inheritParams common_args
#' @param df A dataframe containing U.S. county level data
#' @param map_year Either 2015 or 2024; uses county definitions from that
#'   particular year.
#' @param geoid.name The name of the variable that identifies each county
#' @param geoid.type Either "fips.numeric" or "fips.character"; if "auto", the
#'   function will try to automatically determine geoid.type. \cr See
#'   choroplethr::county.regions.2015 or \cr choroplethr::county.regions.2024 a
#'   lookup table.
#' @param state_zoom An optional vector of states to zoom in on. Elements of
#'   this vector must match one of the columns in choroplethr::state.regions.
#' @param county_zoom An optional vector of counties to zoom in on, written in
#'   the same manner as geoid.name.
#' @param add_state_outline Should state borders be outlined in your map?
#'
#' @examples
#' \donttest{
#' # Create a map based on US county data:
#' data("df_county_demographics")
#' county_choropleth(df_county_demographics, geoid.name = 'region', 
#'                   geoid.type = 'fips.numeric',
#'                   value.name = 'median_hh_income',
#'                   title = "Median Household Income of U.S. Counties",
#'                   legend = 'Median HH Income')
#'
#' county_choropleth(df_county_demographics, geoid.name = 'region', 
#'                   geoid.type = 'fips.numeric',
#'                   value.name = 'median_hh_income',
#'                   state_zoom = c('CA', 'OR', 'WA'),
#'                   title = "Median Household Income of West Coast Counties",
#'                   legend = 'Median HH Income')
#' }
#' @export
#' @importFrom ggplot2 geom_sf
county_choropleth = function(df, map_year = 2024, geoid.name = 'region', geoid.type = 'auto', value.name = 'value',
                             num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL, nbreaks = 5,
                             county_zoom = NULL, state_zoom = NULL, projection = 'albers',
                             border_color = 'grey15', border_thickness = 0.2,
                             background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, whitespace = TRUE,
                             label = NULL, label_text_size = 2.25, label_text_color = 'black', label_box_color = 'white',
                             ggrepel_options = NULL,
                             legend = NULL, legend_position = 'right', title = NULL, return = 'plot',
                             add_state_outline = TRUE)
{
  if (!map_year %in% c(2024, 2015)) {
    stop("map_year must be 2024 or 2015")
  }
  
  if (map_year == 2024) {
    ref.regions = choroplethr::county.regions.2024
    ref.regions.name = 'choroplethr::county.regions.2024'
    map.df = choroplethr::county.map.2024
  } else if (map_year == 2015) {
    ref.regions = choroplethr::county.regions.2015
    ref.regions.name = 'choroplethr::county.regions.2015'
    map.df = choroplethr::county.map.2015
  } else {
    stop("map_year must be 2024 or 2015.")
  }
  
  c = Choropleth$new(ref.regions = ref.regions, 
                     ref.regions.name = ref.regions.name,
                     map.df = map.df, 
                     geoid.all =  c('fips.numeric', 'fips.character'),
                     user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  
  if (!is.null(state_zoom)) {
    state_zoom_geoid = guess_geoid_type(user.regions = state_zoom, geoid.all = c('name.proper', 'name.lower', 'state.abb', 'fips.character', 'fips.numeric'),
                                       ref.regions = choroplethr::state.regions, ref.regions.name = 'choroplethr::state.regions')
    if (!all(state_zoom %in% choroplethr::state.regions[[state_zoom_geoid]])) {
      stop('all elements of state_zoom must match one of the columns in choroplethr::state.regions')
    }
    state_zoom_fipsn = choroplethr::state.regions[choroplethr::state.regions[[state_zoom_geoid]] %in% state_zoom, "fips.numeric"]
    counties_in_state_zoom = ref.regions[ref.regions$state.fips.numeric %in% state_zoom_fipsn, c$geoid.type]
    if(!is.null(county_zoom)) {
      c$set_zoom(intersect(county_zoom, counties_in_state_zoom))
    } else {
      c$set_zoom(counties_in_state_zoom)
    }
  } else {
    c$set_zoom(county_zoom)
  }
  
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  
  ggproj = c$get_projection(projection = projection, limits_lat = NULL, limits_lon = NULL,
                            reproject = FALSE, ignore_latlon = TRUE, whitespace = whitespace)
  
  if (return == 'sf') {
    return(c$choropleth.df)
  }
  
  if (add_state_outline) {
    counties_used = unique(c$choropleth.df$fips.numeric[c$choropleth.df$render])
    states_used = unique(floor(counties_used/1000))
    state_map = choroplethr::state.map.hires
    state_map = state_map[state_map$fips.numeric %in% states_used, ]
    state_outline = geom_sf(data = state_map, color = 'black', fill = NA, linewidth = .75)
  } else {
    state_outline = NULL
  }

  plot = c$render(ggscale = ggscale, projection = ggproj, 
                  border_color = border_color, border_thickness = border_thickness,
                  background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks, 
                  label = label, label_text_size = label_text_size, label_text_color = label_text_color, label_box_color = label_box_color,
                  ggrepel_options = ggrepel_options, occlude_latlon_limits = FALSE,
                  legend = legend, legend_position = legend_position, title = title,
                  addl_gglayer = state_outline)
  
  return(plot)
}
