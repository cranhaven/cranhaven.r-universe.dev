#' Create a choropleth map using country-level data
#' 
#' See choroplethr::country.regions for an object which can help you coerce your
#' country names into the required format; the allowed geoid for this function
#' are columns name.proper, name.lower, iso_a3, and iso_a2 which appear at the
#' beginning of this object.
#'
#' @inheritParams common_args
#' @param df A dataframe containing country level data
#' @param geoid.name The variable that identifies each country
#' @param geoid.type How the variable given by geoid.name specifies each
#'   country. The allowed geoid.type are given by the columns name.proper,
#'   name.lower, iso_a3, \cr and iso_a2 in choroplethr::country.regions. If
#'   "auto", the function will try to automatically determine geoid.type.
#' @param zoom An optional vector of countries to zoom in on, written in the
#'   same manner as geoid.name.
#' @param continent_zoom Zoom in on a particular continent; to see which
#'   countries belong to which continent, see choroplethr::country.regions
#' @param reproject If TRUE, the map will be cropped and centered prior to
#'   applying the projection. This will generally result in a better figure when
#'   using the Robinson and Albers, but may lead to countries near the edge of
#'   the map being occluded.

#' @examples
#' # Create a choropleth map using country level data:
#' data(df_country_demographics)
#' country_choropleth(df_country_demographics, geoid.name = 'region', 
#'                    geoid.type = 'iso_a3', value.name = 'gdp',
#'                    title = "GDP of Countries in the World", 
#'                    legend = 'GDP (millions)')
#' 
#' # Use a divergent continuous color scale and customize map appearance:
#' country_choropleth(df_country_demographics, geoid.name = 'region', 
#'                    geoid.type = 'iso_a3', value.name = 'gdp', 
#'                    num_colors = 0, border_color = 'grey', 
#'                    color.max = 'gold', color.min = 'navyblue',
#'                    projection = 'robinson', latlon_ticks = TRUE, 
#'                    gridlines = TRUE, whitespace = FALSE,
#'                    background_color = 'azure',
#'                    title = "GDP of Countries in the World", 
#'                    legend = 'GDP (millions)')
#' 
#' # Zoom in on South America:
#' country_choropleth(df_country_demographics, geoid.name = 'region', 
#'                    geoid.type = 'iso_a3',
#'                    value.name = 'gdp', num_colors = 0, border_color = 'grey', 
#'                    continent_zoom = 'South America',
#'                    color.max = 'gold', color.min = 'navyblue',
#'                    projection = 'robinson', latlon_ticks = TRUE, 
#'                    gridlines = TRUE, whitespace = FALSE,
#'                    background_color = 'azure',
#'                    title = "GDP of Countries in the World", 
#'                    legend = 'GDP (millions)',
#'                    label = 'iso_a2', label_text_size = 5)
#' @export
country_choropleth = function(df, geoid.name = 'region', geoid.type = 'auto', value.name = 'value',
                              num_colors = 7, color.max = NULL, color.min = NULL, na.color = 'grey', custom.colors = NULL, nbreaks = 5,
                              zoom = NULL, continent_zoom = NULL, 
                              projection = 'cartesian', limits_lat = NULL, limits_lon = NULL, reproject = TRUE, 
                              border_color = 'grey15', border_thickness = 0.2,
                              background_color = 'white', gridlines = FALSE, latlon_ticks = FALSE, whitespace = TRUE,
                              label = NULL, label_text_size = 3, label_text_color = 'black', label_box_color = 'white', ggrepel_options = NULL,
                              legend = NULL, legend_position = 'right', title = NULL, return = 'plot')
                              
  
{
  continent_latlon = list()
  continent_latlon[['Europe']] = list(limits_lat= c(34,72), limits_lon = c(-17,45))
  continent_latlon[['Asia']] = list(limits_lat= c(-10,58), limits_lon = c(32,155))
  continent_latlon[['Africa']] = list(limits_lat= c(-37,39), limits_lon = c(-19,53))
  continent_latlon[['Oceania']] = list(limits_lat= c(-48, -1), limits_lon = c(112,182))
  continent_latlon[['North America']] = list(limits_lat= c(6, 79), limits_lon = c(-170,-48))
  continent_latlon[['South America']] = list(limits_lat= c(-58, 14), limits_lon = c(-84,-32))
  c = Choropleth$new(ref.regions = choroplethr::country.regions, 
                     ref.regions.name = 'choroplethr::country.regions',
                     map.df = choroplethr::country.map, 
                     geoid.all = c('name.proper', 'name.lower', 'iso_a3', 'iso_a2'),
                     user.df = df, geoid.name = geoid.name, geoid.type = geoid.type, 
                     value.name = value.name, num_colors = num_colors, label_col = label)
  
  zoom_cont = NULL
  if (!is.null(continent_zoom)) {
    stopifnot(length(continent_zoom) == 1)
    stopifnot(all(continent_zoom %in% unique(c$ref.regions$continent)))
    zoom_cont = c$ref.regions[c$ref.regions$continent %in% continent_zoom, c$geoid.type]
    if (is.null(limits_lat)) {
      limits_lat = continent_latlon[[continent_zoom]]$limits_lat
    }
    if (is.null(limits_lon)) {
      limits_lon = continent_latlon[[continent_zoom]]$limits_lon
    }
    if (!is.null(zoom)) {
      c$set_zoom(intersect(zoom, zoom_cont))
    } else {
      c$set_zoom(zoom_cont)
    }
  } else {
    c$set_zoom(zoom)
  }
  ggscale = c$get_ggscale(custom.colors = custom.colors, color.max = color.max, color.min = color.min, 
                          na.color = na.color, nbreaks = nbreaks)
  
  ggproj = c$get_projection(projection_name = projection, limits_lat = limits_lat, limits_lon = limits_lon, 
                            ignore_latlon = FALSE, reproject = reproject, whitespace = whitespace)
  
  if (projection %in% c('albers', 'robinson')) {
    occlude = TRUE 
  } else {
    occlude = FALSE
  }
  
  if (return == 'sf') {
    return(c$choropleth.df)
  } 
  plot = c$render(ggscale = ggscale, projection = ggproj, occlude_latlon_limits = occlude,
          border_color = border_color, border_thickness = border_thickness,
          background_color = background_color, gridlines = gridlines, latlon_ticks = latlon_ticks,
          label = label, label_text_size = label_text_size, label_text_color = label_text_color, 
          label_box_color = label_box_color, ggrepel_options = ggrepel_options,
          legend = legend, legend_position = legend_position, title = title, addl_gglayer = NULL)
  return(plot)
}





