#' Create a US State choropleth from ACS data
#'
#' Creates a choropleth of US states using the US Census' American Community
#' Survey (ACS) data.
#' @param variable The variable you wish to plot. A list of available census
#'   variables can be obtained using tidycensus::load_variables()
#' @param tableId Alternatively, you may specify the ACS table you wish to plot.
#'   If the table has more than one variable inside it, you must also specify
#'   the index of the column you wish to plot.
#' @param column_idx The index of the desired column within the table.
#' @param endyear The end year of the survey to use.
#' @param span Either 1, 3, or 5, the ACS vintage you wish to use.
#' @param title A title for the plot; if not specified, a title will be assigned
#'   based on the variable.
#' @param census_api_key Optional. Census API keys can be obtained at: \cr
#'   https://api.census.gov/data/key_signup.html
#' @param ... Other arguments passed to state_choropleth; see
#'   ?state_choropleth()
#' @return A choropleth.

#' @export
#' @examples
#' \donttest{
#' # Create a state choropleth for median household income zooming in
#' # on New York, New Jersey and Connecticut
#' state_choropleth_acs(variable = "B19013_001", endyear = 2011, num_colors=1,
#' zoom=c("new york", "new jersey", "connecticut"))
#'}
#'
#' @importFrom tidycensus load_variables get_acs
state_choropleth_acs = function(variable = NULL, tableId = NULL, column_idx = NULL,
                                endyear, span = 5, title = NULL, census_api_key = NULL,
                                ...)
{
  if (F) {
    variable = 'B19013_001'
    tableId = NULL
    column_idx = NULL
    map = 'state'
    endyear = 2014
    span = 5
    census_api_key = NULL  
  }
  acs_out = get_acs_data(variable = variable, tableId = tableId, column_idx = column_idx,
                         map = 'state', endyear = endyear,
                         span = span, census_api_key = census_api_key)
  
  if (is.null(title)) {
    title = acs_out$title
  } 
  
  # Subset for our map
  df = acs_out$df[, c('NAME', 'estimate')]
  names(df) = c('region', 'value')
  df$region = tolower(df$region)
  df = df[df$region != 'puerto rico', ]
  state_choropleth(df = df, title = title, ...)
}

#' Create a US County choropleth from ACS data
#'
#' Creates a choropleth of US counties using the US Census' American Community
#' Survey (ACS) data.
#' @param variable The variable you wish to plot. A list of available census
#'   variables can be obtained using tidycensus::load_variables()
#' @param tableId Alternatively, you may specify the ACS table you wish to plot.
#'   If the table has more than one variable inside it, you must also specify
#'   the index of the column you wish to plot.
#' @param column_idx The index of the desired column within the table.
#' @param endyear The end year of the survey to use.
#' @param span Either 1, 3, or 5, the ACS vintage you wish to use.
#' @param title A title for the plot; if not specified, a title will be assigned
#'   based on the variable.
#' @param census_api_key Optional. Census API keys can be obtained at: \cr
#'   \url{https://api.census.gov/data/key_signup.html}
#' @param ... Other arguments passed to county_choropleth; see
#'   ?county_choropleth()
#' @return A choropleth.

#' @export
#' @examples
#' \donttest{
#' #  Median household income, zooming in on all counties in New York, New
#' #  Jersey and Connecticut
#' county_choropleth_acs(variable = "B19013_001", num_colors=1, endyear = 2011,
#' state_zoom=c("new york", "new jersey", "connecticut"))
#' }
#' @importFrom tidycensus load_variables get_acs
county_choropleth_acs = function(variable = NULL, tableId = NULL, column_idx = NULL,
                                 endyear, span = 5, title = NULL, 
                                 census_api_key = NULL, ...)
{
  acs_out = get_acs_data(variable = variable, tableId = tableId, column_idx = column_idx,
                         map = 'county', endyear = endyear,
                         span = span, census_api_key = census_api_key)
  if (is.null(title)) {
    title = acs_out$title
  } 
  
  # Subset for our map
  df = acs_out$df
  df$GEOID = as.numeric(df$GEOID)
  df = df[, c('GEOID', 'estimate')]
  names(df) = c('region', 'value')
  df$value = as.numeric(df$value)
  county_choropleth(df = df, title = title, ...)
}

#' Use tidycensus to obtain the data needed to create a choropleth map.
#' @param variable The variable you wish to plot. A list of available census
#'   variables can be obtained using tidycensus::load_variables()
#' @param tableId Alternatively, you may specify the ACS table you wish to plot.
#'   If the table has more than one variable inside it, you must also specify
#'   the index of the column you wish to plot.
#' @param column_idx The index of the desired column within the table.
#' @param endyear The end year of the survey to use.
#' @param map The type map you wish to create; either 'state', 'county', 'zip',
#'   or 'tract'
#' @param span Either 1, 3, or 5, the ACS vintage you wish to use.
#' @param census_api_key Optional. Census API keys can be obtained at: \cr
#'   https://api.census.gov/data/key_signup.html
#' @param include_moe Whether to include the 90 percent margin of error.
#' @export
#' @importFrom tidycensus load_variables get_acs
get_acs_data = function(variable = NULL, tableId = NULL, column_idx = NULL, 
                        map, endyear, span, census_api_key, include_moe = FALSE) {

  if (is.null(tableId) & is.null(variable)) {
    stop('Must specify either tableId or variable')
  }
  
  if (!is.null(tableId) & !is.null(variable)) {
    stop('Must specify either tableId or variable but not both')
  }
  
  if (length(tableId) > 1 | length(variable) > 1) {
    stop('Only a single tableId or variable can be requested at one time.')
  }
  
  span_lookup = c('1' = 'acs1', '3' = 'acs3', '5' = 'acs5')
  dataset = span_lookup[as.character(span)]
  allvars = as.data.frame(tidycensus::load_variables(year = endyear, dataset = dataset, cache = TRUE))
  
  # Case 1: User specifies variable
  if (!is.null(variable)) { 
    if(!variable %in% allvars$name) {
      stop(paste0('The requested variable was not found in the ', dataset, ' dataset.'))
    } 
    acs_df = as.data.frame(tidycensus::get_acs(geography = map, variable = variable, year = endyear, cache_table = FALSE))
  }
  
  # Case 2: User specifies tableId
  if (!is.null(tableId)) { 
    acs_df = as.data.frame(tidycensus::get_acs(geography = map, table = tableId, year = endyear, cache_table = FALSE))
    table_varnames = unique(acs_df$variable)
    nvar = length(table_varnames)
    # If user specifies column_idx, use this column assuming it exists.
    if (!is.null(column_idx)) {
      if (column_idx <= nvar) {
        variable = table_varnames[column_idx]
      } else {
        stop(paste0('Column ', column_idx, ' was requested but the table only contains ', nvar, ' columns.'))
      }
    # If no column is specified, insist that that table only has a single variable.
    } else {
      if (length(table_varnames) > 1) {
        stop('The requested table contains more than one column; please specify the desired column with column_idx.')
      } 
      variable = table_varnames
    }
  }
  stopifnot(c('GEOID', 'NAME', 'variable', 'estimate') %in% names(acs_df))
  acs_df = acs_df[acs_df$variable == variable,]
  if(!include_moe & !is.null(acs_df$moe)) {
    acs_df$moe = NULL
  }
  var_label = allvars[allvars$name == variable, 'label'][[1]]
  var_label = gsub('!!', ' ', var_label)
  var_concept = allvars[allvars$name == variable, 'concept'][[1]]
  map_title = paste(var_concept, var_label)
  return(list(df = acs_df, title = map_title))
}

