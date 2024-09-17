#' @title A function to create a map of all school districts in a state
#' @name sd_map
#' @description This function allows you to create a map of all school
#'   districts, in each state in the United States, symbolized by selected
#'   variables from the EdBuild master dataset.
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2019. Defaults to 2019.
#' @param state The state for which you want to map school districts.  Defaults
#'   to New Jersey.
#' @param county The county for which you want to map school districts.
#'   Defaults to NULL. To view a full list of counties use
#'   \code{sd_shapepull(year = "2019", with_data = TRUE)}
#' @param map_var Variable by which to symbolize the map. \itemize{
#'   \item{\code{Student Poverty} colors by student poverty rate}
#'   \item{\code{Total Revenue} colors by state and local revenue per pupil}
#'   \item{\code{Local Revenue} colors by local revenue per pupil} \item
#'   {\code{State Revenue} colors by state revenue per pupil} \item
#'   {\code{Percent Nonwhite} colors by percent nonwhite enrollment} \item
#'   {\code{Median Household Income} colors by median household income } \item{
#'   \code{Median Property Value} colors by owner-occupied median property value
#'   } \item {\code{FRL} colors by free and reduced price lunch rate} } Defaults
#'   to \code{Student Poverty}
#' @param level Selects which level of school districts you want displayed in
#'   the map. \itemize{ \item{\code{elem} {displays elementary and unified
#'   districts}} \item{\code{secon} {displays secondary and unified districts}}}
#'   Defaults to \code{elem}.
#' @param legend If TRUE, legend is visible. Defaults to TRUE.
#' @keywords school districts map EdBuild
#' @usage sd_map(data_year = "2019", state="New Jersey", county = NULL, map_var = "Student Poverty",
#'   level = "elem", legend= TRUE)
#' @import dplyr sf magrittr
#' @importFrom tmap tm_shape tm_fill tm_borders tm_layout
#' @return An image of the map which can be written out with
#'   \code{tmap::tmap_save(map, '~/Documents/map.png')}
#' @seealso \code{\link{sd_neighbor_map}}
#' @export
#' @examples
#' \donttest{map <- sd_map(state="Georgia", map_var = "Percent Nonwhite",
#'  level = "elem", legend= TRUE)}


sd_map <- function (data_year = "2019", state="New Jersey", county = NULL,  map_var = "Student Poverty", level = "elem", legend= TRUE) {
  shape <- sd_shapepull(data_year = data_year, with_data = TRUE)

  states <- shape
  sf::st_geometry(states) <- NULL

  states_list <- as.list(levels(as.factor(states$State)))

  county_list <- as.list(levels(as.factor(states$County)))

  state_shape <- shape %>%
    dplyr::mutate(NAME = as.character(NAME),
                  FIPS = as.character(FIPS),
                  State = as.character(State),
                  Postal = as.character(Postal),
                  sdType= as.character(sdType)) %>%
    dplyr::filter(State == state) %>%
    dplyr::mutate(frl_rate = dplyr::case_when(frl_rate > 1 ~ NA_real_,
                                              TRUE ~ as.double(frl_rate)),
                  MPV = as.numeric(MPV),
                  MHI = as.numeric(MHI))

  pos_vars <- list("Student Poverty", "Percent Nonwhite", "FRL", "Total Revenue", "Local Revenue",
                   "State Revenue", "Median Household Income", "Median Property Value")

  shape.clean <- sf::st_make_valid(state_shape) # making all geometries valid

  if(map_var %in% pos_vars == FALSE) {

    message("Error: School district mapping is only available for the following variables: Student Poverty, Percent Nonwhite, FRL, Total Revenue, Local Revenue, State Revenue, Median Household Income and Median Property Value. FRL is not available for 2019.")
  }

  else if (state %in% states_list == FALSE) {
    message("Error: Please check your spelling of the state.")
  }

  else if (level != "elem" & level != "secon") {
    message("Error: Please select the school district level you would like displayed.
              'elem' displays elementary and unifed districts.
              'secon' displays secondary and unified distrcts.")
  }
  else {
    if(map_var == "Student Poverty"){
      colors <- c('#dff3fe', '#92DCF0', '#49B4D6', '#2586a5', '#19596d')
      title_name = "Student Poverty"
      variable = "StPovRate"
      breaks = c(0, .1, .2, .3, .4, 1)
      legend_format = list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), " %"))
      format_color = '#2586a5'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/student_poverty_rate_map.png')")
    }
    else if(map_var == "Total Revenue"){
      colors <- c('#edf8fb', '#b2e2e2', '#66c2a4', '#277f4d', '#1a5f38', '#0f3720', '#001107')
      title_name = "Total Revenue Per Pupil"
      variable = "SLRPP"
      breaks = c(0, 7500, 10000, 12500, 15000, 20000, 100000, 1000000)
      #legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#277f4d'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/total_revenue_pp_map.png')")

    }
    else if(map_var == "Local Revenue"){
      colors <- c('#e5f5f9', '#ccece6', '#99d8c9', '#66c2a4', '#41ae76', '#238b45', '#006d2c', '#00441b', '#001107')
      title_name = "Local Revenue Per Pupil"
      variable = "LRPP"
      breaks = c(0, 1000, 2500, 5000, 7500, 10000, 15000, 25000, 100000, 1000000)
      # legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#277f4d'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/local_revenue_pp_map.png')")

    }
    else if(map_var == "State Revenue"){
      colors <- c('#e5f5f9', '#ccece6', '#99d8c9', '#66c2a4', '#41ae76', '#238b45', '#006d2c', '#00441b', '#001107')
      title_name = "State Revenue Per Pupil"
      variable = "SRPP"
      breaks = c(0, 1000, 2500, 5000, 7500, 10000, 15000, 25000, 100000, 1000000)
      # legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#277f4d'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/state_revenue_pp_map.png')")

    }
    else if(map_var == "Median Household Income"){
      colors <- c('#d73027', '#e55d30', '#f28648', '#f9b761', '#ffffbf','#c6e7ef', '#73b9d1', '#4483ba', '#275ea5')
      title_name = "Median Household Income"
      variable = "MHI"
      breaks = c(0, 15000, 25000, 40000, 50000, 65000, 85000, 100000, 150000, 2000001)
      # legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#73b9d1'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/median_household_income_map.png')")

    }
    else if(map_var == "Median Property Value"){
      colors <- c('#bf812d', '#dfc27d', '#f6e8c3', '#f5f5f5', '#c7eae5', '#80cdc1', '#35978f', '#01665e')
      title_name = "Median Property Value"
      variable = "MPV"
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 2000001)
      # legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#80cdc1'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/median_property_value_map.png')")

    }
    else if(map_var == "FRL" & data_year == "2019"){
      message("Error: FRL data is  not available for 2019. To map FRL rates, please use data_year 2018.")
    }
    else if(map_var == "FRL"){
      colors <- c('#e3b1a5', '#ce7965',  '#c2573e', '#823a29', '#4e2319')
      title_name = "FRL Rate"
      variable = "frl_rate"
      breaks = c(0, .2, .4, .6, .8, 1)
      legend_format = list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), " %"))
      format_color = '#823a29'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/frl_rate_map.png')")

    }
    else if(map_var == "Percent Nonwhite"){
      colors <- c('#f2f0f7', '#cbc9e2', '#9c7dc4', '#7d52b7', '#54278f')
      title_name = "Percent Nonwhite"
      variable = "pctNW"
      breaks = c(0, .2, .4, .6, .8, 1)
      legend_format = list(fun=function(x) paste0(formatC(x*100, digits=0, format="f"), " %"))
      format_color = '#7d52b7'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/percent_nonwhite_map.png')")

    }
    else {
      message("Error: School district mapping is only available for the following variables: Student Poverty, Percent Nonwhite, FRL, Total Revenue, Local Revenue, State Revenue, Median Household Income and Median Property Value")
    }

    if(level == "elem") {
      shape.clean <- shape.clean %>%
        dplyr::filter(sdType == "elem" | sdType == "uni")
    }

    else if(level == "secon") {
      shape.clean <- shape.clean %>%
        dplyr::filter(sdType == "secon" | sdType == "uni")
    }

    if(!is.null(county)) {

      if (county %in% county_list == FALSE) {
        message("Error: Please check your spelling of the county. To view correct county spellings use sd_shapepull(with_data = TRUE)")
      }
      else if (county %in% county_list == TRUE){

        county_shape <- shape.clean %>%
        dplyr::filter(County == county)

      map <- tmap::tm_shape(county_shape) +
        tmap::tm_fill(variable, breaks=breaks, title = title_name,
                      palette = colors,
                      legend.format=legend_format) +
        tmap::tm_shape(county_shape) +
        tmap::tm_borders(lwd=.15, col = "#cfcccc", alpha = 1) +
        tmap::tm_layout(bg.color = NA,
                        main.title = paste0(county, ", ", state),
                        main.title.position = "center",
                        main.title.color = format_color,
                        legend.position = c("left", "bottom"),
                        legend.text.color = format_color,
                        legend.title.color = format_color,
                        frame = FALSE,
                        legend.outside = TRUE,
                        legend.outside.position = "left",
                        legend.outside.size = .5,
                        legend.text.size = .6 ) +
        tmap::tm_layout(legend.show = legend)

      }
    }
    else {

    map <- tmap::tm_shape(shape.clean) +
      tmap::tm_fill(variable, breaks=breaks, title = title_name,
                    palette = colors,
                    legend.format=legend_format) +
      tmap::tm_shape(shape.clean) +
      tmap::tm_borders(lwd=.15, col = "#cfcccc", alpha = 1) +
      tmap::tm_layout(bg.color = NA,
                      main.title = state,
                      main.title.position = "center",
                      main.title.color = format_color,
                     # legend.position = c("left", "bottom"),
                      legend.text.color = format_color,
                      legend.title.color = format_color,
                      frame = FALSE,
                      legend.outside = TRUE,
                      legend.outside.position = "left",
                     legend.outside.size = .5,
                     legend.text.size = .6
                     # outer.margins = c(0,0,0,0)
                     ) +
      tmap::tm_layout(legend.show = legend)
    }
     map  ### view the map
  }
}
