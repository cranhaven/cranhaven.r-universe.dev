#' @title A function to create a map of a school district and its neighbors
#' @name sd_neighbor_map
#' @description This function allows you to create a map of any school
#'   district with its neighbors symbolized by a selected variable.
#' @usage sd_neighbor_map(data_year = "2019", school_district = NULL,
#'  map_var = "Student Poverty", legend= TRUE, type = "like")
#' @param data_year Four digit year of master data to pull in. Options include
#'   2013- 2019. Defaults to 2019.
#' @param school_district Seven digit NCESID of the school district. Default is
#'   NULL.
#' @param map_var Variable by which to symbolize the map.
#' \itemize{
#' \item {\code{Student Poverty} colors by student poverty rate}
#'   \item {\code{Total Revenue} colors by state and local revenue per pupil}
#'   \item{ \code{Local Revenue} colors by local revenue per pupil}
#'   \item {\code{State Revenue} colors by state revenue per pupil}
#'   \item {\code{Percent Nonwhite} colors by percent nonwhite enrollment}
#'   \item {\code{Median Household Income} colors by owner-occupied median household income}
#'  \item {\code{Median Property Value} colors by median property value}
#'   \item {\code{FRL} colors by free and reduced price lunch rate}
#'   }
#'   Defaults to \code{Student Poverty}
#' @param legend If TRUE, legend is visible. Defaults to TRUE.
#' @param type Indicate which types of neighbors to return.
#'   Defaults to "like", returning a map of neighbors of the same
#'   district type (unified to unified, elementary to elementary and
#'   secondary to secondary). To view all neighbors use "all". This
#'   becomes important for districts like Chicago which have upwards of 50
#'   neighboring school districts, but only 1 type-like neighbor.
#'   Chicago is a unified district with 1 unified neighbor,
#'   16 secondary neighbors, and 32 elementary neighbors.
#' @keywords neighbors map EdBuild
#' @import dplyr stringr magrittr sf
#' @importFrom tmap tm_shape tm_fill tm_borders tm_layout
#' @return An image of the map which can be written out with
#'   \code{tmap::tmap_save(map, '~/Documents/map.png')}
#' @seealso \code{\link{sd_map}}
#' @export
#' @examples
#' \donttest{map <- sd_neighbor_map(data_year = "2019", school_district = "2901000", "Percent Nonwhite")}

sd_neighbor_map = function(data_year = "2019", school_district = NULL, map_var = "Student Poverty", legend= TRUE, type = "like"){

  shape <- sd_shapepull(data_year = data_year, with_data = TRUE)

  ncesid <- shape$GEOID

  pos_vars <- list("Student Poverty", "Percent Nonwhite", "FRL", "Total Revenue", "Local Revenue",
                   "State Revenue", "Median Household Income", "Median Property Value")
  if(map_var %in% pos_vars == FALSE) {

    message("Error: School district mapping is only available for the following variables: Student Poverty, Percent Nonwhite, FRL, Total Revenue, Local Revenue, State Revenue, Median Household Income and Median Property Value")
  }

  else if(is.null(school_district)) {
    message( "To generate a table, specify a school district by its NCESID.
    If you do not know the NCESID of a school district, use sd_shapepull() to search for your district.")
  }

  else if(school_district %in% ncesid == FALSE) {
    message( "The district you specified is not available. To generate a table, specify a school district by its NCESID.
    If you do not know the NCESID of a school district, use sd_shapepull() to search for your district.")
  }

  else if(map_var == "FRL" & data_year == "2019"){
    message("Error: FRL data is  not available for 2019. To map FRL rates, please use data_year 2018.")
  }

  else  {
    sd_type <- as.data.frame(shape) %>%
      dplyr::select(-geometry) %>%
      dplyr::select(GEOID, sdType)

    shape.clean <- sf::st_make_valid(shape) # making all geometries valid

    pairs_year = case_when(data_year == "2013" | data_year == "2014" ~ "1314",
                           data_year == "2015" | data_year == "2016" ~ "1516",
                           data_year == "2017" | data_year == "2018" ~ "1617",
                           data_year == "2019" ~ "1819",
                           TRUE ~ "1819")

    pairs_url = paste("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs", paste(pairs_year, ".csv", sep=""), sep ="_")
    pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE) %>%
      dplyr::mutate(GEOID = as.character(stringr::str_pad(GEOID, width = 7, "left", pad = "0")),
                    GEOID.1 = as.character(stringr::str_pad(GEOID.1, width = 7,  "left", pad = "0"))) %>%
      dplyr::left_join(sd_type, by = "GEOID") %>%
      dplyr::rename(sdtype_1 = sdType) %>%
      dplyr::left_join(sd_type, by = c("GEOID.1" = "GEOID")) %>%
      dplyr::rename(sdtype_2 = sdType) %>%
      dplyr::mutate(FIPS_1 = substr(GEOID, 0, 2),
                    FIPS_2 = substr(GEOID.1, 0, 2)) %>%
      dplyr::filter(FIPS_1 == FIPS_2,      ## filtering out pairs where they are across state lines
                    length >= 152.4) %>%     ## filtering out pairs where the border is less than 500 feet
      dplyr::filter(GEOID == school_district)

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
      #legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#277f4d'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/local_revenue_pp_map.png')")

    }
    else if(map_var == "State Revenue"){
      colors <- c('#e5f5f9', '#ccece6', '#99d8c9', '#66c2a4', '#41ae76', '#238b45', '#006d2c', '#00441b', '#001107')
      title_name = "State Revenue Per Pupil"
      variable = "SRPP"
      breaks = c(0, 1000, 2500, 5000, 7500, 10000, 15000, 25000, 100000, 1000000)
      #legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#277f4d'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/state_revenue_pp_map.png')")

    }
    else if(map_var == "Median Household Income"){
      colors <- c('#d73027', '#e55d30', '#f28648', '#f9b761', '#ffffbf','#c6e7ef', '#73b9d1', '#4483ba', '#275ea5')
      title_name = "Median Household Income"
      variable = "MHI"
      breaks = c(0, 15000, 25000, 40000, 50000, 65000, 85000, 100000, 150000, 2000001)
      #legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#73b9d1'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/median_household_income_map.png')")

    }
    else if(map_var == "Median Property Value"){
      colors <- c('#bf812d', '#dfc27d', '#f6e8c3', '#f5f5f5', '#c7eae5', '#80cdc1', '#35978f', '#01665e')
      title_name = "Median Property Value"
      variable = "MPV"
      breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 2000001)
      #legend_format = list(fun=function(x) paste0("$ ", formatC(x, digits=0, format="f", big.mark = ",")))
      legend_format = list(fun = function(x) paste0("$", x/1000, "k"))
      format_color = '#80cdc1'

      message("NOTE:: save your map to the desired location using: tmap::tmap_save(map, bg = 'transparent', '~/Documents/median_property_value_map.png')")

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
      message("Error: School district mapping is only available for the following variables: Student Poverty, Percent Nonwhite, FRL, Total Revenue, Median Household Income and Median Property Value")
    }

    if(type == "like"){

      pairs <- pairs %>%
        dplyr::filter(sdtype_1 == sdtype_2)

      GEOIDS <- c(pairs$GEOID.1, school_district)

      shape_small <- shape %>%
        dplyr::filter(GEOID %in% GEOIDS) %>%
        mutate(MPV = as.numeric(MPV),
                MHI = as.numeric(MHI))

      selected <- shape_small %>%
        dplyr::filter(GEOID == school_district)

      tmap::tm_shape(shape_small) +
        tmap::tm_fill(variable, breaks=breaks, title = title_name,
                      palette = colors,
                      legend.format=legend_format) +
        tmap::tm_shape(shape_small) +
        tmap::tm_borders(lwd=.5, col = "#000000", alpha = 1) +
        tmap::tm_shape(selected) +
        tmap::tm_borders(lwd=1.5, col = "#000000", alpha = 1) +
        tmap::tm_layout(bg.color = NA,
                        legend.position = c("left", "bottom"),
                        legend.text.color = format_color,
                        legend.title.color = format_color,
                        main.title = paste0(selected$NAME, ", ", selected$Postal),
                        main.title.color = format_color,
                        frame = FALSE,
                        legend.outside = TRUE,
                        legend.outside.position = "left",
                        legend.outside.size = .4,
                        legend.text.size = .5) +
        tmap::tm_layout(legend.show = legend)


    }

    else if (type == "all"){

      GEOIDS <- c(pairs$GEOID.1, school_district)

      shape_small <- shape %>%
        dplyr::filter(GEOID %in% GEOIDS) %>%
        mutate(MPV = as.numeric(MPV),
               MHI = as.numeric(MHI))

      selected <- shape_small %>%
        dplyr::filter(GEOID == school_district)

      tmap::tm_shape(shape_small) +
        tmap::tm_fill(variable, breaks=breaks, title = title_name,
                      palette = colors,
                      legend.format=legend_format) +
        tmap::tm_shape(shape_small) +
        tmap::tm_borders(lwd=.5, col = "#000000", alpha = 1) +
        tmap::tm_shape(selected) +
        tmap::tm_borders(lwd=1.5, col = "#000000", alpha = 1) +
        tmap::tm_layout(bg.color = NA,
                        legend.position = c("left", "bottom"),
                        legend.text.color = format_color,
                        legend.title.color = format_color,
                        main.title = paste0(selected$NAME, ", ", selected$Postal),
                        main.title.color = format_color,
                        frame = FALSE,
                        legend.outside = TRUE,
                        legend.outside.position = "left",
                        legend.outside.size = .4,
                        legend.text.size = .5) +
        tmap::tm_layout(legend.show = legend)
    }
  }
}
