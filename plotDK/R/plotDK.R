#' Generate choropleth maps of Danish Municipalities, Regions, Provinces and Zip-areas
#'
#' Draws a map that highligths any value of interest across either danish municipalities, regions, provinces or zip-codes. This is essentially 
#' a ggplot2-wrapper incorporating geoms of danish municipalities, regions, provinces and zipcodes. Therefore the output
#' is compatible with further ggplot2 elements. 
#' 
#' @param data A \code{data.frame} containing an ID-variable specifying either a municipality, region, province or zipcode (see \code{id}), as well as a value-variable 
#' containing any value to be plotted on the chosen level. 
#' @param id A \code{character} specifying the name of a column in data containing the ID on the chosen level. 
#' 
#' For municipalities these variables can be either;
#' * A character-variable with danish municipality names. For accepted values see \link[plotDK]{municipality_info}.
#' * A numeric/integer-variable with official municipality numbers. For accepted values see \link[plotDK]{municipality_info}. 
#' 
#' For regions these variables can either;
#' * A character-variable with danish region names. For accepted values see \link[plotDK]{region_info}.
#' * A numeric/integer-variable with danish region numbers. For accepted values see \link[plotDK]{region_info}.
#' 
#' For provinces these variables can be either;
#' * A character-variable with danish province names. For accepted values see \link[plotDK]{province_info}.
#' * A numeric/integer-variable with danish province numbers. For accepted values see \link[plotDK]{province_info}.
#' 
#' For zip-codes these variables can be;
#' * A numeric/integer-variable with danish zip-codes. For accepted values see \link[plotDK]{zipcode_info}.
#' 
#' @param value \code{numeric}-, \code{factor}- or \code{character}-variabel to be plotted on the map. Note that \code{character-variables} will be
#' naively translated to factors behind the scenes. For full control over levels, pre-convert to a factor.  
#' @param plotlevel \code{character}, indicating which level to plot. Valid options are "municipality", "region", "province", and "zipcode".
#' @param show_missing \code{logical}. Should levels not present in data or with NA-values be printed? This can be used to plot only a subset 
#' of entities. 
#' @param show_borders \code{logical}. Should geom borders be drawn?
#' @param interactive \code{logical}. Should the plot be converted to an interactive plotly plot?
#' @param titel \code{character}. Optional plot title. 
#' 
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot theme element_blank element_rect coord_map geom_polygon aes update_geom_defaults geom_path ggtitle
#' @importFrom rlang !! sym .data
#' @importFrom plotly ggplotly
#' @export
#' 
#' @md
#'
#' @examples 
#' 
#' ## Empty plot
#' plotDK()
#' 
#' province_data <- data.frame(
#'  province_name = c(
#'    "nordjylland", 
#'    "østjylland",
#'    "vestjylland", 
#'    "fyn", 
#'    "sydjylland", 
#'    "vest-ogsydsjælland",
#'    "østsjælland", 
#'    "københavnsomegn",
#'    "byenkøbenhavn", 
#'    "bornholm",
#'    "nordsjælland"
#'  ),
#'  value = 1:11, 
#'  stringsAsFactors = FALSE
#')
#' 
#' 
#' 
#' 
plotDK <- function(data = NULL, 
                   id = NULL, 
                   value = NULL, 
                   plotlevel = "municipality", 
                   show_missing = FALSE,
                   show_borders = TRUE,
                   interactive = FALSE,
                   titel = NULL) {
  
  check_input_data(
    data = data,
    id = id,
    value = value,
    plotlevel = plotlevel
  )
  
  plotdata <- create_plot_data(
    data = data,
    id = id,
    plotlevel = plotlevel,
    show_missing = show_missing
  )
  
  update_geom_defaults(
    "polygon", 
    list(
      fill = "lightgrey")
  )
  
  if(is.null(data)) {
    
    geom_aes <- aes(
      x = !!sym("long"),
      y = !!sym("lat"),
      text = !!sym("id"),
      group = !!sym("group"),
      subgroup = !!sym("hole"),
      fill = NULL
    )
    
  } else {
    geom_aes <- aes(
      x = !!sym("long"),
      y = !!sym("lat"),
      text = !!sym("id"),
      group = !!sym("group"),
      subgroup = !!sym("hole"),
      fill = !!sym(value)
    )
  }
  
  p <- ggplot(
    plotdata, 
    mapping = geom_aes
  ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        color = "white", 
        fill = "white"
      )
    ) 
  
  if(show_borders) {
    p <- p +
      geom_polygon(
        color = "black"
      )
  } else {
    p <- p +
      geom_polygon(
      )
  }
  
  p <- p + 
    coord_map()
  
  if(length(titel)) {
    p <- p +
      ggtitle(titel)
  }
  
  if(interactive) {
    p <- ggplotly(p, tooltip = c("text", "fill"))
  }
  
  p
}
