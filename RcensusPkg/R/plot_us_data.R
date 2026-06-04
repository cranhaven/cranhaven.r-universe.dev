#' @title plot_us_data
#'
#' @description This function produces a ggplot2 based choropleth map of a discrete/continuous variable across all/selected US states
#'   including placement of Alaska, Hawaii, and Puerto Rico. The function accepts a data frame with a column of
#'   state names and a column with their respective values.  If the data frame (parameter 'df') is submitted as \code{NULL} then only the simple
#'   feature state geometries are returned for mapping. The function offers several options for
#'   control/selection of state geographies and variable scaling.
#'
#' This function depends extensively on \code{RcensusPkg::tiger_states_sf()} for obtaining state geometries, so many of
#'   that function's parameters are repeated in this function. Also \code{RplotterPkg::create_sf_plot()} is called upon for
#'   displaying the shapefile geometries, so this package should be installed.
#'
#' @param df The data frame with a column of full state names and a second variable column of their respective values.
#'   The column name for the states must be "NAME". If 'df' is \code{NULL}, then only a sf object with the state geometries are returned.
#' @param states_col A required string (if 'df' is not \code{NULL}) that sets the column name from 'df' containing the state names of interest. These are
#'   full state names, either capitalized or lower case.
#' @param value_col A required string (if 'df' is not \code{NULL}) that sets the column name from 'df' where values(discrete or continuous) are defined.
#'   If the column has discrete values then it must be a factor.
#' @param title A string that sets the plot title.
#' @param title_fontsz A numeric that sets the title's font size. The default is 18.
#' @param text_col An optional string that sets the column name from 'df' for labeling each state polygon.
#' @param text_size A numeric value that sets the size of labeled state text.
#' @param text_color A string that sets the color of labeled state text.
#' @param text_fontface A string that sets the fontface of labeled state text.
#'  Acceptable values: "plain", "bold", "italic", "bold.italic". The default is "plain".
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is \code{TRUE}.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if \code{TRUE} will download a less detailed, more generalized version of the state geometries.
#' @param resol If 'general' is \code{TRUE}, then the resolution to return. Acceptable values are strings
#'   "500k", "5m", "20m".
#' @param na_rm A logical which if \code{TRUE}, missing observations are removed. If \code{FALSE}, the default,
#'   missing observations are removed with a warning.
#' @param scale_breaks A required string/numeric vector that defines the scale breaks.
#' @param scale_values A string/numeric vector that defines the possible values. For factor values, this is required
#'   and is a vector string of colors.
#' @param scale_limits A required string/numeric vector that defines the scale limits.
#' @param scale_labels An optional string vector that defines the scale labels. Vector must be the same length
#' as \code{scale_breaks}.
#' @param scale_colors Vector of colors to use for n-color gradient.
#' @param scale_na_value A string that sets the color for missing values.
#' @param own_scale A logical which if \code{TRUE}, then your own scaling may be appended to the plot without using the above
#'   scale_* parameters.
#' @param sf_color A string that sets the polygon border line color.
#' @param sf_fill A string that sets the polygon area fill color.
#' @param sf_linewidth A numeric that sets the border line thickness.
#' @param sf_alpha A numeric that sets the alpha level attribute of the polygon fill.
#' @param display_plot A logical that if \code{TRUE} will display the plot. The default is \code{TRUE}.
#'   If \code{FALSE}, then a ggplot2 object is returned
#' @param show_legend A logical that controls the appearance of the legend.
#' @param legend_pos A string that sets the legend position. Acceptable values are "right",
#'  "top", "bottom".
#' @param legend_key_width A numeric that sets the legend width in cm.
#' @param legend_key_height A numeric that sets the legend height in cm.
#' @param legend_key_backgrd A string that sets the legend's background color.
#'
#' @return A list of ggplot2 objects if 'display_plot' is \code{FALSE}. Included in the
#'   list is the plot of all the states ("us_states") along with the original ggplot2
#'   \code{ggplot2::geom_sf} plots of the lower 48 ("lower_48"), Alaska ("alaska"), Hawaii ("hawaii") and Puerto Rico ("puerto_rico").
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(grid)
#' library(ggplot2)
#' library(ggplotify)
#' library(data.table)
#' library(gtable)
#' library(httr2)
#' library(withr)
#' library(RplotterPkg)
#'
#' # Plot just the states without joining data (the default case)
#' # Define a temporary output folder for the downloaded shapefiles
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#' us_without_data_plot <- RcensusPkg::plot_us_data(
#'  title = "A Default Mapping of US States",
#'  output_dir = output_dir,
#'  delete_files = FALSE
#' )
#'
#' # Requires US Census Bureau API key
#' # Plot of US map with discrete 2020 presidential results
#' output_dir <- withr::local_tempdir()
#' if(!dir.exists(output_dir)){
#'   dir.create(output_dir)
#' }
#' a_plot <- RcensusPkg::plot_us_data(
#'   df = RcensusPkg::vote2020,
#'   title = "US Presidential Vote 2020",
#'   states_col = "State",
#'   value_col = "Party",
#'   output_dir = output_dir,
#'   delete_files = FALSE,
#'   scale_breaks = c("R","D"),
#'   scale_limits = c("R","D"),
#'   scale_values = c("red","blue"),
#'   scale_labels = c("Republican","Democrat"),
#'   sf_color = "white"
#' )
#' }
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom RplotterPkg create_sf_plot
#' @importFrom ggplotify as.ggplot
#' @import ggplot2
#' @import httr2
#'
#' @export
plot_us_data <- function(
  df = NULL,
  states_col = NULL,
  value_col = NULL,
  title = NULL,
  title_fontsz = 18,
  text_col = NULL,
  text_size = 3.0,
  text_color = "black",
  text_fontface = "plain",
  output_dir = tempdir(),
  delete_files = TRUE,
  vintage = 2020,
  general = FALSE,
  resol = "500k",
  na_rm = FALSE,
  scale_breaks = NULL,
  scale_values = NULL,
  scale_limits = NULL,
  scale_labels = waiver(),
  scale_colors = grDevices::heat.colors(8),
  scale_na_value = "gray50",
  own_scale = FALSE,
  sf_color = "black",
  sf_fill = "gray",
  sf_linewidth = 0.1,
  sf_alpha = 1.0,
  display_plot = TRUE,
  show_legend = TRUE,
  legend_pos = "right",
  legend_key_width = 0.5,
  legend_key_height = 0.7,
  legend_key_backgrd = "white"
){
  # If data (i.e. df) is not NULL the check following:
  # Check if states_col and value_col have been defined
  # Check if scale_breaks and scale_limits have been defined
  if(!is.null(df)){
    if(is.null(states_col) | is.null(value_col)){
      stop("Both states_col and value_col parameters must be defined")
    }
    if(is.null(scale_breaks) | is.null(scale_limits)){
      stop("Both scale_breaks and scale_limits parameters must be defined")
    }
  }

  NAME <- NULL

  plots_lst <- list()
  sf_lst <- list()

  lower_48_crs <- 5070
  alaska_crs <- 4425
  hawaii_crs <- 26962
  pureto_crs <- 32161

  # Get the simple feature geometries for the states
  states_sf <- RcensusPkg::tiger_states_sf(
    vintage = vintage,
    resol = resol,
    general = TRUE,
    output_dir = output_dir,
    delete_files = delete_files
  ) |>
    data.table::as.data.table() |>
    _[, NAME := tolower(NAME)] |>
    sf::st_as_sf()

  # join states_sf with df
  if(!is.null(df)){
    dt <- data.table::as.data.table(df)
    # reformat the state names column so we're on the same page
    dt[, NAME := tolower(dt[[states_col]])]

    states_dt <- data.table::as.data.table(states_sf)

    data.table::setkeyv(dt, cols = "NAME")
    data.table::setkeyv(states_dt, cols = "NAME")
    data_sf <- states_dt[dt, nomatch = 0] |>
      sf::st_as_sf()
  }else {
    data_sf <- states_sf
  }

  # Remove AK,HI,PR from data_sf
  lower_48_states_sf <- data_sf  |>
    data.table::as.data.table() |>
    _[!NAME %in% c("alaska","hawaii","puerto rico",
                   "guam","commonwealth of the northern mariana islands",
                   "united states virgin islands","american samoa")
      ] |>
    sf::st_as_sf() |>
    sf::st_transform(lower_48_crs)

  # Get plot grob for lower 48 states
  lower_48_states_plot <- RplotterPkg::create_sf_plot(
    sf = lower_48_states_sf,
    aes_fill = value_col,
    hide_x_tics = TRUE,
    hide_y_tics = TRUE,
    sf_color = sf_color,
    sf_fill = sf_fill,
    sf_linewidth = sf_linewidth,
    sf_alpha = sf_alpha,
    scale_breaks = scale_breaks,
    scale_values = scale_values,
    scale_limits = scale_limits,
    scale_labels = scale_labels,
    scale_colors = scale_colors,
    scale_na_value = scale_na_value,
    own_scale = own_scale,
    show_legend = show_legend,
    legend_pos = legend_pos,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height,
    legend_key_backgrd = legend_key_backgrd
  ) + theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(rep(0.1,4),"cm"),
  )

  if(!is.null(text_col)) {
    lower_48_states_plot <- lower_48_states_plot |>
      RplotterPkg::create_sf_plot(
        sf = lower_48_states_sf,
        aes_text = text_col,
        text_size = text_size,
        text_color = text_color,
        text_fontface = text_fontface,
        sf_alpha = 0
      )
  }

  plots_lst[["lower_48"]] = lower_48_states_plot
  sf_lst[["lower_48"]] = lower_48_states_sf
  # Convert ggplot2 object to grob
  lower_48_states_grob <- ggplot2::ggplotGrob(lower_48_states_plot)

  # Get geometries/grobs for "outer" states
  alaska_grob <- NULL
  if("alaska" %in% data_sf$NAME){
    alaska_sf <- data_sf |>
      data.table::as.data.table() |>
      _[NAME == "alaska",] |>
      sf::st_as_sf() |>
      sf::st_transform(alaska_crs)

    alaska_plot <- RplotterPkg::create_sf_plot(
      sf = alaska_sf,
      aes_fill = value_col,
      hide_x_tics = TRUE,
      hide_y_tics = TRUE,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = FALSE
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm"),
      legend.position = "none"
    )

    if(!is.null(text_col)) {
      alaska_plot <- alaska_plot |>
        RplotterPkg::create_sf_plot(
          sf = alaska_sf,
          aes_text = text_col,
          text_size = text_size,
          text_color = text_color,
          text_fontface = text_fontface,
          sf_alpha = 0
        )
    }

    plots_lst[["alaska"]] <- alaska_plot
    sf_lst[["alaska"]] <- alaska_sf
    alaska_grob <- ggplot2::ggplotGrob(alaska_plot)
  }

  hawaii_grob <- NULL
  if("hawaii" %in% data_sf$NAME){
    hawaii_sf <- data_sf |>
      data.table::as.data.table() |>
      _[NAME == "hawaii",] |>
      sf::st_as_sf() |>
      sf::st_transform(hawaii_crs)
    hawaii_plot <- RplotterPkg::create_sf_plot(
      sf = hawaii_sf,
      aes_fill = value_col,
      hide_x_tics = TRUE,
      hide_y_tics = TRUE,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = FALSE
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm"),
      legend.position = "none"
    )

    if(!is.null(text_col)) {
      hawaii_plot <- hawaii_plot |>
        RplotterPkg::create_sf_plot(
          sf = hawaii_sf,
          aes_text = text_col,
          text_size = text_size,
          text_color = text_color,
          text_fontface = text_fontface,
          sf_alpha = 0
        )
    }

    plots_lst[["hawaii"]] <- hawaii_plot
    sf_lst[["hawaii"]] <- hawaii_sf
    hawaii_grob <- ggplot2::ggplotGrob(hawaii_plot)
  }

  puerto_grob <- NULL
  if("puerto rico" %in% data_sf$NAME){
    puerto_sf <- data_sf |>
      data.table::as.data.table() |>
      _[NAME == "puerto rico",] |>
      sf::st_as_sf() |>
      sf::st_transform(pureto_crs)

    puerto_plot <- RplotterPkg::create_sf_plot(
      sf = puerto_sf,
      aes_fill = value_col,
      aes_text = text_col,
      text_size = text_size,
      text_color = text_color,
      text_fontface = text_fontface,
      hide_x_tics = TRUE,
      hide_y_tics = TRUE,
      sf_color = sf_color,
      sf_fill = sf_fill,
      sf_linewidth = sf_linewidth,
      sf_alpha = sf_alpha,
      scale_breaks = scale_breaks,
      scale_values = scale_values,
      scale_limits = scale_limits,
      scale_labels = scale_labels,
      scale_colors = scale_colors,
      scale_na_value = scale_na_value,
      own_scale = own_scale,
      show_legend = FALSE
    ) + theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(rep(0.1,4),"cm"),
      legend.position = "none"
    )

    if(!is.null(text_col)) {
      puerto_plot <- puerto_plot |>
        RplotterPkg::create_sf_plot(
          sf = puerto_sf,
          aes_text = text_col,
          text_size = text_size,
          text_color = text_color,
          text_fontface = text_fontface,
          sf_alpha = 0
        )
    }

    plots_lst[["puerto_rico"]] <- puerto_plot
    sf_lst[["puerto_rico"]] <- puerto_sf
    puerto_grob <- ggplot2::ggplotGrob(puerto_plot)
  }

  plots_table <- gtable::gtable(
    name = "plots_table",
    widths = grid::unit(rep(1,25),"null"),
    heights = unit(rep(1,20),"null")
  )
  # Debug arrangement
  #gtable::gtable_show_layout(plots_table)

  # Are we doing a title?
  if(!is.null(title)){
    title_grob <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = title_fontsz, fontface = 2L))
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        title_grob
      ),
      t = 7,
      l = 8,
      r = 20
    )
  }

  plots_table <- gtable::gtable_add_grob(
    plots_table,
    grobs = list(
      lower_48_states_grob
    ),
    t = 8,
    l = 4,
    r = 24,
    b = 19
  )

  if("alaska" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        alaska_grob
      ),
      t = 2,
      l = 2,
      r = 8,
      b = 8
    )
  }
  if("hawaii" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        hawaii_grob
      ),
      t = 15,
      l = 1,
      r = 5,
      b = 17
    )
  }
  if("puerto rico" %in% data_sf$NAME){
    plots_table <- gtable::gtable_add_grob(
      plots_table,
      grobs = list(
        puerto_grob
      ),
      t = 19.8,
      l = 21,
      r = 21.5,
      b = 20
    )
  }

  # for debug purposes
  # grid::grid.draw(plots_table)

  # Display plot table?
  a_plot <- ggplotify::as.ggplot(plots_table)
  plots_lst[["us_states"]] <- a_plot


  if(display_plot){
    return(a_plot)
  }else{
    return(list(
      plots = plots_lst,
      sf = sf_lst
    ))
  }
}
