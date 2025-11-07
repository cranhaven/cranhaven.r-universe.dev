#' Make ggplotly and add legend with color as title
#'
#' This function creates a Plotly version of a ggplot2 object and adds a legend with the user-friendly name of the color variable as its title.
#'
#' @param plot A ggplot object.
#' @param color A string specifying the column name to be used as the color variable.
#' @param mapping_table A named list with as name the original colum name and as value the display
#' name
#' @return A plotly object with a formatted legend.
#' @export
#' @examples
#' df <- data.frame(x_var = rnorm(100),
#'                  y_var = rnorm(100),
#'                  color_var = sample(c("Red", "Blue"),
#'                  100,
#'                  replace = TRUE))
#' xlab_setting <- ggplot2::xlab("x label")
#' ylab_setting <- ggplot2::ylab("y label")
#' ggplot_instellingen <- ggplot2::geom_point()
#' scale_y <- ggplot2::scale_y_continuous()
#' plot <- basic_plot(df, "x_var", "y_var", "color_var", xlab_setting,
#'                    ylab_setting, ggplot_instellingen, "none", scale_y)
#' mapping_table <- list(color_var = "user friendly name var")
#' plotly_object <- ggplotly_with_legend(plot, "color_var", mapping_table)
ggplotly_with_legend <- function(plot, color, mapping_table) {
  plot <- plotly::ggplotly(plot) %>%
    plotly::layout(
      legend =
        list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 1.20,
          title = list(text = display_name(color, mapping_table))
        )
    )

  plot <- clean_pltly_legend(plot)

  return(plot)
}


#' Clean the legend of a plotly object
#'
#' This function cleans the legend of a plotly object by removing unnecessary duplication.
#' It is specifically designed to work around a bug that causes facet_wrap to create a separate legend entry for each facet.
#'
#' @param pltly_obj A plotly object with a legend to be cleaned.
#' @param new_legend An optional vector of strings specifying new legend entries. Default is an empty vector.
#' @return The input plotly object with its legend cleaned.
clean_pltly_legend <- function(pltly_obj, new_legend = c()) {

  ## Assigns a legend group from the list of possible entries
  assign_leg_grp <- function(legend_group, leg_nms) {
    leg_nms_rem <- leg_nms

    ## Assigns a .leg_name, if possible
    ## leg_options is a 2-element list: 1 = original value; 2 = remaining options
    parse_leg_nms <- function(leg_options) {
      ## No more legend names to assign
      if (is.na(leg_options)) {
        leg_options
      } else if (length(leg_nms_rem) == 0) {
        leg_options
      } else {
        ## Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]

        leg_nm_new
      }
    }

    legend_group %>%
      purrr::map(~ parse_leg_nms(.))
  }

  ## Simplifies legend groups by removing brackets, position numbers and then de-duplicating
  simplify_leg_grps <- function(legendgroup_vec) {
    leg_grp_cln <-
      purrr::map_chr(legendgroup_vec, ~ stringr::str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))

    purrr::modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~NA_character_)
  }

  pltly_obj_data <-
    pltly_obj$x$data

  ## pltly_leg_grp is a character vector where each element represents a legend group. Element is NA
  ## if legend group not required or doesn't exist
  pltly_leg_grp <- pltly_obj_data %>%
    purrr::map(~ purrr::pluck(., "legendgroup")) %>%
    ## Elements where showlegend = FALSE have legendgroup = NULL
    purrr::map_chr(~ if (is.null(.)) {
      NA_character_
    } else {
      .
    }) %>%
    simplify_leg_grps() %>%
    assign_leg_grp(new_legend)

  pltly_obj_data_new <-
    pltly_obj_data %>%
    purrr::map2(pltly_leg_grp, ~ purrr::list_modify(.x, legendgroup = .y)) %>%
    purrr::map2(pltly_leg_grp, ~ purrr::list_modify(.x, name = .y)) %>%
    ## Set show legend FALSE when there is no legend
    purrr::map2(pltly_leg_grp, ~ purrr::list_modify(.x, showlegend = !is.na(.y)))

  ## Update orginal plotly object
  pltly_obj$x$data <- pltly_obj_data_new

  return(pltly_obj)
}
