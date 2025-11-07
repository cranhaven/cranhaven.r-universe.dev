#' SHAP Summary Plot
#' 
#' A Function for obtaining a beeswarm plot, similar to the summary plot
#' in the `{shap}` python package.
#' 
#' This function allows the user to pass a data frame of SHAP values and
#' variable values and returns a ggplot object displaying a general summary
#' of the effect of Variable level on SHAP value by variable.
#' It is created with `{ggbeeswarm}`, and the returned value is a `{ggplot2}`
#' object that can be modified for given themes/colors.
#' 
#' Please note that for the `variable_values` and `shap_values` arguments,
#' both of which are data frames, the columns must be in the same order.
#' This is essential in assuring that the variable values and labels are 
#' matched to the correct shap values.
#'
#' @param variable_values A data frame of the  values of the variables that 
#'   caused the given SHAP values, generally will be the same data frame or 
#'   matrix that was passed to the model for prediction.
#' @param shap_values A data frame of shap values, either returned by `mshap()` 
#'   or obtained from the python `{shap}` module.
#' @param names A character vector of variable names, corresponding to the
#'   order of the columns in both `variable_values` and `shap_values`. If
#'   `NULL` (default), then the column names of the `variable_values` are 
#'   taken as `names`.
#' @param num_vars An integer specifying the number of variables to show in the
#'   plot, defaults to the 10 most important.
#' @param colorscale The color scale used for the color of the plot. It should
#'   be a character vector of length three, with the low color first, the 
#'   middle color second, and the high color third. These can be hex color 
#'   codes or colors recognized by `{ggplot2}`.
#' @param legend.position The position of the legend.  See `?ggplot2::theme` 
#'   for more information.
#' @param font_family A character string specifying the family of the text on 
#'   the plot. Defaults to Times New Roman.
#' @param title A character string specifying the title of the plot.
#' 
#' @return A `{ggplot2}` object
#' @export
#'
#' @examples
#' 
#' if (interactive()) {
#' library(mshap)
#' library(ggplot2)
#' 
#' # Generate fake data
#' set.seed(18)
#' dat <- data.frame(
#'   age = runif(1000, min = 0, max = 20),
#'   prop_domestic = runif(1000),
#'   model = sample(c(0, 1), 1000, replace = TRUE),
#'   maintain = rexp(1000, .01) + 200
#' )
#' shap <- data.frame(
#'   age = rexp(1000, 1/dat$age) * (-1)^(rbinom(1000, 1, dat$prop_domestic)),
#'   prop_domestic = -200 * rnorm(100, dat$prop_domestic, 0.02) + 100,
#'   model = ifelse(dat$model == 0, rnorm(1000, -50, 30), rnorm(1000, 50, 30)),
#'   maintain = (rnorm(1000, dat$maintain, 100) - 400) * 0.2
#' )
#' expected_value <- 1000
#' 
#' # A Basic sumary plot
#' summary_plot(
#'   variable_values = dat,
#'   shap_values = shap
#' )
#' 
#' # A Customized summary plot
#' summary_plot(
#'   variable_values = dat,
#'   shap_values = shap,
#'   legend.position = "bottom",
#'   names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
#'   colorscale = c("blue", "purple", "red"),
#'   font_family = "Arial",
#'   title = "A Custom Title"
#' )
#' 
#' # A basic observation plot
#' observation_plot(
#'   variable_values = dat[1,],
#'   shap_values = shap[1,],
#'   expected_value = expected_value
#' )
#' 
#' # A Customized Observation plot
#' observation_plot(
#'   variable_values = dat[1,],
#'   shap_values = shap[1,],
#'   expected_value = expected_value,
#'   names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
#'   font_family = "Arial",
#'   title = "A Custom Title",
#'   fill_colors = c("red", "blue"),
#'   connect_color = "black",
#'   expected_color = "purple",
#'   predicted_color = "yellow"
#' )
#' 
#' # Add elements to the returned object
#' # see vignette("mshap_plots") for more information
#' observation_plot(
#'   variable_values = dat[1,],
#'   shap_values = shap[1,],
#'   expected_value = expected_value,
#'   names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
#'   font_family = "Arial",
#'   title = "A Custom Title"
#' ) +
#'   geom_label(
#'     aes(y = 950, x = 4, label = "This is a really big bar!"),
#'     color = "#FFFFFF",
#'     fill = NA
#'   ) +
#'   theme(
#'     plot.background = element_rect(fill = "grey"),
#'     panel.background = element_rect(fill = "lightyellow")
#'   )
#' }
summary_plot <- function(
  variable_values, 
  shap_values, 
  names = NULL,
  num_vars = 10,
  colorscale = c("#A54657", "#FAF0CA", "#0D3B66"),
  legend.position = c(0.8, 0.2),
  font_family = "Times New Roman",
  title = "SHAP Value Summary"
) {
  
  if (is.null(names)) {
    names <- colnames(variable_values)
  }
  
  important_vars <- shap_values %>%
    magrittr::set_colnames(names) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "value",
      cols = colnames(.)
    ) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(avg_value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(avg_value) %>%
    dplyr::pull(variable)
  if (length(important_vars) > num_vars) {
    n <- length(important_vars)
    important_vars <- important_vars[(n - num_vars + 1):n]
  }
  
  variable_values <- variable_values %>%
    magrittr::set_colnames(names) %>%
    dplyr::select(dplyr::all_of(important_vars)) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        .fns = ~as.numeric(as.factor(.x))
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), 
        .fns = ~((.x - min(.x)) / (max(.x) - min(.x)))
      )
    ) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "var_val",
      cols = colnames(.)
    )
  
  shap_values %>%
    magrittr::set_colnames(names) %>%
    dplyr::select(dplyr::all_of(important_vars)) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "value",
      cols = colnames(.)
    ) %>%
    dplyr::bind_cols(var_val = variable_values$var_val) %>%
    dplyr::mutate(variable = factor(variable, levels = important_vars)) %>%
    ggplot2::ggplot() +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(y = variable, x = value, color = var_val),
      groupOnX = FALSE,
      size = 2
    ) + 
    ggplot2::theme_classic() +
    ggplot2::xlab("SHAP Value") +
    ggplot2::ylab("Variable") +
    ggplot2::scale_color_gradient2(
      high = colorscale[1],
      mid = colorscale[2],
      low = colorscale[3],
      midpoint = 0.5,
      breaks = c(0.1, 0.9),
      labels = c("Low", "High")
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(color = "Value of Variable") +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family),
      legend.position = legend.position,
      aspect.ratio = 0.75,
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      legend.background = ggplot2::element_rect(color = "#0D3B66", fill = "#ffffff")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        label = TRUE,
        title.position = "top",
        title.hjust = 0.5,
        ticks = FALSE,
        direction = "horizontal",
        barwidth = ggplot2::unit(1.5, "in"),
        barheight = ggplot2::unit(0.1, "in")
      )
    )
}