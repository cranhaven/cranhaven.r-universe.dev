#' SHAP Observation Plot
#' 
#' This Function plots the given contributions for a single observation, and
#' demonstrates how the model arrived at the prediction for the given
#' observation.
#' 
#' This function allows the user to pass a sing row from a data frame of SHAP 
#' values and variable values along with an expected model output and it 
#' returns a ggplot object displaying a specific map of the effect of Variable 
#' value on SHAP value. It is created with `{ggplot2}`, and the returned value 
#' is a `{ggplot2}` object that can be modified for given themes/colors.
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
#' @param expected_value The expected value of the SHAP explainer, either 
#'   returned by `mshap()` or obtained from the python `{shap}` module.
#' @param names A character vector of variable names, corresponding to the
#'   order of the columns in both `variable_values` and `shap_values`. If
#'   `NULL` (default), then the column names of the `variable_values` are 
#'   taken as `names`.
#' @param num_vars An integer specifying the number of variables to show in the
#'   plot, defaults to the 10 most important.
#' @param fill_colors A character vector of length 2. The first element
#'   specifies the fill of a negative SHAP value and the second element
#'   specifies the fill of a positive SHAP value.
#' @param connect_color A string specifying the color of the line segment that
#'   connects the SHAP value bars
#' @param expected_color A string specifying the color of the line that marks
#'   the baseline value, or the expected model output.
#' @param predicted_color A string specifying the color of the line that marks
#'   the value predicted by the model.
#' @param title A string specifying the title of the plot.
#' @param font_family A string specifying the font family, defaults to Times
#'   New Roman.
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
observation_plot <- function(
  variable_values, 
  shap_values, 
  expected_value, 
  names = NULL,
  num_vars = 10,
  fill_colors = c("#A54657", "#0D3B66"),
  connect_color = "#849698",
  expected_color = "#849698",
  predicted_color = "#EE964B",
  title = "Individual Observation Explanation",
  font_family = "Times New Roman"
) {
  
  if (is.null(names)) {
    names <- colnames(variable_values)
  }
  predicted_value <- rowSums(shap_values) + expected_value
  individual_shap <- shap_values %>%
    magrittr::set_colnames(names) %>%
    tidyr::pivot_longer(
      names_to = "covariate",
      values_to = "shap_val",
      cols = colnames(.)
    ) %>%
    dplyr::full_join(
      variable_values %>%
        magrittr::set_colnames(names) %>%
        # dplyr::mutate_if(
        #   is.numeric,
        #   ~as.character(signif(.x, 3))
        # ) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = where(is.numeric),
            .fns = ~as.character(signif(.x, 3))
          )
        ) %>%
        tidyr::pivot_longer(
          cols = colnames(.),
          names_to = "covariate",
          values_to = "var_val"
        ),
      by = c("covariate")
    )
  if (nrow(individual_shap) > num_vars) {
  to_keep <- individual_shap %>%
    dplyr::arrange(desc(abs(shap_val))) %>%
    dplyr::pull(covariate) %>%
    `[`(1:num_vars)
  individual_shap <- individual_shap %>%
    dplyr::mutate(covariate = forcats::fct_other(covariate, keep = to_keep)) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarize(
      shap_val = sum(shap_val),
      var_val = stringr::str_c(unique(var_val), collapse = "|")
    ) %>%
    dplyr::ungroup()
  }
  
  d <- individual_shap %>%
    dplyr::mutate(covariate = forcats::fct_reorder(covariate, shap_val, abs)) %>%
    dplyr::arrange(desc(covariate)) %>%
    dplyr::mutate(
      bar_end = cumsum(shap_val) + c(expected_value),
      bar_start = c(expected_value, head(bar_end, -1)),
      is_positive = shap_val > 0
    ) 
  d %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = covariate, 
      xmin = as.numeric(covariate) - 0.25, 
      xmax = as.numeric(covariate) + 0.25, 
      ymin = bar_end, 
      ymax = bar_start, 
      label = var_val,
      fill = is_positive
    ) + 
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = expected_value),
      color = expected_color,
      size = 1.5
    ) +
    ggplot2::geom_rect(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = setNames(fill_colors, c(FALSE, TRUE))) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = bar_end,
        yend = bar_end,
        x = as.numeric(covariate) + 0.25,
        xend = as.numeric(covariate) - 1.25
      ),
      color = connect_color,
      lwd = 1
    ) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = predicted_value),
      color = predicted_color,
      size = 1.5
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(expected_value, predicted_value),
      labels = c(
        stringr::str_c(
          "Baseline:\n", 
          signif(expected_value, 6)
        ),
        ""
      ),
      sec.axis = ggplot2::sec_axis(
        trans = ~.,
        breaks = c(predicted_value, expected_value),
        labels = c(
          stringr::str_c(
            "Model Prediction:\n", 
            signif(predicted_value, 6)
          ),
          ""
        )
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = rev(stringr::str_c(d$covariate, "\n", ifelse(d$covariate == "Other", " ", d$var_val)))
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family),
      legend.position = "none",
      axis.text.y = ggplot2::element_text(
        family = font_family,
        size = 10
      ),
      axis.text.x = ggplot2::element_text(
        family = font_family,
        size = 12,
        hjust = 0.5
      ),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.ticks.x = ggplot2::element_blank()
    )
}


