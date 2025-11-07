if (interactive()) {
  library(mshap)
  library(ggplot2)
  
  # Generate fake data
  set.seed(18)
  dat <- data.frame(
    age = runif(1000, min = 0, max = 20),
    prop_domestic = runif(1000),
    model = sample(c(0, 1), 1000, replace = TRUE),
    maintain = rexp(1000, .01) + 200
  )
  shap <- data.frame(
    age = rexp(1000, 1/dat$age) * (-1)^(rbinom(1000, 1, dat$prop_domestic)),
    prop_domestic = -200 * rnorm(100, dat$prop_domestic, 0.02) + 100,
    model = ifelse(dat$model == 0, rnorm(1000, -50, 30), rnorm(1000, 50, 30)),
    maintain = (rnorm(1000, dat$maintain, 100) - 400) * 0.2
  )
  expected_value <- 1000
  
  # A Basic sumary plot
  summary_plot(
    variable_values = dat,
    shap_values = shap
  )
  
  # A Customized summary plot
  summary_plot(
    variable_values = dat,
    shap_values = shap,
    legend.position = "bottom",
    names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
    colorscale = c("blue", "purple", "red"),
    font_family = "Arial",
    title = "A Custom Title"
  )
  
  # A basic observation plot
  observation_plot(
    variable_values = dat[1,],
    shap_values = shap[1,],
    expected_value = expected_value
  )
  
  # A Customized Observation plot
  observation_plot(
    variable_values = dat[1,],
    shap_values = shap[1,],
    expected_value = expected_value,
    names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
    font_family = "Arial",
    title = "A Custom Title",
    fill_colors = c("red", "blue"),
    connect_color = "black",
    expected_color = "purple",
    predicted_color = "yellow"
  )
  
  # Add elements to the returned object
  # see vignette("mshap_plots") for more information
  observation_plot(
    variable_values = dat[1,],
    shap_values = shap[1,],
    expected_value = expected_value,
    names = c("Age", "% Domestic", "Model", "Maintenence Hours"),
    font_family = "Arial",
    title = "A Custom Title"
  ) +
    geom_label(
      aes(y = 950, x = 4, label = "This is a really big bar!"),
      color = "#FFFFFF",
      fill = NA
    ) +
    theme(
      plot.background = element_rect(fill = "grey"),
      panel.background = element_rect(fill = "lightyellow")
    )
}