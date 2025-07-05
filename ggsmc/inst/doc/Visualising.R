## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----sir_data-----------------------------------------------------------------
library(ggsmc)
data(sir_cwna_model)
head(sir_cwna_model)

## ----histogram, fig.dim = c(6.666666, 5)--------------------------------------
plot_histogram(sir_cwna_model,
               parameter = "x",
               dimension = 1,
               target = 20,
               bins = 20)

## ----density, fig.dim = c(6.666666, 5)----------------------------------------
plot_density(sir_cwna_model,
             parameter = "x",
             dimension = 1,
             target = 20)

## ----scatter, fig.dim = c(6.666666, 5)----------------------------------------
plot_scatter(sir_cwna_model,
             x_parameter = "x",
             x_dimension = 1,
             y_parameter = "x",
             y_dimension = 2,
             target = 20,
             alpha = 0.5,
             max_size = 3)

## ----genealogy, fig.dim = c(6.666666, 5)--------------------------------------
plot_genealogy(sir_cwna_model,
               parameter = "x",
               dimension = 1,
               use_initial_points = FALSE,
               vertical = FALSE,
               alpha_lines = 0,
               alpha_points = 0.05,
               arrows = FALSE)

## ----genealogy2, fig.dim = c(6.666666, 5)-------------------------------------
data(mixture_25_particles)
plot_genealogy(mixture_25_particles,
               parameter = "θ",
               dimension = 1,
               alpha_lines = 0.2,
               alpha_points = 0.4)

## ----time_series, fig.dim = c(6.666666, 5)------------------------------------
data(lv_output)
plot_time_series(lv_output,
                 parameters = c("X","Y"),
                 alpha = 0.5,
                 ylimits=c(0,600))

## ----modified_genealogy, fig.dim = c(6.666666, 5)-----------------------------
data(cwna_data)
plot_genealogy(sir_cwna_model,
               parameter = "x",
               dimension = 1,
               use_initial_points = FALSE,
               vertical = FALSE,
               alpha_lines = 0,
               alpha_points = 0.05,
               arrows = FALSE,
               default_title = TRUE) +
ggplot2::geom_line(data=cwna_data,ggplot2::aes(x=Index,y=Position),colour="red",inherit.aes = FALSE) +
ggplot2::theme_minimal() +
ggplot2::theme(legend.position="none")

## ----animated_histogram, fig.dim = c(6.666666, 5), results='hide'-------------
animate_histogram(sir_cwna_model,
                  parameter = "x",
                  dimension = 1,
                  bins = 20,
                  duration = 10)

## ----animated_density, fig.dim = c(6.666666, 5), results='hide'---------------
animate_density(sir_cwna_model,
                parameter = "x",
                dimension = 1,
                duration = 10)

## ----animated_scatter, fig.dim = c(6.666666, 5), results='hide'---------------
animate_scatter(sir_cwna_model,
                x_parameter = "x",
                x_dimension = 1,
                y_parameter = "x",
                y_dimension = 2,
                alpha = 0.5,
                max_size = 3,
                duration = 10)

## ----animated_time_series, fig.dim = c(6.666666, 5), results='hide'-----------
data(`lv_output`)
animate_reveal_time_series(lv_output,
                           parameters = c("X","Y"),
                           alpha = 0.5,
                           ylimits=c(0,600),
                           duration = 10)

