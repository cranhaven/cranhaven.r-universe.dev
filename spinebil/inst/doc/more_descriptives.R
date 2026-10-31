## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)


## -----------------------------------------------------------------------------
# One structure
df_sine <- spinebil::data_gen("sine", n = 400, seed = 1)

# Multiple structures at once
df_all <- spinebil::data_gen("all", n = 400, seed = 1)


## ----fig.height=6.5, fig.width=7.5--------------------------------------------
ggplot2::ggplot(df_all, ggplot2::aes(x, y)) +
  ggplot2::geom_point(alpha = 0.6, size = 0.6) +
  ggplot2::facet_wrap(~structure) +
  ggplot2::labs(
    title = "Synthetic 2D Structures from dataGen()",
    subtitle = "Each panel generated with n = 400"
  ) +
  ggplot2::xlim(-1.5, 1.5) +
  ggplot2::ylim(-1.5, 1.5) +
  ggplot2::theme(
    aspect.ratio = 1,
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )


## -----------------------------------------------------------------------------
# Generate a 4-degree orthogonal polynomial basis
dPoly <- spinebil::data_gen("polynomial", n = 200, degree = 4, seed = 1)

ggplot2::ggplot(dPoly, ggplot2::aes(x = dPoly[, 1], y = dPoly[, 2])) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::xlab("polynomial degree 1") +
  ggplot2::ylab("polynomial degree 2") +
  ggplot2::theme(
    aspect.ratio = 1,
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )


## -----------------------------------------------------------------------------
eps_g <- spinebil::noise_gen(n = 500, type = "gaussian", level = 0.06, seed = 340)
eps_t <- spinebil::noise_gen(n = 500, type = "t_distributed", level = 0.15, seed = 341)
eps_cau <- spinebil::noise_gen(n = 500, type = "cauchy", level = 0.04, seed = 342)


## ----fig.height=4.5, fig.width=7.5--------------------------------------------
set.seed(345)
n <- 500
sine_clean <- spinebil::data_gen("sine", n = n) |> dplyr::mutate(y = sin(x))

# Noise to add on y
eps <- spinebil::noise_gen(n = n, type = "gaussian", level = 0.06, seed = 123)$value
sine_noisy <- sine_clean |> dplyr::mutate(y = y + eps)

dplyr::bind_rows(
  sine_clean |> dplyr::mutate(which = "clean"),
  sine_noisy |> dplyr::mutate(which = "noisy (gaussian, level = 0.06)")
) |>
  ggplot2::ggplot(ggplot2::aes(x, y, color = which)) +
  ggplot2::geom_point(alpha = 0.6, size = 0.6) +
  ggplot2::labs(
    title = "Sine Structure with and without Added Noise",
    color = NULL
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )


## -----------------------------------------------------------------------------
data <- as.data.frame(spinebil::data_gen(type = "polynomial", degree = 2))
res <- spinebil::ppi_mean(data, spinebil::scag_index("stringy"), n_sim = 5)
res


## -----------------------------------------------------------------------------
res <- spinebil::ppi_scale(spinebil::data_gen("polynomial", degree = 3), spinebil::scag_index("stringy"), n_sim = 3)

head(res)


## -----------------------------------------------------------------------------
res |>
  dplyr::mutate(sigma = factor(sigma, levels = c(0, 1), labels = c("structured", "noise"))) |>
  ggplot2::ggplot(ggplot2::aes(x = sigma, y = index, fill = sigma)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~var_pair) +
  ggplot2::labs(
    title = "Index scale on structured vs noisy data",
    x = NULL, y = "index"
  )


## -----------------------------------------------------------------------------
thr <- spinebil::ppi_noise_threshold(
  index_fun   = spinebil::scag_index("stringy"),
  noise_type  = "cauchy",
  noise_level = 0.1,
  n_sim       = 10,
  n_obs       = 100
)
thr


## -----------------------------------------------------------------------------
#| echo: true
#| output: false
result <- spinebil::ppi_samplesize_effect(spinebil::scag_index("stringy"), n_sim = 4, sample_sizes = seq(30, 2000, by = 25))



## -----------------------------------------------------------------------------
head(result)
tail(result)


## ----fig.height=4.5, fig.width=7.5--------------------------------------------
ggplot2::ggplot(result, ggplot2::aes(sample_size, percentile95)) +
  ggplot2::geom_point(alpha = 0.7, size = 1) +
  ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.1) +
  ggplot2::labs(
    x = "Sample size (n)",
    y = "95th percentile of index"
  )

