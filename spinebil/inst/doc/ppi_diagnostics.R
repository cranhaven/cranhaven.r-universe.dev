## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)


## -----------------------------------------------------------------------------
#| fig.width: 9
#| fig.asp: 0.33  
#| fig.align: center
#| out-width: 100%

n <- 500
p <- 4
lst <- list(
  Pipe   = spinebil::pipe_data(n, p),
  Sine   = spinebil::sin_data(n, p, 1),
  Spiral = spinebil::spiral_data(n, p)
)

df_all <- do.call(rbind, lapply(names(lst), function(lbl) {
  d <- lst[[lbl]]
  data.frame(x = d[[p - 1]], y = d[[p]], structure = lbl)
}))

ggplot2::ggplot(df_all, ggplot2::aes(x, y)) +
  ggplot2::geom_point(alpha = 0.6, size = 0.6) +
  ggplot2::facet_wrap(~ structure, nrow = 1, scales="free") +
  ggplot2::theme(
    aspect.ratio = 1,
    axis.text  = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank()
  )



## -----------------------------------------------------------------------------
d <- as.matrix(spinebil::spiral_data(30,4))
tPath <- tourr::save_history(d, max_bases=2)
tPath <- as.list(tourr::interpolate(tPath, 0.3))
idx <- spinebil::scag_index("stringy")
compS <- spinebil::compare_smoothing(d, tPath, idx, alphaV = c(0.01, 0.05), n=2)
spinebil::plot_smoothing_comparison(compS, lPos = "bottom")


## -----------------------------------------------------------------------------
data <- as.matrix(spinebil::spiral_data(50, 4))
indexF <- spinebil::scag_index("stringy")
cutoff <- 0.7
structure_plane <- spinebil::basis_matrix(3,4,4)
spinebil::squint_angle_estimate(data, indexF, cutoff, structure_plane, n=10)


## -----------------------------------------------------------------------------
d <- as.matrix(spinebil::spiral_data(100, 4))
m <- list(spinebil::basis_matrix(1,2,4), spinebil::basis_matrix(3,4,4))
index_list <- list(tourr::holes(), tourr::norm_kol(100))
index_labels <- c("holes", "norm kol")
trace <- spinebil::get_trace(d, m, index_list, index_labels)
spinebil::plot_trace(trace)


## -----------------------------------------------------------------------------
spinebil::plot_trace(trace, rescY = FALSE)


## -----------------------------------------------------------------------------
#| fig-width: 9
#| fig-height: 3
d <- as.matrix(spinebil::sin_data(30, 2))
index_list <- list(tourr::holes(), spinebil::scag_index("stringy"), spinebil::mine_indexE("MIC"))
index_labels <- c("holes", "stringy", "mic")
pRot <- spinebil::profile_rotation(d, index_list, index_labels, n = 50)
spinebil::plot_rotation(pRot)


## -----------------------------------------------------------------------------
d <- as.matrix(spinebil::spiral_data(500, 4))
t <- purrr::map(1:10, ~ tourr::basis_random(4))
idx <- spinebil::scag_index("stringy")
spinebil::time_sequence(d, t, idx, 10)

