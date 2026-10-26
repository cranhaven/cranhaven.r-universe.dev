# Taken from STRBook
plot_cov_strips <- function(C, spat_df, xlab)
{
  group_strip <- NULL
  space <- NULL
  for (i in seq_along(unique(spat_df$group_strip))) {
    spat_strip <- spat_df %>%
      dplyr::filter(group_strip == i) %>%
      dplyr::arrange(space)
    idx <- spat_strip$n
    jitter <- seq(0, 1e-04, length = length(idx))
    fields::image.plot(spat_strip$space + jitter,
                       spat_strip$space + jitter,
                       C[idx, idx],
                       xlab = xlab,
                       ylab = xlab,
                       zlim = c(-15, 85),
                       col = fields::tim.colors(10),
                       cex = 200)
  }
}
