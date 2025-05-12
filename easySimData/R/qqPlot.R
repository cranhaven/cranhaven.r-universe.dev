
qqPlot <- function(samples, quantile_function, var_name = '') {
  n <- length(samples)
  theoretical <- quantile_function(ppoints(n))
  df <- data.frame(
    Theoretical = theoretical,
    Empirical = sort(samples)
  )

  with(df,
  ggplot(data = df, aes(x = Theoretical, y = Empirical)) +
    geom_point(size = .5) +
    geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed') +
    annotate('text', x = Inf, y = -Inf, label = var_name,
              hjust = 1, vjust = -1) +
    labs(x = NULL, y = NULL) +
    theme_minimal()
  )
}

