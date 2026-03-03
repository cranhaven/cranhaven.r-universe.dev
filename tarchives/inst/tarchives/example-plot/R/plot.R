get_plot <- function(data, model) {
  if (interactive()) {
    plot(Sepal.Width ~ Sepal.Length, data = data)
    abline(model)
  }
}
