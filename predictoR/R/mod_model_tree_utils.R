tree_plot <- function(modelo, n) {
  prp(modelo, type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
      fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
      box.col = gg_color_hue(n)[modelo$frame$yval], roundint = FALSE)
}

tree_rules <- function(modelo, varpred) {
  rpart.plot::rpart.rules(
    modelo, cover = TRUE, nn = TRUE, roundint = FALSE, style = "tall",
    digits = 3, response.name = paste0("Rule Number - ", varpred))
}