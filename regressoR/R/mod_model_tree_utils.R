tree_plot <- function(modelo) {
  prp(modelo, type = 2, extra = 100, nn = TRUE, varlen = 0, 
      faclen = 0, fallen.leaves = TRUE, branch.lty = 6, 
      shadow.col = '#dedede',box.col = '#c8b028')
}

tree_rules <- function(modelo, varpred) {
  rpart.rules(
    modelo, cover = TRUE, nn = TRUE, roundint = FALSE, style = "tall",
    digits = 3, response.name = paste0("Rule Number - ", varpred))
}