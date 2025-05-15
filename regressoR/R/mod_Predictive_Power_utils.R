# pairs_power
pairs_power <- function(data, decimals = 2){
  pairs.panels(var.numericas(data), digits = decimals, bg='black', ellipses=FALSE, smooth=FALSE,
               lm=TRUE, cex.cor = 0.5, cex.main=0.1, pch=20, main='',
               hist.col = gg_color_hue(3)[3], oma=c(1,1,1,1))
}