## ----setup--------------------------------------------------------------------
if (!require("geohabnet")) {
  utils::install.packages("geohabnet")
}
library(geohabnet)

## ----fetch--------------------------------------------------------------------
avocado_mon <- geohabnet::cropharvest_rast("potato", "monfreda")

## ----run----------------------------------------------------------------------
avocado_result <- geohabnet::msean(avocado_mon, global = TRUE, link_threshold  = 0.000001,
                                   inv_pl = list(beta = c(0.5),
                                                 metrics = c("betweeness"),
                                                 weights = c(100),
                                                 cutoff = -1), res = 24,
                                   neg_exp = list(gamma = c(0.1),
                                                  metrics = c("betweeness"),
                                                  weights = c(100), cutoff = -1))

