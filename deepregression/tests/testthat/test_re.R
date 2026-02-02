# context("Random Effects")
# 
# test_that("check loss", {
#   
#   n <- 1000
#   g <- 10
#   
#   data = data.frame(matrix(rnorm(4*n), c(n,4)))
#   colnames(data) <- c("x1","x2","x3","xa")
#   data$cat <- gl(g,n/g)
#   
#   formula_lin <- ~ 1 + x1 + cat
#   formula_ri <- ~ 1 + x1 + ri(cat)
#   
#   re_var <- 2
#   
#   b <- rnorm(g, sd = sqrt(re_var))
#   
#   y <- rnorm(n) + 1 + data$x1 + rep(b, each = n/g)
#   data$y <- y
#   
#   data <- data[sample(1:nrow(data)),]
#   y <- data$y
#   
#   
#   mod_lin <- deepregression(
#     list_of_formulas = list(loc = formula_lin, scale = ~ 1),
#     data = data, y = y
#   )
#   
#   mod_lin %>% fit(epochs = 5000, early_stopping = TRUE)
# 
#   # mod_lin %>% coef()
#   
#   mod_ri <- deepregression(
#     list_of_formulas = list(loc = formula_ri, scale = ~ 1),
#     data = data, y = y
#   )
#   
#   mod_ri %>% fit(epochs = 5000, early_stopping = TRUE)
# 
#   # mod_ri$model$weights
#   # mod_ri %>% coef()
# 
#   optm <- optimizer_adam
#   lr <- 1e-5
#   
#   # define MDMM optimizer
#   optimizers = list(
#     optm(learning_rate=lr),
#     optm(learning_rate=-lr)
#   )
#   
#   optimizer <- function(model){
#     
#     all_weights_but_variance <- c(model$layers[1:4],
#                                   model$layers[6:length(model$layers)])
#     
#     
#     optimizers_and_layers = list(tuple(optimizers[[1]], all_weights_but_variance), 
#                                  tuple(optimizers[[2]], model$layers[5]))
#     optimizer = multioptimizer(optimizers_and_layers)
#     return(optimizer)
#   }
#   
#   mod_ri <- deepregression(
#     list_of_formulas = list(loc = formula_ri, scale = ~ 1),
#     data = data, y = y,
#     optimizer = optimizer
#   )
# 
#   mod_ri %>% fit(epochs = 5000, 
#                  early_stopping = TRUE,
#                  patience = 50)
#   
#   exp(mod_ri$model$weights[[3]])^2
#   sum(mod_ri$model$weights[[2]]$numpy())
#   sapply(mod_ri$model$weights[c(4,1,5)], function(x) x$numpy())
#   exp(mod_ri$model$weights[[5]]$numpy())
#   
#   mod_gam <- mgcv::gam(y ~ 1 + x1 + s(cat, bs = "re"), data = data)
#   gam.vcomp(mod_gam)
#   coef(mod_gam)[1:2]
#   
#   plot(coef(mod_gam)[-1*1:2] ~ mod_ri$model$weights[[2]]$numpy())
#   
# })