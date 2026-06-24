########################
#### EMdemoIMDb1 #######
########################

set.seed(0)

data("IMDb")

Y <- IMDb$Y/100

p <- dim(Y)[1]
T <- dim(Y)[2]
n <- dim(Y)[3]

K <- 3

init <- MatTrans.init(Y, K=K, n.start = 2)
system.time(M1 <- MatTrans.EM(Y, initial = init,
  trans = "Power", short.iter = 5, long.iter = 1000,
  all.models = FALSE, silent = FALSE))

M1$model

M1$best.model

M1$bic

M1$best.bic

M1$best.result[[1]]$id








