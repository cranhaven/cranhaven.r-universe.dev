########################
#### EMdemoIMDb2 #######
########################

set.seed(0)

data("IMDb")

Y <- IMDb$Y/100

p <- dim(Y)[1]
T <- dim(Y)[2]
n <- dim(Y)[3]

K <- 3

init <- MatTrans.init(Y, K=K, n.start = 2)
system.time(M2 <- MatTrans.EM(Y, initial = init,
  trans = "Power", short.iter = 5, long.iter = 1000,
  all.models = TRUE, silent = FALSE))

M2$best.model

M2$best.bic

bic.sorted <- sort(M2$bic, index.return = TRUE, decreasing = FALSE)

M2$model[bic.sorted$ix[1:5]]

bic.sorted$x[1:5]






