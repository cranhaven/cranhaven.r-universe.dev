########################
#### EMdemoIMDb3 #######
########################

set.seed(0)

data("IMDb")

Y <- IMDb$Y/100

p <- dim(Y)[1]
T <- dim(Y)[2]
n <- dim(Y)[3]

K <- 3

init <- MatTrans.init(Y, K=K, n.start = 2)

system.time(M3 <- MatTrans.EM(Y, initial = init,
  la = matrix(1.1, K, p), nu = matrix(1.1, K, T),
  trans = "Power", all.models = TRUE))

M3$best.model

M3$best.bic

bic.sorted <- sort(M3$bic, index.return = TRUE, decreasing = FALSE)

M3$model[bic.sorted$ix[1:5]]

bic.sorted$x[1:5]









