########################
#### EMdemoCrime3 ######
########################

set.seed(123)

data(crime)

Y <- crime$Y[c(2,7,10),1:7,]/1000

p <- dim(Y)[1]
T <- dim(Y)[2]
n <- dim(Y)[3]
K <- 2

init <- MatTrans.init(Y, K = K, n.start = 2)

M <- MatTrans.EM(Y, initial = init, la = matrix(1.1,K,p),
  col.skew = FALSE, model = "A-EVE-VV", trans = "Power")

M$best.result[[1]]$la

M$best.result[[1]]$nu

M$best.loglik

M$best.bic







