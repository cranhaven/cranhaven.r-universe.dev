set.seed(123)



data("crime")
X <- crime$Y[c(2,7),,] / 1000

p <- dim(X)[1]
T <- dim(X)[2]
n <- dim(X)[3]
K <- 2

initial <- MatTrans.init(X, K = K, n.start = 1)

Manly <- MatTrans.EM(X, initial = initial, la.type = 1, trans = "Manly", model = "A-EVV-XX", silent = FALSE)

Power <- MatTrans.EM(X, initial = initial, la.type = 1, trans = "Power", model = "X-EVI-EI", silent = FALSE)

Gauss <- MatTrans.EM(X, initial = initial, trans = "None", model = "A-XXX-EI", silent = FALSE)



summary.EM(Manly)
summary.EM(Power)
summary.EM(Gauss)
