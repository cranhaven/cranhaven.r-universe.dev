set.seed(123)


#Application to dataset IMDb
data("IMDb")
X <- IMDb$Y /100


n <- dim(X)[3]
p <- dim(X)[1]
T <- dim(X)[2]
K <- 2

#Run the EM algorithm with parsimonious models

initial <- MatTrans.init(X, K, n.start = 1, scale = 1)

Manly <- MatTrans.EM(X, initial = initial, la.type = 0, trans = "Manly", la = matrix(0.9, K, p), nu = matrix(0.9, K, T), silent = FALSE)

Power <- MatTrans.EM(X, initial = initial, la.type = 0, trans = "Power", la = matrix(0.9, K, p), nu = matrix(0.9, K, T), silent = FALSE)

Gauss <- MatTrans.EM(X, initial = initial, la.type = 0, trans = "None", silent = FALSE)

print.EM(Manly)
print.EM(Power)
print.EM(Gauss)
