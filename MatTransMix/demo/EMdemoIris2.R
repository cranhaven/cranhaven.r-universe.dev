########################
#### EMdemoIris 2 ######
########################

set.seed(123)
data(iris)
data <- as.matrix(iris[,-5])
n <- nrow(data)
p <- 2
T <- 2
X1 <- array(NA, dim = c(p, T, n))
for(i in 1:n){
  X1[1,1,i] <- data[i,1]
  X1[1,2,i] <- data[i,2]
  X1[2,1,i] <- data[i,3]
  X1[2,2,i] <- data[i,4]
}
init <- MatTrans.init(X1, K = 3, n.start = 1, scale = 1)
M1 <- MatTrans.EM(X1, initial = init, model = "A-XXX-VE",
                     trans = "None", silent = TRUE, size.control = 3)
init <- MatTrans.init(X1, K = 3, n.start = 1, scale = 10)
M2 <- MatTrans.EM(X1, initial = init, model = "A-XXX-VE",
                     trans = "None", silent = TRUE, size.control = 3)
init <- MatTrans.init(X1, K = 3, n.start = 1, scale = 0.1)
M3 <- MatTrans.EM(X1, initial = init, model = "A-XXX-VE",
                     trans = "None", silent = TRUE, size.control = 3)

BIC <- cbind(M1$bic, M2$bic, M3$bic)
BIC