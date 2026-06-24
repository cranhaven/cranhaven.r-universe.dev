########################
#### EMdemoIris #######
########################

set.seed(123)
data(iris)
data <- as.matrix(iris[,-5])
n <- nrow(data)
p <- 4
T <- 1
X <- array(NA, dim = c(p, T, n))
for(i in 1:n){
  X[,,i] <- data[i,]
}
init <- MatTrans.init(X, K = 3, n.start = 10)
V <- MatTrans.EM(X, initial = init,                        
                           trans = "None", silent = TRUE)
V$best.bic
V$best.model
table(V$best.result[[1]]$id, iris[,5])


